//! worker: Multi thread worker module

const std = @import("std");
const testing = std.testing;
const WaitGroup = std.Thread.WaitGroup;

const core = @import("./core.zig");
const compat = @import("./compat.zig");
const math = @import("./math.zig");
const matrix = @import("./matrix.zig");
const vectest = @import("./vectest.zig");

/// Create `WaitGroup` wrapper function
///
/// This function ensures to call `WaitGroup.finish()`
fn wgWrapper(comptime f: anytype) type {
    return struct {
        fn call(wg: *WaitGroup, args: anytype) void {
            defer wg.finish();
            @call(.auto, f, args);
        }
    };
}

const Range = struct {
    start: usize,
    end: usize,
};

/// Iterator for Records
///
/// Records are divided to SIMD size segments,
/// then these segments are equally distributed to threads.
const RecordItrator = struct {
    /// Next start position
    start: usize,

    /// Total length
    len: usize,

    /// Number of threads
    nthreads: usize,

    /// SIMD vector size (Number of elements in SIMD / size of segment)
    size: usize,

    /// Number of segments
    batch: usize,

    /// Number of remained segments after equally distributed to threads.
    rem: usize,

    const Self = @This();

    fn init(comptime T: type, len: usize, comptime simd_size: ?usize, nthreads: usize) Self {
        const size = @max(1, core.sizeForSIMD(T, simd_size));
        const segments = std.math.divCeil(usize, len, size) catch unreachable;

        return .{
            .start = 0,
            .len = len,
            .nthreads = nthreads,
            .size = size,
            .batch = @divFloor(segments, nthreads),
            .rem = @mod(segments, nthreads),
        };
    }

    /// Get the next `Range`
    fn next(self: *Self) ?Range {
        if (self.start >= self.len) {
            return null;
        }

        const start = self.start;
        if (self.nthreads == 0) {
            // Single thread mode.
            // Consume all records at once.
            self.start = self.len;

            return .{
                .start = start,
                .end = self.len,
            };
        }

        var end = start + self.batch * self.size;
        if (self.rem > 0) {
            end += self.size;
            self.rem -= 1;
        }

        self.start = end;
        std.debug.assert(start < end);
        return .{
            .start = start,
            .end = @min(end, self.len),
        };
    }
};

pub const Worker = struct {
    pool: ?*std.Thread.Pool,

    const Self = @This();

    pub fn init(options: std.Thread.Pool.Options) !Self {
        var pool = try options.allocator.create(std.Thread.Pool);
        errdefer options.allocator.destroy(pool);

        try pool.init(options);
        errdefer pool.deinit();

        return .{
            .pool = pool,
        };
    }

    pub fn deinit(self: *Self) void {
        if (self.pool) |p| {
            const allocator = p.allocator;
            p.deinit();
            allocator.destroy(p);
            self.pool = null;
        }
    }

    pub fn nThreads(self: Self) usize {
        return if (self.pool) |p| p.threads.len else 0;
    }

    pub fn wait(self: *Self, w: *WaitGroup) !void {
        if (self.pool) |p| {
            p.waitAndWork(w);
            return;
        }
        return error.AlreadyDeinit;
    }

    fn spawnWg(self: *Self, wait_group: *WaitGroup, comptime f: anytype, args: anytype) !void {
        if (self.pool) |p| {
            wait_group.start();
            try p.spawn(wgWrapper(f).call, .{ wait_group, args });
        } else {
            return error.AlreadyDeinit;
        }
    }

    pub fn nullary() void {}

    pub fn unary(self: *Self, comptime options: math.UnaryOptions, wait_group: *WaitGroup, arg: anytype, out: anytype) !void {
        const T = options.type;
        var it = RecordItrator.init(T, out.len, options.simd_size, self.nThreads());

        while (it.next()) |range| {
            const a = arg[range.start..range.end];
            const o = out[range.start..range.end];
            try self.spawnWg(wait_group, math.unary, .{ options, a, o });
        }
    }

    pub fn binary(self: *Self, comptime options: math.BinaryOptions, wait_group: *WaitGroup, arg1: anytype, arg2: anytype, out: anytype) !void {
        const T = options.type;
        var it = RecordItrator.init(T, out.len, options.simd_size, self.nThreads());
        while (it.next()) |range| {
            const a1 = arg1[range.start..range.end];
            const a2 = arg2[range.start..range.end];
            const o = out[range.start..range.end];
            try self.spawnWg(wait_group, math.binary, .{ options, a1, a2, o });
        }
    }

    pub fn ternary(self: *Self, comptime options: math.TernaryOptions, wait_group: *WaitGroup, arg1: anytype, arg2: anytype, arg3: anytype, out: anytype) !void {
        const T = options.type;
        var it = RecordItrator.init(T, out.len, options.simd_size, self.nThreads());
        while (it.next()) |range| {
            const a1 = arg1[range.start..range.end];
            const a2 = arg2[range.start..range.end];
            const a3 = arg3[range.start..range.end];
            const o = out[range.start..range.end];
            try self.spawnWg(wait_group, math.ternary, .{ options, a1, a2, a3, o });
        }
    }

    pub fn reduce(self: *Self, comptime options: math.ReductionOptions, wait_group: *WaitGroup, args: []const options.type, out: *options.type) !void {
        const T = options.type;
        const nthreads = self.nThreads();

        const allocator = self.pool.?.allocator;
        const o = try allocator.alloc(T, nthreads);
        errdefer allocator.free(o);

        const wg = try allocator.create(WaitGroup);
        errdefer allocator.destroy(wg);
        wg.* = WaitGroup{};

        const R = struct {
            const SelfR = @This();

            fn call(a: []const T, oi: *T) void {
                oi.* = math.reduce(options, a);
            }

            fn summarize(w: *WaitGroup, a: []const T, oi: *T, alloc: std.mem.Allocator) void {
                w.wait();
                SelfR.call(a, oi);
                alloc.free(a);
                alloc.destroy(w);
            }
        };

        var it = RecordItrator.init(T, args.len, options.simd_size, nthreads);
        var i: usize = 0;
        while (it.next()) |range| {
            const a = args[range.start..range.end];
            try self.spawnWg(wg, R.call, .{ a, &(o[i]) });
            i += 1;
        }

        try self.spawnWg(wait_group, R.summarize, .{ wg, o[0..i], out, allocator });
    }

    pub fn dot(self: *Self, comptime options: matrix.Options, wait_group: *WaitGroup, a: []const options.type, b: []const options.type, out: *options.type) void {
        const T = options.type;
        const nthreads = self.nThreads();

        const allocator = self.pool.?.allocator;
        const o = try allocator.alloc(T, nthreads);
        errdefer allocator.free(o);

        const wg = try allocator.create(WaitGroup);
        errdefer allocator.destroy(wg);
        wg.* = WaitGroup{};

        const R = struct {
            const SelfR = @This();

            fn call(ai: []const T, bi: []const T, oi: *T) void {
                oi.* = matrix.dot(options, ai, bi);
            }

            fn summarize(w: *WaitGroup, ov: []const T, oo: *T, alloc: std.mem.Allocator) void {
                w.wait();
                oo.* = math.reduce(.{ .type = T, .simd_size = options.simd_size, .f = .sum }, ov);
                alloc.free(ov);
                alloc.destroy(w);
            }
        };

        var it = RecordItrator.init(T, a.len, options.simd_size, nthreads);
        var i: usize = 0;
        while (it.next()) |range| {
            const ai = a[range.start..range.end];
            const bi = b[range.start..range.end];
            try self.spawnWg(wg, R.call, .{ ai, bi, &(o[i]) });
            i += 1;
        }

        try self.spawnWg(wait_group, R.summarize, .{ wg, o[0..i], out, allocator });
    }

    pub fn matMulMV(self: *Self, comptime options: matrix.Options, wait_group: *WaitGroup, m: matrix.Matrix(options.type, true), v: []const options.type, out: []options.type) !void {
        const T = options.type;

        var it = RecordItrator.init(T, out.len, options.simd_size, self.nThreads());
        while (it.next()) |range| {
            const mi = m.subMatrix(range.start, range.end);
            const oi = out[range.start..range.end];
            try self.spawnWg(wait_group, matrix.mulMV, .{ options, mi, v, oi });
        }
    }

    pub fn matMulMM(self: *Self, comptime options: matrix.Options, wait_group: *WaitGroup, m1: matrix.Matrix(options.type, true), m2: matrix.Matrix(options.type, true), out: matrix.Matrix(options.type, false)) !void {
        const T = options.type;

        var it = RecordItrator.init(T, out.row, options.simd_size, self.nThreads());

        while (it.next()) |range| {
            const m1_i = m1.subMatrix(range.start, range.end);
            const oi = out.subMatrix(range.start, range.end);
            try self.spawnWg(wait_group, matrix.mulMM, .{ options, m1_i, m2, oi });
        }
    }
};

test "worker unary" {
    var worker = try Worker.init(.{ .allocator = testing.allocator });
    defer worker.deinit();

    const Test = struct {
        fn do(comptime options: math.UnaryOptions, w: *Worker, N: usize, arg: options.type, out: options.type) !void {
            var a = std.ArrayList(options.type).init(testing.allocator);
            defer a.deinit();
            try a.appendNTimes(arg, N);

            var o = std.ArrayList(options.type).init(testing.allocator);
            defer o.deinit();
            try o.resize(N);

            var t = std.ArrayList(options.type).init(testing.allocator);
            defer t.deinit();
            try t.appendNTimes(out, N);

            var wg = WaitGroup{};
            wg.reset();
            try w.unary(options, &wg, a.items, o.items);

            wg.wait();
            try vectest.expectEqualSlices(options.type, t.items, o.items);
        }
    };

    try Test.do(.{ .type = f32, .f = .ceil }, &worker, 100, 3.2, 4.0);
}

test "worker reduce" {
    var worker = try Worker.init(.{ .allocator = testing.allocator });
    defer worker.deinit();

    const Test = struct {
        fn do(comptime options: math.ReductionOptions, w: *Worker, N: usize, arg: options.type, out: options.type) !void {
            var a = std.ArrayList(options.type).init(testing.allocator);
            defer a.deinit();
            try a.appendNTimes(arg, N);

            var o: options.type = undefined;

            var wg = WaitGroup{};
            wg.reset();
            try w.reduce(options, &wg, a.items, &o);

            wg.wait();
            try vectest.expectEqual(out, o);
        }
    };

    try Test.do(.{ .type = f32, .f = .sum }, &worker, 100, 3.2, 320);
}
