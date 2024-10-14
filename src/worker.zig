//! worker

const std = @import("std");
const testing = std.testing;
const WaitGroup = std.Thread.WaitGroup;

const core = @import("./core.zig");
const compat = @import("./compat.zig");
const math = @import("./math.zig");
const matrix = @import("./matrix.zig");
const vectest = @import("./vectest.zig");

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

const RecordItrator = struct {
    start: usize,
    len: usize,
    nthreads: usize,
    size: usize,
    batch: usize,
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
        var it = RecordItrator.init(options.type, out.len, options.simd_size, self.nThreads());

        while (it.next()) |range| {
            const a = arg[range.start..range.end];
            const o = out[range.start..range.end];
            try self.spawnWg(wait_group, math.unary, .{ options, a, o });
        }
    }

    pub fn binary(self: *Self, comptime options: math.BinaryOptions, wait_group: *WaitGroup, arg1: anytype, arg2: anytype, out: anytype) !void {
        var it = RecordItrator.init(options.type, out.len, options.simd_size, self.nThreads());
        while (it.next()) |range| {
            const a1 = arg1[range.start..range.end];
            const a2 = arg2[range.start..range.end];
            const o = out[range.start..range.end];
            try self.spawnWg(wait_group, math.binary, .{ options, a1, a2, o });
        }
    }

    pub fn ternary(self: *Self, comptime options: math.TernaryOptions, wait_group: *WaitGroup, arg1: anytype, arg2: anytype, arg3: anytype, out: anytype) !void {
        var it = RecordItrator.init(options.type, out.len, options.simd_size, self.nThreads());
        while (it.next()) |range| {
            const a1 = arg1[range.start..range.end];
            const a2 = arg2[range.start..range.end];
            const a3 = arg3[range.start..range.end];
            const o = out[range.start..range.end];
            try self.spawnWg(wait_group, math.ternary, .{ options, a1, a2, a3, o });
        }
    }

    // pub fn reduce(self: *Self, comptime options: math.ReductionOptions, wait_group: *WaitGroup, arg: []const options.type, out: *options.type) void {}

    pub fn dot() void {}

    pub fn matMulMV(self: *Self, comptime options: matrix.Options, wait_group: *WaitGroup, m: matrix.Matrix(options.type, true), v: []const options.type, out: []options.type) !void {
        const cM = matrix.Matrix(options.type, true);

        var it = RecordItrator.init(options.type, out.len, options.simd_size, self.nThreads());
        while (it.next()) |range| {
            const row = range.end - range.start;
            const mi = cM{ .row = row, .column = m.column, .data = m.data[range.start..range.end] };
            const oi = out[range.start..range.end];
            try self.spawnWg(wait_group, matrix.mulMV, .{ options, mi, v, oi });
        }
    }

    pub fn matMulMM(self: *Self, comptime options: matrix.Options, wait_group: *WaitGroup, m1: matrix.Matrix(options.type, true), m2: matrix.Matrix(options.type, true), out: matrix.Matrix(options.type, false)) !void {
        const cM = matrix.Matrix(options.type, true);
        const M = matrix.Matrix(options.type, false);

        var it = RecordItrator.init(options.type, out.row, options.simd_size, self.nThreads());

        while (it.next()) |range| {
            const row = range.end - range.start;
            const m1_i = cM{ .row = row, .column = m1.column, .data = m1.data[range.start..range.end] };
            const oi = M{ .row = row, .column = out.column, .data = out.data[range.start..range.end] };
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

            if (!wg.isDone()) {
                wg.wait();
            }

            try vectest.expectEqualSlices(options.type, t.items, o.items);
        }
    };

    try Test.do(.{ .type = f32, .f = .ceil }, &worker, 100, 3.2, 4.0);
}
