//! matrix module

const std = @import("std");
const testing = std.testing;

const core = @import("./core.zig");

const Options = struct {
    type: type,
    simd_size: ?usize = null,
};

inline fn dotForLoop(comptime T: type, a: []const T, b: []const T) T {
    var r = a[0] * b[0];
    for (a[1..], b[1..]) |ai, bi| {
        r += ai * bi;
    }
    return r;
}

pub fn dot(comptime options: Options, a: []const options.type, b: []const options.type) options.type {
    const T = options.type;
    const size = core.sizeForSIMD(T, options.simd_size);
    const V = @Vector(size, T);

    if ((size < 2) or (a.len < size)) {
        return dotForLoop(T, a, b);
    }

    const rem = @mod(a.len, size);
    const n = a.len - rem;

    var av: V = a[0..size].*;
    var bv: V = b[0..size].*;
    var rv: V = av * bv;

    var i: usize = size;
    while (i < n) : (i += size) {
        av = a[i..][0..size].*;
        bv = b[i..][0..size].*;
        rv += av * bv;
    }

    var r = @reduce(.Add, rv);
    if (rem > 0) {
        r += dotForLoop(T, a[n..], b[n..]);
    }
    return r;
}

test "dot" {
    const N = 100;

    const Test = struct {
        fn do(comptime opt: Options, a: opt.type, b: opt.type) !void {
            var arg1 = std.ArrayList(opt.type).init(testing.allocator);
            defer arg1.deinit();
            try arg1.appendNTimes(a, N);

            var arg2 = std.ArrayList(opt.type).init(testing.allocator);
            defer arg2.deinit();
            try arg2.appendNTimes(b, N);

            const r = dot(opt, arg1.items, arg2.items);
            try testing.expectApproxEqRel(a * b * N, r, 1e-6);
        }
    };

    try Test.do(.{ .type = f32 }, 1.5, 2.0);
    try Test.do(.{ .type = f32, .simd_size = 0 }, 1.5, 2.0);
}

pub fn Matrix(comptime T: type) type {
    return struct {
        row: usize,
        column: usize,
        data: []T,

        const Self = @This();

        pub fn getRow(self: Self, r: usize) []T {
            const ridx = r * self.column;
            return self.data[ridx .. ridx + self.column];
        }
    };
}

pub fn mulMV(comptime options: Options, m: Matrix(options.type), v: []const options.type, out: []options.type) void {
    for (out, 0..) |*o, i| {
        o.* = dot(options, m.getRow(i), v);
    }
}

test "matmul MV" {
    const Test = struct {
        fn do(comptime opt: Options, row: usize, column: usize) !void {
            var m = std.ArrayList(opt.type).init(testing.allocator);
            defer m.deinit();
            try m.appendNTimes(1, row * column);

            var v = std.ArrayList(opt.type).init(testing.allocator);
            defer v.deinit();
            try v.appendNTimes(1, column);

            var o = std.ArrayList(opt.type).init(testing.allocator);
            defer o.deinit();
            try o.resize(row);

            const mat = .{ .row = row, .column = column, .data = m.items };
            mulMV(opt, mat, v.items, o.items);
            for (o.items) |oi| {
                try testing.expectEqual(@as(opt.type, @floatFromInt(column)), oi);
            }
        }
    };

    try Test.do(.{ .type = f32 }, 3, 4);
    try Test.do(.{ .type = f32 }, 325, 431);
    try Test.do(.{ .type = f32, .simd_size = 0 }, 325, 431);
}
