//! matrix module

const std = @import("std");
const testing = std.testing;

const core = @import("./core.zig");
const math = @import("./math.zig");
const vectest = @import("./vectest.zig");

pub const Options = struct {
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

pub fn Matrix(comptime T: type, comptime is_const: bool) type {
    return struct {
        row: usize,
        column: usize,
        data: if (is_const) []const T else []T,

        const Self = @This();

        pub fn getRow(self: Self, r: usize) @TypeOf(self.data) {
            const ridx = r * self.column;
            return self.data[ridx .. ridx + self.column];
        }

        pub fn at(self: Self, r: usize, c: usize) T {
            return self.data[r * self.column + c];
        }

        pub fn subMatrix(self: Self, rstart: usize, rend: usize) Self {
            return .{
                .row = rend - rstart,
                .column = self.column,
                .data = self.data[rstart * self.column .. rend * self.column],
            };
        }
    };
}

pub fn mulMV(comptime options: Options, m: Matrix(options.type, true), v: []const options.type, out: []options.type) void {
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

pub fn mulMM(comptime options: Options, m1: Matrix(options.type, true), m2: Matrix(options.type, true), out: Matrix(options.type, false)) void {
    const T = options.type;
    const size = core.sizeForSIMD(T, options.simd_size);

    const fmaOp = .{ .type = T, .f = .mulAdd, .simd_size = size };

    var r1: usize = 0;
    while (r1 < m1.row) : (r1 += 1) {
        const o = out.getRow(r1);

        var r2: usize = 0;
        while (r2 < m2.row) : (r2 += 1) {
            math.ternary(fmaOp, m1.at(r1, r2), m2.getRow(r2), o, o);
        }
    }
}

test "mul MM" {
    const m1: []const f32 = &.{ 3.2, 2.1, 1.2, 1.5 };
    const m2: []const f32 = &.{ 0.3, 1.2, 1.1, 1.4 };
    const o: []const f32 = &.{
        3.2 * 0.3 + 2.1 * 1.1, 3.2 * 1.2 + 2.1 * 1.4,
        1.2 * 0.3 + 1.5 * 1.1, 1.2 * 1.2 + 1.5 * 1.4,
    };

    var r: [4]f32 = undefined;

    mulMM(.{ .type = f32 }, .{ .row = 2, .column = 2, .data = m1 }, .{ .row = 2, .column = 2, .data = m2 }, .{ .row = 2, .column = 2, .data = &r });

    try vectest.expectEqualSlices(f32, o, &r);
}
