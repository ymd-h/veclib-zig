//! math module

const std = @import("std");
const testing = std.testing;

const compat = @import("./compat.zig");
const core = @import("./core.zig");

const VectorFunction0 = core.VectorFunction0;
const VectorFunction1 = core.VectorFunction1;
const VectorFunction2 = core.VectorFunction2;
const VectorFunction3 = core.VectorFunction3;
const VectorReductionFunction = core.VectorReductionFunction;

const isSIMD = core.isSIMDVector;

const Options = struct {
    type: type,
    simd_size: ?usize = null,
};

pub const NullaryFunction = enum {
    iota,
};

inline fn nullWithIndexFn(comptime T: type, comptime f: NullaryFunction, i: anytype) T {
    return switch (f) {
        .iota => i,
    };
}

const NullaryOptions = struct {
    type: type,
    f: NullaryFunction,
    simd_size: ?usize = null,
};

pub fn nullary(comptime options: NullaryOptions, out: anytype) void {
    const T = options.type;
    const size = core.sizeForSIMD(T, options.simd_size);

    const V0 = VectorFunction0(T, size);
    const F0 = struct {
        pub inline fn call(i: anytype) V0.ReturnType(@TypeOf(i)) {
            return nullWithIndexFn(V0.ReturnType(@TypeOf(i)), options.f, i);
        }
    };

    V0.callWithIndex(F0, out);
}

pub fn iota(out: []usize) void {
    nullary(.{ .type = usize, .f = .iota }, out);
}

test "iota" {
    const N = 100;

    var out = std.ArrayList(usize).init(testing.allocator);
    defer out.deinit();
    try out.resize(N);

    iota(out.items);

    for (out.items, 0..) |o, i| {
        try testing.expectEqual(i, o);
    }
}

/// Unary Function Definition
pub const UnaryFunction = enum {
    copy,
    sqrt,
    sin,
    cos,
    tan,
    exp,
    exp2,
    exp1m,
    log,
    log2,
    log10,
    log1p,
    abs,
    floor,
    ceil,
    trunc,
    round,
    byteSwap,
    bitReverse,
    countLeadingZeros,
    countTrailingZeros,
    popCount,
};

inline fn uniFn(comptime T: type, comptime f: UnaryFunction, a: anytype) T {
    return switch (f) {
        .copy => a,
        .sqrt => @sqrt(a),
        .sin => @sin(a),
        .cos => @cos(a),
        .tan => @tan(a),
        .exp => @exp(a),
        .exp2 => @exp2(a),
        .exp1m => @exp(a - (if (isSIMD(@TypeOf(a))) @as(@TypeOf(a), @splat(1)) else 1)),
        .log => @log(a),
        .log2 => @log2(a),
        .log10 => @log10(a),
        .log1p => @log(a + (if (isSIMD(@TypeOf(a))) @as(@TypeOf(a), @splat(1)) else 1)),
        .abs => @abs(a),
        .floor => @floor(a),
        .ceil => @ceil(a),
        .trunc => @trunc(a),
        .round => @round(a),
        .byteSwap => @byteSwap(a),
        .bitReverse => @bitReverse(a),
        .countLeadingZeros => @clz(a),
        .countTrailingZeros => @ctz(a),
        .popCount => @popCount(a),
    };
}

const UnaryOptions = struct {
    type: type,
    f: UnaryFunction,
    simd_size: ?usize = null,
};

/// Call unary function
///
/// * `arg` can be scalar (`T`) or vector (`[]T`)
/// * `out` must be vector (`[]T`)
pub fn unary(comptime options: UnaryOptions, arg: anytype, out: anytype) void {
    const T = options.type;
    const O = options.type;
    const size = core.sizeForSIMD(T, options.simd_size);

    const V1 = VectorFunction1(T, O, size);

    const Unary = struct {
        pub inline fn call(a: anytype) V1.ReturnType(@TypeOf(a)) {
            const OType = V1.ReturnType(@TypeOf(a));
            return uniFn(OType, options.f, a);
        }
    };

    V1.call(Unary, arg, out);
}

test "unary function" {
    const N = 10;

    const Test = struct {
        fn do(comptime opt: UnaryOptions, a: anytype, o: anytype) !void {
            var arg = std.ArrayList(opt.type).init(testing.allocator);
            defer arg.deinit();
            try arg.appendNTimes(a, N);

            var out = std.ArrayList(opt.type).init(testing.allocator);
            defer out.deinit();
            try out.resize(N);

            var true_out = std.ArrayList(opt.type).init(testing.allocator);
            defer true_out.deinit();
            try true_out.appendNTimes(o, N);

            unary(opt, arg.items, out.items);
            for (true_out.items, out.items) |ti, oi| {
                try testing.expectApproxEqRel(oi, ti, 1e-6);
            }
        }
    };

    try Test.do(.{ .type = f32, .f = .sqrt }, 4.0, 2.0);
    try Test.do(.{ .type = f32, .f = .sqrt, .simd_size = 0 }, 4.0, 2.0);
    try Test.do(.{ .type = f32, .f = .sin }, 1.7, @sin(1.7));
    try Test.do(.{ .type = f32, .f = .sin }, 1.7, @sin(1.7));
    try Test.do(.{ .type = f32, .f = .cos }, 1.7, @cos(1.7));
    try Test.do(.{ .type = f32, .f = .tan }, 1.7, @tan(1.7));
    try Test.do(.{ .type = f32, .f = .exp }, 0.89, @exp(0.89));
    try Test.do(.{ .type = f32, .f = .exp2 }, 0.89, @exp2(0.89));
    try Test.do(.{ .type = f32, .f = .exp1m }, 1.89, @exp(0.89));
    try Test.do(.{ .type = f32, .f = .log }, 2.7, @log(2.7));
    try Test.do(.{ .type = f32, .f = .log2 }, 2.7, @log2(2.7));
    try Test.do(.{ .type = f32, .f = .log10 }, 2.7, @log10(2.7));
    try Test.do(.{ .type = f32, .f = .log1p }, 0.01, @log(1.01));
    try Test.do(.{ .type = f32, .f = .abs }, -3.3, 3.3);
    try Test.do(.{ .type = f32, .f = .floor }, 2.7, 2.0);
    try Test.do(.{ .type = f32, .f = .ceil }, 2.7, 3.0);
    try Test.do(.{ .type = f32, .f = .trunc }, -2.7, -2.0);
    try Test.do(.{ .type = f32, .f = .round }, 2.7, 3.0);
}

pub fn copy(comptime T: type, arg: anytype, out: []T) void {
    unary(.{ .type = T, .f = .copy }, arg, out);
}

pub fn sqrt(comptime T: type, arg: anytype, out: []T) void {
    unary(.{ .type = T, .f = .sqrt }, arg, out);
}

pub fn sin(comptime T: type, arg: anytype, out: []T) void {
    unary(.{ .type = T, .f = .sin }, arg, out);
}

pub fn cos(comptime T: type, arg: anytype, out: []T) void {
    unary(.{ .type = T, .f = .cos }, arg, out);
}

pub fn tan(comptime T: type, arg: anytype, out: []T) void {
    unary(.{ .type = T, .f = .tan }, arg, out);
}

pub fn exp(comptime T: type, arg: anytype, out: []T) void {
    unary(.{ .type = T, .f = .exp }, arg, out);
}

pub fn exp2(comptime T: type, arg: anytype, out: []T) void {
    unary(.{ .type = T, .f = .exp2 }, arg, out);
}

pub fn exp1m(comptime T: type, arg: anytype, out: []T) void {
    unary(.{ .type = T, .f = .exp1m }, arg, out);
}

pub fn log(comptime T: type, arg: anytype, out: []T) void {
    unary(.{ .type = T, .f = .log }, arg, out);
}

pub fn log2(comptime T: type, arg: anytype, out: []T) void {
    unary(.{ .type = T, .f = .log2 }, arg, out);
}

pub fn log10(comptime T: type, arg: anytype, out: []T) void {
    unary(.{ .type = T, .f = .log10 }, arg, out);
}

pub fn log1p(comptime T: type, arg: anytype, out: []T) void {
    unary(.{ .type = T, .f = .log1p }, arg, out);
}

pub fn abs(comptime T: type, arg: anytype, out: []T) void {
    unary(.{ .type = T, .f = .abs }, arg, out);
}

pub fn floor(comptime T: type, arg: anytype, out: []T) void {
    unary(.{ .type = T, .f = .floor }, arg, out);
}

pub fn ceil(comptime T: type, arg: anytype, out: []T) void {
    unary(.{ .type = T, .f = .ceil }, arg, out);
}

pub fn trunc(comptime T: type, arg: anytype, out: []T) void {
    unary(.{ .type = T, .f = .trunc }, arg, out);
}

pub fn round(comptime T: type, arg: anytype, out: []T) void {
    unary(.{ .type = T, .f = .round }, arg, out);
}

pub fn byteSwap(comptime T: type, arg: anytype, out: []T) void {
    unary(.{ .type = T, .f = .byteSwap }, arg, out);
}

pub fn bitReverse(comptime T: type, arg: anytype, out: []T) void {
    unary(.{ .type = T, .f = .bitReverse }, arg, out);
}

pub fn countLeadingZeros(comptime T: type, arg: anytype, out: []T) void {
    unary(.{ .type = T, .f = .countLeadingZeros }, arg, out);
}

pub fn countTrailingZeros(comptime T: type, arg: anytype, out: []T) void {
    unary(.{ .type = T, .f = .countTrailingZeros }, arg, out);
}

pub fn popCount(comptime T: type, arg: anytype, out: []T) void {
    unary(.{ .type = T, .f = .popCount }, arg, out);
}

test "explicit unary" {
    const N = 100;

    const Test = struct {
        fn do(comptime T: type, f: anytype, a: anytype, o: anytype) !void {
            var arg = std.ArrayList(T).init(testing.allocator);
            defer arg.deinit();
            try arg.appendNTimes(a, N);

            var out = std.ArrayList(T).init(testing.allocator);
            defer out.deinit();
            try out.resize(N);

            var true_out = std.ArrayList(T).init(testing.allocator);
            defer true_out.deinit();
            try true_out.appendNTimes(o, N);

            f(T, arg.items, out.items);

            switch (@TypeOf(a)) {
                comptime_float => {
                    for (true_out.items, out.items) |ti, oi| {
                        try testing.expectApproxEqRel(ti, oi, 1e-6);
                    }
                },
                comptime_int => {
                    try testing.expectEqualSlices(T, true_out.items, out.items);
                },
                else => @compileError(""),
            }
        }
    };

    try Test.do(f32, copy, 3.2, 3.2);
    try Test.do(f32, sqrt, 4.0, 2.0);
    try Test.do(f32, sin, 1.7, @sin(1.7));
    try Test.do(f32, cos, 1.7, @cos(1.7));
    try Test.do(f32, tan, 1.7, @tan(1.7));
    try Test.do(f32, exp, 0.89, @exp(0.89));
    try Test.do(f32, exp2, 0.89, @exp2(0.89));
    try Test.do(f32, exp1m, 1.89, @exp(0.89));
    try Test.do(f32, log, 2.7, @log(2.7));
    try Test.do(f32, log2, 2.7, @log2(2.7));
    try Test.do(f32, log10, 2.7, @log10(2.7));
    try Test.do(f32, log1p, 0.01, @log(1.01));
    try Test.do(f32, abs, -3.3, 3.3);
    try Test.do(f32, floor, 2.7, 2.0);
    try Test.do(f32, ceil, 2.7, 3.0);
    try Test.do(f32, trunc, -2.7, -2.0);
    try Test.do(f32, round, 2.7, 3.0);
    try Test.do(u16, byteSwap, 1, 256);
    try Test.do(u8, bitReverse, 182, 109);
    try Test.do(u8, bitReverse, 128, 1);
    try Test.do(u8, countLeadingZeros, 32, 2);
    try Test.do(u8, countTrailingZeros, 32, 5);
    try Test.do(u8, popCount, 31, 5);
}

/// Binary Function Definitions
pub const BinaryFunction = enum {
    add,
    wrap_add,
    sat_add,
    sub,
    wrap_sub,
    sat_sub,
    mul,
    wrap_mul,
    sat_mul,
    div,
    divFloor,
    divTrunc,
    rem,
    mod,
    bit_and,
    bit_or,
    bit_xor,
    eq,
    neq,
    gt,
    gte,
    lt,
    lte,
    min,
    max,
};

inline fn biFn(comptime T: type, comptime f: BinaryFunction, a: anytype, b: anytype) T {
    return switch (f) {
        .add => a + b,
        .wrap_add => a +% b,
        .sat_add => a +| b,
        .sub => a - b,
        .wrap_sub => a -% b,
        .sat_sub => a -| b,
        .mul => a * b,
        .wrap_mul => a *% b,
        .sat_mul => a *| b,
        .div => a / b,
        .divFloor => @divFloor(a, b),
        .divTrunc => @divTrunc(a, b),
        .rem => a % b,
        .mod => @mod(a, b),
        .bit_and => a & b,
        .bit_or => a | b,
        .bit_xor => a ^ b,
        .eq => a == b,
        .neq => a != b,
        .gt => a > b,
        .gte => a >= b,
        .lt => a < b,
        .lte => a <= b,
        .min => @min(a, b),
        .max => @max(a, b),
    };
}

/// Options for `binary` function
const BinaryOptions = struct {
    /// Input Type
    type: type,

    /// Binary Operation Selector
    f: BinaryFunction,

    /// SIMD Vector Size
    ///
    /// If `null` (default), suggested size is used.
    /// By setting this to `0`, SIMD is disabled.
    simd_size: ?usize = null,
};

/// Call binary function
///
/// * `arg1`, `arg2` can be scalar (`T`) or vector (`[]T`)
/// * `out` must be vector (`[]T`)
pub fn binary(comptime options: BinaryOptions, arg1: anytype, arg2: anytype, out: anytype) void {
    const T = options.type;
    const O: type = switch (options.f) {
        .eq, .neq, .gt, .gte, .lt, .lte => bool,
        else => T,
    };
    const size = core.sizeForSIMD(T, options.simd_size);
    const V2 = VectorFunction2(T, T, O, size);

    const Binary = struct {
        pub inline fn call(a1: anytype, a2: anytype) V2.ReturnType(@TypeOf(a1, a2)) {
            const OType = V2.ReturnType(@TypeOf(a1));
            return biFn(OType, options.f, a1, a2);
        }
    };

    V2.call(Binary, arg1, arg2, out);
}

test "binary" {
    const N = 100;

    const Test = struct {
        fn do(comptime opt: BinaryOptions, lhs: anytype, rhs: anytype, out: anytype) !void {
            var a = std.ArrayList(opt.type).init(testing.allocator);
            defer a.deinit();
            try a.appendNTimes(lhs, N);

            var b = std.ArrayList(opt.type).init(testing.allocator);
            defer b.deinit();
            try b.appendNTimes(rhs, N);

            const O = switch (opt.f) {
                .eq, .neq, .gt, .gte, .lt, .lte => bool,
                else => opt.type,
            };

            var o = std.ArrayList(O).init(testing.allocator);
            defer o.deinit();
            try o.resize(N);

            var t = std.ArrayList(O).init(testing.allocator);
            defer t.deinit();
            try t.appendNTimes(out, N);

            // Vector-Vector
            binary(opt, a.items, b.items, o.items);
            try testing.expectEqualSlices(O, t.items, o.items);

            o.clearRetainingCapacity();
            try o.resize(N);

            // Scalar-Vector
            binary(opt, lhs, b.items, o.items);
            try testing.expectEqualSlices(O, t.items, o.items);

            o.clearRetainingCapacity();
            try o.resize(N);

            // Vector-Scalar
            binary(opt, a.items, rhs, o.items);
            try testing.expectEqualSlices(O, t.items, o.items);
        }
    };

    try Test.do(.{ .type = f32, .f = .add }, 4.0, 2.0, 6.0);
    try Test.do(.{ .type = f32, .f = .add, .simd_size = 0 }, 4.0, 2.0, 6.0);
    try Test.do(.{ .type = u16, .f = .eq }, 2, 4, false);
    try Test.do(.{ .type = u16, .f = .eq, .simd_size = 0 }, 2, 4, false);
}

pub fn add(comptime T: type, arg1: anytype, arg2: anytype, out: []T) void {
    binary(.{ .type = T, .f = .add }, arg1, arg2, out);
}

pub fn wrapAdd(comptime T: type, arg1: anytype, arg2: anytype, out: []T) void {
    binary(.{ .type = T, .f = .wrap_add }, arg1, arg2, out);
}

pub fn saturateAdd(comptime T: type, arg1: anytype, arg2: anytype, out: []T) void {
    binary(.{ .type = T, .f = .sat_add }, arg1, arg2, out);
}

pub fn sub(comptime T: type, arg1: anytype, arg2: anytype, out: []T) void {
    binary(.{ .type = T, .f = .sub }, arg1, arg2, out);
}

pub fn wrapSub(comptime T: type, arg1: anytype, arg2: anytype, out: []T) void {
    binary(.{ .type = T, .f = .wrap_sub }, arg1, arg2, out);
}

pub fn saturateSub(comptime T: type, arg1: anytype, arg2: anytype, out: []T) void {
    binary(.{ .type = T, .f = .sat_sub }, arg1, arg2, out);
}

pub fn mul(comptime T: type, arg1: anytype, arg2: anytype, out: []T) void {
    binary(.{ .type = T, .f = .mul }, arg1, arg2, out);
}

pub fn wrapMul(comptime T: type, arg1: anytype, arg2: anytype, out: []T) void {
    binary(.{ .type = T, .f = .wrap_mul }, arg1, arg2, out);
}

pub fn saturateMul(comptime T: type, arg1: anytype, arg2: anytype, out: []T) void {
    binary(.{ .type = T, .f = .sat_mul }, arg1, arg2, out);
}

pub fn div(comptime T: type, arg1: anytype, arg2: anytype, out: []T) void {
    binary(.{ .type = T, .f = .div }, arg1, arg2, out);
}

pub fn divFloor(comptime T: type, arg1: anytype, arg2: anytype, out: []T) void {
    binary(.{ .type = T, .f = .divFloor }, arg1, arg2, out);
}

pub fn divTrunc(comptime T: type, arg1: anytype, arg2: anytype, out: []T) void {
    binary(.{ .type = T, .f = .divTrunc }, arg1, arg2, out);
}

pub fn rem(comptime T: type, arg1: anytype, arg2: anytype, out: []T) void {
    binary(.{ .type = T, .f = .rem }, arg1, arg2, out);
}

pub fn mod(comptime T: type, arg1: anytype, arg2: anytype, out: []T) void {
    binary(.{ .type = T, .f = .mod }, arg1, arg2, out);
}

pub fn bitAnd(comptime T: type, arg1: anytype, arg2: anytype, out: []T) void {
    binary(.{ .type = T, .f = .bit_and }, arg1, arg2, out);
}

pub fn bitOr(comptime T: type, arg1: anytype, arg2: anytype, out: []T) void {
    binary(.{ .type = T, .f = .bit_or }, arg1, arg2, out);
}

pub fn bitXor(comptime T: type, arg1: anytype, arg2: anytype, out: []T) void {
    binary(.{ .type = T, .f = .bit_xor }, arg1, arg2, out);
}

pub fn eq(comptime T: type, arg1: anytype, arg2: anytype, out: []bool) void {
    binary(.{ .type = T, .f = .eq }, arg1, arg2, out);
}

pub fn neq(comptime T: type, arg1: anytype, arg2: anytype, out: []bool) void {
    binary(.{ .type = T, .f = .neq }, arg1, arg2, out);
}

pub fn gt(comptime T: type, arg1: anytype, arg2: anytype, out: []bool) void {
    binary(.{ .type = T, .f = .gt }, arg1, arg2, out);
}

pub fn gte(comptime T: type, arg1: anytype, arg2: anytype, out: []bool) void {
    binary(.{ .type = T, .f = .gte }, arg1, arg2, out);
}

pub fn lt(comptime T: type, arg1: anytype, arg2: anytype, out: []bool) void {
    binary(.{ .type = T, .f = .lt }, arg1, arg2, out);
}

pub fn lte(comptime T: type, arg1: anytype, arg2: anytype, out: []bool) void {
    binary(.{ .type = T, .f = .lte }, arg1, arg2, out);
}

pub fn min(comptime T: type, arg1: anytype, arg2: anytype, out: []T) void {
    binary(.{ .type = T, .f = .min }, arg1, arg2, out);
}

pub fn max(comptime T: type, arg1: anytype, arg2: anytype, out: []T) void {
    binary(.{ .type = T, .f = .max }, arg1, arg2, out);
}

test "explicit binary" {
    const N = 100;

    const Test = struct {
        fn do(comptime T: type, comptime f: anytype, a1: anytype, a2: anytype, o: anytype) !void {
            var arg1 = std.ArrayList(T).init(testing.allocator);
            defer arg1.deinit();
            try arg1.appendNTimes(a1, N);

            var arg2 = std.ArrayList(T).init(testing.allocator);
            defer arg2.deinit();
            try arg2.appendNTimes(a2, N);

            const O = if (@TypeOf(o) == bool) bool else T;

            var out = std.ArrayList(O).init(testing.allocator);
            defer out.deinit();
            try out.resize(N);

            var true_out = std.ArrayList(O).init(testing.allocator);
            defer true_out.deinit();
            try true_out.appendNTimes(o, N);

            f(T, arg1.items, arg2.items, out.items);

            try testing.expectEqualSlices(O, true_out.items, out.items);
        }
    };

    try Test.do(u8, add, 15, 27, 42);
    try Test.do(u8, wrapAdd, 200, 100, 44);
    try Test.do(u8, saturateAdd, 200, 100, 255);
    try Test.do(u8, sub, 27, 15, 12);
    try Test.do(u8, wrapSub, 50, 100, 206);
    try Test.do(u8, saturateSub, 10, 100, 0);
    try Test.do(u8, mul, 5, 7, 35);
    try Test.do(u8, wrapMul, 10, 26, 4);
    try Test.do(u8, saturateMul, 200, 100, 255);
    try Test.do(u8, div, 15, 3, 5);
    try Test.do(u8, divFloor, 15, 4, 3);
    try Test.do(i8, divTrunc, -15, 4, -3);
    try Test.do(u8, rem, 8, 3, 2);
    try Test.do(i8, mod, -8, 3, 1);
    try Test.do(u8, bitAnd, 8, 3, 0);
    try Test.do(u8, bitOr, 8, 3, 11);
    try Test.do(u8, bitXor, 8, 3, 11);
    try Test.do(u8, eq, 8, 3, false);
    try Test.do(u8, neq, 8, 3, true);
    try Test.do(u8, gt, 8, 3, true);
    try Test.do(u8, gte, 8, 3, true);
    try Test.do(u8, lt, 8, 3, false);
    try Test.do(u8, lte, 8, 3, false);
    try Test.do(u8, min, 4, 3, 3);
    try Test.do(u8, max, 4, 3, 4);
}

pub fn gather(comptime options: Options, arg: []const options.type, index: []const usize, out: []options.type) void {
    // TODO: Try to use SIMD
    for (index, out) |i, *o| {
        o.* = arg[i];
    }
}

test "gather" {
    const Test = struct {
        fn do(comptime opt: Options, N: usize) !void {
            var a = std.ArrayList(opt.type).init(testing.allocator);
            defer a.deinit();
            try a.appendNTimes(2.3, N);

            var i = std.ArrayList(usize).init(testing.allocator);
            defer i.deinit();
            try i.resize(N);

            iota(i.items);

            var o = std.ArrayList(opt.type).init(testing.allocator);
            defer o.deinit();
            try o.resize(N);

            gather(opt, a.items, i.items, o.items);
        }
    };

    try Test.do(.{ .type = f32 }, 12);
}

pub const TernaryFunction = enum {
    mulAdd,
    clip,
};

inline fn triFn(comptime T: type, comptime f: TernaryFunction, a1: anytype, a2: anytype, a3: anytype) T {
    return switch (f) {
        .mulAdd => @mulAdd(T, a1, a2, a3),
        .clip => @max(a2, @min(a1, a3)),
    };
}

const TernaryOptions = struct {
    type: type,
    f: TernaryFunction,
    simd_size: ?usize = null,
};

pub fn ternary(comptime options: TernaryOptions, arg1: anytype, arg2: anytype, arg3: anytype, out: anytype) void {
    const T = options.type;
    const size = core.sizeForSIMD(T, options.simd_size);

    const V3 = VectorFunction3(T, T, T, T, size);
    const F3 = struct {
        pub inline fn call(a1: anytype, a2: anytype, a3: anytype) V3.ReturnType(@TypeOf(a1)) {
            return triFn(V3.ReturnType(@TypeOf(a1)), options.f, a1, a2, a3);
        }
    };

    V3.call(F3, arg1, arg2, arg3, out);
}

test "ternary" {
    const N = 100;

    const Test = struct {
        fn do(comptime opt: TernaryOptions, a: anytype, b: anytype, c: anytype, o: anytype) !void {
            var a1 = std.ArrayList(opt.type).init(testing.allocator);
            defer a1.deinit();
            try a1.appendNTimes(a, N);

            var a2 = std.ArrayList(opt.type).init(testing.allocator);
            defer a2.deinit();
            try a2.appendNTimes(b, N);

            var a3 = std.ArrayList(opt.type).init(testing.allocator);
            defer a3.deinit();
            try a3.appendNTimes(c, N);

            var true_out = std.ArrayList(opt.type).init(testing.allocator);
            defer true_out.deinit();
            try true_out.appendNTimes(o, N);

            var out = std.ArrayList(opt.type).init(testing.allocator);
            defer out.deinit();
            try out.resize(N);

            ternary(opt, a1.items, a2.items, a3.items, out.items);
            for (true_out.items, out.items) |ti, oi| {
                try testing.expectApproxEqRel(ti, oi, 1e-6);
            }

            out.clearRetainingCapacity();
            try out.resize(N);

            ternary(opt, a, a2.items, a3.items, out.items);
            for (true_out.items, out.items) |ti, oi| {
                try testing.expectApproxEqRel(ti, oi, 1e-6);
            }

            out.clearRetainingCapacity();
            try out.resize(N);

            ternary(opt, a1.items, b, a3.items, out.items);
            for (true_out.items, out.items) |ti, oi| {
                try testing.expectApproxEqRel(ti, oi, 1e-6);
            }

            out.clearRetainingCapacity();
            try out.resize(N);

            ternary(opt, a1.items, a2.items, c, out.items);
            for (true_out.items, out.items) |ti, oi| {
                try testing.expectApproxEqRel(ti, oi, 1e-6);
            }

            out.clearRetainingCapacity();
            try out.resize(N);
        }
    };

    try Test.do(.{ .type = f32, .f = .mulAdd }, 3.2, 2.8, 1.2, 3.2 * 2.8 + 1.2);
    try Test.do(.{ .type = f32, .f = .mulAdd, .simd_size = 0 }, 3.2, 2.8, 1.2, 3.2 * 2.8 + 1.2);
    try Test.do(.{ .type = f32, .f = .clip }, 1.3, 0.2, 1.0, 1.0);
    try Test.do(.{ .type = f32, .f = .clip }, 1.3, 1.5, 2.0, 1.5);
}

pub fn mulAdd(comptime T: type, arg1: anytype, arg2: anytype, arg3: anytype, out: []T) void {
    ternary(.{ .type = T, .f = .mulAdd }, arg1, arg2, arg3, out);
}

pub fn clip(comptime T: type, arg1: anytype, arg2: anytype, arg3: anytype, out: []T) void {
    ternary(.{ .type = T, .f = .clip }, arg1, arg2, arg3, out);
}

test "explicit ternary" {
    const N = 10;

    const Test = struct {
        fn do(comptime T: type, f: anytype, a: anytype, b: anytype, c: anytype, o: anytype) !void {
            var a1 = std.ArrayList(T).init(testing.allocator);
            defer a1.deinit();
            try a1.appendNTimes(a, N);

            var a2 = std.ArrayList(T).init(testing.allocator);
            defer a2.deinit();
            try a2.appendNTimes(b, N);

            var a3 = std.ArrayList(T).init(testing.allocator);
            defer a3.deinit();
            try a3.appendNTimes(c, N);

            var true_out = std.ArrayList(T).init(testing.allocator);
            defer true_out.deinit();
            try true_out.appendNTimes(o, N);

            var out = std.ArrayList(T).init(testing.allocator);
            defer out.deinit();
            try out.resize(N);

            f(T, a1.items, a2.items, a3.items, out.items);
            for (true_out.items, out.items) |ti, oi| {
                try testing.expectApproxEqRel(ti, oi, 1e-6);
            }

            out.clearRetainingCapacity();
            try out.resize(N);

            f(T, a, a2.items, a3.items, out.items);
            for (true_out.items, out.items) |ti, oi| {
                try testing.expectApproxEqRel(ti, oi, 1e-6);
            }

            out.clearRetainingCapacity();
            try out.resize(N);

            f(T, a1.items, b, a3.items, out.items);
            for (true_out.items, out.items) |ti, oi| {
                try testing.expectApproxEqRel(ti, oi, 1e-6);
            }

            out.clearRetainingCapacity();
            try out.resize(N);

            f(T, a1.items, a2.items, c, out.items);
            for (true_out.items, out.items) |ti, oi| {
                try testing.expectApproxEqRel(ti, oi, 1e-6);
            }
        }
    };

    try Test.do(f32, mulAdd, 3.2, 1.5, 4.3, 3.2 * 1.5 + 4.3);
    try Test.do(f32, clip, 1.5, 1.0, 1.3, 1.3);
}

pub const ReductionFunction = enum {
    sum,
    wrap_sum,
    sat_sum,
    prod,
    wrap_prod,
    sat_prod,
    smallest,
    largest,
    all,
    any,
};

inline fn redFn(comptime f: ReductionFunction, a: anytype, b: @TypeOf(a)) @TypeOf(a) {
    return switch (f) {
        .sum => a + b,
        .wrap_sum => a +% b,
        .sat_sum => a +| b,
        .prod => a * b,
        .wrap_prod => a *% b,
        .sat_prod => a *| b,
        .smallest => @min(a, b),
        .largest => @max(a, b),
        .all => if (isSIMD(@TypeOf(a))) @select(bool, a, b, @as(@TypeOf(a), @splat(false))) else (a and b),
        .any => if (isSIMD(@TypeOf(a))) @select(bool, a, @as(@TypeOf(a), @splat(true)), b) else (a or b),
    };
}

const ReductionOptions = struct {
    type: type,
    f: ReductionFunction,
    simd_size: ?usize = null,
};

pub fn reduce(comptime options: ReductionOptions, arg: []const options.type) options.type {
    const size = core.sizeForSIMD(options.type, options.simd_size);
    const VR = VectorReductionFunction(options.type, size);

    const FR = struct {
        pub inline fn call(a: anytype, b: @TypeOf(a)) @TypeOf(a) {
            return redFn(options.f, a, b);
        }
    };

    return VR.call(FR, arg);
}

test "Reduce" {
    const N = 100;

    const Test = struct {
        fn do(comptime opt: ReductionOptions, a: anytype, r: anytype) !void {
            var arg = std.ArrayList(opt.type).init(testing.allocator);
            defer arg.deinit();
            try arg.appendNTimes(a, N);

            const result = reduce(opt, arg.items);
            try testing.expectEqual(r, result);
        }
    };

    try Test.do(.{ .type = u8, .f = .sum }, 2, 200);
    try Test.do(.{ .type = u8, .f = .sum, .simd_size = 0 }, 2, 200);
    try Test.do(.{ .type = u7, .f = .wrap_sum }, 2, 72);
    try Test.do(.{ .type = u4, .f = .sat_sum }, 3, 15);
    try Test.do(.{ .type = u8, .f = .prod }, 1, 1);
    try Test.do(.{ .type = u8, .f = .wrap_prod }, 2, 0);
    try Test.do(.{ .type = u4, .f = .sat_prod }, 3, 15);
}

pub fn sum(comptime T: type, arg: []const T) T {
    return reduce(.{ .type = T, .f = .sum }, arg);
}

pub fn wrapSum(comptime T: type, arg: []const T) T {
    return reduce(.{ .type = T, .f = .wrap_sum }, arg);
}

pub fn saturateSum(comptime T: type, arg: []const T) T {
    return reduce(.{ .type = T, .f = .sat_sum }, arg);
}

pub fn prod(comptime T: type, arg: []const T) T {
    return reduce(.{ .type = T, .f = .prod }, arg);
}

pub fn wrapProd(comptime T: type, arg: []const T) T {
    return reduce(.{ .type = T, .f = .wrap_prod }, arg);
}

pub fn saturateProd(comptime T: type, arg: []const T) T {
    return reduce(.{ .type = T, .f = .sat_prod }, arg);
}

test "explicit reduce function" {
    const N = 100;

    const Test = struct {
        fn do(comptime T: type, f: anytype, a: anytype, r: anytype) !void {
            var arg = std.ArrayList(T).init(testing.allocator);
            defer arg.deinit();
            try arg.appendNTimes(a, N);

            const result = f(T, arg.items);
            try testing.expectEqual(r, result);
        }
    };

    try Test.do(u8, sum, 2, 200);
    try Test.do(u7, wrapSum, 2, 72);
    try Test.do(u4, saturateSum, 3, 15);
    try Test.do(u8, prod, 1, 1);
    try Test.do(u8, wrapProd, 2, 0);
    try Test.do(u4, saturateProd, 3, 15);
}

pub fn smallest(comptime T: type, arg: []const T) T {
    return reduce(.{ .type = T, .f = .smallest }, arg);
}

pub fn largest(comptime T: type, arg: []const T) T {
    return reduce(.{ .type = T, .f = .largest }, arg);
}

test "smallest/largest" {
    const a = [_]u32{ 32, 4, 6, 95, 20 };

    try testing.expectEqual(4, smallest(u32, &a));
    try testing.expectEqual(95, largest(u32, &a));
}

pub fn all(arg: []const bool) bool {
    return reduce(.{ .type = bool, .f = .all }, arg);
}

pub fn any(arg: []const bool) bool {
    return reduce(.{ .type = bool, .f = .any }, arg);
}

test "all/any" {
    const a = [_]bool{ true, false, false, false, false, true, false, true, false };
    const b = [_]bool{ true, true, true };
    const c = [_]bool{ false, false };

    try testing.expect(!all(&a));
    try testing.expect(any(&a));
    try testing.expect(all(&b));
    try testing.expect(any(&b));
    try testing.expect(!all(&c));
    try testing.expect(!any(&c));
}
