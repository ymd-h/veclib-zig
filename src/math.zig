const std = @import("std");
const testing = std.testing;

const compat = @import("./compat.zig");
const core = @import("./core.zig");

const VectorFunction0 = core.VectorFunction0;
const VectorFunction1 = core.VectorFunction1;
const VectorFunction2 = core.VectorFunction2;

const isSIMD = core.isSIMDVector;


/// Fill Options
const FillOptions = struct {
    /// SIMD Vector Size
    simd_size: ?usize = null,
};

/// Fill values to Vector
pub fn fill(comptime options: FillOptions, value: anytype, out: anytype) void {
    const T = core.ElementType(@TypeOf(out));
    const size = options.simd_size orelse (std.simd.suggestVectorLength(T) orelse 0);
    const V0 = VectorFunction0(T, size);

    const Fill = struct {
        inline fn call() T {
            return value;
        }
    };

    V0.call(Fill.call, out);
}

test "fill" {
    const N = 10;

    const Test = struct {
        fn do(comptime o: FillOptions) !void {
            var out = std.ArrayList(f16).init(testing.allocator);
            defer out.deinit();
            try out.resize(N);

            var true_out = std.ArrayList(f16).init(testing.allocator);
            defer true_out.deinit();

            fill(o, 1.5, out.items);
            try true_out.appendNTimes(1.5, N);
            try testing.expectEqualSlices(f16, true_out.items, out.items);
        }
    };

    // With SIMD
    try Test.do(.{});

    // Without SIMD
    try Test.do(.{ .simd_size = 0 });
}

pub const UnaryFunction = enum {
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
};

inline fn uniFn(comptime T: type, comptime f: UnaryFunction, a: anytype) T {
    return switch (f) {
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
    };
}

const UnaryOptions = struct {
    type: type,
    f: UnaryFunction,
    simd_size: ?usize = null,
};

pub fn unary(comptime options: UnaryOptions, arg: anytype, out: anytype) void {
    const T = options.type;
    const O = options.type;
    const size = options.simd_size orelse (std.simd.suggestVectorLength(T) orelse 0);

    const V1 = VectorFunction1(T, O, size);

    const Unary = struct {
        inline fn call(a: anytype) V1.ReturnType(@TypeOf(a)) {
            const OType = V1.ReturnType(@TypeOf(a));
            return uniFn(OType, options.f, a);
        }
    };

    V1.call(Unary.call, arg, out);
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
    rem,
    bit_and,
    bit_or,
    bit_xor,
    eq,
    neq,
    gt,
    gte,
    lt,
    lte,
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
        .rem => a % b,
        .bit_and => a & b,
        .bit_or => a | b,
        .bit_xor => a ^ b,
        .eq => a == b,
        .neq => a != b,
        .gt => a > b,
        .gte => a >= b,
        .lt => a < b,
        .lte => a <= b,
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
    const size = options.simd_size orelse (std.simd.suggestVectorLength(T) orelse 0);
    const V2 = VectorFunction2(T, T, O, size);

    const Binary = struct {
        inline fn call(a1: anytype, a2: anytype) V2.ReturnType(@TypeOf(a1, a2)) {
            const OType = V2.ReturnType(@TypeOf(a1));
            return biFn(OType, options.f, a1, a2);
        }
    };

    V2.call(Binary.call, arg1, arg2, out);
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
