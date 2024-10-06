const std = @import("std");
const testing = std.testing;

const compat = @import("./compat.zig");

pub const BinaryOp = enum {
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

inline fn bop(comptime T: type, comptime op: BinaryOp, a: anytype, b: anytype) T {
    return switch (op) {
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

const ScalarOrVector = enum {
    scalar,
    vector,

    const Self = @This();

    inline fn which(comptime T: type, comptime V: type) Self {
        comptime {
            const S = []T;
            const cS = []const T;

            return switch (V) {
                S, cS => .vector,
                T, comptime_int, comptime_float => .scalar,
                else => blk: {
                    switch (@typeInfo(V)) {
                        compat.array => |a| {
                            // Array
                            if (a.child == T) {
                                break :blk .vector;
                            }
                        },
                        compat.pointer => |p| {
                            // Pointer to Array
                            switch (@typeInfo(p.child)) {
                                compat.array => |a| {
                                    if (a.child == T) {
                                        break :blk .vector;
                                    }
                                },
                                else => {},
                            }
                        },
                        else => {},
                    }

                    @compileLog("Compile Error: `value` must be Scalar (aka. T, comptime_int, or comptime_float) or Vector (aka. []T, []const T), but {T, @TypeOf(value)} = ", T, V);
                    unreachable;
                },
            };
        }
    }
};

test "Scalar or Vector" {
    const a: f16 = 0.2;
    const b = [_]f16{ 1.2, 0.4 };

    try testing.expectEqual(.scalar, ScalarOrVector.which(f16, @TypeOf(a)));
    try testing.expectEqual(.vector, ScalarOrVector.which(f16, @TypeOf(b[0..])));
}

inline fn isSIMDVector(comptime T: type) bool {
    comptime {
        return switch (@typeInfo(T)) {
            compat.vector => true,
            else => false,
        };
    }
}

inline fn ElementType(comptime T: type) type {
    comptime {
        switch (@typeInfo(T)) {
            compat.array => |a| {
                return a.child;
            },
            compat.pointer => |p| {
                if (p.size == .Slice) {
                    return p.child;
                }

                switch (@typeInfo(p.child)) {
                    compat.array => |a| {
                        return a.child;
                    },
                    else => {},
                }
            },
            else => {},
        }

        @compileLog("T is not array-like: ", T, @typeInfo(T));
        unreachable;
    }
}

fn validateOut(comptime O: type, comptime OType: type) void {
    const sv = ScalarOrVector.which(O, OType);

    if (sv == .scalar) {
        @compileLog("`out` must be Vector but Scalar: {O, OType} = ", O, OType);
        unreachable;
    }
}

test "Validate Out" {
    const a = [_]u32{ 0, 2, 3 };
    validateOut(u32, @TypeOf(a[0..]));
}

pub fn VectorFunction0(comptime O: type, comptime vec_size: usize) type {
    return struct {
        const Self = @This();
        const VecO = @Vector(vec_size, O);

        inline fn forLoop(o: O, out: anytype) void {
            for (out) |*oi| {
                oi.* = o;
            }
        }

        pub fn call(comptime f: anytype, out: anytype) void {
            validateOut(O, @TypeOf(out));

            const o = f();
            if (vec_size < 2) {
                return Self.forLoop(o, out);
            }

            const rem = @mod(out.len, vec_size);
            if (rem > 0) {
                Self.forLoop(o, out[0..rem]);
            }

            var i = rem;
            const ov: VecO = @splat(o);
            while (i < out.len) : (i += vec_size) {
                out[i..][0..vec_size].* = ov;
            }
        }
    };
}

test "Vector Function 0" {
    const N = 1_000_000;

    var out = std.ArrayList(f16).init(testing.allocator);
    defer out.deinit();
    try out.resize(N);

    const Fill = struct {
        inline fn call() f16 {
            return 3.2;
        }
    };

    var true_out = std.ArrayList(f16).init(testing.allocator);
    defer true_out.deinit();
    try true_out.appendNTimes(3.2, N);

    const vec_size = std.simd.suggestVectorLength(f16) orelse 0;
    std.debug.print("Vector Size for f16: {}\n", .{vec_size});

    // With SIMD
    VectorFunction0(f16, vec_size).call(Fill.call, out.items);
    try testing.expectEqualSlices(f16, true_out.items, out.items);

    out.clearRetainingCapacity();
    try out.resize(N);

    // Without SIMD
    VectorFunction0(f16, 0).call(Fill.call, out.items);
    try testing.expectEqualSlices(f16, true_out.items, out.items);
}

pub fn VectorFunction1(comptime T1: type, comptime O: type, comptime vec_size: usize) type {
    return struct {
        const Self = @This();
        const Vec1 = @Vector(vec_size, T1);
        const VecO = @Vector(vec_size, O);

        pub fn ReturnType(comptime T: type) type {
            comptime {
                return if (isSIMDVector(T)) VecO else O;
            }
        }

        inline fn forLoop(comptime f: anytype, arg1: []const T1, out: anytype) void {
            for (arg1, out) |a1_i, *oi| {
                oi.* = f(a1_i);
            }
        }

        pub fn call(comptime f: anytype, arg1: anytype, out: anytype) void {
            validateOut(O, @TypeOf(out));

            switch (ScalarOrVector.which(T1, @TypeOf(arg1))) {
                .scalar => {
                    const F0 = struct {
                        inline fn f0() O {
                            return f(arg1);
                        }
                    };
                    VectorFunction0(O, vec_size).call(F0.f0, out);
                },
                .vector => {
                    if (vec_size < 2) {
                        return Self.forLoop(f, arg1, out);
                    }

                    const rem = @mod(out.len, vec_size);
                    if (rem > 0) {
                        Self.forLoop(f, arg1[0..rem], out[0..rem]);
                    }

                    var i = rem;
                    while (i < out.len) : (i += vec_size) {
                        const a1_v: Vec1 = arg1[i..][0..vec_size].*;
                        out[i..][0..vec_size].* = f(a1_v);
                    }
                },
            }
        }
    };
}

test "Vector Function 1" {
    const N = 10;

    const vec_size = std.simd.suggestVectorLength(u32) orelse 0;
    std.debug.print("Vector Size for u32: {}\n", .{vec_size});

    const TestV1 = struct {
        fn do_test(comptime V1: type) !void {
            const TwoTimes = struct {
                inline fn call(v: anytype) V1.ReturnType(@TypeOf(v)) {
                    return v + v;
                }
            };

            var a = std.ArrayList(u32).init(testing.allocator);
            defer a.deinit();
            try a.appendNTimes(4, N);
            a.items[3] = 2;

            var true_out = std.ArrayList(u32).init(testing.allocator);
            defer true_out.deinit();

            try true_out.appendNTimes(8, N);
            true_out.items[3] = 4;

            var out = std.ArrayList(u32).init(testing.allocator);
            defer out.deinit();
            try out.resize(N);

            // Vector
            V1.call(TwoTimes.call, a.items, out.items);
            try testing.expectEqualSlices(u32, true_out.items, out.items);

            out.clearRetainingCapacity();
            try out.resize(N);

            true_out.clearRetainingCapacity();
            try true_out.appendNTimes(6, N);

            // Scalar
            V1.call(TwoTimes.call, 3, out.items);
            try testing.expectEqualSlices(u32, true_out.items, out.items);
        }
    };

    // With SIMD
    try TestV1.do_test(VectorFunction1(u32, u32, vec_size));

    // Without SIMD
    try TestV1.do_test(VectorFunction1(u32, u32, 0));
}

pub fn VectorFunction2(comptime T1: type, comptime T2: type, comptime O: type, comptime vec_size: usize) type {
    return struct {
        const Self = @This();
        const Vec1 = @Vector(vec_size, T1);
        const Vec2 = @Vector(vec_size, T2);
        const VecO = @Vector(vec_size, O);

        pub fn ReturnType(comptime T: type) type {
            comptime {
                return if (isSIMDVector(T)) VecO else O;
            }
        }

        fn forLoop(comptime f: anytype, arg1: []const T1, arg2: []const T2, out: anytype) void {
            for (arg1, arg2, out) |a1_i, a2_i, *oi| {
                oi.* = f(a1_i, a2_i);
            }
        }

        pub fn call(comptime f: anytype, arg1: anytype, arg2: anytype, out: anytype) void {
            validateOut(O, @TypeOf(out));

            if (ScalarOrVector.which(T1, @TypeOf(arg1)) == .scalar) {
                const V1 = VectorFunction1(T2, O, vec_size);

                const F1 = struct {
                    inline fn call(v: anytype) V1.ReturnType(@TypeOf(v)) {
                        const isSIMD = isSIMDVector(@TypeOf(v));
                        if (isSIMD) {
                            return f(@as(Vec1, @splat(arg1)), v);
                        } else {
                            return f(arg1, v);
                        }
                    }
                };

                return V1.call(F1.call, arg2, out);
            }

            if (ScalarOrVector.which(T2, @TypeOf(arg2)) == .scalar) {
                const V1 = VectorFunction1(T1, O, vec_size);

                const F1 = struct {
                    inline fn call(v: anytype) V1.ReturnType(@TypeOf(v)) {
                        const isSIMD = isSIMDVector(@TypeOf(v));
                        if (isSIMD) {
                            return f(v, @as(Vec2, @splat(arg2)));
                        } else {
                            return f(v, arg2);
                        }
                    }
                };

                return V1.call(F1.call, arg1, out);
            }

            if (vec_size < 2) {
                return Self.forLoop(f, arg1, arg2, out);
            }

            const rem = @mod(out.len, vec_size);
            if (rem > 0) {
                Self.forLoop(f, arg1[0..rem], arg2[0..rem], out[0..rem]);
            }

            var i = rem;
            while (i < out.len) : (i += vec_size) {
                const a1_v: Vec1 = arg1[i..][0..vec_size].*;
                const a2_v: Vec2 = arg2[i..][0..vec_size].*;
                out[i..][0..vec_size].* = f(a1_v, a2_v);
            }
        }
    };
}

test "Vector Function 2" {
    const N = 10;
    const vec_size = std.simd.suggestVectorLength(f32) orelse 0;
    std.debug.print("Vector Size for f32: {}\n", .{vec_size});

    const TestV2 = struct {
        fn do_test(comptime V2: type) !void {
            var a = std.ArrayList(f32).init(testing.allocator);
            defer a.deinit();
            try a.appendNTimes(3.0, N);

            var b = std.ArrayList(f32).init(testing.allocator);
            defer b.deinit();
            try b.appendNTimes(0.5, N);

            var true_out = std.ArrayList(f32).init(testing.allocator);
            defer true_out.deinit();
            try true_out.appendNTimes(1.5, N);

            var out = std.ArrayList(f32).init(testing.allocator);
            defer out.deinit();
            try out.resize(N);

            const Multiply = struct {
                fn call(a1: anytype, a2: anytype) V2.ReturnType(@TypeOf(a1, a2)) {
                    return a1 * a2;
                }
            };

            // Vector-Vector
            V2.call(Multiply.call, a.items, b.items, out.items);
            try testing.expectEqualSlices(f32, true_out.items, out.items);

            out.clearRetainingCapacity();
            try out.resize(N);

            // Vector-Scalar
            V2.call(Multiply.call, a.items, 0.5, out.items);
            try testing.expectEqualSlices(f32, true_out.items, out.items);

            // Scalar-Vector
            V2.call(Multiply.call, 3.0, b.items, out.items);
            try testing.expectEqualSlices(f32, true_out.items, out.items);
        }
    };

    // With SIMD
    try TestV2.do_test(VectorFunction2(f32, f32, f32, vec_size));

    // Without SIMD
    try TestV2.do_test(VectorFunction2(f32, f32, f32, 0));
}

const FillOptions = struct {
    simd_size: ?usize = null,
};

pub fn fill(value: anytype, out: anytype, comptime options: FillOptions) void {
    const T = ElementType(@TypeOf(out));
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

            fill(1.5, out.items, o);
            try true_out.appendNTimes(1.5, N);
            try testing.expectEqualSlices(f16, true_out.items, out.items);
        }
    };

    // With SIMD
    try Test.do(.{});

    // Without SIMD
    try Test.do(.{ .simd_size = 0 });
}

const Options = struct {
    type: type,
    op: BinaryOp,
    simd_size: ?usize = null,
};

/// Call Binary Operation
///
/// - lhs & rhs: T, comptime_int, comptime_float, []const T, or []T
/// - out: []bool for comparison, []T for other operations
pub fn binaryOp(comptime options: Options, lhs: anytype, rhs: anytype, out: anytype) void {
    const T = options.type;
    const S = []T;
    const cS = []const T;
    const L = @TypeOf(lhs);
    const R = @TypeOf(rhs);
    const O: type = switch (options.op) {
        .eq, .neq, .gt, .gte, .lt, .lte => bool,
        else => T,
    };

    if (options.simd_size orelse std.simd.suggestVectorLength(T)) |vec_size| {
        const rem = @mod(out.len, vec_size);
        if (rem > 0) {
            switch (L) {
                S, cS => switch (R) {
                    S, cS => for (lhs[0..rem], rhs[0..rem], out[0..rem]) |li, ri, *oi| {
                        oi.* = bop(O, options.op, li, ri);
                    },
                    T, comptime_int, comptime_float => for (lhs[0..rem], out[0..rem]) |li, *oi| {
                        oi.* = bop(O, options.op, li, rhs);
                    },
                    else => @compileError("rhs must be T or []const T"),
                },
                T, comptime_int, comptime_float => switch (R) {
                    S, cS => for (rhs[0..rem], out[0..rem]) |ri, *oi| {
                        oi.* = bop(O, options.op, lhs, ri);
                    },
                    T, comptime_int, comptime_float => for (out[0..rem]) |*oi| {
                        oi.* = bop(O, options.op, lhs, rhs);
                    },
                    else => @compileError("rhs must be T or []const T"),
                },
                else => @compileError("lhs must be T or []const T"),
            }
        }

        var i = rem;
        const isTT: bool = comptime ((L == T) and (R == T));
        if (isTT) {
            const o: @Vector(vec_size, O) = @splat(bop(O, options.op, lhs, rhs));
            while (i < out.len) : (i += vec_size) {
                out[i..][0..vec_size].* = o;
            }
        } else {
            while (i < out.len) : (i += vec_size) {
                const lv: @Vector(vec_size, T) = switch (L) {
                    S, cS => lhs[i..][0..vec_size].*,
                    T, comptime_int, comptime_float => @splat(lhs),
                    else => @compileError(""),
                };
                const rv: @Vector(vec_size, T) = switch (R) {
                    S, cS => rhs[i..][0..vec_size].*,
                    T, comptime_int, comptime_float => @splat(rhs),
                    else => @compileError(""),
                };

                out[i..][0..vec_size].* = bop(@Vector(vec_size, O), options.op, lv, rv);
            }
        }
    } else {
        // Without SIMD
        switch (L) {
            S => switch (R) {
                S, cS => for (lhs, rhs, out) |li, ri, *oi| {
                    oi.* = bop(O, options.op, li, ri);
                },
                T, comptime_int, comptime_float => for (lhs, out) |li, *oi| {
                    oi.* = bop(O, options.op, li, rhs);
                },
                else => @compileError("rhs must be T or []const T"),
            },
            T => switch (R) {
                S, cS => for (rhs, out) |ri, *oi| {
                    oi.* = bop(O, options.op, lhs, ri);
                },
                T, comptime_int, comptime_float => {
                    const o = bop(O, options.op, lhs, rhs);
                    for (out) |*oi| {
                        oi.* = o;
                    }
                },
                else => @compileError("rhs must be T or []const T"),
            },
            else => @compileError("lhs must be T or []const T"),
        }
    }
}

test "Binary Op" {
    const N = 1_000_000;

    var a = std.ArrayList(f32).init(testing.allocator);
    defer a.deinit();
    try a.appendNTimes(3.4, N);

    var b = std.ArrayList(f32).init(testing.allocator);
    defer b.deinit();
    try b.appendNTimes(2.5, N);

    var c = std.ArrayList(f32).init(testing.allocator);
    defer c.deinit();
    try c.resize(N);

    binaryOp(.{ .type = f32, .op = .add }, a.items, b.items, c.items);

    var d = std.ArrayList(f32).init(testing.allocator);
    defer d.deinit();
    try d.appendNTimes(3.4 + 2.5, N);

    try testing.expectEqualSlices(f32, d.items, c.items);

    binaryOp(.{ .type = f32, .op = .mul }, a.items, 2.0, c.items);
    d.clearRetainingCapacity();
    try d.appendNTimes(3.4 * 2.0, N);
    try testing.expectEqualSlices(f32, d.items, c.items);
}
