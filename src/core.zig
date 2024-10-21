//! core module

const std = @import("std");
const testing = std.testing;

const compat = @import("./compat.zig");

pub inline fn sizeForSIMD(comptime T: type, comptime size: ?usize) usize {
    comptime {
        return size orelse (std.simd.suggestVectorLength(T) orelse 0);
    }
}

pub const ScalarOrVector = enum {
    scalar,
    vector,

    const Self = @This();

    pub inline fn which(comptime T: type, comptime V: type) Self {
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

                    @compileError("Compile Error: `value` must be Scalar (aka. T, comptime_int, or comptime_float) or Vector (aka. []T, []const T), but T = " ++ @typeName(T) ++ ", @TypeOf(value) = " ++ @typeName(V));
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

pub inline fn isSIMDVector(comptime T: type) bool {
    comptime {
        return switch (@typeInfo(T)) {
            compat.vector => true,
            else => false,
        };
    }
}

pub inline fn ElementType(comptime T: type) type {
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

        @compileError("T (" ++ @typeName(T) ++ ") is not array-like");
    }
}

fn validateOut(comptime O: type, comptime OType: type) void {
    const sv = ScalarOrVector.which(O, OType);

    if (sv == .scalar) {
        @compileError("`out` must be Vector but Scalar: O = " ++ @typeName(O) ++ ", OType = " ++ @typeName(OType));
    }
}

test "Validate Out" {
    const a = [_]u32{ 0, 2, 3 };
    validateOut(u32, @TypeOf(a[0..]));
}

/// Vector Function without arguments
pub fn VectorFunction0(comptime O: type, comptime vec_size: usize) type {
    return struct {
        const Self = @This();
        const VecO = @Vector(vec_size, O);

        pub fn ReturnType(comptime T: type) type {
            comptime {
                if (isSIMDVector(T)) {
                    return VecO;
                } else {
                    return O;
                }
            }
        }

        inline fn forLoop(o: O, out: anytype) void {
            for (out) |*oi| {
                oi.* = o;
            }
        }

        inline fn forLoopWithIndex(comptime f: anytype, out: anytype) void {
            for (out, 0..) |*oi, i| {
                oi.* = f.call(i);
            }
        }

        /// Call Vector Function
        pub fn call(f: anytype, out: anytype) void {
            validateOut(O, @TypeOf(out));

            const o = f.call();
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

        pub fn callWithIndex(f: anytype, out: anytype) void {
            validateOut(O, @TypeOf(out));

            if (vec_size < 2) {
                return Self.forLoopWithIndex(f, out);
            }

            const rem = @mod(out.len, vec_size);
            if (rem > 0) {
                Self.forLoopWithIndex(f, out[0..rem]);
            }

            var i = rem;
            const iota = std.simd.iota(usize, vec_size);
            while (i < out.len) : (i += vec_size) {
                out[i..][0..vec_size].* = f.call(iota + @as(@Vector(vec_size, usize), @splat(i)));
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

    const vec_size = sizeForSIMD(f16, null);
    std.debug.print("Vector Size for f16: {}\n", .{vec_size});

    // With SIMD
    VectorFunction0(f16, vec_size).call(Fill, out.items);
    try testing.expectEqualSlices(f16, true_out.items, out.items);

    out.clearRetainingCapacity();
    try out.resize(N);

    // Without SIMD
    VectorFunction0(f16, 0).call(Fill, out.items);
    try testing.expectEqualSlices(f16, true_out.items, out.items);
}

/// Vector Function with 1 argument
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

        inline fn forLoop(f: anytype, arg1: []const T1, out: anytype) void {
            for (arg1, out) |a1_i, *oi| {
                oi.* = f.call(a1_i);
            }
        }

        /// Call Vector Function
        pub fn call(f: anytype, arg1: anytype, out: anytype) void {
            validateOut(O, @TypeOf(out));

            switch (ScalarOrVector.which(T1, @TypeOf(arg1))) {
                .scalar => {
                    const F0 = struct {
                        a1: T1,

                        const Self1 = @This();

                        inline fn call(self: Self1) O {
                            return f.call(self.a1);
                        }
                    };
                    VectorFunction0(O, vec_size).call(F0{ .a1 = arg1 }, out);
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
                        out[i..][0..vec_size].* = f.call(a1_v);
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
            V1.call(TwoTimes, a.items, out.items);
            try testing.expectEqualSlices(u32, true_out.items, out.items);

            out.clearRetainingCapacity();
            try out.resize(N);

            true_out.clearRetainingCapacity();
            try true_out.appendNTimes(6, N);

            // Scalar
            V1.call(TwoTimes, 3, out.items);
            try testing.expectEqualSlices(u32, true_out.items, out.items);
        }
    };

    // With SIMD
    try TestV1.do_test(VectorFunction1(u32, u32, vec_size));

    // Without SIMD
    try TestV1.do_test(VectorFunction1(u32, u32, 0));
}

/// Vector Function with 2 arguments
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

        inline fn forLoop(f: anytype, arg1: []const T1, arg2: []const T2, out: anytype) void {
            for (arg1, arg2, out) |a1_i, a2_i, *oi| {
                oi.* = f.call(a1_i, a2_i);
            }
        }

        /// Call Vector Function
        pub fn call(f: anytype, arg1: anytype, arg2: anytype, out: anytype) void {
            validateOut(O, @TypeOf(out));

            if (ScalarOrVector.which(T1, @TypeOf(arg1)) == .scalar) {
                const V1 = VectorFunction1(T2, O, vec_size);

                const F1 = struct {
                    a1: T1,

                    const Self1 = @This();

                    inline fn call(self: Self1, v: anytype) V1.ReturnType(@TypeOf(v)) {
                        const isSIMD = isSIMDVector(@TypeOf(v));
                        if (isSIMD) {
                            return f.call(@as(Vec1, @splat(self.a1)), v);
                        } else {
                            return f.call(self.a1, v);
                        }
                    }
                };

                return V1.call(F1{ .a1 = arg1 }, arg2, out);
            }

            if (ScalarOrVector.which(T2, @TypeOf(arg2)) == .scalar) {
                const V1 = VectorFunction1(T1, O, vec_size);

                const F1 = struct {
                    a2: T2,

                    const Self2 = @This();

                    inline fn call(self: Self2, v: anytype) V1.ReturnType(@TypeOf(v)) {
                        const isSIMD = isSIMDVector(@TypeOf(v));
                        if (isSIMD) {
                            return f.call(v, @as(Vec2, @splat(self.a2)));
                        } else {
                            return f.call(v, self.a2);
                        }
                    }
                };

                return V1.call(F1{ .a2 = arg2 }, arg1, out);
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
                out[i..][0..vec_size].* = f.call(a1_v, a2_v);
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
            V2.call(Multiply, a.items, b.items, out.items);
            try testing.expectEqualSlices(f32, true_out.items, out.items);

            out.clearRetainingCapacity();
            try out.resize(N);

            // Vector-Scalar
            V2.call(Multiply, a.items, 0.5, out.items);
            try testing.expectEqualSlices(f32, true_out.items, out.items);

            // Scalar-Vector
            V2.call(Multiply, 3.0, b.items, out.items);
            try testing.expectEqualSlices(f32, true_out.items, out.items);
        }
    };

    // With SIMD
    try TestV2.do_test(VectorFunction2(f32, f32, f32, vec_size));

    // Without SIMD
    try TestV2.do_test(VectorFunction2(f32, f32, f32, 0));
}

pub fn VectorFunction3(comptime T1: type, comptime T2: type, comptime T3: type, comptime O: type, comptime vec_size: usize) type {
    return struct {
        const Self = @This();
        const Vec1 = @Vector(vec_size, T1);
        const Vec2 = @Vector(vec_size, T2);
        const Vec3 = @Vector(vec_size, T3);
        const VecO = @Vector(vec_size, O);

        pub fn ReturnType(comptime T: type) type {
            comptime {
                return if (isSIMDVector(T)) VecO else O;
            }
        }

        inline fn forLoop(f: anytype, arg1: anytype, arg2: anytype, arg3: anytype, out: anytype) void {
            for (arg1, arg2, arg3, out) |a1, a2, a3, *o| {
                o.* = f.call(a1, a2, a3);
            }
        }

        pub fn call(f: anytype, arg1: anytype, arg2: anytype, arg3: anytype, out: anytype) void {
            validateOut(O, @TypeOf(out));

            if (ScalarOrVector.which(T1, @TypeOf(arg1)) == .scalar) {
                const V2 = VectorFunction2(T2, T3, O, vec_size);

                const F2 = struct {
                    a1: T1,

                    const Self1 = @This();

                    inline fn call(self: Self1, a2: anytype, a3: anytype) V2.ReturnType(@TypeOf(a2)) {
                        if (isSIMDVector(@TypeOf(a2))) {
                            return f.call(@as(Vec1, @splat(self.a1)), a2, a3);
                        } else {
                            return f.call(self.a1, a2, a3);
                        }
                    }
                };

                return V2.call(F2{ .a1 = arg1 }, arg2, arg3, out);
            }

            if (ScalarOrVector.which(T2, @TypeOf(arg2)) == .scalar) {
                const V2 = VectorFunction2(T1, T3, O, vec_size);

                const F2 = struct {
                    a2: T2,

                    const Self2 = @This();

                    inline fn call(self: Self2, a1: anytype, a3: anytype) V2.ReturnType(@TypeOf(a1)) {
                        if (isSIMDVector(@TypeOf(a1))) {
                            return f.call(a1, @as(Vec2, @splat(self.a2)), a3);
                        } else {
                            return f.call(a1, self.a2, a3);
                        }
                    }
                };

                return V2.call(F2{ .a2 = arg2 }, arg1, arg3, out);
            }

            if (ScalarOrVector.which(T3, @TypeOf(arg3)) == .scalar) {
                const V2 = VectorFunction2(T1, T2, O, vec_size);

                const F2 = struct {
                    a3: T3,

                    const Self3 = @This();

                    inline fn call(self: Self3, a1: anytype, a2: anytype) V2.ReturnType(@TypeOf(a1)) {
                        if (isSIMDVector(@TypeOf(a1))) {
                            return f.call(a1, a2, @as(Vec3, @splat(self.a3)));
                        } else {
                            return f.call(a1, a2, self.a3);
                        }
                    }
                };

                return V2.call(F2{ .a3 = arg3 }, arg1, arg2, out);
            }

            if (vec_size < 2) {
                return Self.forLoop(f, arg1, arg2, arg3, out);
            }

            const rem = @mod(out.len, vec_size);
            if (rem > 0) {
                Self.forLoop(f, arg1[0..rem], arg2[0..rem], arg3[0..rem], out[0..rem]);
            }

            var i = rem;
            while (i < out.len) : (i += vec_size) {
                const a1_v: Vec1 = arg1[i..][0..vec_size].*;
                const a2_v: Vec2 = arg2[i..][0..vec_size].*;
                const a3_v: Vec3 = arg3[i..][0..vec_size].*;
                out[i..][0..vec_size].* = f.call(a1_v, a2_v, a3_v);
            }
        }
    };
}

test "Vector Function 3" {
    const N = 100;

    const Test = struct {
        fn do(comptime T: type, a: T, b: T, c: T, o: T, comptime vec_size: usize) !void {
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

            const V3 = VectorFunction3(T, T, T, T, vec_size);

            const F = struct {
                inline fn call(arg1: anytype, arg2: anytype, arg3: anytype) V3.ReturnType(@TypeOf(arg1)) {
                    return @mulAdd(V3.ReturnType(@TypeOf(arg1)), arg1, arg2, arg3);
                }
            };

            V3.call(F, a1.items, a2.items, a3.items, out.items);
            for (true_out.items, out.items) |ti, oi| {
                try testing.expectApproxEqRel(ti, oi, 1e-6);
            }
        }
    };

    const size = std.simd.suggestVectorLength(f32) orelse 0;
    try Test.do(f32, 3.2, 2.5, 0.3, 3.2 * 2.5 + 0.3, size);
    try Test.do(f32, 3.2, 2.5, 0.3, 3.2 * 2.5 + 0.3, 0);
}

pub fn VectorReductionFunction(comptime T: type, comptime vec_size: usize) type {
    return struct {
        const Self = @This();
        const Vec = @Vector(vec_size, T);

        pub fn ReturnType(comptime A: type) type {
            comptime {
                return if (isSIMDVector(A)) Vec else T;
            }
        }

        inline fn forLoop(f: anytype, arg1: []const T) T {
            var r = arg1[0];
            for (arg1[1..]) |ai| {
                r = f.call(r, ai);
            }
            return r;
        }

        pub fn call(f: anytype, arg1: anytype) T {
            if ((vec_size < 2) or (arg1.len < vec_size)) {
                return Self.forLoop(f, arg1);
            }

            const rem = @mod(arg1.len, vec_size);
            const n = arg1.len - rem;

            var v: Vec = arg1[0..vec_size].*;
            var i = vec_size;
            while (i < n) : (i += vec_size) {
                const av: Vec = arg1[i..][0..vec_size].*;
                v = f.call(v, av);
            }

            // ToDo: Can we use @reduce() builtin function?
            const a: [vec_size]T = v;
            var r = Self.forLoop(f, &a);

            if (rem > 0) {
                r = f.call(r, Self.forLoop(f, arg1[i..]));
            }
            return r;
        }
    };
}

test "Vector Reduction" {
    const N = 100;

    const Test = struct {
        fn do(comptime T: type, comptime size: ?usize, f: anytype, a: T, o: T) !void {
            var arg = std.ArrayList(T).init(testing.allocator);
            defer arg.deinit();
            try arg.appendNTimes(a, N);

            const vec_size = size orelse (std.simd.suggestVectorLength(T) orelse 0);
            const VR = VectorReductionFunction(T, vec_size);

            const r = VR.call(f, arg.items);
            try testing.expectEqual(o, r);
        }
    };

    const Add = struct {
        fn call(a: anytype, b: anytype) @TypeOf(a, b) {
            return a + b;
        }
    };

    const Prod = struct {
        fn call(a: anytype, b: anytype) @TypeOf(a, b) {
            return a * b;
        }
    };

    try Test.do(u32, null, Add, 2, 200);
    try Test.do(u32, 0, Add, 2, 200);
    try Test.do(u128, null, Prod, 2, 1 << 100);
}
