const std = @import("std");
const testing = std.testing;

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

const Options = struct {
    type: type,
    op: BinaryOp,
    simd_size: ?usize = null,
};

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
