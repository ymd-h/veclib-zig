const std = @import("std");
const veclib = @import("./veclib.zig");

fn bench(writer: anytype, comptime name: []const u8, f: anytype, args: anytype) !void {
    const start = std.time.microTimestamp();
    @call(.auto, f, args);
    const end = std.time.microTimestamp();

    const elapsed_us: u64 = @intCast(end - start);
    const us = @mod(elapsed_us, 1000);

    const elapsed_ms = @divFloor(elapsed_us, 1000);
    const ms = @mod(elapsed_ms, 1000);

    const elapsed_sec = @divFloor(elapsed_ms, 1000);
    const sec = @mod(elapsed_sec, 60);

    const elapsed_min = @divFloor(elapsed_sec, 60);

    try writer.print(
        "{s}: {}min {d:0>2}s {d:0>3}ms {d:0>3}us\n",
        .{ name, elapsed_min, sec, ms, us },
    );
}

fn run_bench2(comptime T: type, allocator: std.mem.Allocator, writer: anytype, comptime name: []const u8, f: anytype, N: usize) !void {
    var a = std.ArrayList(T).init(allocator);
    defer a.deinit();
    try a.appendNTimes(2, N);

    var b = std.ArrayList(T).init(allocator);
    defer b.deinit();
    try b.appendNTimes(7, N);

    var c = std.ArrayList(T).init(allocator);
    defer c.deinit();
    try c.resize(N);

    try bench(writer, name, f, .{ a, b, &c });
}

fn run_benchR(comptime T: type, allocator: std.mem.Allocator, writer: anytype, comptime name: []const u8, f: anytype, N: usize) !T {
    var a = std.ArrayList(T).init(allocator);
    defer a.deinit();
    try a.appendNTimes(2, N);

    const F = struct {
        inline fn call(arg: std.ArrayList(T), ret: *T) void {
            ret.* = f(arg);
        }
    };

    var r: T = undefined;
    try bench(writer, name, F.call, .{ a, &r });

    return r;
}

fn run_benchDot(comptime T: type, allocator: std.mem.Allocator, writer: anytype, comptime name: []const u8, f: anytype, N: usize) !T {
    var a = std.ArrayList(T).init(allocator);
    defer a.deinit();
    try a.appendNTimes(0.5, N);

    var b = std.ArrayList(T).init(allocator);
    defer b.deinit();
    try b.appendNTimes(2.0, N);

    const F = struct {
        inline fn call(a1: anytype, a2: anytype, ret: *T) void {
            ret.* = f(a1, a2);
        }
    };

    var r: T = undefined;
    try bench(writer, name, F.call, .{ a.items, b.items, &r });

    return r;
}

fn run_benchMV(comptime T: type, allocator: std.mem.Allocator, writer: anytype, comptime name: []const u8, f: anytype, row: usize, column: usize) !void {
    var m = std.ArrayList(T).init(allocator);
    defer m.deinit();
    try m.appendNTimes(3.2, row * column);

    var v = std.ArrayList(T).init(allocator);
    defer v.deinit();
    try v.appendNTimes(2.8, column);

    var o = std.ArrayList(T).init(allocator);
    defer o.deinit();
    try o.resize(row);

    try bench(writer, name, f, .{ m.items, v.items, o.items });
}

fn Add(comptime T: type) type {
    return struct {
        fn for_loop(a: std.ArrayList(T), b: std.ArrayList(T), c: *std.ArrayList(T)) void {
            for (a.items, b.items, c.items) |ai, bi, *ci| {
                ci.* = ai + bi;
            }
        }

        fn vec_loop(a: std.ArrayList(T), b: std.ArrayList(T), c: *std.ArrayList(T)) void {
            const n = std.simd.suggestVectorLength(T).?;
            const V = @Vector(n, T);

            var i: usize = 0;
            while (i < a.items.len) : (i += n) {
                const av: V = a.items[i..][0..n].*;
                const bv: V = b.items[i..][0..n].*;
                c.items[i..][0..n].* = av + bv;
            }
        }

        fn veclib_loop(a: std.ArrayList(T), b: std.ArrayList(T), c: *std.ArrayList(T)) void {
            veclib.binary(.{ .type = T, .f = .add }, a.items, b.items, c.items);
        }
    };
}

fn Sum(comptime T: type) type {
    return struct {
        fn for_loop(a: std.ArrayList(T)) T {
            var r = a.items[0];
            for (a.items[1..]) |ai| {
                r += ai;
            }
            return r;
        }

        fn vec_loop(a: std.ArrayList(T)) T {
            const n = std.simd.suggestVectorLength(T).?;
            const V = @Vector(n, T);

            var rv: V = a.items[0..n].*;
            var i: usize = n;
            while (i < a.items.len) : (i += n) {
                const av: V = a.items[i..][0..n].*;
                rv += av;
            }

            return @reduce(.Add, rv);
        }

        fn veclib_loop(a: std.ArrayList(T)) T {
            return veclib.reduce(.{ .type = T, .f = .sum }, a.items);
        }
    };
}

fn Dot(comptime T: type) type {
    return struct {
        fn for_loop(a: []const T, b: []const T) T {
            var r: T = 0;
            for (a, b) |ai, bi| {
                r += ai * bi;
            }
            return r;
        }

        fn vec_loop(a: []const T, b: []const T) T {
            const n = std.simd.suggestVectorLength(T).?;
            const V = @Vector(n, T);

            var av: V = a[0..n].*;
            var bv: V = b[0..n].*;
            var rv = av * bv;

            var i: usize = n;
            while (i < a.len) : (i += n) {
                av = a[i..][0..n].*;
                bv = b[i..][0..n].*;
                rv += av * bv;
            }
            std.debug.assert(i == a.len);

            return @reduce(.Add, rv);
        }

        fn vec_red_loop(a: []const T, b: []const T) T {
            const n = std.simd.suggestVectorLength(T).?;
            const V = @Vector(n, T);

            var av: V = a[0..n].*;
            var bv: V = b[0..n].*;
            var rv = @reduce(.Add, av * bv);

            var i: usize = n;
            while (i < a.len) : (i += n) {
                av = a[i..][0..n].*;
                bv = b[i..][0..n].*;
                rv += @reduce(.Add, av * bv);
            }

            return rv;
        }

        fn vec_fma_loop(a: []const T, b: []const T) T {
            const n = std.simd.suggestVectorLength(T).?;
            const V = @Vector(n, T);

            var av: V = a[0..n].*;
            var bv: V = b[0..n].*;
            var rv = av * bv;

            var i: usize = n;
            while (i < a.len) : (i += n) {
                av = a[i..][0..n].*;
                bv = b[i..][0..n].*;
                rv = @mulAdd(V, av, bv, rv);
            }
            std.debug.assert(i == a.len);

            return @reduce(.Add, rv);
        }
    };
}

fn MatMul(comptime T: type, comptime row: usize, comptime column: usize) type {
    return struct {
        fn for_loop(m: []const T, v: []const T, out: []T) void {
            for (out, 0..) |*o, i| {
                const rid = i * column;
                const r = m[rid .. rid + column];

                o.* = r[0] * v[0];
                for (r[1..], v[1..]) |ri, vi| {
                    o.* += ri * vi;
                }
            }
        }

        fn vec_in_row_loop(m: []const T, v: []const T, out: []T) void {
            const n = std.simd.suggestVectorLength(T).?;
            const V = @Vector(n, T);

            for (out, 0..) |*o, i| {
                const rid = i * column;
                const r = m[rid .. rid + column];

                o.* = @reduce(.Add, @as(V, r[0..n].*) * @as(V, v[0..n].*));

                var j: usize = n;
                while (j < column) : (j += n) {
                    o.* += @reduce(.Add, @as(V, r[j..][0..n].*) * @as(V, v[j..][0..n].*));
                }
            }
        }

        fn vec_in_column_loop(m: []const T, v: []const T, out: []T) void {
            const n = std.simd.suggestVectorLength(T).?;
            const V = @Vector(n, T);

            var cid: [row]usize = undefined;
            veclib.iota(&cid);
            veclib.mul(usize, &cid, column, &cid);

            var col: [n]T = undefined;

            var i: usize = 0;
            while (i < row) : (i += n) {
                var ov: V = @splat(0);

                for (v, 0..) |vj, j| {
                    var c: [n]usize = undefined;
                    veclib.add(usize, cid[i .. i + n], j, &c);
                    veclib.gather(.{ .type = T }, m, &c, &col);
                    ov = @mulAdd(V, @as(V, col), @as(V, @splat(vj)), ov);
                }

                out[i..][0..n].* = ov;
            }
        }
    };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();
    _ = try stdout.write("\n");

    const N1 = 3_000_000;
    try run_bench2(u32, allocator, stdout, "Add u32: naive for-loop    ", Add(u32).for_loop, 8 * N1);
    try run_bench2(u32, allocator, stdout, "Add u32: handwriting vector", Add(u32).vec_loop, 8 * N1);
    try run_bench2(u32, allocator, stdout, "Add u32: veclib            ", Add(u32).veclib_loop, 8 * N1);

    const r1 = try run_benchR(u32, allocator, stdout, "Sum u32: native for-loop   ", Sum(u32).for_loop, 8 * N1);
    const r2 = try run_benchR(u32, allocator, stdout, "Sum u32: handwriting vector", Sum(u32).vec_loop, 8 * N1);
    const r3 = try run_benchR(u32, allocator, stdout, "Sum u32: veclib            ", Sum(u32).veclib_loop, 8 * N1);
    std.debug.assert(r1 == r2);
    std.debug.assert(r2 == r3);

    _ = try run_benchDot(f32, allocator, stdout, "dot f32: for-loop", Dot(f32).for_loop, 8 * N1 * 10);
    _ = try run_benchDot(f32, allocator, stdout, "dot f32: vec     ", Dot(f32).vec_loop, 8 * N1 * 10);
    _ = try run_benchDot(f32, allocator, stdout, "dot f32: vec-red ", Dot(f32).vec_red_loop, 8 * N1 * 10);
    _ = try run_benchDot(f32, allocator, stdout, "dot f32: vec-fma ", Dot(f32).vec_fma_loop, 8 * N1 * 10);

    const R = 8 * 1_000;
    const C = 8 * 1_000;
    try run_benchMV(f32, allocator, stdout, "MatMul: for-loop  ", MatMul(f32, R, C).for_loop, R, C);
    try run_benchMV(f32, allocator, stdout, "MatMul: vec in row", MatMul(f32, R, C).vec_in_row_loop, R, C);
    try run_benchMV(f32, allocator, stdout, "MatMul: vec in col", MatMul(f32, R, C).vec_in_column_loop, R, C);

    try bw.flush();
}
