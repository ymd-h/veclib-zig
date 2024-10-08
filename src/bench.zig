const std = @import("std");
const veclib = @import("./veclib.zig");

fn bench(writer: anytype, comptime name: []const u8, f: anytype, args: anytype) !void {
    const start = std.time.nanoTimestamp();
    @call(.auto, f, args);
    const end = std.time.nanoTimestamp();

    const elapsed_ns: u128 = @intCast(end - start);
    const ns = @mod(elapsed_ns, 1000);

    const elapsed_us = @divFloor(elapsed_ns, 1000);
    const us = @mod(elapsed_us, 1000);

    const elapsed_ms = @divFloor(elapsed_us, 1000);
    const ms = @mod(elapsed_ms, 1000);

    const elapsed_sec = @divFloor(elapsed_ms, 1000);
    const sec = @mod(elapsed_sec, 60);

    const elapsed_min = @divFloor(elapsed_sec, 60);

    try writer.print(
        "{s}: {}min {d:0>2}s {d:0>3}ms {d:0>3}us {d:0>3}ns\n",
        .{ name, elapsed_min, sec, ms, us, ns },
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

    try bw.flush();
}
