const std = @import("std");
const veclib = @import("./veclib.zig");

const Worker = veclib.worker.Worker;

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

fn Add(comptime T: type) type {
    return struct {
        const Self = @This();
        const VL = std.ArrayList(T);

        fn benchmark(allocator: std.mem.Allocator, writer: anytype, w: *Worker, N: usize) !void {
            const data = .{
                .{ Self.for_loop, "Add: naive for-loop   " },
                .{ Self.vec_loop, "Add: SIMD hand writing" },
                .{ Self.veclib_loop, "Add: veclib           " },
                .{ Self.veclib_worker, "Add: veclib-worker    " },
            };

            inline for (data) |d| {
                const f = d[0];
                const name = d[1];

                var a = Self.VL.init(allocator);
                defer a.deinit();
                try a.appendNTimes(2, N);

                var b = Self.VL.init(allocator);
                defer b.deinit();
                try b.appendNTimes(7, N);

                var c = Self.VL.init(allocator);
                defer c.deinit();
                try c.resize(N);

                try bench(writer, name, f, .{ w, a, b, &c });
            }
        }

        fn for_loop(_: *Worker, a: Self.VL, b: Self.VL, c: *Self.VL) void {
            for (a.items, b.items, c.items) |ai, bi, *ci| {
                ci.* = ai + bi;
            }
        }

        fn vec_loop(_: *Worker, a: Self.VL, b: Self.VL, c: *Self.VL) void {
            const n = std.simd.suggestVectorLength(T).?;
            const V = @Vector(n, T);

            var i: usize = 0;
            while (i < a.items.len) : (i += n) {
                const av: V = a.items[i..][0..n].*;
                const bv: V = b.items[i..][0..n].*;
                c.items[i..][0..n].* = av + bv;
            }
        }

        fn veclib_loop(_: *Worker, a: Self.VL, b: Self.VL, c: *Self.VL) void {
            veclib.binary(.{ .type = T, .f = .add }, a.items, b.items, c.items);
        }

        fn veclib_worker(w: *Worker, a: Self.VL, b: Self.VL, c: *Self.VL) void {
            var wg = std.Thread.WaitGroup{};
            wg.reset();

            w.binary(.{ .type = T, .f = .add }, &wg, a.items, b.items, c.items) catch unreachable;
            w.pool.?.waitAndWork(&wg);
        }
    };
}

fn Sum(comptime T: type) type {
    return struct {
        const Self = @This();

        fn benchmark(allocator: std.mem.Allocator, writer: anytype, w: *Worker, N: usize) !void {
            const data = .{
                .{ Self.for_loop, "Sum: naive for-loop   " },
                .{ Self.vec_loop, "Sum: hand writing SIMD" },
                .{ Self.veclib_loop, "Sum: veclib           " },
                .{ Self.veclib_worker, "Sum: veclib worker    " },
            };

            var o = std.ArrayList(T).init(allocator);
            defer o.deinit();

            inline for (data) |d| {
                const f = d[0];
                const name = d[1];

                var a = std.ArrayList(T).init(allocator);
                defer a.deinit();
                try a.appendNTimes(2, N);

                const F = struct {
                    inline fn call(work: *Worker, arg: std.ArrayList(T), ret: *T) void {
                        ret.* = f(work, arg);
                    }
                };

                var r: T = undefined;
                try bench(writer, name, F.call, .{ w, a, &r });
                try o.append(r);
            }

            for (o.items[1..]) |oi| {
                std.debug.assert(o.items[0] == oi);
            }
        }

        fn for_loop(_: *Worker, a: std.ArrayList(T)) T {
            var r = a.items[0];
            for (a.items[1..]) |ai| {
                r += ai;
            }
            return r;
        }

        fn vec_loop(_: *Worker, a: std.ArrayList(T)) T {
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

        fn veclib_loop(_: *Worker, a: std.ArrayList(T)) T {
            return veclib.sum(T, a.items);
            // return veclib.reduce(.{ .type = T, .f = .sum }, a.items);
        }

        fn veclib_worker(w: *Worker, a: std.ArrayList(T)) T {
            var wg = std.Thread.WaitGroup{};
            var o: T = undefined;
            w.reduce(.{ .type = T, .f = .sum }, &wg, a.items, &o) catch unreachable;

            if (!wg.isDone()) {
                w.pool.?.waitAndWork(&wg);
            }
            return o;
        }
    };
}

fn Dot(comptime T: type) type {
    return struct {
        const Self = @This();

        fn benchmark(allocator: std.mem.Allocator, writer: anytype, w: *Worker, N: usize) !void {
            const data = .{
                .{ Self.for_loop, "dot: naive for-loop   " },
                .{ Self.vec_loop, "dot: SIMD hand writing" },
                .{ Self.vec_red_loop, "dot: SIMD with @reduce" },
                .{ Self.vec_fma_loop, "dot: SIMD with fuma   " },
                .{ Self.veclib_loop, "dot: veclib           " },
                .{ Self.veclib_worker, "dot: veclib-worker    " },
            };

            inline for (data) |d| {
                const f = d[0];
                const name = d[1];

                var a = std.ArrayList(T).init(allocator);
                defer a.deinit();
                try a.appendNTimes(0.5, N);

                var b = std.ArrayList(T).init(allocator);
                defer b.deinit();
                try b.appendNTimes(2.0, N);

                const F = struct {
                    inline fn call(worker: *Worker, a1: anytype, a2: anytype, ret: *T) void {
                        ret.* = f(worker, a1, a2);
                    }
                };

                var r: T = undefined;
                try bench(writer, name, F.call, .{ w, a.items, b.items, &r });
            }
        }

        fn for_loop(_: *Worker, a: []const T, b: []const T) T {
            var r: T = 0;
            for (a, b) |ai, bi| {
                r += ai * bi;
            }
            return r;
        }

        fn vec_loop(_: *Worker, a: []const T, b: []const T) T {
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

        fn vec_red_loop(_: *Worker, a: []const T, b: []const T) T {
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

        fn vec_fma_loop(_: *Worker, a: []const T, b: []const T) T {
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

        fn veclib_loop(_: *Worker, a: []const T, b: []const T) T {
            return veclib.matrix.dot(.{ .type = T }, a, b);
        }

        fn veclib_worker(w: *Worker, a: []const T, b: []const T) T {
            var wg = std.Thread.WaitGroup{};
            var r: T = undefined;
            w.dot(.{ .type = T }, &wg, a, b, &r) catch unreachable;
            w.wait(&wg) catch unreachable;
            return r;
        }
    };
}

fn MatMul(comptime T: type, comptime row: usize, comptime column: usize) type {
    return struct {
        const Self = @This();

        fn benchmark(allocator: std.mem.Allocator, writer: anytype, w: *Worker) !void {
            const data = .{
                .{ Self.for_loop, "MatMul: naive for-loop            " },
                .{ Self.vec_in_row_loop, "MatMul: SIMD hand writing (row)   " },
                .{ Self.vec_in_column_loop, "MatMul: SIMD hand writing (column)" },
                .{ Self.veclib_loop, "MatMul: veclib                    " },
                .{ Self.veclib_worker, "MatMul: veclib worker             " },
            };

            inline for (data) |d| {
                const f = d[0];
                const name = d[1];

                var m = std.ArrayList(T).init(allocator);
                defer m.deinit();
                try m.appendNTimes(3.2, row * column);

                var v = std.ArrayList(T).init(allocator);
                defer v.deinit();
                try v.appendNTimes(2.8, column);

                var o = std.ArrayList(T).init(allocator);
                defer o.deinit();
                try o.resize(row);

                try bench(writer, name, f, .{ w, m.items, v.items, o.items });
            }
        }

        fn for_loop(_: *Worker, m: []const T, v: []const T, out: []T) void {
            for (out, 0..) |*o, i| {
                const rid = i * column;
                const r = m[rid .. rid + column];

                o.* = r[0] * v[0];
                for (r[1..], v[1..]) |ri, vi| {
                    o.* += ri * vi;
                }
            }
        }

        fn vec_in_row_loop(_: *Worker, m: []const T, v: []const T, out: []T) void {
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

        fn vec_in_column_loop(_: *Worker, m: []const T, v: []const T, out: []T) void {
            const n = std.simd.suggestVectorLength(T).?;
            const V = @Vector(n, T);

            var cid: [row]usize = undefined;
            veclib.iota(&cid);
            veclib.mul(usize, &cid, column, &cid);

            var col: [n]T = undefined;
            var c: [n]usize = undefined;

            var i: usize = 0;
            while (i < row) : (i += n) {
                var ov: V = @splat(0);

                for (v, 0..) |vj, j| {
                    veclib.add(usize, cid[i .. i + n], j, &c);
                    veclib.gather(.{ .type = T }, m, &c, &col);
                    ov = @mulAdd(V, @as(V, col), @as(V, @splat(vj)), ov);
                }

                out[i..][0..n].* = ov;
            }
        }

        fn veclib_loop(_: *Worker, m: []const T, v: []const T, out: []T) void {
            veclib.matrix.mulMV(.{ .type = T }, .{ .row = row, .column = column, .data = m }, v, out);
        }

        fn veclib_worker(w: *Worker, m: []const T, v: []const T, out: []T) void {
            var wg = std.Thread.WaitGroup{};
            w.matMulMV(.{ .type = T }, &wg, .{ .row = row, .column = column, .data = m }, v, out) catch unreachable;
            w.wait(&wg) catch unreachable;
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

    var w = try Worker.init(.{ .allocator = allocator });
    defer w.deinit();

    const N1 = 3_000_000;
    try Add(u32).benchmark(allocator, stdout, &w, 8 * N1);
    try bw.flush();

    try Sum(u32).benchmark(allocator, stdout, &w, 8 * N1);
    try bw.flush();

    try Dot(f32).benchmark(allocator, stdout, &w, 8 * N1 * 10);
    try bw.flush();

    const R = 8 * 1_000;
    const C = 8 * 1_000;
    try MatMul(f32, R, C).benchmark(allocator, stdout, &w);
    try bw.flush();
}
