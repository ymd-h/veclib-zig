const std = @import("std");
const testing = std.testing;

const compat = @import("./compat.zig");

pub fn expectEqualSlices(comptime T: type, true_slice: []const T, actual_slice: []const T) !void {
    switch (@typeInfo(T)) {
        compat.int => {
            try testing.expectEqualSlices(T, true_slice, actual_slice);
        },
        compat.float => {
            for (true_slice, actual_slice, 0..) |t, a, i| {
                testing.expectApproxEqRel(t, a, 1e-6) catch |e| {
                    std.debug.print("Fail at {}. expected: {}, actual: {}\n", .{ i, t, a });
                    return e;
                };
            }
        },
        else => @compileError(@typeName(T) ++ " is not supported."),
    }
}

pub fn expectEqual(expected: anytype, actual: anytype) !void {
    const T = @TypeOf(expected, actual);
    switch (@typeInfo(T)) {
        compat.int => {
            try testing.expectEqual(expected, actual);
        },
        compat.float => {
            try testing.expectApproxEqRel(expected, actual, 1e-6);
        },
        else => @compileError(@typeName(T) ++ " is not supported."),
    }
}
