const std = @import("std");
const testing = std.testing;

const compat = @import("./compat.zig");

pub fn expectEqualSlices(comptime T: type, true_slice: []const T, actual_slice: []const T) !void {
    switch (@typeInfo(T)) {
        compat.int => {
            try testing.expectEqualSlice(T, true_slice, actual_slice);
        },
        compat.float => {
            for (true_slice, actual_slice, 0..) |t, a, i| {
                testing.expectApproxEqRel(t, a, 1e-6) catch |e| {
                    std.debug.print("Fail at {}\n", .{i});
                    return e;
                };
            }
        },
        else => @compileError(@typeName(T) ++ " is not supported."),
    }
}
