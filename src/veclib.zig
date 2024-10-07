//! veclib: SIMD based vector library
//!
//! veclib provides SIMD based functions over slice `[]T`.

const std = @import("std");
const testing = std.testing;

const core = @import("./core.zig");
const math = @import("./math.zig");


pub const VectorFunction0 = core.VectorFunction0;
pub const VectorFunction1 = core.VectorFunction1;
pub const VectorFunction2 = core.VectorFunction2;


pub const fill = math.fill;
pub const unary = math.unary;
pub const binary = math.binary;


test "veclib" {
    _ = @import("./core.zig");
    _ = @import("./math.zig");
}
