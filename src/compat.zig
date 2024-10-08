//! compat module

const std = @import("std");
const version = @import("builtin").zig_version;

const uptoV013 = (version.major == 0) and (version.minor <= 13);

// Tag name of std.builtin.Type was changed at v0.13
pub const pointer = if (uptoV013) .Pointer else .pointer;
pub const array = if (uptoV013) .Array else .array;
pub const vector = if (uptoV013) .Vector else .vector;
