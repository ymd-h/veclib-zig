//! compat module

const std = @import("std");
const version = @import("builtin").zig_version;

const beforeV013 = (version.major == 0) and (version.minor < 13);

// Tag name of std.builtin.Type was changed at v0.13
pub const pointer = if (beforeV013) .Pointer else .pointer;
pub const array = if (beforeV013) .Array else .array;
pub const vector = if (beforeV013) .Vector else .vector;
