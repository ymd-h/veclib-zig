# veclib.zig: SIMD based Vector Calculation Library


> [!WARNING]
> This project is still under development.

veclib utilizes Zig standard SIMD vector calculation.
Aside from the standard, veclib supports scalar-vector mixed calculation
and runtime-known length slice handling.


## Features

- Element-wise Unary Function (`f(comptime T: type, arg: anytype, out: []T) void`)
  - `sqrt`
  - `sin`, `cos`, `tan`
  - `exp`, `exp2`, `exp1m`
  - `log`, `log2`, `log10`, `log1p`
  - `abs`
  - `floor`, `ceil`, `trunc`, `round`
  - `byteSwap`, `bitReverse`
  - `countLeadingZeros`, `countTrailingZeros`, `popCount`
- Element-wise Binary Function (`f(comptime T: type, arg1: anytype, arg2: anytype, out: []T) void`)
  - `add`, `wrapAdd`, `saturateAdd`
  - `sub`, `wrapSub`, `saturateSub`
  - `mul`, `wrapMul`, `saturateMul`,
  - `div`, `divFloor`, `divTrunc`, `rem`, `mod`
  - `bitAdd`, `bitOr`, `bitXor`
  - `min`, `max`
- Element-wise Binary Comparison (`f(comptime T: type, arg1: anytype, arg2: anytype, out: [] bool) void`)
  - `eq`, `neq`, `gt`, `gte`, `lt`, `lte`,
- Ternary Function (`f(comptime T: type, arg1: anytype, arg2: anytype, arg3: anytype, out: []T)`)
  - `mulAdd`
  - `clip`
- Reduction Function (`f(comptime T: type, arg: []const T) T`)
  - `sum`, `wrapSum`, `saturateSum`,
  - `prod`, `wrapProd`, `saturateProd`,
  - `smallest`, `largest`,
  - `all`, `any`


We use `anytype` for type inference, however, there some limitations.
- `arg` at element-wise functions can be scalar or slice.
  - scalar value will be broadcsted (not to slice size, but to SIMD vector size)
- Not all types are supported at some functions.
  - e.g. Bit-wise operations are only supported for integer types.
