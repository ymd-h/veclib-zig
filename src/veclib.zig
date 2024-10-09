//! veclib: SIMD based vector library
//!
//! veclib provides SIMD based functions over slice `[]T`.
//!
//! ## Functions
//! - Element-wise Unary Functions
//!   - `sqrt`
//!   - `sin`, `cos`, `tan`
//!   - `exp`, `exp2`, `exp1m`
//!   - `log`, `log2`, `log10`, `log1p`
//!   - `abs`
//!   - `floor`, `ceil`, `trunc`, `round`
//!   - `byteSwap`, `bitReverse`
//!   - `countLeadingZeros`, `countTrailingZeros`, `popCount`
//! - Element-wise Binary Functions
//!   - `add`, `wrapAdd`, `saturateAdd`
//!   - `sub`, `wrapSub`, `saturateSub`
//!   - `mul`, `wrapMul`, `saturateMul`,
//!   - `div`, `rem`
//!   - `bitAdd`, `bitOr`, `bitXor`
//!   - `min`, `max`
//!   - `eq`, `neq`, `gt`, `gte`, `lt`, `lte`,
//! - Element-wise Ternary Functions
//!   - `mulAdd`
//!   - `clip`
//! - Reduction Functions
//!   - `sum`, `wrapSum`, `saturateSum`,
//!   - `prod`, `wrapProd`, `saturateProd`,
//!   - `smallest`, `largest`,
//!   - `all`, `any`

const core = @import("./core.zig");
const math = @import("./math.zig");

pub const VectorFunction0 = core.VectorFunction0;
pub const VectorFunction1 = core.VectorFunction1;
pub const VectorFunction2 = core.VectorFunction2;
pub const VectorFunction3 = core.VectorFunction3;
pub const VectorReductionFunction = core.VectorReductionFunction;

pub const fill = math.fill;
pub const unary = math.unary;
pub const binary = math.binary;
pub const ternary = math.ternary;

// Unary Function
pub const sqrt = math.sqrt;
pub const sin = math.sin;
pub const cos = math.cos;
pub const tan = math.tan;
pub const exp = math.exp;
pub const exp2 = math.exp2;
pub const exp1m = math.exp1m;
pub const log = math.log;
pub const log2 = math.log2;
pub const log10 = math.log10;
pub const log1p = math.log1p;
pub const abs = math.abs;
pub const floor = math.floor;
pub const ceil = math.ceil;
pub const trunc = math.trunc;
pub const round = math.round;
pub const byteSwap = math.byteSwap;
pub const bitReverse = math.bitReverse;
pub const countLeadingZeros = math.countLeadingZeros;
pub const countTrailingZeros = math.countTrailingZeros;
pub const popCount = math.popCount;

// Binary Function
pub const add = math.add;
pub const wrapAdd = math.wrapAdd;
pub const saturateAdd = math.saturateAdd;
pub const sub = math.sub;
pub const wrapSub = math.wrapSub;
pub const saturateSub = math.saturateSub;
pub const mul = math.mul;
pub const wrapMul = math.wrapMul;
pub const saturateMul = math.saturateMul;
pub const div = math.div;
pub const rem = math.rem;
pub const bitAnd = math.bitAnd;
pub const bitOr = math.bitOr;
pub const bitXor = math.bitXor;
pub const eq = math.eq;
pub const neq = math.neq;
pub const gt = math.gt;
pub const gte = math.gte;
pub const lt = math.lt;
pub const lte = math.lte;
pub const min = math.min;
pub const max = math.max;

// Ternary Function
pub const mulAdd = math.mulAdd;
pub const clip = math.clip;

// Reduce Function
pub const reduce = math.reduce;
pub const sum = math.sum;
pub const wrapSum = math.wrapSum;
pub const saturateSum = math.saturateSum;
pub const prod = math.prod;
pub const wrapProd = math.wrapProd;
pub const saturateProd = math.saturateProd;
pub const smallest = math.smallest;
pub const largest = math.largest;

pub const all = math.all;
pub const any = math.any;

test "veclib" {
    _ = @import("./core.zig");
    _ = @import("./math.zig");
}
