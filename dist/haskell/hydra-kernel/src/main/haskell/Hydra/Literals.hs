-- Note: this is an automatically generated file. Do not edit.
-- | Conversion functions for literal values.

module Hydra.Literals where
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Literals as Literals
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Convert a bigfloat to a floating-point value of a given type (note: lossy)
bigfloatToFloatValue :: Core.FloatType -> Double -> Core.FloatValue
bigfloatToFloatValue ft bf =
    case ft of
      Core.FloatTypeBigfloat -> Core.FloatValueBigfloat bf
      Core.FloatTypeFloat32 -> Core.FloatValueFloat32 (Literals.bigfloatToFloat32 bf)
      Core.FloatTypeFloat64 -> Core.FloatValueFloat64 (Literals.bigfloatToFloat64 bf)
-- | Convert a bigint to an integer value of a given type (note: lossy)
bigintToIntegerValue :: Core.IntegerType -> Integer -> Core.IntegerValue
bigintToIntegerValue it bi =
    case it of
      Core.IntegerTypeBigint -> Core.IntegerValueBigint bi
      Core.IntegerTypeInt8 -> Core.IntegerValueInt8 (Literals.bigintToInt8 bi)
      Core.IntegerTypeInt16 -> Core.IntegerValueInt16 (Literals.bigintToInt16 bi)
      Core.IntegerTypeInt32 -> Core.IntegerValueInt32 (Literals.bigintToInt32 bi)
      Core.IntegerTypeInt64 -> Core.IntegerValueInt64 (Literals.bigintToInt64 bi)
      Core.IntegerTypeUint8 -> Core.IntegerValueUint8 (Literals.bigintToUint8 bi)
      Core.IntegerTypeUint16 -> Core.IntegerValueUint16 (Literals.bigintToUint16 bi)
      Core.IntegerTypeUint32 -> Core.IntegerValueUint32 (Literals.bigintToUint32 bi)
      Core.IntegerTypeUint64 -> Core.IntegerValueUint64 (Literals.bigintToUint64 bi)
-- | Convert a floating-point value of any precision to a bigfloat
floatValueToBigfloat :: Core.FloatValue -> Double
floatValueToBigfloat x =
    case x of
      Core.FloatValueBigfloat v0 -> v0
      Core.FloatValueFloat32 v0 -> Literals.float32ToBigfloat v0
      Core.FloatValueFloat64 v0 -> Literals.float64ToBigfloat v0
-- | Convert an integer value of any precision to a bigint
integerValueToBigint :: Core.IntegerValue -> Integer
integerValueToBigint x =
    case x of
      Core.IntegerValueBigint v0 -> v0
      Core.IntegerValueInt8 v0 -> Literals.int8ToBigint v0
      Core.IntegerValueInt16 v0 -> Literals.int16ToBigint v0
      Core.IntegerValueInt32 v0 -> Literals.int32ToBigint v0
      Core.IntegerValueInt64 v0 -> Literals.int64ToBigint v0
      Core.IntegerValueUint8 v0 -> Literals.uint8ToBigint v0
      Core.IntegerValueUint16 v0 -> Literals.uint16ToBigint v0
      Core.IntegerValueUint32 v0 -> Literals.uint32ToBigint v0
      Core.IntegerValueUint64 v0 -> Literals.uint64ToBigint v0
