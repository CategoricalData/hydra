-- | Conversion functions for literal values.

module Hydra.Literals where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Literals as Literals
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | Convert a floating-point value of any precision to a bigfloat
floatValueToBigfloat :: (Core.FloatValue -> Double)
floatValueToBigfloat x = case x of
  Core.FloatValueBigfloat v1 -> (Equality.identity v1)
  Core.FloatValueFloat32 v1 -> (Literals.float32ToBigfloat v1)
  Core.FloatValueFloat64 v1 -> (Literals.float64ToBigfloat v1)

-- | Convert an integer value of any precision to a bigint
integerValueToBigint :: (Core.IntegerValue -> Integer)
integerValueToBigint x = case x of
  Core.IntegerValueBigint v1 -> (Equality.identity v1)
  Core.IntegerValueInt8 v1 -> (Literals.int8ToBigint v1)
  Core.IntegerValueInt16 v1 -> (Literals.int16ToBigint v1)
  Core.IntegerValueInt32 v1 -> (Literals.int32ToBigint v1)
  Core.IntegerValueInt64 v1 -> (Literals.int64ToBigint v1)
  Core.IntegerValueUint8 v1 -> (Literals.uint8ToBigint v1)
  Core.IntegerValueUint16 v1 -> (Literals.uint16ToBigint v1)
  Core.IntegerValueUint32 v1 -> (Literals.uint32ToBigint v1)
  Core.IntegerValueUint64 v1 -> (Literals.uint64ToBigint v1)
