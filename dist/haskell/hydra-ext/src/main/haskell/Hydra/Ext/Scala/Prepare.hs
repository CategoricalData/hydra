-- Note: this is an automatically generated file. Do not edit.

-- | Type preparation functions for Scala code generation

module Hydra.Ext.Scala.Prepare where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Strip as Strip
import qualified Hydra.Rewriting as Rewriting
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Prepare a literal type for Scala, substituting unsupported types
prepareLiteralType :: Core.LiteralType -> (Core.LiteralType, ((Core.Literal -> Core.Literal), (S.Set String)))
prepareLiteralType at =
    case at of
      Core.LiteralTypeBinary -> (Core.LiteralTypeString, ((\v -> case v of
        Core.LiteralBinary v1 -> Core.LiteralString (Literals.binaryToString v1)
        _ -> v), (Sets.fromList [
        "replace binary strings with character strings"])))
      Core.LiteralTypeFloat v0 ->
        let result = prepareFloatType v0
            rtyp = Pairs.first result
            rep = Pairs.first (Pairs.second result)
            msgs = Pairs.second (Pairs.second result)
        in (Core.LiteralTypeFloat rtyp, ((\v -> case v of
          Core.LiteralFloat v1 -> Core.LiteralFloat (rep v1)
          _ -> v), msgs))
      Core.LiteralTypeInteger v0 ->
        let result = prepareIntegerType v0
            rtyp = Pairs.first result
            rep = Pairs.first (Pairs.second result)
            msgs = Pairs.second (Pairs.second result)
        in (Core.LiteralTypeInteger rtyp, ((\v -> case v of
          Core.LiteralInteger v1 -> Core.LiteralInteger (rep v1)
          _ -> v), msgs))
      _ -> same at

-- | Prepare a float type for Scala
prepareFloatType :: Core.FloatType -> (Core.FloatType, ((Core.FloatValue -> Core.FloatValue), (S.Set String)))
prepareFloatType ft =
    case ft of
      Core.FloatTypeBigfloat -> (Core.FloatTypeFloat64, ((\v -> case v of
        Core.FloatValueBigfloat v1 -> Core.FloatValueFloat64 (Literals.bigfloatToFloat64 v1)
        _ -> v), (Sets.fromList [
        "replace arbitrary-precision floating-point numbers with 64-bit floating-point numbers (doubles)"])))
      _ -> same ft

-- | Prepare an integer type for Scala
prepareIntegerType :: Core.IntegerType -> (Core.IntegerType, ((Core.IntegerValue -> Core.IntegerValue), (S.Set String)))
prepareIntegerType it =
    case it of
      Core.IntegerTypeBigint -> (Core.IntegerTypeInt64, ((\v -> case v of
        Core.IntegerValueBigint v1 -> Core.IntegerValueInt64 (Literals.bigintToInt64 v1)
        _ -> v), (Sets.fromList [
        "replace arbitrary-precision integers with 64-bit integers"])))
      Core.IntegerTypeUint8 -> (Core.IntegerTypeInt8, ((\v -> case v of
        Core.IntegerValueUint8 v1 -> Core.IntegerValueInt8 (Literals.bigintToInt8 (Literals.uint8ToBigint v1))
        _ -> v), (Sets.fromList [
        "replace unsigned 8-bit integers with signed 8-bit integers"])))
      Core.IntegerTypeUint32 -> (Core.IntegerTypeInt32, ((\v -> case v of
        Core.IntegerValueUint32 v1 -> Core.IntegerValueInt32 (Literals.bigintToInt32 (Literals.uint32ToBigint v1))
        _ -> v), (Sets.fromList [
        "replace unsigned 32-bit integers with signed 32-bit integers"])))
      Core.IntegerTypeUint64 -> (Core.IntegerTypeInt64, ((\v -> case v of
        Core.IntegerValueUint64 v1 -> Core.IntegerValueInt64 (Literals.bigintToInt64 (Literals.uint64ToBigint v1))
        _ -> v), (Sets.fromList [
        "replace unsigned 64-bit integers with signed 64-bit integers"])))
      _ -> same it

-- | Prepare a type for Scala code generation, substituting unsupported types
prepareType :: t0 -> Core.Type -> (Core.Type, ((Core.Term -> Core.Term), (S.Set String)))
prepareType cx typ =
    case (Strip.deannotateType typ) of
      Core.TypeLiteral v0 ->
        let result = prepareLiteralType v0
            rtyp = Pairs.first result
            rep = Pairs.first (Pairs.second result)
            msgs = Pairs.second (Pairs.second result)
        in (Core.TypeLiteral rtyp, ((\v -> case v of
          Core.TermLiteral v1 -> Core.TermLiteral (rep v1)
          _ -> v), msgs))
      _ -> same typ

-- | Return a type unchanged with identity transform and no messages
same :: Ord t2 => (t0 -> (t0, ((t1 -> t1), (S.Set t2))))
same x = (x, ((\y -> y), Sets.empty))
