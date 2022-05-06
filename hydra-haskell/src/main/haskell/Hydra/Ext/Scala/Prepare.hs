module Hydra.Ext.Scala.Prepare (
  prepareType,
  module Hydra.Core,
) where

import Hydra.Core
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Default
import qualified Data.Set as S


prepareLiteralType :: LiteralType -> (LiteralType, Literal -> Literal, S.Set String)
prepareLiteralType at = case at of
  LiteralTypeBinary -> subst LiteralTypeString
    "binary strings" "character strings"
    $ \(LiteralBinary v) -> LiteralString v
  LiteralTypeFloat ft -> (LiteralTypeFloat rtyp, \(LiteralFloat v) -> LiteralFloat $ rep v, msgs)
    where
      (rtyp, rep, msgs) = prepareFloatType ft
  LiteralTypeInteger it -> (LiteralTypeInteger rtyp, \(LiteralInteger v) -> LiteralInteger $ rep v, msgs)
    where
      (rtyp, rep, msgs) = prepareIntegerType it
  _ -> same at

prepareFloatType :: FloatType -> (FloatType, FloatValue -> FloatValue, S.Set String)
prepareFloatType ft = case ft of
  FloatTypeBigfloat -> subst FloatTypeFloat64
    "arbitrary-precision floating-point numbers" "64-bit floating-point numbers (doubles)"
    $ \(FloatValueBigfloat v) -> FloatValueFloat64 v
  _ -> same ft

prepareIntegerType :: IntegerType -> (IntegerType, IntegerValue -> IntegerValue, S.Set String)
prepareIntegerType it = case it of
  IntegerTypeBigint -> subst IntegerTypeInt64
    "arbitrary-precision integers" "64-bit integers"
    $ \(IntegerValueBigint v) -> IntegerValueInt64 $ fromIntegral v
  IntegerTypeUint8 -> subst IntegerTypeInt8
    "unsigned 8-bit integers" "signed 8-bit integers"
    $ \(IntegerValueUint8 v) -> IntegerValueInt8 v
  IntegerTypeUint32 -> subst IntegerTypeInt32
    "unsigned 32-bit integers" "signed 32-bit integers"
    $ \(IntegerValueUint32 v) -> IntegerValueInt32 $ fromIntegral v
  IntegerTypeUint64 -> subst IntegerTypeInt64
    "unsigned 64-bit integers" "signed 64-bit integers"
    $ \(IntegerValueUint64 v) -> IntegerValueInt64 v
  _ -> same it

prepareType :: Default m => Type m -> (Type m, Data m -> Data m, S.Set String)
prepareType typ = case typeTerm typ of
  TypeTermLiteral at -> (Types.literal rtyp, \(Data (DataTermLiteral av) m) -> Data (DataTermLiteral $ rep av) m, msgs)
    where
      (rtyp, rep, msgs) = prepareLiteralType at
--  TypeTermElement et ->
--  TypeTermFunction (FunctionType dom cod) ->
--  TypeTermList lt ->
--  TypeTermMap (MapType kt vt) ->
--  TypeTermNominal name ->
--  TypeTermRecord fields ->
--  TypeTermSet st ->
--  TypeTermUnion fields ->

same :: a -> (a, b -> b, S.Set c)
same x = (x, id, S.empty)

subst :: a -> [Char] -> [Char] -> b -> (a, b, S.Set [Char])
subst t from to r = (t, r, S.fromList ["replace " ++ from ++ " with " ++ to])
