-- | A domain-specific language for constructing term-encoded Hydra types in Haskell.

module Hydra.Dsl.TTypes where

import Hydra.Kernel
import Hydra.Dsl.Base as Base
import Hydra.Dsl.Core as Core

import qualified Data.Map as M
import qualified Data.Maybe as Y


boolean :: TTerm Type
boolean = typeLiteral literalTypeBoolean

field :: String -> TTerm Type -> TTerm FieldType
field s = Core.fieldType (Core.name $ Name s)

floatType :: FloatType -> TTerm FloatType
floatType t = unitVariant _FloatType $ case t of
  FloatTypeBigfloat -> _FloatType_bigfloat
  FloatTypeFloat32 -> _FloatType_float32
  FloatTypeFloat64 -> _FloatType_float64

float32 :: TTerm Type
float32 = typeLiteral $ literalTypeFloat $ floatType FloatTypeFloat32

int32 :: TTerm Type
int32 = typeLiteral $ literalTypeInteger integerTypeInt32

integerType :: IntegerType -> TTerm IntegerType
integerType t = unitVariant _IntegerType $ case t of
  IntegerTypeBigint -> _IntegerType_bigint
  IntegerTypeInt8 -> _IntegerType_int8
  IntegerTypeInt16 -> _IntegerType_int16
  IntegerTypeInt32 -> _IntegerType_int32
  IntegerTypeInt64 -> _IntegerType_int64
  IntegerTypeUint8 -> _IntegerType_uint8
  IntegerTypeUint16 -> _IntegerType_uint16
  IntegerTypeUint32 -> _IntegerType_uint32
  IntegerTypeUint64 -> _IntegerType_uint64

lambda :: String -> TTerm Type -> TTerm Type
lambda var body = typeLambda $ lambdaType (name $ Name var) body

mono :: TTerm Type -> TTerm TypeScheme
mono t = Base.record _TypeScheme [
  _TypeScheme_variables>>: list [],
  _TypeScheme_type>>: t]

optional :: TTerm Type -> TTerm Type
optional = typeOptional

poly :: TTerm [Name] -> TTerm Type -> TTerm TypeScheme
poly params t = Base.record _TypeScheme [
  _TypeScheme_variables>>: params,
  _TypeScheme_type>>: t]

record :: TTerm Name -> [TTerm FieldType] -> TTerm Type
record name fields = Core.typeRecord $ Core.rowType name $ list fields

string :: TTerm Type
string = typeLiteral literalTypeString

apply :: TTerm Type -> TTerm Type -> TTerm Type
apply l r = typeApplication $ applicationType l r

uint64 :: TTerm Type
uint64 = typeLiteral $ literalTypeInteger integerTypeUint64

union :: TTerm Name -> [TTerm FieldType] -> TTerm Type
union name fields = Core.typeUnion $ Core.rowType name $ list fields

unit :: TTerm Type
unit = typeRecord $ rowType (name _Unit) $ list []

var :: String -> TTerm Type
var = typeVariable . name . Name
