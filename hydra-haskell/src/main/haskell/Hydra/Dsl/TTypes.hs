-- | A domain-specific language for constructing term-encoded Hydra types in Haskell.

module Hydra.Dsl.TTypes (
  module Hydra.Dsl.TBase,
  module Hydra.Dsl.TTypes,
) where

import Hydra.Kernel
import Hydra.Dsl.Base as Base
import qualified Hydra.Dsl.Core as Core
import Hydra.Dsl.TBase

import qualified Data.Map as M
import qualified Data.Maybe as Y


boolean :: TTerm Type
boolean = Core.typeLiteral Core.literalTypeBoolean

field :: String -> TTerm Type -> TTerm FieldType
field s = Core.fieldType (name s)

floatType :: FloatType -> TTerm FloatType
floatType t = unitVariant _FloatType $ case t of
  FloatTypeBigfloat -> _FloatType_bigfloat
  FloatTypeFloat32 -> _FloatType_float32
  FloatTypeFloat64 -> _FloatType_float64

float32 :: TTerm Type
float32 = Core.typeLiteral $ Core.literalTypeFloat $ floatType FloatTypeFloat32

int32 :: TTerm Type
int32 = Core.typeLiteral $ Core.literalTypeInteger Core.integerTypeInt32

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
lambda var body = Core.typeLambda $ Core.lambdaType (name var) body

mono :: TTerm Type -> TTerm TypeScheme
mono t = Base.record _TypeScheme [
  _TypeScheme_variables>>: list [],
  _TypeScheme_type>>: t]

optional :: TTerm Type -> TTerm Type
optional = Core.typeOptional

poly :: TTerm [Name] -> TTerm Type -> TTerm TypeScheme
poly params t = Base.record _TypeScheme [
  _TypeScheme_variables>>: params,
  _TypeScheme_type>>: t]

record :: TTerm Name -> [TTerm FieldType] -> TTerm Type
record name fields = Core.typeRecord $ Core.rowType name $ list fields

string :: TTerm Type
string = Core.typeLiteral Core.literalTypeString

apply :: TTerm Type -> TTerm Type -> TTerm Type
apply l r = Core.typeApplication $ Core.applicationType l r

uint64 :: TTerm Type
uint64 = Core.typeLiteral $ Core.literalTypeInteger Core.integerTypeUint64

union :: TTerm Name -> [TTerm FieldType] -> TTerm Type
union name fields = Core.typeUnion $ Core.rowType name $ list fields

unit :: TTerm Type
unit = Core.typeRecord $ Core.rowType (Core.name _Unit) $ list []

var :: String -> TTerm Type
var = Core.typeVariable . name
