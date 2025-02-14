-- | A domain-specific language for constructing term-encoded Hydra types in Haskell.

module Hydra.Dsl.TTypes (
  module Hydra.Dsl.TBase,
  module Hydra.Dsl.TTypes,
) where

import Hydra.Kernel
import qualified Hydra.Dsl.Base as Base
import Hydra.Dsl.Core as Core hiding (name, unName)
import Hydra.Dsl.TBase

import qualified Data.Map as M
import qualified Data.Maybe as Y


boolean :: TTerm Type
boolean = typeLiteral literalTypeBoolean

field :: String -> TTerm Type -> TTerm FieldType
field s = fieldType (name s)

floatType :: FloatType -> TTerm FloatType
floatType t = Base.unitVariant _FloatType $ case t of
  FloatTypeBigfloat -> _FloatType_bigfloat
  FloatTypeFloat32 -> _FloatType_float32
  FloatTypeFloat64 -> _FloatType_float64

float32 :: TTerm Type
float32 = typeLiteral $ literalTypeFloat $ floatType FloatTypeFloat32

function :: TTerm Type -> TTerm Type -> TTerm Type
function dom cod = typeFunction $ functionType dom cod

functionN :: [TTerm Type] -> TTerm Type
functionN types = case types of
  [t] -> t
  t:ts -> function t $ functionN ts

int16 :: TTerm Type
int16 = typeLiteral $ literalTypeInteger integerTypeInt16

int32 :: TTerm Type
int32 = typeLiteral $ literalTypeInteger integerTypeInt32

integerType :: IntegerType -> TTerm IntegerType
integerType t = Base.unitVariant _IntegerType $ case t of
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
lambda var body = typeLambda $ lambdaType (name var) body

list :: TTerm Type -> TTerm Type
list = typeList

mono :: TTerm Type -> TTerm TypeScheme
mono t = Base.record _TypeScheme [
  Base.field _TypeScheme_variables $ Base.list [],
  Base.field _TypeScheme_type t]

optional :: TTerm Type -> TTerm Type
optional = typeOptional

poly :: [String] -> TTerm Type -> TTerm TypeScheme
poly params t = Base.record _TypeScheme [
  Base.field _TypeScheme_variables (Base.list (name <$> params)),
  Base.field _TypeScheme_type t]

record :: TTerm Name -> [TTerm FieldType] -> TTerm Type
record name fields = typeRecord $ rowType name $ Base.list fields

string :: TTerm Type
string = typeLiteral literalTypeString

apply :: TTerm Type -> TTerm Type -> TTerm Type
apply l r = typeApplication $ applicationType l r

uint64 :: TTerm Type
uint64 = typeLiteral $ literalTypeInteger integerTypeUint64

union :: TTerm Name -> [TTerm FieldType] -> TTerm Type
union name fields = typeUnion $ rowType name $ Base.list fields

unit :: TTerm Type
unit = typeRecord $ rowType (Base.wrap _Name $ Base.string $ unName _Unit) $ Base.list []

var :: String -> TTerm Type
var = typeVariable . name
