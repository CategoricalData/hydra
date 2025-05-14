-- | A domain-specific language for constructing term-encoded Hydra types in Haskell;
--   these functions enable you to build terms (programs) which build types.

module Hydra.Dsl.TTypes (
  module Hydra.Dsl.TBase,
  module Hydra.Dsl.TTypes,
) where

import Hydra.Kernel
import qualified Hydra.Dsl.Phantoms as Phantoms
import Hydra.Dsl.Core as Core hiding (name, unName)
import Hydra.Dsl.TBase

import qualified Data.Map as M
import qualified Data.Maybe as Y
import Prelude hiding (map, product, sum)

apply :: TTerm Type -> TTerm Type -> TTerm Type
apply l r = typeApplication $ applicationType l r

boolean :: TTerm Type
boolean = typeLiteral literalTypeBoolean

field :: String -> TTerm Type -> TTerm FieldType
field s = fieldType (name s)

floatType :: FloatType -> TTerm FloatType
floatType t = Phantoms.unitVariant _FloatType $ case t of
  FloatTypeBigfloat -> _FloatType_bigfloat
  FloatTypeFloat32 -> _FloatType_float32
  FloatTypeFloat64 -> _FloatType_float64

float32 :: TTerm Type
float32 = typeLiteral $ literalTypeFloat $ floatType FloatTypeFloat32

float64 :: TTerm Type
float64 = typeLiteral $ literalTypeFloat $ floatType FloatTypeFloat64

forAll :: String -> TTerm Type -> TTerm Type
forAll var body = typeLambda $ forallType (name var) body

function :: TTerm Type -> TTerm Type -> TTerm Type
function dom cod = typeFunction $ functionType dom cod

functionMany :: [TTerm Type] -> TTerm Type
functionMany types = case types of
  [t] -> t
  t:ts -> function t $ functionMany ts

int16 :: TTerm Type
int16 = typeLiteral $ literalTypeInteger integerTypeInt16

int32 :: TTerm Type
int32 = typeLiteral $ literalTypeInteger integerTypeInt32

int64 :: TTerm Type
int64 = typeLiteral $ literalTypeInteger integerTypeInt64

integerType :: IntegerType -> TTerm IntegerType
integerType t = Phantoms.unitVariant _IntegerType $ case t of
  IntegerTypeBigint -> _IntegerType_bigint
  IntegerTypeInt8 -> _IntegerType_int8
  IntegerTypeInt16 -> _IntegerType_int16
  IntegerTypeInt32 -> _IntegerType_int32
  IntegerTypeInt64 -> _IntegerType_int64
  IntegerTypeUint8 -> _IntegerType_uint8
  IntegerTypeUint16 -> _IntegerType_uint16
  IntegerTypeUint32 -> _IntegerType_uint32
  IntegerTypeUint64 -> _IntegerType_uint64

list :: TTerm Type -> TTerm Type
list = typeList

map :: TTerm Type -> TTerm Type -> TTerm Type
map k v = typeMap $ mapType k v

mono :: TTerm Type -> TTerm TypeScheme
mono t = Phantoms.record _TypeScheme [
  Phantoms.field _TypeScheme_variables $ Phantoms.list [],
  Phantoms.field _TypeScheme_type t]

optional :: TTerm Type -> TTerm Type
optional = typeOptional

pair :: TTerm Type -> TTerm Type -> TTerm Type
pair l r = product [l, r]

poly :: [String] -> TTerm Type -> TTerm TypeScheme
poly params t = Phantoms.record _TypeScheme [
  Phantoms.field _TypeScheme_variables (Phantoms.list (name <$> params)),
  Phantoms.field _TypeScheme_type t]

product :: [TTerm Type] -> TTerm Type
product types = Core.typeProduct $ TTerm $ TermList (unTTerm <$> types)

record :: TTerm Name -> [(TTerm Name, TTerm Type)] -> TTerm Type
record name pairs = typeRecord $ rowType name $ Phantoms.list (toField <$> pairs)
  where
    toField (n, t) = fieldType n t

set :: TTerm Type -> TTerm Type
set = typeSet

string :: TTerm Type
string = typeLiteral literalTypeString

sum :: [TTerm Type] -> TTerm Type
sum types = Core.typeSum $ TTerm $ TermList (unTTerm <$> types)

uint64 :: TTerm Type
uint64 = typeLiteral $ literalTypeInteger integerTypeUint64

union :: TTerm Name -> [(TTerm Name, TTerm Type)] -> TTerm Type
union name pairs = typeUnion $ rowType name $ Phantoms.list (toField <$> pairs)
  where
    toField (n, t) = fieldType n t

unit :: TTerm Type
unit = typeRecord $ rowType (Phantoms.wrap _Name $ Phantoms.string $ unName _Unit) $ Phantoms.list []

var :: String -> TTerm Type
var = typeVariable . name
