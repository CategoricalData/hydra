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

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y
import Prelude hiding (either, map, product, sum)


-- Operators

-- | Type application operator
-- Example: typeConstructor @@ typeArg
(@@) :: TTerm Type -> TTerm Type -> TTerm Type
(@@) = apply

-- | Function type constructor operator
-- Example: int32 --> string
(-->) :: TTerm Type -> TTerm Type -> TTerm Type
(-->) = function


-- | Apply a term-encoded type to a type argument
-- Example: apply (var "Maybe") int32
apply :: TTerm Type -> TTerm Type -> TTerm Type
apply l r = typeApplication $ applicationType l r

applys :: TTerm Type -> [TTerm Type] -> TTerm Type
applys t ts = L.foldl apply t ts

-- | Create a term-encoded boolean type
-- Example: boolean
boolean :: TTerm Type
boolean = typeLiteral literalTypeBoolean

-- | Create a term-encoded field with the given name and type
-- Example: field "age" int32
field :: String -> TTerm Type -> TTerm FieldType
field s = fieldType (name s)

-- | Create a term-encoded FloatType representation
-- Example: float FloatTypeFloat64
float :: FloatType -> TTerm FloatType
float t = Phantoms.unitVariant _FloatType $ case t of
  FloatTypeBigfloat -> _FloatType_bigfloat
  FloatTypeFloat32 -> _FloatType_float32
  FloatTypeFloat64 -> _FloatType_float64

-- | Create a term-encoded 32-bit floating point type
-- Example: float32
float32 :: TTerm Type
float32 = typeLiteral $ literalTypeFloat $ float FloatTypeFloat32

-- | Create a term-encoded 64-bit floating point type
-- Example: float64
float64 :: TTerm Type
float64 = typeLiteral $ literalTypeFloat $ float FloatTypeFloat64

-- | Create a term-encoded either type
-- Example: either string int32
either :: TTerm Type -> TTerm Type -> TTerm Type
either l r = typeEither $ eitherType l r

-- | Create a term-encoded universal quantification (polymorphic type)
-- Example: forAll "a" (var "a" --> var "a")
forAll :: String -> TTerm Type -> TTerm Type
forAll var body = typeForall $ forallType (name var) body

forAlls :: [String] -> TTerm Type -> TTerm Type
forAlls vs body = L.foldr forAll body vs

-- | Create a term-encoded function type
-- Example: function int32 string
function :: TTerm Type -> TTerm Type -> TTerm Type
function dom cod = typeFunction $ functionType dom cod

-- | Create a term-encoded multi-parameter function type
-- Example: functionMany [int32, string, boolean]
functionMany :: [TTerm Type] -> TTerm Type
functionMany types = case types of
  [t] -> t
  t:ts -> function t $ functionMany ts

-- | Create a term-encoded 16-bit signed integer type
-- Example: int16
int16 :: TTerm Type
int16 = typeLiteral $ literalTypeInteger integerTypeInt16

-- | Create a term-encoded 32-bit signed integer type
-- Example: int32
int32 :: TTerm Type
int32 = typeLiteral $ literalTypeInteger integerTypeInt32

-- | Create a term-encoded 64-bit signed integer type
-- Example: int64
int64 :: TTerm Type
int64 = typeLiteral $ literalTypeInteger integerTypeInt64

-- | Create a term-encoded IntegerType representation
-- Example: integer IntegerTypeInt32
integer :: IntegerType -> TTerm IntegerType
integer t = Phantoms.unitVariant _IntegerType $ case t of
  IntegerTypeBigint -> _IntegerType_bigint
  IntegerTypeInt8 -> _IntegerType_int8
  IntegerTypeInt16 -> _IntegerType_int16
  IntegerTypeInt32 -> _IntegerType_int32
  IntegerTypeInt64 -> _IntegerType_int64
  IntegerTypeUint8 -> _IntegerType_uint8
  IntegerTypeUint16 -> _IntegerType_uint16
  IntegerTypeUint32 -> _IntegerType_uint32
  IntegerTypeUint64 -> _IntegerType_uint64

-- | Create a term-encoded list type
-- Example: list string
list :: TTerm Type -> TTerm Type
list = typeList

-- | Create a term-encoded map/dictionary type
-- Example: map string int32
map :: TTerm Type -> TTerm Type -> TTerm Type
map k v = typeMap $ mapType k v

-- | Create a term-encoded monomorphic type scheme
-- Example: mono int32
mono :: TTerm Type -> TTerm TypeScheme
mono t = Phantoms.record _TypeScheme [
  Phantoms.field _TypeScheme_variables $ Phantoms.list [],
  Phantoms.field _TypeScheme_type t]

-- | Create a term-encoded optional (nullable) type
-- Example: optional string
optional :: TTerm Type -> TTerm Type
optional = typeMaybe

-- | Create a term-encoded pair type
-- Example: pair string int32
pair :: TTerm Type -> TTerm Type -> TTerm Type
pair first second = Core.typePair $ Core.pairType first second

-- | Create a term-encoded 2-tuple type
-- Example: tuple2 string int32
tuple2 :: TTerm Type -> TTerm Type -> TTerm Type
tuple2 l r = product [l, r]

-- | Create a term-encoded polymorphic type scheme
-- Example: poly ["a", "b"] (var "a" --> var "b")
poly :: [String] -> TTerm Type -> TTerm TypeScheme
poly params t = Phantoms.record _TypeScheme [
  Phantoms.field _TypeScheme_variables (Phantoms.list (name <$> params)),
  Phantoms.field _TypeScheme_type t]

-- | Create a term-encoded product type (tuple) with multiple components
-- Example: product [string, int32, boolean]
product :: [TTerm Type] -> TTerm Type
product types = Core.typeProduct $ TTerm $ TermList (unTTerm <$> types)

-- | Create a term-encoded record type with named fields
-- Example: record (name "Person") ["name">: string, "age">: int32]
record :: TTerm Name -> [(TTerm Name, TTerm Type)] -> TTerm Type
record name pairs = typeRecord $ rowType name $ Phantoms.list (toField <$> pairs)
  where
    toField (n, t) = fieldType n t

-- | Create a term-encoded set type
-- Example: set string
set :: TTerm Type -> TTerm Type
set = typeSet

-- | Create a term-encoded string type
-- Example: string
string :: TTerm Type
string = typeLiteral literalTypeString

-- | Create a term-encoded sum type (disjoint union) with multiple variants
-- Example: sum [string, int32, boolean]
sum :: [TTerm Type] -> TTerm Type
sum types = Core.typeSum $ TTerm $ TermList (unTTerm <$> types)

-- | Create a term-encoded 64-bit unsigned integer type
-- Example: uint64
uint64 :: TTerm Type
uint64 = typeLiteral $ literalTypeInteger integerTypeUint64

-- | Create a term-encoded union type with named variants
-- Example: union (name "Result") ["success">: int32, "error">: string]
union :: TTerm Name -> [(TTerm Name, TTerm Type)] -> TTerm Type
union name pairs = typeUnion $ rowType name $ Phantoms.list (toField <$> pairs)
  where
    toField (n, t) = fieldType n t

-- | Term-encoded unit type (empty record type)
-- Example: unit
unit :: TTerm Type
unit = typeUnit

-- | Create a term-encoded type variable
-- Example: var "a"
var :: String -> TTerm Type
var = typeVariable . name

wrap :: TTerm Name -> TTerm Type -> TTerm Type
wrap name t = typeWrap $ wrappedType name t
