-- | A domain-specific language for constructing term-encoded Hydra types in Haskell;
--   these functions enable you to build terms (programs) which build types.

{-# LANGUAGE FlexibleContexts #-}

module Hydra.Dsl.Meta.Types (
  module Hydra.Dsl.Meta.Base,
  module Hydra.Dsl.Meta.Types,
) where

import Hydra.Kernel
import Hydra.Dsl.AsTerm
import qualified Hydra.Dsl.Meta.Phantoms as Phantoms
import Hydra.Dsl.Meta.Core as Core hiding (name, unName)
import Hydra.Dsl.Meta.Base

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y
import qualified Data.Set as S
import Prelude hiding (either, map, maybe, product, sum)


-- Operators

-- | Type application operator
-- Example: typeConstructor @@ typeArg
(@@) :: TypedTerm Type -> TypedTerm Type -> TypedTerm Type
(@@) = apply

-- | Function type constructor operator
-- Example: int32 --> string
(-->) :: TypedTerm Type -> TypedTerm Type -> TypedTerm Type
(-->) = function


-- | Attach an annotation to a term-encoded type
-- Example: annot annotationMap myType
annot :: TypedTerm (M.Map Name Term) -> TypedTerm Type -> TypedTerm Type
annot ann typ = typeAnnotated $ annotatedType typ ann

-- | Apply a term-encoded type to a type argument
-- Example: apply (var "Maybe") int32
apply :: TypedTerm Type -> TypedTerm Type -> TypedTerm Type
apply l r = typeApplication $ applicationType l r

applys :: TypedTerm Type -> [TypedTerm Type] -> TypedTerm Type
applys t ts = L.foldl apply t ts

-- | Create a term-encoded arbitrary-precision integer type
-- Example: bigint
bigint :: TypedTerm Type
bigint = typeLiteral $ literalTypeInteger integerTypeBigint

-- | Create a term-encoded binary type
-- Example: binary
binary :: TypedTerm Type
binary = typeLiteral literalTypeBinary

-- | Create a term-encoded boolean type
-- Example: boolean
boolean :: TypedTerm Type
boolean = typeLiteral literalTypeBoolean

-- | Create a term-encoded arbitrary-precision decimal type
-- Example: decimal
decimal :: TypedTerm Type
decimal = typeLiteral literalTypeDecimal

-- | Create a term-encoded either type
-- Example: either_ string int32
either_ :: TypedTerm Type -> TypedTerm Type -> TypedTerm Type
either_ l r = typeEither $ eitherType l r

-- | Create a term-encoded enum type with the given variant names (conventionally in camelCase)
-- Example: enum (name "Color") ["red", "green", "blue"]
-- Accepts TypedTerm Name or TypedBinding Name (via AsTerm)
enum :: AsTerm t Name => t -> [String] -> TypedTerm Type
enum tname names = union tname $ (\n -> (name n, unit)) <$> names

-- | Create a term-encoded field with the given name and type
-- Example: field "age" int32
field :: String -> TypedTerm Type -> TypedTerm FieldType
field s = fieldType (name s)

-- | Create a term-encoded FloatType representation
-- Example: float FloatTypeFloat64
float :: FloatType -> TypedTerm FloatType
float t = Phantoms.injectUnit _FloatType $ case t of
  FloatTypeFloat32 -> _FloatType_float32
  FloatTypeFloat64 -> _FloatType_float64

-- | Create a term-encoded 32-bit floating point type
-- Example: float32
float32 :: TypedTerm Type
float32 = typeLiteral $ literalTypeFloat $ float FloatTypeFloat32

-- | Create a term-encoded 64-bit floating point type
-- Example: float64
float64 :: TypedTerm Type
float64 = typeLiteral $ literalTypeFloat $ float FloatTypeFloat64

-- | Create a term-encoded universal quantification (polymorphic type)
-- Example: forAll "a" (var "a" --> var "a")
forAll :: String -> TypedTerm Type -> TypedTerm Type
forAll var body = typeForall $ forallType (name var) body

forAlls :: [String] -> TypedTerm Type -> TypedTerm Type
forAlls vs body = L.foldr forAll body vs

-- | Create a term-encoded function type
-- Example: function int32 string
function :: TypedTerm Type -> TypedTerm Type -> TypedTerm Type
function dom cod = typeFunction $ functionType dom cod

-- | Create a term-encoded multi-parameter function type
-- Example: functionMany [int32, string, boolean]
functionMany :: [TypedTerm Type] -> TypedTerm Type
functionMany types = case types of
  [t] -> t
  t:ts -> function t $ functionMany ts

-- | Create a term-encoded 16-bit signed integer type
-- Example: int16
int16 :: TypedTerm Type
int16 = typeLiteral $ literalTypeInteger integerTypeInt16

-- | Create a term-encoded 32-bit signed integer type
-- Example: int32
int32 :: TypedTerm Type
int32 = typeLiteral $ literalTypeInteger integerTypeInt32

-- | Create a term-encoded 64-bit signed integer type
-- Example: int64
int64 :: TypedTerm Type
int64 = typeLiteral $ literalTypeInteger integerTypeInt64

-- | Create a term-encoded 8-bit signed integer type
-- Example: int8
int8 :: TypedTerm Type
int8 = typeLiteral $ literalTypeInteger integerTypeInt8

-- | Create a term-encoded IntegerType representation
-- Example: integer IntegerTypeInt32
integer :: IntegerType -> TypedTerm IntegerType
integer t = Phantoms.injectUnit _IntegerType $ case t of
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
list :: TypedTerm Type -> TypedTerm Type
list = typeList

-- | Create a term-encoded literal type from a LiteralType value
-- Example: literal (literalTypeInteger integerTypeInt32)
literal :: TypedTerm LiteralType -> TypedTerm Type
literal = typeLiteral

-- | Create a term-encoded map/dictionary type
-- Example: map string int32
map :: TypedTerm Type -> TypedTerm Type -> TypedTerm Type
map k v = typeMap $ mapType k v

-- | Create a term-encoded maybe (optional/nullable) type
-- Example: maybe string
maybe :: TypedTerm Type -> TypedTerm Type
maybe = typeMaybe

-- | Create a term-encoded monomorphic type scheme
-- Example: mono int32
mono :: TypedTerm Type -> TypedTerm TypeScheme
mono t = Phantoms.record _TypeScheme [
  Phantoms.field _TypeScheme_variables $ Phantoms.list ([] :: [TypedTerm Name]),
  Phantoms.field _TypeScheme_body t,
  Phantoms.field _TypeScheme_constraints Phantoms.nothing]

-- | Term-encoded non-negative 32-bit integer type
-- Currently an alias for int32; intended for semantic annotation
-- In future versions, this may include validation constraints
-- Example: nonNegativeInt32
nonNegativeInt32 :: TypedTerm Type
nonNegativeInt32 = Hydra.Dsl.Meta.Types.int32

-- | Create a term-encoded optional (nullable) type (alias for 'maybe')
-- Example: optional string
optional :: TypedTerm Type -> TypedTerm Type
optional = maybe

-- | Create a term-encoded pair type
-- Example: pair string int32
pair :: TypedTerm Type -> TypedTerm Type -> TypedTerm Type
pair first second = Core.typePair $ Core.pairType first second

-- | Create a term-encoded polymorphic type scheme
-- Example: poly ["a", "b"] (var "a" --> var "b")
poly :: [String] -> TypedTerm Type -> TypedTerm TypeScheme
poly params t = Phantoms.record _TypeScheme [
  Phantoms.field _TypeScheme_variables (Phantoms.list (name <$> params)),
  Phantoms.field _TypeScheme_body t,
  Phantoms.field _TypeScheme_constraints Phantoms.nothing]

-- | Create a term-encoded polymorphic type scheme with class constraints
-- Example: polyConstrained ["k", "v"] [("k", ["ordering"])] (map (var "k") (var "v"))
polyConstrained :: [String] -> [(String, [String])] -> TypedTerm Type -> TypedTerm TypeScheme
polyConstrained params constraints t = Phantoms.record _TypeScheme [
  Phantoms.field _TypeScheme_variables (Phantoms.list (name <$> params)),
  Phantoms.field _TypeScheme_body t,
  Phantoms.field _TypeScheme_constraints constraintsTerm]
  where
    constraintsTerm = Phantoms.just $ Phantoms.map $ M.fromList
      [(name v, Core.typeVariableConstraints (Phantoms.list (Core.typeClassConstraintSimple . name <$> classes)))
      | (v, classes) <- constraints]

-- | Create a term-encoded product type (tuple) with multiple components using nested pairs
-- Example: product [string, int32, boolean]
product :: [TypedTerm Type] -> TypedTerm Type
product [] = unit
product [a] = a
product [a, b] = pair a b
product (a:rest) = pair a (product rest)

-- | Create a term-encoded record type with named fields
-- Example: record (name "Person") ["name">: string, "age">: int32]
-- Note: the name parameter is now ignored; record types no longer carry a type name
record :: AsTerm t Name => t -> [(TypedTerm Name, TypedTerm Type)] -> TypedTerm Type
record _n pairs = typeRecord $ Phantoms.list (toField <$> pairs)
  where
    toField (fn, t) = fieldType fn t

-- | Create a term-encoded set type
-- Example: set string
set :: TypedTerm Type -> TypedTerm Type
set = typeSet

-- | Create a term-encoded string type
-- Example: string
string :: TypedTerm Type
string = typeLiteral literalTypeString

-- | Create a term-encoded 16-bit unsigned integer type
-- Example: uint16
uint16 :: TypedTerm Type
uint16 = typeLiteral $ literalTypeInteger integerTypeUint16

-- | Create a term-encoded 32-bit unsigned integer type
-- Example: uint32
uint32 :: TypedTerm Type
uint32 = typeLiteral $ literalTypeInteger integerTypeUint32

-- | Create a term-encoded 64-bit unsigned integer type
-- Example: uint64
uint64 :: TypedTerm Type
uint64 = typeLiteral $ literalTypeInteger integerTypeUint64

-- | Create a term-encoded 8-bit unsigned integer type
-- Example: uint8
uint8 :: TypedTerm Type
uint8 = typeLiteral $ literalTypeInteger integerTypeUint8

-- | Create a term-encoded union type with named variants
-- Example: union (name "Result") ["success">: int32, "error">: string]
-- Note: the name parameter is now ignored; union types no longer carry a type name
union :: AsTerm t Name => t -> [(TypedTerm Name, TypedTerm Type)] -> TypedTerm Type
union _n pairs = typeUnion $ Phantoms.list (toField <$> pairs)
  where
    toField (fn, ft) = fieldType fn ft

-- | Term-encoded unit type (empty record type)
-- Example: unit
unit :: TypedTerm Type
unit = typeUnit

-- | Create a term-encoded type variable (alias for 'variable')
-- Example: var "a"
var :: String -> TypedTerm Type
var = variable

-- | Create a term-encoded type variable
-- Example: variable "a"
variable :: String -> TypedTerm Type
variable = typeVariable . name

-- | Term-encoded void type (uninhabited / bottom type)
-- Example: void
void :: TypedTerm Type
void = typeVoid

-- | Create a term-encoded wrapped type (newtype)
-- Note: the name parameter is now ignored; wrapped types no longer carry a type name
wrap :: AsTerm t Name => t -> TypedTerm Type -> TypedTerm Type
wrap _n t = typeWrap t
