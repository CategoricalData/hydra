-- | A domain-specific language for constructing Hydra types in Haskell.
module Hydra.Dsl.Types where

import Prelude hiding (maybe, product)

import Hydra.Core
import Hydra.Dsl.AsType
import Hydra.Dsl.Meta.Common
import Hydra.Constants

import qualified Data.List as L
import qualified Data.Map as M


-- Operators

-- | Function type constructor with infix syntax
-- Example: int32 ~> string
-- Use this for more readable function type definitions
infixr 0 ~>
(~>) :: (AsType a, AsType b) => a -> b -> Type
dom ~> cod = function (asType dom) (asType cod)

-- | Function type constructor with infix syntax (deprecated, use ~> instead)
-- Example: int32 --> string
infixr 0 -->
(-->) :: (AsType a, AsType b) => a -> b -> Type
dom --> cod = function (asType dom) (asType cod)

-- | Type application operator
-- Example: list @@ int32
(@@) :: (AsType a, AsType b) => a -> b -> Type
fun @@ arg = apply (asType fun) (asType arg)

-- | Field definition operator
-- Example: "name">: string
infixr 0 >:
(>:) :: AsType a => String -> a -> FieldType
name>: typ = field name (asType typ)


-- | Attach an annotation to a type
-- Example: annot (M.fromList [(Name "min", int32 0), (Name "max", int32 100)]) int32
annot :: AsType a => M.Map Name Term -> a -> Type
annot ann typ = TypeAnnotated $ AnnotatedType (asType typ) ann

-- | Apply a type to a type argument
-- Example: apply (var "f") int32
apply :: (AsType a, AsType b) => a -> b -> Type
apply lhs rhs = TypeApplication (ApplicationType (asType lhs) (asType rhs))

-- | Apply a type to multiple type arguments
-- Example: applys (var "Either") [string, int32]
applys :: AsType a => a -> [Type] -> Type
applys t ts = L.foldl apply (asType t) ts

-- | Arbitrary-precision floating point type
bigfloat :: Type
bigfloat = float FloatTypeBigfloat

-- | Arbitrary-precision integer type
bigint :: Type
bigint = integer IntegerTypeBigint

-- | Binary data type
binary :: Type
binary = literal LiteralTypeBinary

-- | Boolean type
boolean :: Type
boolean = literal LiteralTypeBoolean

-- | Create an either type (a choice between two types)
-- Example: either_ string int32
either_ :: (AsType a, AsType b) => a -> b -> Type
either_ left right = TypeEither $ EitherType (asType left) (asType right)

-- | Create an enum type with the given variant names (conventionally in camelCase)
-- Example: enum ["red", "green", "blue"]
enum :: [String] -> Type
enum names = union $ (`field` unit) <$> names

-- | Create a field with the given name and type
-- Example: field "age" int32
field :: AsType a => String -> a -> FieldType
field fn t = FieldType (Name fn) (asType t)

-- | Create a floating point type with the specified precision
-- Example: float FloatTypeFloat32
float :: FloatType -> Type
float = literal . LiteralTypeFloat

-- | 32-bit floating point type
float32 :: Type
float32 = float FloatTypeFloat32

-- | 64-bit floating point type
float64 :: Type
float64 = float FloatTypeFloat64

-- | Create a universally quantified type (polymorphic type) with a singe variable
-- Example: forAll "a" (var "a" --> var "a")
-- This creates the polymorphic identity function type: ∀a. a -> a
-- Universal quantification introduces type variables that can be used in the body
forAll :: AsType a => String -> a -> Type
forAll v body = TypeForall $ ForallType (Name v) (asType body)

-- | Universal quantification with multiple variables
-- Example: forAlls ["a", "b"] (var "a" --> var "b")
forAlls :: AsType a => [String] -> a -> Type
forAlls vs body = L.foldr forAll (asType body) vs

-- | Create a function type
-- Example: function int32 string
function :: (AsType a, AsType b) => a -> b -> Type
function dom cod = TypeFunction $ FunctionType (asType dom) (asType cod)

-- | Create an n-ary function type
-- Example: functionMany [int32, string, boolean]
functionMany :: [Type] -> Type
functionMany ts = L.foldl (\cod dom -> function dom cod) (L.head r) (L.tail r)
  where
    r = L.reverse ts

-- | 8-bit signed integer type
int8 :: Type
int8 = integer IntegerTypeInt8

-- | 16-bit signed integer type
int16 :: Type
int16 = integer IntegerTypeInt16

-- | 32-bit signed integer type
int32 :: Type
int32 = integer IntegerTypeInt32

-- | 64-bit signed integer type
int64 :: Type
int64 = integer IntegerTypeInt64

-- | Create an integer type with the specified bit width
-- Example: integer IntegerTypeInt32
integer :: IntegerType -> Type
integer = literal . LiteralTypeInteger

-- | List type
-- Example: list string
list :: AsType a => a -> Type
list = TypeList . asType

-- | Literal primitive type
-- Example: literal LiteralTypeString
literal :: LiteralType -> Type
literal = TypeLiteral

-- | Map/dictionary type with key and value types
-- Example: map string int32
map :: (AsType a, AsType b) => a -> b -> Type
map keys vals = TypeMap $ MapType (asType keys) (asType vals)

-- | Create a monomorphic type scheme
-- Example: mono int32
mono :: AsType a => a -> TypeScheme
mono = TypeScheme [] . asType

-- | Non-negative 32-bit integer type
-- Currently an alias for int32; intended for semantic annotation
-- In future versions, this may include validation constraints
nonNegativeInt32 :: Type
nonNegativeInt32 = int32

-- | Maybe (optional/nullable) type
-- Example: maybe string
maybe :: AsType a => a -> Type
maybe = TypeMaybe . asType

-- | Optional (nullable) type (alias for 'maybe')
-- Example: optional string
optional :: AsType a => a -> Type
optional = maybe

-- | Create a pair type
-- Example: pair string int32
pair :: (AsType a, AsType b) => a -> b -> Type
pair first second = TypePair $ PairType (asType first) (asType second)

-- | Create a polymorphic type scheme with explicit type variables
-- Example: poly ["a", "b"] (var "a" --> var "b")
-- This represents a type forall a b. a -> b that can be instantiated with different types
poly :: AsType a => [String] -> a -> TypeScheme
poly vs t = TypeScheme (Name <$> vs) (asType t)

-- | Create a product type using nested pairs (deprecated: use pair directly)
-- Example: product [string, int32, boolean] creates pair string (pair int32 boolean)
product :: [Type] -> Type
product [] = unit
product [a] = a
product [a, b] = pair a b
product (a:rest) = pair a (product rest)

-- | Create a record type with the given fields and the default type name
-- Example: record ["name">: string, "age">: int32]
-- Use 'recordWithName' to specify a custom type name
record :: [FieldType] -> Type
record = recordWithName placeholderName

-- | Create a record type with the given fields and a provided type name
-- Example: recordWithName (Name "Person") ["name" >: string, "age" >: int32]
recordWithName :: Name -> [FieldType] -> Type
recordWithName tname fields = TypeRecord $ RowType tname fields

-- | Set type
-- Example: set string
set :: AsType a => a -> Type
set = TypeSet . asType

-- | String type
string :: Type
string = literal LiteralTypeString

-- | 8-bit unsigned integer type
uint8 :: Type
uint8 = integer IntegerTypeUint8

-- | 16-bit unsigned integer type
uint16 :: Type
uint16 = integer IntegerTypeUint16

-- | 32-bit unsigned integer type
uint32 :: Type
uint32 = integer IntegerTypeUint32

-- | 64-bit unsigned integer type
uint64 :: Type
uint64 = integer IntegerTypeUint64

-- | Unit type (empty record type)
unit :: Type
unit = TypeUnit

-- | Create a union type with the given variants and the default type name
-- Example: union ["success">: int32, "failure">: string]
-- This creates a tagged union type (sum type with named variants)
-- Similar to sum [int32, string] but with named branches
union :: [FieldType] -> Type
union fields = TypeUnion $ RowType placeholderName fields

-- | Create a type variable with the given name
-- Example: variable "a"
variable :: String -> Type
variable = TypeVariable . Name

-- | Create a type variable with the given name (alias for 'variable')
-- Example: var "a"
var :: String -> Type
var = variable

-- | Create a wrapped type (newtype) with a provided base type and the default type name
-- Example: wrap string
-- Creates a newtype with placeholder name; use 'wrapWithName' for custom names
wrap :: AsType a => a -> Type
wrap = wrapWithName placeholderName

-- | Create a wrapped type (newtype) with a provided base type and type name
-- Example: wrapWithName (Name "Email") string
wrapWithName :: AsType a => Name -> a -> Type
wrapWithName name t = TypeWrap $ WrappedType name (asType t)
