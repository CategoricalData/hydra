-- | A domain-specific language for constructing Hydra types in Haskell.
module Hydra.Dsl.Types where

import Hydra.Core
import Hydra.Dsl.Common
import Hydra.Constants

import qualified Data.List as L
import qualified Data.Map as M


-- | Field definition operator
-- Example: "name">: string
infixr 0 >:
(>:) :: String -> Type -> FieldType
name >: typ = field name typ

-- | Function type constructor
-- Example: int32 --> string
infixr 0 -->
(-->) :: Type -> Type -> Type
dom --> cod = function dom cod

-- | Type application operator
-- Example: list @@ int32
(@@) :: Type -> Type -> Type
fun @@ arg = apply fun arg

-- | Attach an annotation to a type
-- Example: annot (M.fromList [(Name "min", int32 0), (Name "max", int32 100)]) int32
annot :: M.Map Name Term -> Type -> Type
annot ann typ = TypeAnnotated $ AnnotatedType typ ann

-- | Apply a type to a type argument
-- Example: apply (var "f") int32
apply :: Type -> Type -> Type
apply lhs rhs = TypeApplication (ApplicationType lhs rhs)

-- | Apply a type to multiple type arguments
-- Example: applyMany [var "Either", string, int32]
applyMany :: [Type] -> Type
applyMany ts = L.foldl apply (L.head ts) (L.tail ts)

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

-- | Create an enum type with the given variant names (conventionally in camelCase)
-- Example: enum ["red", "green", "blue"]
enum :: [String] -> Type
enum names = union $ (`field` unit) <$> names

-- | Create a field with the given name and type
-- Example: field "age" int32
field :: String -> Type -> FieldType
field fn = FieldType (Name fn)

-- | 32-bit floating point type
float32 :: Type
float32 = float FloatTypeFloat32

-- | 64-bit floating point type
float64 :: Type
float64 = float FloatTypeFloat64

-- | Create a floating point type with the specified precision
-- Example: float FloatTypeFloat32
float :: FloatType -> Type
float = literal . LiteralTypeFloat

-- | Universal quantification with a single variable
-- Example: forAll "a" (var "a" --> var "a")
forAll :: String -> Type -> Type
forAll v body = TypeForall $ ForallType (Name v) body

-- | Universal quantification with multiple variables
-- Example: forAlls ["a", "b"] (var "a" --> var "b")
forAlls :: [String] -> Type -> Type
forAlls vs body = L.foldr forAll body vs

-- | Create a function type
-- Example: function int32 string
function :: Type -> Type -> Type
function dom cod = TypeFunction $ FunctionType dom cod

-- | Create an n-ary function type
-- Example: functionMany [int32, string, boolean]
functionMany :: [Type] -> Type
functionMany ts = L.foldl (\cod dom -> function dom cod) (L.head r) (L.tail r)
  where
    r = L.reverse ts

-- | 16-bit signed integer type
int16 :: Type
int16 = integer IntegerTypeInt16

-- | 32-bit signed integer type
int32 :: Type
int32 = integer IntegerTypeInt32

-- | 64-bit signed integer type
int64 :: Type
int64 = integer IntegerTypeInt64

-- | 8-bit signed integer type
int8 :: Type
int8 = integer IntegerTypeInt8

-- | Create an integer type with the specified bit width
-- Example: integer IntegerTypeInt32
integer :: IntegerType -> Type
integer = literal . LiteralTypeInteger

-- | List type
-- Example: list string
list :: Type -> Type
list = TypeList

-- | Literal primitive type
-- Example: literal LiteralTypeString
literal :: LiteralType -> Type
literal = TypeLiteral

-- | Map/dictionary type with key and value types
-- Example: map string int32
map :: Type -> Type -> Type
map keys vals = TypeMap $ MapType keys vals

-- | Create a monomorphic type scheme
-- Example: mono int32
mono :: Type -> TypeScheme
mono = TypeScheme []

-- | Non-negative 32-bit integer type
nonNegativeInt32 :: Type
nonNegativeInt32 = int32

-- | Optional (nullable) type
-- Example: optional string
optional :: Type -> Type
optional = TypeOptional

-- | Create a pair (2-tuple) type
-- Example: pair string int32
pair :: Type -> Type -> Type
pair a b = TypeProduct [a, b]

-- | Create a polymorphic type scheme with type variables
-- Example: poly ["a", "b"] (var "a" --> var "b")
poly :: [String] -> Type -> TypeScheme
poly vs t = TypeScheme (Name <$> vs) t

-- | Create a product type (tuple) with multiple components
-- Example: product [string, int32, boolean]
product :: [Type] -> Type
product = TypeProduct

-- | Create a record type with the given fields and the default name
-- Example: record ["name">: string, "age">: int32]
record :: [FieldType] -> Type
record = recordWithName placeholderName

-- | Create a named record type with the given fields
-- Example: recordWithName (Name "Person") ["name" >: string, "age" >: int32]
recordWithName :: Name -> [FieldType] -> Type
recordWithName tname fields = TypeRecord $ RowType tname fields

-- | Set type
-- Example: set string
set :: Type -> Type
set = TypeSet

-- | String type
string :: Type
string = literal LiteralTypeString

-- | Create a sum type (disjoint union) with multiple variants
-- Example: sum [string, int32, boolean]
sum :: [Type] -> Type
sum = TypeSum

-- | 16-bit unsigned integer type
uint16 :: Type
uint16 = integer IntegerTypeUint16

-- | 32-bit unsigned integer type
uint32 :: Type
uint32 = integer IntegerTypeUint32

-- | 64-bit unsigned integer type
uint64 :: Type
uint64 = integer IntegerTypeUint64

-- | 8-bit unsigned integer type
uint8 :: Type
uint8 = integer IntegerTypeUint8

-- | Create a union type with the given variants
-- Example: union ["success">: int32, "failure" >: string]
union :: [FieldType] -> Type
union fields = TypeUnion $ RowType placeholderName fields

-- | Unit type (empty record type)
unit :: Type
unit = TypeRecord $ RowType (Name "hydra.core.Unit") []

-- | Create a type variable with the given name
-- Example: var "a"
var :: String -> Type
var = TypeVariable . Name

-- | Create a wrapped type (newtype) with the default name
-- Example: wrap string
wrap :: Type -> Type
wrap = wrapWithName placeholderName

-- | Create a named wrapped type (newtype)
-- Example: wrapWithName (Name "Email") string
wrapWithName :: Name -> Type -> Type
wrapWithName name t = TypeWrap $ WrappedType name t
