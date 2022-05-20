module Hydra.Impl.Haskell.Dsl.Types where

import Hydra.Core
import Hydra.Impl.Haskell.Default

import qualified Data.Map as M


bigfloat :: Default m => Type m
bigfloat = float FloatTypeBigfloat

bigint :: Default m => Type m
bigint = integer IntegerTypeBigint

binary :: Default m => Type m
binary = literal LiteralTypeBinary

boolean :: Default m => Type m
boolean = literal LiteralTypeBoolean

defaultType  :: Default m => TypeTerm m -> Type m
defaultType e = Type e dflt

element :: Default m => Type m -> Type m
element = defaultType . TypeTermElement

enum :: Default m => [FieldName] -> Type m
enum names = union $ (`field` unit) <$> names

field :: Default m => FieldName -> Type m -> FieldType m
field = FieldType

fieldsToMap :: Default m => [FieldType m] -> M.Map FieldName (Type m)
fieldsToMap fields = M.fromList $ (\(FieldType name typ) -> (name, typ)) <$> fields

float32 :: Default m => Type m
float32 = float FloatTypeFloat32

float64 :: Default m => Type m
float64 = float FloatTypeFloat64

float :: Default m => FloatType -> Type m
float = literal . LiteralTypeFloat

function :: Default m => Type m -> Type m -> Type m
function dom cod = defaultType $ TypeTermFunction $ FunctionType dom cod

int16 :: Default m => Type m
int16 = integer IntegerTypeInt16

int32 :: Default m => Type m
int32 = integer IntegerTypeInt32

int64 :: Default m => Type m
int64 = integer IntegerTypeInt64

int8 :: Default m => Type m
int8 = integer IntegerTypeInt8

integer :: Default m => IntegerType -> Type m
integer = literal . LiteralTypeInteger

list :: Default m => Type m -> Type m
list = defaultType . TypeTermList

literal :: Default m => LiteralType -> Type m
literal = defaultType . TypeTermLiteral

map :: Default m => Type m -> Type m -> Type m
map kt vt = defaultType $ TypeTermMap $ MapType kt vt

nominal :: Default m => Name -> Type m
nominal = defaultType . TypeTermNominal

optional :: Default m => Type m -> Type m
optional = defaultType . TypeTermOptional

record :: Default m => [FieldType m] -> Type m
record = defaultType . TypeTermRecord

set :: Default m => Type m -> Type m
set = defaultType . TypeTermSet

string :: Default m => Type m
string = literal LiteralTypeString

variable :: Default m => String -> Type m
variable = defaultType . TypeTermVariable . TypeVariable

uint16 :: Default m => Type m
uint16 = integer IntegerTypeUint16

uint32 :: Default m => Type m
uint32 = integer IntegerTypeUint32

uint64 :: Default m => Type m
uint64 = integer IntegerTypeUint64

uint8 :: Default m => Type m
uint8 = integer IntegerTypeUint8

union :: Default m => [FieldType m] -> Type m
union = defaultType . TypeTermUnion

unit :: Default m => Type m
unit = record []

universal :: Default m => String -> Type m -> Type m
universal v body = defaultType $ TypeTermUniversal $ UniversalType (TypeVariable v) body
