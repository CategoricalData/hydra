module Hydra.Impl.Haskell.Dsl.Types where

import Hydra.Core
import qualified Data.Map as M


bigfloat :: Type
bigfloat = float FloatTypeBigfloat

bigint :: Type
bigint = integer IntegerTypeBigint

binary :: Type
binary = literal LiteralTypeBinary

boolean :: Type
boolean = literal LiteralTypeBoolean

element :: Type -> Type
element = TypeElement

enum :: [FieldName] -> Type
enum names = union $ (`field` unit) <$> names

field :: FieldName -> Type -> FieldType
field = FieldType

fieldsToMap :: [FieldType] -> M.Map FieldName Type
fieldsToMap fields = M.fromList $ (\(FieldType name typ) -> (name, typ)) <$> fields

float32 :: Type
float32 = float FloatTypeFloat32

float64 :: Type
float64 = float FloatTypeFloat64

float :: FloatType -> Type
float = literal . LiteralTypeFloat

function :: Type -> Type -> Type
function dom cod = TypeFunction $ FunctionType dom cod

int16 :: Type
int16 = integer IntegerTypeInt16

int32 :: Type
int32 = integer IntegerTypeInt32

int64 :: Type
int64 = integer IntegerTypeInt64

int8 :: Type
int8 = integer IntegerTypeInt8

integer :: IntegerType -> Type
integer = literal . LiteralTypeInteger

list :: Type -> Type
list = TypeList

literal :: LiteralType -> Type
literal = TypeLiteral

map :: Type -> Type -> Type
map kt vt = TypeMap $ MapType kt vt

nominal :: Name -> Type
nominal = TypeNominal

optional :: Type -> Type
optional = TypeOptional

record :: [FieldType] -> Type
record = TypeRecord

set :: Type -> Type
set = TypeSet

string :: Type
string = literal LiteralTypeString

variable :: TypeVariable -> Type
variable = TypeVariable

uint16 :: Type
uint16 = integer IntegerTypeUint16

uint32 :: Type
uint32 = integer IntegerTypeUint32

uint64 :: Type
uint64 = integer IntegerTypeUint64

uint8 :: Type
uint8 = integer IntegerTypeUint8

union :: [FieldType] -> Type
union = TypeUnion

unit :: Type
unit = record []

universal :: TypeVariable -> Type -> Type
universal v body = TypeUniversal $ UniversalType v body
