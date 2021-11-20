module Hydra.Impl.Haskell.Dsl (
  DataError,
  Default(..),
  Meta(..),
  SchemaError,
  apply,
  atomic,
  bigfloatType,
  bigfloatValue,
  bigintType,
  bigintValue,
  binaryTerm,
  binaryType,
  booleanType,
  booleanValue,
  cases,
  compareTo,
  compose,
  constFunction,
  dataTerm,
  defaultTerm,
  deref,
  element,
  elementRef,
  expectAtomicValue,
  expectNArgs,
  expectRecordTerm,
  expectStringTerm,
  expectUnionTerm,
  fieldsToMap,
  fieldTypesToMap,
  float32Type,
  float32Value,
  float64Type,
  float64Value,
  floatType,
  floatValue,
  function,
  functionType,
  int16Type,
  int16Value,
  int32Type,
  int32Value,
  int64Type,
  int64Value,
  int8Type,
  int8Value,
  integerType,
  integerValue,
  lambda,
  list,
  listType,
  map,
  mapType,
  match,
  matchWithVariants,
  nominalType,
  optional,
  primitive,
  projection,
  record,
  requireField,
  set,
  stringTerm,
  stringType,
  stringValue,
  uint16Type,
  uint16Value,
  uint32Type,
  uint32Value,
  uint64Type,
  uint64Value,
  uint8Type,
  uint8Value,
  union,
  unitTerm,
  unitType,
  unitVariant,
  variable,
  variant,
  withFunction,
  withVariant,
) where

import Hydra.V2.Core
import Hydra.V2.Graph
import Hydra.Prototyping.Steps
import Prelude hiding (map)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import Data.Int


type DataError = String
class Default a where dflt :: a
type SchemaError = String
instance Default () where dflt = ()
data Meta = Meta deriving (Eq, Ord, Read, Show)
instance Default Meta where dflt = Meta

apply :: Default a => Term a -> Term a -> Term a
apply func arg = defaultTerm $ ExpressionApplication $ Application func arg

atomic :: Default a => AtomicValue -> Term a
atomic = defaultTerm . ExpressionAtomic

bigfloatType :: Type
bigfloatType = floatType FloatTypeBigfloat

bigfloatValue :: Default a => Double -> Term a
bigfloatValue = floatValue . FloatValueBigfloat

bigintType :: Type
bigintType = integerType IntegerTypeBigint

bigintValue :: Default a => Integer -> Term a
bigintValue = integerValue . IntegerValueBigint . fromIntegral

binaryTerm :: Default a => String -> Term a
binaryTerm = defaultTerm . ExpressionAtomic . AtomicValueBinary

binaryType :: Type
binaryType = TypeAtomic AtomicTypeBinary

booleanType :: Type
booleanType = TypeAtomic AtomicTypeBoolean

booleanValue :: Default a => Bool -> Term a
booleanValue b = defaultTerm $ ExpressionAtomic $ AtomicValueBoolean $ if b then BooleanValueTrue else BooleanValueFalse

cases :: Default a => [Field a] -> Term a
cases = defaultTerm . ExpressionFunction . FunctionCases

compareTo :: Default a => Term a -> Term a
compareTo = defaultTerm . ExpressionFunction . FunctionCompareTo

compose :: Default a => Term a -> Term a -> Term a
compose f2 f1 = lambda var $ apply f2 (apply f1 (variable var))
  where var = "x"

constFunction :: Default a => Term a -> Term a
constFunction = lambda "_"

dataTerm :: Default a => Term a
dataTerm = defaultTerm $ ExpressionFunction FunctionData

defaultTerm  :: Default a => Expression a -> Term a
defaultTerm e = Term e dflt

deref :: Default a => Name -> Term a
deref name = apply dataTerm $ defaultTerm $ ExpressionElement name

element :: Default a => Name -> Term a
element = defaultTerm . ExpressionElement

elementRef :: Default a => Element a -> Term a
elementRef el = apply dataTerm $ defaultTerm $ ExpressionElement $ elementName el

expectAtomicValue :: Show a => Term a -> Result AtomicValue
expectAtomicValue term = case termData term of
  ExpressionAtomic av -> pure av
  _ -> fail $ "expected an atomic value, got " ++ show term

expectNArgs :: Int -> [Term a] -> Result ()
expectNArgs n args = if L.length args /= n
  then fail $ "expected " ++ show n ++ " arguments, but found " ++ show (L.length args)
  else pure ()

expectRecordTerm :: Show a => Term a -> Result [Field a]
expectRecordTerm term = case termData term of
  ExpressionRecord fields -> pure fields
  _ -> fail $ "expected a record, got " ++ show term

expectStringTerm :: Show a => Term a -> Result String
expectStringTerm term = case termData term of
  ExpressionAtomic (AtomicValueString s) -> pure s
  _ -> fail $ "expected a string, got " ++ show term

expectUnionTerm :: Show a => Term a -> Result (Field a)
expectUnionTerm term = case termData term of
  ExpressionUnion field -> pure field
  _ -> fail $ "expected a union, got " ++ show term

fieldsToMap :: [Field a] -> M.Map FieldName (Term a)
fieldsToMap fields = M.fromList $ (\(Field name term) -> (name, term)) <$> fields

fieldTypesToMap :: [FieldType] -> M.Map FieldName Type
fieldTypesToMap fields = M.fromList $ (\(FieldType name typ) -> (name, typ)) <$> fields

float32Type :: Type
float32Type = floatType FloatTypeFloat32

float32Value :: Default a => Float -> Term a
float32Value = floatValue . FloatValueFloat32

float64Type :: Type
float64Type = floatType FloatTypeFloat64

float64Value :: Default a => Double -> Term a
float64Value = floatValue . FloatValueFloat64

floatType :: FloatType -> Type
floatType = TypeAtomic . AtomicTypeFloat

floatValue :: Default a => FloatValue -> Term a
floatValue = defaultTerm . ExpressionAtomic . AtomicValueFloat

function :: Default a => Name -> Term a
function = defaultTerm . ExpressionFunction . FunctionPrimitive

functionType :: Type -> Type -> Type
functionType dom cod = TypeFunction $ FunctionType dom cod

int16Type :: Type
int16Type = integerType IntegerTypeInt16

int16Value :: Default a => Int -> Term a
int16Value = integerValue . IntegerValueInt16 . fromIntegral

int32Type :: Type
int32Type = integerType IntegerTypeInt32

int32Value :: Default a => Int -> Term a
int32Value = integerValue . IntegerValueInt32

int64Type :: Type
int64Type = integerType IntegerTypeInt64

int64Value :: Default a => Int64 -> Term a
int64Value = integerValue . IntegerValueInt64

int8Type :: Type
int8Type = integerType IntegerTypeInt8

int8Value :: Default a => Int -> Term a
int8Value = integerValue . IntegerValueInt8 . fromIntegral

integerType :: IntegerType -> Type
integerType = TypeAtomic . AtomicTypeInteger

integerValue :: Default a => IntegerValue -> Term a
integerValue = defaultTerm . ExpressionAtomic . AtomicValueInteger

lambda :: Default a => Variable -> Term a -> Term a
lambda param body = defaultTerm $ ExpressionFunction $ FunctionLambda $ Lambda param body

list :: Default a => [Term a] -> Term a
list = defaultTerm . ExpressionList

listType :: Type -> Type
listType = TypeList

map :: Default a => M.Map (Term a) (Term a) -> Term a
map = defaultTerm . ExpressionMap

mapType :: Type -> Type -> Type
mapType kt vt = TypeMap $ MapType kt vt

match :: Default a => [(FieldName, Term a)] -> Term a
match = cases . fmap toField
  where
    toField (name, term) = Field name term

matchWithVariants :: Default a => [(FieldName, FieldName)] -> Term a
matchWithVariants = cases . fmap toField
  where
    toField (from, to) = Field from $ constFunction $ unitVariant to

nominalType :: Name -> Type
nominalType = TypeNominal

optional :: Default a => Y.Maybe (Term a) -> Term a
optional = defaultTerm . ExpressionOptional

primitive :: Default a => Name -> Term a
primitive = defaultTerm . ExpressionFunction . FunctionPrimitive

projection :: Default a => Name -> Term a
projection = defaultTerm . ExpressionFunction . FunctionProjection

record :: Default a => [Field a] -> Term a
record = defaultTerm . ExpressionRecord

requireField :: M.Map FieldName (Term a) -> FieldName -> Result (Term a)
requireField fields fname = Y.maybe error ResultSuccess $ M.lookup fname fields
  where
    error = fail $ "no such field: " ++ fname

set :: Default a => S.Set (Term a) -> Term a
set = defaultTerm . ExpressionSet

stringTerm :: Default a => String -> Term a
stringTerm = defaultTerm . ExpressionAtomic . AtomicValueString

stringType :: Type
stringType = TypeAtomic AtomicTypeString

stringValue :: Default a => String -> Term a
stringValue = defaultTerm . ExpressionAtomic . AtomicValueString

uint16Type :: Type
uint16Type = integerType IntegerTypeUint16

uint16Value :: Default a => Integer -> Term a
uint16Value = integerValue . IntegerValueUint16 . fromIntegral

uint32Type :: Type
uint32Type = integerType IntegerTypeUint32

uint32Value :: Default a => Integer -> Term a
uint32Value = integerValue . IntegerValueUint32 . fromIntegral

uint64Type :: Type
uint64Type = integerType IntegerTypeUint64

uint64Value :: Default a => Integer -> Term a
uint64Value = integerValue . IntegerValueUint64 . fromIntegral

uint8Type :: Type
uint8Type = integerType IntegerTypeUint8

uint8Value :: Default a => Integer -> Term a
uint8Value = integerValue . IntegerValueUint8 . fromIntegral

union :: Default a => Field a -> Term a
union = defaultTerm . ExpressionUnion

unitTerm :: Default a => Term a
unitTerm = defaultTerm $ ExpressionRecord []

unitType :: Type
unitType = TypeRecord []

unitVariant :: Default a => FieldName -> Term a
unitVariant fname = variant fname unitTerm

variable :: Default a => Variable -> Term a
variable = defaultTerm . ExpressionVariable

variant :: Default a => FieldName -> Term a -> Term a
variant fname term = defaultTerm $ ExpressionUnion (Field fname term)

withFunction :: Default a => FieldName -> Element a -> Term a
withFunction name el = lambda var $ variant name $ apply (elementRef el) (variable var)
  where var = "x"

withVariant :: Default a => FieldName -> Term a
withVariant name = constFunction $ unitVariant name
