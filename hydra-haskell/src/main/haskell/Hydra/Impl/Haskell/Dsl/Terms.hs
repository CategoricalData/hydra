module Hydra.Impl.Haskell.Dsl.Terms (
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
  element,
  elementRef,
  elementRefByName,
  elementType,
  expectInt32Term,
  expectLiteral,
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
  letTerm,
  list,
  listType,
  map,
  mapTerm,
  mapType,
  match,
  matchWithVariants,
  nominal,
  nominalType,
  optional,
  optionalType,
  primitive,
  projection,
  record,
  recordType,
  requireField,
  set,
  setType,
  stringTerm,
  stringType,
  stringValue,
  typeVariable,
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
  unionType,
  unitType,
  unitVariant,
  variable,
  variant,
  withFunction,
  withVariant,
) where

import Hydra.Core
import Hydra.Graph
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
instance Default Meta where dflt = Meta {
  metaDescription = Nothing,
  metaTypeAnnotation = Nothing}

apply :: Default a => Term a -> Term a -> Term a
apply func arg = defaultTerm $ ExpressionApplication $ Application func arg

atomic :: Default a => Literal -> Term a
atomic = defaultTerm . ExpressionLiteral

bigfloatType :: Type
bigfloatType = floatType FloatTypeBigfloat

bigfloatValue :: Default a => Double -> Term a
bigfloatValue = floatValue . FloatValueBigfloat

bigintType :: Type
bigintType = integerType IntegerTypeBigint

bigintValue :: Default a => Integer -> Term a
bigintValue = integerValue . IntegerValueBigint . fromIntegral

binaryTerm :: Default a => String -> Term a
binaryTerm = defaultTerm . ExpressionLiteral . LiteralBinary

binaryType :: Type
binaryType = TypeLiteral LiteralTypeBinary

booleanType :: Type
booleanType = TypeLiteral LiteralTypeBoolean

booleanValue :: Default a => Bool -> Term a
booleanValue b = defaultTerm $ ExpressionLiteral $ LiteralBoolean $ if b then BooleanValueTrue else BooleanValueFalse

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

element :: Default a => Name -> Term a
element = defaultTerm . ExpressionElement

elementRef :: Default a => Element a -> Term a
elementRef el = apply dataTerm $ defaultTerm $ ExpressionElement $ elementName el

elementRefByName :: Default a => Name -> Term a
elementRefByName name = apply dataTerm $ defaultTerm $ ExpressionElement name

elementType :: Type -> Type
elementType = TypeElement

expectInt32Term :: Show a => Term a -> Result Int
expectInt32Term term = case termData term of
  ExpressionLiteral (LiteralInteger (IntegerValueInt32 v)) -> pure v
  _ -> fail $ "expected an int32, got " ++ show term

expectLiteral :: Show a => Term a -> Result Literal
expectLiteral term = case termData term of
  ExpressionLiteral av -> pure av
  _ -> fail $ "expected a literal value, got " ++ show term

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
  ExpressionLiteral (LiteralString s) -> pure s
  _ -> fail $ "expected a string, got " ++ show term

expectUnionTerm :: Show a => Term a -> Result (Field a)
expectUnionTerm term = case termData term of
  ExpressionUnion (UnionExpression _ field) -> pure field
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
floatType = TypeLiteral . LiteralTypeFloat

floatValue :: Default a => FloatValue -> Term a
floatValue = defaultTerm . ExpressionLiteral . LiteralFloat

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
integerType = TypeLiteral . LiteralTypeInteger

integerValue :: Default a => IntegerValue -> Term a
integerValue = defaultTerm . ExpressionLiteral . LiteralInteger

lambda :: Default a => Variable -> Term a -> Term a
lambda param body = defaultTerm $ ExpressionFunction $ FunctionLambda $ Lambda param body

letTerm :: Default a => Variable -> Term a -> Term a -> Term a
letTerm v t1 t2 = defaultTerm $ ExpressionLet $ Let v t1 t2

list :: Default a => [Term a] -> Term a
list = defaultTerm . ExpressionList

listType :: Type -> Type
listType = TypeList

map :: Default a => M.Map (Term a) (Term a) -> Term a
map = defaultTerm . ExpressionMap

mapTerm :: Default a => M.Map (Term a) (Term a) -> Term a
mapTerm = defaultTerm . ExpressionMap

mapType :: Type -> Type -> Type
mapType kt vt = TypeMap $ MapType kt vt

match :: Default a => [(FieldName, Term a)] -> Term a
match = cases . fmap toField
  where
    toField (name, term) = Field name term

matchWithVariants :: Default a => Name -> [(FieldName, FieldName)] -> Term a
matchWithVariants sname = cases . fmap toField
  where
    toField (from, to) = Field from $ constFunction $ unitVariant sname to

nominal :: Default a => Name -> Term a -> Term a
nominal name term = defaultTerm $ ExpressionNominal $ NominalTerm name term

nominalType :: Name -> Type
nominalType = TypeNominal

optional :: Default a => Y.Maybe (Term a) -> Term a
optional = defaultTerm . ExpressionOptional

optionalType :: Type -> Type
optionalType = TypeOptional

primitive :: Default a => Name -> Term a
primitive = defaultTerm . ExpressionFunction . FunctionPrimitive

projection :: Default a => Projection -> Term a
projection = defaultTerm . ExpressionFunction . FunctionProjection

record :: Default a => [Field a] -> Term a
record = defaultTerm . ExpressionRecord

recordType :: [FieldType] -> Type
recordType = TypeRecord

requireField :: M.Map FieldName (Term a) -> FieldName -> Result (Term a)
requireField fields fname = Y.maybe error ResultSuccess $ M.lookup fname fields
  where
    error = fail $ "no such field: " ++ fname

set :: Default a => S.Set (Term a) -> Term a
set = defaultTerm . ExpressionSet

setType :: Type -> Type
setType = TypeSet

stringTerm :: Default a => String -> Term a
stringTerm = defaultTerm . ExpressionLiteral . LiteralString

stringType :: Type
stringType = TypeLiteral LiteralTypeString

stringValue :: Default a => String -> Term a
stringValue = defaultTerm . ExpressionLiteral . LiteralString

typeVariable :: TypeVariable -> Type
typeVariable = TypeVariable

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

union :: Default a => Name -> Field a -> Term a
union sname field = defaultTerm $ ExpressionUnion $ UnionExpression sname field

unitTerm :: Default a => Term a
unitTerm = defaultTerm $ ExpressionRecord []

unionType :: [FieldType] -> Type
unionType = TypeUnion

unitType :: Type
unitType = TypeRecord []

unitVariant :: Default a => Name -> FieldName -> Term a
unitVariant sname fname = variant sname fname unitTerm

variable :: Default a => Variable -> Term a
variable = defaultTerm . ExpressionVariable

variant :: Default a => Name -> FieldName -> Term a -> Term a
variant sname fname term = defaultTerm $ ExpressionUnion $ UnionExpression sname (Field fname term)

withFunction :: Default a => Name -> FieldName -> Element a -> Term a
withFunction sname name el = lambda var $ variant sname name $ apply (elementRef el) (variable var)
  where var = "x"

withVariant :: Default a => Name -> FieldName -> Term a
withVariant sname name = constFunction $ unitVariant sname name
