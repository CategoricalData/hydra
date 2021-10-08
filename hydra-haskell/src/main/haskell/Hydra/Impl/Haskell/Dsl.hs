module Hydra.Impl.Haskell.Dsl (
  DataError,
  SchemaError,
  apply,
  bigfloatType,
  bigintType,
  booleanTerm,
  booleanType,
  cases,
  compose,
  constFunction,
  deref,
  expectNArgs,
  expectRecordTerm,
  expectStringTerm,
  expectUnionTerm,
  fieldsToMap,
  fieldTypesToMap,
  float32Type,
  float64Type,
  floatType,
  funcRef,
  function,
  functionType,
  int16Type,
  int32Type,
  int32Value,
  int64Type,
  int64Value,
  int8Type,
  integerType,
  lambda,
  mapType,
  match,
  matchWithVariants,
  nominalType,
  requireField,
  stringTerm,
  stringType,
  stringValue,
  uint16Type,
  uint32Type,
  uint64Type,
  uint8Type,
  unitTerm,
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
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y
import Data.Int


type DataError = String
type SchemaError = String


apply :: Term -> Term -> Term
apply func arg = TermApplication $ Application func arg

bigfloatType :: Type
bigfloatType = floatType FloatTypeBigfloat

bigintType :: Type
bigintType = integerType IntegerTypeBigint

booleanTerm :: Bool -> Term
booleanTerm b = TermAtomic $ AtomicValueBoolean $ if b then BooleanValueTrue else BooleanValueFalse

booleanType :: Type
booleanType = TypeAtomic AtomicTypeBoolean

cases :: [Field] -> Term
cases = TermCases

compose :: Term -> Term -> Term
compose f2 f1 = lambda var $ apply f2 (apply f1 (variable var))
  where var = "x"

constFunction :: Term -> Term
constFunction = lambda "_"

deref :: Name -> Term
deref name = apply TermData $ TermElement name

expectNArgs :: Int -> [Term] -> Result ()
expectNArgs n args = if L.length args /= n
  then fail $ "expected " ++ show n ++ " arguments, but found " ++ show (L.length args)
  else pure ()

expectRecordTerm :: Term -> Result [Field]
expectRecordTerm term = case term of
  TermRecord fields -> pure fields
  _ -> fail $ "expected a record, got " ++ show term

expectStringTerm :: Term -> Result String
expectStringTerm term = case term of
  TermAtomic (AtomicValueString s) -> pure s
  _ -> fail $ "expected a string, got " ++ show term

expectUnionTerm :: Term -> Result Field
expectUnionTerm term = case term of
  TermUnion field -> pure field
  _ -> fail $ "expected a union, got " ++ show term

fieldsToMap :: [Field] -> M.Map FieldName Term
fieldsToMap fields = M.fromList $ (\(Field name term) -> (name, term)) <$> fields

fieldTypesToMap :: [FieldType] -> M.Map FieldName Type
fieldTypesToMap fields = M.fromList $ (\(FieldType name typ) -> (name, typ)) <$> fields

float32Type :: Type
float32Type = floatType FloatTypeFloat32

float64Type :: Type
float64Type = floatType FloatTypeFloat64

floatType :: FloatType -> Type
floatType = TypeAtomic . AtomicTypeFloat

funcRef :: Element -> Term
funcRef el = apply TermData $ TermElement $ elementName el

function :: Name -> Term
function = TermFunction

functionType :: Type -> Type -> Type
functionType dom cod = TypeFunction $ FunctionType dom cod

int8Type :: Type
int8Type = integerType IntegerTypeInt8

int16Type :: Type
int16Type = integerType IntegerTypeInt16

int32Type :: Type
int32Type = integerType IntegerTypeInt32

int32Value :: Int -> Term
int32Value = TermAtomic . AtomicValueInteger . IntegerValueInt32

int64Type :: Type
int64Type = integerType IntegerTypeInt64

int64Value :: Int64 -> Term
int64Value = TermAtomic . AtomicValueInteger . IntegerValueInt64

integerType :: IntegerType -> Type
integerType = TypeAtomic . AtomicTypeInteger

lambda :: Variable -> Term -> Term
lambda param body = TermLambda $ Lambda param body

mapType :: Type -> Type -> Type
mapType kt vt = TypeMap $ MapType kt vt

match :: [(FieldName, Term)] -> Term
match = cases . fmap toField
  where
    toField (name, term) = Field name term

matchWithVariants :: [(FieldName, FieldName)] -> Term
matchWithVariants = cases . fmap toField
  where
    toField (from, to) = Field from $ constFunction $ unitVariant to

nominalType :: Name -> Type
nominalType = TypeNominal

requireField :: M.Map FieldName Term -> FieldName -> Result Term
requireField fields fname = Y.maybe error ResultSuccess $ M.lookup fname fields
  where
    error = fail $ "no such field: " ++ fname

stringTerm :: String -> Term
stringTerm = TermAtomic . AtomicValueString

stringType :: Type
stringType = TypeAtomic AtomicTypeString

stringValue :: String -> Term
stringValue = TermAtomic . AtomicValueString

uint16Type :: Type
uint16Type = integerType IntegerTypeUint16

uint32Type :: Type
uint32Type = integerType IntegerTypeUint32

uint64Type :: Type
uint64Type = integerType IntegerTypeUint64

uint8Type :: Type
uint8Type = integerType IntegerTypeUint8

unitTerm :: Term
unitTerm = TermRecord []

unitType :: Type
unitType = TypeRecord []

unitVariant :: FieldName -> Term
unitVariant fname = variant fname unitTerm

variable :: Variable -> Term
variable = TermVariable

variant :: FieldName -> Term -> Term
variant fname term = TermUnion (Field fname term)

withFunction :: FieldName -> Element -> Term
withFunction name el = lambda var $ variant name $ apply (funcRef el) (variable var)
  where var = "x"

withVariant :: FieldName -> Term
withVariant name = constFunction $ unitVariant name
