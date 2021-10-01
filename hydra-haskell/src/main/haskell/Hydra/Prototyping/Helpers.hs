module Hydra.Prototyping.Helpers (
  apply,
  bigfloatType,
  bigintType,
  booleanTerm,
  booleanType,
  cases,
  compose,
  constFunction,
  deref,
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


apply func arg = TermApplication $ Application func arg

bigfloatType = floatType FloatTypeBigfloat
bigintType = integerType IntegerTypeBigint

booleanTerm :: Bool -> Term
booleanTerm b = TermAtomic $ AtomicValueBoolean $ if b then BooleanValueTrue else BooleanValueFalse

booleanType :: Type
booleanType = TypeAtomic AtomicTypeBoolean

cases :: [Field] -> Term
cases = TermCases

compose :: Term -> Term -> Term
compose f2 f1 = lambda var $ apply f2 $ (apply f1 (variable var))
  where var = "x"

constFunction :: Term -> Term
constFunction term = lambda "_" term

deref :: Name -> Term
deref name = apply TermData $ TermElement name

float32Type = floatType FloatTypeFloat32
float64Type = floatType FloatTypeFloat64
floatType = TypeAtomic . AtomicTypeFloat

funcRef :: Element -> Term
funcRef el = apply TermData $ TermElement $ elementName el

function :: Name -> Term
function = TermFunction

functionType :: Type -> Type -> Type
functionType dom cod = TypeFunction $ FunctionType dom cod

int8Type = integerType IntegerTypeInt8
int16Type = integerType IntegerTypeInt16
int32Type = integerType IntegerTypeInt32
int32Value = TermAtomic . AtomicValueInteger . IntegerValueInt32
int64Type = integerType IntegerTypeInt64
int64Value = TermAtomic . AtomicValueInteger . IntegerValueInt64
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

stringTerm :: String -> Term
stringTerm = TermAtomic . AtomicValueString

stringType :: Type
stringType = TypeAtomic AtomicTypeString

stringValue :: String -> Term
stringValue = TermAtomic . AtomicValueString

uint16Type = integerType IntegerTypeUint16
uint32Type = integerType IntegerTypeUint32
uint64Type = integerType IntegerTypeUint64
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
