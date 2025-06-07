-- | A DSL for decoding and validating Hydra terms at runtime.
-- This module provides functions to extract typed values from Hydra terms
-- with appropriate error handling.
module Hydra.Dsl.Expect where

import Hydra.Compute
import Hydra.Core
import Hydra.Graph
import Hydra.Strip
import Hydra.Rewriting
import Hydra.Errors
import Hydra.Lexical
import qualified Hydra.Lib.Flows as Flows

import Prelude hiding (map)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Control.Monad as CM
import Data.Int

-- * Validation

-- | Ensure a function has the expected number of arguments
-- Example: nArgs _add 2 args
nArgs :: Name -> Int -> [Term] -> Flow s ()
nArgs name n args = if L.length args /= n
  then unexpected (show n ++ " arguments to primitive " ++ show (unName name)) $ show (L.length args)
  else pure ()

-- * Literal values

-- | Extract a binary data value from a term
-- Example: binary term
binary :: Term -> Flow Graph String
binary t = literal t >>= binaryLiteral

-- | Extract a binary literal from a Literal value
-- Example: literal term >>= binaryLiteral
binaryLiteral :: Literal -> Flow Graph String
binaryLiteral v = case v of
  LiteralBinary b -> pure b
  _ -> unexpected "binary" $ show v

-- | Extract a boolean value from a term
-- Example: boolean term
boolean :: Term -> Flow Graph Bool
boolean t = literal t >>= booleanLiteral

-- | Extract a boolean literal from a Literal value
-- Example: literal term >>= booleanLiteral
booleanLiteral :: Literal -> Flow Graph Bool
booleanLiteral v = case v of
  LiteralBoolean b -> pure b
  _ -> unexpected "boolean" $ show v

-- | Extract a literal value from a term
-- Example: literal term >>= stringLiteral
literal :: Term -> Flow Graph Literal
literal term0 = do
  term <- stripAndDereferenceTerm term0
  case term of
    TermLiteral lit -> pure lit
    _ -> unexpected "literal" $ show term

-- | Extract a string value from a term
-- Example: string term
string :: Term -> Flow Graph String
string t = literal t >>= stringLiteral

-- | Extract a string literal from a Literal value
-- Example: literal term >>= stringLiteral
stringLiteral :: Literal -> Flow Graph String
stringLiteral v = case v of
  LiteralString s -> pure s
  _ -> unexpected "string" $ show v

-- ** Integers

-- | Extract an arbitrary-precision integer value from a term
-- Example: bigint term
bigint :: Term -> Flow Graph Integer
bigint t = literal t >>= integerLiteral >>= bigintValue

-- | Extract a bigint value from an IntegerValue
-- Example: integerLiteral lit >>= bigintValue
bigintValue :: IntegerValue -> Flow Graph Integer
bigintValue v = case v of
  IntegerValueBigint i -> pure i
  _ -> unexpected "bigint" $ show v

-- | Extract an integer literal from a Literal value
-- Example: literal term >>= integerLiteral
integerLiteral :: Literal -> Flow Graph IntegerValue
integerLiteral lit = case lit of
  LiteralInteger v -> pure v
  _ -> unexpected "integer value" $ show lit

-- | Extract an 8-bit signed integer value from a term
-- Example: int8 term
int8 :: Term -> Flow Graph Int8
int8 t = literal t >>= integerLiteral >>= int8Value

-- | Extract an int8 value from an IntegerValue
-- Example: integerLiteral lit >>= int8Value
int8Value :: IntegerValue -> Flow Graph Int8
int8Value v = case v of
  IntegerValueInt8 i -> pure i
  _ -> unexpected "int8" $ show v

-- | Extract a 16-bit signed integer value from a term
-- Example: int16 term
int16 :: Term -> Flow Graph Int16
int16 t = literal t >>= integerLiteral >>= int16Value

-- | Extract an int16 value from an IntegerValue
-- Example: integerLiteral lit >>= int16Value
int16Value :: IntegerValue -> Flow Graph Int16
int16Value v = case v of
  IntegerValueInt16 i -> pure i
  _ -> unexpected "int16" $ show v

-- | Extract a 32-bit signed integer value from a term
-- Example: int32 term
int32 :: Term -> Flow Graph Int
int32 t = literal t >>= integerLiteral >>= int32Value

-- | Extract an int32 value from an IntegerValue
-- Example: integerLiteral lit >>= int32Value
int32Value :: IntegerValue -> Flow Graph Int
int32Value v = case v of
  IntegerValueInt32 i -> pure i
  _ -> unexpected "int32" $ show v

-- | Extract a 64-bit signed integer value from a term
-- Example: int64 term
int64 :: Term -> Flow Graph Int64
int64 t = literal t >>= integerLiteral >>= int64Value

-- | Extract an int64 value from an IntegerValue
-- Example: integerLiteral lit >>= int64Value
int64Value :: IntegerValue -> Flow Graph Int64
int64Value v = case v of
  IntegerValueInt64 i -> pure i
  _ -> unexpected "int64" $ show v

-- | Extract an 8-bit unsigned integer value from a term
-- Example: uint8 term
uint8 :: Term -> Flow Graph Int16
uint8 t = literal t >>= integerLiteral >>= uint8Value

-- | Extract a uint8 value from an IntegerValue
-- Example: integerLiteral lit >>= uint8Value
uint8Value :: IntegerValue -> Flow Graph Int16
uint8Value v = case v of
  IntegerValueUint8 i -> pure i
  _ -> unexpected "uint8" $ show v

-- | Extract a 16-bit unsigned integer value from a term
-- Example: uint16 term
uint16 :: Term -> Flow Graph Int
uint16 t = literal t >>= integerLiteral >>= uint16Value

-- | Extract a uint16 value from an IntegerValue
-- Example: integerLiteral lit >>= uint16Value
uint16Value :: IntegerValue -> Flow Graph Int
uint16Value v = case v of
  IntegerValueUint16 i -> pure i
  _ -> unexpected "uint16" $ show v

-- | Extract a 32-bit unsigned integer value from a term
-- Example: uint32 term
uint32 :: Term -> Flow Graph Int64
uint32 t = literal t >>= integerLiteral >>= uint32Value

-- | Extract a uint32 value from an IntegerValue
-- Example: integerLiteral lit >>= uint32Value
uint32Value :: IntegerValue -> Flow Graph Int64
uint32Value v = case v of
  IntegerValueUint32 i -> pure i
  _ -> unexpected "uint32" $ show v

-- | Extract a 64-bit unsigned integer value from a term
-- Example: uint64 term
uint64 :: Term -> Flow Graph Integer
uint64 t = literal t >>= integerLiteral >>= uint64Value

-- | Extract a uint64 value from an IntegerValue
-- Example: integerLiteral lit >>= uint64Value
uint64Value :: IntegerValue -> Flow Graph Integer
uint64Value v = case v of
  IntegerValueUint64 i -> pure i
  _ -> unexpected "uint64" $ show v

-- ** Floating-point values

-- | Extract an arbitrary-precision floating-point value from a term
-- Example: bigfloat term
bigfloat :: Term -> Flow Graph Double
bigfloat t = literal t >>= floatLiteral >>= bigfloatValue

-- | Extract a bigfloat value from a FloatValue
-- Example: floatLiteral lit >>= bigfloatValue
bigfloatValue :: FloatValue -> Flow Graph Double
bigfloatValue v = case v of
  FloatValueBigfloat f -> pure f
  _ -> unexpected "bigfloat" $ show v

-- | Extract a floating-point literal from a Literal value
-- Example: literal term >>= floatLiteral
floatLiteral :: Literal -> Flow Graph FloatValue
floatLiteral lit = case lit of
  LiteralFloat v -> pure v
  _ -> unexpected "floating-point value" $ show lit

-- | Extract a 32-bit floating-point value from a term
-- Example: float32 term
float32 :: Term -> Flow Graph Float
float32 t = literal t >>= floatLiteral >>= float32Value

-- | Extract a float32 value from a FloatValue
-- Example: floatLiteral lit >>= float32Value
float32Value :: FloatValue -> Flow Graph Float
float32Value v = case v of
  FloatValueFloat32 f -> pure f
  _ -> unexpected "float32" $ show v

-- | Extract a 64-bit floating-point value from a term
-- Example: float64 term
float64 :: Term -> Flow Graph Double
float64 t = literal t >>= floatLiteral >>= float64Value

-- | Extract a float64 value from a FloatValue
-- Example: floatLiteral lit >>= float64Value
float64Value :: FloatValue -> Flow Graph Double
float64Value v = case v of
  FloatValueFloat64 f -> pure f
  _ -> unexpected "float64" $ show v

-- * Collections

-- | Extract a list of values from a term, mapping a function over each element
-- Example: list string term
list :: (Term -> Flow Graph x) -> Term -> Flow Graph [x]
list f term0 = do
  term <- stripAndDereferenceTerm term0
  case term of
    TermList l -> CM.mapM f l
    _ -> unexpected "list" $ show term

-- | Extract the first element of a list term
-- Example: listHead term
listHead :: Term -> Flow Graph Term
listHead term = do
  l <- list pure term
  if L.null l
    then fail "empty list"
    else pure $ L.head l

-- | Extract the element type from a list type
-- Example: listType typ
listType :: Type -> Flow s Type
listType typ = case stripType typ of
  TypeList t -> pure t
  _ -> unexpected "list type" $ show typ

-- | Extract a map of key-value pairs from a term, mapping functions over each key and value
-- Example: map string int32 term
map :: Ord k => (Term -> Flow Graph k) -> (Term -> Flow Graph v) -> Term -> Flow Graph (M.Map k v)
map fk fv term0 = do
  term <- stripAndDereferenceTerm term0
  case term of
    TermMap m -> M.fromList <$> CM.mapM pair (M.toList m)
      where
        pair (kterm, vterm) = do
          kval <- fk kterm
          vval <- fv vterm
          return (kval, vval)
    _ -> unexpected "map" $ show term

-- | Extract the key and value types from a map type
-- Example: mapType typ
mapType :: Type -> Flow s MapType
mapType typ = case stripType typ of
  TypeMap mt -> pure mt
  _ -> unexpected "map type" $ show typ

-- | Extract an optional value from a term, applying a function to the value if present
-- Example: optional string term
optional :: (Term -> Flow Graph x) -> Term -> Flow Graph (Y.Maybe x)
optional f term0 = do
  term <- stripAndDereferenceTerm term0
  case term of
    TermOptional mt -> case mt of
      Nothing -> pure Nothing
      Just t -> Just <$> f t
    _ -> unexpected "optional value" $ show term

-- | Extract the base type from an optional type
-- Example: optionalType typ
optionalType :: Type -> Flow s Type
optionalType typ = case stripType typ of
  TypeOptional t -> pure t
  _ -> unexpected "optional type" $ show typ

-- | Extract a set of values from a term, mapping a function over each element
-- Example: set string term
set :: Ord x => (Term -> Flow Graph x) -> Term -> Flow Graph (S.Set x)
set f term0 = do
  term <- stripAndDereferenceTerm term0
  case term of
    TermSet s -> S.fromList <$> CM.mapM f (S.toList s)
    _ -> unexpected "set" $ show term

-- | Extract the element type from a set type
-- Example: setType typ
setType :: Type -> Flow s Type
setType typ = case stripType typ of
  TypeSet t -> pure t
  _ -> unexpected "set type" $ show typ

-- * Products and sums

-- | Extract a pair of values from a term, applying functions to each component
-- Example: pair string int32 term
pair :: (Term -> Flow Graph k) -> (Term -> Flow Graph v) -> Term -> Flow Graph (k, v)
pair kf vf term0 = do
  term <- stripAndDereferenceTerm term0
  case term of
    TermProduct terms -> case terms of
      [kTerm, vTerm] -> do
        kVal <- kf kTerm
        vVal <- vf vTerm
        return (kVal, vVal)
      _ -> unexpected "pair" $ show term
    _ -> unexpected "product" $ show term

-- | Extract the component types from a product type
-- Example: productType typ
productType :: Type -> Flow s [Type]
productType typ = case stripType typ of
  TypeProduct types -> pure types
  _ -> unexpected "product type" $ show typ

-- | Extract the component types from a sum type
-- Example: sumType typ
sumType :: Type -> Flow s [Type]
sumType typ = case stripType typ of
  TypeSum types -> pure types
  _ -> unexpected "sum type" $ show typ

-- * Function types

-- | Extract a function type from a type
-- Example: functionType typ
functionType :: Type -> Flow s FunctionType
functionType typ = case stripType typ of
  TypeFunction ft -> pure ft
  _ -> unexpected "function type" $ show typ

-- | Extract a lambda from a term
-- Example: lambda term
lambda :: Term -> Flow Graph Lambda
lambda term0 = do
  term <- stripAndDereferenceTerm term0
  case term of
    TermFunction (FunctionLambda l) -> pure l
    _ -> unexpected "lambda" $ show term

-- | Extract the body of a lambda term
-- Example: lambdaBody term
lambdaBody :: Term -> Flow Graph Term
lambdaBody term = Hydra.Core.lambdaBody <$> lambda term

-- * Let expressions

-- | Extract a let expression from a term
-- Example: letTerm term
letTerm :: Term -> Flow Graph Let
letTerm term0 = do
  term <- stripAndDereferenceTerm term0
  case term of
    TermLet lt -> pure lt
    _ -> unexpected "let term" $ show term

-- | Extract a binding with the given name from a let term
-- Example: letBinding "x" term
letBinding :: String -> Term -> Flow Graph Term
letBinding n term = do
  bindings <- letBindings <$> letTerm term
  case L.filter (\b -> letBindingName b == Name n) bindings of
    [] -> fail $ "no such binding: " ++ n
    [b] -> pure $ letBindingTerm b
    _ -> fail $ "multiple bindings named " ++ n

-- * Records, unions, and newtypes

-- | Extract case statement from a term
-- Example: cases (Name "Result") term
cases :: Name -> Term -> Flow Graph CaseStatement
cases name term0 = do
  term <- stripAndDereferenceTerm term0
  case term of
    TermFunction (FunctionElimination (EliminationUnion cs)) -> if caseStatementTypeName cs == name
      then pure cs
      else unexpected ("case statement for type " ++ unName name) $ show term
    _ -> unexpected "case statement" $ show term

-- | Extract a specific case handler from a case statement term
-- Example: caseField (Name "Result") "success" term
caseField :: Name -> String -> Term -> Flow Graph Field
caseField name n term = do
  cs <- cases name term
  let matching = L.filter (\f -> fieldName f == Name n) $ caseStatementCases cs
  if L.null matching
    then fail $ "not enough cases"
    else pure $ L.head matching

-- | Extract a field value from a list of fields
-- Example: field (Name "name") string fields
field :: Name -> (Term -> Flow Graph x) -> [Field] -> Flow Graph x
field fname mapping fields = case L.filter (\f -> fieldName f == fname) fields of
  [] -> fail $ "field " ++ unName fname ++ " not found"
  [f] -> (stripAndDereferenceTerm $ fieldTerm f) >>= mapping
  _ -> fail $ "multiple fields named " ++ unName fname

-- | Extract a record's fields from a term
-- Example: record (Name "Person") term
record :: Name -> Term -> Flow Graph [Field]
record expected term0 = do
  term <- stripAndDereferenceTerm term0
  case term of
    TermRecord (Record actual fields) -> if actual == expected
      then pure fields
      else unexpected ("record of type " ++ unName expected) (unName actual)
    _ -> unexpected "record" $ show term

-- | Extract the field types from a record type
-- Example: recordType (Name "Person") typ
recordType :: Name -> Type -> Flow s [FieldType]
recordType ename typ = case stripType typ of
  TypeRecord (RowType tname fields) -> if tname == ename
    then pure fields
    else unexpected ("record of type " ++ unName ename) $ "record of type " ++ unName tname
  _ -> unexpected "record type" $ show typ

-- | Extract a unit value (empty record) from a term
-- Example: unit term
unit :: Term -> Flow Graph ()
unit term0 = do
  term <- stripAndDereferenceTerm term0
  fields <- record _Unit term
  if L.null fields
    then pure ()
    else unexpected "unit" $ show term

-- | Extract a field from a union term
-- Example: injection (Name "Result") term
injection :: Name -> Term -> Flow Graph Field
injection expected term0 = do
  term <- stripAndDereferenceTerm term0
  case term of
    TermUnion (Injection actual field) -> if actual == expected
      then pure field
      else unexpected ("injection of type " ++ unName expected) (unName actual)
    _ -> unexpected "injection" $ show term

-- | Extract a field from a union term (alias for injection)
-- Example: variant (Name "Result") term
variant :: Name -> Term -> Flow Graph Field
variant = injection

-- | Extract a unit variant (a variant with an empty record value) from a union term
-- Example: unitVariant (Name "Option") term
unitVariant :: Name -> Term -> Flow Graph Name
unitVariant tname term = do
  field <- variant tname term
  unit $ fieldTerm field
  pure $ fieldName field

-- | Extract the field types from a union type
-- Example: unionType (Name "Result") typ
unionType :: Name -> Type -> Flow s [FieldType]
unionType ename typ = case stripType typ of
  TypeUnion (RowType tname fields) -> if tname == ename
    then pure fields
    else unexpected ("union of type " ++ unName ename) $ "union of type " ++ unName tname
  _ -> unexpected "union type" $ show typ

-- | Extract the wrapped value from a wrapped term
-- Example: wrap (Name "Email") term
wrap :: Name -> Term -> Flow Graph Term
wrap expected term0 = do
  term <- stripAndDereferenceTerm term0
  case term of
    TermWrap (WrappedTerm actual term) -> if actual == expected
      then pure term
      else unexpected ("wrapper of type " ++ unName expected) (unName actual)
    _ -> unexpected ("wrap(" ++ unName expected ++ ")") $ show term

-- | Extract the wrapped type from a wrapper type
-- Example: wrappedType (Name "Email") typ
wrappedType :: Name -> Type -> Flow s Type
wrappedType ename typ = case stripType typ of
  TypeWrap (WrappedType tname t) -> if tname == ename
    then pure t
    else unexpected ("wrapped type " ++ unName ename) $ "wrapped type " ++ unName tname
  _ -> unexpected "wrapped type" $ show typ
