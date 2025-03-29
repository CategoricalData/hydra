-- | A DSL for decoding Hydra terms

module Hydra.Dsl.Expect where

import Hydra.Compute
import Hydra.Core
import Hydra.Graph
import Hydra.Strip
import Hydra.Rewriting
import Hydra.Errors
import Hydra.Staging.Lexical
import qualified Hydra.Lib.Flows as Flows

import Prelude hiding (map)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Control.Monad as CM
import Data.Int


bigfloat :: Term -> Flow Graph Double
bigfloat t = literal t >>= floatLiteral >>= bigfloatValue

bigfloatValue :: FloatValue -> Flow Graph Double
bigfloatValue v = case v of
  FloatValueBigfloat f -> pure f
  _ -> unexpected "bigfloat" $ show v

bigint :: Term -> Flow Graph Integer
bigint t = literal t >>= integerLiteral >>= bigintValue

bigintValue :: IntegerValue -> Flow Graph Integer
bigintValue v = case v of
  IntegerValueBigint i -> pure i
  _ -> unexpected "bigint" $ show v

binary :: Term -> Flow Graph String
binary t = literal t >>= binaryLiteral

binaryLiteral :: Literal -> Flow Graph String
binaryLiteral v = case v of
  LiteralBinary b -> pure b
  _ -> unexpected "binary" $ show v

boolean :: Term -> Flow Graph Bool
boolean t = literal t >>= booleanLiteral

booleanLiteral :: Literal -> Flow Graph Bool
booleanLiteral v = case v of
  LiteralBoolean b -> pure b
  _ -> unexpected "boolean" $ show v

cases :: Name -> Term -> Flow Graph CaseStatement
cases name term0 = do
  term <- stripAndDereferenceTerm term0
  case term of
    TermFunction (FunctionElimination (EliminationUnion cs)) -> if caseStatementTypeName cs == name
      then pure cs
      else unexpected ("case statement for type " ++ unName name) $ show term
    _ -> unexpected "case statement" $ show term

casesCase :: Name -> String -> Term -> Flow Graph Field
casesCase name n term = do
  cs <- cases name term
  let matching = L.filter (\f -> fieldName f == Name n) $ caseStatementCases cs
  if L.null matching
    then fail $ "not enough cases"
    else pure $ L.head matching

field :: Name -> (Term -> Flow Graph x) -> [Field] -> Flow Graph x
field fname mapping fields = case L.filter (\f -> fieldName f == fname) fields of
  [] -> fail $ "field " ++ unName fname ++ " not found"
  [f] -> (stripAndDereferenceTerm $ fieldTerm f) >>= mapping
  _ -> fail $ "multiple fields named " ++ unName fname

float32 :: Term -> Flow Graph Float
float32 t = literal t >>= floatLiteral >>= float32Value

float32Value :: FloatValue -> Flow Graph Float
float32Value v = case v of
  FloatValueFloat32 f -> pure f
  _ -> unexpected "float32" $ show v

float64 :: Term -> Flow Graph Double
float64 t = literal t >>= floatLiteral >>= float64Value

float64Value :: FloatValue -> Flow Graph Double
float64Value v = case v of
  FloatValueFloat64 f -> pure f
  _ -> unexpected "float64" $ show v

floatLiteral :: Literal -> Flow Graph FloatValue
floatLiteral lit = case lit of
  LiteralFloat v -> pure v
  _ -> unexpected "floating-point value" $ show lit

functionType :: Type -> Flow s FunctionType
functionType typ = case stripType typ of
  TypeFunction ft -> pure ft
  _ -> unexpected "function type" $ show typ

injection :: Name -> Term -> Flow Graph Field
injection expected term0 = do
  term <- stripAndDereferenceTerm term0
  case term of
    TermUnion (Injection actual field) -> if actual == expected
      then pure field
      else unexpected ("injection of type " ++ unName expected) (unName actual)
    _ -> unexpected "injection" $ show term

int8 :: Term -> Flow Graph Int8
int8 t = literal t >>= integerLiteral >>= int8Value

int8Value :: IntegerValue -> Flow Graph Int8
int8Value v = case v of
  IntegerValueInt8 i -> pure i
  _ -> unexpected "int8" $ show v

int16 :: Term -> Flow Graph Int16
int16 t = literal t >>= integerLiteral >>= int16Value

int16Value :: IntegerValue -> Flow Graph Int16
int16Value v = case v of
  IntegerValueInt16 i -> pure i
  _ -> unexpected "int16" $ show v

int32 :: Term -> Flow Graph Int
int32 t = literal t >>= integerLiteral >>= int32Value

int32Value :: IntegerValue -> Flow Graph Int
int32Value v = case v of
  IntegerValueInt32 i -> pure i
  _ -> unexpected "int32" $ show v

int64 :: Term -> Flow Graph Int64
int64 t = literal t >>= integerLiteral >>= int64Value

int64Value :: IntegerValue -> Flow Graph Int64
int64Value v = case v of
  IntegerValueInt64 i -> pure i
  _ -> unexpected "int64" $ show v

integerLiteral :: Literal -> Flow Graph IntegerValue
integerLiteral lit = case lit of
  LiteralInteger v -> pure v
  _ -> unexpected "integer value" $ show lit

lambda :: Term -> Flow Graph Lambda
lambda term0 = do
  term <- stripAndDereferenceTerm term0
  case term of
    TermFunction (FunctionLambda l) -> pure l
    _ -> unexpected "lambda" $ show term

letBinding :: String -> Term -> Flow Graph Term
letBinding n term = do
  bindings <- letBindings <$> letTerm term
  case L.filter (\b -> letBindingName b == Name n) bindings of
    [] -> fail $ "no such binding: " ++ n
    [b] -> pure $ letBindingTerm b
    _ -> fail $ "multiple bindings named " ++ n

lambdaBody :: Term -> Flow Graph Term
lambdaBody term = Hydra.Core.lambdaBody <$> lambda term

letTerm :: Term -> Flow Graph Let
letTerm term0 = do
  term <- stripAndDereferenceTerm term0
  case term of
    TermLet lt -> pure lt
    _ -> unexpected "let term" $ show term

list :: (Term -> Flow Graph x) -> Term -> Flow Graph [x]
list f term0 = do
  term <- stripAndDereferenceTerm term0
  case term of
    TermList l -> CM.mapM f l
    _ -> unexpected "list" $ show term

listHead :: Term -> Flow Graph Term
listHead term = do
  l <- list pure term
  if L.null l
    then fail "empty list"
    else pure $ L.head l

listType :: Type -> Flow s Type
listType typ = case stripType typ of
  TypeList t -> pure t
  _ -> unexpected "list type" $ show typ

literal :: Term -> Flow Graph Literal
literal term0 = do
  term <- stripAndDereferenceTerm term0
  case term of
    TermLiteral lit -> pure lit
    _ -> unexpected "literal" $ show term

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

mapType :: Type -> Flow s MapType
mapType typ = case stripType typ of
  TypeMap mt -> pure mt
  _ -> unexpected "map type" $ show typ

nArgs :: Int -> [Term] -> Flow s ()
nArgs n args = if L.length args /= n
  then unexpected (show n ++ " arguments") $ show (L.length args)
  else pure ()

optional :: (Term -> Flow Graph x) -> Term -> Flow Graph (Y.Maybe x)
optional f term0 = do
  term <- stripAndDereferenceTerm term0
  case term of
    TermOptional mt -> case mt of
      Nothing -> pure Nothing
      Just t -> Just <$> f t
    _ -> unexpected "optional value" $ show term

optionalType :: Type -> Flow s Type
optionalType typ = case stripType typ of
  TypeOptional t -> pure t
  _ -> unexpected "optional type" $ show typ

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

productType :: Type -> Flow s [Type]
productType typ = case stripType typ of
  TypeProduct types -> pure types
  _ -> unexpected "product type" $ show typ

record :: Name -> Term -> Flow Graph [Field]
record expected term0 = do
  term <- stripAndDereferenceTerm term0
  case term of
    TermRecord (Record actual fields) -> if actual == expected
      then pure fields
      else unexpected ("record of type " ++ unName expected) (unName actual)
    _ -> unexpected "record" $ show term

recordType :: Name -> Type -> Flow s [FieldType]
recordType ename typ = case stripType typ of
  TypeRecord (RowType tname fields) -> if tname == ename
    then pure fields
    else unexpected ("record of type " ++ unName ename) $ "record of type " ++ unName tname
  _ -> unexpected "record type" $ show typ

set :: Ord x => (Term -> Flow Graph x) -> Term -> Flow Graph (S.Set x)
set f term0 = do
  term <- stripAndDereferenceTerm term0
  case term of
    TermSet s -> S.fromList <$> CM.mapM f (S.toList s)
    _ -> unexpected "set" $ show term

setType :: Type -> Flow s Type
setType typ = case stripType typ of
  TypeSet t -> pure t
  _ -> unexpected "set type" $ show typ

string :: Term -> Flow Graph String
string t = literal t >>= stringLiteral

stringLiteral :: Literal -> Flow Graph String
stringLiteral v = case v of
  LiteralString s -> pure s
  _ -> unexpected "string" $ show v

sumType :: Type -> Flow s [Type]
sumType typ = case stripType typ of
  TypeSum types -> pure types
  _ -> unexpected "sum type" $ show typ

uint8 :: Term -> Flow Graph Int16
uint8 t = literal t >>= integerLiteral >>= uint8Value

uint8Value :: IntegerValue -> Flow Graph Int16
uint8Value v = case v of
  IntegerValueUint8 i -> pure i
  _ -> unexpected "uint8" $ show v

uint16 :: Term -> Flow Graph Int
uint16 t = literal t >>= integerLiteral >>= uint16Value

uint16Value :: IntegerValue -> Flow Graph Int
uint16Value v = case v of
  IntegerValueUint16 i -> pure i
  _ -> unexpected "uint16" $ show v

uint32 :: Term -> Flow Graph Int64
uint32 t = literal t >>= integerLiteral >>= uint32Value

uint32Value :: IntegerValue -> Flow Graph Int64
uint32Value v = case v of
  IntegerValueUint32 i -> pure i
  _ -> unexpected "uint32" $ show v

uint64 :: Term -> Flow Graph Integer
uint64 t = literal t >>= integerLiteral >>= uint64Value

uint64Value :: IntegerValue -> Flow Graph Integer
uint64Value v = case v of
  IntegerValueUint64 i -> pure i
  _ -> unexpected "uint64" $ show v

unionType :: Name -> Type -> Flow s [FieldType]
unionType ename typ = case stripType typ of
  TypeUnion (RowType tname fields) -> if tname == ename
    then pure fields
    else unexpected ("union of type " ++ unName ename) $ "union of type " ++ unName tname
  _ -> unexpected "union type" $ show typ

unit :: Term -> Flow Graph ()
unit term0 = do
  term <- stripAndDereferenceTerm term0
  fields <- record _Unit term
  if L.null fields
    then pure ()
    else unexpected "unit" $ show term

unitVariant :: Name -> Term -> Flow Graph Name
unitVariant tname term = do
  field <- variant tname term
  unit $ fieldTerm field
  pure $ fieldName field

variant :: Name -> Term -> Flow Graph Field
variant = injection

wrap :: Name -> Term -> Flow Graph Term
wrap expected term0 = do
  term <- stripAndDereferenceTerm term0
  case term of
    TermWrap (WrappedTerm actual term) -> if actual == expected
      then pure term
      else unexpected ("wrapper of type " ++ unName expected) (unName actual)
    _ -> unexpected ("wrap(" ++ unName expected ++ ")") $ show term

wrappedType :: Name -> Type -> Flow s Type
wrappedType ename typ = case stripType typ of
  TypeWrap (WrappedType tname t) -> if tname == ename
    then pure t
    else unexpected ("wrapped type " ++ unName ename) $ "wrapped type " ++ unName tname
  _ -> unexpected "wrapped type" $ show typ
