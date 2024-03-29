-- | A DSL for constructing Hydra terms

module Hydra.Dsl.Expect where

import Hydra.Compute
import Hydra.Core
import Hydra.Graph
import Hydra.Strip
import Hydra.Tier1
import Hydra.Tier2
import qualified Hydra.Lib.Flows as Flows

import Prelude hiding (map)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Control.Monad as CM
import Data.Int


bigfloat :: Show a => Term a -> Flow s Double
bigfloat t = literal t >>= floatLiteral >>= bigfloatValue

bigfloatValue :: FloatValue -> Flow s Double
bigfloatValue v = case v of
  FloatValueBigfloat f -> pure f
  _ -> unexpected "bigfloat" $ show v

bigint :: Show a => Term a -> Flow s Integer
bigint t = literal t >>= integerLiteral >>= bigintValue

bigintValue :: IntegerValue -> Flow s Integer
bigintValue v = case v of
  IntegerValueBigint i -> pure i
  _ -> unexpected "bigint" $ show v

binary :: Show a => Term a -> Flow s String
binary t = literal t >>= binaryLiteral

binaryLiteral :: Literal -> Flow s String
binaryLiteral v = case v of
  LiteralBinary b -> pure b
  _ -> unexpected "binary" $ show v

boolean :: Show a => Term a -> Flow s Bool
boolean t = literal t >>= booleanLiteral

booleanLiteral :: Literal -> Flow s Bool
booleanLiteral v = case v of
  LiteralBoolean b -> pure b
  _ -> unexpected "boolean" $ show v

cases :: Show a => Name -> Term a -> Flow s (CaseStatement a)
cases name term = case stripTerm term of
  TermFunction (FunctionElimination (EliminationUnion cs)) -> if caseStatementTypeName cs == name
    then pure cs
    else unexpected ("case statement for type " ++ unName name) $ show term
  _ -> unexpected "case statement" $ show term

casesCase :: Show a => Name -> String -> Term a -> Flow s (Field a)
casesCase name n term = do
  cs <- cases name term
  let matching = L.filter (\f -> fieldName f == FieldName n) $ caseStatementCases cs
  if L.null matching
    then fail $ "not enough cases"
    else pure $ L.head matching

field :: Show a => FieldName -> (Term a -> Flow s x) -> [Field a] -> Flow s x
field fname mapping fields = case L.filter (\f -> fieldName f == fname) fields of
  [] -> fail $ "field " ++ unFieldName fname ++ " not found"
  [f] -> mapping $ fieldTerm f
  _ -> fail $ "multiple fields named " ++ unFieldName fname

float32 :: Show a => Term a -> Flow s Float
float32 t = literal t >>= floatLiteral >>= float32Value

float32Value :: FloatValue -> Flow s Float
float32Value v = case v of
  FloatValueFloat32 f -> pure f
  _ -> unexpected "float32" $ show v

float64 :: Show a => Term a -> Flow s Double
float64 t = literal t >>= floatLiteral >>= float64Value

float64Value :: FloatValue -> Flow s Double
float64Value v = case v of
  FloatValueFloat64 f -> pure f
  _ -> unexpected "float64" $ show v

floatLiteral :: Literal -> Flow s FloatValue
floatLiteral lit = case lit of
  LiteralFloat v -> pure v
  _ -> unexpected "floating-point value" $ show lit

inject :: Show a => Name -> Term a -> Flow s (Field a)
inject name term = case stripTerm term of
  TermUnion (Injection name' field) -> if name' == name
    then pure field
    else unexpected ("injection of type " ++ unName name) (unName name')
  _ -> unexpected "injection" $ show term

injection :: Show a => Term a -> Flow s (Field a)
injection term = case stripTerm term of
  TermUnion (Injection _ field) -> pure field
  _ -> unexpected "injection" $ show term

injectionWithName :: Show a => Name -> Term a -> Flow s (Field a)
injectionWithName expected term = case stripTerm term of
  TermUnion (Injection actual field) -> if actual == expected
    then pure field
    else unexpected ("injection of type " ++ unName expected) (unName actual)
  _ -> unexpected "injection" $ show term

int8 :: Show a => Term a -> Flow s Int8
int8 t = literal t >>= integerLiteral >>= int8Value

int8Value :: IntegerValue -> Flow s Int8
int8Value v = case v of
  IntegerValueInt8 i -> pure i
  _ -> unexpected "int8" $ show v

int16 :: Show a => Term a -> Flow s Int16
int16 t = literal t >>= integerLiteral >>= int16Value

int16Value :: IntegerValue -> Flow s Int16
int16Value v = case v of
  IntegerValueInt16 i -> pure i
  _ -> unexpected "int16" $ show v

int32 :: Show a => Term a -> Flow s Int
int32 t = literal t >>= integerLiteral >>= int32Value

int32Value :: IntegerValue -> Flow s Int
int32Value v = case v of
  IntegerValueInt32 i -> pure i
  _ -> unexpected "int32" $ show v

int64 :: Show a => Term a -> Flow s Int64
int64 t = literal t >>= integerLiteral >>= int64Value

int64Value :: IntegerValue -> Flow s Int64
int64Value v = case v of
  IntegerValueInt64 i -> pure i
  _ -> unexpected "int64" $ show v

integerLiteral :: Literal -> Flow s IntegerValue
integerLiteral lit = case lit of
  LiteralInteger v -> pure v
  _ -> unexpected "integer value" $ show lit

lambda :: Show a => Term a -> Flow s (Lambda a)
lambda term = case stripTerm term of
  TermFunction (FunctionLambda l) -> pure l
  _ -> unexpected "lambda" $ show term

letBinding :: Show a => String -> Term a -> Flow s (Term a)
letBinding n term = do
  bindings <- letBindings <$> letTerm term
  case M.lookup (Name n) bindings of
    Nothing -> fail $ "no such binding: " ++ show n
    Just term' -> pure term'

lambdaBody :: Show a => Term a -> Flow s (Term a)
lambdaBody term = Hydra.Core.lambdaBody <$> lambda term

letTerm :: Show a => Term a -> Flow s (Let a)
letTerm term = case stripTerm term of
  TermLet lt -> pure lt
  _ -> unexpected "let term" $ show term

list :: Show a => (Term a -> Flow s x) -> Term a -> Flow s [x]
list f term = case stripTerm term of
  TermList l -> CM.mapM f l
  _ -> unexpected "list" $ show term

listHead :: Show a => Term a -> Flow s (Term a)
listHead term = do
  l <- list pure term
  if L.null l
    then fail "empty list"
    else pure $ L.head l

literal :: Show a => Term a -> Flow s Literal
literal term = case stripTerm term of
  TermLiteral lit -> pure lit
  _ -> unexpected "literal" $ show term

map :: (Ord k, Show a) => (Term a -> Flow s k) -> (Term a -> Flow s v) -> Term a -> Flow s (M.Map k v)
map fk fv term = case stripTerm term of
  TermMap m -> M.fromList <$> CM.mapM pair (M.toList m)
    where
      pair (kterm, vterm) = do
        kval <- fk kterm
        vval <- fv vterm
        return (kval, vval)
  _ -> unexpected "map" $ show term

nArgs :: Int -> [Term a] -> Flow s ()
nArgs n args = if L.length args /= n
  then unexpected (show n ++ " arguments") $ show (L.length args)
  else pure ()

optCases :: Show a => Term a -> Flow s (OptionalCases a)
optCases term = case stripTerm term of
  TermFunction (FunctionElimination (EliminationOptional cs)) -> pure cs
  _ -> unexpected "optional cases" $ show term

optCasesJust :: Show a => Term a -> Flow s (Term a)
optCasesJust term = optionalCasesJust <$> optCases term

optCasesNothing :: Show a => Term a -> Flow s (Term a)
optCasesNothing term = optionalCasesNothing <$> optCases term

optional :: Show a => (Term a -> Flow s x) -> Term a -> Flow s (Y.Maybe x)
optional f term = case stripTerm term of
  TermOptional mt -> case mt of
    Nothing -> pure Nothing
    Just t -> Just <$> f t
  _ -> unexpected "optional value" $ show term

pair :: Show a => (Term a -> Flow s k) -> (Term a -> Flow s v) -> Term a -> Flow s (k, v)
pair kf vf term = case stripTerm term of
  TermProduct terms -> case terms of
    [kTerm, vTerm] -> do
      kVal <- kf kTerm
      vVal <- vf vTerm
      return (kVal, vVal)
    _ -> unexpected "pair" $ show term
  _ -> unexpected "product" $ show term

record :: Show a => Term a -> Flow s [Field a]
record term = case stripTerm term of
  TermRecord (Record _ fields) -> pure fields
  _ -> unexpected "record" $ show term

recordWithName :: Show a => Name -> Term a -> Flow s [Field a]
recordWithName expected term = case stripTerm term of
  TermRecord (Record actual fields) -> if actual == expected
    then pure fields
    else unexpected ("record of type" ++ unName expected) (unName actual)
  _ -> unexpected "record" $ show term

set :: (Ord x, Show a) => (Term a -> Flow s x) -> Term a -> Flow s (S.Set x)
set f term = case stripTerm term of
  TermSet s -> S.fromList <$> CM.mapM f (S.toList s)
  _ -> unexpected "set" $ show term

string :: Show a => Term a -> Flow s String
string t = literal t >>= stringLiteral

stringLiteral :: Literal -> Flow s String
stringLiteral v = case v of
  LiteralString s -> pure s
  _ -> unexpected "string" $ show v

uint8 :: Show a => Term a -> Flow s Int16
uint8 t = literal t >>= integerLiteral >>= uint8Value

uint8Value :: IntegerValue -> Flow s Int16
uint8Value v = case v of
  IntegerValueUint8 i -> pure i
  _ -> unexpected "uint8" $ show v

uint16 :: Show a => Term a -> Flow s Int
uint16 t = literal t >>= integerLiteral >>= uint16Value

uint16Value :: IntegerValue -> Flow s Int
uint16Value v = case v of
  IntegerValueUint16 i -> pure i
  _ -> unexpected "uint16" $ show v

uint32 :: Show a => Term a -> Flow s Int64
uint32 t = literal t >>= integerLiteral >>= uint32Value

uint32Value :: IntegerValue -> Flow s Int64
uint32Value v = case v of
  IntegerValueUint32 i -> pure i
  _ -> unexpected "uint32" $ show v

uint64 :: Show a => Term a -> Flow s Integer
uint64 t = literal t >>= integerLiteral >>= uint64Value

uint64Value :: IntegerValue -> Flow s Integer
uint64Value v = case v of
  IntegerValueUint64 i -> pure i
  _ -> unexpected "uint64" $ show v

unit :: Show a => Term a -> Flow s ()
unit term = do
  fields <- recordWithName _Unit term
  if L.null fields
    then pure ()
    else unexpected "unit" $ show term

unitVariant :: Show a => Name -> Term a -> Flow s FieldName
unitVariant tname term = do
  field <- variant tname term
  unit $ fieldTerm field
  pure $ fieldName field

variable :: Show a => Term a -> Flow s Name
variable term = case stripTerm term of
  TermVariable name -> pure name
  _ -> unexpected "variable" $ show term

variant :: Show a => Name -> Term a -> Flow s (Field a)
variant = injectionWithName

wrap :: Show a => Name -> Term a -> Flow s (Term a)
wrap expected term = case stripTerm term of
  TermWrap (Nominal actual term) -> if actual == expected
    then pure term
    else unexpected ("wrapper of type " ++ unName expected) (unName actual)
  _ -> unexpected ("wrap(" ++ unName expected ++ ")") $ show term
