-- | A DSL for constructing Hydra terms

module Hydra.Dsl.Expect where

import qualified Hydra.Common as Common
import Hydra.Compute
import Hydra.Core
import Hydra.Graph
import Hydra.Flows

import Prelude hiding (map)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Control.Monad as CM
import Data.Int


binary :: Show a => Term a -> Flow s String
binary t = literal t >>= binaryLiteral

binaryLiteral :: Literal -> Flow s String
binaryLiteral v = case v of
  LiteralBinary b -> pure b
  _ -> unexpected "binary" v

boolean :: Show a => Term a -> Flow s Bool
boolean t = literal t >>= booleanLiteral

booleanLiteral :: Literal -> Flow s Bool
booleanLiteral v = case v of
  LiteralBoolean b -> pure b
  _ -> unexpected "boolean" v

caseStatement :: Show a => Name -> Term a -> Flow s (CaseStatement a)
caseStatement name term = case Common.stripTerm term of
  TermFunction (FunctionElimination (EliminationUnion cs)) -> if caseStatementTypeName cs == name
    then pure cs
    else unexpected ("case statement for type " ++ unName name) term
  _ -> unexpected "case statement" term

float32 :: Show a => Term a -> Flow s Float
float32 t = literal t >>= float32Literal

float32Literal :: Literal -> Flow s Float
float32Literal v = case v of
  LiteralFloat (FloatValueFloat32 f) -> pure f
  _ -> unexpected "float32" v

float64 :: Show a => Term a -> Flow s Double
float64 t = literal t >>= float64Literal

float64Literal :: Literal -> Flow s Double
float64Literal v = case v of
  LiteralFloat (FloatValueFloat64 f) -> pure f
  _ -> unexpected "float64" v

injection :: Show a => Term a -> Flow s (Field a)
injection term = case Common.stripTerm term of
  TermUnion (Injection _ field) -> pure field
  _ -> unexpected "injection" term

injectionWithName :: Show a => Name -> Term a -> Flow s (Field a)
injectionWithName expected term = case Common.stripTerm term of
  TermUnion (Injection actual field) -> if actual == expected
    then pure field
    else fail $ "found an injection of type " ++ unName actual ++ ", expected " ++ unName expected
  _ -> unexpected "injection" term

int32 :: Show a => Term a -> Flow s Int
int32 t = literal t >>= int32Literal

int32Literal :: Literal -> Flow s Int
int32Literal v = case v of
  LiteralInteger (IntegerValueInt32 i) -> pure i
  _ -> unexpected "int32" v

int64 :: Show a => Term a -> Flow s Integer
int64 t = literal t >>= int64Literal

int64Literal :: Literal -> Flow s Integer
int64Literal v = case v of
  LiteralInteger (IntegerValueInt64 i) -> pure i
  _ -> unexpected "int64" v

lambda :: Show a => Term a -> Flow s (Lambda a)
lambda term = case Common.stripTerm term of
  TermFunction (FunctionLambda l) -> pure l
  _ -> unexpected "lambda" term

list :: Show a => (Term a -> Flow s x) -> Term a -> Flow s [x]
list f term = case Common.stripTerm term of
  TermList l -> CM.mapM f l
  _ -> unexpected "list" term

literal :: Show a => Term a -> Flow s Literal
literal term = case Common.stripTerm term of
  TermLiteral lit -> pure lit
  _ -> unexpected "literal" term

map :: (Ord k, Show a) => (Term a -> Flow s k) -> (Term a -> Flow s v) -> Term a -> Flow s (M.Map k v)
map fk fv term = case Common.stripTerm term of
  TermMap m -> M.fromList <$> CM.mapM pair (M.toList m)
    where
      pair (kterm, vterm) = do
        kval <- fk kterm
        vval <- fv vterm
        return (kval, vval)
  _ -> unexpected "map" term

nArgs :: Int -> [Term a] -> Flow s ()
nArgs n args = if L.length args /= n
  then unexpected (show n ++ " arguments") (L.length args)
  else pure ()

optional :: Show a => (Term a -> Flow s x) -> Term a -> Flow s (Y.Maybe x)
optional f term = case Common.stripTerm term of
  TermOptional mt -> case mt of
    Nothing -> pure Nothing
    Just t -> Just <$> f t
  _ -> unexpected "optional value" term

pair :: Show a => (Term a -> Flow s k) -> (Term a -> Flow s v) -> Term a -> Flow s (k, v)
pair kf vf term = case Common.stripTerm term of
  TermProduct terms -> case terms of
    [kTerm, vTerm] -> do
      kVal <- kf kTerm
      vVal <- vf vTerm
      return (kVal, vVal)
    _ -> unexpected "pair" term
  _ -> unexpected "product" term

record :: Show a => Term a -> Flow s [Field a]
record term = case Common.stripTerm term of
  TermRecord (Record _ fields) -> pure fields
  _ -> unexpected "record" term

recordWithName :: Show a => Name -> Term a -> Flow s [Field a]
recordWithName expected term = case Common.stripTerm term of
  TermRecord (Record actual fields) -> if actual == expected
    then pure fields
    else fail $ "found a record of type " ++ unName actual ++ ", expected " ++ unName expected
  _ -> unexpected "record" term

set :: (Ord x, Show a) => (Term a -> Flow s x) -> Term a -> Flow s (S.Set x)
set f term = case Common.stripTerm term of
  TermSet s -> S.fromList <$> CM.mapM f (S.toList s)
  _ -> unexpected "set" term

string :: Show a => Term a -> Flow s String
string t = literal t >>= stringLiteral

stringLiteral :: Literal -> Flow s String
stringLiteral v = case v of
  LiteralString s -> pure s
  _ -> unexpected "string" v

inject :: Show a => Name -> Term a -> Flow s (Field a)
inject name term = case Common.stripTerm term of
  TermUnion (Injection name' field) -> if name' == name
    then pure field
    else fail $ "found an injection of type " ++ unName name' ++ ", expected " ++ unName name
  _ -> unexpected "injection" term

wrapWithName :: Show a => Name -> Term a -> Flow s (Term a)
wrapWithName expected term = case Common.stripTerm term of
  TermWrap (Nominal actual term) -> if actual == expected
    then pure term
    else fail $ "found a wrapper of type " ++ unName actual ++ ", expected " ++ unName expected
  _ -> unexpected "wrap" term
