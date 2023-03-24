-- | A DSL for constructing Hydra terms

module Hydra.Dsl.Expect where

import Hydra.Common
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


expectBinary :: Show a => Term a -> Flow s String
expectBinary t = expectLiteral t >>= expectBinaryLiteral

expectBinaryLiteral :: Literal -> Flow s String
expectBinaryLiteral v = case v of
  LiteralBinary b -> pure b
  _ -> unexpected "binary" v

expectBoolean :: Show a => Term a -> Flow s Bool
expectBoolean t = expectLiteral t >>= expectBooleanLiteral

expectBooleanLiteral :: Literal -> Flow s Bool
expectBooleanLiteral v = case v of
  LiteralBoolean b -> pure b
  _ -> unexpected "boolean" v

expectFloat32 :: Show a => Term a -> Flow s Float
expectFloat32 t = expectLiteral t >>= expectFloat32Literal

expectFloat32Literal :: Literal -> Flow s Float
expectFloat32Literal v = case v of
  LiteralFloat (FloatValueFloat32 f) -> pure f
  _ -> unexpected "float32" v

expectFloat64 :: Show a => Term a -> Flow s Double
expectFloat64 t = expectLiteral t >>= expectFloat64Literal

expectFloat64Literal :: Literal -> Flow s Double
expectFloat64Literal v = case v of
  LiteralFloat (FloatValueFloat64 f) -> pure f
  _ -> unexpected "float64" v

expectInt32 :: Show a => Term a -> Flow s Int
expectInt32 t = expectLiteral t >>= expectInt32Literal

expectInt32Literal :: Literal -> Flow s Int
expectInt32Literal v = case v of
  LiteralInteger (IntegerValueInt32 i) -> pure i
  _ -> unexpected "int32" v

expectInt64 :: Show a => Term a -> Flow s Integer
expectInt64 t = expectLiteral t >>= expectInt64Literal

expectInt64Literal :: Literal -> Flow s Integer
expectInt64Literal v = case v of
  LiteralInteger (IntegerValueInt64 i) -> pure i
  _ -> unexpected "int64" v

expectList :: Show a => (Term a -> Flow s x) -> Term a -> Flow s [x]
expectList f term = case stripTerm term of
  TermList l -> CM.mapM f l
  _ -> unexpected "list" term

expectLiteral :: Show a => Term a -> Flow s Literal
expectLiteral term = case stripTerm term of
  TermLiteral lit -> pure lit
  _ -> unexpected "literal" term

expectMap :: (Ord k, Show a) => (Term a -> Flow s k) -> (Term a -> Flow s v) -> Term a -> Flow s (M.Map k v)
expectMap fk fv term = case stripTerm term of
  TermMap m -> M.fromList <$> CM.mapM expectPair (M.toList m)
    where
      expectPair (kterm, vterm) = do
        kval <- fk kterm
        vval <- fv vterm
        return (kval, vval)
  _ -> unexpected "map" term

expectNArgs :: Int -> [Term a] -> Flow s ()
expectNArgs n args = if L.length args /= n
  then unexpected (show n ++ " arguments") (L.length args)
  else pure ()

expectOptional :: Show a => (Term a -> Flow s x) -> Term a -> Flow s (Y.Maybe x)
expectOptional f term = case stripTerm term of
  TermOptional mt -> case mt of
    Nothing -> pure Nothing
    Just t -> Just <$> f t
  _ -> unexpected "optional value" term

expectPair :: Show a => (Term a -> Flow s k) -> (Term a -> Flow s v) -> Term a -> Flow s (k, v)
expectPair kf vf term = case stripTerm term of
  TermProduct terms -> case terms of
    [kTerm, vTerm] -> do
      kVal <- kf kTerm
      vVal <- vf vTerm
      return (kVal, vVal)
    _ -> unexpected "pair" term
  _ -> unexpected "product" term

expectRecord :: Show a => Term a -> Flow s [Field a]
expectRecord term = case stripTerm term of
  TermRecord (Record _ fields) -> pure fields
  _ -> unexpected "record" term

expectRecordWithName :: Show a => Name -> Term a -> Flow s [Field a]
expectRecordWithName expected term = case stripTerm term of
  TermRecord (Record actual fields) -> if actual == expected
    then pure fields
    else fail $ "found a record of type " ++ unName actual ++ ", expected " ++ unName expected
  _ -> unexpected "record" term

expectSet :: (Ord x, Show a) => (Term a -> Flow s x) -> Term a -> Flow s (S.Set x)
expectSet f term = case stripTerm term of
  TermSet s -> S.fromList <$> CM.mapM f (S.toList s)
  _ -> unexpected "set" term

expectString :: Show a => Term a -> Flow s String
expectString t = expectLiteral t >>= expectStringLiteral

expectStringLiteral :: Literal -> Flow s String
expectStringLiteral v = case v of
  LiteralString s -> pure s
  _ -> unexpected "string" v

expectInjection :: Show a => Term a -> Flow s (Field a)
expectInjection term = case stripTerm term of
  TermUnion (Injection _ field) -> pure field
  _ -> unexpected "injection" term

expectInjectionWithName :: Show a => Name -> Term a -> Flow s (Field a)
expectInjectionWithName expected term = case stripTerm term of
  TermUnion (Injection actual field) -> if actual == expected
    then pure field
    else fail $ "found an injection of type " ++ unName actual ++ ", expected " ++ unName expected
  _ -> unexpected "injection" term

expectWrapWithName :: Show a => Name -> Term a -> Flow s (Term a)
expectWrapWithName expected term = case stripTerm term of
  TermWrap (Nominal actual term) -> if actual == expected
    then pure term
    else fail $ "found a wrapper of type " ++ unName actual ++ ", expected " ++ unName expected
  _ -> unexpected "wrap" term
