-- | Functions for retrieving elements and primitive functions from a graph context

module Hydra.Lexical (
  module Hydra.Lexical,
  module Hydra.Common,
  ) where

import Hydra.Common
import Hydra.Core
import Hydra.Module
import Hydra.Compute
import Hydra.Mantle
import Hydra.Monads

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y
import Control.Monad


deref :: Term m -> GraphFlow m (Term m)
deref term = case stripTerm term of
  TermElement name -> dereferenceElement name >>= deref
  TermNominal (Named _ term') -> deref term'
  _ -> pure term

dereferenceElement :: Name -> GraphFlow m (Term m)
dereferenceElement en = do
    cx <- getState
    case M.lookup en (graphElements $ contextGraph cx) of
      Nothing -> fail $ "element " ++ unName en ++ " does not exist"
      Just e -> pure $ elementData e

lookupPrimitiveFunction :: Context m -> Name -> Maybe (PrimitiveFunction m)
lookupPrimitiveFunction cx fn = M.lookup fn $ contextFunctions cx

primitiveFunctionArity :: PrimitiveFunction m -> Int
primitiveFunctionArity = arity . primitiveFunctionType
  where
    arity (FunctionType _ cod) = 1 + case stripType cod of
      TypeFunction ft -> arity ft
      _ -> 0

requireElement :: Name -> GraphFlow m (Element m)
requireElement name = do
    cx <- getState
    Y.maybe (err cx) pure $ M.lookup name $ graphElements $ contextGraph cx
  where
    err cx = fail $ "no such element: " ++ unName name
        ++ ". Available elements: {" ++ L.intercalate ", " (ellipsis (unName . elementName <$> M.elems (graphElements $ contextGraph cx))) ++ "}"
      where
        ellipsis strings = if L.length strings > 3
--        ellipsis strings = if L.length strings < 0
          then L.take 3 strings ++ ["..."]
          else strings

requirePrimitiveFunction :: Name -> GraphFlow m (PrimitiveFunction m)
requirePrimitiveFunction fn = do
    cx <- getState
    Y.maybe err pure $ lookupPrimitiveFunction cx fn
  where
    err = fail $ "no such primitive function: " ++ unName fn

-- Note: assuming for now that primitive functions and evaluation strategy are the same in the schema graph
schemaContext :: Context m -> Context m
schemaContext cx = case graphSchema (contextGraph cx) of
  Nothing -> cx
  Just g -> cx {contextGraph = g}

withSchemaContext :: GraphFlow m a -> GraphFlow m a
withSchemaContext f = do
  cx <- getState
  withState (schemaContext cx) f
