-- | Functions for retrieving elements and primitive functions from a graph context

module Hydra.Lexical (
  module Hydra.Lexical,
  module Hydra.Common,
  ) where

import Hydra.Common
import Hydra.Basics
import Hydra.Strip
import Hydra.Core
import Hydra.Extras
import Hydra.Graph
import Hydra.Compute
import Hydra.Flows
import Hydra.Tier1
import Hydra.Tier2

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y
import Control.Monad


dereferenceElement :: Name -> GraphFlow a (Term a)
dereferenceElement en = do
    cx <- getState
    case M.lookup en (graphElements cx) of
      Nothing -> fail $ "element " ++ unName en ++ " does not exist"
      Just e -> pure $ elementData e

requireElement :: Name -> GraphFlow a (Element a)
requireElement name = do
    cx <- getState
    Y.maybe (err cx) pure $ M.lookup name $ graphElements cx
  where
    err cx = fail $ "no such element: " ++ unName name
        ++ ". Available elements: {" ++ L.intercalate ", " (ellipsis (unName . elementName <$> M.elems (graphElements cx))) ++ "}"
      where
        showAll = False
        ellipsis strings = if L.length strings > 3 && not showAll
          then L.take 3 strings ++ ["..."]
          else strings

requirePrimitive :: Name -> GraphFlow a (Primitive a)
requirePrimitive fn = do
    cx <- getState
    Y.maybe err pure $ lookupPrimitive cx fn
  where
    err = fail $ "no such primitive function: " ++ unName fn

-- TODO: distinguish between lambda-bound and let-bound variables
resolveTerm :: Name -> GraphFlow a (Maybe (Term a))
resolveTerm name = do
    g <- getState
    Y.maybe (pure Nothing) recurse $ M.lookup name $ graphElements g
  where
    recurse el = case stripTerm (elementData el) of
      TermVariable name' -> resolveTerm name'
      _ -> pure $ Just $ elementData el

-- Note: assuming for now that primitive functions are the same in the schema graph
schemaContext :: Graph a -> Graph a
schemaContext g = Y.fromMaybe g (graphSchema g)

withSchemaContext :: GraphFlow a x -> GraphFlow a x
withSchemaContext f = do
  cx <- getState
  withState (schemaContext cx) f
