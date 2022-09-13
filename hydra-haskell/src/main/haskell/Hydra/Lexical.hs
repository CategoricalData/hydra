module Hydra.Lexical (
  module Hydra.Lexical,
  module Hydra.Common,
  ) where

import Hydra.Common
import Hydra.Core
import Hydra.Graph
import Hydra.Evaluation
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
    case M.lookup en (contextElements cx) of
      Nothing -> fail $ "element " ++ unName en ++ " does not exist in graph " ++ unGraphName (graphSetRoot (contextGraphs cx))
      Just e -> pure $ elementData e

graphElementsMap :: Graph m -> M.Map Name (Element m)
graphElementsMap g = M.fromList $ (\e -> (elementName e , e)) <$> graphElements g

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
    Y.maybe (err cx) pure $ M.lookup name $ contextElements cx
  where
    err cx = fail $ "no such element: " ++ unName name
        ++ " in graph " ++ h (graphSetRoot (contextGraphs cx))
        ++ ". Available elements: {" ++ L.intercalate ", " (unName . elementName <$> M.elems (contextElements cx)) ++ "}"
      where
        h (GraphName n) = n

requirePrimitiveFunction :: Name -> GraphFlow m (PrimitiveFunction m)
requirePrimitiveFunction fn = do
    cx <- getState
    Y.maybe err pure $ lookupPrimitiveFunction cx fn
  where
    err = fail $ "no such primitive function: " ++ unName fn

-- Note: contexts are assumed to be valid; this function will fail if the named data or schema graph does not exist
-- Also note: assuming for now that primitive functions and evaluation strategy are the same in the schema graph
schemaContext :: Context m -> Context m
schemaContext cx = pushTrace "schema" $ cx {
      contextGraphs = graphs { graphSetRoot = graphName sgraph },
      contextElements = graphElementsMap sgraph}
  where
    sgraph = getGraph $ graphSchemaGraph $ getGraph $ graphSetRoot graphs
    graphs = contextGraphs cx
    getGraph name = Y.fromJust $ M.lookup name (graphSetGraphs graphs)

setContextElements :: [Graph m] -> Context m -> Context m
setContextElements graphs cx = cx { contextElements = M.fromList $
  ((\e -> (elementName e, e)) <$> (L.concat (graphElements <$> graphs)))}

withSchemaContext :: GraphFlow m a -> GraphFlow m a
withSchemaContext f = do
  cx <- getState
  withState (schemaContext cx) f
