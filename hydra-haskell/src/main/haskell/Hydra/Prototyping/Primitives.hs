module Hydra.Prototyping.Primitives (
  deref,
  dereferenceElement,
  graphElementsMap,
  lookupPrimitiveFunction,
  primitiveFunctionArity,
  requireElement,
  requirePrimitiveFunction,
  schemaContext,
) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Prototyping.Steps

import qualified Data.Map as M
import qualified Data.Maybe as Y


deref :: Context a -> Term a -> Result (Term a)
deref context term = case termData term of
  ExpressionElement name -> dereferenceElement context name >>= deref context
  ExpressionNominal (NominalTerm _ term') -> deref context term'
  _ -> pure term

dereferenceElement :: Context a -> Name -> Result (Term a)
dereferenceElement context en = case M.lookup en (contextElements context) of
  Nothing -> fail $ "element " ++ en ++ " does not exist in graph " ++ graphSetRoot (contextGraphs context)
  Just e -> pure $ elementData e

getGraph :: GraphSet a -> GraphName -> Result (Graph a)
getGraph graphs name = Y.maybe error pure $ M.lookup name (graphSetGraphs graphs)
  where
    error = fail $ "no such graph: " ++ name

graphElementsMap :: Graph a -> M.Map Name (Element a)
graphElementsMap g = M.fromList $ (\e -> (elementName e , e)) <$> graphElements g

lookupPrimitiveFunction :: Context a -> Name -> Maybe (PrimitiveFunction a)
lookupPrimitiveFunction context fn = M.lookup fn $ contextFunctions context

primitiveFunctionArity :: PrimitiveFunction a -> Int
primitiveFunctionArity = arity . primitiveFunctionType
  where
    arity (FunctionType _ cod) = 1 + case cod of
      TypeFunction ft -> arity ft
      _ -> 0

requireElement :: Context a -> Name -> Result (Element a)
requireElement context name = Y.maybe error pure $ M.lookup name $ contextElements context
  where
    error = fail $ "no such element: " ++ name
      ++ " in graph " ++ graphSetRoot (contextGraphs context)

requirePrimitiveFunction :: Context a -> Name -> Result (PrimitiveFunction a)
requirePrimitiveFunction context fn = Y.maybe error pure $ lookupPrimitiveFunction context fn
  where
    error = fail $ "no such primitive function: " ++ fn

schemaContext :: Context a -> Result (Context a)
schemaContext context = do
    dgraph <- getGraph graphs $ graphSetRoot graphs
    sgraph <- getGraph graphs $ graphSchemaGraph dgraph
    -- Note: assuming for now that primitive functions and evaluation strategy are the same in the schema graph
    return context {
      contextGraphs = graphs { graphSetRoot = graphName sgraph },
      contextElements = graphElementsMap sgraph}
  where
    graphs = contextGraphs context
