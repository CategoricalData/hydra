module Hydra.Primitives (
  deref,
  dereferenceElement,
  graphElementsMap,
  lookupPrimitiveFunction,
  primitiveFunctionArity,
  requireElement,
  requirePrimitiveFunction,
  schemaContext,
) where

import Hydra.Common
import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Steps
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y


deref :: Context m -> Data m -> Result (Data m)
deref cx term = case dataTerm term of
  DataTermElement name -> dereferenceElement cx name >>= deref cx
  DataTermNominal (Named _ term') -> deref cx term'
  _ -> ResultSuccess term

dereferenceElement :: Context m -> Name -> Result (Data m)
dereferenceElement cx en = case M.lookup en (contextElements cx) of
    Nothing -> ResultFailure $ "element " ++ showName en ++ " does not exist in graph " ++ h (graphSetRoot (contextGraphs cx))
    Just e -> ResultSuccess $ elementData e
  where
    h (GraphName n) = n

getGraph :: GraphSet m -> GraphName -> Result (Graph m)
getGraph graphs name@(GraphName n) = Y.maybe error ResultSuccess $ M.lookup name (graphSetGraphs graphs)
  where
    error = ResultFailure $ "no such graph: " ++ n

graphElementsMap :: Graph m -> M.Map Name (Element m)
graphElementsMap g = M.fromList $ (\e -> (elementName e , e)) <$> graphElements g

lookupPrimitiveFunction :: Context m -> Name -> Maybe (PrimitiveFunction m)
lookupPrimitiveFunction cx fn = M.lookup fn $ contextFunctions cx

primitiveFunctionArity :: PrimitiveFunction m -> Int
primitiveFunctionArity = arity . primitiveFunctionType
  where
    arity (FunctionType _ cod) = 1 + case typeTerm cod of
      TypeTermFunction ft -> arity ft
      _ -> 0

requireElement :: Context m -> Name -> Result (Element m)
requireElement cx name = Y.maybe error ResultSuccess $ M.lookup name $ contextElements cx
  where
    error = ResultFailure $ "no such element: " ++ showName name
        ++ " in graph " ++ h (graphSetRoot (contextGraphs cx))
        ++ ". Available elements: {" ++ (L.intercalate ", " (showName . elementName <$> M.elems (contextElements cx))) ++ "}"
      where
        h (GraphName n) = n
          
requirePrimitiveFunction :: Context m -> Name -> Result (PrimitiveFunction m)
requirePrimitiveFunction cx fn = Y.maybe error ResultSuccess $ lookupPrimitiveFunction cx fn
  where
    error = ResultFailure $ "no such primitive function: " ++ showName fn

schemaContext :: Context m -> Result (Context m)
schemaContext cx = do
    dgraph <- getGraph graphs $ graphSetRoot graphs
    sgraph <- getGraph graphs $ graphSchemaGraph dgraph
    -- Note: assuming for now that primitive functions and evaluation strategy are the same in the schema graph
    return cx {
      contextGraphs = graphs { graphSetRoot = graphName sgraph },
      contextElements = graphElementsMap sgraph}
  where
    graphs = contextGraphs cx
