module Hydra.Primitives where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Common
import Hydra.Steps
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y


deref :: Context m -> Term m -> Result (Term m)
deref cx term = case termExpr term of
  TermElement name -> dereferenceElement cx name >>= deref cx
  TermNominal (Named _ term') -> deref cx term'
  _ -> ResultSuccess term

dereferenceElement :: Context m -> Name -> Result (Term m)
dereferenceElement cx en = case M.lookup en (contextElements cx) of
    Nothing -> ResultFailure $ "element " ++ unName en ++ " does not exist in graph " ++ h (graphSetRoot (contextGraphs cx))
    Just e -> ResultSuccess $ elementData e
  where
    h (GraphName n) = n

graphElementsMap :: Graph m -> M.Map Name (Element m)
graphElementsMap g = M.fromList $ (\e -> (elementName e , e)) <$> graphElements g

lookupPrimitiveFunction :: Context m -> Name -> Maybe (PrimitiveFunction m)
lookupPrimitiveFunction cx fn = M.lookup fn $ contextFunctions cx

primitiveFunctionArity :: PrimitiveFunction m -> Int
primitiveFunctionArity = arity . primitiveFunctionType
  where
    arity (FunctionType _ cod) = 1 + case typeExpr cod of
      TypeFunction ft -> arity ft
      _ -> 0

requireElement :: Maybe String -> Context m -> Name -> Result (Element m)
requireElement debug cx name = Y.maybe err ResultSuccess $ M.lookup name $ contextElements cx
  where
    err = ResultFailure $ "no such element: " ++ unName name
        ++ " in graph " ++ h (graphSetRoot (contextGraphs cx))
        ++ Y.maybe "" (\d -> " (context: " ++ d ++ ")") debug
        ++ ". Available elements: {" ++ L.intercalate ", " (unName . elementName <$> M.elems (contextElements cx)) ++ "}"
      where
        h (GraphName n) = n

requirePrimitiveFunction :: Context m -> Name -> Result (PrimitiveFunction m)
requirePrimitiveFunction cx fn = Y.maybe err ResultSuccess $ lookupPrimitiveFunction cx fn
  where
    err = ResultFailure $ "no such primitive function: " ++ unName fn

schemaContext :: Context m -> Result (Context m)
schemaContext cx = do
    dgraph <- getGraph $ graphSetRoot graphs
    sgraph <- getGraph $ graphSchemaGraph dgraph
    -- Note: assuming for now that primitive functions and evaluation strategy are the same in the schema graph
    return cx {
      contextGraphs = graphs { graphSetRoot = graphName sgraph },
      contextElements = graphElementsMap sgraph}
  where
    graphs = contextGraphs cx
    
    getGraph name@(GraphName n) = Y.maybe err ResultSuccess $ M.lookup name (graphSetGraphs graphs)
      where
        err = ResultFailure $ "no such graph: " ++ n
