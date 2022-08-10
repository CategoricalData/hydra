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
deref cx term = case termExpr cx term of
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

primitiveFunctionArity :: Context m -> PrimitiveFunction m -> Int
primitiveFunctionArity cx = arity . primitiveFunctionType
  where
    arity (FunctionType _ cod) = 1 + case typeExpr cx cod of
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
