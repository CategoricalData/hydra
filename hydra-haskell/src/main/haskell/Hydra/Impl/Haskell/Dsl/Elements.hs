module Hydra.Impl.Haskell.Dsl.Elements where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Prototyping.CoreEncoding
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Impl.Haskell.Dsl.CoreMeta

import qualified Data.Map as M
import qualified Data.Set as S


emptyCoreContext :: Context Meta
emptyCoreContext = Context {
    contextGraphs = GraphSet {
      graphSetGraphs = M.fromList [(emptyGraphName, emptyGraph)],
      graphSetRoot = emptyGraphName},
    contextElements = M.empty,
    contextFunctions = M.empty,
    contextStrategy = EvaluationStrategy {
      evaluationStrategyOpaqueTermVariants = S.fromList []},
    contextDescriptionOf = metaDescription,
    contextTypeOf = metaType,
    contextSetTypeOf = \t m -> m {metaType = t}}
  where
    emptyGraphName = "empty"
    emptyGraph = Graph emptyGraphName [] (const True) "empty"

standardElement :: Context Meta -> Name -> String -> String -> Type -> Term Meta -> Element Meta
standardElement cx ns name desc typ term = Element (ns ++ "." ++ name) (encodeType cx typ) $ withDoc desc term

standardFunction :: Context Meta -> Name -> String -> String -> Type -> Type -> Term Meta -> Element Meta
standardFunction cx ns name desc dom cod = standardElement cx ns name desc typ
  where
    typ = functionType dom cod

standardGraph :: Name -> [Context Meta -> Element Meta] -> Graph Meta
standardGraph name els = Graph name elements dataTerms schemaGraph
  where
    cx = emptyCoreContext
    dataTerms = const True -- TODO
    elements = (\f -> f cx) <$> els
    schemaGraph = "hydra/core"

typeElement :: Context Meta -> Name -> Type -> Element Meta
typeElement cx name typ = Element {
  elementName = name,
  elementSchema = defaultTerm $ ExpressionElement _Type,
  elementData = encodeType cx typ}
