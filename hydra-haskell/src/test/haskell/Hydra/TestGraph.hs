module Hydra.TestGraph (
  graphElementsForContext,
  testContext,
  testGraph,
  testStrategy,
) where

import Hydra.Core
import Hydra.Graph
import Hydra.Evaluation
import Hydra.Prototyping.Basics
import Hydra.Prototyping.Helpers
import Hydra.Prototyping.Primitives

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Set  as S


graphElementsForContext :: Graph -> M.Map Name Element
graphElementsForContext g = M.fromList $ fmap toPair $ graphElements g
  where
    toPair e = (elementName e, e)

testContext :: Context
testContext = Context elements functions strategy
  where
    elements = graphElementsForContext testGraph
    strategy = EvaluationStrategy {
      evaluationStrategyOpaqueTermVariants = S.fromList [ -- TODO: revisit this list
        TermVariantAtomic,
        TermVariantCases,
        TermVariantData,
        TermVariantElement,
        TermVariantFunction,
        TermVariantLambda,
        TermVariantProjection]}
    functions = M.fromList [
      ("concat", PrimitiveFunction concat (FunctionType stringType (TypeFunction $ FunctionType stringType stringType))),
      ("toLower", PrimitiveFunction toLower (FunctionType stringType stringType)),
      ("toUpper", PrimitiveFunction toUpper (FunctionType stringType stringType))]

    concat :: [Term] -> Term
    concat [x, y] = case (x, y) of
      (TermAtomic (AtomicValueString x'), TermAtomic (AtomicValueString y'))
        -> TermAtomic $ AtomicValueString $ x' ++ y'

    toLower :: [Term] -> Term
    toLower [x] = case x of
      TermAtomic (AtomicValueString s)
        -> TermAtomic $ AtomicValueString $ fmap C.toLower s

    toUpper :: [Term] -> Term
    toUpper [x] = case x of
      TermAtomic (AtomicValueString s)
        -> TermAtomic $ AtomicValueString $ fmap C.toUpper s

testGraph :: Graph
testGraph = Graph e d s
  where
    e = []
    d = \t -> True
    s = testGraph

testStrategy :: EvaluationStrategy
testStrategy = contextStrategy testContext
