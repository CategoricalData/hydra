module Hydra.TestGraph (
  graphElementsMap,
  testContext,
  testGraph,
  testStrategy,
  module Hydra.Impl.Haskell.Lib.Strings,
) where

import Hydra.Core
import Hydra.Graph
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl
import Hydra.Prototyping.CoreGraph
import Hydra.Prototyping.Primitives
import Hydra.Prototyping.CoreEncoding
import Hydra.Impl.Haskell.Lib.Strings

import qualified Data.Map  as M
import qualified Data.Set  as S


testContext :: Context Meta
testContext = Context {
    contextGraphs = GraphSet {
      graphSetGraphs = M.fromList [
        ("testGraph", testGraph),
        ("testSchemaGraph", testSchemaGraph),
        ("hydra/core", hydraCoreGraph)],
      graphSetRoot = "testGraph"},
    contextElements = graphElementsMap testGraph,
    contextFunctions = M.fromList $ fmap (\p -> (primitiveFunctionName p, p)) stringPrimitives,
    contextStrategy = EvaluationStrategy {
      evaluationStrategyOpaqueTermVariants = S.fromList [ -- TODO: revisit this list
        TermVariantAtomic,
        TermVariantElement,
        TermVariantFunction]}}

testGraph :: Graph Meta
testGraph = Graph "testGraph" [arthur] allTerms "testSchemaGraph"
  where
    arthur = Element {
      elementName = "ArthurDent",
      elementSchema = element "Person",
      elementData = record [
        Field "firstName" $ stringValue "Arthur",
        Field "lastName" $ stringValue "Dent",
        Field "age" $ int32Value 42]}

testSchemaGraph :: Graph Meta
testSchemaGraph = Graph "testSchemaGraph" [exampleNominalType] allTerms "hydra/core"
  where
    exampleNominalType = typeElement "StringTypeAlias" stringType
    exampleVertexType = typeElement "Person" $
      TypeRecord [
        FieldType "firstName" stringType,
        FieldType "lastName" stringType,
        FieldType "age" int32Type]

testStrategy :: EvaluationStrategy
testStrategy = contextStrategy testContext

noElements :: [Element Meta]
noElements = []

allTerms :: Term Meta -> Bool
allTerms _ = True

typeElement :: Name -> Type -> Element Meta
typeElement name typ = Element {
  elementName = name,
  elementSchema = encodeType $ TypeNominal "hydra/core.Type",
  elementData = encodeType typ}
