module Hydra.TestGraph (
  graphElementsMap,
  testContext,
  testGraph,
  testStrategy,
  testTypePerson,
  testTypeTimestamp,
  module Hydra.Impl.Haskell.Lib.Strings,
) where

import Hydra.Core
import Hydra.Graph
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl
import Hydra.Prototyping.CoreGraph
import Hydra.Prototyping.Primitives
import Hydra.Prototyping.CoreEncoding
import Hydra.Impl.Haskell.Lib.Math
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
    contextFunctions = M.fromList $ fmap (\p -> (primitiveFunctionName p, p)) allPrimitives,
    contextStrategy = EvaluationStrategy {
      evaluationStrategyOpaqueTermVariants = S.fromList [ -- TODO: revisit this list
        TermVariantLiteral,
        TermVariantElement,
        TermVariantFunction]}}
  where
    allPrimitives = mathPrimitives ++ stringPrimitives

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
testSchemaGraph = Graph "testSchemaGraph" [
      typeElement "StringTypeAlias" stringType,
      typeElement "Person" testTypePerson,
      typeElement "Timestamp" testTypeTimestamp]
    allTerms "hydra/core"

testStrategy :: EvaluationStrategy
testStrategy = contextStrategy testContext

testTypePerson :: Type
testTypePerson = TypeRecord [
  FieldType "firstName" stringType,
  FieldType "lastName" stringType,
  FieldType "age" int32Type]

testTypeTimestamp :: Type
testTypeTimestamp = TypeUnion [
  FieldType "unixTimeMillis" uint64Type,
  FieldType "date" stringType]
  
allTerms :: Term Meta -> Bool
allTerms _ = True

typeElement :: Name -> Type -> Element Meta
typeElement name typ = Element {
  elementName = name,
  elementSchema = encodeType $ TypeNominal "hydra/core.Type",
  elementData = encodeType typ}
