module Hydra.TestGraph (
  graphElementsMap,
  testContext,
  testElementArthur,
  testElementFirstName,
  testGraph,
  testStrategy,
  testDataArthur,
  testTypePerson,
  testTypeTimestamp,
  module Hydra.Impl.Haskell.Sources.Libraries,
) where

import Hydra.Core
import Hydra.Graph
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl.CoreMeta
import Hydra.Impl.Haskell.Dsl.Standard as Standard
import Hydra.Impl.Haskell.Sources.Core
import Hydra.Primitives
import Hydra.Impl.Haskell.Sources.Libraries
import Hydra.CoreEncoding
import qualified Hydra.Impl.Haskell.Dsl.Types as Types

import qualified Data.Map  as M
import qualified Data.Set  as S


cx :: Context Meta
cx = standardContext

testGraphName :: GraphName
testGraphName = GraphName "testGraph"

testSchemaGraphName :: GraphName
testSchemaGraphName = GraphName "testSchemaGraph"

testContext :: Context Meta
testContext = standardContext {
    contextGraphs = GraphSet {
      graphSetGraphs = M.fromList [
        (testGraphName, testGraph),
        (testSchemaGraphName, testSchemaGraph),
        (hydraCoreName, hydraCore)],
      graphSetRoot = testGraphName},
    contextElements = graphElementsMap testGraph,
    contextFunctions = M.fromList $ fmap (\p -> (primitiveFunctionName p, p)) standardPrimitives,
    contextStrategy = EvaluationStrategy {
      evaluationStrategyOpaqueDataVariants = S.fromList [ -- TODO: revisit this list
        DataVariantLiteral,
        DataVariantElement,
        DataVariantFunction]}}

testElementArthur :: Element Meta
testElementArthur = Element {
  elementName = Name "ArthurDent",
  elementSchema = element $ Name "Person",
  elementData = testDataArthur}

testElementFirstName :: Element Meta
testElementFirstName = Element {
  elementName = Name "firstName",
  elementSchema = encodeType cx (Types.function (Types.nominal $ Name "Person") Types.string),
  elementData = projection $ FieldName "firstName"}

testGraph :: Graph Meta
testGraph = Graph testGraphName [testElementArthur, testElementFirstName] allTerms testSchemaGraphName

testSchemaGraph :: Graph Meta
testSchemaGraph = Graph testSchemaGraphName [
    typeElement cx (Name "StringTypeAlias") $ Standard.doc "An alias for the string type" Types.string,
    typeElement cx (Name "Person") testTypePerson,
    typeElement cx (Name "Timestamp") testTypeTimestamp]
  allTerms hydraCoreName

testStrategy :: EvaluationStrategy
testStrategy = contextStrategy testContext

testDataArthur :: Data Meta
testDataArthur = nominalRecord cx (Name "Person") [
  Field (FieldName "firstName") $ stringValue "Arthur",
  Field (FieldName "lastName") $ stringValue "Dent",
  Field (FieldName "age") $ int32Value 42]

testTypePerson :: Type Meta
testTypePerson = Types.record [
  FieldType (FieldName "firstName") Types.string,
  FieldType (FieldName "lastName") Types.string,
  FieldType (FieldName "age") Types.int32]

testTypeTimestamp :: Type Meta
testTypeTimestamp = Types.union [
  FieldType (FieldName "unixTimeMillis") Types.uint64,
  FieldType (FieldName "date") Types.string]

allTerms :: Data Meta -> Bool
allTerms _ = True
