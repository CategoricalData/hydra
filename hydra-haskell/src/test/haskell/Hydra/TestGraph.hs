module Hydra.TestGraph (
  module Hydra.TestGraph,
  module Hydra.Impl.Haskell.Sources.Libraries,
) where

import Hydra.Core
import Hydra.Module
import Hydra.Compute
import Hydra.Impl.Haskell.Dsl.Standard as Standard
import Hydra.Impl.Haskell.Sources.Core
import Hydra.Lexical
import Hydra.Impl.Haskell.Sources.Libraries
import Hydra.CoreEncoding
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Terms

import qualified Data.Map  as M
import qualified Data.Set  as S


latLonName :: Name
latLonName = Name "LatLon"

latlonRecord :: Float -> Float -> Term m
latlonRecord lat lon = record latLonName [Field (FieldName "lat") $ float32 lat, Field (FieldName "lon") $ float32 lon]

latLonType :: Type m
latLonType = TypeRecord $ RowType latLonName [Types.field "lat" Types.float32, Types.field "lon" Types.float32]

testContext :: Context Meta
testContext = coreContext {
    contextGraph = testGraph,
    contextStrategy = EvaluationStrategy {
      evaluationStrategyOpaqueTermVariants = S.fromList [ -- TODO: revisit this list
        TermVariantLiteral,
        TermVariantElement,
        TermVariantFunction]}}

testElementArthur :: Element Meta
testElementArthur = Element {
  elementName = Name "ArthurDent",
  elementSchema = element $ Name "Person",
  elementData = testDataArthur}

testElementFirstName :: Element Meta
testElementFirstName = Element {
  elementName = Name "firstName",
  elementSchema = encodeType (Types.function (Types.nominal testTypePersonName) Types.string),
  elementData = projection testTypePersonName $ FieldName "firstName"}

testGraph :: Graph Meta
testGraph = elementsToGraph (Just testSchemaGraph) [testElementArthur, testElementFirstName]

testNamespace :: Namespace
testNamespace = Namespace "testGraph"

testSchemaGraph :: Graph Meta
testSchemaGraph = standardGraph [
    def (Name "StringTypeAlias") $ Standard.doc "An alias for the string type" Types.string,
    def testTypeFoobarValueName testTypeFoobarValue,
    def testTypeComparisonName testTypeComparison,
    def latLonName latLonType,
    def testTypePersonName testTypePerson,
    def testTypePersonOrSomethingName testTypePersonOrSomething,
    def testTypeTimestampName testTypeTimestamp]
  where
    def = typeElement

testSchemaNamespace :: Namespace
testSchemaNamespace = Namespace "testSchemaGraph"

testStrategy :: EvaluationStrategy
testStrategy = contextStrategy testContext

testDataArthur :: Term Meta
testDataArthur = record testTypePersonName [
  Field (FieldName "firstName") $ string "Arthur",
  Field (FieldName "lastName") $ string "Dent",
  Field (FieldName "age") $ int32 42]

testTypeComparison :: Type m
testTypeComparison = TypeUnion $ RowType testTypeComparisonName [
  Types.field "lessThan" Types.unit,
  Types.field "equalTo" Types.unit,
  Types.field "greaterThan" Types.unit]

testTypeComparisonName :: Name
testTypeComparisonName = Name "Comparison"

testTypeFoobarValue :: Type m
testTypeFoobarValue = TypeUnion $ RowType testTypeFoobarValueName [
  Types.field "bool" Types.boolean,
  Types.field "string" Types.string,
  Types.field "unit" Types.unit]

testTypeFoobarValueName :: Name
testTypeFoobarValueName = Name "FoobarValue"

testTypePerson :: Type Meta
testTypePerson = TypeRecord $ RowType testTypePersonName [
  Types.field "firstName" Types.string,
  Types.field "lastName" Types.string,
  Types.field "age" Types.int32]

testTypePersonName :: Name
testTypePersonName = Name "Person"

testTypePersonOrSomething :: Type Meta
testTypePersonOrSomething = Types.lambda "a" $ TypeUnion $ RowType testTypePersonOrSomethingName [
  Types.field "person" testTypePerson,
  Types.field "other" $ Types.variable "a"]

testTypePersonOrSomethingName :: Name
testTypePersonOrSomethingName = Name "PersonOrSomething"

testTypeTimestamp :: Type Meta
testTypeTimestamp = TypeUnion $ RowType testTypeTimestampName [
  FieldType (FieldName "unixTimeMillis") Types.uint64,
  FieldType (FieldName "date") Types.string]

testTypeTimestampName :: Name
testTypeTimestampName = Name "Timestamp"
