module Hydra.TestGraph (
  module Hydra.TestGraph,
  module Hydra.Sources.Libraries,
) where

import Hydra.Kernel
import Hydra.Dsl.Standard as Standard
import Hydra.Sources.Core
import Hydra.Sources.Libraries
import Hydra.CoreEncoding
import qualified Hydra.Dsl.Types as Types
import Hydra.Dsl.Terms

import qualified Data.Map  as M
import qualified Data.Set  as S


latLonName :: Name
latLonName = Name "LatLon"

latlonRecord :: Float -> Float -> Term m
latlonRecord lat lon = record latLonName [Field (FieldName "lat") $ float32 lat, Field (FieldName "lon") $ float32 lon]

latLonType :: Type m
latLonType = TypeRecord $ RowType latLonName Nothing [Types.field "lat" Types.float32, Types.field "lon" Types.float32]

testContext :: Context Kv
testContext = coreContext {
    contextGraph = testGraph}

testElementArthur :: Element Kv
testElementArthur = Element {
  elementName = Name "ArthurDent",
  elementSchema = element $ Name "Person",
  elementData = testDataArthur}

testElementFirstName :: Element Kv
testElementFirstName = Element {
  elementName = Name "firstName",
  elementSchema = encodeType (Types.function (Types.wrap testTypePersonName) Types.string),
  elementData = projection testTypePersonName $ FieldName "firstName"}

testGraph :: Graph Kv
testGraph = elementsToGraph (Just testSchemaGraph) [testElementArthur, testElementFirstName]

testNamespace :: Namespace
testNamespace = Namespace "testGraph"

testSchemaGraph :: Graph Kv
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

testDataArthur :: Term Kv
testDataArthur = record testTypePersonName [
  Field (FieldName "firstName") $ string "Arthur",
  Field (FieldName "lastName") $ string "Dent",
  Field (FieldName "age") $ int32 42]

testTypeComparison :: Type m
testTypeComparison = TypeUnion $ RowType testTypeComparisonName Nothing [
  Types.field "lessThan" Types.unit,
  Types.field "equalTo" Types.unit,
  Types.field "greaterThan" Types.unit]

testTypeComparisonName :: Name
testTypeComparisonName = Name "Comparison"

testTypeFoobarValue :: Type m
testTypeFoobarValue = TypeUnion $ RowType testTypeFoobarValueName Nothing [
  Types.field "bool" Types.boolean,
  Types.field "string" Types.string,
  Types.field "unit" Types.unit]

testTypeFoobarValueName :: Name
testTypeFoobarValueName = Name "FoobarValue"

testTypePerson :: Type Kv
testTypePerson = TypeRecord $ RowType testTypePersonName Nothing [
  Types.field "firstName" Types.string,
  Types.field "lastName" Types.string,
  Types.field "age" Types.int32]

testTypePersonName :: Name
testTypePersonName = Name "Person"

testTypePersonOrSomething :: Type Kv
testTypePersonOrSomething = Types.lambda "a" $ TypeUnion $ RowType testTypePersonOrSomethingName Nothing [
  Types.field "person" testTypePerson,
  Types.field "other" $ Types.variable "a"]

testTypePersonOrSomethingName :: Name
testTypePersonOrSomethingName = Name "PersonOrSomething"

testTypeTimestamp :: Type Kv
testTypeTimestamp = TypeUnion $ RowType testTypeTimestampName Nothing [
  FieldType (FieldName "unixTimeMillis") Types.uint64,
  FieldType (FieldName "date") Types.string]

testTypeTimestampName :: Name
testTypeTimestampName = Name "Timestamp"
