module Hydra.TestGraph (
  graphElementsMap,
  latLonName,
  latlonRecord,
  latLonType,
  testContext,
  testElementArthur,
  testElementFirstName,
  testGraph,
  testGraphName,
  testStrategy,
  testDataArthur,
  testTypeColor,
  testTypeColorName,
  testTypeComparison,
  testTypeComparisonName,
  testTypePerson,
  testTypePersonName,
  testTypeTimestamp,
  testTypeTimestampName,
  module Hydra.Impl.Haskell.Sources.Libraries,
) where

import Hydra.Common
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
import Hydra.Impl.Haskell.Dsl.Terms

import qualified Data.Map  as M
import qualified Data.Set  as S


cx :: Context Meta
cx = coreContext

latLonName :: Name
latLonName = Name "LatLon"

latlonRecord :: (Eq m, Ord m, Read m, Show m) => Float -> Float -> Term m
latlonRecord lat lon = record latLonName [Field (FieldName "lat") $ float32 lat, Field (FieldName "lon") $ float32 lon]

latLonType :: Type m
latLonType = TypeRecord $ RowType latLonName [Types.field "lat" Types.float32, Types.field "lon" Types.float32]

testContext :: Context Meta
testContext = coreContext {
    contextGraphs = GraphSet {
      graphSetGraphs = M.fromList [
        (testGraphName, testGraph),
        (testSchemaGraphName, testSchemaGraph),
        (hydraCoreName, hydraCore)],
      graphSetRoot = testGraphName},
    contextElements = graphElementsMap testGraph,
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
  elementSchema = encodeType cx (Types.function (Types.nominal testTypePersonName) Types.string),
  elementData = projection testTypePersonName $ FieldName "firstName"}

testGraph :: Graph Meta
testGraph = Graph testGraphName [testElementArthur, testElementFirstName] allTerms testSchemaGraphName

testGraphName :: GraphName
testGraphName = GraphName "testGraph"

testSchemaGraph :: Graph Meta
testSchemaGraph = Graph testSchemaGraphName [
    typeElement cx (Name "StringTypeAlias") $ Standard.doc "An alias for the string type" Types.string,
    typeElement cx testTypeColorName testTypeColor,
    typeElement cx testTypeComparisonName testTypeComparison,
    typeElement cx latLonName latLonType,
    typeElement cx testTypePersonName testTypePerson,
    typeElement cx testTypeTimestampName testTypeTimestamp]
  allTerms hydraCoreName

testSchemaGraphName :: GraphName
testSchemaGraphName = GraphName "testSchemaGraph"

testStrategy :: EvaluationStrategy
testStrategy = contextStrategy testContext

testDataArthur :: Term Meta
testDataArthur = nominalRecord cx (Name "Person") [
  Field (FieldName "firstName") $ string "Arthur",
  Field (FieldName "lastName") $ string "Dent",
  Field (FieldName "age") $ int32 42]

testTypeColor :: Type m
testTypeColor = TypeUnion $ RowType testTypeColorName [
  Types.field "bool" Types.boolean,
  Types.field "string" Types.string,
  Types.field "unit" Types.unit]

testTypeColorName :: Name
testTypeColorName = Name "Color"

testTypeComparison :: Type m
testTypeComparison = TypeUnion $ RowType testTypeComparisonName [
  Types.field "lessThan" Types.unit,
  Types.field "equalTo" Types.unit,
  Types.field "greaterThan" Types.unit]

testTypeComparisonName :: Name
testTypeComparisonName = Name "Comparison"

testTypePerson :: Type Meta
testTypePerson = TypeRecord $ RowType testTypePersonName [
  Types.field "firstName" Types.string,
  Types.field "lastName" Types.string,
  Types.field "age" Types.int32]

testTypePersonName :: Name
testTypePersonName = Name "Person"

testTypeTimestamp :: Type Meta
testTypeTimestamp = TypeUnion $ RowType testTypeTimestampName [
  FieldType (FieldName "unixTimeMillis") Types.uint64,
  FieldType (FieldName "date") Types.string]

testTypeTimestampName :: Name
testTypeTimestampName = Name "Timestamp"

allTerms :: Term Meta -> Bool
allTerms _ = True
