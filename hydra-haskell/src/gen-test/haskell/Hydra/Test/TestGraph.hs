-- Note: this is an automatically generated file. Do not edit.

-- | A module defining the graph used in the test suite.

module Hydra.Test.TestGraph where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Module as Module
import qualified Hydra.Test.TestTerms as TestTerms
import qualified Hydra.Test.TestTypes as TestTypes
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

testTerms :: (M.Map Core.Name Core.Term)
testTerms = (Maps.fromList [
  (Core.Name "testDataArthur", TestTerms.testDataArthur)])

testTypes :: (M.Map Core.Name Core.Type)
testTypes = (Maps.fromList [
  (TestTypes.testTypeBuddyListAName, TestTypes.testTypeBuddyListA),
  (TestTypes.testTypeBuddyListBName, TestTypes.testTypeBuddyListB),
  (TestTypes.testTypeComparisonName, TestTypes.testTypeComparison),
  (TestTypes.testTypeEitherName, TestTypes.testTypeEither),
  (TestTypes.testTypeFlowName, TestTypes.testTypeFlow),
  (TestTypes.testTypeFlowStateName, TestTypes.testTypeFlowState),
  (TestTypes.testTypeHydraLiteralTypeName, TestTypes.testTypeHydraLiteralType),
  (TestTypes.testTypeHydraTypeName, TestTypes.testTypeHydraType),
  (TestTypes.testTypeIntListName, TestTypes.testTypeIntList),
  (TestTypes.testTypeLatLonName, TestTypes.testTypeLatLon),
  (TestTypes.testTypeLatLonPolyName, TestTypes.testTypeLatLonPoly),
  (TestTypes.testTypeListName, TestTypes.testTypeList),
  (TestTypes.testTypeNumberName, TestTypes.testTypeNumber),
  (TestTypes.testTypePersonName, TestTypes.testTypePerson),
  (TestTypes.testTypePersonOrSomethingName, TestTypes.testTypePersonOrSomething),
  (TestTypes.testTypePolymorphicWrapperName, TestTypes.testTypePolymorphicWrapper),
  (TestTypes.testTypeSimpleNumberName, TestTypes.testTypeSimpleNumber),
  (TestTypes.testTypeStringAliasName, TestTypes.testTypeStringAlias),
  (TestTypes.testTypeSymmetricTripleName, TestTypes.testTypeSymmetricTriple),
  (TestTypes.testTypeTimestampName, TestTypes.testTypeTimestamp),
  (TestTypes.testTypeTraceName, TestTypes.testTypeTrace),
  (TestTypes.testTypeTripleName, TestTypes.testTypeTriple),
  (TestTypes.testTypeUnionMonomorphicName, TestTypes.testTypeUnionMonomorphic),
  (TestTypes.testTypeUnionPolymorphicRecursiveName, TestTypes.testTypeUnionPolymorphicRecursive),
  (TestTypes.testTypeUnitName, TestTypes.testTypeUnit)])

testNamespace :: Module.Namespace
testNamespace = (Module.Namespace "testGraph")

testSchemaNamespace :: Module.Namespace
testSchemaNamespace = (Module.Namespace "testSchemaGraph")
