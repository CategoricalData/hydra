-- Note: this is an automatically generated file. Do not edit.

-- | A module defining the graph used in the test suite.

module Hydra.Test.TestGraph where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Test.TestEnv as TestEnv
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Module as Module
import qualified Hydra.Test.TestTerms as TestTerms
import qualified Hydra.Test.TestTypes as TestTypes
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M

testContext :: Context.Context
testContext = TestEnv.testContext

testGraph :: Graph.Graph
testGraph = TestEnv.testGraph testTypes

testNamespace :: Module.Namespace
testNamespace = Module.Namespace "testGraph"

testSchemaNamespace :: Module.Namespace
testSchemaNamespace = Module.Namespace "testSchemaGraph"

testTerms :: M.Map Core.Name Core.Term
testTerms = Maps.fromList [
  (Core.Name "testDataArthur", TestTerms.testDataArthur)]

testTypes :: M.Map Core.Name Core.Type
testTypes =
    Maps.fromList [
      (TestTypes.testTypeBuddyListAName, TestTypes.testTypeBuddyListA),
      (TestTypes.testTypeBuddyListBName, TestTypes.testTypeBuddyListB),
      (TestTypes.testTypeComparisonName, TestTypes.testTypeComparison),
      (TestTypes.testTypeEitherName, TestTypes.testTypeEither),
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
      (TestTypes.testTypeTripleName, TestTypes.testTypeTriple),
      (TestTypes.testTypeUnionMonomorphicName, TestTypes.testTypeUnionMonomorphic),
      (TestTypes.testTypeUnionPolymorphicRecursiveName, TestTypes.testTypeUnionPolymorphicRecursive),
      (TestTypes.testTypeUnitName, TestTypes.testTypeUnit)]
