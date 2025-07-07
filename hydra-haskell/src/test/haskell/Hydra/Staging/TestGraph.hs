module Hydra.Staging.TestGraph (
  module Hydra.Staging.TestGraph,
  module Hydra.Sources.Libraries,
  module Hydra.Test.TestGraph,
) where

import Hydra.Kernel
import Hydra.Sources.Libraries
import Hydra.Dsl.Terms
import Hydra.Sources.Kernel.Types.Core
import Hydra.Dsl.Annotations as Ann
import Hydra.Dsl.Bootstrap
import qualified Hydra.Dsl.Types as Types
import Hydra.Test.TestGraph

import qualified Data.Map  as M
import qualified Data.Set  as S
import qualified Hydra.Dsl.Terms as Terms


testGraph :: Graph
testGraph = elementsToGraph hydraCoreGraph (Just testSchemaGraph) [testElementArthur, testElementFirstName]

testSchemaGraph :: Graph
testSchemaGraph = elementsToGraph hydraCoreGraph (Just hydraCoreGraph) [
    def testTypeBuddyListAName testTypeBuddyListA,
    def testTypeBuddyListBName testTypeBuddyListB,
    def testTypeComparisonName testTypeComparison,
    def testTypeIntListName testTypeIntList,
    def testTypeHydraLiteralTypeName testTypeHydraLiteralType,
    def testTypeHydraTypeName testTypeHydraType,
    def testTypeLatLonName testTypeLatLon,
    def testTypeLatLonPolyName testTypeLatLonPoly,
    def testTypeListName testTypeList,
    def testTypeNumberName testTypeNumber,
    def testTypePersonName testTypePerson,
    def testTypePersonOrSomethingName testTypePersonOrSomething,
    def testTypeSimpleNumberName testTypeSimpleNumber,
    def testTypeStringAliasName $ Ann.doc "An alias for the string type" testTypeStringAlias,
    def testTypePolymorphicWrapperName testTypePolymorphicWrapper,
    def testTypeTimestampName testTypeTimestamp,
    def testTypeUnionMonomorphicName testTypeUnionMonomorphic,
    def testTypeUnionPolymorphicRecursiveName testTypeUnionPolymorphicRecursive]
  where
    def = typeElement
