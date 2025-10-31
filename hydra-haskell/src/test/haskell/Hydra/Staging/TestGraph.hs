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
import Hydra.Sources.Kernel.Types.All

import qualified Data.List  as L
import qualified Data.Map  as M
import qualified Data.Set  as S
import qualified Hydra.Dsl.Terms as Terms


testGraph :: Graph
testGraph = elementsToGraph hydraCoreGraph (Just testSchemaGraph) [testElementArthur, testElementFirstName]

testSchemaGraph :: Graph
testSchemaGraph = elementsToGraph hydraCoreGraph (Just hydraCoreGraph)
    -- We include all types from the Hydra kernel, as well as some additional types specifically for tests.
    (kernelElements ++ newElements)
  where
    def = typeElement
    kernelElements = L.concat $ fmap moduleElements kernelTypesModules
    newElements = [
      def testTypeBuddyListAName testTypeBuddyListA,
      def testTypeBuddyListBName testTypeBuddyListB,
      def testTypeComparisonName testTypeComparison,
      def testTypeEitherName testTypeEither,
      def testTypeIntListName testTypeIntList,
      def testTypeHydraLiteralTypeName testTypeHydraLiteralType,
      def testTypeHydraTypeName testTypeHydraType,
      def testTypeLatLonName testTypeLatLon,
      def testTypeLatLonPolyName testTypeLatLonPoly,
      def testTypeListName testTypeList,
      def testTypeNumberName testTypeNumber,
      def testTypePersonName testTypePerson,
      def testTypePersonOrSomethingName testTypePersonOrSomething,
      def testTypePolymorphicWrapperName testTypePolymorphicWrapper,
      def testTypeSimpleNumberName testTypeSimpleNumber,
      def testTypeStringAliasName $ Ann.doc "An alias for the string type" testTypeStringAlias,
      def testTypeSymmetricTripleName testTypeSymmetricTriple,
      def testTypeTimestampName testTypeTimestamp,
      def testTypeTripleName testTypeTriple,
      def testTypeUnionMonomorphicName testTypeUnionMonomorphic,
      def testTypeUnionPolymorphicRecursiveName testTypeUnionPolymorphicRecursive,
      def testTypeUnitName testTypeUnit,
      -- Simplified hydra.compute types
      def testTypeFlowName testTypeFlow,
      def testTypeFlowStateName testTypeFlowState,
      def testTypeTraceName testTypeTrace]