module Hydra.Sources.Test.TestGraph (
  module Hydra.Sources.Test.TestGraph,
  module Hydra.Sources.Test.TestTerms,
  module Hydra.Sources.Test.TestTypes,
) where

import Hydra.Kernel
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Graph         as Graph
import qualified Hydra.Dsl.Module        as DModule
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Types         as Types
import           Hydra.Sources.Kernel.Types.All
import qualified Hydra.Sources.Kernel.Types.Core as CoreTypes
import           Prelude hiding ((++))
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Dsl.TTypes as T
import           Hydra.Dsl.TTerms as TTerms
import           Hydra.Sources.Test.TestTerms
import           Hydra.Sources.Test.TestTypes

testGraphDefinition :: String -> TTerm a -> TBinding a
testGraphDefinition = definitionInModule testGraphModule

testGraphModule :: HydraModule
testGraphModule = Module (Namespace "hydra.test.testGraph") elements
    [testTermsModule, testTypesModule]
    kernelTypesModules $
    Just ("A module defining the graph used in the test suite.")
  where
    elements = [
      el testTermsDef,
      el testTypesDef,
      el testNamespaceDef,
      el testSchemaNamespaceDef]

testTermsDef :: TBinding (M.Map Name Term)
testTermsDef = testGraphDefinition "testTerms" $
  Maps.fromList $ Phantoms.list [
    Phantoms.tuple2 (name "testDataArthur") (ref testDataArthurDef)]

testNamespaceDef :: TBinding Namespace
testNamespaceDef = testGraphDefinition "testNamespace" $ DModule.namespace $ Phantoms.string "testGraph"

testTypesDef :: TBinding (M.Map Name Type)
testTypesDef = testGraphDefinition "testTypes" $
  Maps.fromList $ Phantoms.list [
    Phantoms.tuple2 (ref testTypeBuddyListANameDef) (ref testTypeBuddyListADef),
    Phantoms.tuple2 (ref testTypeBuddyListBNameDef) (ref testTypeBuddyListBDef),
    Phantoms.tuple2 (ref testTypeComparisonNameDef) (ref testTypeComparisonDef),
    Phantoms.tuple2 (ref testTypeEitherNameDef) (ref testTypeEitherDef),
    Phantoms.tuple2 (ref testTypeFlowNameDef) (ref testTypeFlowDef),
    Phantoms.tuple2 (ref testTypeFlowStateNameDef) (ref testTypeFlowStateDef),
    Phantoms.tuple2 (ref testTypeHydraLiteralTypeNameDef) (ref testTypeHydraLiteralTypeDef),
    Phantoms.tuple2 (ref testTypeHydraTypeNameDef) (ref testTypeHydraTypeDef),
    Phantoms.tuple2 (ref testTypeIntListNameDef) (ref testTypeIntListDef),
    Phantoms.tuple2 (ref testTypeLatLonNameDef) (ref testTypeLatLonDef),
    Phantoms.tuple2 (ref testTypeLatLonPolyNameDef) (ref testTypeLatLonPolyDef),
    Phantoms.tuple2 (ref testTypeListNameDef) (ref testTypeListDef),
    Phantoms.tuple2 (ref testTypeNumberNameDef) (ref testTypeNumberDef),
    Phantoms.tuple2 (ref testTypePersonNameDef) (ref testTypePersonDef),
    Phantoms.tuple2 (ref testTypePersonOrSomethingNameDef) (ref testTypePersonOrSomethingDef),
    Phantoms.tuple2 (ref testTypePolymorphicWrapperNameDef) (ref testTypePolymorphicWrapperDef),
    Phantoms.tuple2 (ref testTypeSimpleNumberNameDef) (ref testTypeSimpleNumberDef),
    Phantoms.tuple2 (ref testTypeStringAliasNameDef) (ref testTypeStringAliasDef),
    Phantoms.tuple2 (ref testTypeSymmetricTripleNameDef) (ref testTypeSymmetricTripleDef),
    Phantoms.tuple2 (ref testTypeTimestampNameDef) (ref testTypeTimestampDef),
    Phantoms.tuple2 (ref testTypeTraceNameDef) (ref testTypeTraceDef),
    Phantoms.tuple2 (ref testTypeTripleNameDef) (ref testTypeTripleDef),
    Phantoms.tuple2 (ref testTypeUnionMonomorphicNameDef) (ref testTypeUnionMonomorphicDef),
    Phantoms.tuple2 (ref testTypeUnionPolymorphicRecursiveNameDef) (ref testTypeUnionPolymorphicRecursiveDef),
    Phantoms.tuple2 (ref testTypeUnitNameDef) (ref testTypeUnitDef)]

testSchemaNamespaceDef :: TBinding Namespace
testSchemaNamespaceDef = testGraphDefinition "testSchemaNamespace" $ DModule.namespace $ Phantoms.string "testSchemaGraph"
