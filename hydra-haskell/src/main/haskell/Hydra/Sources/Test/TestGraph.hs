module Hydra.Sources.Test.TestGraph where

import Hydra.Kernel
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Module        as DModule
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import           Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Types         as Types
import           Hydra.Sources.Kernel.Types.All
import qualified Hydra.Sources.Kernel.Types.Core as CoreTypes
import           Prelude hiding ((++))
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Dsl.Meta.Types as T
import           Hydra.Dsl.Meta.Terms as MetaTerms
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes


module_ :: Module
module_ = Module (Namespace "hydra.test.testGraph") elements
    [TestTerms.module_, TestTypes.module_]
    kernelTypesModules $
    Just ("A module defining the graph used in the test suite.")
  where
   elements = [
     el testTermsDef,
     el testTypesDef,
     el testNamespaceDef,
     el testSchemaNamespaceDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

testTermsDef :: TBinding (M.Map Name Term)
testTermsDef = define "testTerms" $
  Maps.fromList $ Phantoms.list [
    Phantoms.pair (name "testDataArthur") (ref TestTerms.testDataArthurDef)]

testNamespaceDef :: TBinding Namespace
testNamespaceDef = define "testNamespace" $ DModule.namespace $ Phantoms.string "testGraph"

testTypesDef :: TBinding (M.Map Name Type)
testTypesDef = define "testTypes" $
  Maps.fromList $ Phantoms.list [
    Phantoms.pair (ref TestTypes.testTypeBuddyListANameDef) (ref TestTypes.testTypeBuddyListADef),
    Phantoms.pair (ref TestTypes.testTypeBuddyListBNameDef) (ref TestTypes.testTypeBuddyListBDef),
    Phantoms.pair (ref TestTypes.testTypeComparisonNameDef) (ref TestTypes.testTypeComparisonDef),
    Phantoms.pair (ref TestTypes.testTypeEitherNameDef) (ref TestTypes.testTypeEitherDef),
    Phantoms.pair (ref TestTypes.testTypeFlowNameDef) (ref TestTypes.testTypeFlowDef),
    Phantoms.pair (ref TestTypes.testTypeFlowStateNameDef) (ref TestTypes.testTypeFlowStateDef),
    Phantoms.pair (ref TestTypes.testTypeHydraLiteralTypeNameDef) (ref TestTypes.testTypeHydraLiteralTypeDef),
    Phantoms.pair (ref TestTypes.testTypeHydraTypeNameDef) (ref TestTypes.testTypeHydraTypeDef),
    Phantoms.pair (ref TestTypes.testTypeIntListNameDef) (ref TestTypes.testTypeIntListDef),
    Phantoms.pair (ref TestTypes.testTypeLatLonNameDef) (ref TestTypes.testTypeLatLonDef),
    Phantoms.pair (ref TestTypes.testTypeLatLonPolyNameDef) (ref TestTypes.testTypeLatLonPolyDef),
    Phantoms.pair (ref TestTypes.testTypeListNameDef) (ref TestTypes.testTypeListDef),
    Phantoms.pair (ref TestTypes.testTypeNumberNameDef) (ref TestTypes.testTypeNumberDef),
    Phantoms.pair (ref TestTypes.testTypePersonNameDef) (ref TestTypes.testTypePersonDef),
    Phantoms.pair (ref TestTypes.testTypePersonOrSomethingNameDef) (ref TestTypes.testTypePersonOrSomethingDef),
    Phantoms.pair (ref TestTypes.testTypePolymorphicWrapperNameDef) (ref TestTypes.testTypePolymorphicWrapperDef),
    Phantoms.pair (ref TestTypes.testTypeSimpleNumberNameDef) (ref TestTypes.testTypeSimpleNumberDef),
    Phantoms.pair (ref TestTypes.testTypeStringAliasNameDef) (ref TestTypes.testTypeStringAliasDef),
    Phantoms.pair (ref TestTypes.testTypeSymmetricTripleNameDef) (ref TestTypes.testTypeSymmetricTripleDef),
    Phantoms.pair (ref TestTypes.testTypeTimestampNameDef) (ref TestTypes.testTypeTimestampDef),
    Phantoms.pair (ref TestTypes.testTypeTraceNameDef) (ref TestTypes.testTypeTraceDef),
    Phantoms.pair (ref TestTypes.testTypeTripleNameDef) (ref TestTypes.testTypeTripleDef),
    Phantoms.pair (ref TestTypes.testTypeUnionMonomorphicNameDef) (ref TestTypes.testTypeUnionMonomorphicDef),
    Phantoms.pair (ref TestTypes.testTypeUnionPolymorphicRecursiveNameDef) (ref TestTypes.testTypeUnionPolymorphicRecursiveDef),
    Phantoms.pair (ref TestTypes.testTypeUnitNameDef) (ref TestTypes.testTypeUnitDef)]

testSchemaNamespaceDef :: TBinding Namespace
testSchemaNamespaceDef = define "testSchemaNamespace" $ DModule.namespace $ Phantoms.string "testSchemaGraph"
