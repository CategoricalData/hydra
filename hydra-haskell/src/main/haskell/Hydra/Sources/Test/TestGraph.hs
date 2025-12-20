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


ns :: Namespace
ns = Namespace "hydra.test.testGraph"

module_ :: Module
module_ = Module ns elements
    [TestTerms.ns, TestTypes.ns]
    kernelTypesNamespaces $
    Just ("A module defining the graph used in the test suite.")
  where
   elements = [
     Phantoms.toBinding testTerms,
     Phantoms.toBinding testTypes,
     Phantoms.toBinding testNamespace,
     Phantoms.toBinding testSchemaNamespace]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

testTerms :: TBinding (M.Map Name Term)
testTerms = define "testTerms" $
  Maps.fromList $ Phantoms.list [
    Phantoms.pair (name "testDataArthur") TestTerms.testDataArthur]

testNamespace :: TBinding Namespace
testNamespace = define "testNamespace" $ DModule.namespace $ Phantoms.string "testGraph"

testTypes :: TBinding (M.Map Name Type)
testTypes = define "testTypes" $
  Maps.fromList $ Phantoms.list [
    Phantoms.pair TestTypes.testTypeBuddyListAName TestTypes.testTypeBuddyListA,
    Phantoms.pair TestTypes.testTypeBuddyListBName TestTypes.testTypeBuddyListB,
    Phantoms.pair TestTypes.testTypeComparisonName TestTypes.testTypeComparison,
    Phantoms.pair TestTypes.testTypeEitherName TestTypes.testTypeEither,
    Phantoms.pair TestTypes.testTypeFlowName TestTypes.testTypeFlow,
    Phantoms.pair TestTypes.testTypeFlowStateName TestTypes.testTypeFlowState,
    Phantoms.pair TestTypes.testTypeHydraLiteralTypeName TestTypes.testTypeHydraLiteralType,
    Phantoms.pair TestTypes.testTypeHydraTypeName TestTypes.testTypeHydraType,
    Phantoms.pair TestTypes.testTypeIntListName TestTypes.testTypeIntList,
    Phantoms.pair TestTypes.testTypeLatLonName TestTypes.testTypeLatLon,
    Phantoms.pair TestTypes.testTypeLatLonPolyName TestTypes.testTypeLatLonPoly,
    Phantoms.pair TestTypes.testTypeListName TestTypes.testTypeList,
    Phantoms.pair TestTypes.testTypeNumberName TestTypes.testTypeNumber,
    Phantoms.pair TestTypes.testTypePersonName TestTypes.testTypePerson,
    Phantoms.pair TestTypes.testTypePersonOrSomethingName TestTypes.testTypePersonOrSomething,
    Phantoms.pair TestTypes.testTypePolymorphicWrapperName TestTypes.testTypePolymorphicWrapper,
    Phantoms.pair TestTypes.testTypeSimpleNumberName TestTypes.testTypeSimpleNumber,
    Phantoms.pair TestTypes.testTypeStringAliasName TestTypes.testTypeStringAlias,
    Phantoms.pair TestTypes.testTypeSymmetricTripleName TestTypes.testTypeSymmetricTriple,
    Phantoms.pair TestTypes.testTypeTimestampName TestTypes.testTypeTimestamp,
    Phantoms.pair TestTypes.testTypeTraceName TestTypes.testTypeTrace,
    Phantoms.pair TestTypes.testTypeTripleName TestTypes.testTypeTriple,
    Phantoms.pair TestTypes.testTypeUnionMonomorphicName TestTypes.testTypeUnionMonomorphic,
    Phantoms.pair TestTypes.testTypeUnionPolymorphicRecursiveName TestTypes.testTypeUnionPolymorphicRecursive,
    Phantoms.pair TestTypes.testTypeUnitName TestTypes.testTypeUnit]

testSchemaNamespace :: TBinding Namespace
testSchemaNamespace = define "testSchemaNamespace" $ DModule.namespace $ Phantoms.string "testSchemaGraph"
