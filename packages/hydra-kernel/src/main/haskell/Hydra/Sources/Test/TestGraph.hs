module Hydra.Sources.Test.TestGraph where

-- Standard imports for kernel test fixtures
import Hydra.Kernel
import           Hydra.Dsl.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Terms                   as Terms hiding ((@@))
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import           Hydra.Dsl.Meta.Phantoms                ((@@))
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical
import qualified Hydra.Sources.Test.TestEnv as TestEnv
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Packaging        as DPackaging
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import qualified Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Terms              as DslTerms
import qualified Hydra.Dsl.Types              as Types
import qualified Hydra.Sources.Kernel.Types.Core as CoreTypes
import           Prelude hiding ((++))
import qualified Data.List                    as L
import qualified Data.Map                     as M
import qualified Data.Set                     as S
import qualified Data.Maybe                   as Y


ns :: ModuleName
ns = ModuleName "hydra.test.testGraph"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([TestTerms.ns, TestTypes.ns, TestEnv.ns, Lexical.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata (Just ("A module defining the graph used in the test suite."))}
  where
   definitions = [
     Phantoms.toDefinition testTerms,
     Phantoms.toDefinition testTypes,
     Phantoms.toDefinition testModuleName,
     Phantoms.toDefinition testSchemaModuleName,
     Phantoms.toDefinition testGraph,
     Phantoms.toDefinition testContext]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

-- | The test context. Emits a reference to the hand-written
-- Hydra.Test.TestEnv.testContext.
testContext :: TypedTermDefinition InferenceContext
testContext = define "testContext" $ asTerm TestEnv.testContext

-- | The test graph. Emits a call to the hand-written
-- Hydra.Test.TestEnv.testGraph (applied to testTypes and testTerms).
testGraph :: TypedTermDefinition Graph
testGraph = define "testGraph" $
  TestEnv.testGraph @@ testTypes @@ testTerms

testModuleName :: TypedTermDefinition ModuleName
testModuleName = define "testModuleName" $ DPackaging.moduleName2 $ Phantoms.string "testGraph"

testSchemaModuleName :: TypedTermDefinition ModuleName
testSchemaModuleName = define "testSchemaModuleName" $ DPackaging.moduleName2 $ Phantoms.string "testSchemaGraph"

testTerms :: TypedTermDefinition (M.Map Name Term)
testTerms = define "testTerms" $
  Maps.fromList $ Phantoms.list [
    Phantoms.pair (name "testDataArthur") TestTerms.testDataArthur]

testTypes :: TypedTermDefinition (M.Map Name Type)
testTypes = define "testTypes" $
  Maps.fromList $ Phantoms.list [
    Phantoms.pair TestTypes.testTypeBuddyListAName TestTypes.testTypeBuddyListA,
    Phantoms.pair TestTypes.testTypeBuddyListBName TestTypes.testTypeBuddyListB,
    Phantoms.pair TestTypes.testTypeComparisonName TestTypes.testTypeComparison,
    Phantoms.pair TestTypes.testTypeEitherName TestTypes.testTypeEither,
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
    Phantoms.pair TestTypes.testTypeTripleName TestTypes.testTypeTriple,
    Phantoms.pair TestTypes.testTypeUnionMonomorphicName TestTypes.testTypeUnionMonomorphic,
    Phantoms.pair TestTypes.testTypeUnionPolymorphicRecursiveName TestTypes.testTypeUnionPolymorphicRecursive,
    Phantoms.pair TestTypes.testTypeUnitName TestTypes.testTypeUnit]
