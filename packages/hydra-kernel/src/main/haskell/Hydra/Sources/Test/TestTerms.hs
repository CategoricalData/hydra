module Hydra.Sources.Test.TestTerms where

-- Standard imports for kernel test fixtures
import Hydra.Kernel
import           Hydra.Dsl.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

import qualified Hydra.Dsl.Packaging        as DPackaging
import           Prelude hiding ((++))


-- Type alias to avoid confusion with DSL Module
type HydraModule = Module

ns :: ModuleName
ns = ModuleName "hydra.test.testTerms"

module_ :: HydraModule
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> [TestTypes.ns, ModuleName "hydra.core"],
            moduleMetadata = descriptionMetadata ((Just "Term definitions for the test suite"))}
  where
    definitions = [
      Phantoms.toDefinition latlonRecord,
      Phantoms.toDefinition testDataArthur,
      Phantoms.toDefinition testElementArthur,
      Phantoms.toDefinition testElementFirstName]

defineTerm :: String -> TypedTerm a -> TypedTermDefinition a
defineTerm = definitionInModule module_

latlonRecord :: TypedTermDefinition (Float -> Float -> Term)
latlonRecord = defineTerm "latlonRecord" $
  Phantoms.lambdas ["lat", "lon"] $ record TestTypes.testTypeLatLonName [
    "lat">: float32Lift $ varPhantom "lat",
    "lon">: float32Lift $ varPhantom "lon"]

testDataArthur :: TypedTermDefinition Term
testDataArthur = defineTerm "testDataArthur" $
  record TestTypes.testTypePersonName [
    "firstName">: string "Arthur",
    "lastName">: string "Dent",
    "age">: int32 42]

testElementArthur :: TypedTermDefinition Binding
testElementArthur = defineTerm "testElementArthur" $
  Core.binding
    (name "firstName")
    testDataArthur
    (Phantoms.just $ Core.typeScheme (Phantoms.list ([] :: [TypedTerm Name])) (Core.typeVariable TestTypes.testTypePersonName) Phantoms.nothing)

testElementFirstName :: TypedTermDefinition Binding
testElementFirstName = defineTerm "testElementFirstName" $
  Core.binding
    (name "firstName")
    (project TestTypes.testTypePersonName (name "firstName"))
    (Phantoms.just $ Core.typeScheme (Phantoms.list ([] :: [TypedTerm Name]))
      (Core.typeFunction $ Core.functionType (Core.typeVariable TestTypes.testTypePersonName) T.string) Phantoms.nothing)
