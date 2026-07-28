-- | Complete test suite for hydra-build, combining all of the package's own test groups.
--
-- Package-scoped counterpart to Hydra.Sources.Test.TestSuite (hydra-kernel); part of #547's
-- per-package test aggregation, replacing hydra-kernel's former direct reference to
-- hydra-build's test groups (#546 Option A).

module Hydra.Sources.Build.Test.Suite where

-- Standard imports for deep DSL tests (produces TypedTerm a with specific types)
import Hydra.Kernel
import           Hydra.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Overlay.Haskell.Dsl.Typed.Testing                 as Testing
import Hydra.Overlay.Haskell.Dsl.Typed.Phantoms                as Phantoms hiding ((++))
import Hydra.Sources.Kernel.Types.All

import qualified Hydra.Sources.Build.Test.Libraries as BuildLibraries
import qualified Hydra.Sources.Build.Test.Modules as BuildModules
import qualified Hydra.Sources.Build.Test.Reconcile as BuildReconcile
import qualified Hydra.Sources.Build.Test.Routing as BuildRouting


ns :: ModuleName
ns = ModuleName "hydra.test.build.testSuite"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> (namespaces ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata (Just "The test suite for hydra-build, combining its test groups.")}
  where
    definitions = [Phantoms.toDefinition allTests]
    namespaces = fst <$> testPairs

allTests :: TypedTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    doc "The group of all hydra-build tests" $
    Testing.testGroup (string "build") nothing (list subgroups) (list ([] :: [TypedTerm TestCaseWithMetadata]))
  where
    subgroups = snd <$> testPairs

testPairs :: [(ModuleName, TypedTermDefinition TestGroup)]
testPairs = [
  (BuildLibraries.ns, BuildLibraries.allTests),
  (BuildModules.ns, BuildModules.allTests),
  (BuildReconcile.ns, BuildReconcile.allTests),
  (BuildRouting.ns, BuildRouting.allTests)]
