-- | Complete test suite for hydra-pg, combining all of the package's own test groups.
--
-- Package-scoped counterpart to Hydra.Sources.Test.TestSuite (hydra-kernel), following the
-- hydra-build precedent (packages/hydra-build/.../Build/Test/Suite.hs).

module Hydra.Sources.Pg.Test.Suite where

-- Standard imports for deep DSL tests (produces TypedTerm a with specific types)
import Hydra.Kernel
import           Hydra.Core.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing                 as Testing
import Hydra.Core.Overlay.Haskell.Dsl.Phantoms                as Phantoms hiding ((++))
import Hydra.Sources.Kernel.Types.All

import qualified Hydra.Sources.Pg.Test.SubtermGraph as PgSubtermGraphTest


ns :: ModuleName
ns = ModuleName "hydra.pg.test.testSuite"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> (namespaces ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata (Just "The test suite for hydra-pg, combining its test groups.")}
  where
    definitions = [Phantoms.toDefinition allTests]
    namespaces = fst <$> testPairs

allTests :: TypedTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    doc "The group of all hydra-pg tests" $
    Testing.testGroup (string "pg") nothing (list subgroups) (list ([] :: [TypedTerm TestCaseWithMetadata]))
  where
    subgroups = snd <$> testPairs

testPairs :: [(ModuleName, TypedTermDefinition TestGroup)]
testPairs = [
  (PgSubtermGraphTest.ns, PgSubtermGraphTest.allTests)]
