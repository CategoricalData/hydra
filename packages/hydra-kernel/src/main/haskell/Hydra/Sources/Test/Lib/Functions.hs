module Hydra.Sources.Test.Lib.Functions where

-- Standard imports for term-encoded tests
import Hydra.Kernel
import           Hydra.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Overlay.Haskell.Dsl.Typed.Testing                 as Testing
import Hydra.Overlay.Haskell.Dsl.Typed.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core          as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Phantoms      as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types         as T
import qualified Data.List                    as L
import qualified Data.Map                     as M

-- Additional imports specific to this file
import Hydra.Testing
import qualified Hydra.Overlay.Haskell.Dsl.Prims as Prims
import qualified Hydra.Lib.Functions as DefFunctions


ns :: ModuleName
ns = ModuleName "hydra.test.lib.functions"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> [ModuleName "hydra.reduction", ModuleName "hydra.print.core", ModuleName "hydra.core", ModuleName "hydra.errors", ModuleName "hydra.test.testGraph", ModuleName "hydra.testing", ModuleName "hydra.util"],
            moduleMetadata = descriptionMetadata (Just "Test cases for hydra.lib.functions primitives")}
  where
    definitions = [Phantoms.toDefinition allTests]

-- Test groups for hydra.lib.functions primitives

allTests :: TypedTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.lib.functions primitives" $
    supergroup "hydra.lib.functions primitives" [
      functionsIdentity]

functionsIdentity :: TypedTerm TestGroup
functionsIdentity = subgroup "identity" [
  test "integer" 42 42]
  where
    test name x result = primCase name DefFunctions.identity [int32 x] (int32 result)
