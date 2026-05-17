-- | Package manifest for hydra-haskell.
--
-- Owns the Haskell coder DSL sources. See feature_290_packaging-plan.md,
-- "Sync system redesign / Package manifests".

module Hydra.Sources.Haskell.Manifest (
  mainModules,
  testModules,
  dslTypeModules,
) where

import Hydra.Kernel

import qualified Hydra.Sources.Haskell.Coder as HaskellCoder
import qualified Hydra.Sources.Haskell.Environment as HaskellEnvironment
import qualified Hydra.Sources.Haskell.Language as HaskellLanguage
import qualified Hydra.Sources.Haskell.Operators as HaskellOperators
import qualified Hydra.Sources.Haskell.Serde as HaskellSerde
import qualified Hydra.Sources.Haskell.Syntax as HaskellSyntax
import qualified Hydra.Sources.Haskell.Testing as HaskellTesting
import qualified Hydra.Sources.Haskell.Utils as HaskellUtilsSource

mainModules :: [Module]
mainModules = [
  HaskellCoder.module_,
  HaskellEnvironment.module_,
  HaskellLanguage.module_,
  HaskellOperators.module_,
  HaskellSerde.module_,
  HaskellSyntax.module_,
  HaskellTesting.module_,
  HaskellUtilsSource.module_]

-- | Modules in this package whose type definitions should produce derived
-- DSL wrapper modules (Hydra/Dsl/Haskell/<Name>.hs). The DSL generator
-- consumes this list to emit constructors / accessors / withXxx updaters
-- for each TypeDefinition. Term-only modules (Coder, Serde, Operators,
-- etc.) are deliberately excluded. Extend the list when a new
-- type-defining module needs DSL wrappers.
dslTypeModules :: [Module]
dslTypeModules = [
  HaskellEnvironment.module_,
  HaskellSyntax.module_]

testModules :: [Module]
testModules = []
