-- | Package manifest for hydra-scala.
--
-- Owns the Scala coder DSL sources. See feature_290_packaging-plan.md,
-- "Sync system redesign / Package manifests".
--
-- Note: Hydra.Sources.Scala.Prepare is intentionally excluded — it is a helper
-- module used by other Scala sources, not itself a generated module.

module Hydra.Sources.Scala.Manifest (
  mainModules,
  testModules,
  dslTypeModules,
) where

import Hydra.Kernel

import qualified Hydra.Sources.Scala.Coder as ScalaCoder
import qualified Hydra.Sources.Scala.Language as ScalaLanguageSource
import qualified Hydra.Sources.Scala.Serde as ScalaSerdeSource
import qualified Hydra.Sources.Scala.Syntax as ScalaSyntax
import qualified Hydra.Sources.Scala.Utils as ScalaUtilsSource

mainModules :: [Module]
mainModules = [
  ScalaCoder.module_,
  ScalaLanguageSource.module_,
  ScalaSerdeSource.module_,
  ScalaSyntax.module_,
  ScalaUtilsSource.module_]

-- | Modules in this package whose type definitions should produce derived
-- DSL wrapper modules (Hydra/Dsl/Scala/<Name>.hs). The DSL generator
-- consumes this list to emit constructors / accessors / withXxx updaters
-- for each TypeDefinition. Term-only modules (Coder, Language, Serde,
-- Utils) are deliberately excluded. Extend the list when a new
-- type-defining module needs DSL wrappers.
dslTypeModules :: [Module]
dslTypeModules = [
  ScalaSyntax.module_]

testModules :: [Module]
testModules = []
