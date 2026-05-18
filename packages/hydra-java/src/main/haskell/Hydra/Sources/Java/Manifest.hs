-- | Package manifest for hydra-java.
--
-- Owns the Java coder DSL sources. See feature_290_packaging-plan.md,
-- "Sync system redesign / Package manifests".

module Hydra.Sources.Java.Manifest (
  mainModules,
  testModules,
  dslTypeModules,
) where

import Hydra.Kernel

import qualified Hydra.Sources.Java.Coder as JavaCoder
import qualified Hydra.Sources.Java.Environment as JavaEnvironmentSource
import qualified Hydra.Sources.Java.Language as JavaLanguageSource
import qualified Hydra.Sources.Java.Names as JavaNamesSource
import qualified Hydra.Sources.Java.Serde as JavaSerdeSource
import qualified Hydra.Sources.Java.Syntax as JavaSyntax
import qualified Hydra.Sources.Java.Testing as JavaTesting
import qualified Hydra.Sources.Java.Utils as JavaUtilsSource

mainModules :: [Module]
mainModules = [
  JavaCoder.module_,
  JavaEnvironmentSource.module_,
  JavaLanguageSource.module_,
  JavaNamesSource.module_,
  JavaSerdeSource.module_,
  JavaSyntax.module_,
  JavaTesting.module_,
  JavaUtilsSource.module_]

-- | Modules in this package whose type definitions should produce derived
-- DSL wrapper modules (Hydra/Dsl/Java/<Name>.hs). The DSL generator
-- consumes this list to emit constructors / accessors / withXxx updaters
-- for each TypeDefinition. Term-only modules (Coder, Serde, Language,
-- Names, Testing, Utils) are deliberately excluded. Extend the list when
-- a new type-defining module needs DSL wrappers.
dslTypeModules :: [Module]
dslTypeModules = [
  JavaEnvironmentSource.module_,
  JavaSyntax.module_]

testModules :: [Module]
testModules = []
