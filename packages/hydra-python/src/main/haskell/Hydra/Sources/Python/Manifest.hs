-- | Package manifest for hydra-python.
--
-- Owns the Python coder DSL sources. See feature_290_packaging-plan.md,
-- "Sync system redesign / Package manifests".

module Hydra.Sources.Python.Manifest (
  mainModules,
  testModules,
  dslTypeModules,
) where

import Hydra.Kernel

import qualified Hydra.Sources.Python.Coder as PythonCoder
import qualified Hydra.Sources.Python.Environment as PyEnvironmentSource
import qualified Hydra.Sources.Python.Language as PyLanguage
import qualified Hydra.Sources.Python.Names as PyNames
import qualified Hydra.Sources.Python.Serde as PySerde
import qualified Hydra.Sources.Python.Syntax as PySyntax
import qualified Hydra.Sources.Python.Testing as PythonTesting
import qualified Hydra.Sources.Python.Utils as PyUtils

mainModules :: [Module]
mainModules = [
  PythonCoder.module_,
  PyEnvironmentSource.module_,
  PyLanguage.module_,
  PyNames.module_,
  PySerde.module_,
  PySyntax.module_,
  PythonTesting.module_,
  PyUtils.module_]

-- | Modules in this package whose type definitions should produce derived
-- DSL wrapper modules (Hydra/Dsl/Python/<Name>.hs). The DSL generator
-- consumes this list to emit constructors / accessors / withXxx updaters
-- for each TypeDefinition. Term-only modules (Coder, Serde, Language,
-- Names, Testing, Utils) are deliberately excluded. Extend the list when
-- a new type-defining module needs DSL wrappers.
dslTypeModules :: [Module]
dslTypeModules = [
  PyEnvironmentSource.module_,
  PySyntax.module_]

testModules :: [Module]
testModules = []
