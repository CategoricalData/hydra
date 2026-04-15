-- | Package manifest for hydra-python.
--
-- Owns the Python coder DSL sources. See feature_290_packaging-plan.md,
-- "Sync system redesign / Package manifests".

module Hydra.Sources.Python.Manifest (
  mainModules,
  testModules,
) where

import Hydra.Kernel

import qualified Hydra.Sources.Python.Coder as PythonCoder
import qualified Hydra.Sources.Python.Environment as PythonEnvironment
import qualified Hydra.Sources.Python.Language as PythonLanguage
import qualified Hydra.Sources.Python.Names as PythonNames
import qualified Hydra.Sources.Python.Serde as PythonSerde
import qualified Hydra.Sources.Python.Syntax as PythonSyntax
import qualified Hydra.Sources.Python.Testing as PythonTesting
import qualified Hydra.Sources.Python.Utils as PythonUtils

mainModules :: [Module]
mainModules = [
  PythonCoder.module_,
  PythonEnvironment.module_,
  PythonLanguage.module_,
  PythonNames.module_,
  PythonSerde.module_,
  PythonSyntax.module_,
  PythonTesting.module_,
  PythonUtils.module_]

testModules :: [Module]
testModules = []
