-- | Package manifest for hydra-java.
--
-- Owns the Java coder DSL sources. See feature_290_packaging-plan.md,
-- "Sync system redesign / Package manifests".

module Hydra.Sources.Java.Manifest (
  mainModules,
  testModules,
) where

import Hydra.Kernel

import qualified Hydra.Sources.Java.Coder as JavaCoder
import qualified Hydra.Sources.Java.Environment as JavaEnvironment
import qualified Hydra.Sources.Java.Language as JavaLanguage
import qualified Hydra.Sources.Java.Names as JavaNames
import qualified Hydra.Sources.Java.Serde as JavaSerde
import qualified Hydra.Sources.Java.Syntax as JavaSyntax
import qualified Hydra.Sources.Java.Testing as JavaTesting
import qualified Hydra.Sources.Java.Utils as JavaUtils

mainModules :: [Module]
mainModules = [
  JavaCoder.module_,
  JavaEnvironment.module_,
  JavaLanguage.module_,
  JavaNames.module_,
  JavaSerde.module_,
  JavaSyntax.module_,
  JavaTesting.module_,
  JavaUtils.module_]

testModules :: [Module]
testModules = []
