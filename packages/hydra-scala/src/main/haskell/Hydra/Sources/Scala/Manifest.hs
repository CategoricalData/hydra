-- | Package manifest for hydra-scala.
--
-- Owns the Scala coder DSL sources. See feature_290_packaging-plan.md,
-- "Sync system redesign / Package manifests".

module Hydra.Sources.Scala.Manifest (
  mainModules,
  testModules,
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

testModules :: [Module]
testModules = []
