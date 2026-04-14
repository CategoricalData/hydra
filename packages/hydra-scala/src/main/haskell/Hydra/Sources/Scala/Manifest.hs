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
) where

import Hydra.Kernel

import qualified Hydra.Sources.Scala.Coder as ScalaCoder
import qualified Hydra.Sources.Scala.Language as ScalaLanguage
import qualified Hydra.Sources.Scala.Serde as ScalaSerde
import qualified Hydra.Sources.Scala.Syntax as ScalaSyntax
import qualified Hydra.Sources.Scala.Utils as ScalaUtils

mainModules :: [Module]
mainModules = [
  ScalaCoder.module_,
  ScalaLanguage.module_,
  ScalaSerde.module_,
  ScalaSyntax.module_,
  ScalaUtils.module_]

testModules :: [Module]
testModules = []
