-- | Package manifest for hydra-javascript.
--
-- Owns the JavaScript coder DSL sources. See feature_290_packaging-plan.md,
-- "Sync system redesign / Package manifests".
--
-- Note: TypeScript DSL sources physically live in packages/hydra-ext/ today
-- and are declared by hydra-ext's manifest.

module Hydra.Sources.JavaScript.Manifest (
  mainModules,
  testModules,
) where

import Hydra.Kernel

import qualified Hydra.Sources.JavaScript.Language as JavaScriptLanguage
import qualified Hydra.Sources.JavaScript.Operators as JavaScriptOperators
import qualified Hydra.Sources.JavaScript.Serde as JavaScriptSerde
import qualified Hydra.Sources.JavaScript.Syntax as JavaScriptSyntax

mainModules :: [Module]
mainModules = [
  JavaScriptLanguage.module_,
  JavaScriptOperators.module_,
  JavaScriptSerde.module_,
  JavaScriptSyntax.module_]

testModules :: [Module]
testModules = []
