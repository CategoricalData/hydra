-- | Package manifest for hydra-typescript.
--
-- Owns the TypeScript coder DSL sources. See feature_290_packaging-plan.md,
-- "Sync system redesign / Package manifests".

module Hydra.Sources.TypeScript.Manifest (
  mainModules,
  testModules,
) where

import Hydra.Kernel

import qualified Hydra.Sources.TypeScript.Coder as TypeScriptCoder
import qualified Hydra.Sources.TypeScript.Language as TypeScriptLanguage
import qualified Hydra.Sources.TypeScript.Operators as TypeScriptOperators
import qualified Hydra.Sources.TypeScript.Serde as TypeScriptSerde
import qualified Hydra.Sources.TypeScript.Syntax as TypeScriptSyntax

mainModules :: [Module]
mainModules = [
  TypeScriptCoder.module_,
  TypeScriptLanguage.module_,
  TypeScriptOperators.module_,
  TypeScriptSerde.module_,
  TypeScriptSyntax.module_]

testModules :: [Module]
testModules = []
