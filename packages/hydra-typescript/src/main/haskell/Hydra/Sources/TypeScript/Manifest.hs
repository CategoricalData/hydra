-- | Package manifest for hydra-typescript.
--
-- Owns the TypeScript coder DSL sources. See feature_290_packaging-plan.md,
-- "Sync system redesign / Package manifests".

module Hydra.Sources.TypeScript.Manifest (
  mainModules,
  testModules,
  mainDslModules,
  mainEncodingModules,
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

-- Source modules from which dsl/encode/decode are derived (#474): every
-- type-defining module in the package, no per-module curation.
mainDslModules :: [Module]
mainDslModules = filter moduleDefinesType mainModules

-- | Empty for now: encode/decode for this package's modules is not yet supported across eta-expanding targets (see #475). Re-add modules here once #475 is fixed.
mainEncodingModules :: [Module]
mainEncodingModules = []

-- | True if a module defines at least one type.
moduleDefinesType :: Module -> Bool
moduleDefinesType m = any isTypeDef (moduleDefinitions m)
  where
    isTypeDef (DefinitionType _) = True
    isTypeDef _                  = False

testModules :: [Module]
testModules = []
