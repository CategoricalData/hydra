-- | Package manifest for hydra-wasm: DSL sources for the WebAssembly (WAT) coder.

module Hydra.Sources.Wasm.Manifest (
  mainModules,
  testModules,
  mainDslModules,
  mainEncodingModules,
) where

import Hydra.Kernel

import qualified Hydra.Sources.Wasm.Coder     as WasmCoder
import qualified Hydra.Sources.Wasm.Language  as WasmLanguageSource
import qualified Hydra.Sources.Wasm.Serde     as WasmSerdeSource
import qualified Hydra.Sources.Wasm.Syntax    as WasmSyntax

mainModules :: [Module]
mainModules = [
  WasmCoder.module_,
  WasmLanguageSource.module_,
  WasmSerdeSource.module_,
  WasmSyntax.module_]

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
