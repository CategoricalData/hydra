-- | Package manifest for hydra-wasm: DSL sources for the WebAssembly (WAT) coder.

module Hydra.Sources.Wasm.Manifest (
  mainModules,
  testModules,
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

testModules :: [Module]
testModules = []
