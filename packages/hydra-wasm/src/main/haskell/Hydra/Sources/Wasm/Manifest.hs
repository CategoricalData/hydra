-- | Package manifest for hydra-wasm: DSL sources for the WebAssembly (WAT) coder.

module Hydra.Sources.Wasm.Manifest (
  mainModules,
  testModules,
  dslTypeModules,
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

-- | Modules in this package whose type definitions should produce derived
-- DSL wrapper modules. Empty today — WasmSyntax is the natural
-- candidate if/when the wrappers are wanted.
dslTypeModules :: [Module]
dslTypeModules = []

testModules :: [Module]
testModules = []
