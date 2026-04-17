-- Note: this is an automatically generated file. Do not edit.
-- (Bootstrap stub: the real generated file is overwritten by the next
--  run of heads/haskell/bin/sync-ext.sh. See feature_325_wasm-plan.md.)

-- | WebAssembly code generator: converts Hydra type and term modules to WAT source code

module Hydra.Wasm.Coder where

import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Packaging as Packaging

import qualified Data.Map as M

-- | Bootstrap stub. Overwritten by update-haskell-ext-main on the next sync-ext.
moduleToWasm ::
  Packaging.Module ->
  [Packaging.Definition] ->
  cx ->
  Graph.Graph ->
  Either Errors.Error (M.Map String String)
moduleToWasm _mod _defs _cx _g = Right M.empty
