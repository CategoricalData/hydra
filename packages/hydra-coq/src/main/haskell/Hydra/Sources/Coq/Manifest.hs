-- | Package manifest for hydra-coq.
--
-- Owns the Coq coder DSL sources. See feature_290_packaging-plan.md,
-- "Sync system redesign / Package manifests".
--
-- Note: CoqCoder is intentionally excluded. The hand-written Coq term encoder
-- at heads/haskell/.../Hydra/Ext/Coq/Coder.hs is more capable than the DSL
-- version would be, and the DSL source would overwrite it with a simpler
-- (broken) version if included in regeneration.

module Hydra.Sources.Coq.Manifest (
  mainModules,
  testModules,
) where

import Hydra.Kernel

import qualified Hydra.Sources.Coq.Language as CoqLanguage
import qualified Hydra.Sources.Coq.Serde as CoqSerde

mainModules :: [Module]
mainModules = [
  CoqLanguage.module_,
  CoqSerde.module_]

testModules :: [Module]
testModules = []
