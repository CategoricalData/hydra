-- | Package manifest for hydra-coq.
--
-- Owns the Coq coder DSL sources. See feature_290_packaging-plan.md,
-- "Sync system redesign / Package manifests".
--
-- As of #337 (feature_326_coq), the entire Coq code generator lives in
-- DSL form under `Sources/Coq/`: Coder, Environment, Generate, Language,
-- Serde, Syntax, and Utils. Only a 36-line host-side driver remains in
-- `heads/haskell/.../Hydra/Coq/GenerateDriver.hs` for the
-- `generateSources` signature adaptation and the `_CoqProject` writer.

module Hydra.Sources.Coq.Manifest (
  mainModules,
  testModules,
) where

import Hydra.Kernel

import qualified Hydra.Sources.Coq.Coder as CoqCoder
import qualified Hydra.Sources.Coq.Environment as CoqEnvironment
import qualified Hydra.Sources.Coq.Generate as CoqGenerate
import qualified Hydra.Sources.Coq.Language as CoqLanguage
import qualified Hydra.Sources.Coq.Serde as CoqSerde
import qualified Hydra.Sources.Coq.Syntax as CoqSyntax
import qualified Hydra.Sources.Coq.Utils as CoqUtils

mainModules :: [Module]
mainModules = [
  CoqCoder.module_,
  CoqEnvironment.module_,
  CoqGenerate.module_,
  CoqLanguage.module_,
  CoqSerde.module_,
  CoqSyntax.module_,
  CoqUtils.module_]

testModules :: [Module]
testModules = []
