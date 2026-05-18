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
  dslTypeModules,
) where

import Hydra.Kernel

import qualified Hydra.Sources.Coq.Coder as CoqCoder
import qualified Hydra.Sources.Coq.Environment as CoqEnvironmentSource
import qualified Hydra.Sources.Coq.Generate as CoqGenerate
import qualified Hydra.Sources.Coq.Language as CoqLanguage
import qualified Hydra.Sources.Coq.Serde as CoqSerdeSource
import qualified Hydra.Sources.Coq.Syntax as CoqSyntax
import qualified Hydra.Sources.Coq.Utils as CoqUtils

mainModules :: [Module]
mainModules = [
  CoqCoder.module_,
  CoqEnvironmentSource.module_,
  CoqGenerate.module_,
  CoqLanguage.module_,
  CoqSerdeSource.module_,
  CoqSyntax.module_,
  CoqUtils.module_]

-- | Modules in this package whose type definitions should produce derived
-- DSL wrapper modules (Hydra/Dsl/<Pkg>/<Name>.hs). Only modules whose
-- derived DSL is actually imported elsewhere are listed — extend the
-- list as new consumers appear.
--
-- Current consumers (as of 2026-05-16):
--   * Hydra.Dsl.Coq.Syntax — imported by Hydra.Sources.Coq.Generate
dslTypeModules :: [Module]
dslTypeModules = [
  CoqSyntax.module_]

testModules :: [Module]
testModules = []
