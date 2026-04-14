-- | Aggregates Hydra source modules via per-package manifest modules.
--
-- The per-package sublists (kernelModules, haskellModules, jsonModules,
-- otherModules) are defined in terms of imports from each package's
-- Manifest module. See feature_290_packaging-plan.md, "Sync system redesign
-- / Package manifests".

module Hydra.Sources.All(
  module Hydra.Sources.All,
  module Hydra.Sources.Kernel.Terms.All,
  module Hydra.Sources.Kernel.Types.All,
  module Hydra.Sources.Test.All,
) where

import Hydra.Kernel
import Hydra.Sources.Kernel.Terms.All
import Hydra.Sources.Kernel.Types.All
import Hydra.Sources.Test.All

import qualified Hydra.Sources.Kernel.Manifest as KernelManifest
import qualified Hydra.Sources.Haskell.Manifest as HaskellManifest

import qualified Hydra.Sources.Kernel.Terms.Dsls as Dsls


mainModules :: [Module]
mainModules = kernelModules ++ haskellModules ++ jsonModules ++ otherModules

-- | The DSL source module (hydra.dsls) must be generated separately from mainModules
-- because including it in the main generation causes a stack overflow due to the
-- complexity of its term definitions (which reference decoders, the full type graph, etc.)
dslSourceModules :: [Module]
dslSourceModules = [Dsls.module_]

-- | Kernel types and terms plus JSON runtime.
--
-- Note: this is the subset used by update-json-kernel. It is a strict subset
-- of KernelManifest.mainModules (which also includes otherModules).
kernelModules :: [Module]
kernelModules = kernelTypesModules ++ kernelTermsModules ++ jsonModules

haskellModules :: [Module]
haskellModules = HaskellManifest.mainModules

jsonModules :: [Module]
jsonModules = KernelManifest.jsonModules

otherModules :: [Module]
otherModules = KernelManifest.otherModules
