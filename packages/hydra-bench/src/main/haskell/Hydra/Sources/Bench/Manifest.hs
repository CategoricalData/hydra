-- | Package manifest for hydra-bench.
--
-- Owns the synthetic inference benchmark workloads (hydra.bench.*).
-- These modules are deliberately stress-shaped (deep curried cascades, wide
-- records, branchy DAGs) and are NOT part of the standard sync pipeline:
-- regenerate them on demand with bin/sync-bench.sh before running
-- bin/run-inference-bench.sh.

module Hydra.Sources.Bench.Manifest (
  mainModules,
  testModules,
  mainDslModules,
  mainEncodingModules,
) where

import Hydra.Kernel

import qualified Hydra.Sources.Bench.FanOut as BenchFanOut
import qualified Hydra.Sources.Bench.LinearChain as BenchLinearChain
import qualified Hydra.Sources.Bench.PolymorphicChain as BenchPolymorphicChain

mainModules :: [Module]
mainModules = [
  BenchFanOut.module_,
  BenchLinearChain.module_,
  BenchPolymorphicChain.module_]

-- | Modules in this package whose type definitions should produce derived
-- DSL wrapper modules. Empty — benchmark workloads are deliberately
-- stress-shaped term modules, not type modules, and would not benefit
-- from DSL wrappers.
mainDslModules :: [Module]
mainDslModules = []

-- | Empty for now: encode/decode for this package's modules is not yet supported across eta-expanding targets (see #475). Re-add modules here once #475 is fixed.
mainEncodingModules :: [Module]
mainEncodingModules = []

testModules :: [Module]
testModules = []
