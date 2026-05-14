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

testModules :: [Module]
testModules = []
