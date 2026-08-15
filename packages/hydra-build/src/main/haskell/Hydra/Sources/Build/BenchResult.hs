module Hydra.Sources.Build.BenchResult where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types                 ((>:))
import qualified Hydra.Overlay.Haskell.Dsl.Types                 as T
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y


-- | Type-level model for benchmark result JSON (#416 P5): the typed result model
-- that the three bench drivers (run-benchmark-tests.sh, run-inference-bench.sh,
-- bench-generator-hosts.sh) previously hand-assembled with @echo@/@>>@ string
-- concatenation and manual comma bookkeeping (a real correctness hazard). Encoding
-- this as a Hydra type gives a generated JSON codec that replaces the hand-rolled
-- assembly. The @median@ / metadata-injection LOGIC lives in the sibling terms
-- module; the timed benchmark run + git-fact gathering stay native (passed in as
-- data). See https://github.com/CategoricalData/hydra/issues/416
ns :: ModuleName
ns = ModuleName "hydra.build.benchresult"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = [],
            moduleMetadata = descriptionMetadata (Just ("Type-level model for benchmark result JSON:"
              ++ " a typed result model whose generated codec replaces the hand-rolled JSON assembly in the"
              ++ " bench drivers. See https://github.com/CategoricalData/hydra/issues/416"))}
  where
    definitions = [
      benchMetadata,
      genBenchResult,
      hostResult,
      runStats]

-- | Per-run timing metadata spliced to the front of a bench result: when the run
-- happened and the git context it ran against. Gathered natively (git rev-parse /
-- git log) and passed in; modeled here so it serializes as part of the report.
benchMetadata :: TypeDefinition
benchMetadata = define "BenchMetadata" $
  doc "Per-run metadata: timestamp and git context of a benchmark run" $
  T.record [
    "timestamp">:
      doc "ISO-8601 UTC timestamp of the run"
      T.string,
    "language">:
      doc "The host language the benchmark ran on"
      T.string,
    "branch">:
      doc "The git branch"
      T.string,
    "commit">:
      doc "The short git commit hash"
      T.string,
    "commitMessage">:
      doc "The commit subject line"
      T.string]

-- | A generator-benchmark result: one target, the run count, and per-host results.
genBenchResult :: TypeDefinition
genBenchResult = define "GenBenchResult" $
  doc "A generator-benchmark result: target, run count, and per-host results" $
  T.record [
    "target">:
      doc "The target language being generated"
      T.string,
    "runs">:
      doc "The number of timed runs per (host, package)"
      T.int32,
    "hosts">:
      doc "Per-host results, keyed by host language name"
      (T.map T.string hostResult),
    "metadata">:
      doc "Optional run metadata"
      (T.optional benchMetadata)]

-- | One host's generator-benchmark result: its setup time and per-package timings.
hostResult :: TypeDefinition
hostResult = define "HostResult" $
  doc "One host's generator-benchmark result: setup time and per-package timings" $
  T.record [
    "setupMs">:
      doc "Host setup/build time in milliseconds"
      T.int32,
    "packages">:
      doc "Per-package run statistics, keyed by package name"
      (T.map T.string runStats)]

-- | The timing statistics for one (host, package): every run's elapsed time and
-- the median. The median is computed by the terms module, not re-derived by
-- consumers.
runStats :: TypeDefinition
runStats = define "RunStats" $
  doc "Timing statistics for one (host, package): per-run elapsed times and the median" $
  T.record [
    "runsMs">:
      doc "Each run's elapsed time in milliseconds"
      (T.list T.int32),
    "medianMs">:
      doc "The median elapsed time in milliseconds"
      T.int32]
