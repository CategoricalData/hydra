-- | Package manifest for hydra-build.
--
-- Owns Hydra's build-system DSL sources: the build-format type schemas
-- (hydra.build.format), the manifest-derived module-to-package
-- router (hydra.build.routing), the kernel/host reconciliation utilities
-- (hydra.build.reconcile), the pure module-list utilities
-- (hydra.build.modules), the translingual expected-libraries registry
-- (hydra.build.libraries), the distribution assembly-plan model
-- (hydra.build.assembly) and its pure derivation (hydra.build.assemblyplan),
-- and the pure directory-traversal decision helpers (hydra.build.walk),
-- plus their test modules. See #546 (extraction from
-- hydra-kernel), #512 (build formats as Hydra types), #533 (the libraries
-- registry), and #416 (promotion of the build system into Hydra).
--
-- hydra-build is the first non-kernel package to declare non-empty testModules;
-- the JSON-writing drivers route hydra.test.build.* to this package's test tree
-- via each package's Manifest.testModules (see Hydra.Sources.Ext.extRoutingInput
-- and heads/haskell/src/exec/transform-haskell-dsl-to-json packageTestModules).
--
-- testModules includes its own generated test-aggregate module
-- (hydra.test.build.testSuite, from Hydra.Sources.Build.Test.Suite), combining
-- hydra-build's test groups (#547). Each host runs it via its own runner file
-- (independent of hydra-kernel's hydra.test.testSuite runner), replacing the
-- #546 "Option A" arrangement where the kernel's test suite imported
-- hydra-build's test groups directly.

module Hydra.Sources.Build.Manifest (
  mainModules,
  testModules,
  mainDslModules,
  mainEncodingModules,
) where

import Hydra.Kernel

import qualified Hydra.Sources.Build.Assembly as BuildAssembly
import qualified Hydra.Sources.Build.AssemblyPlan as BuildAssemblyPlan
import qualified Hydra.Sources.Build.BenchResult as BuildBenchResult
import qualified Hydra.Sources.Build.CompareReport as BuildCompareReport
import qualified Hydra.Sources.Build.CompareReportLogic as BuildCompareReportLogic
import qualified Hydra.Sources.Build.Format as BuildFormat
import qualified Hydra.Sources.Build.LangExpansion as BuildLangExpansion
import qualified Hydra.Sources.Build.Libraries as BuildLibraries
import qualified Hydra.Sources.Build.ManifestWriter as BuildManifestWriter
import qualified Hydra.Sources.Build.Modules as BuildModules
import qualified Hydra.Sources.Build.PackagingProfile as BuildPackagingProfile
import qualified Hydra.Sources.Build.PublishSets as BuildPublishSets
import qualified Hydra.Sources.Build.Reconcile as BuildReconcile
import qualified Hydra.Sources.Build.Routing as BuildRouting
import qualified Hydra.Sources.Build.SyncMatrix as BuildSyncMatrix
import qualified Hydra.Sources.Build.VersionConsistency as BuildVersionConsistency
import qualified Hydra.Sources.Build.Walk as BuildWalk
import qualified Hydra.Sources.Build.Test.Libraries as TestBuildLibraries
import qualified Hydra.Sources.Build.Test.Modules as TestBuildModules
import qualified Hydra.Sources.Build.Test.Reconcile as TestBuildReconcile
import qualified Hydra.Sources.Build.Test.Routing as TestBuildRouting
import qualified Hydra.Sources.Build.Test.Suite as BuildTestSuite

mainModules :: [Module]
mainModules = [
  BuildAssembly.module_,
  BuildAssemblyPlan.module_,
  BuildBenchResult.module_,
  BuildCompareReport.module_,
  BuildCompareReportLogic.module_,
  BuildFormat.module_,
  BuildLangExpansion.module_,
  BuildLibraries.module_,
  BuildManifestWriter.module_,
  BuildModules.module_,
  BuildPackagingProfile.module_,
  BuildPublishSets.module_,
  BuildReconcile.module_,
  BuildRouting.module_,
  BuildSyncMatrix.module_,
  BuildVersionConsistency.module_,
  BuildWalk.module_]

-- | hydra.build.format is the package's one type-defining module (#512); it
-- gives rise to generated DSL wrappers.
mainDslModules :: [Module]
mainDslModules = [
  BuildAssembly.module_,
  BuildBenchResult.module_,
  BuildCompareReport.module_,
  BuildFormat.module_]

-- | Encoding and decoding modules are generated for the build-format types,
-- making each on-disk build-format JSON file decodable as its defined type.
mainEncodingModules :: [Module]
mainEncodingModules = [
  BuildAssembly.module_,
  BuildBenchResult.module_,
  BuildCompareReport.module_,
  BuildFormat.module_]

testModules :: [Module]
testModules = [
  TestBuildLibraries.module_,
  TestBuildModules.module_,
  TestBuildReconcile.module_,
  TestBuildRouting.module_,
  BuildTestSuite.module_]
