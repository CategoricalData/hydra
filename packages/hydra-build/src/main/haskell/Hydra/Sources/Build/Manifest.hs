-- | Package manifest for hydra-build.
--
-- Owns Hydra's build-system DSL sources: the manifest-derived module-to-package
-- router (hydra.build.routing), the kernel/host reconciliation utilities
-- (hydra.build.reconcile), and the pure module-list utilities
-- (hydra.build.modules), plus their test modules. See #546 (extraction from
-- hydra-kernel) and #416 (promotion of the build system into Hydra).
--
-- hydra-build is the first non-kernel package to declare non-empty testModules;
-- the JSON-writing drivers route hydra.test.build.* to this package's test tree
-- via each package's Manifest.testModules (see Hydra.Sources.Ext.extRoutingInput
-- and heads/haskell/src/exec/transform-haskell-dsl-to-json packageTestModules).

module Hydra.Sources.Build.Manifest (
  mainModules,
  testModules,
  mainDslModules,
  mainEncodingModules,
) where

import Hydra.Kernel

import qualified Hydra.Sources.Build.Modules as BuildModules
import qualified Hydra.Sources.Build.Reconcile as BuildReconcile
import qualified Hydra.Sources.Build.Routing as BuildRouting
import qualified Hydra.Sources.Build.Test.Modules as TestBuildModules
import qualified Hydra.Sources.Build.Test.Reconcile as TestBuildReconcile
import qualified Hydra.Sources.Build.Test.Routing as TestBuildRouting

mainModules :: [Module]
mainModules = [
  BuildModules.module_,
  BuildReconcile.module_,
  BuildRouting.module_]

-- | The build modules define terms (utilities), not type schemas, so there are
-- no type-defining modules from which to derive DSL wrappers. Empty, like
-- hydra-rdf's encoding list.
mainDslModules :: [Module]
mainDslModules = []

-- | No type-defining modules to encode/decode. Empty.
mainEncodingModules :: [Module]
mainEncodingModules = []

testModules :: [Module]
testModules = [
  TestBuildModules.module_,
  TestBuildReconcile.module_,
  TestBuildRouting.module_]
