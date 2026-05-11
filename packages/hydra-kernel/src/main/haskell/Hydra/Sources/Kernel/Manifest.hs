-- | Package manifest for hydra-kernel.
--
-- Each Hydra package exposes its module set through a Manifest module so that
-- the sync system and execs can consume a uniform interface instead of
-- hardcoding per-package module lists. See feature_290_packaging-plan.md,
-- "Sync system redesign / Package manifests".
--
-- The kernel additionally exposes two sublists of mainModules:
--
--   * jsonModules — the JSON runtime modules (hydra.json.*)
--   * otherModules — yaml support, test transform helpers, json/yaml bridges,
--     and synthetic bench workloads (hydra.bench.*)
--
-- These sublists exist because existing execs consume them separately; future
-- cleanup may fold them into mainModules-as-a-whole.

module Hydra.Sources.Kernel.Manifest (
  mainModules,
  testModules,
  jsonModules,
  otherModules,
) where

import Hydra.Kernel
import Hydra.Sources.Kernel.Types.All (kernelTypesModules)
import Hydra.Sources.Kernel.Terms.All (kernelTermsModules)

import qualified Hydra.Sources.Bench.LinearChain      as BenchLinearChain
import qualified Hydra.Sources.Bench.PolymorphicChain as BenchPolymorphicChain
import qualified Hydra.Sources.Bench.FanOut           as BenchFanOut
import qualified Hydra.Sources.Json.Bootstrap as JsonBootstrap
import qualified Hydra.Sources.Json.Decode as JsonDecode
import qualified Hydra.Sources.Json.Decoding as JsonDecoding
import qualified Hydra.Sources.Json.Encode as JsonEncode
import qualified Hydra.Sources.Json.Extract as JsonExtract
import qualified Hydra.Sources.Json.Parser as JsonParser
import qualified Hydra.Sources.Json.Writer as JsonWriter
import qualified Hydra.Sources.Json.Yaml.Decode as JsonYamlDecode
import qualified Hydra.Sources.Json.Yaml.Encode as JsonYamlEncode
import qualified Hydra.Sources.Test.Transform as TestTransform
import qualified Hydra.Sources.Test.Utils as TestUtils
import qualified Hydra.Sources.Yaml.Model as YamlModel

import qualified Hydra.Sources.Test.All as Test

jsonModules :: [Module]
jsonModules = [
  JsonBootstrap.module_,
  JsonDecode.module_,
  JsonDecoding.module_,
  JsonEncode.module_,
  JsonExtract.module_,
  JsonParser.module_,
  JsonWriter.module_]

-- | otherModules: yaml support, test transform helpers, json/yaml bridges,
-- and synthetic benchmark workloads.
--
-- The bench modules (hydra.bench.*) are synthetic workloads used by cross-host
-- inference performance comparison runners. They aren't used at runtime by the
-- kernel and aren't part of the test suite, but living in 'otherModules' lets
-- them flow through the standard sync pipeline so all hosts see the same JSON.
otherModules :: [Module]
otherModules = [
  TestTransform.module_,
  TestUtils.module_,
  YamlModel.module_,
  JsonYamlDecode.module_,
  JsonYamlEncode.module_,
  BenchLinearChain.module_,
  BenchPolymorphicChain.module_,
  BenchFanOut.module_]

mainModules :: [Module]
mainModules = kernelTypesModules ++ kernelTermsModules ++ jsonModules ++ otherModules

testModules :: [Module]
testModules = Test.testModules
