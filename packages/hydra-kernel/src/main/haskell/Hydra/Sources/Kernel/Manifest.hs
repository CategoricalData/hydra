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
--   * otherModules — yaml support, test transform helpers, json/yaml bridges
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

otherModules :: [Module]
otherModules = [
  TestTransform.module_,
  TestUtils.module_,
  YamlModel.module_,
  JsonYamlDecode.module_,
  JsonYamlEncode.module_]

mainModules :: [Module]
mainModules = kernelTypesModules ++ kernelTermsModules ++ jsonModules ++ otherModules

testModules :: [Module]
testModules = Test.testModules
