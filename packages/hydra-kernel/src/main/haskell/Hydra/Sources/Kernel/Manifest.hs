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
  mainDslModules,
  mainEncodingModules,
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

-- | otherModules: yaml support, test transform helpers, json/yaml bridges.
otherModules :: [Module]
otherModules = [
  TestTransform.module_,
  TestUtils.module_,
  YamlModel.module_,
  JsonYamlDecode.module_,
  JsonYamlEncode.module_]

mainModules :: [Module]
mainModules = kernelTypesModules ++ kernelTermsModules ++ jsonModules ++ otherModules

-- | Source modules from which DSL wrapper modules (hydra.dsl.<x>) are derived.
-- Broad: every type-defining module in the package. (#474)
mainDslModules :: [Module]
mainDslModules = kernelTypesModules ++ jsonModules ++ otherModules

-- | Source modules from which term encoder + decoder modules
-- (hydra.encode.<x> / hydra.decode.<x>) are derived. Narrower than
-- 'mainDslModules': it excludes modules whose synthesized encoders/decoders
-- the eta-expanding targets (Java/Python) cannot yet compile — currently
-- hydra.validation, hydra.error.packaging, and hydra.yaml.model, whose types
-- are polymorphic / recursive / Set-typed (see #475). Re-add them here once
-- #475 is fixed; the DSL side is unaffected and stays broad.
mainEncodingModules :: [Module]
mainEncodingModules = filter (not . isEncodingUnsupported) mainDslModules
  where
    isEncodingUnsupported m = unModuleName (moduleName m) `elem`
      ["hydra.validation", "hydra.error.packaging", "hydra.yaml.model"]

testModules :: [Module]
testModules = Test.testModules
