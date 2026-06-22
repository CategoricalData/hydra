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
import Hydra.Sources.Kernel.Terms.All (kernelTermsModules, kernelLibModules)

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
-- Every type-defining module in the package (#474), plus the primitive-defining
-- hydra.lib.* modules (#467): each primitive projects to a hydra.dsl.lib.<x>
-- reference wrapper via the term/primitive DSL ref path. The hydra.dsl.lib.*
-- namespace is collision-free (no hand-written Hydra.Dsl.Lib.* modules exist;
-- only Hydra.Dsl.Meta.Lib.*), so primitives can widen ahead of the term-module
-- widening, which still awaits the Hydra.Dsl.{Annotations,Literals} renames.
mainDslModules :: [Module]
mainDslModules = kernelTypesModules ++ jsonModules ++ otherModules ++ kernelLibModules

-- | Source modules from which term encoder + decoder modules
-- (hydra.encode.<x> / hydra.decode.<x>) are derived. Broad: every
-- type-defining module in the package. (#475 unblock — the previous
-- narrowing excluded hydra.validation, hydra.error.packaging,
-- hydra.yaml.model; restored here as part of the #475 fix.)
--
-- Decoupled from mainDslModules (#467): the primitive-defining hydra.lib.*
-- modules belong in the DSL set but NOT here — encode/decode synthesis applies
-- to type-defining modules, and primitives carry no type definitions to encode.
mainEncodingModules :: [Module]
mainEncodingModules = kernelTypesModules ++ jsonModules ++ otherModules

testModules :: [Module]
testModules = Test.testModules
