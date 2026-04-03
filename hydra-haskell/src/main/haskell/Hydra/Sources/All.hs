-- | Aggregates all Hydra source modules (kernel, test, and extension modules)

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

import qualified Hydra.Sources.CoderUtils as CoderUtils
import qualified Hydra.Sources.Haskell.Syntax as HaskellSyntax
import qualified Hydra.Sources.Haskell.Coder as HaskellCoder
import qualified Hydra.Sources.Haskell.Environment as HaskellEnvironment
import qualified Hydra.Sources.Haskell.Language as HaskellLanguage
import qualified Hydra.Sources.Haskell.Operators as HaskellOperators
import qualified Hydra.Sources.Haskell.Serde as HaskellSerde
import qualified Hydra.Sources.Haskell.Testing as HaskellTesting
import qualified Hydra.Sources.Haskell.Utils as HaskellUtils
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
import qualified Hydra.Sources.Kernel.Terms.Dsls as Dsls
import qualified Hydra.Sources.Yaml.Model as YamlModel


mainModules :: [Module]
mainModules = kernelModules ++ haskellModules ++ jsonModules ++ otherModules

-- | The DSL source module (hydra.dsls) must be generated separately from mainModules
-- because including it in the main generation causes a stack overflow due to the
-- complexity of its term definitions (which reference decoders, the full type graph, etc.)
dslSourceModules :: [Module]
dslSourceModules = [Dsls.module_]

kernelModules :: [Module]
kernelModules = kernelTypesModules ++ kernelTermsModules ++ jsonModules

haskellModules :: [Module]
haskellModules = [
  HaskellSyntax.module_,
  HaskellCoder.module_,
  HaskellEnvironment.module_,
  HaskellLanguage.module_,
  HaskellOperators.module_,
  HaskellSerde.module_,
  HaskellTesting.module_,
  HaskellUtils.module_]

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
  CoderUtils.module_,
  TestTransform.module_,
  TestUtils.module_,
  YamlModel.module_,
  JsonYamlDecode.module_,
  JsonYamlEncode.module_]
