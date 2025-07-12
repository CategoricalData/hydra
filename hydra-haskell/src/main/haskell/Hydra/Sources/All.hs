module Hydra.Sources.All(
  module Hydra.Sources.All,
  module Hydra.Sources.Kernel.Terms.All,
  module Hydra.Sources.Kernel.Types.All,
) where

import Hydra.Kernel
import Hydra.Sources.Kernel.Terms.All
import Hydra.Sources.Kernel.Types.All

import Hydra.Sources.Haskell.Ast
import Hydra.Sources.Haskell.Coder
import Hydra.Sources.Haskell.Language
import Hydra.Sources.Haskell.Operators
import Hydra.Sources.Haskell.Serde
import Hydra.Sources.Haskell.Utils
import qualified Hydra.Sources.Json.Decoding as JsonDecoding
import qualified Hydra.Sources.Json.Extract as JsonExtract
import qualified Hydra.Sources.Json.Language as JsonLanguage
import qualified Hydra.Sources.Json.Schema as JsonSchema
import qualified Hydra.Sources.Json.Schema.Language as JsonSchemaLanguage
import Hydra.Sources.Test.TestGraph
import Hydra.Sources.Test.TestSuite
import Hydra.Sources.Yaml.Model


mainModules :: [Module]
mainModules = kernelModules ++ jsonModules ++ otherModules

jsonModules :: [Module]
jsonModules = [
  JsonDecoding.module_,
  JsonExtract.module_,
  JsonLanguage.module_,
  JsonSchema.module_,
  JsonSchemaLanguage.module_]

otherModules :: [Module]
otherModules = [
  haskellAstModule,
  haskellCoderModule,
  haskellLanguageModule,
  haskellOperatorsModule,
  haskellSerdeModule,
  haskellUtilsModule,
  yamlModelModule]

testModules :: [Module]
testModules = [
  testGraphModule,
  testSuiteModule]

kernelModules :: [Module]
kernelModules = kernelTypesModules ++ kernelTermsModules
