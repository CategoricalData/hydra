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
import Hydra.Sources.Json.Decoding
import Hydra.Sources.Json.Schema
import Hydra.Sources.Test.TestGraph
import Hydra.Sources.Test.TestSuite
import Hydra.Sources.Yaml.Model


mainModules :: [Module]
mainModules = kernelModules ++ otherModules

otherModules :: [Module]
otherModules = [
  haskellAstModule,
  haskellCoderModule,
  haskellLanguageModule,
  haskellOperatorsModule,
  haskellSerdeModule,
  haskellUtilsModule,
  jsonDecodingModule,
  jsonSchemaModule,
  yamlModelModule]

testModules :: [Module]
testModules = [
  testGraphModule,
  testSuiteModule]

kernelModules :: [Module]
kernelModules = kernelTypesModules ++ kernelTermsModules
