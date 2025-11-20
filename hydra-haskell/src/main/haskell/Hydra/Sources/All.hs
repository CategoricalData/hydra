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

import Hydra.Sources.Haskell.Ast
import Hydra.Sources.Haskell.Coder
import Hydra.Sources.Haskell.Language
import Hydra.Sources.Haskell.Operators
import Hydra.Sources.Haskell.Serde
import Hydra.Sources.Haskell.Utils
import qualified Hydra.Sources.Json.Coder as JsonCoder
import qualified Hydra.Sources.Json.Decoding as JsonDecoding
import qualified Hydra.Sources.Json.Extract as JsonExtract
import qualified Hydra.Sources.Json.Language as JsonLanguage
import Hydra.Sources.Yaml.Model


mainModules :: [Module]
mainModules = kernelModules ++ jsonModules ++ otherModules

jsonModules :: [Module]
jsonModules = [
  JsonCoder.module_,
  JsonDecoding.module_,
  JsonExtract.module_,
  JsonLanguage.module_]

otherModules :: [Module]
otherModules = [
  haskellAstModule,
  haskellCoderModule,
  haskellLanguageModule,
  haskellOperatorsModule,
  haskellSerdeModule,
  haskellUtilsModule,
  yamlModelModule]

kernelModules :: [Module]
kernelModules = kernelTypesModules ++ kernelTermsModules
