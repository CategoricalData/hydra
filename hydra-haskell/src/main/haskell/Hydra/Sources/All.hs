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

import qualified Hydra.Sources.Haskell.Ast as HaskellAst
import qualified Hydra.Sources.Haskell.Coder as HaskellCoder
import qualified Hydra.Sources.Haskell.Language as HaskellLanguage
import qualified Hydra.Sources.Haskell.Operators as HaskellOperators
import qualified Hydra.Sources.Haskell.Serde as HaskellSerde
import qualified Hydra.Sources.Haskell.Utils as HaskellUtils
import qualified Hydra.Sources.Json.Coder as JsonCoder
import qualified Hydra.Sources.Json.Decoding as JsonDecoding
import qualified Hydra.Sources.Json.Extract as JsonExtract
import qualified Hydra.Sources.Json.Language as JsonLanguage
import qualified Hydra.Sources.Yaml.Model as YamlModel


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
  HaskellAst.module_,
  HaskellCoder.module_,
  HaskellLanguage.module_,
  HaskellOperators.module_,
  HaskellSerde.module_,
  HaskellUtils.module_,
  YamlModel.module_]

kernelModules :: [Module]
kernelModules = kernelTypesModules ++ kernelTermsModules
