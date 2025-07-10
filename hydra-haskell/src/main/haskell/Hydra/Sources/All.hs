module Hydra.Sources.All(
  module Hydra.Sources.All,
  module Hydra.Sources.Kernel.Terms.All,
) where

import Hydra.Kernel
import Hydra.Sources.Kernel.Terms.All
import Hydra.Sources.Haskell.Ast
import Hydra.Sources.Haskell.Ast
import Hydra.Sources.Haskell.Coder
import Hydra.Sources.Haskell.Language
import Hydra.Sources.Haskell.Operators
import Hydra.Sources.Haskell.Serde
import Hydra.Sources.Haskell.Utils
import Hydra.Sources.Json.Decoding
import Hydra.Sources.Json.Schema
import Hydra.Sources.Yaml.Model


mainModules :: [Module]
mainModules = kernelModules ++ tier3ExtModules

tier3ExtModules :: [Module]
tier3ExtModules = [
  haskellAstModule,
  haskellCoderModule,
  haskellLanguageModule,
  haskellOperatorsModule,
  haskellSerdeModule,
  haskellUtilsModule,
  jsonDecodingModule,
  jsonSchemaModule,
  yamlModelModule]
