module Hydra.Sources.Ext.All(
  module Hydra.Sources.Ext.All,
  module Hydra.Sources.Kernel.Terms.All,
) where

import Hydra.Kernel
import Hydra.Sources.Kernel.Terms.All
import Hydra.Sources.Ext.Haskell.Ast
import Hydra.Sources.Ext.Haskell.Ast
import Hydra.Sources.Ext.Haskell.Coder
import Hydra.Sources.Ext.Haskell.Language
import Hydra.Sources.Ext.Haskell.Operators
import Hydra.Sources.Ext.Haskell.Serde
import Hydra.Sources.Ext.Haskell.Utils
import Hydra.Sources.Ext.Json.Decoding
import Hydra.Sources.Ext.Json.Schema
import Hydra.Sources.Ext.Yaml.Model


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
