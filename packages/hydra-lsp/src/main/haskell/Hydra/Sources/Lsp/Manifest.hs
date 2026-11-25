-- | Package manifest for hydra-lsp: DSL sources for the Hydra language server (#836).

module Hydra.Sources.Lsp.Manifest (
  mainModules,
  testModules,
  mainDslModules,
  mainEncodingModules,
) where

import Hydra.Kernel

import qualified Hydra.Sources.Lsp.Analysis as LspAnalysis
import qualified Hydra.Sources.Lsp.Protocol as LspProtocol
import qualified Hydra.Sources.Lsp.Wire     as LspWire

mainModules :: [Module]
mainModules = [
  LspAnalysis.module_,
  LspProtocol.module_,
  LspWire.module_]

-- Source modules from which dsl/encode/decode are derived (#474): every
-- type-defining module in the package, no per-module curation.
mainDslModules :: [Module]
mainDslModules = filter moduleDefinesType mainModules

-- | Empty for now: encode/decode for this package's modules is not yet supported across eta-expanding targets (see #475). Re-add modules here once #475 is fixed.
mainEncodingModules :: [Module]
mainEncodingModules = []

-- | True if a module defines at least one type.
moduleDefinesType :: Module -> Bool
moduleDefinesType m = any isTypeDef (moduleDefinitions m)
  where
    isTypeDef (DefinitionType _) = True
    isTypeDef _                  = False

testModules :: [Module]
testModules = []
