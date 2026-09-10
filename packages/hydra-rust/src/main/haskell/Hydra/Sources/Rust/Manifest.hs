-- | Package manifest for hydra-rust: DSL sources for the Rust coder.

module Hydra.Sources.Rust.Manifest (
  mainModules,
  testModules,
  mainDslModules,
  mainEncodingModules,
) where

import Hydra.Kernel

import qualified Hydra.Sources.Rust.Coder     as RustCoder
import qualified Hydra.Sources.Rust.Language  as RustLanguageSource
import qualified Hydra.Sources.Rust.Operators as RustOperators
import qualified Hydra.Sources.Rust.Serde     as RustSerdeSource
import qualified Hydra.Sources.Rust.Syntax    as RustSyntax

mainModules :: [Module]
mainModules = [
  RustCoder.module_,
  RustLanguageSource.module_,
  RustOperators.module_,
  RustSerdeSource.module_,
  RustSyntax.module_]

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
