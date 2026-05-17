-- | Package manifest for hydra-go.
--
-- Owns the Go syntax and language DSL sources. Promotion of the Go coder
-- (Hydra.Go.Coder) and runtime serializer (Hydra.Go.Serde) to DSL sources is
-- pending; see issue #289. Until then, those modules live as plain Haskell
-- under heads/haskell/src/main/haskell/Hydra/Go/.

module Hydra.Sources.Go.Manifest (
  mainModules,
  testModules,
  dslTypeModules,
) where

import Hydra.Kernel

import qualified Hydra.Sources.Go.Language as GoLanguage
import qualified Hydra.Sources.Go.Syntax as GoSyntax

mainModules :: [Module]
mainModules = [
  GoLanguage.module_,
  GoSyntax.module_]

-- | Modules in this package whose type definitions should produce derived
-- DSL wrapper modules (Hydra/Dsl/Go/<Name>.hs). The DSL generator
-- consumes this list to emit constructors / accessors / withXxx updaters
-- for each TypeDefinition. Term-only modules (Language) are
-- deliberately excluded. Extend the list when a new type-defining
-- module needs DSL wrappers.
dslTypeModules :: [Module]
dslTypeModules = [
  GoSyntax.module_]

testModules :: [Module]
testModules = []
