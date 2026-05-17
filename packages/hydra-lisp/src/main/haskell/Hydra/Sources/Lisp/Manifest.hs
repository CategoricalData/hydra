-- | Package manifest for hydra-lisp.
--
-- Owns the Lisp coder DSL sources, shared by all Lisp dialects (Clojure,
-- Scheme, Common Lisp, Emacs Lisp). See feature_290_packaging-plan.md,
-- "Sync system redesign / Package manifests".

module Hydra.Sources.Lisp.Manifest (
  mainModules,
  testModules,
  dslTypeModules,
) where

import Hydra.Kernel

import qualified Hydra.Sources.Lisp.Coder as LispCoder
import qualified Hydra.Sources.Lisp.Language as LispLanguageSource
import qualified Hydra.Sources.Lisp.Serde as LispSerde
import qualified Hydra.Sources.Lisp.Syntax as LispSyntax

mainModules :: [Module]
mainModules = [
  LispCoder.module_,
  LispLanguageSource.module_,
  LispSerde.module_,
  LispSyntax.module_]

-- | Modules in this package whose type definitions should produce derived
-- DSL wrapper modules (Hydra/Dsl/Lisp/<Name>.hs). The DSL generator
-- consumes this list to emit constructors / accessors / withXxx updaters
-- for each TypeDefinition. Term-only modules (Coder, Language, Serde)
-- are deliberately excluded. Extend the list when a new type-defining
-- module needs DSL wrappers.
dslTypeModules :: [Module]
dslTypeModules = [
  LispSyntax.module_]

testModules :: [Module]
testModules = []
