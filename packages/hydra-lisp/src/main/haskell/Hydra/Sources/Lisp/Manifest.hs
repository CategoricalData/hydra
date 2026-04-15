-- | Package manifest for hydra-lisp.
--
-- Owns the Lisp coder DSL sources, shared by all Lisp dialects (Clojure,
-- Scheme, Common Lisp, Emacs Lisp). See feature_290_packaging-plan.md,
-- "Sync system redesign / Package manifests".

module Hydra.Sources.Lisp.Manifest (
  mainModules,
  testModules,
) where

import Hydra.Kernel

import qualified Hydra.Sources.Lisp.Coder as LispCoder
import qualified Hydra.Sources.Lisp.Language as LispLanguage
import qualified Hydra.Sources.Lisp.Serde as LispSerde
import qualified Hydra.Sources.Lisp.Syntax as LispSyntax

mainModules :: [Module]
mainModules = [
  LispCoder.module_,
  LispLanguage.module_,
  LispSerde.module_,
  LispSyntax.module_]

testModules :: [Module]
testModules = []
