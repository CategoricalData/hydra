{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Types.Phantoms where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.phantoms"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [Core.module_] [Core.module_] $
    Just "Phantom types for use with Hydra DSLs"
  where
    elements = [
      tBinding,
      tTerm]

tBinding :: Binding
tBinding = define "TBinding" $
  doc "An association of a named term (element) with a phantom type" $
  T.forAll "a" $ T.record [
    "name">:
      doc "The name of the term" $
      use Core.name,
    "term">:
      doc "The term with its phantom type" $
      use tTerm @@ T.var "a"]

tTerm :: Binding
tTerm = define "TTerm" $
  doc "An association of a term with a phantom type" $
  T.forAll "a" $ T.wrap $ use Core.term
