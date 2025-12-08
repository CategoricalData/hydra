{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Types.Classes where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.classes"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [Core.module_] [Core.module_] $
    Just "Type classes"
  where
    elements = [
      typeClass]

typeClass :: Binding
typeClass = define "TypeClass" $
  doc "Any of a small number of built-in type classes" $
  T.enum [
    "equality",
    "ordering"]
