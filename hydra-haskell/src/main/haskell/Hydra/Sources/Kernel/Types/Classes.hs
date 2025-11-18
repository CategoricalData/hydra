{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Types.Classes where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import qualified Hydra.Dsl.Terms                 as Terms
import           Hydra.Dsl.Types                 as Types
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y


module_ :: Module
module_ = Module ns elements [Core.module_] [Core.module_] $
    Just ("Type classes")
  where
    ns = Namespace "hydra.classes"
    core = typeref $ moduleNamespace Core.module_
    meta = typeref ns
    def = datatype ns

    elements = [

      def "TypeClass" $
        doc "Any of a small number of built-in type classes" $
        enum [
          "equality",
          "ordering"]]
