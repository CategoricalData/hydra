{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Types.Phantoms where

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
    Just "Phantom types for use with Hydra DSLs"
  where
    ns = Namespace "hydra.phantoms"
    core = typeref $ moduleNamespace Core.module_
    phantoms = typeref ns
    def = datatype ns

    elements = [

      def "TElement" $
        doc "An association of a named term (element) with a phantom type" $
        forAll "a" $ record [
          "name">: core "Name",
          "term">: phantoms "TTerm" @@ "a"],

      def "TTerm" $
        doc "An association of a term with a phantom type" $
        forAll "a" $ wrap $ core "Term"]
