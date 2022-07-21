{-# LANGUAGE OverloadedStrings #-}

module Hydra.Impl.Haskell.Sources.Errors where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


hydraErrorsModule :: Module Meta
hydraErrorsModule = Module hydraErrors []

-- Note: here, the element namespace doubles as a graph name
hydraErrorsName :: GraphName
hydraErrorsName = GraphName "hydra/errors"

hydraErrors :: Graph Meta
hydraErrors = Graph hydraErrorsName elements (const True) hydraCoreName
  where
    def = datatype hydraErrorsName
    elements = [

      def "Qualified" $
        doc "An optional value qualified with a (possibly empty) list of warnings" $
        universal "m" $ record [
          field "value" $ optional "m",
          field "warnings" $ list string]]
