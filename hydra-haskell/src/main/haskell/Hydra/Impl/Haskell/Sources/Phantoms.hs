{-# LANGUAGE OverloadedStrings #-}

module Hydra.Impl.Haskell.Sources.Phantoms where

import Hydra.Impl.Haskell.Sources.Core
import Hydra.Impl.Haskell.Sources.Compute

import Hydra.Core
import Hydra.Compute
import Hydra.Module
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


hydraPhantomsModule :: Module Meta
hydraPhantomsModule = Module ns elements [hydraComputeModule] $
    Just "Phantom types for use in model definitions"
  where
    ns = Namespace "hydra/phantoms"
    core = nsref $ moduleNamespace hydraCoreModule
    evaluation = nsref $ moduleNamespace hydraComputeModule
    phantoms = nsref ns
    def = datatype ns

    elements = [
      def "Case" $
        lambda "a" $ core "FieldName",

     def "Datum" $
       lambda "a" $ (core "Term") @@ (evaluation "Meta"),

      def "Definition" $
        lambda "a" $ record [
          "name">: core "Name",
          "datum">: phantoms "Datum" @@ "a"],

     def "Reference" $
       lambda "a" $ unit]
