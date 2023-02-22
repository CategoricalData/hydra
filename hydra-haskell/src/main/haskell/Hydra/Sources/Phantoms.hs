{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Phantoms where

import Hydra.Kernel
import Hydra.Dsl.Types as Types
import Hydra.Dsl.Standard
import Hydra.Sources.Core
import Hydra.Sources.Compute


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
        doc "An association of a field name (as in a case statement) with a phantom type" $
        lambda "a" $ core "FieldName",

      def "Datum" $
        doc "An association of a term with a phantom type" $
        lambda "a" $ (core "Term") @@ (evaluation "Meta"),

      def "Definition" $
        doc "An association with a named term with a phantom type" $
        lambda "a" $ record [
          "name">: core "Name",
          "datum">: phantoms "Datum" @@ "a"],

      def "Fld" $
        doc "An association with a term-level field with a phantom type" $
        lambda "a" $ (core "Field") @@ (evaluation "Meta"),

      def "Reference" $
        doc "A pure association with a phantom type" $
        lambda "a" $ unit]
