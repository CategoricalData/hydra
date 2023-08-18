{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier0.Phantoms where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Sources.Tier0.Core
import Hydra.Sources.Tier0.Compute
import Hydra.Dsl.Types as Types


hydraPhantomsModule :: Module Kv
hydraPhantomsModule = Module ns elements [hydraComputeModule] $
    Just "Phantom types for use in model definitions"
  where
    ns = Namespace "hydra/phantoms"
    core = typeref $ moduleNamespace hydraCoreModule
    evaluation = typeref $ moduleNamespace hydraComputeModule
    phantoms = typeref ns
    def = datatype ns

    elements = [
      def "Case" $
        doc "An association of a field name (as in a case statement) with a phantom type" $
        lambda "a" $ core "FieldName",

      def "Datum" $
        doc "An association of a term with a phantom type" $
        lambda "a" $ (core "Term") @@ (evaluation "Kv"),

      def "Definition" $
        doc "An association with a named term with a phantom type" $
        lambda "a" $ record [
          "name">: core "Name",
          "datum">: phantoms "Datum" @@ "a"],

      def "Fld" $
        doc "An association with a term-level field with a phantom type" $
        lambda "a" $ (core "Field") @@ (evaluation "Kv"),

      def "Reference" $
        doc "A pure association with a phantom type" $
        lambda "a" $ unit]
