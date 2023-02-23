{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Module where

import Hydra.Kernel
import Hydra.Dsl.Types as Types
import Hydra.Dsl.Standard
import Hydra.Sources.Mantle


hydraModuleModule :: Module Kv
hydraModuleModule = Module ns elements [hydraMantleModule] $
    Just "A model for Hydra namespaces and modules (collections of elements in the same namespace)"
  where
    ns = Namespace "hydra/module"
    mantle = nsref $ moduleNamespace hydraMantleModule
    mod = nsref ns
    def = datatype ns

    elements = [

      def "FileExtension" string,
      
      def "Module" $
        doc "A logical collection of elements in the same namespace, having dependencies on zero or more other modules" $
        lambda "m" $ record [
          "namespace">:
            doc "A common prefix for all element names in the module" $
            mod "Namespace",
          "elements">:
            doc "The elements defined in this module" $
            list $ mantle "Element" @@ "m",
          "dependencies">:
            doc "Any additional modules this one has a direct dependency upon" $
            list $ mod "Module" @@ "m",
          "description">:
            doc "An optional human-readable description of the module" $
            optional string],

      def "Namespace" $
        doc "A prefix for element names"
        string]
