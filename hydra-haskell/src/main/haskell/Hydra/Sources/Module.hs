{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Module where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Sources.Graph
import Hydra.Dsl.Types as Types


hydraModuleModule :: Module Kv
hydraModuleModule = Module ns elements [hydraGraphModule] $
    Just "A model for Hydra namespaces and modules (collections of elements in the same namespace)"
  where
    ns = Namespace "hydra/module"
    graph = nsref $ moduleNamespace hydraGraphModule
    mod = nsref ns
    def = datatype ns

    elements = [

      def "FileExtension" string,
      
      def "Module" $
        doc "A logical collection of elements in the same namespace, having dependencies on zero or more other modules" $
        lambda "a" $ record [
          "namespace">:
            doc "A common prefix for all element names in the module" $
            mod "Namespace",
          "elements">:
            doc "The elements defined in this module" $
            list $ graph "Element" @@ "a",
          "dependencies">:
            doc "Any additional modules this one has a direct dependency upon" $
            list $ mod "Module" @@ "a",
          "description">:
            doc "An optional human-readable description of the module" $
            optional string],

      def "Namespace" $
        doc "A prefix for element names"
        string]
