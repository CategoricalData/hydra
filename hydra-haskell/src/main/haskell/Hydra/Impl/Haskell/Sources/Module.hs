{-# LANGUAGE OverloadedStrings #-}

module Hydra.Impl.Haskell.Sources.Module where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Compute
import Hydra.Module
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


hydraModuleModule :: Module Meta
hydraModuleModule = Module ns elements [hydraCoreModule] $
    Just "A model for Hydra namespaces and modules (collections of elements in the same namespace)"
  where
    ns = Namespace "hydra/module"
    core = nsref $ moduleNamespace hydraCoreModule
    graph = nsref ns
    def = datatype ns

    elements = [

      def "Module" $
        doc "A logical collection of elements in the same namespace, having dependencies on zero or more other modules" $
        lambda "m" $ record [
          "namespace">:
            doc "A common prefix for all element names in the module" $
            graph "Namespace",
          "elements">:
            doc "The elements defined in this module" $
            list $ core "Element" @@ "m",
          "dependencies">:
            doc "Any additional modules this one has a direct dependency upon" $
            list $ graph "Module" @@ "m",
          "description">:
            doc "An optional human-readable description of the module" $
            optional string],

      def "Namespace" $
        doc "A prefix for element names"
        string]
