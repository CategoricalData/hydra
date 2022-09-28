{-# LANGUAGE OverloadedStrings #-}

module Hydra.Impl.Haskell.Sources.Graph where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


hydraGraphModule :: Module Meta
hydraGraphModule = Module ns elements [hydraCoreModule]
  where
    ns = Namespace "hydra/graph"
    core = nsref $ moduleNamespace hydraCoreModule
    graph = nsref ns
    def = datatype ns

    elements = [

      def "Module" $
        doc "A logical collection of elements; a graph subset with dependencies on zero or more other subsets" $
        lambda "m" $ record [
          "namespace">:
            doc "A common prefix for all element names in the module" $
            graph "Namespace",
          "elements">:
            doc "The elements defined in this module" $
            list $ core "Element" @@ "m",
          "dependencies">:
            doc "Any additional modules this one has a direct dependency upon" $
            list $ graph "Module" @@ "m"],

      def "Namespace" $
        doc "A prefix for element names"
        string]
