{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Constraints where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Sources.Core
import Hydra.Sources.Query
import Hydra.Dsl.Types as Types


hydraConstraintsModule :: Module Kv
hydraConstraintsModule = Module ns elements [hydraCoreModule, hydraQueryModule] $
    Just "A model for path- and pattern-based graph constraints, which may be considered as part of the schema of a graph"
  where
    ns = Namespace "hydra/constraints"
    core = typeref $ moduleNamespace hydraCoreModule
    query = typeref $ moduleNamespace hydraQueryModule
    constraints = typeref ns
    def = datatype ns

    elements = [
      
      def "PathEquation" $
        doc "A declared equivalence between two abstract paths in a graph" $
        record [
          "left">: query "Path",
          "right">: query "Path"],

      def "PatternImplication" $
        doc "A pattern which, if it matches in a given graph, implies that another pattern must also match. Query variables are shared between the two patterns." $
        lambda "a" $ record [
          "antecedent">: query "Pattern" @@ "a",
          "consequent">: query "Pattern" @@ "a"]]
