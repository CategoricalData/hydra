{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Types.Constraints where

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

import qualified Hydra.Sources.Kernel.Types.Query as Query


module_ :: Module
module_ = Module ns elements [Query.module_] [Core.module_] $
    Just "A model for path- and pattern-based graph constraints, which may be considered as part of the schema of a graph"
  where
    ns = Namespace "hydra.constraints"
    core = typeref $ moduleNamespace Core.module_
    query = typeref $ moduleNamespace Query.module_
    constraints = typeref ns
    def = datatype ns

    elements = [

      def "PathEquation" $
        doc "A declared equivalence between two abstract paths in a graph" $
        record [
          "left">:
            doc "The left-hand side of the equation" $
            query "Path",
          "right">:
            doc "The right-hand side of the equation" $
            query "Path"],

      def "PatternImplication" $
        doc "A pattern which, if it matches in a given graph, implies that another pattern must also match. Query variables are shared between the two patterns." $
        record [
          "antecedent">:
            doc "The pattern which, if it matches, triggers the constraint" $
            query "Pattern",
          "consequent">:
            doc "The pattern which must also match when the antecedent matches" $
            query "Pattern"]]
