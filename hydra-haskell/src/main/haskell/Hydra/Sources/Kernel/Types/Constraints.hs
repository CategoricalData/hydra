{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Types.Constraints where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Query as Query


ns :: Namespace
ns = Namespace "hydra.constraints"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [Query.module_] [Core.module_] $
    Just "A model for path- and pattern-based graph constraints, which may be considered as part of the schema of a graph"
  where
    elements = [
      pathEquation,
      patternImplication]

pathEquation :: Binding
pathEquation = define "PathEquation" $
  doc "A declared equivalence between two abstract paths in a graph" $
  T.record [
    "left">:
      doc "The left-hand side of the equation" $
      use Query.path,
    "right">:
      doc "The right-hand side of the equation" $
      use Query.path]

patternImplication :: Binding
patternImplication = define "PatternImplication" $
  doc "A pattern which, if it matches in a given graph, implies that another pattern must also match. Query variables are shared between the two patterns." $
  T.record [
    "antecedent">:
      doc "The pattern which, if it matches, triggers the constraint" $
      use Query.pattern,
    "consequent">:
      doc "The pattern which must also match when the antecedent matches" $
      use Query.pattern]
