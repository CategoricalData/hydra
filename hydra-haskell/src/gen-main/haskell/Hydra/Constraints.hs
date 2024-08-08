-- | A model for path- and pattern-based graph constraints, which may be considered as part of the schema of a graph

module Hydra.Constraints where

import qualified Hydra.Core as Core
import qualified Hydra.Query as Query
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | A declared equivalence between two abstract paths in a graph
data PathEquation = 
  PathEquation {
    pathEquationLeft :: Query.Path,
    pathEquationRight :: Query.Path}
  deriving (Eq, Ord, Read, Show)

_PathEquation = (Core.Name "hydra/constraints.PathEquation")

_PathEquation_left = (Core.Name "left")

_PathEquation_right = (Core.Name "right")

_PathEquation_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/constraints.PathEquation"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "left"),
      Core.fieldTypeType = Query._Path_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "right"),
      Core.fieldTypeType = Query._Path_type_}]}))

-- | A pattern which, if it matches in a given graph, implies that another pattern must also match. Query variables are shared between the two patterns.
data PatternImplication = 
  PatternImplication {
    patternImplicationAntecedent :: Query.Pattern,
    patternImplicationConsequent :: Query.Pattern}
  deriving (Eq, Ord, Read, Show)

_PatternImplication = (Core.Name "hydra/constraints.PatternImplication")

_PatternImplication_antecedent = (Core.Name "antecedent")

_PatternImplication_consequent = (Core.Name "consequent")

_PatternImplication_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/constraints.PatternImplication"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "antecedent"),
      Core.fieldTypeType = Query._Pattern_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "consequent"),
      Core.fieldTypeType = Query._Pattern_type_}]}))