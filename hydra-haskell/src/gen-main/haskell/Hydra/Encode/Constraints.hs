-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.constraints

module Hydra.Encode.Constraints where

import qualified Hydra.Constraints as Constraints
import qualified Hydra.Core as Core
import qualified Hydra.Encode.Query as Query
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

pathEquation :: (Constraints.PathEquation -> Core.Term)
pathEquation x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.constraints.PathEquation"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "left"),
      Core.fieldTerm = (Query.path (Constraints.pathEquationLeft x))},
    Core.Field {
      Core.fieldName = (Core.Name "right"),
      Core.fieldTerm = (Query.path (Constraints.pathEquationRight x))}]}))

patternImplication :: (Constraints.PatternImplication -> Core.Term)
patternImplication x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.constraints.PatternImplication"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "antecedent"),
      Core.fieldTerm = (Query.pattern (Constraints.patternImplicationAntecedent x))},
    Core.Field {
      Core.fieldName = (Core.Name "consequent"),
      Core.fieldTerm = (Query.pattern (Constraints.patternImplicationConsequent x))}]}))
