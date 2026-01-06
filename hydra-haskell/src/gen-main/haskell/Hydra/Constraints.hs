-- Note: this is an automatically generated file. Do not edit.

-- | A model for path- and pattern-based graph constraints, which may be considered as part of the schema of a graph

module Hydra.Constraints where

import qualified Hydra.Core as Core
import qualified Hydra.Query as Query
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | A declared equivalence between two abstract paths in a graph
data PathEquation = 
  PathEquation {
    -- | The left-hand side of the equation
    pathEquationLeft :: Query.Path,
    -- | The right-hand side of the equation
    pathEquationRight :: Query.Path}
  deriving (Eq, Ord, Read, Show)

_PathEquation = (Core.Name "hydra.constraints.PathEquation")

_PathEquation_left = (Core.Name "left")

_PathEquation_right = (Core.Name "right")

-- | A pattern which, if it matches in a given graph, implies that another pattern must also match. Query variables are shared between the two patterns.
data PatternImplication = 
  PatternImplication {
    -- | The pattern which, if it matches, triggers the constraint
    patternImplicationAntecedent :: Query.Pattern,
    -- | The pattern which must also match when the antecedent matches
    patternImplicationConsequent :: Query.Pattern}
  deriving (Eq, Ord, Read, Show)

_PatternImplication = (Core.Name "hydra.constraints.PatternImplication")

_PatternImplication_antecedent = (Core.Name "antecedent")

_PatternImplication_consequent = (Core.Name "consequent")
