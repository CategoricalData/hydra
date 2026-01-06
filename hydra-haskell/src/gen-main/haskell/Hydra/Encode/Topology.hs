-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.topology

module Hydra.Encode.Topology where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Topology as Topology
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

graph :: (M.Map Int [Int] -> Core.Term)
graph m = (Core.TermMap (Maps.bimap vertex (\xs -> Core.TermList (Lists.map vertex xs)) m))

tarjanState :: (Topology.TarjanState -> Core.Term)
tarjanState x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.topology.TarjanState"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "counter"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x))) (Topology.tarjanStateCounter x))},
    Core.Field {
      Core.fieldName = (Core.Name "indices"),
      Core.fieldTerm = ((\m -> Core.TermMap (Maps.bimap vertex (\x -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x))) m)) (Topology.tarjanStateIndices x))},
    Core.Field {
      Core.fieldName = (Core.Name "lowLinks"),
      Core.fieldTerm = ((\m -> Core.TermMap (Maps.bimap vertex (\x -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x))) m)) (Topology.tarjanStateLowLinks x))},
    Core.Field {
      Core.fieldName = (Core.Name "stack"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map vertex xs)) (Topology.tarjanStateStack x))},
    Core.Field {
      Core.fieldName = (Core.Name "onStack"),
      Core.fieldTerm = ((\s -> Core.TermSet (Sets.map vertex s)) (Topology.tarjanStateOnStack x))},
    Core.Field {
      Core.fieldName = (Core.Name "sccs"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map (\xs -> Core.TermList (Lists.map vertex xs)) xs)) (Topology.tarjanStateSccs x))}]}))

vertex :: (Int -> Core.Term)
vertex x = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x)))
