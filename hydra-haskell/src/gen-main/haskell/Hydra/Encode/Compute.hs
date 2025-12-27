-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.compute

module Hydra.Encode.Compute where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as Core_
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

flowState :: ((t0 -> Core.Term) -> (t1 -> Core.Term) -> Compute.FlowState t0 t1 -> Core.Term)
flowState s v x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.compute.FlowState"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = ((\opt -> Core.TermMaybe (Maybes.map v opt)) (Compute.flowStateValue x))},
    Core.Field {
      Core.fieldName = (Core.Name "state"),
      Core.fieldTerm = (s (Compute.flowStateState x))},
    Core.Field {
      Core.fieldName = (Core.Name "trace"),
      Core.fieldTerm = (trace (Compute.flowStateTrace x))}]}))

trace :: (Compute.Trace -> Core.Term)
trace x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "stack"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map (\x -> Core.TermLiteral (Core.LiteralString x)) xs)) (Compute.traceStack x))},
    Core.Field {
      Core.fieldName = (Core.Name "messages"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map (\x -> Core.TermLiteral (Core.LiteralString x)) xs)) (Compute.traceMessages x))},
    Core.Field {
      Core.fieldName = (Core.Name "other"),
      Core.fieldTerm = ((\m -> Core.TermMap (Maps.bimap Core_.name Core_.term m)) (Compute.traceOther x))}]}))
