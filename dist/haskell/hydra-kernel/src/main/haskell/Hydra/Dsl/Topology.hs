-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.topology

module Hydra.Dsl.Topology where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S
-- | DSL constructor for hydra.topology.OrderingIsomorphism
orderingIsomorphism :: Typed.TypedTerm ([a] -> [a]) -> Typed.TypedTerm ([a] -> [a]) -> Typed.TypedTerm (Topology.OrderingIsomorphism a)
orderingIsomorphism encode decode =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.topology.OrderingIsomorphism"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Typed.unTypedTerm encode)},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Typed.unTypedTerm decode)}]}))
-- | DSL accessor for the decode field of hydra.topology.OrderingIsomorphism
orderingIsomorphismDecode :: Typed.TypedTerm (Topology.OrderingIsomorphism a) -> Typed.TypedTerm ([a] -> [a])
orderingIsomorphismDecode x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.topology.OrderingIsomorphism"),
        Core.projectionFieldName = (Core.Name "decode")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the encode field of hydra.topology.OrderingIsomorphism
orderingIsomorphismEncode :: Typed.TypedTerm (Topology.OrderingIsomorphism a) -> Typed.TypedTerm ([a] -> [a])
orderingIsomorphismEncode x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.topology.OrderingIsomorphism"),
        Core.projectionFieldName = (Core.Name "encode")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the decode field of hydra.topology.OrderingIsomorphism
orderingIsomorphismWithDecode :: Typed.TypedTerm (Topology.OrderingIsomorphism a) -> Typed.TypedTerm ([a] -> [a]) -> Typed.TypedTerm (Topology.OrderingIsomorphism a)
orderingIsomorphismWithDecode original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.topology.OrderingIsomorphism"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.OrderingIsomorphism"),
              Core.projectionFieldName = (Core.Name "encode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the encode field of hydra.topology.OrderingIsomorphism
orderingIsomorphismWithEncode :: Typed.TypedTerm (Topology.OrderingIsomorphism a) -> Typed.TypedTerm ([a] -> [a]) -> Typed.TypedTerm (Topology.OrderingIsomorphism a)
orderingIsomorphismWithEncode original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.topology.OrderingIsomorphism"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.OrderingIsomorphism"),
              Core.projectionFieldName = (Core.Name "decode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.topology.TarjanState
tarjanState :: Typed.TypedTerm Int -> Typed.TypedTerm (M.Map Topology.Vertex Int) -> Typed.TypedTerm (M.Map Topology.Vertex Int) -> Typed.TypedTerm [Topology.Vertex] -> Typed.TypedTerm (S.Set Topology.Vertex) -> Typed.TypedTerm [[Topology.Vertex]] -> Typed.TypedTerm Topology.TarjanState
tarjanState counter indices lowLinks stack onStack sccs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.topology.TarjanState"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "counter"),
          Core.fieldTerm = (Typed.unTypedTerm counter)},
        Core.Field {
          Core.fieldName = (Core.Name "indices"),
          Core.fieldTerm = (Typed.unTypedTerm indices)},
        Core.Field {
          Core.fieldName = (Core.Name "lowLinks"),
          Core.fieldTerm = (Typed.unTypedTerm lowLinks)},
        Core.Field {
          Core.fieldName = (Core.Name "stack"),
          Core.fieldTerm = (Typed.unTypedTerm stack)},
        Core.Field {
          Core.fieldName = (Core.Name "onStack"),
          Core.fieldTerm = (Typed.unTypedTerm onStack)},
        Core.Field {
          Core.fieldName = (Core.Name "sccs"),
          Core.fieldTerm = (Typed.unTypedTerm sccs)}]}))
-- | DSL accessor for the counter field of hydra.topology.TarjanState
tarjanStateCounter :: Typed.TypedTerm Topology.TarjanState -> Typed.TypedTerm Int
tarjanStateCounter x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
        Core.projectionFieldName = (Core.Name "counter")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the indices field of hydra.topology.TarjanState
tarjanStateIndices :: Typed.TypedTerm Topology.TarjanState -> Typed.TypedTerm (M.Map Topology.Vertex Int)
tarjanStateIndices x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
        Core.projectionFieldName = (Core.Name "indices")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the lowLinks field of hydra.topology.TarjanState
tarjanStateLowLinks :: Typed.TypedTerm Topology.TarjanState -> Typed.TypedTerm (M.Map Topology.Vertex Int)
tarjanStateLowLinks x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
        Core.projectionFieldName = (Core.Name "lowLinks")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the onStack field of hydra.topology.TarjanState
tarjanStateOnStack :: Typed.TypedTerm Topology.TarjanState -> Typed.TypedTerm (S.Set Topology.Vertex)
tarjanStateOnStack x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
        Core.projectionFieldName = (Core.Name "onStack")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the sccs field of hydra.topology.TarjanState
tarjanStateSccs :: Typed.TypedTerm Topology.TarjanState -> Typed.TypedTerm [[Topology.Vertex]]
tarjanStateSccs x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
        Core.projectionFieldName = (Core.Name "sccs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the stack field of hydra.topology.TarjanState
tarjanStateStack :: Typed.TypedTerm Topology.TarjanState -> Typed.TypedTerm [Topology.Vertex]
tarjanStateStack x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
        Core.projectionFieldName = (Core.Name "stack")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the counter field of hydra.topology.TarjanState
tarjanStateWithCounter :: Typed.TypedTerm Topology.TarjanState -> Typed.TypedTerm Int -> Typed.TypedTerm Topology.TarjanState
tarjanStateWithCounter original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.topology.TarjanState"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "counter"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "indices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "indices")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lowLinks"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "lowLinks")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stack"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "stack")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "onStack"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "onStack")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sccs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "sccs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the indices field of hydra.topology.TarjanState
tarjanStateWithIndices :: Typed.TypedTerm Topology.TarjanState -> Typed.TypedTerm (M.Map Topology.Vertex Int) -> Typed.TypedTerm Topology.TarjanState
tarjanStateWithIndices original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.topology.TarjanState"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "counter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "counter")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indices"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "lowLinks"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "lowLinks")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stack"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "stack")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "onStack"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "onStack")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sccs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "sccs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the lowLinks field of hydra.topology.TarjanState
tarjanStateWithLowLinks :: Typed.TypedTerm Topology.TarjanState -> Typed.TypedTerm (M.Map Topology.Vertex Int) -> Typed.TypedTerm Topology.TarjanState
tarjanStateWithLowLinks original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.topology.TarjanState"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "counter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "counter")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "indices")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lowLinks"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "stack"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "stack")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "onStack"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "onStack")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sccs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "sccs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the onStack field of hydra.topology.TarjanState
tarjanStateWithOnStack :: Typed.TypedTerm Topology.TarjanState -> Typed.TypedTerm (S.Set Topology.Vertex) -> Typed.TypedTerm Topology.TarjanState
tarjanStateWithOnStack original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.topology.TarjanState"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "counter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "counter")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "indices")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lowLinks"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "lowLinks")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stack"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "stack")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "onStack"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sccs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "sccs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the sccs field of hydra.topology.TarjanState
tarjanStateWithSccs :: Typed.TypedTerm Topology.TarjanState -> Typed.TypedTerm [[Topology.Vertex]] -> Typed.TypedTerm Topology.TarjanState
tarjanStateWithSccs original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.topology.TarjanState"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "counter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "counter")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "indices")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lowLinks"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "lowLinks")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stack"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "stack")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "onStack"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "onStack")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sccs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the stack field of hydra.topology.TarjanState
tarjanStateWithStack :: Typed.TypedTerm Topology.TarjanState -> Typed.TypedTerm [Topology.Vertex] -> Typed.TypedTerm Topology.TarjanState
tarjanStateWithStack original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.topology.TarjanState"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "counter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "counter")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "indices")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lowLinks"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "lowLinks")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stack"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "onStack"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "onStack")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sccs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionFieldName = (Core.Name "sccs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
