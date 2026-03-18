-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.topology

module Hydra.Dsl.Topology where

import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Topology as Topology
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

orderingIsomorphism :: Phantoms.TTerm ([a] -> [a]) -> Phantoms.TTerm ([a] -> [a]) -> Phantoms.TTerm (Topology.OrderingIsomorphism a)
orderingIsomorphism encode decode =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.topology.OrderingIsomorphism"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Phantoms.unTTerm encode)},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Phantoms.unTTerm decode)}]}))

orderingIsomorphismEncode :: Phantoms.TTerm (Topology.OrderingIsomorphism a) -> Phantoms.TTerm ([a] -> [a])
orderingIsomorphismEncode x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.topology.OrderingIsomorphism"),
        Core.projectionField = (Core.Name "encode")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

orderingIsomorphismDecode :: Phantoms.TTerm (Topology.OrderingIsomorphism a) -> Phantoms.TTerm ([a] -> [a])
orderingIsomorphismDecode x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.topology.OrderingIsomorphism"),
        Core.projectionField = (Core.Name "decode")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

orderingIsomorphismWithEncode :: Phantoms.TTerm (Topology.OrderingIsomorphism a) -> Phantoms.TTerm ([a] -> [a]) -> Phantoms.TTerm (Topology.OrderingIsomorphism a)
orderingIsomorphismWithEncode original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.topology.OrderingIsomorphism"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.OrderingIsomorphism"),
              Core.projectionField = (Core.Name "decode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

orderingIsomorphismWithDecode :: Phantoms.TTerm (Topology.OrderingIsomorphism a) -> Phantoms.TTerm ([a] -> [a]) -> Phantoms.TTerm (Topology.OrderingIsomorphism a)
orderingIsomorphismWithDecode original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.topology.OrderingIsomorphism"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.OrderingIsomorphism"),
              Core.projectionField = (Core.Name "encode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

tarjanState :: Phantoms.TTerm Int -> Phantoms.TTerm (M.Map Topology.Vertex Int) -> Phantoms.TTerm (M.Map Topology.Vertex Int) -> Phantoms.TTerm [Topology.Vertex] -> Phantoms.TTerm (S.Set Topology.Vertex) -> Phantoms.TTerm [[Topology.Vertex]] -> Phantoms.TTerm Topology.TarjanState
tarjanState counter indices lowLinks stack onStack sccs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.topology.TarjanState"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "counter"),
          Core.fieldTerm = (Phantoms.unTTerm counter)},
        Core.Field {
          Core.fieldName = (Core.Name "indices"),
          Core.fieldTerm = (Phantoms.unTTerm indices)},
        Core.Field {
          Core.fieldName = (Core.Name "lowLinks"),
          Core.fieldTerm = (Phantoms.unTTerm lowLinks)},
        Core.Field {
          Core.fieldName = (Core.Name "stack"),
          Core.fieldTerm = (Phantoms.unTTerm stack)},
        Core.Field {
          Core.fieldName = (Core.Name "onStack"),
          Core.fieldTerm = (Phantoms.unTTerm onStack)},
        Core.Field {
          Core.fieldName = (Core.Name "sccs"),
          Core.fieldTerm = (Phantoms.unTTerm sccs)}]}))

tarjanStateCounter :: Phantoms.TTerm Topology.TarjanState -> Phantoms.TTerm Int
tarjanStateCounter x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
        Core.projectionField = (Core.Name "counter")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tarjanStateIndices :: Phantoms.TTerm Topology.TarjanState -> Phantoms.TTerm (M.Map Topology.Vertex Int)
tarjanStateIndices x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
        Core.projectionField = (Core.Name "indices")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tarjanStateLowLinks :: Phantoms.TTerm Topology.TarjanState -> Phantoms.TTerm (M.Map Topology.Vertex Int)
tarjanStateLowLinks x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
        Core.projectionField = (Core.Name "lowLinks")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tarjanStateStack :: Phantoms.TTerm Topology.TarjanState -> Phantoms.TTerm [Topology.Vertex]
tarjanStateStack x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
        Core.projectionField = (Core.Name "stack")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tarjanStateOnStack :: Phantoms.TTerm Topology.TarjanState -> Phantoms.TTerm (S.Set Topology.Vertex)
tarjanStateOnStack x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
        Core.projectionField = (Core.Name "onStack")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tarjanStateSccs :: Phantoms.TTerm Topology.TarjanState -> Phantoms.TTerm [[Topology.Vertex]]
tarjanStateSccs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
        Core.projectionField = (Core.Name "sccs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tarjanStateWithCounter :: Phantoms.TTerm Topology.TarjanState -> Phantoms.TTerm Int -> Phantoms.TTerm Topology.TarjanState
tarjanStateWithCounter original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.topology.TarjanState"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "counter"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "indices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "indices")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lowLinks"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "lowLinks")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stack"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "stack")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "onStack"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "onStack")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sccs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "sccs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tarjanStateWithIndices :: Phantoms.TTerm Topology.TarjanState -> Phantoms.TTerm (M.Map Topology.Vertex Int) -> Phantoms.TTerm Topology.TarjanState
tarjanStateWithIndices original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.topology.TarjanState"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "counter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "counter")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indices"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "lowLinks"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "lowLinks")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stack"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "stack")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "onStack"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "onStack")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sccs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "sccs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tarjanStateWithLowLinks :: Phantoms.TTerm Topology.TarjanState -> Phantoms.TTerm (M.Map Topology.Vertex Int) -> Phantoms.TTerm Topology.TarjanState
tarjanStateWithLowLinks original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.topology.TarjanState"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "counter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "counter")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "indices")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lowLinks"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "stack"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "stack")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "onStack"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "onStack")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sccs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "sccs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tarjanStateWithStack :: Phantoms.TTerm Topology.TarjanState -> Phantoms.TTerm [Topology.Vertex] -> Phantoms.TTerm Topology.TarjanState
tarjanStateWithStack original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.topology.TarjanState"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "counter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "counter")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "indices")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lowLinks"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "lowLinks")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stack"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "onStack"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "onStack")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sccs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "sccs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tarjanStateWithOnStack :: Phantoms.TTerm Topology.TarjanState -> Phantoms.TTerm (S.Set Topology.Vertex) -> Phantoms.TTerm Topology.TarjanState
tarjanStateWithOnStack original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.topology.TarjanState"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "counter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "counter")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "indices")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lowLinks"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "lowLinks")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stack"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "stack")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "onStack"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sccs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "sccs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tarjanStateWithSccs :: Phantoms.TTerm Topology.TarjanState -> Phantoms.TTerm [[Topology.Vertex]] -> Phantoms.TTerm Topology.TarjanState
tarjanStateWithSccs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.topology.TarjanState"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "counter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "counter")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "indices")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lowLinks"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "lowLinks")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stack"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "stack")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "onStack"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
              Core.projectionField = (Core.Name "onStack")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sccs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
