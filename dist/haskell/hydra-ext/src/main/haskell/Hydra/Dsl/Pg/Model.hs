-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.pg.model

module Hydra.Dsl.Pg.Model where

import qualified Hydra.Core as Core
import qualified Hydra.Pg.Model as Model
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M

adjacentEdge :: Phantoms.TTerm Model.EdgeLabel -> Phantoms.TTerm v -> Phantoms.TTerm v -> Phantoms.TTerm (M.Map Model.PropertyKey v) -> Phantoms.TTerm (Model.AdjacentEdge v)
adjacentEdge label id vertex properties =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Phantoms.unTTerm vertex)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm properties)}]}))

adjacentEdgeId :: Phantoms.TTerm (Model.AdjacentEdge v) -> Phantoms.TTerm v
adjacentEdgeId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
        Core.projectionField = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

adjacentEdgeLabel :: Phantoms.TTerm (Model.AdjacentEdge v) -> Phantoms.TTerm Model.EdgeLabel
adjacentEdgeLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
        Core.projectionField = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

adjacentEdgeProperties :: Phantoms.TTerm (Model.AdjacentEdge v) -> Phantoms.TTerm (M.Map Model.PropertyKey v)
adjacentEdgeProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
        Core.projectionField = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

adjacentEdgeVertex :: Phantoms.TTerm (Model.AdjacentEdge v) -> Phantoms.TTerm v
adjacentEdgeVertex x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
        Core.projectionField = (Core.Name "vertex")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

adjacentEdgeWithId :: Phantoms.TTerm (Model.AdjacentEdge v) -> Phantoms.TTerm v -> Phantoms.TTerm (Model.AdjacentEdge v)
adjacentEdgeWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionField = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionField = (Core.Name "vertex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

adjacentEdgeWithLabel :: Phantoms.TTerm (Model.AdjacentEdge v) -> Phantoms.TTerm Model.EdgeLabel -> Phantoms.TTerm (Model.AdjacentEdge v)
adjacentEdgeWithLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionField = (Core.Name "vertex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

adjacentEdgeWithProperties :: Phantoms.TTerm (Model.AdjacentEdge v) -> Phantoms.TTerm (M.Map Model.PropertyKey v) -> Phantoms.TTerm (Model.AdjacentEdge v)
adjacentEdgeWithProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionField = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionField = (Core.Name "vertex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

adjacentEdgeWithVertex :: Phantoms.TTerm (Model.AdjacentEdge v) -> Phantoms.TTerm v -> Phantoms.TTerm (Model.AdjacentEdge v)
adjacentEdgeWithVertex original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionField = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

directionBoth :: Phantoms.TTerm Model.Direction
directionBoth =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Direction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "both"),
        Core.fieldTerm = Core.TermUnit}}))

directionIn :: Phantoms.TTerm Model.Direction
directionIn =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Direction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "in"),
        Core.fieldTerm = Core.TermUnit}}))

directionOut :: Phantoms.TTerm Model.Direction
directionOut =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Direction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "out"),
        Core.fieldTerm = Core.TermUnit}}))

directionUndirected :: Phantoms.TTerm Model.Direction
directionUndirected =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Direction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undirected"),
        Core.fieldTerm = Core.TermUnit}}))

edge :: Phantoms.TTerm Model.EdgeLabel -> Phantoms.TTerm v -> Phantoms.TTerm v -> Phantoms.TTerm v -> Phantoms.TTerm (M.Map Model.PropertyKey v) -> Phantoms.TTerm (Model.Edge v)
edge label id out in_ properties =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Edge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Phantoms.unTTerm out)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Phantoms.unTTerm in_)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm properties)}]}))

edgeId :: Phantoms.TTerm (Model.Edge v) -> Phantoms.TTerm v
edgeId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
        Core.projectionField = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgeIn :: Phantoms.TTerm (Model.Edge v) -> Phantoms.TTerm v
edgeIn x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
        Core.projectionField = (Core.Name "in")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgeLabel :: Phantoms.TTerm (Model.Edge v) -> Phantoms.TTerm Model.EdgeLabel
edgeLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
        Core.projectionField = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgeLabel_ :: Phantoms.TTerm String -> Phantoms.TTerm Model.EdgeLabel
edgeLabel_ x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.model.EdgeLabel"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

edgeOut :: Phantoms.TTerm (Model.Edge v) -> Phantoms.TTerm v
edgeOut x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
        Core.projectionField = (Core.Name "out")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgeProperties :: Phantoms.TTerm (Model.Edge v) -> Phantoms.TTerm (M.Map Model.PropertyKey v)
edgeProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
        Core.projectionField = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgeType :: Phantoms.TTerm Model.EdgeLabel -> Phantoms.TTerm t -> Phantoms.TTerm Model.VertexLabel -> Phantoms.TTerm Model.VertexLabel -> Phantoms.TTerm [Model.PropertyType t] -> Phantoms.TTerm (Model.EdgeType t)
edgeType label id out in_ properties =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.EdgeType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Phantoms.unTTerm out)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Phantoms.unTTerm in_)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm properties)}]}))

edgeTypeId :: Phantoms.TTerm (Model.EdgeType t) -> Phantoms.TTerm t
edgeTypeId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
        Core.projectionField = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgeTypeIn :: Phantoms.TTerm (Model.EdgeType t) -> Phantoms.TTerm Model.VertexLabel
edgeTypeIn x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
        Core.projectionField = (Core.Name "in")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgeTypeLabel :: Phantoms.TTerm (Model.EdgeType t) -> Phantoms.TTerm Model.EdgeLabel
edgeTypeLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
        Core.projectionField = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgeTypeOut :: Phantoms.TTerm (Model.EdgeType t) -> Phantoms.TTerm Model.VertexLabel
edgeTypeOut x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
        Core.projectionField = (Core.Name "out")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgeTypeProperties :: Phantoms.TTerm (Model.EdgeType t) -> Phantoms.TTerm [Model.PropertyType t]
edgeTypeProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
        Core.projectionField = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgeTypeWithId :: Phantoms.TTerm (Model.EdgeType t) -> Phantoms.TTerm t -> Phantoms.TTerm (Model.EdgeType t)
edgeTypeWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.EdgeType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionField = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionField = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionField = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

edgeTypeWithIn :: Phantoms.TTerm (Model.EdgeType t) -> Phantoms.TTerm Model.VertexLabel -> Phantoms.TTerm (Model.EdgeType t)
edgeTypeWithIn original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.EdgeType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionField = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionField = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

edgeTypeWithLabel :: Phantoms.TTerm (Model.EdgeType t) -> Phantoms.TTerm Model.EdgeLabel -> Phantoms.TTerm (Model.EdgeType t)
edgeTypeWithLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.EdgeType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionField = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionField = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

edgeTypeWithOut :: Phantoms.TTerm (Model.EdgeType t) -> Phantoms.TTerm Model.VertexLabel -> Phantoms.TTerm (Model.EdgeType t)
edgeTypeWithOut original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.EdgeType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionField = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionField = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

edgeTypeWithProperties :: Phantoms.TTerm (Model.EdgeType t) -> Phantoms.TTerm [Model.PropertyType t] -> Phantoms.TTerm (Model.EdgeType t)
edgeTypeWithProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.EdgeType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionField = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionField = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionField = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

edgeWithId :: Phantoms.TTerm (Model.Edge v) -> Phantoms.TTerm v -> Phantoms.TTerm (Model.Edge v)
edgeWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Edge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionField = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionField = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionField = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

edgeWithIn :: Phantoms.TTerm (Model.Edge v) -> Phantoms.TTerm v -> Phantoms.TTerm (Model.Edge v)
edgeWithIn original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Edge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionField = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionField = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

edgeWithLabel :: Phantoms.TTerm (Model.Edge v) -> Phantoms.TTerm Model.EdgeLabel -> Phantoms.TTerm (Model.Edge v)
edgeWithLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Edge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionField = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionField = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

edgeWithOut :: Phantoms.TTerm (Model.Edge v) -> Phantoms.TTerm v -> Phantoms.TTerm (Model.Edge v)
edgeWithOut original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Edge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionField = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionField = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

edgeWithProperties :: Phantoms.TTerm (Model.Edge v) -> Phantoms.TTerm (M.Map Model.PropertyKey v) -> Phantoms.TTerm (Model.Edge v)
edgeWithProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Edge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionField = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionField = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionField = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

elementEdge :: Phantoms.TTerm (Model.Edge v) -> Phantoms.TTerm (Model.Element v)
elementEdge x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Element"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edge"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

elementKindEdge :: Phantoms.TTerm Model.ElementKind
elementKindEdge =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.ElementKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edge"),
        Core.fieldTerm = Core.TermUnit}}))

elementKindVertex :: Phantoms.TTerm Model.ElementKind
elementKindVertex =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.ElementKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vertex"),
        Core.fieldTerm = Core.TermUnit}}))

elementTree :: Phantoms.TTerm (Model.Element v) -> Phantoms.TTerm [Model.ElementTree v] -> Phantoms.TTerm (Model.ElementTree v)
elementTree self dependencies =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.ElementTree"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Phantoms.unTTerm self)},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Phantoms.unTTerm dependencies)}]}))

elementTreeDependencies :: Phantoms.TTerm (Model.ElementTree v) -> Phantoms.TTerm [Model.ElementTree v]
elementTreeDependencies x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTree"),
        Core.projectionField = (Core.Name "dependencies")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

elementTreeSelf :: Phantoms.TTerm (Model.ElementTree v) -> Phantoms.TTerm (Model.Element v)
elementTreeSelf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTree"),
        Core.projectionField = (Core.Name "self")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

elementTreeWithDependencies :: Phantoms.TTerm (Model.ElementTree v) -> Phantoms.TTerm [Model.ElementTree v] -> Phantoms.TTerm (Model.ElementTree v)
elementTreeWithDependencies original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.ElementTree"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTree"),
              Core.projectionField = (Core.Name "self")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

elementTreeWithSelf :: Phantoms.TTerm (Model.ElementTree v) -> Phantoms.TTerm (Model.Element v) -> Phantoms.TTerm (Model.ElementTree v)
elementTreeWithSelf original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.ElementTree"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTree"),
              Core.projectionField = (Core.Name "dependencies")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

elementTypeEdge :: Phantoms.TTerm (Model.EdgeType t) -> Phantoms.TTerm (Model.ElementType t)
elementTypeEdge x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.ElementType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edge"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

elementTypeTree :: Phantoms.TTerm (Model.ElementType t) -> Phantoms.TTerm [Model.ElementTypeTree t] -> Phantoms.TTerm (Model.ElementTypeTree t)
elementTypeTree self dependencies =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.ElementTypeTree"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Phantoms.unTTerm self)},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Phantoms.unTTerm dependencies)}]}))

elementTypeTreeDependencies :: Phantoms.TTerm (Model.ElementTypeTree t) -> Phantoms.TTerm [Model.ElementTypeTree t]
elementTypeTreeDependencies x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTypeTree"),
        Core.projectionField = (Core.Name "dependencies")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

elementTypeTreeSelf :: Phantoms.TTerm (Model.ElementTypeTree t) -> Phantoms.TTerm (Model.ElementType t)
elementTypeTreeSelf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTypeTree"),
        Core.projectionField = (Core.Name "self")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

elementTypeTreeWithDependencies :: Phantoms.TTerm (Model.ElementTypeTree t) -> Phantoms.TTerm [Model.ElementTypeTree t] -> Phantoms.TTerm (Model.ElementTypeTree t)
elementTypeTreeWithDependencies original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.ElementTypeTree"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTypeTree"),
              Core.projectionField = (Core.Name "self")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

elementTypeTreeWithSelf :: Phantoms.TTerm (Model.ElementTypeTree t) -> Phantoms.TTerm (Model.ElementType t) -> Phantoms.TTerm (Model.ElementTypeTree t)
elementTypeTreeWithSelf original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.ElementTypeTree"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTypeTree"),
              Core.projectionField = (Core.Name "dependencies")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

elementTypeVertex :: Phantoms.TTerm (Model.VertexType t) -> Phantoms.TTerm (Model.ElementType t)
elementTypeVertex x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.ElementType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vertex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

elementVertex :: Phantoms.TTerm (Model.Vertex v) -> Phantoms.TTerm (Model.Element v)
elementVertex x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Element"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vertex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

graph :: Ord v => (Phantoms.TTerm (M.Map v (Model.Vertex v)) -> Phantoms.TTerm (M.Map v (Model.Edge v)) -> Phantoms.TTerm (Model.Graph v))
graph vertices edges =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = (Phantoms.unTTerm vertices)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Phantoms.unTTerm edges)}]}))

graphEdges :: Ord v => (Phantoms.TTerm (Model.Graph v) -> Phantoms.TTerm (M.Map v (Model.Edge v)))
graphEdges x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Graph"),
        Core.projectionField = (Core.Name "edges")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

graphSchema :: Phantoms.TTerm (M.Map Model.VertexLabel (Model.VertexType t)) -> Phantoms.TTerm (M.Map Model.EdgeLabel (Model.EdgeType t)) -> Phantoms.TTerm (Model.GraphSchema t)
graphSchema vertices edges =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.GraphSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = (Phantoms.unTTerm vertices)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Phantoms.unTTerm edges)}]}))

graphSchemaEdges :: Phantoms.TTerm (Model.GraphSchema t) -> Phantoms.TTerm (M.Map Model.EdgeLabel (Model.EdgeType t))
graphSchemaEdges x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.GraphSchema"),
        Core.projectionField = (Core.Name "edges")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

graphSchemaVertices :: Phantoms.TTerm (Model.GraphSchema t) -> Phantoms.TTerm (M.Map Model.VertexLabel (Model.VertexType t))
graphSchemaVertices x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.GraphSchema"),
        Core.projectionField = (Core.Name "vertices")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

graphSchemaWithEdges :: Phantoms.TTerm (Model.GraphSchema t) -> Phantoms.TTerm (M.Map Model.EdgeLabel (Model.EdgeType t)) -> Phantoms.TTerm (Model.GraphSchema t)
graphSchemaWithEdges original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.GraphSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.GraphSchema"),
              Core.projectionField = (Core.Name "vertices")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

graphSchemaWithVertices :: Phantoms.TTerm (Model.GraphSchema t) -> Phantoms.TTerm (M.Map Model.VertexLabel (Model.VertexType t)) -> Phantoms.TTerm (Model.GraphSchema t)
graphSchemaWithVertices original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.GraphSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.GraphSchema"),
              Core.projectionField = (Core.Name "edges")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

graphVertices :: Ord v => (Phantoms.TTerm (Model.Graph v) -> Phantoms.TTerm (M.Map v (Model.Vertex v)))
graphVertices x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Graph"),
        Core.projectionField = (Core.Name "vertices")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

graphWithEdges :: Ord v => (Phantoms.TTerm (Model.Graph v) -> Phantoms.TTerm (M.Map v (Model.Edge v)) -> Phantoms.TTerm (Model.Graph v))
graphWithEdges original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Graph"),
              Core.projectionField = (Core.Name "vertices")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

graphWithVertices :: Ord v => (Phantoms.TTerm (Model.Graph v) -> Phantoms.TTerm (M.Map v (Model.Vertex v)) -> Phantoms.TTerm (Model.Graph v))
graphWithVertices original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Graph"),
              Core.projectionField = (Core.Name "edges")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

labelEdge :: Phantoms.TTerm Model.EdgeLabel -> Phantoms.TTerm Model.Label
labelEdge x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Label"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edge"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

labelVertex :: Phantoms.TTerm Model.VertexLabel -> Phantoms.TTerm Model.Label
labelVertex x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Label"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vertex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

lazyGraph :: Phantoms.TTerm [Model.Vertex v] -> Phantoms.TTerm [Model.Edge v] -> Phantoms.TTerm (Model.LazyGraph v)
lazyGraph vertices edges =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.LazyGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = (Phantoms.unTTerm vertices)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Phantoms.unTTerm edges)}]}))

lazyGraphEdges :: Phantoms.TTerm (Model.LazyGraph v) -> Phantoms.TTerm [Model.Edge v]
lazyGraphEdges x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.LazyGraph"),
        Core.projectionField = (Core.Name "edges")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lazyGraphVertices :: Phantoms.TTerm (Model.LazyGraph v) -> Phantoms.TTerm [Model.Vertex v]
lazyGraphVertices x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.LazyGraph"),
        Core.projectionField = (Core.Name "vertices")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lazyGraphWithEdges :: Phantoms.TTerm (Model.LazyGraph v) -> Phantoms.TTerm [Model.Edge v] -> Phantoms.TTerm (Model.LazyGraph v)
lazyGraphWithEdges original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.LazyGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.LazyGraph"),
              Core.projectionField = (Core.Name "vertices")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

lazyGraphWithVertices :: Phantoms.TTerm (Model.LazyGraph v) -> Phantoms.TTerm [Model.Vertex v] -> Phantoms.TTerm (Model.LazyGraph v)
lazyGraphWithVertices original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.LazyGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.LazyGraph"),
              Core.projectionField = (Core.Name "edges")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

property :: Phantoms.TTerm Model.PropertyKey -> Phantoms.TTerm v -> Phantoms.TTerm (Model.Property v)
property key value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

propertyKey :: Phantoms.TTerm (Model.Property v) -> Phantoms.TTerm Model.PropertyKey
propertyKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Property"),
        Core.projectionField = (Core.Name "key")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyKey_ :: Phantoms.TTerm String -> Phantoms.TTerm Model.PropertyKey
propertyKey_ x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.model.PropertyKey"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

propertyType :: Phantoms.TTerm Model.PropertyKey -> Phantoms.TTerm t -> Phantoms.TTerm Bool -> Phantoms.TTerm (Model.PropertyType t)
propertyType key value required =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.PropertyType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "required"),
          Core.fieldTerm = (Phantoms.unTTerm required)}]}))

propertyTypeKey :: Phantoms.TTerm (Model.PropertyType t) -> Phantoms.TTerm Model.PropertyKey
propertyTypeKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
        Core.projectionField = (Core.Name "key")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyTypeRequired :: Phantoms.TTerm (Model.PropertyType t) -> Phantoms.TTerm Bool
propertyTypeRequired x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
        Core.projectionField = (Core.Name "required")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyTypeValue :: Phantoms.TTerm (Model.PropertyType t) -> Phantoms.TTerm t
propertyTypeValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyTypeWithKey :: Phantoms.TTerm (Model.PropertyType t) -> Phantoms.TTerm Model.PropertyKey -> Phantoms.TTerm (Model.PropertyType t)
propertyTypeWithKey original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.PropertyType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "required"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
              Core.projectionField = (Core.Name "required")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

propertyTypeWithRequired :: Phantoms.TTerm (Model.PropertyType t) -> Phantoms.TTerm Bool -> Phantoms.TTerm (Model.PropertyType t)
propertyTypeWithRequired original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.PropertyType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
              Core.projectionField = (Core.Name "key")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "required"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

propertyTypeWithValue :: Phantoms.TTerm (Model.PropertyType t) -> Phantoms.TTerm t -> Phantoms.TTerm (Model.PropertyType t)
propertyTypeWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.PropertyType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
              Core.projectionField = (Core.Name "key")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "required"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
              Core.projectionField = (Core.Name "required")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

propertyValue :: Phantoms.TTerm (Model.Property v) -> Phantoms.TTerm v
propertyValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Property"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyWithKey :: Phantoms.TTerm (Model.Property v) -> Phantoms.TTerm Model.PropertyKey -> Phantoms.TTerm (Model.Property v)
propertyWithKey original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Property"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

propertyWithValue :: Phantoms.TTerm (Model.Property v) -> Phantoms.TTerm v -> Phantoms.TTerm (Model.Property v)
propertyWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Property"),
              Core.projectionField = (Core.Name "key")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unEdgeLabel :: Phantoms.TTerm Model.EdgeLabel -> Phantoms.TTerm String
unEdgeLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.pg.model.EdgeLabel")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unPropertyKey :: Phantoms.TTerm Model.PropertyKey -> Phantoms.TTerm String
unPropertyKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.pg.model.PropertyKey")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unVertexLabel :: Phantoms.TTerm Model.VertexLabel -> Phantoms.TTerm String
unVertexLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.pg.model.VertexLabel")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertex :: Phantoms.TTerm Model.VertexLabel -> Phantoms.TTerm v -> Phantoms.TTerm (M.Map Model.PropertyKey v) -> Phantoms.TTerm (Model.Vertex v)
vertex label id properties =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Vertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm properties)}]}))

vertexId :: Phantoms.TTerm (Model.Vertex v) -> Phantoms.TTerm v
vertexId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
        Core.projectionField = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexLabel :: Phantoms.TTerm (Model.Vertex v) -> Phantoms.TTerm Model.VertexLabel
vertexLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
        Core.projectionField = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexLabel_ :: Phantoms.TTerm String -> Phantoms.TTerm Model.VertexLabel
vertexLabel_ x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.model.VertexLabel"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

vertexProperties :: Phantoms.TTerm (Model.Vertex v) -> Phantoms.TTerm (M.Map Model.PropertyKey v)
vertexProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
        Core.projectionField = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexType :: Phantoms.TTerm Model.VertexLabel -> Phantoms.TTerm t -> Phantoms.TTerm [Model.PropertyType t] -> Phantoms.TTerm (Model.VertexType t)
vertexType label id properties =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.VertexType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm properties)}]}))

vertexTypeId :: Phantoms.TTerm (Model.VertexType t) -> Phantoms.TTerm t
vertexTypeId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
        Core.projectionField = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexTypeLabel :: Phantoms.TTerm (Model.VertexType t) -> Phantoms.TTerm Model.VertexLabel
vertexTypeLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
        Core.projectionField = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexTypeProperties :: Phantoms.TTerm (Model.VertexType t) -> Phantoms.TTerm [Model.PropertyType t]
vertexTypeProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
        Core.projectionField = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexTypeWithId :: Phantoms.TTerm (Model.VertexType t) -> Phantoms.TTerm t -> Phantoms.TTerm (Model.VertexType t)
vertexTypeWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.VertexType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
              Core.projectionField = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

vertexTypeWithLabel :: Phantoms.TTerm (Model.VertexType t) -> Phantoms.TTerm Model.VertexLabel -> Phantoms.TTerm (Model.VertexType t)
vertexTypeWithLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.VertexType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

vertexTypeWithProperties :: Phantoms.TTerm (Model.VertexType t) -> Phantoms.TTerm [Model.PropertyType t] -> Phantoms.TTerm (Model.VertexType t)
vertexTypeWithProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.VertexType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
              Core.projectionField = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

vertexWithAdjacentEdges :: Phantoms.TTerm (Model.Vertex v) -> Phantoms.TTerm [Model.AdjacentEdge v] -> Phantoms.TTerm [Model.AdjacentEdge v] -> Phantoms.TTerm (Model.VertexWithAdjacentEdges v)
vertexWithAdjacentEdges vertex ins outs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Phantoms.unTTerm vertex)},
        Core.Field {
          Core.fieldName = (Core.Name "ins"),
          Core.fieldTerm = (Phantoms.unTTerm ins)},
        Core.Field {
          Core.fieldName = (Core.Name "outs"),
          Core.fieldTerm = (Phantoms.unTTerm outs)}]}))

vertexWithAdjacentEdgesIns :: Phantoms.TTerm (Model.VertexWithAdjacentEdges v) -> Phantoms.TTerm [Model.AdjacentEdge v]
vertexWithAdjacentEdgesIns x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
        Core.projectionField = (Core.Name "ins")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexWithAdjacentEdgesOuts :: Phantoms.TTerm (Model.VertexWithAdjacentEdges v) -> Phantoms.TTerm [Model.AdjacentEdge v]
vertexWithAdjacentEdgesOuts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
        Core.projectionField = (Core.Name "outs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexWithAdjacentEdgesVertex :: Phantoms.TTerm (Model.VertexWithAdjacentEdges v) -> Phantoms.TTerm (Model.Vertex v)
vertexWithAdjacentEdgesVertex x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
        Core.projectionField = (Core.Name "vertex")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexWithAdjacentEdgesWithIns :: Phantoms.TTerm (Model.VertexWithAdjacentEdges v) -> Phantoms.TTerm [Model.AdjacentEdge v] -> Phantoms.TTerm (Model.VertexWithAdjacentEdges v)
vertexWithAdjacentEdgesWithIns original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
              Core.projectionField = (Core.Name "vertex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ins"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "outs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
              Core.projectionField = (Core.Name "outs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

vertexWithAdjacentEdgesWithOuts :: Phantoms.TTerm (Model.VertexWithAdjacentEdges v) -> Phantoms.TTerm [Model.AdjacentEdge v] -> Phantoms.TTerm (Model.VertexWithAdjacentEdges v)
vertexWithAdjacentEdgesWithOuts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
              Core.projectionField = (Core.Name "vertex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ins"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
              Core.projectionField = (Core.Name "ins")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

vertexWithAdjacentEdgesWithVertex :: Phantoms.TTerm (Model.VertexWithAdjacentEdges v) -> Phantoms.TTerm (Model.Vertex v) -> Phantoms.TTerm (Model.VertexWithAdjacentEdges v)
vertexWithAdjacentEdgesWithVertex original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ins"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
              Core.projectionField = (Core.Name "ins")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
              Core.projectionField = (Core.Name "outs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

vertexWithId :: Phantoms.TTerm (Model.Vertex v) -> Phantoms.TTerm v -> Phantoms.TTerm (Model.Vertex v)
vertexWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Vertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
              Core.projectionField = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

vertexWithLabel :: Phantoms.TTerm (Model.Vertex v) -> Phantoms.TTerm Model.VertexLabel -> Phantoms.TTerm (Model.Vertex v)
vertexWithLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Vertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

vertexWithProperties :: Phantoms.TTerm (Model.Vertex v) -> Phantoms.TTerm (M.Map Model.PropertyKey v) -> Phantoms.TTerm (Model.Vertex v)
vertexWithProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Vertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
              Core.projectionField = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
              Core.projectionField = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
