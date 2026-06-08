-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.pg.model

module Hydra.Dsl.Pg.Model where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Pg.Model as Model
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
-- | DSL constructor for hydra.pg.model.AdjacentEdge
adjacentEdge :: Typed.TypedTerm Model.EdgeLabel -> Typed.TypedTerm v -> Typed.TypedTerm v -> Typed.TypedTerm (M.Map Model.PropertyKey v) -> Typed.TypedTerm (Model.AdjacentEdge v)
adjacentEdge label id vertex properties =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Typed.unTypedTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Typed.unTypedTerm vertex)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Typed.unTypedTerm properties)}]}))
-- | DSL accessor for the id field of hydra.pg.model.AdjacentEdge
adjacentEdgeId :: Typed.TypedTerm (Model.AdjacentEdge v) -> Typed.TypedTerm v
adjacentEdgeId x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the label field of hydra.pg.model.AdjacentEdge
adjacentEdgeLabel :: Typed.TypedTerm (Model.AdjacentEdge v) -> Typed.TypedTerm Model.EdgeLabel
adjacentEdgeLabel x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the properties field of hydra.pg.model.AdjacentEdge
adjacentEdgeProperties :: Typed.TypedTerm (Model.AdjacentEdge v) -> Typed.TypedTerm (M.Map Model.PropertyKey v)
adjacentEdgeProperties x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
        Core.projectionFieldName = (Core.Name "properties")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the vertex field of hydra.pg.model.AdjacentEdge
adjacentEdgeVertex :: Typed.TypedTerm (Model.AdjacentEdge v) -> Typed.TypedTerm v
adjacentEdgeVertex x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
        Core.projectionFieldName = (Core.Name "vertex")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the id field of hydra.pg.model.AdjacentEdge
adjacentEdgeWithId :: Typed.TypedTerm (Model.AdjacentEdge v) -> Typed.TypedTerm v -> Typed.TypedTerm (Model.AdjacentEdge v)
adjacentEdgeWithId original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "vertex")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the label field of hydra.pg.model.AdjacentEdge
adjacentEdgeWithLabel :: Typed.TypedTerm (Model.AdjacentEdge v) -> Typed.TypedTerm Model.EdgeLabel -> Typed.TypedTerm (Model.AdjacentEdge v)
adjacentEdgeWithLabel original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "vertex")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the properties field of hydra.pg.model.AdjacentEdge
adjacentEdgeWithProperties :: Typed.TypedTerm (Model.AdjacentEdge v) -> Typed.TypedTerm (M.Map Model.PropertyKey v) -> Typed.TypedTerm (Model.AdjacentEdge v)
adjacentEdgeWithProperties original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "vertex")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the vertex field of hydra.pg.model.AdjacentEdge
adjacentEdgeWithVertex :: Typed.TypedTerm (Model.AdjacentEdge v) -> Typed.TypedTerm v -> Typed.TypedTerm (Model.AdjacentEdge v)
adjacentEdgeWithVertex original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the both variant of hydra.pg.model.Direction
directionBoth :: Typed.TypedTerm Model.Direction
directionBoth =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Direction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "both"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the in variant of hydra.pg.model.Direction
directionIn :: Typed.TypedTerm Model.Direction
directionIn =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Direction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "in"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the out variant of hydra.pg.model.Direction
directionOut :: Typed.TypedTerm Model.Direction
directionOut =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Direction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "out"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the undirected variant of hydra.pg.model.Direction
directionUndirected :: Typed.TypedTerm Model.Direction
directionUndirected =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Direction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undirected"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.pg.model.Edge
edge :: Typed.TypedTerm Model.EdgeLabel -> Typed.TypedTerm v -> Typed.TypedTerm v -> Typed.TypedTerm v -> Typed.TypedTerm (M.Map Model.PropertyKey v) -> Typed.TypedTerm (Model.Edge v)
edge label id out in_ properties =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Edge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Typed.unTypedTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Typed.unTypedTerm out)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Typed.unTypedTerm in_)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Typed.unTypedTerm properties)}]}))
-- | DSL accessor for the id field of hydra.pg.model.Edge
edgeId :: Typed.TypedTerm (Model.Edge v) -> Typed.TypedTerm v
edgeId x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the in field of hydra.pg.model.Edge
edgeIn :: Typed.TypedTerm (Model.Edge v) -> Typed.TypedTerm v
edgeIn x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
        Core.projectionFieldName = (Core.Name "in")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the label field of hydra.pg.model.Edge
edgeLabel :: Typed.TypedTerm (Model.Edge v) -> Typed.TypedTerm Model.EdgeLabel
edgeLabel x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.pg.model.EdgeLabel wrapper
edgeLabel2 :: Typed.TypedTerm String -> Typed.TypedTerm Model.EdgeLabel
edgeLabel2 x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.model.EdgeLabel"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL accessor for the out field of hydra.pg.model.Edge
edgeOut :: Typed.TypedTerm (Model.Edge v) -> Typed.TypedTerm v
edgeOut x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
        Core.projectionFieldName = (Core.Name "out")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the properties field of hydra.pg.model.Edge
edgeProperties :: Typed.TypedTerm (Model.Edge v) -> Typed.TypedTerm (M.Map Model.PropertyKey v)
edgeProperties x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
        Core.projectionFieldName = (Core.Name "properties")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.pg.model.EdgeType
edgeType :: Typed.TypedTerm Model.EdgeLabel -> Typed.TypedTerm t -> Typed.TypedTerm Model.VertexLabel -> Typed.TypedTerm Model.VertexLabel -> Typed.TypedTerm [Model.PropertyType t] -> Typed.TypedTerm (Model.EdgeType t)
edgeType label id out in_ properties =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.EdgeType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Typed.unTypedTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Typed.unTypedTerm out)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Typed.unTypedTerm in_)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Typed.unTypedTerm properties)}]}))
-- | DSL accessor for the id field of hydra.pg.model.EdgeType
edgeTypeId :: Typed.TypedTerm (Model.EdgeType t) -> Typed.TypedTerm t
edgeTypeId x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the in field of hydra.pg.model.EdgeType
edgeTypeIn :: Typed.TypedTerm (Model.EdgeType t) -> Typed.TypedTerm Model.VertexLabel
edgeTypeIn x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
        Core.projectionFieldName = (Core.Name "in")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the label field of hydra.pg.model.EdgeType
edgeTypeLabel :: Typed.TypedTerm (Model.EdgeType t) -> Typed.TypedTerm Model.EdgeLabel
edgeTypeLabel x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the out field of hydra.pg.model.EdgeType
edgeTypeOut :: Typed.TypedTerm (Model.EdgeType t) -> Typed.TypedTerm Model.VertexLabel
edgeTypeOut x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
        Core.projectionFieldName = (Core.Name "out")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the properties field of hydra.pg.model.EdgeType
edgeTypeProperties :: Typed.TypedTerm (Model.EdgeType t) -> Typed.TypedTerm [Model.PropertyType t]
edgeTypeProperties x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
        Core.projectionFieldName = (Core.Name "properties")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the id field of hydra.pg.model.EdgeType
edgeTypeWithId :: Typed.TypedTerm (Model.EdgeType t) -> Typed.TypedTerm t -> Typed.TypedTerm (Model.EdgeType t)
edgeTypeWithId original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.EdgeType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the in field of hydra.pg.model.EdgeType
edgeTypeWithIn :: Typed.TypedTerm (Model.EdgeType t) -> Typed.TypedTerm Model.VertexLabel -> Typed.TypedTerm (Model.EdgeType t)
edgeTypeWithIn original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.EdgeType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the label field of hydra.pg.model.EdgeType
edgeTypeWithLabel :: Typed.TypedTerm (Model.EdgeType t) -> Typed.TypedTerm Model.EdgeLabel -> Typed.TypedTerm (Model.EdgeType t)
edgeTypeWithLabel original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.EdgeType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the out field of hydra.pg.model.EdgeType
edgeTypeWithOut :: Typed.TypedTerm (Model.EdgeType t) -> Typed.TypedTerm Model.VertexLabel -> Typed.TypedTerm (Model.EdgeType t)
edgeTypeWithOut original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.EdgeType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the properties field of hydra.pg.model.EdgeType
edgeTypeWithProperties :: Typed.TypedTerm (Model.EdgeType t) -> Typed.TypedTerm [Model.PropertyType t] -> Typed.TypedTerm (Model.EdgeType t)
edgeTypeWithProperties original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.EdgeType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the id field of hydra.pg.model.Edge
edgeWithId :: Typed.TypedTerm (Model.Edge v) -> Typed.TypedTerm v -> Typed.TypedTerm (Model.Edge v)
edgeWithId original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Edge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the in field of hydra.pg.model.Edge
edgeWithIn :: Typed.TypedTerm (Model.Edge v) -> Typed.TypedTerm v -> Typed.TypedTerm (Model.Edge v)
edgeWithIn original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Edge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the label field of hydra.pg.model.Edge
edgeWithLabel :: Typed.TypedTerm (Model.Edge v) -> Typed.TypedTerm Model.EdgeLabel -> Typed.TypedTerm (Model.Edge v)
edgeWithLabel original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Edge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the out field of hydra.pg.model.Edge
edgeWithOut :: Typed.TypedTerm (Model.Edge v) -> Typed.TypedTerm v -> Typed.TypedTerm (Model.Edge v)
edgeWithOut original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Edge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the properties field of hydra.pg.model.Edge
edgeWithProperties :: Typed.TypedTerm (Model.Edge v) -> Typed.TypedTerm (M.Map Model.PropertyKey v) -> Typed.TypedTerm (Model.Edge v)
edgeWithProperties original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Edge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the edge variant of hydra.pg.model.Element
elementEdge :: Typed.TypedTerm (Model.Edge v) -> Typed.TypedTerm (Model.Element v)
elementEdge x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Element"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edge"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the edge variant of hydra.pg.model.ElementKind
elementKindEdge :: Typed.TypedTerm Model.ElementKind
elementKindEdge =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.ElementKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edge"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the vertex variant of hydra.pg.model.ElementKind
elementKindVertex :: Typed.TypedTerm Model.ElementKind
elementKindVertex =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.ElementKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vertex"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.pg.model.ElementTree
elementTree :: Typed.TypedTerm (Model.Element v) -> Typed.TypedTerm [Model.ElementTree v] -> Typed.TypedTerm (Model.ElementTree v)
elementTree self dependencies =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.ElementTree"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Typed.unTypedTerm self)},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Typed.unTypedTerm dependencies)}]}))
-- | DSL accessor for the dependencies field of hydra.pg.model.ElementTree
elementTreeDependencies :: Typed.TypedTerm (Model.ElementTree v) -> Typed.TypedTerm [Model.ElementTree v]
elementTreeDependencies x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTree"),
        Core.projectionFieldName = (Core.Name "dependencies")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the self field of hydra.pg.model.ElementTree
elementTreeSelf :: Typed.TypedTerm (Model.ElementTree v) -> Typed.TypedTerm (Model.Element v)
elementTreeSelf x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTree"),
        Core.projectionFieldName = (Core.Name "self")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the dependencies field of hydra.pg.model.ElementTree
elementTreeWithDependencies :: Typed.TypedTerm (Model.ElementTree v) -> Typed.TypedTerm [Model.ElementTree v] -> Typed.TypedTerm (Model.ElementTree v)
elementTreeWithDependencies original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.ElementTree"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTree"),
              Core.projectionFieldName = (Core.Name "self")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the self field of hydra.pg.model.ElementTree
elementTreeWithSelf :: Typed.TypedTerm (Model.ElementTree v) -> Typed.TypedTerm (Model.Element v) -> Typed.TypedTerm (Model.ElementTree v)
elementTreeWithSelf original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.ElementTree"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTree"),
              Core.projectionFieldName = (Core.Name "dependencies")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the edge variant of hydra.pg.model.ElementType
elementTypeEdge :: Typed.TypedTerm (Model.EdgeType t) -> Typed.TypedTerm (Model.ElementType t)
elementTypeEdge x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.ElementType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edge"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.pg.model.ElementTypeTree
elementTypeTree :: Typed.TypedTerm (Model.ElementType t) -> Typed.TypedTerm [Model.ElementTypeTree t] -> Typed.TypedTerm (Model.ElementTypeTree t)
elementTypeTree self dependencies =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.ElementTypeTree"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Typed.unTypedTerm self)},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Typed.unTypedTerm dependencies)}]}))
-- | DSL accessor for the dependencies field of hydra.pg.model.ElementTypeTree
elementTypeTreeDependencies :: Typed.TypedTerm (Model.ElementTypeTree t) -> Typed.TypedTerm [Model.ElementTypeTree t]
elementTypeTreeDependencies x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTypeTree"),
        Core.projectionFieldName = (Core.Name "dependencies")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the self field of hydra.pg.model.ElementTypeTree
elementTypeTreeSelf :: Typed.TypedTerm (Model.ElementTypeTree t) -> Typed.TypedTerm (Model.ElementType t)
elementTypeTreeSelf x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTypeTree"),
        Core.projectionFieldName = (Core.Name "self")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the dependencies field of hydra.pg.model.ElementTypeTree
elementTypeTreeWithDependencies :: Typed.TypedTerm (Model.ElementTypeTree t) -> Typed.TypedTerm [Model.ElementTypeTree t] -> Typed.TypedTerm (Model.ElementTypeTree t)
elementTypeTreeWithDependencies original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.ElementTypeTree"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTypeTree"),
              Core.projectionFieldName = (Core.Name "self")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the self field of hydra.pg.model.ElementTypeTree
elementTypeTreeWithSelf :: Typed.TypedTerm (Model.ElementTypeTree t) -> Typed.TypedTerm (Model.ElementType t) -> Typed.TypedTerm (Model.ElementTypeTree t)
elementTypeTreeWithSelf original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.ElementTypeTree"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTypeTree"),
              Core.projectionFieldName = (Core.Name "dependencies")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the vertex variant of hydra.pg.model.ElementType
elementTypeVertex :: Typed.TypedTerm (Model.VertexType t) -> Typed.TypedTerm (Model.ElementType t)
elementTypeVertex x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.ElementType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vertex"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the vertex variant of hydra.pg.model.Element
elementVertex :: Typed.TypedTerm (Model.Vertex v) -> Typed.TypedTerm (Model.Element v)
elementVertex x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Element"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vertex"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.pg.model.Graph
graph :: Ord v => (Typed.TypedTerm (M.Map v (Model.Vertex v)) -> Typed.TypedTerm (M.Map v (Model.Edge v)) -> Typed.TypedTerm (Model.Graph v))
graph vertices edges =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = (Typed.unTypedTerm vertices)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Typed.unTypedTerm edges)}]}))
-- | DSL accessor for the edges field of hydra.pg.model.Graph
graphEdges :: Ord v => (Typed.TypedTerm (Model.Graph v) -> Typed.TypedTerm (M.Map v (Model.Edge v)))
graphEdges x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Graph"),
        Core.projectionFieldName = (Core.Name "edges")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.pg.model.GraphSchema
graphSchema :: Typed.TypedTerm (M.Map Model.VertexLabel (Model.VertexType t)) -> Typed.TypedTerm (M.Map Model.EdgeLabel (Model.EdgeType t)) -> Typed.TypedTerm (Model.GraphSchema t)
graphSchema vertices edges =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.GraphSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = (Typed.unTypedTerm vertices)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Typed.unTypedTerm edges)}]}))
-- | DSL accessor for the edges field of hydra.pg.model.GraphSchema
graphSchemaEdges :: Typed.TypedTerm (Model.GraphSchema t) -> Typed.TypedTerm (M.Map Model.EdgeLabel (Model.EdgeType t))
graphSchemaEdges x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.GraphSchema"),
        Core.projectionFieldName = (Core.Name "edges")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the vertices field of hydra.pg.model.GraphSchema
graphSchemaVertices :: Typed.TypedTerm (Model.GraphSchema t) -> Typed.TypedTerm (M.Map Model.VertexLabel (Model.VertexType t))
graphSchemaVertices x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.GraphSchema"),
        Core.projectionFieldName = (Core.Name "vertices")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the edges field of hydra.pg.model.GraphSchema
graphSchemaWithEdges :: Typed.TypedTerm (Model.GraphSchema t) -> Typed.TypedTerm (M.Map Model.EdgeLabel (Model.EdgeType t)) -> Typed.TypedTerm (Model.GraphSchema t)
graphSchemaWithEdges original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.GraphSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.GraphSchema"),
              Core.projectionFieldName = (Core.Name "vertices")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the vertices field of hydra.pg.model.GraphSchema
graphSchemaWithVertices :: Typed.TypedTerm (Model.GraphSchema t) -> Typed.TypedTerm (M.Map Model.VertexLabel (Model.VertexType t)) -> Typed.TypedTerm (Model.GraphSchema t)
graphSchemaWithVertices original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.GraphSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.GraphSchema"),
              Core.projectionFieldName = (Core.Name "edges")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL accessor for the vertices field of hydra.pg.model.Graph
graphVertices :: Ord v => (Typed.TypedTerm (Model.Graph v) -> Typed.TypedTerm (M.Map v (Model.Vertex v)))
graphVertices x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Graph"),
        Core.projectionFieldName = (Core.Name "vertices")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the edges field of hydra.pg.model.Graph
graphWithEdges :: Ord v => (Typed.TypedTerm (Model.Graph v) -> Typed.TypedTerm (M.Map v (Model.Edge v)) -> Typed.TypedTerm (Model.Graph v))
graphWithEdges original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Graph"),
              Core.projectionFieldName = (Core.Name "vertices")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the vertices field of hydra.pg.model.Graph
graphWithVertices :: Ord v => (Typed.TypedTerm (Model.Graph v) -> Typed.TypedTerm (M.Map v (Model.Vertex v)) -> Typed.TypedTerm (Model.Graph v))
graphWithVertices original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Graph"),
              Core.projectionFieldName = (Core.Name "edges")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the edge variant of hydra.pg.model.Label
labelEdge :: Typed.TypedTerm Model.EdgeLabel -> Typed.TypedTerm Model.Label
labelEdge x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Label"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edge"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the vertex variant of hydra.pg.model.Label
labelVertex :: Typed.TypedTerm Model.VertexLabel -> Typed.TypedTerm Model.Label
labelVertex x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Label"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vertex"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.pg.model.LazyGraph
lazyGraph :: Typed.TypedTerm [Model.Vertex v] -> Typed.TypedTerm [Model.Edge v] -> Typed.TypedTerm (Model.LazyGraph v)
lazyGraph vertices edges =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.LazyGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = (Typed.unTypedTerm vertices)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Typed.unTypedTerm edges)}]}))
-- | DSL accessor for the edges field of hydra.pg.model.LazyGraph
lazyGraphEdges :: Typed.TypedTerm (Model.LazyGraph v) -> Typed.TypedTerm [Model.Edge v]
lazyGraphEdges x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.LazyGraph"),
        Core.projectionFieldName = (Core.Name "edges")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the vertices field of hydra.pg.model.LazyGraph
lazyGraphVertices :: Typed.TypedTerm (Model.LazyGraph v) -> Typed.TypedTerm [Model.Vertex v]
lazyGraphVertices x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.LazyGraph"),
        Core.projectionFieldName = (Core.Name "vertices")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the edges field of hydra.pg.model.LazyGraph
lazyGraphWithEdges :: Typed.TypedTerm (Model.LazyGraph v) -> Typed.TypedTerm [Model.Edge v] -> Typed.TypedTerm (Model.LazyGraph v)
lazyGraphWithEdges original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.LazyGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.LazyGraph"),
              Core.projectionFieldName = (Core.Name "vertices")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the vertices field of hydra.pg.model.LazyGraph
lazyGraphWithVertices :: Typed.TypedTerm (Model.LazyGraph v) -> Typed.TypedTerm [Model.Vertex v] -> Typed.TypedTerm (Model.LazyGraph v)
lazyGraphWithVertices original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.LazyGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.LazyGraph"),
              Core.projectionFieldName = (Core.Name "edges")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.pg.model.Property
property :: Typed.TypedTerm Model.PropertyKey -> Typed.TypedTerm v -> Typed.TypedTerm (Model.Property v)
property key value =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Typed.unTypedTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)}]}))
-- | DSL accessor for the key field of hydra.pg.model.Property
propertyKey :: Typed.TypedTerm (Model.Property v) -> Typed.TypedTerm Model.PropertyKey
propertyKey x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Property"),
        Core.projectionFieldName = (Core.Name "key")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.pg.model.PropertyKey wrapper
propertyKey2 :: Typed.TypedTerm String -> Typed.TypedTerm Model.PropertyKey
propertyKey2 x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.model.PropertyKey"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.pg.model.PropertyType
propertyType :: Typed.TypedTerm Model.PropertyKey -> Typed.TypedTerm t -> Typed.TypedTerm Bool -> Typed.TypedTerm (Model.PropertyType t)
propertyType key value required =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.PropertyType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Typed.unTypedTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "required"),
          Core.fieldTerm = (Typed.unTypedTerm required)}]}))
-- | DSL accessor for the key field of hydra.pg.model.PropertyType
propertyTypeKey :: Typed.TypedTerm (Model.PropertyType t) -> Typed.TypedTerm Model.PropertyKey
propertyTypeKey x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
        Core.projectionFieldName = (Core.Name "key")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the required field of hydra.pg.model.PropertyType
propertyTypeRequired :: Typed.TypedTerm (Model.PropertyType t) -> Typed.TypedTerm Bool
propertyTypeRequired x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
        Core.projectionFieldName = (Core.Name "required")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.pg.model.PropertyType
propertyTypeValue :: Typed.TypedTerm (Model.PropertyType t) -> Typed.TypedTerm t
propertyTypeValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the key field of hydra.pg.model.PropertyType
propertyTypeWithKey :: Typed.TypedTerm (Model.PropertyType t) -> Typed.TypedTerm Model.PropertyKey -> Typed.TypedTerm (Model.PropertyType t)
propertyTypeWithKey original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.PropertyType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "required"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
              Core.projectionFieldName = (Core.Name "required")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the required field of hydra.pg.model.PropertyType
propertyTypeWithRequired :: Typed.TypedTerm (Model.PropertyType t) -> Typed.TypedTerm Bool -> Typed.TypedTerm (Model.PropertyType t)
propertyTypeWithRequired original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.PropertyType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "required"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the value field of hydra.pg.model.PropertyType
propertyTypeWithValue :: Typed.TypedTerm (Model.PropertyType t) -> Typed.TypedTerm t -> Typed.TypedTerm (Model.PropertyType t)
propertyTypeWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.PropertyType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "required"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
              Core.projectionFieldName = (Core.Name "required")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL accessor for the value field of hydra.pg.model.Property
propertyValue :: Typed.TypedTerm (Model.Property v) -> Typed.TypedTerm v
propertyValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Property"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the key field of hydra.pg.model.Property
propertyWithKey :: Typed.TypedTerm (Model.Property v) -> Typed.TypedTerm Model.PropertyKey -> Typed.TypedTerm (Model.Property v)
propertyWithKey original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Property"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the value field of hydra.pg.model.Property
propertyWithValue :: Typed.TypedTerm (Model.Property v) -> Typed.TypedTerm v -> Typed.TypedTerm (Model.Property v)
propertyWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Property"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL accessor for the body of hydra.pg.model.EdgeLabel
unEdgeLabel :: Typed.TypedTerm Model.EdgeLabel -> Typed.TypedTerm String
unEdgeLabel x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.pg.model.EdgeLabel")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.pg.model.PropertyKey
unPropertyKey :: Typed.TypedTerm Model.PropertyKey -> Typed.TypedTerm String
unPropertyKey x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.pg.model.PropertyKey")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.pg.model.VertexLabel
unVertexLabel :: Typed.TypedTerm Model.VertexLabel -> Typed.TypedTerm String
unVertexLabel x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.pg.model.VertexLabel")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.pg.model.Vertex
vertex :: Typed.TypedTerm Model.VertexLabel -> Typed.TypedTerm v -> Typed.TypedTerm (M.Map Model.PropertyKey v) -> Typed.TypedTerm (Model.Vertex v)
vertex label id properties =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Vertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Typed.unTypedTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Typed.unTypedTerm properties)}]}))
-- | DSL accessor for the id field of hydra.pg.model.Vertex
vertexId :: Typed.TypedTerm (Model.Vertex v) -> Typed.TypedTerm v
vertexId x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the label field of hydra.pg.model.Vertex
vertexLabel :: Typed.TypedTerm (Model.Vertex v) -> Typed.TypedTerm Model.VertexLabel
vertexLabel x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.pg.model.VertexLabel wrapper
vertexLabel2 :: Typed.TypedTerm String -> Typed.TypedTerm Model.VertexLabel
vertexLabel2 x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.model.VertexLabel"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL accessor for the properties field of hydra.pg.model.Vertex
vertexProperties :: Typed.TypedTerm (Model.Vertex v) -> Typed.TypedTerm (M.Map Model.PropertyKey v)
vertexProperties x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
        Core.projectionFieldName = (Core.Name "properties")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.pg.model.VertexType
vertexType :: Typed.TypedTerm Model.VertexLabel -> Typed.TypedTerm t -> Typed.TypedTerm [Model.PropertyType t] -> Typed.TypedTerm (Model.VertexType t)
vertexType label id properties =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.VertexType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Typed.unTypedTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Typed.unTypedTerm properties)}]}))
-- | DSL accessor for the id field of hydra.pg.model.VertexType
vertexTypeId :: Typed.TypedTerm (Model.VertexType t) -> Typed.TypedTerm t
vertexTypeId x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the label field of hydra.pg.model.VertexType
vertexTypeLabel :: Typed.TypedTerm (Model.VertexType t) -> Typed.TypedTerm Model.VertexLabel
vertexTypeLabel x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the properties field of hydra.pg.model.VertexType
vertexTypeProperties :: Typed.TypedTerm (Model.VertexType t) -> Typed.TypedTerm [Model.PropertyType t]
vertexTypeProperties x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
        Core.projectionFieldName = (Core.Name "properties")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the id field of hydra.pg.model.VertexType
vertexTypeWithId :: Typed.TypedTerm (Model.VertexType t) -> Typed.TypedTerm t -> Typed.TypedTerm (Model.VertexType t)
vertexTypeWithId original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.VertexType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the label field of hydra.pg.model.VertexType
vertexTypeWithLabel :: Typed.TypedTerm (Model.VertexType t) -> Typed.TypedTerm Model.VertexLabel -> Typed.TypedTerm (Model.VertexType t)
vertexTypeWithLabel original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.VertexType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the properties field of hydra.pg.model.VertexType
vertexTypeWithProperties :: Typed.TypedTerm (Model.VertexType t) -> Typed.TypedTerm [Model.PropertyType t] -> Typed.TypedTerm (Model.VertexType t)
vertexTypeWithProperties original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.VertexType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.pg.model.VertexWithAdjacentEdges
vertexWithAdjacentEdges :: Typed.TypedTerm (Model.Vertex v) -> Typed.TypedTerm [Model.AdjacentEdge v] -> Typed.TypedTerm [Model.AdjacentEdge v] -> Typed.TypedTerm (Model.VertexWithAdjacentEdges v)
vertexWithAdjacentEdges vertex ins outs =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Typed.unTypedTerm vertex)},
        Core.Field {
          Core.fieldName = (Core.Name "ins"),
          Core.fieldTerm = (Typed.unTypedTerm ins)},
        Core.Field {
          Core.fieldName = (Core.Name "outs"),
          Core.fieldTerm = (Typed.unTypedTerm outs)}]}))
-- | DSL accessor for the ins field of hydra.pg.model.VertexWithAdjacentEdges
vertexWithAdjacentEdgesIns :: Typed.TypedTerm (Model.VertexWithAdjacentEdges v) -> Typed.TypedTerm [Model.AdjacentEdge v]
vertexWithAdjacentEdgesIns x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
        Core.projectionFieldName = (Core.Name "ins")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the outs field of hydra.pg.model.VertexWithAdjacentEdges
vertexWithAdjacentEdgesOuts :: Typed.TypedTerm (Model.VertexWithAdjacentEdges v) -> Typed.TypedTerm [Model.AdjacentEdge v]
vertexWithAdjacentEdgesOuts x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
        Core.projectionFieldName = (Core.Name "outs")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the vertex field of hydra.pg.model.VertexWithAdjacentEdges
vertexWithAdjacentEdgesVertex :: Typed.TypedTerm (Model.VertexWithAdjacentEdges v) -> Typed.TypedTerm (Model.Vertex v)
vertexWithAdjacentEdgesVertex x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
        Core.projectionFieldName = (Core.Name "vertex")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the ins field of hydra.pg.model.VertexWithAdjacentEdges
vertexWithAdjacentEdgesWithIns :: Typed.TypedTerm (Model.VertexWithAdjacentEdges v) -> Typed.TypedTerm [Model.AdjacentEdge v] -> Typed.TypedTerm (Model.VertexWithAdjacentEdges v)
vertexWithAdjacentEdgesWithIns original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
              Core.projectionFieldName = (Core.Name "vertex")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ins"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "outs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
              Core.projectionFieldName = (Core.Name "outs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the outs field of hydra.pg.model.VertexWithAdjacentEdges
vertexWithAdjacentEdgesWithOuts :: Typed.TypedTerm (Model.VertexWithAdjacentEdges v) -> Typed.TypedTerm [Model.AdjacentEdge v] -> Typed.TypedTerm (Model.VertexWithAdjacentEdges v)
vertexWithAdjacentEdgesWithOuts original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
              Core.projectionFieldName = (Core.Name "vertex")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ins"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
              Core.projectionFieldName = (Core.Name "ins")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outs"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the vertex field of hydra.pg.model.VertexWithAdjacentEdges
vertexWithAdjacentEdgesWithVertex :: Typed.TypedTerm (Model.VertexWithAdjacentEdges v) -> Typed.TypedTerm (Model.Vertex v) -> Typed.TypedTerm (Model.VertexWithAdjacentEdges v)
vertexWithAdjacentEdgesWithVertex original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ins"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
              Core.projectionFieldName = (Core.Name "ins")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
              Core.projectionFieldName = (Core.Name "outs")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the id field of hydra.pg.model.Vertex
vertexWithId :: Typed.TypedTerm (Model.Vertex v) -> Typed.TypedTerm v -> Typed.TypedTerm (Model.Vertex v)
vertexWithId original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Vertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the label field of hydra.pg.model.Vertex
vertexWithLabel :: Typed.TypedTerm (Model.Vertex v) -> Typed.TypedTerm Model.VertexLabel -> Typed.TypedTerm (Model.Vertex v)
vertexWithLabel original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Vertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the properties field of hydra.pg.model.Vertex
vertexWithProperties :: Typed.TypedTerm (Model.Vertex v) -> Typed.TypedTerm (M.Map Model.PropertyKey v) -> Typed.TypedTerm (Model.Vertex v)
vertexWithProperties original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Vertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
