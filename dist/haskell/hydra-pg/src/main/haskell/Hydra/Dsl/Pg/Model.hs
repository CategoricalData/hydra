-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.pg.model

module Hydra.Dsl.Pg.Model where
import qualified Hydra.Core as Core
import qualified Hydra.Pg.Model as Model
import qualified Hydra.Typed as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
-- | DSL constructor for hydra.pg.model.AdjacentEdge
adjacentEdge :: Phantoms.TypedTerm Model.EdgeLabel -> Phantoms.TypedTerm v -> Phantoms.TypedTerm v -> Phantoms.TypedTerm (M.Map Model.PropertyKey v) -> Phantoms.TypedTerm (Model.AdjacentEdge v)
adjacentEdge label id vertex properties =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTypedTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Phantoms.unTypedTerm vertex)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTypedTerm properties)}]}))
-- | DSL accessor for the id field of hydra.pg.model.AdjacentEdge
adjacentEdgeId :: Phantoms.TypedTerm (Model.AdjacentEdge v) -> Phantoms.TypedTerm v
adjacentEdgeId x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the label field of hydra.pg.model.AdjacentEdge
adjacentEdgeLabel :: Phantoms.TypedTerm (Model.AdjacentEdge v) -> Phantoms.TypedTerm Model.EdgeLabel
adjacentEdgeLabel x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the properties field of hydra.pg.model.AdjacentEdge
adjacentEdgeProperties :: Phantoms.TypedTerm (Model.AdjacentEdge v) -> Phantoms.TypedTerm (M.Map Model.PropertyKey v)
adjacentEdgeProperties x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
        Core.projectionFieldName = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the vertex field of hydra.pg.model.AdjacentEdge
adjacentEdgeVertex :: Phantoms.TypedTerm (Model.AdjacentEdge v) -> Phantoms.TypedTerm v
adjacentEdgeVertex x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
        Core.projectionFieldName = (Core.Name "vertex")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the id field of hydra.pg.model.AdjacentEdge
adjacentEdgeWithId :: Phantoms.TypedTerm (Model.AdjacentEdge v) -> Phantoms.TypedTerm v -> Phantoms.TypedTerm (Model.AdjacentEdge v)
adjacentEdgeWithId original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "vertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the label field of hydra.pg.model.AdjacentEdge
adjacentEdgeWithLabel :: Phantoms.TypedTerm (Model.AdjacentEdge v) -> Phantoms.TypedTerm Model.EdgeLabel -> Phantoms.TypedTerm (Model.AdjacentEdge v)
adjacentEdgeWithLabel original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "vertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the properties field of hydra.pg.model.AdjacentEdge
adjacentEdgeWithProperties :: Phantoms.TypedTerm (Model.AdjacentEdge v) -> Phantoms.TypedTerm (M.Map Model.PropertyKey v) -> Phantoms.TypedTerm (Model.AdjacentEdge v)
adjacentEdgeWithProperties original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "vertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the vertex field of hydra.pg.model.AdjacentEdge
adjacentEdgeWithVertex :: Phantoms.TypedTerm (Model.AdjacentEdge v) -> Phantoms.TypedTerm v -> Phantoms.TypedTerm (Model.AdjacentEdge v)
adjacentEdgeWithVertex original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL injection for the both variant of hydra.pg.model.Direction
directionBoth :: Phantoms.TypedTerm Model.Direction
directionBoth =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Direction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "both"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the in variant of hydra.pg.model.Direction
directionIn :: Phantoms.TypedTerm Model.Direction
directionIn =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Direction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "in"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the out variant of hydra.pg.model.Direction
directionOut :: Phantoms.TypedTerm Model.Direction
directionOut =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Direction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "out"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the undirected variant of hydra.pg.model.Direction
directionUndirected :: Phantoms.TypedTerm Model.Direction
directionUndirected =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Direction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undirected"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.pg.model.Edge
edge :: Phantoms.TypedTerm Model.EdgeLabel -> Phantoms.TypedTerm v -> Phantoms.TypedTerm v -> Phantoms.TypedTerm v -> Phantoms.TypedTerm (M.Map Model.PropertyKey v) -> Phantoms.TypedTerm (Model.Edge v)
edge label id out in_ properties =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Edge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTypedTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Phantoms.unTypedTerm out)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Phantoms.unTypedTerm in_)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTypedTerm properties)}]}))
-- | DSL accessor for the id field of hydra.pg.model.Edge
edgeId :: Phantoms.TypedTerm (Model.Edge v) -> Phantoms.TypedTerm v
edgeId x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the in field of hydra.pg.model.Edge
edgeIn :: Phantoms.TypedTerm (Model.Edge v) -> Phantoms.TypedTerm v
edgeIn x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
        Core.projectionFieldName = (Core.Name "in")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the label field of hydra.pg.model.Edge
edgeLabel :: Phantoms.TypedTerm (Model.Edge v) -> Phantoms.TypedTerm Model.EdgeLabel
edgeLabel x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL constructor for the hydra.pg.model.EdgeLabel wrapper
edgeLabel2 :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Model.EdgeLabel
edgeLabel2 x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.model.EdgeLabel"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the out field of hydra.pg.model.Edge
edgeOut :: Phantoms.TypedTerm (Model.Edge v) -> Phantoms.TypedTerm v
edgeOut x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
        Core.projectionFieldName = (Core.Name "out")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the properties field of hydra.pg.model.Edge
edgeProperties :: Phantoms.TypedTerm (Model.Edge v) -> Phantoms.TypedTerm (M.Map Model.PropertyKey v)
edgeProperties x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
        Core.projectionFieldName = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL constructor for hydra.pg.model.EdgeType
edgeType :: Phantoms.TypedTerm Model.EdgeLabel -> Phantoms.TypedTerm t -> Phantoms.TypedTerm Model.VertexLabel -> Phantoms.TypedTerm Model.VertexLabel -> Phantoms.TypedTerm [Model.PropertyType t] -> Phantoms.TypedTerm (Model.EdgeType t)
edgeType label id out in_ properties =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.EdgeType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTypedTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Phantoms.unTypedTerm out)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Phantoms.unTypedTerm in_)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTypedTerm properties)}]}))
-- | DSL accessor for the id field of hydra.pg.model.EdgeType
edgeTypeId :: Phantoms.TypedTerm (Model.EdgeType t) -> Phantoms.TypedTerm t
edgeTypeId x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the in field of hydra.pg.model.EdgeType
edgeTypeIn :: Phantoms.TypedTerm (Model.EdgeType t) -> Phantoms.TypedTerm Model.VertexLabel
edgeTypeIn x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
        Core.projectionFieldName = (Core.Name "in")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the label field of hydra.pg.model.EdgeType
edgeTypeLabel :: Phantoms.TypedTerm (Model.EdgeType t) -> Phantoms.TypedTerm Model.EdgeLabel
edgeTypeLabel x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the out field of hydra.pg.model.EdgeType
edgeTypeOut :: Phantoms.TypedTerm (Model.EdgeType t) -> Phantoms.TypedTerm Model.VertexLabel
edgeTypeOut x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
        Core.projectionFieldName = (Core.Name "out")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the properties field of hydra.pg.model.EdgeType
edgeTypeProperties :: Phantoms.TypedTerm (Model.EdgeType t) -> Phantoms.TypedTerm [Model.PropertyType t]
edgeTypeProperties x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
        Core.projectionFieldName = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the id field of hydra.pg.model.EdgeType
edgeTypeWithId :: Phantoms.TypedTerm (Model.EdgeType t) -> Phantoms.TypedTerm t -> Phantoms.TypedTerm (Model.EdgeType t)
edgeTypeWithId original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.EdgeType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the in field of hydra.pg.model.EdgeType
edgeTypeWithIn :: Phantoms.TypedTerm (Model.EdgeType t) -> Phantoms.TypedTerm Model.VertexLabel -> Phantoms.TypedTerm (Model.EdgeType t)
edgeTypeWithIn original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.EdgeType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the label field of hydra.pg.model.EdgeType
edgeTypeWithLabel :: Phantoms.TypedTerm (Model.EdgeType t) -> Phantoms.TypedTerm Model.EdgeLabel -> Phantoms.TypedTerm (Model.EdgeType t)
edgeTypeWithLabel original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.EdgeType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the out field of hydra.pg.model.EdgeType
edgeTypeWithOut :: Phantoms.TypedTerm (Model.EdgeType t) -> Phantoms.TypedTerm Model.VertexLabel -> Phantoms.TypedTerm (Model.EdgeType t)
edgeTypeWithOut original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.EdgeType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the properties field of hydra.pg.model.EdgeType
edgeTypeWithProperties :: Phantoms.TypedTerm (Model.EdgeType t) -> Phantoms.TypedTerm [Model.PropertyType t] -> Phantoms.TypedTerm (Model.EdgeType t)
edgeTypeWithProperties original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.EdgeType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the id field of hydra.pg.model.Edge
edgeWithId :: Phantoms.TypedTerm (Model.Edge v) -> Phantoms.TypedTerm v -> Phantoms.TypedTerm (Model.Edge v)
edgeWithId original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Edge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the in field of hydra.pg.model.Edge
edgeWithIn :: Phantoms.TypedTerm (Model.Edge v) -> Phantoms.TypedTerm v -> Phantoms.TypedTerm (Model.Edge v)
edgeWithIn original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Edge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the label field of hydra.pg.model.Edge
edgeWithLabel :: Phantoms.TypedTerm (Model.Edge v) -> Phantoms.TypedTerm Model.EdgeLabel -> Phantoms.TypedTerm (Model.Edge v)
edgeWithLabel original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Edge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the out field of hydra.pg.model.Edge
edgeWithOut :: Phantoms.TypedTerm (Model.Edge v) -> Phantoms.TypedTerm v -> Phantoms.TypedTerm (Model.Edge v)
edgeWithOut original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Edge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the properties field of hydra.pg.model.Edge
edgeWithProperties :: Phantoms.TypedTerm (Model.Edge v) -> Phantoms.TypedTerm (M.Map Model.PropertyKey v) -> Phantoms.TypedTerm (Model.Edge v)
edgeWithProperties original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Edge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL injection for the edge variant of hydra.pg.model.Element
elementEdge :: Phantoms.TypedTerm (Model.Edge v) -> Phantoms.TypedTerm (Model.Element v)
elementEdge x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Element"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edge"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the edge variant of hydra.pg.model.ElementKind
elementKindEdge :: Phantoms.TypedTerm Model.ElementKind
elementKindEdge =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.ElementKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edge"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the vertex variant of hydra.pg.model.ElementKind
elementKindVertex :: Phantoms.TypedTerm Model.ElementKind
elementKindVertex =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.ElementKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vertex"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.pg.model.ElementTree
elementTree :: Phantoms.TypedTerm (Model.Element v) -> Phantoms.TypedTerm [Model.ElementTree v] -> Phantoms.TypedTerm (Model.ElementTree v)
elementTree self dependencies =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.ElementTree"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Phantoms.unTypedTerm self)},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Phantoms.unTypedTerm dependencies)}]}))
-- | DSL accessor for the dependencies field of hydra.pg.model.ElementTree
elementTreeDependencies :: Phantoms.TypedTerm (Model.ElementTree v) -> Phantoms.TypedTerm [Model.ElementTree v]
elementTreeDependencies x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTree"),
        Core.projectionFieldName = (Core.Name "dependencies")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the self field of hydra.pg.model.ElementTree
elementTreeSelf :: Phantoms.TypedTerm (Model.ElementTree v) -> Phantoms.TypedTerm (Model.Element v)
elementTreeSelf x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTree"),
        Core.projectionFieldName = (Core.Name "self")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the dependencies field of hydra.pg.model.ElementTree
elementTreeWithDependencies :: Phantoms.TypedTerm (Model.ElementTree v) -> Phantoms.TypedTerm [Model.ElementTree v] -> Phantoms.TypedTerm (Model.ElementTree v)
elementTreeWithDependencies original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.ElementTree"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTree"),
              Core.projectionFieldName = (Core.Name "self")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the self field of hydra.pg.model.ElementTree
elementTreeWithSelf :: Phantoms.TypedTerm (Model.ElementTree v) -> Phantoms.TypedTerm (Model.Element v) -> Phantoms.TypedTerm (Model.ElementTree v)
elementTreeWithSelf original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.ElementTree"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTree"),
              Core.projectionFieldName = (Core.Name "dependencies")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL injection for the edge variant of hydra.pg.model.ElementType
elementTypeEdge :: Phantoms.TypedTerm (Model.EdgeType t) -> Phantoms.TypedTerm (Model.ElementType t)
elementTypeEdge x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.ElementType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edge"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL constructor for hydra.pg.model.ElementTypeTree
elementTypeTree :: Phantoms.TypedTerm (Model.ElementType t) -> Phantoms.TypedTerm [Model.ElementTypeTree t] -> Phantoms.TypedTerm (Model.ElementTypeTree t)
elementTypeTree self dependencies =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.ElementTypeTree"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Phantoms.unTypedTerm self)},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Phantoms.unTypedTerm dependencies)}]}))
-- | DSL accessor for the dependencies field of hydra.pg.model.ElementTypeTree
elementTypeTreeDependencies :: Phantoms.TypedTerm (Model.ElementTypeTree t) -> Phantoms.TypedTerm [Model.ElementTypeTree t]
elementTypeTreeDependencies x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTypeTree"),
        Core.projectionFieldName = (Core.Name "dependencies")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the self field of hydra.pg.model.ElementTypeTree
elementTypeTreeSelf :: Phantoms.TypedTerm (Model.ElementTypeTree t) -> Phantoms.TypedTerm (Model.ElementType t)
elementTypeTreeSelf x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTypeTree"),
        Core.projectionFieldName = (Core.Name "self")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the dependencies field of hydra.pg.model.ElementTypeTree
elementTypeTreeWithDependencies :: Phantoms.TypedTerm (Model.ElementTypeTree t) -> Phantoms.TypedTerm [Model.ElementTypeTree t] -> Phantoms.TypedTerm (Model.ElementTypeTree t)
elementTypeTreeWithDependencies original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.ElementTypeTree"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTypeTree"),
              Core.projectionFieldName = (Core.Name "self")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the self field of hydra.pg.model.ElementTypeTree
elementTypeTreeWithSelf :: Phantoms.TypedTerm (Model.ElementTypeTree t) -> Phantoms.TypedTerm (Model.ElementType t) -> Phantoms.TypedTerm (Model.ElementTypeTree t)
elementTypeTreeWithSelf original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.ElementTypeTree"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTypeTree"),
              Core.projectionFieldName = (Core.Name "dependencies")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL injection for the vertex variant of hydra.pg.model.ElementType
elementTypeVertex :: Phantoms.TypedTerm (Model.VertexType t) -> Phantoms.TypedTerm (Model.ElementType t)
elementTypeVertex x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.ElementType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vertex"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the vertex variant of hydra.pg.model.Element
elementVertex :: Phantoms.TypedTerm (Model.Vertex v) -> Phantoms.TypedTerm (Model.Element v)
elementVertex x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Element"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vertex"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL constructor for hydra.pg.model.Graph
graph :: Ord v => (Phantoms.TypedTerm (M.Map v (Model.Vertex v)) -> Phantoms.TypedTerm (M.Map v (Model.Edge v)) -> Phantoms.TypedTerm (Model.Graph v))
graph vertices edges =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = (Phantoms.unTypedTerm vertices)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Phantoms.unTypedTerm edges)}]}))
-- | DSL accessor for the edges field of hydra.pg.model.Graph
graphEdges :: Ord v => (Phantoms.TypedTerm (Model.Graph v) -> Phantoms.TypedTerm (M.Map v (Model.Edge v)))
graphEdges x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Graph"),
        Core.projectionFieldName = (Core.Name "edges")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL constructor for hydra.pg.model.GraphSchema
graphSchema :: Phantoms.TypedTerm (M.Map Model.VertexLabel (Model.VertexType t)) -> Phantoms.TypedTerm (M.Map Model.EdgeLabel (Model.EdgeType t)) -> Phantoms.TypedTerm (Model.GraphSchema t)
graphSchema vertices edges =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.GraphSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = (Phantoms.unTypedTerm vertices)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Phantoms.unTypedTerm edges)}]}))
-- | DSL accessor for the edges field of hydra.pg.model.GraphSchema
graphSchemaEdges :: Phantoms.TypedTerm (Model.GraphSchema t) -> Phantoms.TypedTerm (M.Map Model.EdgeLabel (Model.EdgeType t))
graphSchemaEdges x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.GraphSchema"),
        Core.projectionFieldName = (Core.Name "edges")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the vertices field of hydra.pg.model.GraphSchema
graphSchemaVertices :: Phantoms.TypedTerm (Model.GraphSchema t) -> Phantoms.TypedTerm (M.Map Model.VertexLabel (Model.VertexType t))
graphSchemaVertices x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.GraphSchema"),
        Core.projectionFieldName = (Core.Name "vertices")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the edges field of hydra.pg.model.GraphSchema
graphSchemaWithEdges :: Phantoms.TypedTerm (Model.GraphSchema t) -> Phantoms.TypedTerm (M.Map Model.EdgeLabel (Model.EdgeType t)) -> Phantoms.TypedTerm (Model.GraphSchema t)
graphSchemaWithEdges original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.GraphSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.GraphSchema"),
              Core.projectionFieldName = (Core.Name "vertices")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the vertices field of hydra.pg.model.GraphSchema
graphSchemaWithVertices :: Phantoms.TypedTerm (Model.GraphSchema t) -> Phantoms.TypedTerm (M.Map Model.VertexLabel (Model.VertexType t)) -> Phantoms.TypedTerm (Model.GraphSchema t)
graphSchemaWithVertices original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.GraphSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.GraphSchema"),
              Core.projectionFieldName = (Core.Name "edges")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL accessor for the vertices field of hydra.pg.model.Graph
graphVertices :: Ord v => (Phantoms.TypedTerm (Model.Graph v) -> Phantoms.TypedTerm (M.Map v (Model.Vertex v)))
graphVertices x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Graph"),
        Core.projectionFieldName = (Core.Name "vertices")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the edges field of hydra.pg.model.Graph
graphWithEdges :: Ord v => (Phantoms.TypedTerm (Model.Graph v) -> Phantoms.TypedTerm (M.Map v (Model.Edge v)) -> Phantoms.TypedTerm (Model.Graph v))
graphWithEdges original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Graph"),
              Core.projectionFieldName = (Core.Name "vertices")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the vertices field of hydra.pg.model.Graph
graphWithVertices :: Ord v => (Phantoms.TypedTerm (Model.Graph v) -> Phantoms.TypedTerm (M.Map v (Model.Vertex v)) -> Phantoms.TypedTerm (Model.Graph v))
graphWithVertices original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Graph"),
              Core.projectionFieldName = (Core.Name "edges")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL injection for the edge variant of hydra.pg.model.Label
labelEdge :: Phantoms.TypedTerm Model.EdgeLabel -> Phantoms.TypedTerm Model.Label
labelEdge x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Label"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edge"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL injection for the vertex variant of hydra.pg.model.Label
labelVertex :: Phantoms.TypedTerm Model.VertexLabel -> Phantoms.TypedTerm Model.Label
labelVertex x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Label"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vertex"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))
-- | DSL constructor for hydra.pg.model.LazyGraph
lazyGraph :: Phantoms.TypedTerm [Model.Vertex v] -> Phantoms.TypedTerm [Model.Edge v] -> Phantoms.TypedTerm (Model.LazyGraph v)
lazyGraph vertices edges =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.LazyGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = (Phantoms.unTypedTerm vertices)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Phantoms.unTypedTerm edges)}]}))
-- | DSL accessor for the edges field of hydra.pg.model.LazyGraph
lazyGraphEdges :: Phantoms.TypedTerm (Model.LazyGraph v) -> Phantoms.TypedTerm [Model.Edge v]
lazyGraphEdges x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.LazyGraph"),
        Core.projectionFieldName = (Core.Name "edges")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the vertices field of hydra.pg.model.LazyGraph
lazyGraphVertices :: Phantoms.TypedTerm (Model.LazyGraph v) -> Phantoms.TypedTerm [Model.Vertex v]
lazyGraphVertices x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.LazyGraph"),
        Core.projectionFieldName = (Core.Name "vertices")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the edges field of hydra.pg.model.LazyGraph
lazyGraphWithEdges :: Phantoms.TypedTerm (Model.LazyGraph v) -> Phantoms.TypedTerm [Model.Edge v] -> Phantoms.TypedTerm (Model.LazyGraph v)
lazyGraphWithEdges original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.LazyGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.LazyGraph"),
              Core.projectionFieldName = (Core.Name "vertices")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the vertices field of hydra.pg.model.LazyGraph
lazyGraphWithVertices :: Phantoms.TypedTerm (Model.LazyGraph v) -> Phantoms.TypedTerm [Model.Vertex v] -> Phantoms.TypedTerm (Model.LazyGraph v)
lazyGraphWithVertices original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.LazyGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.LazyGraph"),
              Core.projectionFieldName = (Core.Name "edges")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.pg.model.Property
property :: Phantoms.TypedTerm Model.PropertyKey -> Phantoms.TypedTerm v -> Phantoms.TypedTerm (Model.Property v)
property key value =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTypedTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm value)}]}))
-- | DSL accessor for the key field of hydra.pg.model.Property
propertyKey :: Phantoms.TypedTerm (Model.Property v) -> Phantoms.TypedTerm Model.PropertyKey
propertyKey x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Property"),
        Core.projectionFieldName = (Core.Name "key")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL constructor for the hydra.pg.model.PropertyKey wrapper
propertyKey2 :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Model.PropertyKey
propertyKey2 x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.model.PropertyKey"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))
-- | DSL constructor for hydra.pg.model.PropertyType
propertyType :: Phantoms.TypedTerm Model.PropertyKey -> Phantoms.TypedTerm t -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm (Model.PropertyType t)
propertyType key value required =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.PropertyType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTypedTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "required"),
          Core.fieldTerm = (Phantoms.unTypedTerm required)}]}))
-- | DSL accessor for the key field of hydra.pg.model.PropertyType
propertyTypeKey :: Phantoms.TypedTerm (Model.PropertyType t) -> Phantoms.TypedTerm Model.PropertyKey
propertyTypeKey x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
        Core.projectionFieldName = (Core.Name "key")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the required field of hydra.pg.model.PropertyType
propertyTypeRequired :: Phantoms.TypedTerm (Model.PropertyType t) -> Phantoms.TypedTerm Bool
propertyTypeRequired x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
        Core.projectionFieldName = (Core.Name "required")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.pg.model.PropertyType
propertyTypeValue :: Phantoms.TypedTerm (Model.PropertyType t) -> Phantoms.TypedTerm t
propertyTypeValue x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the key field of hydra.pg.model.PropertyType
propertyTypeWithKey :: Phantoms.TypedTerm (Model.PropertyType t) -> Phantoms.TypedTerm Model.PropertyKey -> Phantoms.TypedTerm (Model.PropertyType t)
propertyTypeWithKey original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.PropertyType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "required"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
              Core.projectionFieldName = (Core.Name "required")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the required field of hydra.pg.model.PropertyType
propertyTypeWithRequired :: Phantoms.TypedTerm (Model.PropertyType t) -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm (Model.PropertyType t)
propertyTypeWithRequired original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.PropertyType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "required"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the value field of hydra.pg.model.PropertyType
propertyTypeWithValue :: Phantoms.TypedTerm (Model.PropertyType t) -> Phantoms.TypedTerm t -> Phantoms.TypedTerm (Model.PropertyType t)
propertyTypeWithValue original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.PropertyType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "required"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
              Core.projectionFieldName = (Core.Name "required")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL accessor for the value field of hydra.pg.model.Property
propertyValue :: Phantoms.TypedTerm (Model.Property v) -> Phantoms.TypedTerm v
propertyValue x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Property"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the key field of hydra.pg.model.Property
propertyWithKey :: Phantoms.TypedTerm (Model.Property v) -> Phantoms.TypedTerm Model.PropertyKey -> Phantoms.TypedTerm (Model.Property v)
propertyWithKey original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Property"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the value field of hydra.pg.model.Property
propertyWithValue :: Phantoms.TypedTerm (Model.Property v) -> Phantoms.TypedTerm v -> Phantoms.TypedTerm (Model.Property v)
propertyWithValue original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Property"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL accessor for the body of hydra.pg.model.EdgeLabel
unEdgeLabel :: Phantoms.TypedTerm Model.EdgeLabel -> Phantoms.TypedTerm String
unEdgeLabel x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.pg.model.EdgeLabel")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.pg.model.PropertyKey
unPropertyKey :: Phantoms.TypedTerm Model.PropertyKey -> Phantoms.TypedTerm String
unPropertyKey x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.pg.model.PropertyKey")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.pg.model.VertexLabel
unVertexLabel :: Phantoms.TypedTerm Model.VertexLabel -> Phantoms.TypedTerm String
unVertexLabel x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.pg.model.VertexLabel")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL constructor for hydra.pg.model.Vertex
vertex :: Phantoms.TypedTerm Model.VertexLabel -> Phantoms.TypedTerm v -> Phantoms.TypedTerm (M.Map Model.PropertyKey v) -> Phantoms.TypedTerm (Model.Vertex v)
vertex label id properties =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Vertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTypedTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTypedTerm properties)}]}))
-- | DSL accessor for the id field of hydra.pg.model.Vertex
vertexId :: Phantoms.TypedTerm (Model.Vertex v) -> Phantoms.TypedTerm v
vertexId x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the label field of hydra.pg.model.Vertex
vertexLabel :: Phantoms.TypedTerm (Model.Vertex v) -> Phantoms.TypedTerm Model.VertexLabel
vertexLabel x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL constructor for the hydra.pg.model.VertexLabel wrapper
vertexLabel2 :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Model.VertexLabel
vertexLabel2 x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.model.VertexLabel"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the properties field of hydra.pg.model.Vertex
vertexProperties :: Phantoms.TypedTerm (Model.Vertex v) -> Phantoms.TypedTerm (M.Map Model.PropertyKey v)
vertexProperties x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
        Core.projectionFieldName = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL constructor for hydra.pg.model.VertexType
vertexType :: Phantoms.TypedTerm Model.VertexLabel -> Phantoms.TypedTerm t -> Phantoms.TypedTerm [Model.PropertyType t] -> Phantoms.TypedTerm (Model.VertexType t)
vertexType label id properties =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.VertexType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTypedTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTypedTerm properties)}]}))
-- | DSL accessor for the id field of hydra.pg.model.VertexType
vertexTypeId :: Phantoms.TypedTerm (Model.VertexType t) -> Phantoms.TypedTerm t
vertexTypeId x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the label field of hydra.pg.model.VertexType
vertexTypeLabel :: Phantoms.TypedTerm (Model.VertexType t) -> Phantoms.TypedTerm Model.VertexLabel
vertexTypeLabel x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the properties field of hydra.pg.model.VertexType
vertexTypeProperties :: Phantoms.TypedTerm (Model.VertexType t) -> Phantoms.TypedTerm [Model.PropertyType t]
vertexTypeProperties x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
        Core.projectionFieldName = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the id field of hydra.pg.model.VertexType
vertexTypeWithId :: Phantoms.TypedTerm (Model.VertexType t) -> Phantoms.TypedTerm t -> Phantoms.TypedTerm (Model.VertexType t)
vertexTypeWithId original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.VertexType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the label field of hydra.pg.model.VertexType
vertexTypeWithLabel :: Phantoms.TypedTerm (Model.VertexType t) -> Phantoms.TypedTerm Model.VertexLabel -> Phantoms.TypedTerm (Model.VertexType t)
vertexTypeWithLabel original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.VertexType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the properties field of hydra.pg.model.VertexType
vertexTypeWithProperties :: Phantoms.TypedTerm (Model.VertexType t) -> Phantoms.TypedTerm [Model.PropertyType t] -> Phantoms.TypedTerm (Model.VertexType t)
vertexTypeWithProperties original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.VertexType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.pg.model.VertexWithAdjacentEdges
vertexWithAdjacentEdges :: Phantoms.TypedTerm (Model.Vertex v) -> Phantoms.TypedTerm [Model.AdjacentEdge v] -> Phantoms.TypedTerm [Model.AdjacentEdge v] -> Phantoms.TypedTerm (Model.VertexWithAdjacentEdges v)
vertexWithAdjacentEdges vertex ins outs =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Phantoms.unTypedTerm vertex)},
        Core.Field {
          Core.fieldName = (Core.Name "ins"),
          Core.fieldTerm = (Phantoms.unTypedTerm ins)},
        Core.Field {
          Core.fieldName = (Core.Name "outs"),
          Core.fieldTerm = (Phantoms.unTypedTerm outs)}]}))
-- | DSL accessor for the ins field of hydra.pg.model.VertexWithAdjacentEdges
vertexWithAdjacentEdgesIns :: Phantoms.TypedTerm (Model.VertexWithAdjacentEdges v) -> Phantoms.TypedTerm [Model.AdjacentEdge v]
vertexWithAdjacentEdgesIns x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
        Core.projectionFieldName = (Core.Name "ins")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the outs field of hydra.pg.model.VertexWithAdjacentEdges
vertexWithAdjacentEdgesOuts :: Phantoms.TypedTerm (Model.VertexWithAdjacentEdges v) -> Phantoms.TypedTerm [Model.AdjacentEdge v]
vertexWithAdjacentEdgesOuts x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
        Core.projectionFieldName = (Core.Name "outs")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL accessor for the vertex field of hydra.pg.model.VertexWithAdjacentEdges
vertexWithAdjacentEdgesVertex :: Phantoms.TypedTerm (Model.VertexWithAdjacentEdges v) -> Phantoms.TypedTerm (Model.Vertex v)
vertexWithAdjacentEdgesVertex x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
        Core.projectionFieldName = (Core.Name "vertex")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
-- | DSL updater for the ins field of hydra.pg.model.VertexWithAdjacentEdges
vertexWithAdjacentEdgesWithIns :: Phantoms.TypedTerm (Model.VertexWithAdjacentEdges v) -> Phantoms.TypedTerm [Model.AdjacentEdge v] -> Phantoms.TypedTerm (Model.VertexWithAdjacentEdges v)
vertexWithAdjacentEdgesWithIns original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
              Core.projectionFieldName = (Core.Name "vertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ins"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "outs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
              Core.projectionFieldName = (Core.Name "outs")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the outs field of hydra.pg.model.VertexWithAdjacentEdges
vertexWithAdjacentEdgesWithOuts :: Phantoms.TypedTerm (Model.VertexWithAdjacentEdges v) -> Phantoms.TypedTerm [Model.AdjacentEdge v] -> Phantoms.TypedTerm (Model.VertexWithAdjacentEdges v)
vertexWithAdjacentEdgesWithOuts original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
              Core.projectionFieldName = (Core.Name "vertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ins"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
              Core.projectionFieldName = (Core.Name "ins")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outs"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
-- | DSL updater for the vertex field of hydra.pg.model.VertexWithAdjacentEdges
vertexWithAdjacentEdgesWithVertex :: Phantoms.TypedTerm (Model.VertexWithAdjacentEdges v) -> Phantoms.TypedTerm (Model.Vertex v) -> Phantoms.TypedTerm (Model.VertexWithAdjacentEdges v)
vertexWithAdjacentEdgesWithVertex original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ins"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
              Core.projectionFieldName = (Core.Name "ins")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
              Core.projectionFieldName = (Core.Name "outs")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the id field of hydra.pg.model.Vertex
vertexWithId :: Phantoms.TypedTerm (Model.Vertex v) -> Phantoms.TypedTerm v -> Phantoms.TypedTerm (Model.Vertex v)
vertexWithId original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Vertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the label field of hydra.pg.model.Vertex
vertexWithLabel :: Phantoms.TypedTerm (Model.Vertex v) -> Phantoms.TypedTerm Model.VertexLabel -> Phantoms.TypedTerm (Model.Vertex v)
vertexWithLabel original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Vertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
-- | DSL updater for the properties field of hydra.pg.model.Vertex
vertexWithProperties :: Phantoms.TypedTerm (Model.Vertex v) -> Phantoms.TypedTerm (M.Map Model.PropertyKey v) -> Phantoms.TypedTerm (Model.Vertex v)
vertexWithProperties original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Vertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
