-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.pg.model

module Hydra.Dsl.Pg.Model where
import qualified Hydra.Core as Core
import qualified Hydra.Pg.Model as Model
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
-- | DSL constructor for hydra.pg.model.AdjacentEdge
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
-- | DSL accessor for the id field of hydra.pg.model.AdjacentEdge
adjacentEdgeId :: Phantoms.TTerm (Model.AdjacentEdge v) -> Phantoms.TTerm v
adjacentEdgeId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the label field of hydra.pg.model.AdjacentEdge
adjacentEdgeLabel :: Phantoms.TTerm (Model.AdjacentEdge v) -> Phantoms.TTerm Model.EdgeLabel
adjacentEdgeLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the properties field of hydra.pg.model.AdjacentEdge
adjacentEdgeProperties :: Phantoms.TTerm (Model.AdjacentEdge v) -> Phantoms.TTerm (M.Map Model.PropertyKey v)
adjacentEdgeProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
        Core.projectionFieldName = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the vertex field of hydra.pg.model.AdjacentEdge
adjacentEdgeVertex :: Phantoms.TTerm (Model.AdjacentEdge v) -> Phantoms.TTerm v
adjacentEdgeVertex x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
        Core.projectionFieldName = (Core.Name "vertex")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the id field of hydra.pg.model.AdjacentEdge
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
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "vertex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the label field of hydra.pg.model.AdjacentEdge
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
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "vertex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the properties field of hydra.pg.model.AdjacentEdge
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
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "vertex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the vertex field of hydra.pg.model.AdjacentEdge
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
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the both variant of hydra.pg.model.Direction
directionBoth :: Phantoms.TTerm Model.Direction
directionBoth =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Direction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "both"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the in variant of hydra.pg.model.Direction
directionIn :: Phantoms.TTerm Model.Direction
directionIn =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Direction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "in"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the out variant of hydra.pg.model.Direction
directionOut :: Phantoms.TTerm Model.Direction
directionOut =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Direction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "out"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the undirected variant of hydra.pg.model.Direction
directionUndirected :: Phantoms.TTerm Model.Direction
directionUndirected =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Direction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undirected"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.pg.model.Edge
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
-- | DSL accessor for the id field of hydra.pg.model.Edge
edgeId :: Phantoms.TTerm (Model.Edge v) -> Phantoms.TTerm v
edgeId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the in field of hydra.pg.model.Edge
edgeIn :: Phantoms.TTerm (Model.Edge v) -> Phantoms.TTerm v
edgeIn x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
        Core.projectionFieldName = (Core.Name "in")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the label field of hydra.pg.model.Edge
edgeLabel :: Phantoms.TTerm (Model.Edge v) -> Phantoms.TTerm Model.EdgeLabel
edgeLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.pg.model.EdgeLabel wrapper
edgeLabel2 :: Phantoms.TTerm String -> Phantoms.TTerm Model.EdgeLabel
edgeLabel2 x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.model.EdgeLabel"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL accessor for the out field of hydra.pg.model.Edge
edgeOut :: Phantoms.TTerm (Model.Edge v) -> Phantoms.TTerm v
edgeOut x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
        Core.projectionFieldName = (Core.Name "out")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the properties field of hydra.pg.model.Edge
edgeProperties :: Phantoms.TTerm (Model.Edge v) -> Phantoms.TTerm (M.Map Model.PropertyKey v)
edgeProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
        Core.projectionFieldName = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.pg.model.EdgeType
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
-- | DSL accessor for the id field of hydra.pg.model.EdgeType
edgeTypeId :: Phantoms.TTerm (Model.EdgeType t) -> Phantoms.TTerm t
edgeTypeId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the in field of hydra.pg.model.EdgeType
edgeTypeIn :: Phantoms.TTerm (Model.EdgeType t) -> Phantoms.TTerm Model.VertexLabel
edgeTypeIn x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
        Core.projectionFieldName = (Core.Name "in")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the label field of hydra.pg.model.EdgeType
edgeTypeLabel :: Phantoms.TTerm (Model.EdgeType t) -> Phantoms.TTerm Model.EdgeLabel
edgeTypeLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the out field of hydra.pg.model.EdgeType
edgeTypeOut :: Phantoms.TTerm (Model.EdgeType t) -> Phantoms.TTerm Model.VertexLabel
edgeTypeOut x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
        Core.projectionFieldName = (Core.Name "out")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the properties field of hydra.pg.model.EdgeType
edgeTypeProperties :: Phantoms.TTerm (Model.EdgeType t) -> Phantoms.TTerm [Model.PropertyType t]
edgeTypeProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
        Core.projectionFieldName = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the id field of hydra.pg.model.EdgeType
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
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the in field of hydra.pg.model.EdgeType
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
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the label field of hydra.pg.model.EdgeType
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
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the out field of hydra.pg.model.EdgeType
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
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the properties field of hydra.pg.model.EdgeType
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
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.EdgeType"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the id field of hydra.pg.model.Edge
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
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the in field of hydra.pg.model.Edge
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
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the label field of hydra.pg.model.Edge
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
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the out field of hydra.pg.model.Edge
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
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the properties field of hydra.pg.model.Edge
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
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Edge"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL injection for the edge variant of hydra.pg.model.Element
elementEdge :: Phantoms.TTerm (Model.Edge v) -> Phantoms.TTerm (Model.Element v)
elementEdge x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Element"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edge"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the edge variant of hydra.pg.model.ElementKind
elementKindEdge :: Phantoms.TTerm Model.ElementKind
elementKindEdge =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.ElementKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edge"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the vertex variant of hydra.pg.model.ElementKind
elementKindVertex :: Phantoms.TTerm Model.ElementKind
elementKindVertex =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.ElementKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vertex"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.pg.model.ElementTree
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
-- | DSL accessor for the dependencies field of hydra.pg.model.ElementTree
elementTreeDependencies :: Phantoms.TTerm (Model.ElementTree v) -> Phantoms.TTerm [Model.ElementTree v]
elementTreeDependencies x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTree"),
        Core.projectionFieldName = (Core.Name "dependencies")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the self field of hydra.pg.model.ElementTree
elementTreeSelf :: Phantoms.TTerm (Model.ElementTree v) -> Phantoms.TTerm (Model.Element v)
elementTreeSelf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTree"),
        Core.projectionFieldName = (Core.Name "self")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the dependencies field of hydra.pg.model.ElementTree
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
              Core.projectionFieldName = (Core.Name "self")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the self field of hydra.pg.model.ElementTree
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
              Core.projectionFieldName = (Core.Name "dependencies")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the edge variant of hydra.pg.model.ElementType
elementTypeEdge :: Phantoms.TTerm (Model.EdgeType t) -> Phantoms.TTerm (Model.ElementType t)
elementTypeEdge x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.ElementType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edge"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.pg.model.ElementTypeTree
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
-- | DSL accessor for the dependencies field of hydra.pg.model.ElementTypeTree
elementTypeTreeDependencies :: Phantoms.TTerm (Model.ElementTypeTree t) -> Phantoms.TTerm [Model.ElementTypeTree t]
elementTypeTreeDependencies x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTypeTree"),
        Core.projectionFieldName = (Core.Name "dependencies")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the self field of hydra.pg.model.ElementTypeTree
elementTypeTreeSelf :: Phantoms.TTerm (Model.ElementTypeTree t) -> Phantoms.TTerm (Model.ElementType t)
elementTypeTreeSelf x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.ElementTypeTree"),
        Core.projectionFieldName = (Core.Name "self")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the dependencies field of hydra.pg.model.ElementTypeTree
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
              Core.projectionFieldName = (Core.Name "self")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the self field of hydra.pg.model.ElementTypeTree
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
              Core.projectionFieldName = (Core.Name "dependencies")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the vertex variant of hydra.pg.model.ElementType
elementTypeVertex :: Phantoms.TTerm (Model.VertexType t) -> Phantoms.TTerm (Model.ElementType t)
elementTypeVertex x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.ElementType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vertex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the vertex variant of hydra.pg.model.Element
elementVertex :: Phantoms.TTerm (Model.Vertex v) -> Phantoms.TTerm (Model.Element v)
elementVertex x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Element"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vertex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.pg.model.Graph
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
-- | DSL accessor for the edges field of hydra.pg.model.Graph
graphEdges :: Ord v => (Phantoms.TTerm (Model.Graph v) -> Phantoms.TTerm (M.Map v (Model.Edge v)))
graphEdges x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Graph"),
        Core.projectionFieldName = (Core.Name "edges")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.pg.model.GraphSchema
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
-- | DSL accessor for the edges field of hydra.pg.model.GraphSchema
graphSchemaEdges :: Phantoms.TTerm (Model.GraphSchema t) -> Phantoms.TTerm (M.Map Model.EdgeLabel (Model.EdgeType t))
graphSchemaEdges x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.GraphSchema"),
        Core.projectionFieldName = (Core.Name "edges")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the vertices field of hydra.pg.model.GraphSchema
graphSchemaVertices :: Phantoms.TTerm (Model.GraphSchema t) -> Phantoms.TTerm (M.Map Model.VertexLabel (Model.VertexType t))
graphSchemaVertices x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.GraphSchema"),
        Core.projectionFieldName = (Core.Name "vertices")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the edges field of hydra.pg.model.GraphSchema
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
              Core.projectionFieldName = (Core.Name "vertices")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the vertices field of hydra.pg.model.GraphSchema
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
              Core.projectionFieldName = (Core.Name "edges")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL accessor for the vertices field of hydra.pg.model.Graph
graphVertices :: Ord v => (Phantoms.TTerm (Model.Graph v) -> Phantoms.TTerm (M.Map v (Model.Vertex v)))
graphVertices x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Graph"),
        Core.projectionFieldName = (Core.Name "vertices")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the edges field of hydra.pg.model.Graph
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
              Core.projectionFieldName = (Core.Name "vertices")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the vertices field of hydra.pg.model.Graph
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
              Core.projectionFieldName = (Core.Name "edges")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the edge variant of hydra.pg.model.Label
labelEdge :: Phantoms.TTerm Model.EdgeLabel -> Phantoms.TTerm Model.Label
labelEdge x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Label"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edge"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the vertex variant of hydra.pg.model.Label
labelVertex :: Phantoms.TTerm Model.VertexLabel -> Phantoms.TTerm Model.Label
labelVertex x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.model.Label"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vertex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for hydra.pg.model.LazyGraph
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
-- | DSL accessor for the edges field of hydra.pg.model.LazyGraph
lazyGraphEdges :: Phantoms.TTerm (Model.LazyGraph v) -> Phantoms.TTerm [Model.Edge v]
lazyGraphEdges x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.LazyGraph"),
        Core.projectionFieldName = (Core.Name "edges")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the vertices field of hydra.pg.model.LazyGraph
lazyGraphVertices :: Phantoms.TTerm (Model.LazyGraph v) -> Phantoms.TTerm [Model.Vertex v]
lazyGraphVertices x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.LazyGraph"),
        Core.projectionFieldName = (Core.Name "vertices")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the edges field of hydra.pg.model.LazyGraph
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
              Core.projectionFieldName = (Core.Name "vertices")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the vertices field of hydra.pg.model.LazyGraph
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
              Core.projectionFieldName = (Core.Name "edges")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.pg.model.Property
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
-- | DSL accessor for the key field of hydra.pg.model.Property
propertyKey :: Phantoms.TTerm (Model.Property v) -> Phantoms.TTerm Model.PropertyKey
propertyKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Property"),
        Core.projectionFieldName = (Core.Name "key")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.pg.model.PropertyKey wrapper
propertyKey2 :: Phantoms.TTerm String -> Phantoms.TTerm Model.PropertyKey
propertyKey2 x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.model.PropertyKey"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.pg.model.PropertyType
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
-- | DSL accessor for the key field of hydra.pg.model.PropertyType
propertyTypeKey :: Phantoms.TTerm (Model.PropertyType t) -> Phantoms.TTerm Model.PropertyKey
propertyTypeKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
        Core.projectionFieldName = (Core.Name "key")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the required field of hydra.pg.model.PropertyType
propertyTypeRequired :: Phantoms.TTerm (Model.PropertyType t) -> Phantoms.TTerm Bool
propertyTypeRequired x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
        Core.projectionFieldName = (Core.Name "required")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the value field of hydra.pg.model.PropertyType
propertyTypeValue :: Phantoms.TTerm (Model.PropertyType t) -> Phantoms.TTerm t
propertyTypeValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the key field of hydra.pg.model.PropertyType
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
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "required"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
              Core.projectionFieldName = (Core.Name "required")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the required field of hydra.pg.model.PropertyType
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
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "required"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the value field of hydra.pg.model.PropertyType
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
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "required"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.PropertyType"),
              Core.projectionFieldName = (Core.Name "required")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL accessor for the value field of hydra.pg.model.Property
propertyValue :: Phantoms.TTerm (Model.Property v) -> Phantoms.TTerm v
propertyValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Property"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the key field of hydra.pg.model.Property
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
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the value field of hydra.pg.model.Property
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
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL accessor for the body of hydra.pg.model.EdgeLabel
unEdgeLabel :: Phantoms.TTerm Model.EdgeLabel -> Phantoms.TTerm String
unEdgeLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.pg.model.EdgeLabel")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.pg.model.PropertyKey
unPropertyKey :: Phantoms.TTerm Model.PropertyKey -> Phantoms.TTerm String
unPropertyKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.pg.model.PropertyKey")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.pg.model.VertexLabel
unVertexLabel :: Phantoms.TTerm Model.VertexLabel -> Phantoms.TTerm String
unVertexLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.pg.model.VertexLabel")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.pg.model.Vertex
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
-- | DSL accessor for the id field of hydra.pg.model.Vertex
vertexId :: Phantoms.TTerm (Model.Vertex v) -> Phantoms.TTerm v
vertexId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the label field of hydra.pg.model.Vertex
vertexLabel :: Phantoms.TTerm (Model.Vertex v) -> Phantoms.TTerm Model.VertexLabel
vertexLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.pg.model.VertexLabel wrapper
vertexLabel2 :: Phantoms.TTerm String -> Phantoms.TTerm Model.VertexLabel
vertexLabel2 x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.model.VertexLabel"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL accessor for the properties field of hydra.pg.model.Vertex
vertexProperties :: Phantoms.TTerm (Model.Vertex v) -> Phantoms.TTerm (M.Map Model.PropertyKey v)
vertexProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
        Core.projectionFieldName = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.pg.model.VertexType
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
-- | DSL accessor for the id field of hydra.pg.model.VertexType
vertexTypeId :: Phantoms.TTerm (Model.VertexType t) -> Phantoms.TTerm t
vertexTypeId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the label field of hydra.pg.model.VertexType
vertexTypeLabel :: Phantoms.TTerm (Model.VertexType t) -> Phantoms.TTerm Model.VertexLabel
vertexTypeLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the properties field of hydra.pg.model.VertexType
vertexTypeProperties :: Phantoms.TTerm (Model.VertexType t) -> Phantoms.TTerm [Model.PropertyType t]
vertexTypeProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
        Core.projectionFieldName = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the id field of hydra.pg.model.VertexType
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
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the label field of hydra.pg.model.VertexType
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
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the properties field of hydra.pg.model.VertexType
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
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexType"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.pg.model.VertexWithAdjacentEdges
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
-- | DSL accessor for the ins field of hydra.pg.model.VertexWithAdjacentEdges
vertexWithAdjacentEdgesIns :: Phantoms.TTerm (Model.VertexWithAdjacentEdges v) -> Phantoms.TTerm [Model.AdjacentEdge v]
vertexWithAdjacentEdgesIns x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
        Core.projectionFieldName = (Core.Name "ins")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the outs field of hydra.pg.model.VertexWithAdjacentEdges
vertexWithAdjacentEdgesOuts :: Phantoms.TTerm (Model.VertexWithAdjacentEdges v) -> Phantoms.TTerm [Model.AdjacentEdge v]
vertexWithAdjacentEdgesOuts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
        Core.projectionFieldName = (Core.Name "outs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the vertex field of hydra.pg.model.VertexWithAdjacentEdges
vertexWithAdjacentEdgesVertex :: Phantoms.TTerm (Model.VertexWithAdjacentEdges v) -> Phantoms.TTerm (Model.Vertex v)
vertexWithAdjacentEdgesVertex x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
        Core.projectionFieldName = (Core.Name "vertex")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the ins field of hydra.pg.model.VertexWithAdjacentEdges
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
              Core.projectionFieldName = (Core.Name "vertex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ins"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "outs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
              Core.projectionFieldName = (Core.Name "outs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the outs field of hydra.pg.model.VertexWithAdjacentEdges
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
              Core.projectionFieldName = (Core.Name "vertex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ins"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
              Core.projectionFieldName = (Core.Name "ins")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the vertex field of hydra.pg.model.VertexWithAdjacentEdges
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
              Core.projectionFieldName = (Core.Name "ins")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
              Core.projectionFieldName = (Core.Name "outs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the id field of hydra.pg.model.Vertex
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
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the label field of hydra.pg.model.Vertex
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
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the properties field of hydra.pg.model.Vertex
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
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.model.Vertex"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
