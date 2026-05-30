-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.pg.rdf.environment

module Hydra.Dsl.Pg.Rdf.Environment where

import qualified Hydra.Core as Core
import qualified Hydra.Pg.Model as Model
import qualified Hydra.Pg.Rdf.Environment as Environment
import qualified Hydra.Typed as Phantoms
import qualified Hydra.Rdf.Syntax as Syntax
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

pgRdfEnvironment :: Phantoms.TypedTerm (v -> Syntax.Iri) -> Phantoms.TypedTerm (Model.VertexLabel -> Syntax.Iri) -> Phantoms.TypedTerm (v -> Syntax.Iri) -> Phantoms.TypedTerm (Model.EdgeLabel -> Syntax.Iri) -> Phantoms.TypedTerm (Model.PropertyKey -> Syntax.Iri) -> Phantoms.TypedTerm (v -> Syntax.Literal) -> Phantoms.TypedTerm (Environment.PgRdfEnvironment v)
pgRdfEnvironment encodeVertexId encodeVertexLabel encodeEdgeId encodeEdgeLabel encodePropertyKey encodePropertyValue =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encodeVertexId"),
          Core.fieldTerm = (Phantoms.unTypedTerm encodeVertexId)},
        Core.Field {
          Core.fieldName = (Core.Name "encodeVertexLabel"),
          Core.fieldTerm = (Phantoms.unTypedTerm encodeVertexLabel)},
        Core.Field {
          Core.fieldName = (Core.Name "encodeEdgeId"),
          Core.fieldTerm = (Phantoms.unTypedTerm encodeEdgeId)},
        Core.Field {
          Core.fieldName = (Core.Name "encodeEdgeLabel"),
          Core.fieldTerm = (Phantoms.unTypedTerm encodeEdgeLabel)},
        Core.Field {
          Core.fieldName = (Core.Name "encodePropertyKey"),
          Core.fieldTerm = (Phantoms.unTypedTerm encodePropertyKey)},
        Core.Field {
          Core.fieldName = (Core.Name "encodePropertyValue"),
          Core.fieldTerm = (Phantoms.unTypedTerm encodePropertyValue)}]}))

pgRdfEnvironmentEncodeEdgeId :: Phantoms.TypedTerm (Environment.PgRdfEnvironment v) -> Phantoms.TypedTerm (v -> Syntax.Iri)
pgRdfEnvironmentEncodeEdgeId x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
        Core.projectionFieldName = (Core.Name "encodeEdgeId")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

pgRdfEnvironmentEncodeEdgeLabel :: Phantoms.TypedTerm (Environment.PgRdfEnvironment v) -> Phantoms.TypedTerm (Model.EdgeLabel -> Syntax.Iri)
pgRdfEnvironmentEncodeEdgeLabel x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
        Core.projectionFieldName = (Core.Name "encodeEdgeLabel")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

pgRdfEnvironmentEncodePropertyKey :: Phantoms.TypedTerm (Environment.PgRdfEnvironment v) -> Phantoms.TypedTerm (Model.PropertyKey -> Syntax.Iri)
pgRdfEnvironmentEncodePropertyKey x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
        Core.projectionFieldName = (Core.Name "encodePropertyKey")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

pgRdfEnvironmentEncodePropertyValue :: Phantoms.TypedTerm (Environment.PgRdfEnvironment v) -> Phantoms.TypedTerm (v -> Syntax.Literal)
pgRdfEnvironmentEncodePropertyValue x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
        Core.projectionFieldName = (Core.Name "encodePropertyValue")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

pgRdfEnvironmentEncodeVertexId :: Phantoms.TypedTerm (Environment.PgRdfEnvironment v) -> Phantoms.TypedTerm (v -> Syntax.Iri)
pgRdfEnvironmentEncodeVertexId x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
        Core.projectionFieldName = (Core.Name "encodeVertexId")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

pgRdfEnvironmentEncodeVertexLabel :: Phantoms.TypedTerm (Environment.PgRdfEnvironment v) -> Phantoms.TypedTerm (Model.VertexLabel -> Syntax.Iri)
pgRdfEnvironmentEncodeVertexLabel x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
        Core.projectionFieldName = (Core.Name "encodeVertexLabel")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

pgRdfEnvironmentWithEncodeEdgeId :: Phantoms.TypedTerm (Environment.PgRdfEnvironment v) -> Phantoms.TypedTerm (v -> Syntax.Iri) -> Phantoms.TypedTerm (Environment.PgRdfEnvironment v)
pgRdfEnvironmentWithEncodeEdgeId original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encodeVertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodeVertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodeVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeEdgeId"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "encodeEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodeEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodePropertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodePropertyKey")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodePropertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodePropertyValue")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

pgRdfEnvironmentWithEncodeEdgeLabel :: Phantoms.TypedTerm (Environment.PgRdfEnvironment v) -> Phantoms.TypedTerm (Model.EdgeLabel -> Syntax.Iri) -> Phantoms.TypedTerm (Environment.PgRdfEnvironment v)
pgRdfEnvironmentWithEncodeEdgeLabel original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encodeVertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodeVertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodeVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeEdgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodeEdgeId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeEdgeLabel"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "encodePropertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodePropertyKey")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodePropertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodePropertyValue")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

pgRdfEnvironmentWithEncodePropertyKey :: Phantoms.TypedTerm (Environment.PgRdfEnvironment v) -> Phantoms.TypedTerm (Model.PropertyKey -> Syntax.Iri) -> Phantoms.TypedTerm (Environment.PgRdfEnvironment v)
pgRdfEnvironmentWithEncodePropertyKey original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encodeVertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodeVertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodeVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeEdgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodeEdgeId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodeEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodePropertyKey"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "encodePropertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodePropertyValue")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

pgRdfEnvironmentWithEncodePropertyValue :: Phantoms.TypedTerm (Environment.PgRdfEnvironment v) -> Phantoms.TypedTerm (v -> Syntax.Literal) -> Phantoms.TypedTerm (Environment.PgRdfEnvironment v)
pgRdfEnvironmentWithEncodePropertyValue original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encodeVertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodeVertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodeVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeEdgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodeEdgeId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodeEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodePropertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodePropertyKey")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodePropertyValue"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

pgRdfEnvironmentWithEncodeVertexId :: Phantoms.TypedTerm (Environment.PgRdfEnvironment v) -> Phantoms.TypedTerm (v -> Syntax.Iri) -> Phantoms.TypedTerm (Environment.PgRdfEnvironment v)
pgRdfEnvironmentWithEncodeVertexId original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encodeVertexId"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "encodeVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodeVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeEdgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodeEdgeId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodeEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodePropertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodePropertyKey")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodePropertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodePropertyValue")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

pgRdfEnvironmentWithEncodeVertexLabel :: Phantoms.TypedTerm (Environment.PgRdfEnvironment v) -> Phantoms.TypedTerm (Model.VertexLabel -> Syntax.Iri) -> Phantoms.TypedTerm (Environment.PgRdfEnvironment v)
pgRdfEnvironmentWithEncodeVertexLabel original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encodeVertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodeVertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeVertexLabel"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "encodeEdgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodeEdgeId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodeEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodePropertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodePropertyKey")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodePropertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionFieldName = (Core.Name "encodePropertyValue")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
