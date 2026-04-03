-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.pg.rdf.environment

module Hydra.Dsl.Pg.Rdf.Environment where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Org.W3.Rdf.Syntax as Syntax
import qualified Hydra.Pg.Model as Model
import qualified Hydra.Pg.Rdf.Environment as Environment
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

pgRdfEnvironment :: Phantoms.TTerm (v -> Syntax.Iri) -> Phantoms.TTerm (Model.VertexLabel -> Syntax.Iri) -> Phantoms.TTerm (v -> Syntax.Iri) -> Phantoms.TTerm (Model.EdgeLabel -> Syntax.Iri) -> Phantoms.TTerm (Model.PropertyKey -> Syntax.Iri) -> Phantoms.TTerm (v -> Syntax.Literal) -> Phantoms.TTerm (Environment.PgRdfEnvironment v)
pgRdfEnvironment encodeVertexId encodeVertexLabel encodeEdgeId encodeEdgeLabel encodePropertyKey encodePropertyValue =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encodeVertexId"),
          Core.fieldTerm = (Phantoms.unTTerm encodeVertexId)},
        Core.Field {
          Core.fieldName = (Core.Name "encodeVertexLabel"),
          Core.fieldTerm = (Phantoms.unTTerm encodeVertexLabel)},
        Core.Field {
          Core.fieldName = (Core.Name "encodeEdgeId"),
          Core.fieldTerm = (Phantoms.unTTerm encodeEdgeId)},
        Core.Field {
          Core.fieldName = (Core.Name "encodeEdgeLabel"),
          Core.fieldTerm = (Phantoms.unTTerm encodeEdgeLabel)},
        Core.Field {
          Core.fieldName = (Core.Name "encodePropertyKey"),
          Core.fieldTerm = (Phantoms.unTTerm encodePropertyKey)},
        Core.Field {
          Core.fieldName = (Core.Name "encodePropertyValue"),
          Core.fieldTerm = (Phantoms.unTTerm encodePropertyValue)}]}))

pgRdfEnvironmentEncodeEdgeId :: Phantoms.TTerm (Environment.PgRdfEnvironment v) -> Phantoms.TTerm (v -> Syntax.Iri)
pgRdfEnvironmentEncodeEdgeId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
        Core.projectionField = (Core.Name "encodeEdgeId")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pgRdfEnvironmentEncodeEdgeLabel :: Phantoms.TTerm (Environment.PgRdfEnvironment v) -> Phantoms.TTerm (Model.EdgeLabel -> Syntax.Iri)
pgRdfEnvironmentEncodeEdgeLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
        Core.projectionField = (Core.Name "encodeEdgeLabel")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pgRdfEnvironmentEncodePropertyKey :: Phantoms.TTerm (Environment.PgRdfEnvironment v) -> Phantoms.TTerm (Model.PropertyKey -> Syntax.Iri)
pgRdfEnvironmentEncodePropertyKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
        Core.projectionField = (Core.Name "encodePropertyKey")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pgRdfEnvironmentEncodePropertyValue :: Phantoms.TTerm (Environment.PgRdfEnvironment v) -> Phantoms.TTerm (v -> Syntax.Literal)
pgRdfEnvironmentEncodePropertyValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
        Core.projectionField = (Core.Name "encodePropertyValue")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pgRdfEnvironmentEncodeVertexId :: Phantoms.TTerm (Environment.PgRdfEnvironment v) -> Phantoms.TTerm (v -> Syntax.Iri)
pgRdfEnvironmentEncodeVertexId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
        Core.projectionField = (Core.Name "encodeVertexId")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pgRdfEnvironmentEncodeVertexLabel :: Phantoms.TTerm (Environment.PgRdfEnvironment v) -> Phantoms.TTerm (Model.VertexLabel -> Syntax.Iri)
pgRdfEnvironmentEncodeVertexLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
        Core.projectionField = (Core.Name "encodeVertexLabel")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pgRdfEnvironmentWithEncodeEdgeId :: Phantoms.TTerm (Environment.PgRdfEnvironment v) -> Phantoms.TTerm (v -> Syntax.Iri) -> Phantoms.TTerm (Environment.PgRdfEnvironment v)
pgRdfEnvironmentWithEncodeEdgeId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encodeVertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodeVertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodeVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeEdgeId"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "encodeEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodeEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodePropertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodePropertyKey")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodePropertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodePropertyValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pgRdfEnvironmentWithEncodeEdgeLabel :: Phantoms.TTerm (Environment.PgRdfEnvironment v) -> Phantoms.TTerm (Model.EdgeLabel -> Syntax.Iri) -> Phantoms.TTerm (Environment.PgRdfEnvironment v)
pgRdfEnvironmentWithEncodeEdgeLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encodeVertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodeVertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodeVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeEdgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodeEdgeId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeEdgeLabel"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "encodePropertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodePropertyKey")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodePropertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodePropertyValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pgRdfEnvironmentWithEncodePropertyKey :: Phantoms.TTerm (Environment.PgRdfEnvironment v) -> Phantoms.TTerm (Model.PropertyKey -> Syntax.Iri) -> Phantoms.TTerm (Environment.PgRdfEnvironment v)
pgRdfEnvironmentWithEncodePropertyKey original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encodeVertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodeVertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodeVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeEdgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodeEdgeId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodeEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodePropertyKey"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "encodePropertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodePropertyValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pgRdfEnvironmentWithEncodePropertyValue :: Phantoms.TTerm (Environment.PgRdfEnvironment v) -> Phantoms.TTerm (v -> Syntax.Literal) -> Phantoms.TTerm (Environment.PgRdfEnvironment v)
pgRdfEnvironmentWithEncodePropertyValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encodeVertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodeVertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodeVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeEdgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodeEdgeId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodeEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodePropertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodePropertyKey")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodePropertyValue"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pgRdfEnvironmentWithEncodeVertexId :: Phantoms.TTerm (Environment.PgRdfEnvironment v) -> Phantoms.TTerm (v -> Syntax.Iri) -> Phantoms.TTerm (Environment.PgRdfEnvironment v)
pgRdfEnvironmentWithEncodeVertexId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encodeVertexId"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "encodeVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodeVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeEdgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodeEdgeId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodeEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodePropertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodePropertyKey")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodePropertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodePropertyValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pgRdfEnvironmentWithEncodeVertexLabel :: Phantoms.TTerm (Environment.PgRdfEnvironment v) -> Phantoms.TTerm (Model.VertexLabel -> Syntax.Iri) -> Phantoms.TTerm (Environment.PgRdfEnvironment v)
pgRdfEnvironmentWithEncodeVertexLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encodeVertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodeVertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeVertexLabel"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "encodeEdgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodeEdgeId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodeEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodePropertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodePropertyKey")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodePropertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.rdf.environment.PgRdfEnvironment"),
              Core.projectionField = (Core.Name "encodePropertyValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
