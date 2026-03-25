-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.pg.mapping

module Hydra.Dsl.Pg.Mapping where

import qualified Hydra.Core as Core
import qualified Hydra.Pg.Mapping as Mapping
import qualified Hydra.Pg.Model as Model
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

annotationSchema :: Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm Mapping.AnnotationSchema
annotationSchema vertexLabel edgeLabel vertexId edgeId propertyKey propertyValue outVertex outVertexLabel inVertex inVertexLabel outEdge outEdgeLabel inEdge inEdgeLabel ignore =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Phantoms.unTTerm vertexLabel)},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Phantoms.unTTerm edgeLabel)},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Phantoms.unTTerm vertexId)},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Phantoms.unTTerm edgeId)},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Phantoms.unTTerm propertyKey)},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Phantoms.unTTerm propertyValue)},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Phantoms.unTTerm outVertex)},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Phantoms.unTTerm outVertexLabel)},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Phantoms.unTTerm inVertex)},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Phantoms.unTTerm inVertexLabel)},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Phantoms.unTTerm outEdge)},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Phantoms.unTTerm outEdgeLabel)},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Phantoms.unTTerm inEdge)},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Phantoms.unTTerm inEdgeLabel)},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Phantoms.unTTerm ignore)}]}))

annotationSchemaEdgeId :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String
annotationSchemaEdgeId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionField = (Core.Name "edgeId")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationSchemaEdgeLabel :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String
annotationSchemaEdgeLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionField = (Core.Name "edgeLabel")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationSchemaIgnore :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String
annotationSchemaIgnore x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionField = (Core.Name "ignore")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationSchemaInEdge :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String
annotationSchemaInEdge x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionField = (Core.Name "inEdge")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationSchemaInEdgeLabel :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String
annotationSchemaInEdgeLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionField = (Core.Name "inEdgeLabel")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationSchemaInVertex :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String
annotationSchemaInVertex x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionField = (Core.Name "inVertex")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationSchemaInVertexLabel :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String
annotationSchemaInVertexLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionField = (Core.Name "inVertexLabel")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationSchemaOutEdge :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String
annotationSchemaOutEdge x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionField = (Core.Name "outEdge")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationSchemaOutEdgeLabel :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String
annotationSchemaOutEdgeLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionField = (Core.Name "outEdgeLabel")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationSchemaOutVertex :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String
annotationSchemaOutVertex x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionField = (Core.Name "outVertex")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationSchemaOutVertexLabel :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String
annotationSchemaOutVertexLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionField = (Core.Name "outVertexLabel")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationSchemaPropertyKey :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String
annotationSchemaPropertyKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionField = (Core.Name "propertyKey")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationSchemaPropertyValue :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String
annotationSchemaPropertyValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionField = (Core.Name "propertyValue")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationSchemaVertexId :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String
annotationSchemaVertexId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionField = (Core.Name "vertexId")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationSchemaVertexLabel :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String
annotationSchemaVertexLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionField = (Core.Name "vertexLabel")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

annotationSchemaWithEdgeId :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String -> Phantoms.TTerm Mapping.AnnotationSchema
annotationSchemaWithEdgeId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "vertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "edgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "vertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "propertyKey")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "propertyValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outVertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inVertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outEdge")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inEdge")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "ignore")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationSchemaWithEdgeLabel :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String -> Phantoms.TTerm Mapping.AnnotationSchema
annotationSchemaWithEdgeLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "vertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "vertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "edgeId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "propertyKey")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "propertyValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outVertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inVertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outEdge")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inEdge")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "ignore")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationSchemaWithIgnore :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String -> Phantoms.TTerm Mapping.AnnotationSchema
annotationSchemaWithIgnore original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "vertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "edgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "vertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "edgeId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "propertyKey")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "propertyValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outVertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inVertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outEdge")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inEdge")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

annotationSchemaWithInEdge :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String -> Phantoms.TTerm Mapping.AnnotationSchema
annotationSchemaWithInEdge original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "vertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "edgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "vertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "edgeId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "propertyKey")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "propertyValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outVertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inVertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outEdge")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "ignore")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationSchemaWithInEdgeLabel :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String -> Phantoms.TTerm Mapping.AnnotationSchema
annotationSchemaWithInEdgeLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "vertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "edgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "vertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "edgeId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "propertyKey")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "propertyValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outVertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inVertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outEdge")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inEdge")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "ignore")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationSchemaWithInVertex :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String -> Phantoms.TTerm Mapping.AnnotationSchema
annotationSchemaWithInVertex original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "vertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "edgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "vertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "edgeId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "propertyKey")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "propertyValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outVertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outEdge")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inEdge")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "ignore")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationSchemaWithInVertexLabel :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String -> Phantoms.TTerm Mapping.AnnotationSchema
annotationSchemaWithInVertexLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "vertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "edgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "vertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "edgeId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "propertyKey")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "propertyValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outVertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inVertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outEdge")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inEdge")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "ignore")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationSchemaWithOutEdge :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String -> Phantoms.TTerm Mapping.AnnotationSchema
annotationSchemaWithOutEdge original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "vertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "edgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "vertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "edgeId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "propertyKey")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "propertyValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outVertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inVertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inEdge")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "ignore")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationSchemaWithOutEdgeLabel :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String -> Phantoms.TTerm Mapping.AnnotationSchema
annotationSchemaWithOutEdgeLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "vertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "edgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "vertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "edgeId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "propertyKey")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "propertyValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outVertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inVertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outEdge")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inEdge")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "ignore")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationSchemaWithOutVertex :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String -> Phantoms.TTerm Mapping.AnnotationSchema
annotationSchemaWithOutVertex original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "vertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "edgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "vertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "edgeId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "propertyKey")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "propertyValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inVertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outEdge")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inEdge")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "ignore")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationSchemaWithOutVertexLabel :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String -> Phantoms.TTerm Mapping.AnnotationSchema
annotationSchemaWithOutVertexLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "vertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "edgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "vertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "edgeId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "propertyKey")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "propertyValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outVertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inVertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outEdge")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inEdge")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "ignore")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationSchemaWithPropertyKey :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String -> Phantoms.TTerm Mapping.AnnotationSchema
annotationSchemaWithPropertyKey original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "vertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "edgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "vertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "edgeId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "propertyValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outVertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inVertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outEdge")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inEdge")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "ignore")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationSchemaWithPropertyValue :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String -> Phantoms.TTerm Mapping.AnnotationSchema
annotationSchemaWithPropertyValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "vertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "edgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "vertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "edgeId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "propertyKey")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outVertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inVertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outEdge")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inEdge")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "ignore")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationSchemaWithVertexId :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String -> Phantoms.TTerm Mapping.AnnotationSchema
annotationSchemaWithVertexId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "vertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "edgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "edgeId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "propertyKey")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "propertyValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outVertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inVertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outEdge")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inEdge")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "ignore")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

annotationSchemaWithVertexLabel :: Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm String -> Phantoms.TTerm Mapping.AnnotationSchema
annotationSchemaWithVertexLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "edgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "vertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "edgeId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "propertyKey")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "propertyValue")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outVertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inVertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inVertexLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outEdge")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "outEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inEdge")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "inEdgeLabel")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionField = (Core.Name "ignore")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

edgeSpec :: Phantoms.TTerm Model.EdgeLabel -> Phantoms.TTerm Mapping.ValueSpec -> Phantoms.TTerm Mapping.ValueSpec -> Phantoms.TTerm Mapping.ValueSpec -> Phantoms.TTerm [Mapping.PropertySpec] -> Phantoms.TTerm Mapping.EdgeSpec
edgeSpec label id out in_ properties =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
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

edgeSpecId :: Phantoms.TTerm Mapping.EdgeSpec -> Phantoms.TTerm Mapping.ValueSpec
edgeSpecId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
        Core.projectionField = (Core.Name "id")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgeSpecIn :: Phantoms.TTerm Mapping.EdgeSpec -> Phantoms.TTerm Mapping.ValueSpec
edgeSpecIn x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
        Core.projectionField = (Core.Name "in")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgeSpecLabel :: Phantoms.TTerm Mapping.EdgeSpec -> Phantoms.TTerm Model.EdgeLabel
edgeSpecLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
        Core.projectionField = (Core.Name "label")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgeSpecOut :: Phantoms.TTerm Mapping.EdgeSpec -> Phantoms.TTerm Mapping.ValueSpec
edgeSpecOut x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
        Core.projectionField = (Core.Name "out")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgeSpecProperties :: Phantoms.TTerm Mapping.EdgeSpec -> Phantoms.TTerm [Mapping.PropertySpec]
edgeSpecProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
        Core.projectionField = (Core.Name "properties")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgeSpecWithId :: Phantoms.TTerm Mapping.EdgeSpec -> Phantoms.TTerm Mapping.ValueSpec -> Phantoms.TTerm Mapping.EdgeSpec
edgeSpecWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionField = (Core.Name "label")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionField = (Core.Name "out")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionField = (Core.Name "in")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionField = (Core.Name "properties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

edgeSpecWithIn :: Phantoms.TTerm Mapping.EdgeSpec -> Phantoms.TTerm Mapping.ValueSpec -> Phantoms.TTerm Mapping.EdgeSpec
edgeSpecWithIn original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionField = (Core.Name "label")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionField = (Core.Name "out")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionField = (Core.Name "properties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

edgeSpecWithLabel :: Phantoms.TTerm Mapping.EdgeSpec -> Phantoms.TTerm Model.EdgeLabel -> Phantoms.TTerm Mapping.EdgeSpec
edgeSpecWithLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionField = (Core.Name "out")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionField = (Core.Name "in")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionField = (Core.Name "properties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

edgeSpecWithOut :: Phantoms.TTerm Mapping.EdgeSpec -> Phantoms.TTerm Mapping.ValueSpec -> Phantoms.TTerm Mapping.EdgeSpec
edgeSpecWithOut original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionField = (Core.Name "label")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionField = (Core.Name "in")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionField = (Core.Name "properties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

edgeSpecWithProperties :: Phantoms.TTerm Mapping.EdgeSpec -> Phantoms.TTerm [Mapping.PropertySpec] -> Phantoms.TTerm Mapping.EdgeSpec
edgeSpecWithProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionField = (Core.Name "label")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionField = (Core.Name "out")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionField = (Core.Name "in")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

elementSpecEdge :: Phantoms.TTerm Mapping.EdgeSpec -> Phantoms.TTerm Mapping.ElementSpec
elementSpecEdge x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.mapping.ElementSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edge"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

elementSpecVertex :: Phantoms.TTerm Mapping.VertexSpec -> Phantoms.TTerm Mapping.ElementSpec
elementSpecVertex x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.mapping.ElementSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vertex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

propertySpec :: Phantoms.TTerm Model.PropertyKey -> Phantoms.TTerm Mapping.ValueSpec -> Phantoms.TTerm Mapping.PropertySpec
propertySpec key value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.PropertySpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

propertySpecKey :: Phantoms.TTerm Mapping.PropertySpec -> Phantoms.TTerm Model.PropertyKey
propertySpecKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.PropertySpec"),
        Core.projectionField = (Core.Name "key")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertySpecValue :: Phantoms.TTerm Mapping.PropertySpec -> Phantoms.TTerm Mapping.ValueSpec
propertySpecValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.PropertySpec"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertySpecWithKey :: Phantoms.TTerm Mapping.PropertySpec -> Phantoms.TTerm Model.PropertyKey -> Phantoms.TTerm Mapping.PropertySpec
propertySpecWithKey original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.PropertySpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.PropertySpec"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

propertySpecWithValue :: Phantoms.TTerm Mapping.PropertySpec -> Phantoms.TTerm Mapping.ValueSpec -> Phantoms.TTerm Mapping.PropertySpec
propertySpecWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.PropertySpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.PropertySpec"),
              Core.projectionField = (Core.Name "key")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

schema :: Phantoms.TTerm (Util.Coder Core.Type t) -> Phantoms.TTerm (Util.Coder Core.Term v) -> Phantoms.TTerm (Util.Coder Core.Type t) -> Phantoms.TTerm (Util.Coder Core.Term v) -> Phantoms.TTerm (Util.Coder Core.Type t) -> Phantoms.TTerm (Util.Coder Core.Term v) -> Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm v -> Phantoms.TTerm v -> Phantoms.TTerm (Mapping.Schema s t v)
schema vertexIdTypes vertexIds edgeIdTypes edgeIds propertyTypes propertyValues annotations defaultVertexId defaultEdgeId =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.Schema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexIdTypes"),
          Core.fieldTerm = (Phantoms.unTTerm vertexIdTypes)},
        Core.Field {
          Core.fieldName = (Core.Name "vertexIds"),
          Core.fieldTerm = (Phantoms.unTTerm vertexIds)},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIdTypes"),
          Core.fieldTerm = (Phantoms.unTTerm edgeIdTypes)},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIds"),
          Core.fieldTerm = (Phantoms.unTTerm edgeIds)},
        Core.Field {
          Core.fieldName = (Core.Name "propertyTypes"),
          Core.fieldTerm = (Phantoms.unTTerm propertyTypes)},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValues"),
          Core.fieldTerm = (Phantoms.unTTerm propertyValues)},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultVertexId"),
          Core.fieldTerm = (Phantoms.unTTerm defaultVertexId)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultEdgeId"),
          Core.fieldTerm = (Phantoms.unTTerm defaultEdgeId)}]}))

schemaAnnotations :: Phantoms.TTerm (Mapping.Schema s t v) -> Phantoms.TTerm Mapping.AnnotationSchema
schemaAnnotations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
        Core.projectionField = (Core.Name "annotations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

schemaDefaultEdgeId :: Phantoms.TTerm (Mapping.Schema s t v) -> Phantoms.TTerm v
schemaDefaultEdgeId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
        Core.projectionField = (Core.Name "defaultEdgeId")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

schemaDefaultVertexId :: Phantoms.TTerm (Mapping.Schema s t v) -> Phantoms.TTerm v
schemaDefaultVertexId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
        Core.projectionField = (Core.Name "defaultVertexId")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

schemaEdgeIdTypes :: Phantoms.TTerm (Mapping.Schema s t v) -> Phantoms.TTerm (Util.Coder Core.Type t)
schemaEdgeIdTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
        Core.projectionField = (Core.Name "edgeIdTypes")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

schemaEdgeIds :: Phantoms.TTerm (Mapping.Schema s t v) -> Phantoms.TTerm (Util.Coder Core.Term v)
schemaEdgeIds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
        Core.projectionField = (Core.Name "edgeIds")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

schemaPropertyTypes :: Phantoms.TTerm (Mapping.Schema s t v) -> Phantoms.TTerm (Util.Coder Core.Type t)
schemaPropertyTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
        Core.projectionField = (Core.Name "propertyTypes")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

schemaPropertyValues :: Phantoms.TTerm (Mapping.Schema s t v) -> Phantoms.TTerm (Util.Coder Core.Term v)
schemaPropertyValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
        Core.projectionField = (Core.Name "propertyValues")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

schemaVertexIdTypes :: Phantoms.TTerm (Mapping.Schema s t v) -> Phantoms.TTerm (Util.Coder Core.Type t)
schemaVertexIdTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
        Core.projectionField = (Core.Name "vertexIdTypes")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

schemaVertexIds :: Phantoms.TTerm (Mapping.Schema s t v) -> Phantoms.TTerm (Util.Coder Core.Term v)
schemaVertexIds x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
        Core.projectionField = (Core.Name "vertexIds")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

schemaWithAnnotations :: Phantoms.TTerm (Mapping.Schema s t v) -> Phantoms.TTerm Mapping.AnnotationSchema -> Phantoms.TTerm (Mapping.Schema s t v)
schemaWithAnnotations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.Schema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "vertexIdTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "vertexIds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "edgeIdTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "edgeIds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "propertyTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "propertyValues")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultVertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "defaultVertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultEdgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "defaultEdgeId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

schemaWithDefaultEdgeId :: Phantoms.TTerm (Mapping.Schema s t v) -> Phantoms.TTerm v -> Phantoms.TTerm (Mapping.Schema s t v)
schemaWithDefaultEdgeId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.Schema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "vertexIdTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "vertexIds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "edgeIdTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "edgeIds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "propertyTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "propertyValues")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultVertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "defaultVertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultEdgeId"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

schemaWithDefaultVertexId :: Phantoms.TTerm (Mapping.Schema s t v) -> Phantoms.TTerm v -> Phantoms.TTerm (Mapping.Schema s t v)
schemaWithDefaultVertexId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.Schema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "vertexIdTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "vertexIds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "edgeIdTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "edgeIds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "propertyTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "propertyValues")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultVertexId"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultEdgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "defaultEdgeId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

schemaWithEdgeIdTypes :: Phantoms.TTerm (Mapping.Schema s t v) -> Phantoms.TTerm (Util.Coder Core.Type t) -> Phantoms.TTerm (Mapping.Schema s t v)
schemaWithEdgeIdTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.Schema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "vertexIdTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "vertexIds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIdTypes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "edgeIds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "propertyTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "propertyValues")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultVertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "defaultVertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultEdgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "defaultEdgeId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

schemaWithEdgeIds :: Phantoms.TTerm (Mapping.Schema s t v) -> Phantoms.TTerm (Util.Coder Core.Term v) -> Phantoms.TTerm (Mapping.Schema s t v)
schemaWithEdgeIds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.Schema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "vertexIdTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "vertexIds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "edgeIdTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "propertyTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "propertyTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "propertyValues")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultVertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "defaultVertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultEdgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "defaultEdgeId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

schemaWithPropertyTypes :: Phantoms.TTerm (Mapping.Schema s t v) -> Phantoms.TTerm (Util.Coder Core.Type t) -> Phantoms.TTerm (Mapping.Schema s t v)
schemaWithPropertyTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.Schema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "vertexIdTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "vertexIds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "edgeIdTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "edgeIds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyTypes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "propertyValues")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultVertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "defaultVertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultEdgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "defaultEdgeId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

schemaWithPropertyValues :: Phantoms.TTerm (Mapping.Schema s t v) -> Phantoms.TTerm (Util.Coder Core.Term v) -> Phantoms.TTerm (Mapping.Schema s t v)
schemaWithPropertyValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.Schema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "vertexIdTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "vertexIds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "edgeIdTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "edgeIds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "propertyTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValues"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultVertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "defaultVertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultEdgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "defaultEdgeId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

schemaWithVertexIdTypes :: Phantoms.TTerm (Mapping.Schema s t v) -> Phantoms.TTerm (Util.Coder Core.Type t) -> Phantoms.TTerm (Mapping.Schema s t v)
schemaWithVertexIdTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.Schema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexIdTypes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "vertexIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "vertexIds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "edgeIdTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "edgeIds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "propertyTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "propertyValues")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultVertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "defaultVertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultEdgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "defaultEdgeId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

schemaWithVertexIds :: Phantoms.TTerm (Mapping.Schema s t v) -> Phantoms.TTerm (Util.Coder Core.Term v) -> Phantoms.TTerm (Mapping.Schema s t v)
schemaWithVertexIds original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.Schema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "vertexIdTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexIds"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "edgeIdTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "edgeIds")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "propertyTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "propertyValues")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "annotations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultVertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "defaultVertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultEdgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionField = (Core.Name "defaultEdgeId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

valueSpecPattern :: Phantoms.TTerm String -> Phantoms.TTerm Mapping.ValueSpec
valueSpecPattern x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.mapping.ValueSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueSpecValue :: Phantoms.TTerm Mapping.ValueSpec
valueSpecValue =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.mapping.ValueSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = Core.TermUnit}}))

vertexSpec :: Phantoms.TTerm Model.VertexLabel -> Phantoms.TTerm Mapping.ValueSpec -> Phantoms.TTerm [Mapping.PropertySpec] -> Phantoms.TTerm Mapping.VertexSpec
vertexSpec label id properties =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.VertexSpec"),
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

vertexSpecId :: Phantoms.TTerm Mapping.VertexSpec -> Phantoms.TTerm Mapping.ValueSpec
vertexSpecId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.VertexSpec"),
        Core.projectionField = (Core.Name "id")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexSpecLabel :: Phantoms.TTerm Mapping.VertexSpec -> Phantoms.TTerm Model.VertexLabel
vertexSpecLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.VertexSpec"),
        Core.projectionField = (Core.Name "label")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexSpecProperties :: Phantoms.TTerm Mapping.VertexSpec -> Phantoms.TTerm [Mapping.PropertySpec]
vertexSpecProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.VertexSpec"),
        Core.projectionField = (Core.Name "properties")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexSpecWithId :: Phantoms.TTerm Mapping.VertexSpec -> Phantoms.TTerm Mapping.ValueSpec -> Phantoms.TTerm Mapping.VertexSpec
vertexSpecWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.VertexSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.VertexSpec"),
              Core.projectionField = (Core.Name "label")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.VertexSpec"),
              Core.projectionField = (Core.Name "properties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

vertexSpecWithLabel :: Phantoms.TTerm Mapping.VertexSpec -> Phantoms.TTerm Model.VertexLabel -> Phantoms.TTerm Mapping.VertexSpec
vertexSpecWithLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.VertexSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.VertexSpec"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.VertexSpec"),
              Core.projectionField = (Core.Name "properties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

vertexSpecWithProperties :: Phantoms.TTerm Mapping.VertexSpec -> Phantoms.TTerm [Mapping.PropertySpec] -> Phantoms.TTerm Mapping.VertexSpec
vertexSpecWithProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.VertexSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.VertexSpec"),
              Core.projectionField = (Core.Name "label")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.VertexSpec"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
