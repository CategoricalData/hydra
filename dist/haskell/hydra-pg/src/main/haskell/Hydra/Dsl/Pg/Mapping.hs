-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.pg.mapping

module Hydra.Dsl.Pg.Mapping where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Pg.Mapping as Mapping
import qualified Hydra.Pg.Model as Model
import qualified Hydra.Typed as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

annotationSchema :: Phantoms.TypedTerm String -> Phantoms.TypedTerm String -> Phantoms.TypedTerm String -> Phantoms.TypedTerm String -> Phantoms.TypedTerm String -> Phantoms.TypedTerm String -> Phantoms.TypedTerm String -> Phantoms.TypedTerm String -> Phantoms.TypedTerm String -> Phantoms.TypedTerm String -> Phantoms.TypedTerm String -> Phantoms.TypedTerm String -> Phantoms.TypedTerm String -> Phantoms.TypedTerm String -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Mapping.AnnotationSchema
annotationSchema vertexLabel edgeLabel vertexId edgeId propertyKey propertyValue outVertex outVertexLabel inVertex inVertexLabel outEdge outEdgeLabel inEdge inEdgeLabel ignore =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Phantoms.unTypedTerm vertexLabel)},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Phantoms.unTypedTerm edgeLabel)},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Phantoms.unTypedTerm vertexId)},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Phantoms.unTypedTerm edgeId)},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Phantoms.unTypedTerm propertyKey)},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Phantoms.unTypedTerm propertyValue)},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Phantoms.unTypedTerm outVertex)},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Phantoms.unTypedTerm outVertexLabel)},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Phantoms.unTypedTerm inVertex)},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Phantoms.unTypedTerm inVertexLabel)},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Phantoms.unTypedTerm outEdge)},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Phantoms.unTypedTerm outEdgeLabel)},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Phantoms.unTypedTerm inEdge)},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Phantoms.unTypedTerm inEdgeLabel)},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Phantoms.unTypedTerm ignore)}]}))

annotationSchemaEdgeId :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String
annotationSchemaEdgeId x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionFieldName = (Core.Name "edgeId")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

annotationSchemaEdgeLabel :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String
annotationSchemaEdgeLabel x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionFieldName = (Core.Name "edgeLabel")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

annotationSchemaIgnore :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String
annotationSchemaIgnore x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionFieldName = (Core.Name "ignore")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

annotationSchemaInEdge :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String
annotationSchemaInEdge x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionFieldName = (Core.Name "inEdge")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

annotationSchemaInEdgeLabel :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String
annotationSchemaInEdgeLabel x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionFieldName = (Core.Name "inEdgeLabel")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

annotationSchemaInVertex :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String
annotationSchemaInVertex x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionFieldName = (Core.Name "inVertex")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

annotationSchemaInVertexLabel :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String
annotationSchemaInVertexLabel x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionFieldName = (Core.Name "inVertexLabel")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

annotationSchemaOutEdge :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String
annotationSchemaOutEdge x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionFieldName = (Core.Name "outEdge")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

annotationSchemaOutEdgeLabel :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String
annotationSchemaOutEdgeLabel x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionFieldName = (Core.Name "outEdgeLabel")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

annotationSchemaOutVertex :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String
annotationSchemaOutVertex x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionFieldName = (Core.Name "outVertex")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

annotationSchemaOutVertexLabel :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String
annotationSchemaOutVertexLabel x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionFieldName = (Core.Name "outVertexLabel")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

annotationSchemaPropertyKey :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String
annotationSchemaPropertyKey x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionFieldName = (Core.Name "propertyKey")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

annotationSchemaPropertyValue :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String
annotationSchemaPropertyValue x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionFieldName = (Core.Name "propertyValue")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

annotationSchemaVertexId :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String
annotationSchemaVertexId x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionFieldName = (Core.Name "vertexId")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

annotationSchemaVertexLabel :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String
annotationSchemaVertexLabel x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
        Core.projectionFieldName = (Core.Name "vertexLabel")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

annotationSchemaWithEdgeId :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Mapping.AnnotationSchema
annotationSchemaWithEdgeId original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "vertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "edgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "vertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "propertyKey")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "propertyValue")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outVertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inVertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outEdge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inEdge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "ignore")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

annotationSchemaWithEdgeLabel :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Mapping.AnnotationSchema
annotationSchemaWithEdgeLabel original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "vertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "vertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "edgeId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "propertyKey")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "propertyValue")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outVertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inVertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outEdge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inEdge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "ignore")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

annotationSchemaWithIgnore :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Mapping.AnnotationSchema
annotationSchemaWithIgnore original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "vertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "edgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "vertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "edgeId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "propertyKey")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "propertyValue")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outVertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inVertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outEdge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inEdge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

annotationSchemaWithInEdge :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Mapping.AnnotationSchema
annotationSchemaWithInEdge original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "vertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "edgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "vertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "edgeId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "propertyKey")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "propertyValue")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outVertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inVertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outEdge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "ignore")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

annotationSchemaWithInEdgeLabel :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Mapping.AnnotationSchema
annotationSchemaWithInEdgeLabel original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "vertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "edgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "vertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "edgeId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "propertyKey")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "propertyValue")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outVertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inVertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outEdge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inEdge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "ignore")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

annotationSchemaWithInVertex :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Mapping.AnnotationSchema
annotationSchemaWithInVertex original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "vertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "edgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "vertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "edgeId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "propertyKey")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "propertyValue")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outVertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outEdge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inEdge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "ignore")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

annotationSchemaWithInVertexLabel :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Mapping.AnnotationSchema
annotationSchemaWithInVertexLabel original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "vertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "edgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "vertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "edgeId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "propertyKey")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "propertyValue")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outVertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inVertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outEdge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inEdge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "ignore")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

annotationSchemaWithOutEdge :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Mapping.AnnotationSchema
annotationSchemaWithOutEdge original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "vertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "edgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "vertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "edgeId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "propertyKey")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "propertyValue")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outVertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inVertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inEdge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "ignore")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

annotationSchemaWithOutEdgeLabel :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Mapping.AnnotationSchema
annotationSchemaWithOutEdgeLabel original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "vertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "edgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "vertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "edgeId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "propertyKey")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "propertyValue")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outVertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inVertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outEdge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inEdge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "ignore")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

annotationSchemaWithOutVertex :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Mapping.AnnotationSchema
annotationSchemaWithOutVertex original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "vertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "edgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "vertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "edgeId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "propertyKey")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "propertyValue")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inVertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outEdge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inEdge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "ignore")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

annotationSchemaWithOutVertexLabel :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Mapping.AnnotationSchema
annotationSchemaWithOutVertexLabel original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "vertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "edgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "vertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "edgeId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "propertyKey")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "propertyValue")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outVertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inVertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outEdge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inEdge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "ignore")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

annotationSchemaWithPropertyKey :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Mapping.AnnotationSchema
annotationSchemaWithPropertyKey original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "vertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "edgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "vertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "edgeId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "propertyValue")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outVertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inVertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outEdge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inEdge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "ignore")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

annotationSchemaWithPropertyValue :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Mapping.AnnotationSchema
annotationSchemaWithPropertyValue original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "vertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "edgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "vertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "edgeId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "propertyKey")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outVertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inVertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outEdge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inEdge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "ignore")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

annotationSchemaWithVertexId :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Mapping.AnnotationSchema
annotationSchemaWithVertexId original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "vertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "edgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "edgeId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "propertyKey")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "propertyValue")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outVertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inVertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outEdge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inEdge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "ignore")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

annotationSchemaWithVertexLabel :: Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Mapping.AnnotationSchema
annotationSchemaWithVertexLabel original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexLabel"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "edgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "edgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "vertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "edgeId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyKey"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "propertyKey")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "propertyValue")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outVertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inVertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inVertexLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inVertexLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outEdge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "outEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inEdge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdgeLabel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "inEdgeLabel")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ignore"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
              Core.projectionFieldName = (Core.Name "ignore")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

edgeSpec :: Phantoms.TypedTerm Model.EdgeLabel -> Phantoms.TypedTerm Mapping.ValueSpec -> Phantoms.TypedTerm Mapping.ValueSpec -> Phantoms.TypedTerm Mapping.ValueSpec -> Phantoms.TypedTerm [Mapping.PropertySpec] -> Phantoms.TypedTerm Mapping.EdgeSpec
edgeSpec label id out in_ properties =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
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

edgeSpecId :: Phantoms.TypedTerm Mapping.EdgeSpec -> Phantoms.TypedTerm Mapping.ValueSpec
edgeSpecId x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

edgeSpecIn :: Phantoms.TypedTerm Mapping.EdgeSpec -> Phantoms.TypedTerm Mapping.ValueSpec
edgeSpecIn x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
        Core.projectionFieldName = (Core.Name "in")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

edgeSpecLabel :: Phantoms.TypedTerm Mapping.EdgeSpec -> Phantoms.TypedTerm Model.EdgeLabel
edgeSpecLabel x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

edgeSpecOut :: Phantoms.TypedTerm Mapping.EdgeSpec -> Phantoms.TypedTerm Mapping.ValueSpec
edgeSpecOut x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
        Core.projectionFieldName = (Core.Name "out")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

edgeSpecProperties :: Phantoms.TypedTerm Mapping.EdgeSpec -> Phantoms.TypedTerm [Mapping.PropertySpec]
edgeSpecProperties x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
        Core.projectionFieldName = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

edgeSpecWithId :: Phantoms.TypedTerm Mapping.EdgeSpec -> Phantoms.TypedTerm Mapping.ValueSpec -> Phantoms.TypedTerm Mapping.EdgeSpec
edgeSpecWithId original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

edgeSpecWithIn :: Phantoms.TypedTerm Mapping.EdgeSpec -> Phantoms.TypedTerm Mapping.ValueSpec -> Phantoms.TypedTerm Mapping.EdgeSpec
edgeSpecWithIn original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

edgeSpecWithLabel :: Phantoms.TypedTerm Mapping.EdgeSpec -> Phantoms.TypedTerm Model.EdgeLabel -> Phantoms.TypedTerm Mapping.EdgeSpec
edgeSpecWithLabel original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

edgeSpecWithOut :: Phantoms.TypedTerm Mapping.EdgeSpec -> Phantoms.TypedTerm Mapping.ValueSpec -> Phantoms.TypedTerm Mapping.EdgeSpec
edgeSpecWithOut original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

edgeSpecWithProperties :: Phantoms.TypedTerm Mapping.EdgeSpec -> Phantoms.TypedTerm [Mapping.PropertySpec] -> Phantoms.TypedTerm Mapping.EdgeSpec
edgeSpecWithProperties original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionFieldName = (Core.Name "out")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
              Core.projectionFieldName = (Core.Name "in")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

elementSpecEdge :: Phantoms.TypedTerm Mapping.EdgeSpec -> Phantoms.TypedTerm Mapping.ElementSpec
elementSpecEdge x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.mapping.ElementSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edge"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

elementSpecVertex :: Phantoms.TypedTerm Mapping.VertexSpec -> Phantoms.TypedTerm Mapping.ElementSpec
elementSpecVertex x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.mapping.ElementSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vertex"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

propertySpec :: Phantoms.TypedTerm Model.PropertyKey -> Phantoms.TypedTerm Mapping.ValueSpec -> Phantoms.TypedTerm Mapping.PropertySpec
propertySpec key value =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.PropertySpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTypedTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm value)}]}))

propertySpecKey :: Phantoms.TypedTerm Mapping.PropertySpec -> Phantoms.TypedTerm Model.PropertyKey
propertySpecKey x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.PropertySpec"),
        Core.projectionFieldName = (Core.Name "key")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

propertySpecValue :: Phantoms.TypedTerm Mapping.PropertySpec -> Phantoms.TypedTerm Mapping.ValueSpec
propertySpecValue x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.PropertySpec"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

propertySpecWithKey :: Phantoms.TypedTerm Mapping.PropertySpec -> Phantoms.TypedTerm Model.PropertyKey -> Phantoms.TypedTerm Mapping.PropertySpec
propertySpecWithKey original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.PropertySpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.PropertySpec"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

propertySpecWithValue :: Phantoms.TypedTerm Mapping.PropertySpec -> Phantoms.TypedTerm Mapping.ValueSpec -> Phantoms.TypedTerm Mapping.PropertySpec
propertySpecWithValue original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.PropertySpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.PropertySpec"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

schema :: Phantoms.TypedTerm (Coders.Coder Core.Type t) -> Phantoms.TypedTerm (Coders.Coder Core.Term v) -> Phantoms.TypedTerm (Coders.Coder Core.Type t) -> Phantoms.TypedTerm (Coders.Coder Core.Term v) -> Phantoms.TypedTerm (Coders.Coder Core.Type t) -> Phantoms.TypedTerm (Coders.Coder Core.Term v) -> Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm v -> Phantoms.TypedTerm v -> Phantoms.TypedTerm (Mapping.Schema s t v)
schema vertexIdTypes vertexIds edgeIdTypes edgeIds propertyTypes propertyValues annotations defaultVertexId defaultEdgeId =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.Schema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexIdTypes"),
          Core.fieldTerm = (Phantoms.unTypedTerm vertexIdTypes)},
        Core.Field {
          Core.fieldName = (Core.Name "vertexIds"),
          Core.fieldTerm = (Phantoms.unTypedTerm vertexIds)},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIdTypes"),
          Core.fieldTerm = (Phantoms.unTypedTerm edgeIdTypes)},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIds"),
          Core.fieldTerm = (Phantoms.unTypedTerm edgeIds)},
        Core.Field {
          Core.fieldName = (Core.Name "propertyTypes"),
          Core.fieldTerm = (Phantoms.unTypedTerm propertyTypes)},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm propertyValues)},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTypedTerm annotations)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultVertexId"),
          Core.fieldTerm = (Phantoms.unTypedTerm defaultVertexId)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultEdgeId"),
          Core.fieldTerm = (Phantoms.unTypedTerm defaultEdgeId)}]}))

schemaAnnotations :: Phantoms.TypedTerm (Mapping.Schema s t v) -> Phantoms.TypedTerm Mapping.AnnotationSchema
schemaAnnotations x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
        Core.projectionFieldName = (Core.Name "annotations")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

schemaDefaultEdgeId :: Phantoms.TypedTerm (Mapping.Schema s t v) -> Phantoms.TypedTerm v
schemaDefaultEdgeId x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
        Core.projectionFieldName = (Core.Name "defaultEdgeId")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

schemaDefaultVertexId :: Phantoms.TypedTerm (Mapping.Schema s t v) -> Phantoms.TypedTerm v
schemaDefaultVertexId x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
        Core.projectionFieldName = (Core.Name "defaultVertexId")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

schemaEdgeIdTypes :: Phantoms.TypedTerm (Mapping.Schema s t v) -> Phantoms.TypedTerm (Coders.Coder Core.Type t)
schemaEdgeIdTypes x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
        Core.projectionFieldName = (Core.Name "edgeIdTypes")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

schemaEdgeIds :: Phantoms.TypedTerm (Mapping.Schema s t v) -> Phantoms.TypedTerm (Coders.Coder Core.Term v)
schemaEdgeIds x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
        Core.projectionFieldName = (Core.Name "edgeIds")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

schemaPropertyTypes :: Phantoms.TypedTerm (Mapping.Schema s t v) -> Phantoms.TypedTerm (Coders.Coder Core.Type t)
schemaPropertyTypes x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
        Core.projectionFieldName = (Core.Name "propertyTypes")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

schemaPropertyValues :: Phantoms.TypedTerm (Mapping.Schema s t v) -> Phantoms.TypedTerm (Coders.Coder Core.Term v)
schemaPropertyValues x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
        Core.projectionFieldName = (Core.Name "propertyValues")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

schemaVertexIdTypes :: Phantoms.TypedTerm (Mapping.Schema s t v) -> Phantoms.TypedTerm (Coders.Coder Core.Type t)
schemaVertexIdTypes x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
        Core.projectionFieldName = (Core.Name "vertexIdTypes")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

schemaVertexIds :: Phantoms.TypedTerm (Mapping.Schema s t v) -> Phantoms.TypedTerm (Coders.Coder Core.Term v)
schemaVertexIds x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
        Core.projectionFieldName = (Core.Name "vertexIds")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

schemaWithAnnotations :: Phantoms.TypedTerm (Mapping.Schema s t v) -> Phantoms.TypedTerm Mapping.AnnotationSchema -> Phantoms.TypedTerm (Mapping.Schema s t v)
schemaWithAnnotations original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.Schema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "vertexIdTypes")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "vertexIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "edgeIdTypes")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "edgeIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "propertyTypes")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "propertyValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultVertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "defaultVertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultEdgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "defaultEdgeId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

schemaWithDefaultEdgeId :: Phantoms.TypedTerm (Mapping.Schema s t v) -> Phantoms.TypedTerm v -> Phantoms.TypedTerm (Mapping.Schema s t v)
schemaWithDefaultEdgeId original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.Schema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "vertexIdTypes")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "vertexIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "edgeIdTypes")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "edgeIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "propertyTypes")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "propertyValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultVertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "defaultVertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultEdgeId"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

schemaWithDefaultVertexId :: Phantoms.TypedTerm (Mapping.Schema s t v) -> Phantoms.TypedTerm v -> Phantoms.TypedTerm (Mapping.Schema s t v)
schemaWithDefaultVertexId original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.Schema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "vertexIdTypes")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "vertexIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "edgeIdTypes")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "edgeIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "propertyTypes")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "propertyValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultVertexId"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultEdgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "defaultEdgeId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

schemaWithEdgeIdTypes :: Phantoms.TypedTerm (Mapping.Schema s t v) -> Phantoms.TypedTerm (Coders.Coder Core.Type t) -> Phantoms.TypedTerm (Mapping.Schema s t v)
schemaWithEdgeIdTypes original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.Schema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "vertexIdTypes")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "vertexIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIdTypes"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "edgeIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "propertyTypes")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "propertyValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultVertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "defaultVertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultEdgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "defaultEdgeId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

schemaWithEdgeIds :: Phantoms.TypedTerm (Mapping.Schema s t v) -> Phantoms.TypedTerm (Coders.Coder Core.Term v) -> Phantoms.TypedTerm (Mapping.Schema s t v)
schemaWithEdgeIds original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.Schema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "vertexIdTypes")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "vertexIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "edgeIdTypes")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIds"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "propertyTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "propertyTypes")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "propertyValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultVertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "defaultVertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultEdgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "defaultEdgeId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

schemaWithPropertyTypes :: Phantoms.TypedTerm (Mapping.Schema s t v) -> Phantoms.TypedTerm (Coders.Coder Core.Type t) -> Phantoms.TypedTerm (Mapping.Schema s t v)
schemaWithPropertyTypes original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.Schema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "vertexIdTypes")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "vertexIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "edgeIdTypes")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "edgeIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyTypes"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "propertyValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultVertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "defaultVertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultEdgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "defaultEdgeId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

schemaWithPropertyValues :: Phantoms.TypedTerm (Mapping.Schema s t v) -> Phantoms.TypedTerm (Coders.Coder Core.Term v) -> Phantoms.TypedTerm (Mapping.Schema s t v)
schemaWithPropertyValues original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.Schema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "vertexIdTypes")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "vertexIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "edgeIdTypes")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "edgeIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "propertyTypes")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValues"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultVertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "defaultVertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultEdgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "defaultEdgeId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

schemaWithVertexIdTypes :: Phantoms.TypedTerm (Mapping.Schema s t v) -> Phantoms.TypedTerm (Coders.Coder Core.Type t) -> Phantoms.TypedTerm (Mapping.Schema s t v)
schemaWithVertexIdTypes original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.Schema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexIdTypes"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "vertexIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "vertexIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "edgeIdTypes")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "edgeIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "propertyTypes")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "propertyValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultVertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "defaultVertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultEdgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "defaultEdgeId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

schemaWithVertexIds :: Phantoms.TypedTerm (Mapping.Schema s t v) -> Phantoms.TypedTerm (Coders.Coder Core.Term v) -> Phantoms.TypedTerm (Mapping.Schema s t v)
schemaWithVertexIds original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.Schema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertexIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "vertexIdTypes")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexIds"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIdTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "edgeIdTypes")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edgeIds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "edgeIds")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "propertyTypes")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "propertyValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "propertyValues")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "annotations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "annotations")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultVertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "defaultVertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultEdgeId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.Schema"),
              Core.projectionFieldName = (Core.Name "defaultEdgeId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

valueSpecPattern :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Mapping.ValueSpec
valueSpecPattern x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.mapping.ValueSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

valueSpecValue :: Phantoms.TypedTerm Mapping.ValueSpec
valueSpecValue =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.mapping.ValueSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = Core.TermUnit}}))

vertexSpec :: Phantoms.TypedTerm Model.VertexLabel -> Phantoms.TypedTerm Mapping.ValueSpec -> Phantoms.TypedTerm [Mapping.PropertySpec] -> Phantoms.TypedTerm Mapping.VertexSpec
vertexSpec label id properties =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.VertexSpec"),
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

vertexSpecId :: Phantoms.TypedTerm Mapping.VertexSpec -> Phantoms.TypedTerm Mapping.ValueSpec
vertexSpecId x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.VertexSpec"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

vertexSpecLabel :: Phantoms.TypedTerm Mapping.VertexSpec -> Phantoms.TypedTerm Model.VertexLabel
vertexSpecLabel x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.VertexSpec"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

vertexSpecProperties :: Phantoms.TypedTerm Mapping.VertexSpec -> Phantoms.TypedTerm [Mapping.PropertySpec]
vertexSpecProperties x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.mapping.VertexSpec"),
        Core.projectionFieldName = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

vertexSpecWithId :: Phantoms.TypedTerm Mapping.VertexSpec -> Phantoms.TypedTerm Mapping.ValueSpec -> Phantoms.TypedTerm Mapping.VertexSpec
vertexSpecWithId original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.VertexSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.VertexSpec"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.VertexSpec"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

vertexSpecWithLabel :: Phantoms.TypedTerm Mapping.VertexSpec -> Phantoms.TypedTerm Model.VertexLabel -> Phantoms.TypedTerm Mapping.VertexSpec
vertexSpecWithLabel original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.VertexSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.VertexSpec"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.VertexSpec"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

vertexSpecWithProperties :: Phantoms.TypedTerm Mapping.VertexSpec -> Phantoms.TypedTerm [Mapping.PropertySpec] -> Phantoms.TypedTerm Mapping.VertexSpec
vertexSpecWithProperties original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.mapping.VertexSpec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.VertexSpec"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.mapping.VertexSpec"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
