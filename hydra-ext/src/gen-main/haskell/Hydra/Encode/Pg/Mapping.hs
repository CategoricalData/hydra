-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.pg.mapping

module Hydra.Encode.Pg.Mapping where

import qualified Hydra.Core as Core
import qualified Hydra.Encode.Pg.Model as Model
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Pg.Mapping as Mapping
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

annotationSchema :: (Mapping.AnnotationSchema -> Core.Term)
annotationSchema x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.pg.mapping.AnnotationSchema"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "vertexLabel"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Mapping.annotationSchemaVertexLabel x))},
    Core.Field {
      Core.fieldName = (Core.Name "edgeLabel"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Mapping.annotationSchemaEdgeLabel x))},
    Core.Field {
      Core.fieldName = (Core.Name "vertexId"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Mapping.annotationSchemaVertexId x))},
    Core.Field {
      Core.fieldName = (Core.Name "edgeId"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Mapping.annotationSchemaEdgeId x))},
    Core.Field {
      Core.fieldName = (Core.Name "propertyKey"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Mapping.annotationSchemaPropertyKey x))},
    Core.Field {
      Core.fieldName = (Core.Name "propertyValue"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Mapping.annotationSchemaPropertyValue x))},
    Core.Field {
      Core.fieldName = (Core.Name "outVertex"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Mapping.annotationSchemaOutVertex x))},
    Core.Field {
      Core.fieldName = (Core.Name "outVertexLabel"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Mapping.annotationSchemaOutVertexLabel x))},
    Core.Field {
      Core.fieldName = (Core.Name "inVertex"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Mapping.annotationSchemaInVertex x))},
    Core.Field {
      Core.fieldName = (Core.Name "inVertexLabel"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Mapping.annotationSchemaInVertexLabel x))},
    Core.Field {
      Core.fieldName = (Core.Name "outEdge"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Mapping.annotationSchemaOutEdge x))},
    Core.Field {
      Core.fieldName = (Core.Name "outEdgeLabel"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Mapping.annotationSchemaOutEdgeLabel x))},
    Core.Field {
      Core.fieldName = (Core.Name "inEdge"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Mapping.annotationSchemaInEdge x))},
    Core.Field {
      Core.fieldName = (Core.Name "inEdgeLabel"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Mapping.annotationSchemaInEdgeLabel x))},
    Core.Field {
      Core.fieldName = (Core.Name "ignore"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Mapping.annotationSchemaIgnore x))}]}))

edgeSpec :: (Mapping.EdgeSpec -> Core.Term)
edgeSpec x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.pg.mapping.EdgeSpec"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "label"),
      Core.fieldTerm = (Model.edgeLabel (Mapping.edgeSpecLabel x))},
    Core.Field {
      Core.fieldName = (Core.Name "id"),
      Core.fieldTerm = (valueSpec (Mapping.edgeSpecId x))},
    Core.Field {
      Core.fieldName = (Core.Name "out"),
      Core.fieldTerm = (valueSpec (Mapping.edgeSpecOut x))},
    Core.Field {
      Core.fieldName = (Core.Name "in"),
      Core.fieldTerm = (valueSpec (Mapping.edgeSpecIn x))},
    Core.Field {
      Core.fieldName = (Core.Name "properties"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map propertySpec xs)) (Mapping.edgeSpecProperties x))}]}))

elementSpec :: (Mapping.ElementSpec -> Core.Term)
elementSpec x = case x of
  Mapping.ElementSpecVertex v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.pg.mapping.ElementSpec"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "vertex"),
      Core.fieldTerm = (vertexSpec v1)}}))
  Mapping.ElementSpecEdge v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.pg.mapping.ElementSpec"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "edge"),
      Core.fieldTerm = (edgeSpec v1)}}))

propertySpec :: (Mapping.PropertySpec -> Core.Term)
propertySpec x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.pg.mapping.PropertySpec"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "key"),
      Core.fieldTerm = (Model.propertyKey (Mapping.propertySpecKey x))},
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (valueSpec (Mapping.propertySpecValue x))}]}))

valueSpec :: (Mapping.ValueSpec -> Core.Term)
valueSpec x = case x of
  Mapping.ValueSpecValue -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.pg.mapping.ValueSpec"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = Core.TermUnit}}))
  Mapping.ValueSpecPattern v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.pg.mapping.ValueSpec"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "pattern"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString v1))}}))

vertexSpec :: (Mapping.VertexSpec -> Core.Term)
vertexSpec x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.pg.mapping.VertexSpec"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "label"),
      Core.fieldTerm = (Model.vertexLabel (Mapping.vertexSpecLabel x))},
    Core.Field {
      Core.fieldName = (Core.Name "id"),
      Core.fieldTerm = (valueSpec (Mapping.vertexSpecId x))},
    Core.Field {
      Core.fieldName = (Core.Name "properties"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map propertySpec xs)) (Mapping.vertexSpecProperties x))}]}))
