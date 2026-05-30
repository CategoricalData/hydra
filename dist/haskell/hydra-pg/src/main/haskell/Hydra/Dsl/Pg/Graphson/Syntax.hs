-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.pg.graphson.syntax

module Hydra.Dsl.Pg.Graphson.Syntax where

import qualified Hydra.Core as Core
import qualified Hydra.Pg.Graphson.Syntax as Syntax
import qualified Hydra.Typed as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Int as I
import qualified Data.Map as M

adjacentEdge :: Phantoms.TypedTerm Syntax.Value -> Phantoms.TypedTerm Syntax.Value -> Phantoms.TypedTerm (M.Map Syntax.PropertyKey Syntax.Value) -> Phantoms.TypedTerm Syntax.AdjacentEdge
adjacentEdge id vertexId properties =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.AdjacentEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Phantoms.unTypedTerm vertexId)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTypedTerm properties)}]}))

adjacentEdgeId :: Phantoms.TypedTerm Syntax.AdjacentEdge -> Phantoms.TypedTerm Syntax.Value
adjacentEdgeId x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.AdjacentEdge"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

adjacentEdgeProperties :: Phantoms.TypedTerm Syntax.AdjacentEdge -> Phantoms.TypedTerm (M.Map Syntax.PropertyKey Syntax.Value)
adjacentEdgeProperties x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.AdjacentEdge"),
        Core.projectionFieldName = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

adjacentEdgeVertexId :: Phantoms.TypedTerm Syntax.AdjacentEdge -> Phantoms.TypedTerm Syntax.Value
adjacentEdgeVertexId x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.AdjacentEdge"),
        Core.projectionFieldName = (Core.Name "vertexId")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

adjacentEdgeWithId :: Phantoms.TypedTerm Syntax.AdjacentEdge -> Phantoms.TypedTerm Syntax.Value -> Phantoms.TypedTerm Syntax.AdjacentEdge
adjacentEdgeWithId original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.AdjacentEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "vertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

adjacentEdgeWithProperties :: Phantoms.TypedTerm Syntax.AdjacentEdge -> Phantoms.TypedTerm (M.Map Syntax.PropertyKey Syntax.Value) -> Phantoms.TypedTerm Syntax.AdjacentEdge
adjacentEdgeWithProperties original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.AdjacentEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "vertexId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

adjacentEdgeWithVertexId :: Phantoms.TypedTerm Syntax.AdjacentEdge -> Phantoms.TypedTerm Syntax.Value -> Phantoms.TypedTerm Syntax.AdjacentEdge
adjacentEdgeWithVertexId original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.AdjacentEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.AdjacentEdge"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

bigDecimalValue :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Syntax.BigDecimalValue
bigDecimalValue x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.graphson.syntax.BigDecimalValue"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

compositeTypedValue :: Phantoms.TypedTerm Syntax.TypeName -> Phantoms.TypedTerm Syntax.Map -> Phantoms.TypedTerm Syntax.CompositeTypedValue
compositeTypedValue type_ fields =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.CompositeTypedValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTypedTerm fields)}]}))

compositeTypedValueFields :: Phantoms.TypedTerm Syntax.CompositeTypedValue -> Phantoms.TypedTerm Syntax.Map
compositeTypedValueFields x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.CompositeTypedValue"),
        Core.projectionFieldName = (Core.Name "fields")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

compositeTypedValueType :: Phantoms.TypedTerm Syntax.CompositeTypedValue -> Phantoms.TypedTerm Syntax.TypeName
compositeTypedValueType x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.CompositeTypedValue"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

compositeTypedValueWithFields :: Phantoms.TypedTerm Syntax.CompositeTypedValue -> Phantoms.TypedTerm Syntax.Map -> Phantoms.TypedTerm Syntax.CompositeTypedValue
compositeTypedValueWithFields original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.CompositeTypedValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.CompositeTypedValue"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

compositeTypedValueWithType :: Phantoms.TypedTerm Syntax.CompositeTypedValue -> Phantoms.TypedTerm Syntax.TypeName -> Phantoms.TypedTerm Syntax.CompositeTypedValue
compositeTypedValueWithType original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.CompositeTypedValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.CompositeTypedValue"),
              Core.projectionFieldName = (Core.Name "fields")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

dateTime :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Syntax.DateTime
dateTime x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.graphson.syntax.DateTime"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

doubleValueFinite :: Phantoms.TypedTerm Double -> Phantoms.TypedTerm Syntax.DoubleValue
doubleValueFinite x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.DoubleValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "finite"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

doubleValueInfinity :: Phantoms.TypedTerm Syntax.DoubleValue
doubleValueInfinity =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.DoubleValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "infinity"),
        Core.fieldTerm = Core.TermUnit}}))

doubleValueNegativeInfinity :: Phantoms.TypedTerm Syntax.DoubleValue
doubleValueNegativeInfinity =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.DoubleValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "negativeInfinity"),
        Core.fieldTerm = Core.TermUnit}}))

doubleValueNotANumber :: Phantoms.TypedTerm Syntax.DoubleValue
doubleValueNotANumber =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.DoubleValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notANumber"),
        Core.fieldTerm = Core.TermUnit}}))

duration :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Syntax.Duration
duration x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.graphson.syntax.Duration"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

edgeLabel :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Syntax.EdgeLabel
edgeLabel x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.graphson.syntax.EdgeLabel"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

floatValueFinite :: Phantoms.TypedTerm Float -> Phantoms.TypedTerm Syntax.FloatValue
floatValueFinite x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.FloatValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "finite"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

floatValueInfinity :: Phantoms.TypedTerm Syntax.FloatValue
floatValueInfinity =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.FloatValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "infinity"),
        Core.fieldTerm = Core.TermUnit}}))

floatValueNegativeInfinity :: Phantoms.TypedTerm Syntax.FloatValue
floatValueNegativeInfinity =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.FloatValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "negativeInfinity"),
        Core.fieldTerm = Core.TermUnit}}))

floatValueNotANumber :: Phantoms.TypedTerm Syntax.FloatValue
floatValueNotANumber =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.FloatValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notANumber"),
        Core.fieldTerm = Core.TermUnit}}))

map :: Phantoms.TypedTerm [Syntax.ValuePair] -> Phantoms.TypedTerm Syntax.Map
map x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.graphson.syntax.Map"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

primitiveTypedValue :: Phantoms.TypedTerm Syntax.TypeName -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Syntax.PrimitiveTypedValue
primitiveTypedValue type_ value =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.PrimitiveTypedValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm value)}]}))

primitiveTypedValueType :: Phantoms.TypedTerm Syntax.PrimitiveTypedValue -> Phantoms.TypedTerm Syntax.TypeName
primitiveTypedValueType x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.PrimitiveTypedValue"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

primitiveTypedValueValue :: Phantoms.TypedTerm Syntax.PrimitiveTypedValue -> Phantoms.TypedTerm String
primitiveTypedValueValue x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.PrimitiveTypedValue"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

primitiveTypedValueWithType :: Phantoms.TypedTerm Syntax.PrimitiveTypedValue -> Phantoms.TypedTerm Syntax.TypeName -> Phantoms.TypedTerm Syntax.PrimitiveTypedValue
primitiveTypedValueWithType original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.PrimitiveTypedValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.PrimitiveTypedValue"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

primitiveTypedValueWithValue :: Phantoms.TypedTerm Syntax.PrimitiveTypedValue -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Syntax.PrimitiveTypedValue
primitiveTypedValueWithValue original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.PrimitiveTypedValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.PrimitiveTypedValue"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

propertyKey :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Syntax.PropertyKey
propertyKey x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.graphson.syntax.PropertyKey"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

typeName :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Syntax.TypeName
typeName x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.graphson.syntax.TypeName"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

unBigDecimalValue :: Phantoms.TypedTerm Syntax.BigDecimalValue -> Phantoms.TypedTerm String
unBigDecimalValue x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.pg.graphson.syntax.BigDecimalValue")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unDateTime :: Phantoms.TypedTerm Syntax.DateTime -> Phantoms.TypedTerm String
unDateTime x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.pg.graphson.syntax.DateTime")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unDuration :: Phantoms.TypedTerm Syntax.Duration -> Phantoms.TypedTerm String
unDuration x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.pg.graphson.syntax.Duration")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unEdgeLabel :: Phantoms.TypedTerm Syntax.EdgeLabel -> Phantoms.TypedTerm String
unEdgeLabel x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.pg.graphson.syntax.EdgeLabel")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unMap :: Phantoms.TypedTerm Syntax.Map -> Phantoms.TypedTerm [Syntax.ValuePair]
unMap x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.pg.graphson.syntax.Map")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unPropertyKey :: Phantoms.TypedTerm Syntax.PropertyKey -> Phantoms.TypedTerm String
unPropertyKey x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.pg.graphson.syntax.PropertyKey")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unTypeName :: Phantoms.TypedTerm Syntax.TypeName -> Phantoms.TypedTerm String
unTypeName x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.pg.graphson.syntax.TypeName")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unUuid :: Phantoms.TypedTerm Syntax.Uuid -> Phantoms.TypedTerm String
unUuid x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.pg.graphson.syntax.Uuid")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unVertexLabel :: Phantoms.TypedTerm Syntax.VertexLabel -> Phantoms.TypedTerm String
unVertexLabel x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.pg.graphson.syntax.VertexLabel")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

uuid :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Syntax.Uuid
uuid x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.graphson.syntax.Uuid"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

valueBigDecimal :: Phantoms.TypedTerm Syntax.BigDecimalValue -> Phantoms.TypedTerm Syntax.Value
valueBigDecimal x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bigDecimal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

valueBigInteger :: Phantoms.TypedTerm Integer -> Phantoms.TypedTerm Syntax.Value
valueBigInteger x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bigInteger"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

valueBinary :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Syntax.Value
valueBinary x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "binary"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

valueBoolean :: Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Syntax.Value
valueBoolean x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

valueByte :: Phantoms.TypedTerm I.Int16 -> Phantoms.TypedTerm Syntax.Value
valueByte x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "byte"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

valueChar :: Phantoms.TypedTerm I.Int64 -> Phantoms.TypedTerm Syntax.Value
valueChar x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "char"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

valueComposite :: Phantoms.TypedTerm Syntax.CompositeTypedValue -> Phantoms.TypedTerm Syntax.Value
valueComposite x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "composite"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

valueDateTime :: Phantoms.TypedTerm Syntax.DateTime -> Phantoms.TypedTerm Syntax.Value
valueDateTime x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dateTime"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

valueDouble :: Phantoms.TypedTerm Syntax.DoubleValue -> Phantoms.TypedTerm Syntax.Value
valueDouble x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "double"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

valueDuration :: Phantoms.TypedTerm Syntax.Duration -> Phantoms.TypedTerm Syntax.Value
valueDuration x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duration"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

valueFloat :: Phantoms.TypedTerm Syntax.FloatValue -> Phantoms.TypedTerm Syntax.Value
valueFloat x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

valueInteger :: Phantoms.TypedTerm Int -> Phantoms.TypedTerm Syntax.Value
valueInteger x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

valueList :: Phantoms.TypedTerm [Syntax.Value] -> Phantoms.TypedTerm Syntax.Value
valueList x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

valueLong :: Phantoms.TypedTerm I.Int64 -> Phantoms.TypedTerm Syntax.Value
valueLong x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "long"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

valueMap :: Phantoms.TypedTerm Syntax.Map -> Phantoms.TypedTerm Syntax.Value
valueMap x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

valueNull :: Phantoms.TypedTerm Syntax.Value
valueNull =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = Core.TermUnit}}))

valuePair :: Phantoms.TypedTerm Syntax.Value -> Phantoms.TypedTerm Syntax.Value -> Phantoms.TypedTerm Syntax.ValuePair
valuePair first second =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.ValuePair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTypedTerm first)},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Phantoms.unTypedTerm second)}]}))

valuePairFirst :: Phantoms.TypedTerm Syntax.ValuePair -> Phantoms.TypedTerm Syntax.Value
valuePairFirst x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.ValuePair"),
        Core.projectionFieldName = (Core.Name "first")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

valuePairSecond :: Phantoms.TypedTerm Syntax.ValuePair -> Phantoms.TypedTerm Syntax.Value
valuePairSecond x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.ValuePair"),
        Core.projectionFieldName = (Core.Name "second")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

valuePairWithFirst :: Phantoms.TypedTerm Syntax.ValuePair -> Phantoms.TypedTerm Syntax.Value -> Phantoms.TypedTerm Syntax.ValuePair
valuePairWithFirst original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.ValuePair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.ValuePair"),
              Core.projectionFieldName = (Core.Name "second")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

valuePairWithSecond :: Phantoms.TypedTerm Syntax.ValuePair -> Phantoms.TypedTerm Syntax.Value -> Phantoms.TypedTerm Syntax.ValuePair
valuePairWithSecond original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.ValuePair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.ValuePair"),
              Core.projectionFieldName = (Core.Name "first")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

valuePrimitive :: Phantoms.TypedTerm Syntax.PrimitiveTypedValue -> Phantoms.TypedTerm Syntax.Value
valuePrimitive x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitive"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

valueSet :: Phantoms.TypedTerm [Syntax.Value] -> Phantoms.TypedTerm Syntax.Value
valueSet x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

valueShort :: Phantoms.TypedTerm I.Int16 -> Phantoms.TypedTerm Syntax.Value
valueShort x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "short"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

valueString :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Syntax.Value
valueString x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

valueUuid :: Phantoms.TypedTerm Syntax.Uuid -> Phantoms.TypedTerm Syntax.Value
valueUuid x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uuid"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

vertex :: Phantoms.TypedTerm Syntax.Value -> Phantoms.TypedTerm (Maybe Syntax.VertexLabel) -> Phantoms.TypedTerm (M.Map Syntax.EdgeLabel [Syntax.AdjacentEdge]) -> Phantoms.TypedTerm (M.Map Syntax.EdgeLabel [Syntax.AdjacentEdge]) -> Phantoms.TypedTerm (M.Map Syntax.PropertyKey [Syntax.VertexPropertyValue]) -> Phantoms.TypedTerm Syntax.Vertex
vertex id label inEdges outEdges properties =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTypedTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "inEdges"),
          Core.fieldTerm = (Phantoms.unTypedTerm inEdges)},
        Core.Field {
          Core.fieldName = (Core.Name "outEdges"),
          Core.fieldTerm = (Phantoms.unTypedTerm outEdges)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTypedTerm properties)}]}))

vertexId :: Phantoms.TypedTerm Syntax.Vertex -> Phantoms.TypedTerm Syntax.Value
vertexId x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

vertexInEdges :: Phantoms.TypedTerm Syntax.Vertex -> Phantoms.TypedTerm (M.Map Syntax.EdgeLabel [Syntax.AdjacentEdge])
vertexInEdges x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
        Core.projectionFieldName = (Core.Name "inEdges")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

vertexLabel :: Phantoms.TypedTerm Syntax.Vertex -> Phantoms.TypedTerm (Maybe Syntax.VertexLabel)
vertexLabel x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

vertexLabel_ :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Syntax.VertexLabel
vertexLabel_ x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.graphson.syntax.VertexLabel"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

vertexOutEdges :: Phantoms.TypedTerm Syntax.Vertex -> Phantoms.TypedTerm (M.Map Syntax.EdgeLabel [Syntax.AdjacentEdge])
vertexOutEdges x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
        Core.projectionFieldName = (Core.Name "outEdges")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

vertexProperties :: Phantoms.TypedTerm Syntax.Vertex -> Phantoms.TypedTerm (M.Map Syntax.PropertyKey [Syntax.VertexPropertyValue])
vertexProperties x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
        Core.projectionFieldName = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

vertexPropertyValue :: Phantoms.TypedTerm (Maybe Syntax.Value) -> Phantoms.TypedTerm Syntax.Value -> Phantoms.TypedTerm Syntax.VertexPropertyValue
vertexPropertyValue id value =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.VertexPropertyValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm value)}]}))

vertexPropertyValueId :: Phantoms.TypedTerm Syntax.VertexPropertyValue -> Phantoms.TypedTerm (Maybe Syntax.Value)
vertexPropertyValueId x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.VertexPropertyValue"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

vertexPropertyValueValue :: Phantoms.TypedTerm Syntax.VertexPropertyValue -> Phantoms.TypedTerm Syntax.Value
vertexPropertyValueValue x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.VertexPropertyValue"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

vertexPropertyValueWithId :: Phantoms.TypedTerm Syntax.VertexPropertyValue -> Phantoms.TypedTerm (Maybe Syntax.Value) -> Phantoms.TypedTerm Syntax.VertexPropertyValue
vertexPropertyValueWithId original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.VertexPropertyValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.VertexPropertyValue"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

vertexPropertyValueWithValue :: Phantoms.TypedTerm Syntax.VertexPropertyValue -> Phantoms.TypedTerm Syntax.Value -> Phantoms.TypedTerm Syntax.VertexPropertyValue
vertexPropertyValueWithValue original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.VertexPropertyValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.VertexPropertyValue"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

vertexWithId :: Phantoms.TypedTerm Syntax.Vertex -> Phantoms.TypedTerm Syntax.Value -> Phantoms.TypedTerm Syntax.Vertex
vertexWithId original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionFieldName = (Core.Name "inEdges")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionFieldName = (Core.Name "outEdges")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

vertexWithInEdges :: Phantoms.TypedTerm Syntax.Vertex -> Phantoms.TypedTerm (M.Map Syntax.EdgeLabel [Syntax.AdjacentEdge]) -> Phantoms.TypedTerm Syntax.Vertex
vertexWithInEdges original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdges"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "outEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionFieldName = (Core.Name "outEdges")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

vertexWithLabel :: Phantoms.TypedTerm Syntax.Vertex -> Phantoms.TypedTerm (Maybe Syntax.VertexLabel) -> Phantoms.TypedTerm Syntax.Vertex
vertexWithLabel original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionFieldName = (Core.Name "inEdges")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionFieldName = (Core.Name "outEdges")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

vertexWithOutEdges :: Phantoms.TypedTerm Syntax.Vertex -> Phantoms.TypedTerm (M.Map Syntax.EdgeLabel [Syntax.AdjacentEdge]) -> Phantoms.TypedTerm Syntax.Vertex
vertexWithOutEdges original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionFieldName = (Core.Name "inEdges")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdges"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

vertexWithProperties :: Phantoms.TypedTerm Syntax.Vertex -> Phantoms.TypedTerm (M.Map Syntax.PropertyKey [Syntax.VertexPropertyValue]) -> Phantoms.TypedTerm Syntax.Vertex
vertexWithProperties original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionFieldName = (Core.Name "inEdges")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionFieldName = (Core.Name "outEdges")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
