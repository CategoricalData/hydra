-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.pg.graphson.syntax

module Hydra.Dsl.Pg.Graphson.Syntax where

import qualified Hydra.Core as Core
import qualified Hydra.Pg.Graphson.Syntax as Syntax
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

bigDecimalValue :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.BigDecimalValue
bigDecimalValue x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.graphson.syntax.BigDecimalValue"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unBigDecimalValue :: Phantoms.TTerm Syntax.BigDecimalValue -> Phantoms.TTerm String
unBigDecimalValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.pg.graphson.syntax.BigDecimalValue")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

compositeTypedValue :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.Map -> Phantoms.TTerm Syntax.CompositeTypedValue
compositeTypedValue type_ fields =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.CompositeTypedValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm fields)}]}))

compositeTypedValueType :: Phantoms.TTerm Syntax.CompositeTypedValue -> Phantoms.TTerm Syntax.TypeName
compositeTypedValueType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.CompositeTypedValue"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

compositeTypedValueFields :: Phantoms.TTerm Syntax.CompositeTypedValue -> Phantoms.TTerm Syntax.Map
compositeTypedValueFields x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.CompositeTypedValue"),
        Core.projectionField = (Core.Name "fields")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

compositeTypedValueWithType :: Phantoms.TTerm Syntax.CompositeTypedValue -> Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.CompositeTypedValue
compositeTypedValueWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.CompositeTypedValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.CompositeTypedValue"),
              Core.projectionField = (Core.Name "fields")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

compositeTypedValueWithFields :: Phantoms.TTerm Syntax.CompositeTypedValue -> Phantoms.TTerm Syntax.Map -> Phantoms.TTerm Syntax.CompositeTypedValue
compositeTypedValueWithFields original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.CompositeTypedValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.CompositeTypedValue"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dateTime :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.DateTime
dateTime x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.graphson.syntax.DateTime"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unDateTime :: Phantoms.TTerm Syntax.DateTime -> Phantoms.TTerm String
unDateTime x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.pg.graphson.syntax.DateTime")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

doubleValueFinite :: Phantoms.TTerm Double -> Phantoms.TTerm Syntax.DoubleValue
doubleValueFinite x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.DoubleValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "finite"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

doubleValueInfinity :: Phantoms.TTerm Syntax.DoubleValue
doubleValueInfinity =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.DoubleValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "infinity"),
        Core.fieldTerm = Core.TermUnit}}))

doubleValueNegativeInfinity :: Phantoms.TTerm Syntax.DoubleValue
doubleValueNegativeInfinity =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.DoubleValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "negativeInfinity"),
        Core.fieldTerm = Core.TermUnit}}))

doubleValueNotANumber :: Phantoms.TTerm Syntax.DoubleValue
doubleValueNotANumber =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.DoubleValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notANumber"),
        Core.fieldTerm = Core.TermUnit}}))

duration :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Duration
duration x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.graphson.syntax.Duration"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unDuration :: Phantoms.TTerm Syntax.Duration -> Phantoms.TTerm String
unDuration x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.pg.graphson.syntax.Duration")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgeLabel :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.EdgeLabel
edgeLabel x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.graphson.syntax.EdgeLabel"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unEdgeLabel :: Phantoms.TTerm Syntax.EdgeLabel -> Phantoms.TTerm String
unEdgeLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.pg.graphson.syntax.EdgeLabel")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

floatValueFinite :: Phantoms.TTerm Float -> Phantoms.TTerm Syntax.FloatValue
floatValueFinite x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.FloatValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "finite"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

floatValueInfinity :: Phantoms.TTerm Syntax.FloatValue
floatValueInfinity =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.FloatValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "infinity"),
        Core.fieldTerm = Core.TermUnit}}))

floatValueNegativeInfinity :: Phantoms.TTerm Syntax.FloatValue
floatValueNegativeInfinity =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.FloatValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "negativeInfinity"),
        Core.fieldTerm = Core.TermUnit}}))

floatValueNotANumber :: Phantoms.TTerm Syntax.FloatValue
floatValueNotANumber =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.FloatValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notANumber"),
        Core.fieldTerm = Core.TermUnit}}))

map :: Phantoms.TTerm [Syntax.ValuePair] -> Phantoms.TTerm Syntax.Map
map x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.graphson.syntax.Map"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unMap :: Phantoms.TTerm Syntax.Map -> Phantoms.TTerm [Syntax.ValuePair]
unMap x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.pg.graphson.syntax.Map")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

adjacentEdge :: Phantoms.TTerm Syntax.Value -> Phantoms.TTerm Syntax.Value -> Phantoms.TTerm (M.Map Syntax.PropertyKey Syntax.Value) -> Phantoms.TTerm Syntax.AdjacentEdge
adjacentEdge id vertexId properties =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.AdjacentEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Phantoms.unTTerm vertexId)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm properties)}]}))

adjacentEdgeId :: Phantoms.TTerm Syntax.AdjacentEdge -> Phantoms.TTerm Syntax.Value
adjacentEdgeId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.AdjacentEdge"),
        Core.projectionField = (Core.Name "id")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

adjacentEdgeVertexId :: Phantoms.TTerm Syntax.AdjacentEdge -> Phantoms.TTerm Syntax.Value
adjacentEdgeVertexId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.AdjacentEdge"),
        Core.projectionField = (Core.Name "vertexId")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

adjacentEdgeProperties :: Phantoms.TTerm Syntax.AdjacentEdge -> Phantoms.TTerm (M.Map Syntax.PropertyKey Syntax.Value)
adjacentEdgeProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.AdjacentEdge"),
        Core.projectionField = (Core.Name "properties")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

adjacentEdgeWithId :: Phantoms.TTerm Syntax.AdjacentEdge -> Phantoms.TTerm Syntax.Value -> Phantoms.TTerm Syntax.AdjacentEdge
adjacentEdgeWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.AdjacentEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.AdjacentEdge"),
              Core.projectionField = (Core.Name "vertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.AdjacentEdge"),
              Core.projectionField = (Core.Name "properties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

adjacentEdgeWithVertexId :: Phantoms.TTerm Syntax.AdjacentEdge -> Phantoms.TTerm Syntax.Value -> Phantoms.TTerm Syntax.AdjacentEdge
adjacentEdgeWithVertexId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.AdjacentEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.AdjacentEdge"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.AdjacentEdge"),
              Core.projectionField = (Core.Name "properties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

adjacentEdgeWithProperties :: Phantoms.TTerm Syntax.AdjacentEdge -> Phantoms.TTerm (M.Map Syntax.PropertyKey Syntax.Value) -> Phantoms.TTerm Syntax.AdjacentEdge
adjacentEdgeWithProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.AdjacentEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.AdjacentEdge"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertexId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.AdjacentEdge"),
              Core.projectionField = (Core.Name "vertexId")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

primitiveTypedValue :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.PrimitiveTypedValue
primitiveTypedValue type_ value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.PrimitiveTypedValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

primitiveTypedValueType :: Phantoms.TTerm Syntax.PrimitiveTypedValue -> Phantoms.TTerm Syntax.TypeName
primitiveTypedValueType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.PrimitiveTypedValue"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

primitiveTypedValueValue :: Phantoms.TTerm Syntax.PrimitiveTypedValue -> Phantoms.TTerm String
primitiveTypedValueValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.PrimitiveTypedValue"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

primitiveTypedValueWithType :: Phantoms.TTerm Syntax.PrimitiveTypedValue -> Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm Syntax.PrimitiveTypedValue
primitiveTypedValueWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.PrimitiveTypedValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.PrimitiveTypedValue"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

primitiveTypedValueWithValue :: Phantoms.TTerm Syntax.PrimitiveTypedValue -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.PrimitiveTypedValue
primitiveTypedValueWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.PrimitiveTypedValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.PrimitiveTypedValue"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

propertyKey :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.PropertyKey
propertyKey x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.graphson.syntax.PropertyKey"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unPropertyKey :: Phantoms.TTerm Syntax.PropertyKey -> Phantoms.TTerm String
unPropertyKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.pg.graphson.syntax.PropertyKey")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeName :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.TypeName
typeName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.graphson.syntax.TypeName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unTypeName :: Phantoms.TTerm Syntax.TypeName -> Phantoms.TTerm String
unTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.pg.graphson.syntax.TypeName")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

uuid :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Uuid
uuid x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.graphson.syntax.Uuid"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unUuid :: Phantoms.TTerm Syntax.Uuid -> Phantoms.TTerm String
unUuid x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.pg.graphson.syntax.Uuid")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

valueBigDecimal :: Phantoms.TTerm Syntax.BigDecimalValue -> Phantoms.TTerm Syntax.Value
valueBigDecimal x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bigDecimal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueBigInteger :: Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.Value
valueBigInteger x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bigInteger"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueBinary :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Value
valueBinary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "binary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueBoolean :: Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Value
valueBoolean x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueByte :: Phantoms.TTerm I.Int16 -> Phantoms.TTerm Syntax.Value
valueByte x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "byte"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueChar :: Phantoms.TTerm I.Int64 -> Phantoms.TTerm Syntax.Value
valueChar x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "char"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueComposite :: Phantoms.TTerm Syntax.CompositeTypedValue -> Phantoms.TTerm Syntax.Value
valueComposite x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "composite"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueDateTime :: Phantoms.TTerm Syntax.DateTime -> Phantoms.TTerm Syntax.Value
valueDateTime x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dateTime"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueDouble :: Phantoms.TTerm Syntax.DoubleValue -> Phantoms.TTerm Syntax.Value
valueDouble x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "double"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueDuration :: Phantoms.TTerm Syntax.Duration -> Phantoms.TTerm Syntax.Value
valueDuration x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duration"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueFloat :: Phantoms.TTerm Syntax.FloatValue -> Phantoms.TTerm Syntax.Value
valueFloat x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueInteger :: Phantoms.TTerm Int -> Phantoms.TTerm Syntax.Value
valueInteger x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueList :: Phantoms.TTerm [Syntax.Value] -> Phantoms.TTerm Syntax.Value
valueList x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueLong :: Phantoms.TTerm I.Int64 -> Phantoms.TTerm Syntax.Value
valueLong x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "long"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueMap :: Phantoms.TTerm Syntax.Map -> Phantoms.TTerm Syntax.Value
valueMap x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueNull :: Phantoms.TTerm Syntax.Value
valueNull =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = Core.TermUnit}}))

valuePrimitive :: Phantoms.TTerm Syntax.PrimitiveTypedValue -> Phantoms.TTerm Syntax.Value
valuePrimitive x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueSet :: Phantoms.TTerm [Syntax.Value] -> Phantoms.TTerm Syntax.Value
valueSet x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueShort :: Phantoms.TTerm I.Int16 -> Phantoms.TTerm Syntax.Value
valueShort x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "short"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueString :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Value
valueString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valueUuid :: Phantoms.TTerm Syntax.Uuid -> Phantoms.TTerm Syntax.Value
valueUuid x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Value"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uuid"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

valuePair :: Phantoms.TTerm Syntax.Value -> Phantoms.TTerm Syntax.Value -> Phantoms.TTerm Syntax.ValuePair
valuePair first second =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.ValuePair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTTerm first)},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Phantoms.unTTerm second)}]}))

valuePairFirst :: Phantoms.TTerm Syntax.ValuePair -> Phantoms.TTerm Syntax.Value
valuePairFirst x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.ValuePair"),
        Core.projectionField = (Core.Name "first")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

valuePairSecond :: Phantoms.TTerm Syntax.ValuePair -> Phantoms.TTerm Syntax.Value
valuePairSecond x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.ValuePair"),
        Core.projectionField = (Core.Name "second")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

valuePairWithFirst :: Phantoms.TTerm Syntax.ValuePair -> Phantoms.TTerm Syntax.Value -> Phantoms.TTerm Syntax.ValuePair
valuePairWithFirst original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.ValuePair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.ValuePair"),
              Core.projectionField = (Core.Name "second")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

valuePairWithSecond :: Phantoms.TTerm Syntax.ValuePair -> Phantoms.TTerm Syntax.Value -> Phantoms.TTerm Syntax.ValuePair
valuePairWithSecond original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.ValuePair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "first"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.ValuePair"),
              Core.projectionField = (Core.Name "first")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

vertex :: Phantoms.TTerm Syntax.Value -> Phantoms.TTerm (Maybe Syntax.VertexLabel) -> Phantoms.TTerm (M.Map Syntax.EdgeLabel [Syntax.AdjacentEdge]) -> Phantoms.TTerm (M.Map Syntax.EdgeLabel [Syntax.AdjacentEdge]) -> Phantoms.TTerm (M.Map Syntax.PropertyKey [Syntax.VertexPropertyValue]) -> Phantoms.TTerm Syntax.Vertex
vertex id label inEdges outEdges properties =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "inEdges"),
          Core.fieldTerm = (Phantoms.unTTerm inEdges)},
        Core.Field {
          Core.fieldName = (Core.Name "outEdges"),
          Core.fieldTerm = (Phantoms.unTTerm outEdges)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm properties)}]}))

vertexId :: Phantoms.TTerm Syntax.Vertex -> Phantoms.TTerm Syntax.Value
vertexId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
        Core.projectionField = (Core.Name "id")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexLabel :: Phantoms.TTerm Syntax.Vertex -> Phantoms.TTerm (Maybe Syntax.VertexLabel)
vertexLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
        Core.projectionField = (Core.Name "label")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexInEdges :: Phantoms.TTerm Syntax.Vertex -> Phantoms.TTerm (M.Map Syntax.EdgeLabel [Syntax.AdjacentEdge])
vertexInEdges x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
        Core.projectionField = (Core.Name "inEdges")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexOutEdges :: Phantoms.TTerm Syntax.Vertex -> Phantoms.TTerm (M.Map Syntax.EdgeLabel [Syntax.AdjacentEdge])
vertexOutEdges x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
        Core.projectionField = (Core.Name "outEdges")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexProperties :: Phantoms.TTerm Syntax.Vertex -> Phantoms.TTerm (M.Map Syntax.PropertyKey [Syntax.VertexPropertyValue])
vertexProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
        Core.projectionField = (Core.Name "properties")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexWithId :: Phantoms.TTerm Syntax.Vertex -> Phantoms.TTerm Syntax.Value -> Phantoms.TTerm Syntax.Vertex
vertexWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionField = (Core.Name "label")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionField = (Core.Name "inEdges")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionField = (Core.Name "outEdges")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionField = (Core.Name "properties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

vertexWithLabel :: Phantoms.TTerm Syntax.Vertex -> Phantoms.TTerm (Maybe Syntax.VertexLabel) -> Phantoms.TTerm Syntax.Vertex
vertexWithLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionField = (Core.Name "inEdges")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionField = (Core.Name "outEdges")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionField = (Core.Name "properties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

vertexWithInEdges :: Phantoms.TTerm Syntax.Vertex -> Phantoms.TTerm (M.Map Syntax.EdgeLabel [Syntax.AdjacentEdge]) -> Phantoms.TTerm Syntax.Vertex
vertexWithInEdges original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionField = (Core.Name "label")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdges"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "outEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionField = (Core.Name "outEdges")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionField = (Core.Name "properties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

vertexWithOutEdges :: Phantoms.TTerm Syntax.Vertex -> Phantoms.TTerm (M.Map Syntax.EdgeLabel [Syntax.AdjacentEdge]) -> Phantoms.TTerm Syntax.Vertex
vertexWithOutEdges original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionField = (Core.Name "label")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionField = (Core.Name "inEdges")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdges"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionField = (Core.Name "properties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

vertexWithProperties :: Phantoms.TTerm Syntax.Vertex -> Phantoms.TTerm (M.Map Syntax.PropertyKey [Syntax.VertexPropertyValue]) -> Phantoms.TTerm Syntax.Vertex
vertexWithProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionField = (Core.Name "label")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionField = (Core.Name "inEdges")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outEdges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.Vertex"),
              Core.projectionField = (Core.Name "outEdges")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

vertexLabel_ :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.VertexLabel
vertexLabel_ x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.graphson.syntax.VertexLabel"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unVertexLabel :: Phantoms.TTerm Syntax.VertexLabel -> Phantoms.TTerm String
unVertexLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.pg.graphson.syntax.VertexLabel")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexPropertyValue :: Phantoms.TTerm (Maybe Syntax.Value) -> Phantoms.TTerm Syntax.Value -> Phantoms.TTerm Syntax.VertexPropertyValue
vertexPropertyValue id value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.VertexPropertyValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

vertexPropertyValueId :: Phantoms.TTerm Syntax.VertexPropertyValue -> Phantoms.TTerm (Maybe Syntax.Value)
vertexPropertyValueId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.VertexPropertyValue"),
        Core.projectionField = (Core.Name "id")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexPropertyValueValue :: Phantoms.TTerm Syntax.VertexPropertyValue -> Phantoms.TTerm Syntax.Value
vertexPropertyValueValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.VertexPropertyValue"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexPropertyValueWithId :: Phantoms.TTerm Syntax.VertexPropertyValue -> Phantoms.TTerm (Maybe Syntax.Value) -> Phantoms.TTerm Syntax.VertexPropertyValue
vertexPropertyValueWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.VertexPropertyValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.VertexPropertyValue"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

vertexPropertyValueWithValue :: Phantoms.TTerm Syntax.VertexPropertyValue -> Phantoms.TTerm Syntax.Value -> Phantoms.TTerm Syntax.VertexPropertyValue
vertexPropertyValueWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.graphson.syntax.VertexPropertyValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.graphson.syntax.VertexPropertyValue"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
