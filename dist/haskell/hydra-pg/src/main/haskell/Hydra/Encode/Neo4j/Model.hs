-- Note: this is an automatically generated file. Do not edit.
-- | Term encoders for hydra.neo4j.model

module Hydra.Encode.Neo4j.Model where
import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Overlay.Haskell.Lib.Lists as Lists
import qualified Hydra.Overlay.Haskell.Lib.Maps as Maps
import qualified Hydra.Overlay.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Overlay.Haskell.Lib.Sets as Sets
import qualified Hydra.Neo4j.Model as Model
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Encoder for hydra.neo4j.model.Constraint
constraint :: Model.Constraint -> Core.Term
constraint x =
    case x of
      Model.ConstraintKey v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.Constraint"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (keyConstraint v0)}})
      Model.ConstraintPropertyExistence v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.Constraint"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "propertyExistence"),
          Core.fieldTerm = (propertyExistenceConstraint v0)}})
      Model.ConstraintPropertyType v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.Constraint"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "propertyType"),
          Core.fieldTerm = (propertyTypeConstraint v0)}})
      Model.ConstraintPropertyUniqueness v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.Constraint"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "propertyUniqueness"),
          Core.fieldTerm = (propertyUniquenessConstraint v0)}})
-- | Encoder for hydra.neo4j.model.ConstraintDefinition
constraintDefinition :: Model.ConstraintDefinition -> Core.Term
constraintDefinition x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.neo4j.model.ConstraintDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map (\x2 -> Core.TermLiteral (Core.LiteralString x2)) opt)) (Model.constraintDefinitionName x))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (constraint (Model.constraintDefinitionBody x))}]})
-- | Encoder for hydra.neo4j.model.Element
element :: Model.Element -> Core.Term
element x =
    case x of
      Model.ElementNode v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.Element"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "node"),
          Core.fieldTerm = (node v0)}})
      Model.ElementRelationship v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.Element"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "relationship"),
          Core.fieldTerm = (relationship v0)}})
-- | Encoder for hydra.neo4j.model.ElementId
elementId :: Model.ElementId -> Core.Term
elementId x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.neo4j.model.ElementId"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Model.unElementId x))})
-- | Encoder for hydra.neo4j.model.GraphType
graphType :: Model.GraphType -> Core.Term
graphType x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.neo4j.model.GraphType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodes"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map nodeElementType xs)) (Model.graphTypeNodes x))},
        Core.Field {
          Core.fieldName = (Core.Name "relationships"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map relationshipElementType xs)) (Model.graphTypeRelationships x))}]})
-- | Encoder for hydra.neo4j.model.IsoDuration
isoDuration :: Model.IsoDuration -> Core.Term
isoDuration x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.neo4j.model.IsoDuration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "months"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 x2))) (Model.isoDurationMonths x))},
        Core.Field {
          Core.fieldName = (Core.Name "days"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 x2))) (Model.isoDurationDays x))},
        Core.Field {
          Core.fieldName = (Core.Name "seconds"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 x2))) (Model.isoDurationSeconds x))},
        Core.Field {
          Core.fieldName = (Core.Name "nanoseconds"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x2))) (Model.isoDurationNanoseconds x))}]})
-- | Encoder for hydra.neo4j.model.Key
key :: Model.Key -> Core.Term
key x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.neo4j.model.Key"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Model.unKey x))})
-- | Encoder for hydra.neo4j.model.KeyConstraint
keyConstraint :: Model.KeyConstraint -> Core.Term
keyConstraint x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.neo4j.model.KeyConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map key xs)) (Model.keyConstraintProperties x))}]})
-- | Encoder for hydra.neo4j.model.LocalDate
localDate :: Model.LocalDate -> Core.Term
localDate x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.neo4j.model.LocalDate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "year"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x2))) (Model.localDateYear x))},
        Core.Field {
          Core.fieldName = (Core.Name "month"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x2))) (Model.localDateMonth x))},
        Core.Field {
          Core.fieldName = (Core.Name "day"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x2))) (Model.localDateDay x))}]})
-- | Encoder for hydra.neo4j.model.LocalDateTime
localDateTime :: Model.LocalDateTime -> Core.Term
localDateTime x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.neo4j.model.LocalDateTime"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "date"),
          Core.fieldTerm = (localDate (Model.localDateTimeDate x))},
        Core.Field {
          Core.fieldName = (Core.Name "time"),
          Core.fieldTerm = (localTime (Model.localDateTimeTime x))}]})
-- | Encoder for hydra.neo4j.model.LocalTime
localTime :: Model.LocalTime -> Core.Term
localTime x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.neo4j.model.LocalTime"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "hour"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x2))) (Model.localTimeHour x))},
        Core.Field {
          Core.fieldName = (Core.Name "minute"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x2))) (Model.localTimeMinute x))},
        Core.Field {
          Core.fieldName = (Core.Name "second"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x2))) (Model.localTimeSecond x))},
        Core.Field {
          Core.fieldName = (Core.Name "nanosecond"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x2))) (Model.localTimeNanosecond x))}]})
-- | Encoder for hydra.neo4j.model.Node
node :: Model.Node -> Core.Term
node x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.neo4j.model.Node"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (elementId (Model.nodeId x))},
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = ((\s -> Core.TermSet (Sets.map nodeLabel s)) (Model.nodeLabels x))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = ((\m -> Core.TermMap (Maps.bimap key value m)) (Model.nodeProperties x))}]})
-- | Encoder for hydra.neo4j.model.NodeElementType
nodeElementType :: Model.NodeElementType -> Core.Term
nodeElementType x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.neo4j.model.NodeElementType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "identifyingLabel"),
          Core.fieldTerm = (nodeLabel (Model.nodeElementTypeIdentifyingLabel x))},
        Core.Field {
          Core.fieldName = (Core.Name "impliedLabels"),
          Core.fieldTerm = ((\s -> Core.TermSet (Sets.map nodeLabel s)) (Model.nodeElementTypeImpliedLabels x))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map constraintDefinition xs)) (Model.nodeElementTypeConstraints x))}]})
-- | Encoder for hydra.neo4j.model.NodeLabel
nodeLabel :: Model.NodeLabel -> Core.Term
nodeLabel x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.neo4j.model.NodeLabel"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Model.unNodeLabel x))})
-- | Encoder for hydra.neo4j.model.OffsetTime
offsetTime :: Model.OffsetTime -> Core.Term
offsetTime x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.neo4j.model.OffsetTime"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "time"),
          Core.fieldTerm = (localTime (Model.offsetTimeTime x))},
        Core.Field {
          Core.fieldName = (Core.Name "offset"),
          Core.fieldTerm = (zoneOffset (Model.offsetTimeOffset x))}]})
-- | Encoder for hydra.neo4j.model.Path
path :: Model.Path -> Core.Term
path x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.neo4j.model.Path"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (node (Model.pathStart x))},
        Core.Field {
          Core.fieldName = (Core.Name "segments"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map segment xs)) (Model.pathSegments x))}]})
-- | Encoder for hydra.neo4j.model.Point
point :: Model.Point -> Core.Term
point x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.neo4j.model.Point"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "srid"),
          Core.fieldTerm = (spatialReferenceIdentifier (Model.pointSrid x))},
        Core.Field {
          Core.fieldName = (Core.Name "x"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 x2))) (Model.pointX x))},
        Core.Field {
          Core.fieldName = (Core.Name "y"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 x2))) (Model.pointY x))},
        Core.Field {
          Core.fieldName = (Core.Name "z"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map (\x2 -> Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 x2))) opt)) (Model.pointZ x))}]})
-- | Encoder for hydra.neo4j.model.PropertyExistenceConstraint
propertyExistenceConstraint :: Model.PropertyExistenceConstraint -> Core.Term
propertyExistenceConstraint x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.neo4j.model.PropertyExistenceConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (key (Model.propertyExistenceConstraintProperty x))}]})
-- | Encoder for hydra.neo4j.model.PropertyTypeConstraint
propertyTypeConstraint :: Model.PropertyTypeConstraint -> Core.Term
propertyTypeConstraint x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.neo4j.model.PropertyTypeConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (key (Model.propertyTypeConstraintProperty x))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (valueType (Model.propertyTypeConstraintType x))}]})
-- | Encoder for hydra.neo4j.model.PropertyUniquenessConstraint
propertyUniquenessConstraint :: Model.PropertyUniquenessConstraint -> Core.Term
propertyUniquenessConstraint x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.neo4j.model.PropertyUniquenessConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map key xs)) (Model.propertyUniquenessConstraintProperties x))}]})
-- | Encoder for hydra.neo4j.model.Relationship
relationship :: Model.Relationship -> Core.Term
relationship x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.neo4j.model.Relationship"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (elementId (Model.relationshipId x))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = ((\m -> Core.TermMap (Maps.bimap key value m)) (Model.relationshipProperties x))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (relationshipType (Model.relationshipType x))},
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (elementId (Model.relationshipStart x))},
        Core.Field {
          Core.fieldName = (Core.Name "end"),
          Core.fieldTerm = (elementId (Model.relationshipEnd x))}]})
-- | Encoder for hydra.neo4j.model.RelationshipElementType
relationshipElementType :: Model.RelationshipElementType -> Core.Term
relationshipElementType x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.neo4j.model.RelationshipElementType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (relationshipType (Model.relationshipElementTypeType x))},
        Core.Field {
          Core.fieldName = (Core.Name "startLabel"),
          Core.fieldTerm = (nodeLabel (Model.relationshipElementTypeStartLabel x))},
        Core.Field {
          Core.fieldName = (Core.Name "endLabel"),
          Core.fieldTerm = (nodeLabel (Model.relationshipElementTypeEndLabel x))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map constraintDefinition xs)) (Model.relationshipElementTypeConstraints x))}]})
-- | Encoder for hydra.neo4j.model.RelationshipType
relationshipType :: Model.RelationshipType -> Core.Term
relationshipType x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.neo4j.model.RelationshipType"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Model.unRelationshipType x))})
-- | Encoder for hydra.neo4j.model.Segment
segment :: Model.Segment -> Core.Term
segment x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.neo4j.model.Segment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "relationship"),
          Core.fieldTerm = (relationship (Model.segmentRelationship x))},
        Core.Field {
          Core.fieldName = (Core.Name "end"),
          Core.fieldTerm = (node (Model.segmentEnd x))}]})
-- | Encoder for hydra.neo4j.model.SpatialReferenceIdentifier
spatialReferenceIdentifier :: Model.SpatialReferenceIdentifier -> Core.Term
spatialReferenceIdentifier x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.neo4j.model.SpatialReferenceIdentifier"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x2))) (Model.unSpatialReferenceIdentifier x))})
-- | Encoder for hydra.neo4j.model.Value
value :: Model.Value -> Core.Term
value x =
    case x of
      Model.ValueBoolean v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.Value"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "boolean"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralBoolean v0))}})
      Model.ValueBytes v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.Value"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "bytes"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralBinary v0))}})
      Model.ValueDate v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.Value"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "date"),
          Core.fieldTerm = (localDate v0)}})
      Model.ValueDateTime v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.Value"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "dateTime"),
          Core.fieldTerm = (zonedDateTime v0)}})
      Model.ValueDuration v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.Value"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "duration"),
          Core.fieldTerm = (isoDuration v0)}})
      Model.ValueFloat v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.Value"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "float"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 v0)))}})
      Model.ValueInteger v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.Value"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "integer"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 v0)))}})
      Model.ValueList v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.Value"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "list"),
          Core.fieldTerm = (Core.TermList (Lists.map value v0))}})
      Model.ValueLocalDateTime v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.Value"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "localDateTime"),
          Core.fieldTerm = (localDateTime v0)}})
      Model.ValueLocalTime v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.Value"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "localTime"),
          Core.fieldTerm = (localTime v0)}})
      Model.ValueMap v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.Value"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "map"),
          Core.fieldTerm = (Core.TermMap (Maps.bimap key value v0))}})
      Model.ValueNode v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.Value"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "node"),
          Core.fieldTerm = (node v0)}})
      Model.ValueNull -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.Value"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "null"),
          Core.fieldTerm = Core.TermUnit}})
      Model.ValuePath v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.Value"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (path v0)}})
      Model.ValuePoint v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.Value"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "point"),
          Core.fieldTerm = (point v0)}})
      Model.ValueRelationship v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.Value"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "relationship"),
          Core.fieldTerm = (relationship v0)}})
      Model.ValueString v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.Value"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralString v0))}})
      Model.ValueTime v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.Value"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "time"),
          Core.fieldTerm = (offsetTime v0)}})
-- | Encoder for hydra.neo4j.model.ValueType
valueType :: Model.ValueType -> Core.Term
valueType x =
    case x of
      Model.ValueTypeBoolean -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.ValueType"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "boolean"),
          Core.fieldTerm = Core.TermUnit}})
      Model.ValueTypeString -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.ValueType"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = Core.TermUnit}})
      Model.ValueTypeInteger -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.ValueType"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "integer"),
          Core.fieldTerm = Core.TermUnit}})
      Model.ValueTypeFloat -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.ValueType"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "float"),
          Core.fieldTerm = Core.TermUnit}})
      Model.ValueTypeDate -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.ValueType"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "date"),
          Core.fieldTerm = Core.TermUnit}})
      Model.ValueTypeLocalTime -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.ValueType"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "localTime"),
          Core.fieldTerm = Core.TermUnit}})
      Model.ValueTypeZonedTime -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.ValueType"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "zonedTime"),
          Core.fieldTerm = Core.TermUnit}})
      Model.ValueTypeLocalDateTime -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.ValueType"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "localDateTime"),
          Core.fieldTerm = Core.TermUnit}})
      Model.ValueTypeZonedDateTime -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.ValueType"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "zonedDateTime"),
          Core.fieldTerm = Core.TermUnit}})
      Model.ValueTypeDuration -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.ValueType"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "duration"),
          Core.fieldTerm = Core.TermUnit}})
      Model.ValueTypePoint -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.ValueType"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "point"),
          Core.fieldTerm = Core.TermUnit}})
      Model.ValueTypeList v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.ValueType"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "list"),
          Core.fieldTerm = (valueType v0)}})
      Model.ValueTypeVector v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.ValueType"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "vector"),
          Core.fieldTerm = (vectorType v0)}})
      Model.ValueTypeUnion v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.neo4j.model.ValueType"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "union"),
          Core.fieldTerm = (Core.TermList (Lists.map valueType v0))}})
-- | Encoder for hydra.neo4j.model.VectorType
vectorType :: Model.VectorType -> Core.Term
vectorType x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.neo4j.model.VectorType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (valueType (Model.vectorTypeElement x))},
        Core.Field {
          Core.fieldName = (Core.Name "dimension"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x2))) (Model.vectorTypeDimension x))}]})
-- | Encoder for hydra.neo4j.model.ZoneId
zoneId :: Model.ZoneId -> Core.Term
zoneId x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.neo4j.model.ZoneId"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Model.unZoneId x))})
-- | Encoder for hydra.neo4j.model.ZoneOffset
zoneOffset :: Model.ZoneOffset -> Core.Term
zoneOffset x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.neo4j.model.ZoneOffset"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x2))) (Model.unZoneOffset x))})
-- | Encoder for hydra.neo4j.model.ZonedDateTime
zonedDateTime :: Model.ZonedDateTime -> Core.Term
zonedDateTime x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.neo4j.model.ZonedDateTime"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "dateTime"),
          Core.fieldTerm = (localDateTime (Model.zonedDateTimeDateTime x))},
        Core.Field {
          Core.fieldName = (Core.Name "offset"),
          Core.fieldTerm = (zoneOffset (Model.zonedDateTimeOffset x))},
        Core.Field {
          Core.fieldName = (Core.Name "zone"),
          Core.fieldTerm = (zoneId (Model.zonedDateTimeZone x))}]})
