-- Note: this is an automatically generated file. Do not edit.
-- | Term decoders for hydra.neo4j.model

module Hydra.Decode.Neo4j.Model where
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Overlay.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Overlay.Haskell.Lib.Maps as Maps
import qualified Hydra.Overlay.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Overlay.Haskell.Lib.Strings as Strings
import qualified Hydra.Neo4j.Model as Model
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Decoder for hydra.neo4j.model.Constraint
constraint :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.Constraint
constraint cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "key", (\input -> Eithers.map (\t -> Model.ConstraintKey t) (keyConstraint cx input))),
                      (
                        Core.Name "propertyExistence",
                        (\input -> Eithers.map (\t -> Model.ConstraintPropertyExistence t) (propertyExistenceConstraint cx input))),
                      (
                        Core.Name "propertyType",
                        (\input -> Eithers.map (\t -> Model.ConstraintPropertyType t) (propertyTypeConstraint cx input))),
                      (
                        Core.Name "propertyUniqueness",
                        (\input -> Eithers.map (\t -> Model.ConstraintPropertyUniqueness t) (propertyUniquenessConstraint cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.ConstraintDefinition
constraintDefinition :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.ConstraintDefinition
constraintDefinition cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" (ExtractCore.decodeMaybe (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2))) fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "body" constraint fieldMap cx) (\field_body -> Right (Model.ConstraintDefinition {
          Model.constraintDefinitionName = field_name,
          Model.constraintDefinitionBody = field_body}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.neo4j.model.ConstraintDefinition")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.Element
element :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.Element
element cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "node", (\input -> Eithers.map (\t -> Model.ElementNode t) (node cx input))),
                      (Core.Name "relationship", (\input -> Eithers.map (\t -> Model.ElementRelationship t) (relationship cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.ElementId
elementId :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.ElementId
elementId cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Model.ElementId b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.GraphType
graphType :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.GraphType
graphType cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "nodes" (ExtractCore.decodeList nodeElementType) fieldMap cx) (\field_nodes -> Eithers.bind (ExtractCore.requireField "relationships" (ExtractCore.decodeList relationshipElementType) fieldMap cx) (\field_relationships -> Right (Model.GraphType {
          Model.graphTypeNodes = field_nodes,
          Model.graphTypeRelationships = field_relationships}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.neo4j.model.GraphType")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.IsoDuration
isoDuration :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.IsoDuration
isoDuration cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "months" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt64 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected int64 value")
            _ -> Left (Errors.DecodingError "expected int64 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_months -> Eithers.bind (ExtractCore.requireField "days" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt64 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected int64 value")
            _ -> Left (Errors.DecodingError "expected int64 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_days -> Eithers.bind (ExtractCore.requireField "seconds" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt64 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected int64 value")
            _ -> Left (Errors.DecodingError "expected int64 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_seconds -> Eithers.bind (ExtractCore.requireField "nanoseconds" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt32 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected int32 value")
            _ -> Left (Errors.DecodingError "expected int32 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_nanoseconds -> Right (Model.IsoDuration {
          Model.isoDurationMonths = field_months,
          Model.isoDurationDays = field_days,
          Model.isoDurationSeconds = field_seconds,
          Model.isoDurationNanoseconds = field_nanoseconds}))))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.neo4j.model.IsoDuration")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.Key
key :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.Key
key cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Model.Key b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.KeyConstraint
keyConstraint :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.KeyConstraint
keyConstraint cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "properties" (ExtractCore.decodeList key) fieldMap cx) (\field_properties -> Right (Model.KeyConstraint {
          Model.keyConstraintProperties = field_properties})))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.neo4j.model.KeyConstraint")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.LocalDate
localDate :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.LocalDate
localDate cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "year" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt32 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected int32 value")
            _ -> Left (Errors.DecodingError "expected int32 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_year -> Eithers.bind (ExtractCore.requireField "month" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt32 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected int32 value")
            _ -> Left (Errors.DecodingError "expected int32 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_month -> Eithers.bind (ExtractCore.requireField "day" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt32 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected int32 value")
            _ -> Left (Errors.DecodingError "expected int32 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_day -> Right (Model.LocalDate {
          Model.localDateYear = field_year,
          Model.localDateMonth = field_month,
          Model.localDateDay = field_day})))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.neo4j.model.LocalDate")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.LocalDateTime
localDateTime :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.LocalDateTime
localDateTime cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "date" localDate fieldMap cx) (\field_date -> Eithers.bind (ExtractCore.requireField "time" localTime fieldMap cx) (\field_time -> Right (Model.LocalDateTime {
          Model.localDateTimeDate = field_date,
          Model.localDateTimeTime = field_time}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.neo4j.model.LocalDateTime")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.LocalTime
localTime :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.LocalTime
localTime cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "hour" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt32 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected int32 value")
            _ -> Left (Errors.DecodingError "expected int32 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_hour -> Eithers.bind (ExtractCore.requireField "minute" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt32 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected int32 value")
            _ -> Left (Errors.DecodingError "expected int32 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_minute -> Eithers.bind (ExtractCore.requireField "second" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt32 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected int32 value")
            _ -> Left (Errors.DecodingError "expected int32 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_second -> Eithers.bind (ExtractCore.requireField "nanosecond" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt32 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected int32 value")
            _ -> Left (Errors.DecodingError "expected int32 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_nanosecond -> Right (Model.LocalTime {
          Model.localTimeHour = field_hour,
          Model.localTimeMinute = field_minute,
          Model.localTimeSecond = field_second,
          Model.localTimeNanosecond = field_nanosecond}))))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.neo4j.model.LocalTime")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.Node
node :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.Node
node cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "id" elementId fieldMap cx) (\field_id -> Eithers.bind (ExtractCore.requireField "labels" (ExtractCore.decodeSet nodeLabel) fieldMap cx) (\field_labels -> Eithers.bind (ExtractCore.requireField "properties" (ExtractCore.decodeMap key value) fieldMap cx) (\field_properties -> Right (Model.Node {
          Model.nodeId = field_id,
          Model.nodeLabels = field_labels,
          Model.nodeProperties = field_properties})))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.neo4j.model.Node")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.NodeElementType
nodeElementType :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.NodeElementType
nodeElementType cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "identifyingLabel" nodeLabel fieldMap cx) (\field_identifyingLabel -> Eithers.bind (ExtractCore.requireField "impliedLabels" (ExtractCore.decodeSet nodeLabel) fieldMap cx) (\field_impliedLabels -> Eithers.bind (ExtractCore.requireField "constraints" (ExtractCore.decodeList constraintDefinition) fieldMap cx) (\field_constraints -> Right (Model.NodeElementType {
          Model.nodeElementTypeIdentifyingLabel = field_identifyingLabel,
          Model.nodeElementTypeImpliedLabels = field_impliedLabels,
          Model.nodeElementTypeConstraints = field_constraints})))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.neo4j.model.NodeElementType")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.NodeLabel
nodeLabel :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.NodeLabel
nodeLabel cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Model.NodeLabel b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.OffsetTime
offsetTime :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.OffsetTime
offsetTime cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "time" localTime fieldMap cx) (\field_time -> Eithers.bind (ExtractCore.requireField "offset" zoneOffset fieldMap cx) (\field_offset -> Right (Model.OffsetTime {
          Model.offsetTimeTime = field_time,
          Model.offsetTimeOffset = field_offset}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.neo4j.model.OffsetTime")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.Path
path :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.Path
path cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "start" node fieldMap cx) (\field_start -> Eithers.bind (ExtractCore.requireField "segments" (ExtractCore.decodeList segment) fieldMap cx) (\field_segments -> Right (Model.Path {
          Model.pathStart = field_start,
          Model.pathSegments = field_segments}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.neo4j.model.Path")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.Point
point :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.Point
point cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "srid" spatialReferenceIdentifier fieldMap cx) (\field_srid -> Eithers.bind (ExtractCore.requireField "x" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralFloat v2 -> case v2 of
              Core.FloatValueFloat64 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected float64 value")
            _ -> Left (Errors.DecodingError "expected float64 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_x -> Eithers.bind (ExtractCore.requireField "y" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralFloat v2 -> case v2 of
              Core.FloatValueFloat64 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected float64 value")
            _ -> Left (Errors.DecodingError "expected float64 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_y -> Eithers.bind (ExtractCore.requireField "z" (ExtractCore.decodeMaybe (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralFloat v2 -> case v2 of
              Core.FloatValueFloat64 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected float64 value")
            _ -> Left (Errors.DecodingError "expected float64 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2))) fieldMap cx) (\field_z -> Right (Model.Point {
          Model.pointSrid = field_srid,
          Model.pointX = field_x,
          Model.pointY = field_y,
          Model.pointZ = field_z}))))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.neo4j.model.Point")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.PropertyExistenceConstraint
propertyExistenceConstraint :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.PropertyExistenceConstraint
propertyExistenceConstraint cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "property" key fieldMap cx) (\field_property -> Right (Model.PropertyExistenceConstraint {
          Model.propertyExistenceConstraintProperty = field_property})))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.neo4j.model.PropertyExistenceConstraint")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.PropertyTypeConstraint
propertyTypeConstraint :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.PropertyTypeConstraint
propertyTypeConstraint cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "property" key fieldMap cx) (\field_property -> Eithers.bind (ExtractCore.requireField "type" valueType fieldMap cx) (\field_type -> Right (Model.PropertyTypeConstraint {
          Model.propertyTypeConstraintProperty = field_property,
          Model.propertyTypeConstraintType = field_type}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.neo4j.model.PropertyTypeConstraint")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.PropertyUniquenessConstraint
propertyUniquenessConstraint :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.PropertyUniquenessConstraint
propertyUniquenessConstraint cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "properties" (ExtractCore.decodeList key) fieldMap cx) (\field_properties -> Right (Model.PropertyUniquenessConstraint {
          Model.propertyUniquenessConstraintProperties = field_properties})))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.neo4j.model.PropertyUniquenessConstraint")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.Relationship
relationship :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.Relationship
relationship cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "id" elementId fieldMap cx) (\field_id -> Eithers.bind (ExtractCore.requireField "properties" (ExtractCore.decodeMap key value) fieldMap cx) (\field_properties -> Eithers.bind (ExtractCore.requireField "type" relationshipType fieldMap cx) (\field_type -> Eithers.bind (ExtractCore.requireField "start" elementId fieldMap cx) (\field_start -> Eithers.bind (ExtractCore.requireField "end" elementId fieldMap cx) (\field_end -> Right (Model.Relationship {
          Model.relationshipId = field_id,
          Model.relationshipProperties = field_properties,
          Model.relationshipType = field_type,
          Model.relationshipStart = field_start,
          Model.relationshipEnd = field_end})))))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.neo4j.model.Relationship")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.RelationshipElementType
relationshipElementType :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.RelationshipElementType
relationshipElementType cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "type" relationshipType fieldMap cx) (\field_type -> Eithers.bind (ExtractCore.requireField "startLabel" nodeLabel fieldMap cx) (\field_startLabel -> Eithers.bind (ExtractCore.requireField "endLabel" nodeLabel fieldMap cx) (\field_endLabel -> Eithers.bind (ExtractCore.requireField "constraints" (ExtractCore.decodeList constraintDefinition) fieldMap cx) (\field_constraints -> Right (Model.RelationshipElementType {
          Model.relationshipElementTypeType = field_type,
          Model.relationshipElementTypeStartLabel = field_startLabel,
          Model.relationshipElementTypeEndLabel = field_endLabel,
          Model.relationshipElementTypeConstraints = field_constraints}))))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.neo4j.model.RelationshipElementType")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.RelationshipType
relationshipType :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.RelationshipType
relationshipType cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Model.RelationshipType b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.Segment
segment :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.Segment
segment cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "relationship" relationship fieldMap cx) (\field_relationship -> Eithers.bind (ExtractCore.requireField "end" node fieldMap cx) (\field_end -> Right (Model.Segment {
          Model.segmentRelationship = field_relationship,
          Model.segmentEnd = field_end}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.neo4j.model.Segment")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.SpatialReferenceIdentifier
spatialReferenceIdentifier :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.SpatialReferenceIdentifier
spatialReferenceIdentifier cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Model.SpatialReferenceIdentifier b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralInteger v2 -> case v2 of
            Core.IntegerValueInt32 v3 -> Right v3
            _ -> Left (Errors.DecodingError "expected int32 value")
          _ -> Left (Errors.DecodingError "expected int32 literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.Value
value :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.Value
value cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (
                        Core.Name "boolean",
                        (\input -> Eithers.map (\t -> Model.ValueBoolean t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                          Core.TermLiteral v1 -> case v1 of
                            Core.LiteralBoolean v2 -> Right v2
                            _ -> Left (Errors.DecodingError "expected boolean literal")
                          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (
                        Core.Name "bytes",
                        (\input -> Eithers.map (\t -> Model.ValueBytes t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                          Core.TermLiteral v1 -> case v1 of
                            Core.LiteralBinary v2 -> Right v2
                            _ -> Left (Errors.DecodingError "expected binary literal")
                          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (Core.Name "date", (\input -> Eithers.map (\t -> Model.ValueDate t) (localDate cx input))),
                      (Core.Name "dateTime", (\input -> Eithers.map (\t -> Model.ValueDateTime t) (zonedDateTime cx input))),
                      (Core.Name "duration", (\input -> Eithers.map (\t -> Model.ValueDuration t) (isoDuration cx input))),
                      (
                        Core.Name "float",
                        (\input -> Eithers.map (\t -> Model.ValueFloat t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                          Core.TermLiteral v1 -> case v1 of
                            Core.LiteralFloat v2 -> case v2 of
                              Core.FloatValueFloat64 v3 -> Right v3
                              _ -> Left (Errors.DecodingError "expected float64 value")
                            _ -> Left (Errors.DecodingError "expected float64 literal")
                          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (
                        Core.Name "integer",
                        (\input -> Eithers.map (\t -> Model.ValueInteger t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                          Core.TermLiteral v1 -> case v1 of
                            Core.LiteralInteger v2 -> case v2 of
                              Core.IntegerValueInt64 v3 -> Right v3
                              _ -> Left (Errors.DecodingError "expected int64 value")
                            _ -> Left (Errors.DecodingError "expected int64 literal")
                          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (Core.Name "list", (\input -> Eithers.map (\t -> Model.ValueList t) (ExtractCore.decodeList value cx input))),
                      (Core.Name "localDateTime", (\input -> Eithers.map (\t -> Model.ValueLocalDateTime t) (localDateTime cx input))),
                      (Core.Name "localTime", (\input -> Eithers.map (\t -> Model.ValueLocalTime t) (localTime cx input))),
                      (Core.Name "map", (\input -> Eithers.map (\t -> Model.ValueMap t) (ExtractCore.decodeMap key value cx input))),
                      (Core.Name "node", (\input -> Eithers.map (\t -> Model.ValueNode t) (node cx input))),
                      (Core.Name "null", (\input -> Eithers.map (\t -> Model.ValueNull) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "path", (\input -> Eithers.map (\t -> Model.ValuePath t) (path cx input))),
                      (Core.Name "point", (\input -> Eithers.map (\t -> Model.ValuePoint t) (point cx input))),
                      (Core.Name "relationship", (\input -> Eithers.map (\t -> Model.ValueRelationship t) (relationship cx input))),
                      (
                        Core.Name "string",
                        (\input -> Eithers.map (\t -> Model.ValueString t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                          Core.TermLiteral v1 -> case v1 of
                            Core.LiteralString v2 -> Right v2
                            _ -> Left (Errors.DecodingError "expected string literal")
                          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (Core.Name "time", (\input -> Eithers.map (\t -> Model.ValueTime t) (offsetTime cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.ValueType
valueType :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.ValueType
valueType cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "boolean", (\input -> Eithers.map (\t -> Model.ValueTypeBoolean) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "string", (\input -> Eithers.map (\t -> Model.ValueTypeString) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "integer", (\input -> Eithers.map (\t -> Model.ValueTypeInteger) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "float", (\input -> Eithers.map (\t -> Model.ValueTypeFloat) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "date", (\input -> Eithers.map (\t -> Model.ValueTypeDate) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "localTime", (\input -> Eithers.map (\t -> Model.ValueTypeLocalTime) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "zonedTime", (\input -> Eithers.map (\t -> Model.ValueTypeZonedTime) (ExtractCore.decodeUnit cx input))),
                      (
                        Core.Name "localDateTime",
                        (\input -> Eithers.map (\t -> Model.ValueTypeLocalDateTime) (ExtractCore.decodeUnit cx input))),
                      (
                        Core.Name "zonedDateTime",
                        (\input -> Eithers.map (\t -> Model.ValueTypeZonedDateTime) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "duration", (\input -> Eithers.map (\t -> Model.ValueTypeDuration) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "point", (\input -> Eithers.map (\t -> Model.ValueTypePoint) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "list", (\input -> Eithers.map (\t -> Model.ValueTypeList t) (valueType cx input))),
                      (Core.Name "vector", (\input -> Eithers.map (\t -> Model.ValueTypeVector t) (vectorType cx input))),
                      (Core.Name "union", (\input -> Eithers.map (\t -> Model.ValueTypeUnion t) (ExtractCore.decodeList valueType cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.VectorType
vectorType :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.VectorType
vectorType cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "element" valueType fieldMap cx) (\field_element -> Eithers.bind (ExtractCore.requireField "dimension" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralInteger v2 -> case v2 of
              Core.IntegerValueInt32 v3 -> Right v3
              _ -> Left (Errors.DecodingError "expected int32 value")
            _ -> Left (Errors.DecodingError "expected int32 literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_dimension -> Right (Model.VectorType {
          Model.vectorTypeElement = field_element,
          Model.vectorTypeDimension = field_dimension}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.neo4j.model.VectorType")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.ZoneId
zoneId :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.ZoneId
zoneId cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Model.ZoneId b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.ZoneOffset
zoneOffset :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.ZoneOffset
zoneOffset cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Model.ZoneOffset b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralInteger v2 -> case v2 of
            Core.IntegerValueInt32 v3 -> Right v3
            _ -> Left (Errors.DecodingError "expected int32 value")
          _ -> Left (Errors.DecodingError "expected int32 literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.neo4j.model.ZonedDateTime
zonedDateTime :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.ZonedDateTime
zonedDateTime cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "dateTime" localDateTime fieldMap cx) (\field_dateTime -> Eithers.bind (ExtractCore.requireField "offset" zoneOffset fieldMap cx) (\field_offset -> Eithers.bind (ExtractCore.requireField "zone" zoneId fieldMap cx) (\field_zone -> Right (Model.ZonedDateTime {
          Model.zonedDateTimeDateTime = field_dateTime,
          Model.zonedDateTimeOffset = field_offset,
          Model.zonedDateTimeZone = field_zone})))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.neo4j.model.ZonedDateTime")) (ExtractCore.stripWithDecodingError cx raw)
