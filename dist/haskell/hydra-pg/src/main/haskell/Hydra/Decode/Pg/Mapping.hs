-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.pg.mapping

module Hydra.Decode.Pg.Mapping where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Pg.Model as Model
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Pg.Mapping as Mapping
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

annotationSchema :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Mapping.AnnotationSchema
annotationSchema cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "vertexLabel" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_vertexLabel -> Eithers.bind (ExtractCore.requireField "edgeLabel" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_edgeLabel -> Eithers.bind (ExtractCore.requireField "vertexId" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_vertexId -> Eithers.bind (ExtractCore.requireField "edgeId" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_edgeId -> Eithers.bind (ExtractCore.requireField "propertyKey" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_propertyKey -> Eithers.bind (ExtractCore.requireField "propertyValue" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_propertyValue -> Eithers.bind (ExtractCore.requireField "outVertex" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_outVertex -> Eithers.bind (ExtractCore.requireField "outVertexLabel" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_outVertexLabel -> Eithers.bind (ExtractCore.requireField "inVertex" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_inVertex -> Eithers.bind (ExtractCore.requireField "inVertexLabel" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_inVertexLabel -> Eithers.bind (ExtractCore.requireField "outEdge" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_outEdge -> Eithers.bind (ExtractCore.requireField "outEdgeLabel" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_outEdgeLabel -> Eithers.bind (ExtractCore.requireField "inEdge" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_inEdge -> Eithers.bind (ExtractCore.requireField "inEdgeLabel" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_inEdgeLabel -> Eithers.bind (ExtractCore.requireField "ignore" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_ignore -> Right (Mapping.AnnotationSchema {
          Mapping.annotationSchemaVertexLabel = field_vertexLabel,
          Mapping.annotationSchemaEdgeLabel = field_edgeLabel,
          Mapping.annotationSchemaVertexId = field_vertexId,
          Mapping.annotationSchemaEdgeId = field_edgeId,
          Mapping.annotationSchemaPropertyKey = field_propertyKey,
          Mapping.annotationSchemaPropertyValue = field_propertyValue,
          Mapping.annotationSchemaOutVertex = field_outVertex,
          Mapping.annotationSchemaOutVertexLabel = field_outVertexLabel,
          Mapping.annotationSchemaInVertex = field_inVertex,
          Mapping.annotationSchemaInVertexLabel = field_inVertexLabel,
          Mapping.annotationSchemaOutEdge = field_outEdge,
          Mapping.annotationSchemaOutEdgeLabel = field_outEdgeLabel,
          Mapping.annotationSchemaInEdge = field_inEdge,
          Mapping.annotationSchemaInEdgeLabel = field_inEdgeLabel,
          Mapping.annotationSchemaIgnore = field_ignore})))))))))))))))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

edgeSpec :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Mapping.EdgeSpec
edgeSpec cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "label" Model.edgeLabel fieldMap cx) (\field_label -> Eithers.bind (ExtractCore.requireField "id" valueSpec fieldMap cx) (\field_id -> Eithers.bind (ExtractCore.requireField "out" valueSpec fieldMap cx) (\field_out -> Eithers.bind (ExtractCore.requireField "in" valueSpec fieldMap cx) (\field_in -> Eithers.bind (ExtractCore.requireField "properties" (ExtractCore.decodeList propertySpec) fieldMap cx) (\field_properties -> Right (Mapping.EdgeSpec {
          Mapping.edgeSpecLabel = field_label,
          Mapping.edgeSpecId = field_id,
          Mapping.edgeSpecOut = field_out,
          Mapping.edgeSpecIn = field_in,
          Mapping.edgeSpecProperties = field_properties})))))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

elementSpec :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Mapping.ElementSpec
elementSpec cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "vertex", (\input -> Eithers.map (\t -> Mapping.ElementSpecVertex t) (vertexSpec cx input))),
                      (Core.Name "edge", (\input -> Eithers.map (\t -> Mapping.ElementSpecEdge t) (edgeSpec cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)

propertySpec :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Mapping.PropertySpec
propertySpec cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "key" Model.propertyKey fieldMap cx) (\field_key -> Eithers.bind (ExtractCore.requireField "value" valueSpec fieldMap cx) (\field_value -> Right (Mapping.PropertySpec {
          Mapping.propertySpecKey = field_key,
          Mapping.propertySpecValue = field_value}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

valueSpec :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Mapping.ValueSpec
valueSpec cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "value", (\input -> Eithers.map (\t -> Mapping.ValueSpecValue) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "pattern", (\input -> Eithers.map (\t -> Mapping.ValueSpecPattern t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralString v2 -> Right v2
                          _ -> Left (Errors.DecodingError "expected string literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input))))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)

vertexSpec :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Mapping.VertexSpec
vertexSpec cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "label" Model.vertexLabel fieldMap cx) (\field_label -> Eithers.bind (ExtractCore.requireField "id" valueSpec fieldMap cx) (\field_id -> Eithers.bind (ExtractCore.requireField "properties" (ExtractCore.decodeList propertySpec) fieldMap cx) (\field_properties -> Right (Mapping.VertexSpec {
          Mapping.vertexSpecLabel = field_label,
          Mapping.vertexSpecId = field_id,
          Mapping.vertexSpecProperties = field_properties})))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
