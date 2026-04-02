-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.pg.mapping

module Hydra.Decode.Pg.Mapping where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Pg.Model as Model
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Pg.Mapping as Mapping
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

annotationSchema :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Mapping.AnnotationSchema
annotationSchema cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core_.toFieldMap v0
        in (Eithers.bind (Core_.requireField "vertexLabel" (\cx -> \raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_vertexLabel -> Eithers.bind (Core_.requireField "edgeLabel" (\cx -> \raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_edgeLabel -> Eithers.bind (Core_.requireField "vertexId" (\cx -> \raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_vertexId -> Eithers.bind (Core_.requireField "edgeId" (\cx -> \raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_edgeId -> Eithers.bind (Core_.requireField "propertyKey" (\cx -> \raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_propertyKey -> Eithers.bind (Core_.requireField "propertyValue" (\cx -> \raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_propertyValue -> Eithers.bind (Core_.requireField "outVertex" (\cx -> \raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_outVertex -> Eithers.bind (Core_.requireField "outVertexLabel" (\cx -> \raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_outVertexLabel -> Eithers.bind (Core_.requireField "inVertex" (\cx -> \raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_inVertex -> Eithers.bind (Core_.requireField "inVertexLabel" (\cx -> \raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_inVertexLabel -> Eithers.bind (Core_.requireField "outEdge" (\cx -> \raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_outEdge -> Eithers.bind (Core_.requireField "outEdgeLabel" (\cx -> \raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_outEdgeLabel -> Eithers.bind (Core_.requireField "inEdge" (\cx -> \raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_inEdge -> Eithers.bind (Core_.requireField "inEdgeLabel" (\cx -> \raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_inEdgeLabel -> Eithers.bind (Core_.requireField "ignore" (\cx -> \raw -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_ignore -> Right (Mapping.AnnotationSchema {
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
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

edgeSpec :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Mapping.EdgeSpec
edgeSpec cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core_.toFieldMap v0
        in (Eithers.bind (Core_.requireField "label" Model.edgeLabel fieldMap cx) (\field_label -> Eithers.bind (Core_.requireField "id" valueSpec fieldMap cx) (\field_id -> Eithers.bind (Core_.requireField "out" valueSpec fieldMap cx) (\field_out -> Eithers.bind (Core_.requireField "in" valueSpec fieldMap cx) (\field_in -> Eithers.bind (Core_.requireField "properties" (Core_.decodeList propertySpec) fieldMap cx) (\field_properties -> Right (Mapping.EdgeSpec {
          Mapping.edgeSpecLabel = field_label,
          Mapping.edgeSpecId = field_id,
          Mapping.edgeSpecOut = field_out,
          Mapping.edgeSpecIn = field_in,
          Mapping.edgeSpecProperties = field_properties})))))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

elementSpec :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Mapping.ElementSpec
elementSpec cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermUnion v0 ->
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
      _ -> Left (Errors.DecodingError "expected union")) (Lexical.stripAndDereferenceTermEither cx raw)

propertySpec :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Mapping.PropertySpec
propertySpec cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core_.toFieldMap v0
        in (Eithers.bind (Core_.requireField "key" Model.propertyKey fieldMap cx) (\field_key -> Eithers.bind (Core_.requireField "value" valueSpec fieldMap cx) (\field_value -> Right (Mapping.PropertySpec {
          Mapping.propertySpecKey = field_key,
          Mapping.propertySpecValue = field_value}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

valueSpec :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Mapping.ValueSpec
valueSpec cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "value", (\input -> Eithers.map (\t -> Mapping.ValueSpecValue) (Core_.decodeUnit cx input))),
                      (Core.Name "pattern", (\input -> Eithers.map (\t -> Mapping.ValueSpecPattern t) (Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralString v2 -> Right v2
                          _ -> Left (Errors.DecodingError "expected string literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx input))))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (Lexical.stripAndDereferenceTermEither cx raw)

vertexSpec :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Mapping.VertexSpec
vertexSpec cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core_.toFieldMap v0
        in (Eithers.bind (Core_.requireField "label" Model.vertexLabel fieldMap cx) (\field_label -> Eithers.bind (Core_.requireField "id" valueSpec fieldMap cx) (\field_id -> Eithers.bind (Core_.requireField "properties" (Core_.decodeList propertySpec) fieldMap cx) (\field_properties -> Right (Mapping.VertexSpec {
          Mapping.vertexSpecLabel = field_label,
          Mapping.vertexSpecId = field_id,
          Mapping.vertexSpecProperties = field_properties})))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)
