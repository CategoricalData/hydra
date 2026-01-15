-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.pg.mapping

module Hydra.Decode.Pg.Mapping where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Pg.Model as Model
import qualified Hydra.Extract.Helpers as Helpers
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Pg.Mapping as Mapping
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

annotationSchema :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Mapping.AnnotationSchema)
annotationSchema cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "vertexLabel" (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_vertexLabel -> Eithers.bind (Helpers.requireField "edgeLabel" (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_edgeLabel -> Eithers.bind (Helpers.requireField "vertexId" (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_vertexId -> Eithers.bind (Helpers.requireField "edgeId" (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_edgeId -> Eithers.bind (Helpers.requireField "propertyKey" (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_propertyKey -> Eithers.bind (Helpers.requireField "propertyValue" (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_propertyValue -> Eithers.bind (Helpers.requireField "outVertex" (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_outVertex -> Eithers.bind (Helpers.requireField "outVertexLabel" (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_outVertexLabel -> Eithers.bind (Helpers.requireField "inVertex" (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_inVertex -> Eithers.bind (Helpers.requireField "inVertexLabel" (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_inVertexLabel -> Eithers.bind (Helpers.requireField "outEdge" (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_outEdge -> Eithers.bind (Helpers.requireField "outEdgeLabel" (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_outEdgeLabel -> Eithers.bind (Helpers.requireField "inEdge" (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_inEdge -> Eithers.bind (Helpers.requireField "inEdgeLabel" (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_inEdgeLabel -> Eithers.bind (Helpers.requireField "ignore" (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_ignore -> Right (Mapping.AnnotationSchema {
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
  _ -> (Left (Util.DecodingError "expected record of type hydra.pg.mapping.AnnotationSchema"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

edgeSpec :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Mapping.EdgeSpec)
edgeSpec cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "label" Model.edgeLabel fieldMap cx) (\field_label -> Eithers.bind (Helpers.requireField "id" valueSpec fieldMap cx) (\field_id -> Eithers.bind (Helpers.requireField "out" valueSpec fieldMap cx) (\field_out -> Eithers.bind (Helpers.requireField "in" valueSpec fieldMap cx) (\field_in -> Eithers.bind (Helpers.requireField "properties" (Helpers.decodeList propertySpec) fieldMap cx) (\field_properties -> Right (Mapping.EdgeSpec {
      Mapping.edgeSpecLabel = field_label,
      Mapping.edgeSpecId = field_id,
      Mapping.edgeSpecOut = field_out,
      Mapping.edgeSpecIn = field_in,
      Mapping.edgeSpecProperties = field_properties})))))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.pg.mapping.EdgeSpec"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

elementSpec :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Mapping.ElementSpec)
elementSpec cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "vertex", (\input -> Eithers.map (\t -> Mapping.ElementSpecVertex t) (vertexSpec cx input))),
                (Core.Name "edge", (\input -> Eithers.map (\t -> Mapping.ElementSpecEdge t) (edgeSpec cx input)))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.pg.mapping.ElementSpec"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

propertySpec :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Mapping.PropertySpec)
propertySpec cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "key" Model.propertyKey fieldMap cx) (\field_key -> Eithers.bind (Helpers.requireField "value" valueSpec fieldMap cx) (\field_value -> Right (Mapping.PropertySpec {
      Mapping.propertySpecKey = field_key,
      Mapping.propertySpecValue = field_value}))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.pg.mapping.PropertySpec"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

valueSpec :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Mapping.ValueSpec)
valueSpec cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "value", (\input -> Eithers.map (\t -> Mapping.ValueSpecValue) (Helpers.decodeUnit cx input))),
                (Core.Name "pattern", (\input -> Eithers.map (\t -> Mapping.ValueSpecPattern t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralString v3 -> (Right v3)
                    _ -> (Left (Util.DecodingError "expected string literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input))))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.pg.mapping.ValueSpec"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

vertexSpec :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Mapping.VertexSpec)
vertexSpec cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "label" Model.vertexLabel fieldMap cx) (\field_label -> Eithers.bind (Helpers.requireField "id" valueSpec fieldMap cx) (\field_id -> Eithers.bind (Helpers.requireField "properties" (Helpers.decodeList propertySpec) fieldMap cx) (\field_properties -> Right (Mapping.VertexSpec {
      Mapping.vertexSpecLabel = field_label,
      Mapping.vertexSpecId = field_id,
      Mapping.vertexSpecProperties = field_properties})))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.pg.mapping.VertexSpec"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
