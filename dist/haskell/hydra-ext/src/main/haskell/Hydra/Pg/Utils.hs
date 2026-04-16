-- Note: this is an automatically generated file. Do not edit.

-- | Utility functions for property graph operations

module Hydra.Pg.Utils where

import qualified Hydra.Coders as Coders
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as JsonModel
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Pg.Coder as Coder
import qualified Hydra.Pg.Mapping as Mapping
import qualified Hydra.Pg.Model as PgModel
import qualified Hydra.Show.Core as ShowCore
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | Default Tinkerpop annotation schema
defaultTinkerpopAnnotations :: Mapping.AnnotationSchema
defaultTinkerpopAnnotations =
    Mapping.AnnotationSchema {
      Mapping.annotationSchemaVertexLabel = "vertexLabel",
      Mapping.annotationSchemaEdgeLabel = "edgeLabel",
      Mapping.annotationSchemaVertexId = "vertexId",
      Mapping.annotationSchemaEdgeId = "edgeId",
      Mapping.annotationSchemaPropertyKey = "key",
      Mapping.annotationSchemaPropertyValue = "value",
      Mapping.annotationSchemaOutVertex = "outVertex",
      Mapping.annotationSchemaOutVertexLabel = "outVertexLabel",
      Mapping.annotationSchemaInVertex = "inVertex",
      Mapping.annotationSchemaInVertexLabel = "inVertexLabel",
      Mapping.annotationSchemaOutEdge = "outEdge",
      Mapping.annotationSchemaOutEdgeLabel = "outEdgeLabel",
      Mapping.annotationSchemaInEdge = "inEdge",
      Mapping.annotationSchemaInEdgeLabel = "inEdgeLabel",
      Mapping.annotationSchemaIgnore = "ignore"}

-- | Example property graph schema with string values
examplePgSchema :: Mapping.Schema t0 () String
examplePgSchema =
    Mapping.Schema {
      Mapping.schemaVertexIdTypes = Coders.Coder {
        Coders.coderEncode = (\_ -> \_2 -> Right ()),
        Coders.coderDecode = (\_ -> \_2 -> Right Core.TypeUnit)},
      Mapping.schemaVertexIds = Coders.Coder {
        Coders.coderEncode = (\cx -> \t -> expString cx t),
        Coders.coderDecode = (\_cx -> \s -> Right (Core.TermLiteral (Core.LiteralString s)))},
      Mapping.schemaEdgeIdTypes = Coders.Coder {
        Coders.coderEncode = (\_ -> \_2 -> Right ()),
        Coders.coderDecode = (\_ -> \_2 -> Right Core.TypeUnit)},
      Mapping.schemaEdgeIds = Coders.Coder {
        Coders.coderEncode = (\cx -> \t -> expString cx t),
        Coders.coderDecode = (\_cx -> \s -> Right (Core.TermLiteral (Core.LiteralString s)))},
      Mapping.schemaPropertyTypes = Coders.Coder {
        Coders.coderEncode = (\_ -> \_2 -> Right ()),
        Coders.coderDecode = (\_ -> \_2 -> Right Core.TypeUnit)},
      Mapping.schemaPropertyValues = Coders.Coder {
        Coders.coderEncode = (\cx -> \t -> expString cx t),
        Coders.coderDecode = (\_cx -> \s -> Right (Core.TermLiteral (Core.LiteralString s)))},
      Mapping.schemaAnnotations = defaultTinkerpopAnnotations,
      Mapping.schemaDefaultVertexId = "defaultVertexId",
      Mapping.schemaDefaultEdgeId = "defaultEdgeId"}

-- | Extract a string from a term using the empty graph
expString :: t0 -> Core.Term -> Either Errors.Error String
expString cx term =
    ExtractCore.string (Graph.Graph {
      Graph.graphBoundTerms = Maps.empty,
      Graph.graphBoundTypes = Maps.empty,
      Graph.graphClassConstraints = Maps.empty,
      Graph.graphLambdaVariables = Sets.empty,
      Graph.graphMetadata = Maps.empty,
      Graph.graphPrimitives = Maps.empty,
      Graph.graphSchemaTypes = Maps.empty,
      Graph.graphTypeVariables = Sets.empty}) term

-- | Get all elements from a lazy graph
lazyGraphToElements :: PgModel.LazyGraph t0 -> [PgModel.Element t0]
lazyGraphToElements lg =
    Lists.concat2 (Lists.map (\x -> PgModel.ElementVertex x) (PgModel.lazyGraphVertices lg)) (Lists.map (\x -> PgModel.ElementEdge x) (PgModel.lazyGraphEdges lg))

-- | Convert a property graph element to JSON
pgElementToJson :: Mapping.Schema t0 t1 t2 -> PgModel.Element t2 -> Context.Context -> Either Errors.Error JsonModel.Value
pgElementToJson schema el cx =
    (\x -> case x of
      PgModel.ElementVertex v0 -> Eithers.bind (Coders.coderDecode (Mapping.schemaVertexIds schema) cx (PgModel.vertexId v0)) (\term ->
        let labelJson = JsonModel.ValueString (PgModel.unVertexLabel (PgModel.vertexLabel v0))
        in (Eithers.map (\propsJson -> JsonModel.ValueObject (Maps.fromList (Maybes.cat [
          Just ("label", labelJson),
          (Just ("id", (JsonModel.ValueString (ShowCore.term term)))),
          propsJson]))) ((\pairs -> Logic.ifElse (Maps.null pairs) (Right Nothing) (Eithers.map (\p -> Just ("properties", (JsonModel.ValueObject (Maps.fromList p)))) (Eithers.mapList (\pair ->
          let key = Pairs.first pair
              v = Pairs.second pair
          in (Eithers.bind (Coders.coderDecode (Mapping.schemaPropertyValues schema) cx v) (\term2 -> Right (PgModel.unPropertyKey key, (JsonModel.ValueString (ShowCore.term term2)))))) (Maps.toList pairs)))) (PgModel.vertexProperties v0))))
      PgModel.ElementEdge v0 -> Eithers.bind (Coders.coderDecode (Mapping.schemaEdgeIds schema) cx (PgModel.edgeId v0)) (\term -> Eithers.bind (Coders.coderDecode (Mapping.schemaVertexIds schema) cx (PgModel.edgeOut v0)) (\termOut -> Eithers.bind (Coders.coderDecode (Mapping.schemaVertexIds schema) cx (PgModel.edgeIn v0)) (\termIn ->
        let labelJson = JsonModel.ValueString (PgModel.unEdgeLabel (PgModel.edgeLabel v0))
        in (Eithers.map (\propsJson -> JsonModel.ValueObject (Maps.fromList (Maybes.cat [
          Just ("label", labelJson),
          (Just ("id", (JsonModel.ValueString (ShowCore.term term)))),
          (Just ("out", (JsonModel.ValueString (ShowCore.term termOut)))),
          (Just ("in", (JsonModel.ValueString (ShowCore.term termIn)))),
          propsJson]))) ((\pairs -> Logic.ifElse (Maps.null pairs) (Right Nothing) (Eithers.map (\p -> Just ("properties", (JsonModel.ValueObject (Maps.fromList p)))) (Eithers.mapList (\pair ->
          let key = Pairs.first pair
              v = Pairs.second pair
          in (Eithers.bind (Coders.coderDecode (Mapping.schemaPropertyValues schema) cx v) (\term2 -> Right (PgModel.unPropertyKey key, (JsonModel.ValueString (ShowCore.term term2)))))) (Maps.toList pairs)))) (PgModel.edgeProperties v0))))))) el

-- | Convert a list of property graph elements to JSON
pgElementsToJson :: Mapping.Schema t0 t1 t2 -> [PgModel.Element t2] -> Context.Context -> Either Errors.Error JsonModel.Value
pgElementsToJson schema els cx =
    Eithers.map (\els_ -> JsonModel.ValueArray els_) (Eithers.mapList (\el -> pgElementToJson schema el cx) els)

-- | Get all elements from a property graph
propertyGraphElements :: Ord t0 => (PgModel.Graph t0 -> [PgModel.Element t0])
propertyGraphElements g =
    Lists.concat2 (Lists.map (\x -> PgModel.ElementVertex x) (Maps.elems (PgModel.graphVertices g))) (Lists.map (\x -> PgModel.ElementEdge x) (Maps.elems (PgModel.graphEdges g)))

-- | Convert a type-annotated term to property graph elements
typeApplicationTermToPropertyGraph :: Mapping.Schema t0 t1 t2 -> Core.Type -> t1 -> t1 -> Context.Context -> Graph.Graph -> Either Errors.Error (Core.Term -> Context.Context -> Either Errors.Error [PgModel.Element t2])
typeApplicationTermToPropertyGraph schema typ vidType eidType cx g =
    Eithers.bind (Coder.elementCoder Nothing schema typ vidType eidType cx g) (\adapter -> Right (\term -> \cx_ -> Eithers.map (\tree ->
      let flattenTree =
              \t -> Lists.cons (PgModel.elementTreeSelf t) (Lists.concat (Lists.map flattenTree (PgModel.elementTreeDependencies t)))
      in (flattenTree tree)) (Coders.coderEncode (Coders.adapterCoder adapter) cx_ term)))
