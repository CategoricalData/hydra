-- Note: this is an automatically generated file. Do not edit.

-- | Utility functions for property graph operations

module Hydra.Pg.Utils where

import qualified Hydra.Coders as Coders
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Pg.Coder as Coder
import qualified Hydra.Pg.Mapping as Mapping
import qualified Hydra.Pg.Model as Model_
import qualified Hydra.Show.Core as Core__
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

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
expString :: Context.Context -> Core.Term -> Either (Context.InContext Errors.Error) String
expString cx term =
    Core_.string cx (Graph.Graph {
      Graph.graphBoundTerms = Maps.empty,
      Graph.graphBoundTypes = Maps.empty,
      Graph.graphClassConstraints = Maps.empty,
      Graph.graphLambdaVariables = Sets.empty,
      Graph.graphMetadata = Maps.empty,
      Graph.graphPrimitives = Maps.empty,
      Graph.graphSchemaTypes = Maps.empty,
      Graph.graphTypeVariables = Sets.empty}) term

-- | Get all elements from a lazy graph
lazyGraphToElements :: Model_.LazyGraph t0 -> [Model_.Element t0]
lazyGraphToElements lg =
    Lists.concat2 (Lists.map (\x -> Model_.ElementVertex x) (Model_.lazyGraphVertices lg)) (Lists.map (\x -> Model_.ElementEdge x) (Model_.lazyGraphEdges lg))

-- | Convert a property graph element to JSON
pgElementToJson :: Mapping.Schema t0 t1 t2 -> Model_.Element t2 -> Context.Context -> Either (Context.InContext Errors.Error) Model.Value
pgElementToJson schema el cx =
    (\x -> case x of
      Model_.ElementVertex v0 -> Eithers.bind (Coders.coderDecode (Mapping.schemaVertexIds schema) cx (Model_.vertexId v0)) (\term ->
        let labelJson = Model.ValueString (Model_.unVertexLabel (Model_.vertexLabel v0))
        in (Eithers.map (\propsJson -> Model.ValueObject (Maps.fromList (Maybes.cat [
          Just ("label", labelJson),
          (Just ("id", (Model.ValueString (Core__.term term)))),
          propsJson]))) ((\pairs -> Logic.ifElse (Maps.null pairs) (Right Nothing) (Eithers.map (\p -> Just ("properties", (Model.ValueObject (Maps.fromList p)))) (Eithers.mapList (\pair ->
          let key = Pairs.first pair
              v = Pairs.second pair
          in (Eithers.bind (Coders.coderDecode (Mapping.schemaPropertyValues schema) cx v) (\term2 -> Right (Model_.unPropertyKey key, (Model.ValueString (Core__.term term2)))))) (Maps.toList pairs)))) (Model_.vertexProperties v0))))
      Model_.ElementEdge v0 -> Eithers.bind (Coders.coderDecode (Mapping.schemaEdgeIds schema) cx (Model_.edgeId v0)) (\term -> Eithers.bind (Coders.coderDecode (Mapping.schemaVertexIds schema) cx (Model_.edgeOut v0)) (\termOut -> Eithers.bind (Coders.coderDecode (Mapping.schemaVertexIds schema) cx (Model_.edgeIn v0)) (\termIn ->
        let labelJson = Model.ValueString (Model_.unEdgeLabel (Model_.edgeLabel v0))
        in (Eithers.map (\propsJson -> Model.ValueObject (Maps.fromList (Maybes.cat [
          Just ("label", labelJson),
          (Just ("id", (Model.ValueString (Core__.term term)))),
          (Just ("out", (Model.ValueString (Core__.term termOut)))),
          (Just ("in", (Model.ValueString (Core__.term termIn)))),
          propsJson]))) ((\pairs -> Logic.ifElse (Maps.null pairs) (Right Nothing) (Eithers.map (\p -> Just ("properties", (Model.ValueObject (Maps.fromList p)))) (Eithers.mapList (\pair ->
          let key = Pairs.first pair
              v = Pairs.second pair
          in (Eithers.bind (Coders.coderDecode (Mapping.schemaPropertyValues schema) cx v) (\term2 -> Right (Model_.unPropertyKey key, (Model.ValueString (Core__.term term2)))))) (Maps.toList pairs)))) (Model_.edgeProperties v0))))))) el

-- | Convert a list of property graph elements to JSON
pgElementsToJson :: Mapping.Schema t0 t1 t2 -> [Model_.Element t2] -> Context.Context -> Either (Context.InContext Errors.Error) Model.Value
pgElementsToJson schema els cx =
    Eithers.map (\els_ -> Model.ValueArray els_) (Eithers.mapList (\el -> pgElementToJson schema el cx) els)

-- | Get all elements from a property graph
propertyGraphElements :: Ord t0 => (Model_.Graph t0 -> [Model_.Element t0])
propertyGraphElements g =
    Lists.concat2 (Lists.map (\x -> Model_.ElementVertex x) (Maps.elems (Model_.graphVertices g))) (Lists.map (\x -> Model_.ElementEdge x) (Maps.elems (Model_.graphEdges g)))

-- | Convert a type-annotated term to property graph elements
typeApplicationTermToPropertyGraph :: Mapping.Schema t0 t1 t2 -> Core.Type -> t1 -> t1 -> Context.Context -> Graph.Graph -> Either (Context.InContext Errors.Error) (Core.Term -> Context.Context -> Either (Context.InContext Errors.Error) [Model_.Element t2])
typeApplicationTermToPropertyGraph schema typ vidType eidType cx g =
    Eithers.bind (Coder.elementCoder Nothing schema typ vidType eidType cx g) (\adapter -> Right (\term -> \cx_ -> Eithers.map (\tree ->
      let flattenTree =
              \t -> Lists.cons (Model_.elementTreeSelf t) (Lists.concat (Lists.map flattenTree (Model_.elementTreeDependencies t)))
      in (flattenTree tree)) (Coders.coderEncode (Coders.adapterCoder adapter) cx_ term)))
