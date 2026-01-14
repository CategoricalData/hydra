module Hydra.Ext.Staging.Pg.Utils where

import Hydra.Kernel
import Hydra.Ext.Dsl.Pg.Mappings
import Hydra.Ext.Org.Json.Coder
import Hydra.Ext.Staging.Pg.Coder
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Json.Model as Json
import qualified Hydra.Json.Writer as JsonWriter
import qualified Hydra.Pg.Graphson.Syntax as G
import qualified Hydra.Pg.Graphson.Utils as GraphsonUtils
import qualified Hydra.Pg.Model as PG
import qualified Hydra.Pg.Mapping as PGM

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y


defaultTinkerpopAnnotations :: PGM.AnnotationSchema
defaultTinkerpopAnnotations = PGM.AnnotationSchema {
  PGM.annotationSchemaVertexLabel = "vertexLabel",
  PGM.annotationSchemaEdgeLabel = "edgeLabel",
  PGM.annotationSchemaVertexId = "vertexId",
  PGM.annotationSchemaEdgeId = "edgeId",
  PGM.annotationSchemaPropertyKey = "key",
  PGM.annotationSchemaPropertyValue = "value",
  PGM.annotationSchemaOutVertex = "outVertex",
  PGM.annotationSchemaOutVertexLabel = "outVertexLabel",
  PGM.annotationSchemaInVertex = "inVertex",
  PGM.annotationSchemaInVertexLabel = "inVertexLabel",
  PGM.annotationSchemaOutEdge = "outEdge",
  PGM.annotationSchemaOutEdgeLabel = "outEdgeLabel",
  PGM.annotationSchemaInEdge = "inEdge",
  PGM.annotationSchemaInEdgeLabel = "inEdgeLabel",
  PGM.annotationSchemaIgnore = "ignore"}

examplePgSchema :: PGM.Schema s () String
examplePgSchema = PGM.Schema {
    PGM.schemaVertexIdTypes = mkCoder "encode vertex id type" encodeType decodeType,
    PGM.schemaVertexIds = mkCoder "encode vertex id" expString (pure . Terms.string),
    PGM.schemaEdgeIdTypes = mkCoder "encode edge id type" encodeType decodeType,
    PGM.schemaEdgeIds = mkCoder "encode edge id" expString (pure . Terms.string),
    PGM.schemaPropertyTypes = mkCoder "encode property type" encodeType decodeType,
    PGM.schemaPropertyValues = mkCoder "encode property value" expString (pure . Terms.string),
    PGM.schemaAnnotations = defaultTinkerpopAnnotations,
    PGM.schemaDefaultVertexId = "defaultVertexId",
    PGM.schemaDefaultEdgeId = "defaultEdgeId"}
  where
    mkCoder lab encode decode = Coder (withTrace lab . encode) decode
    encodeType _ = pure ()
    decodeType _ = pure Types.unit

expString :: Term -> Flow s String
expString term = withEmptyGraph $ ExtractCore.string term

propertyGraphElements :: PG.Graph v -> [PG.Element v]
propertyGraphElements g = fmap PG.ElementVertex (M.elems $ PG.graphVertices g)
  ++ fmap PG.ElementEdge (M.elems $ PG.graphEdges g)

typeApplicationTermToPropertyGraph :: (Show t, Show v) => PGM.Schema Graph t v -> Type -> t -> t -> Flow Graph (Term -> Graph -> Flow Graph [PG.Element v])
typeApplicationTermToPropertyGraph schema typ vidType eidType = do
    adapter <- elementCoder Nothing schema typ vidType eidType
    return $ \term graph -> flattenTree <$> coderEncode (adapterCoder adapter) term
  where
    flattenTree tree = (PG.elementTreeSelf tree):(L.concat $ (flattenTree <$> PG.elementTreeDependencies tree))

lazyGraphToElements :: LazyGraph v -> [PG.Element v]
lazyGraphToElements (LazyGraph vertices edges) = fmap PG.ElementVertex vertices ++ fmap PG.ElementEdge edges

pgElementToJson :: PGM.Schema s t v -> PG.Element v -> Flow s Json.Value
pgElementToJson schema el = case el of
    PG.ElementVertex vertex -> do
      let labelJson = Json.ValueString $ PG.unVertexLabel $ PG.vertexLabel vertex
      idJson <- coderDecode (PGM.schemaVertexIds schema) (PG.vertexId vertex) >>= untypedTermToJson
      propsJson <- propsToJson $ PG.vertexProperties vertex
      return $ Json.ValueObject $ M.fromList $ Y.catMaybes [
        Just ("label", labelJson),
        Just ("id", idJson),
        propsJson]
    PG.ElementEdge edge -> do
      let labelJson = Json.ValueString $ PG.unEdgeLabel $ PG.edgeLabel edge
      idJson <- coderDecode (PGM.schemaEdgeIds schema) (PG.edgeId edge) >>= untypedTermToJson
      outJson <- coderDecode (PGM.schemaVertexIds schema) (PG.edgeOut edge) >>= untypedTermToJson
      inJson <- coderDecode (PGM.schemaVertexIds schema) (PG.edgeIn edge) >>= untypedTermToJson
      propsJson <- propsToJson $ PG.edgeProperties edge
      return $ Json.ValueObject $ M.fromList $ Y.catMaybes [
        Just ("label", labelJson),
        Just ("id", idJson),
        Just ("out", outJson),
        Just ("in", inJson),
        propsJson]
  where
    propsToJson pairs = if L.null pairs
        then pure Nothing
        else do
          p <- CM.mapM propToJson $ M.toList pairs
          return $ Just $ ("properties", Json.ValueObject $ M.fromList p)
      where
        propToJson (PG.PropertyKey key, v) = do
          json <- coderDecode (PGM.schemaPropertyValues schema) v >>= untypedTermToJson
          return (key, json)

pgElementsToJson :: PGM.Schema s t v -> [PG.Element v] -> Flow s Json.Value
pgElementsToJson schema els = Json.ValueArray <$> CM.mapM (pgElementToJson schema) els

propertyGraphGraphsonLastMile :: (Ord v, Show t, Show v) => (v -> Flow Graph G.Value) -> PGM.Schema Graph t v -> t -> t -> LastMile Graph (PG.Element v)
propertyGraphGraphsonLastMile encodeValue schema vidType eidType =
  LastMile (\typ -> typeApplicationTermToPropertyGraph schema typ vidType eidType) (\els -> jsonValuesToString <$> GraphsonUtils.pgElementsToGraphson encodeValue els) "jsonl"
  where
    jsonValuesToString = L.intercalate "\n" . fmap JsonWriter.printJson

propertyGraphJsonLastMile :: (Show t, Show v) => PGM.Schema Graph t v -> t -> t -> LastMile Graph (PG.Element v)
propertyGraphJsonLastMile schema vidType eidType =
  LastMile (\typ -> typeApplicationTermToPropertyGraph schema typ vidType eidType) (\els -> JsonWriter.printJson <$> pgElementsToJson schema els) "json"
