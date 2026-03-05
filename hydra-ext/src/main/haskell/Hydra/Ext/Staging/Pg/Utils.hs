module Hydra.Ext.Staging.Pg.Utils where

import Hydra.Kernel
import Hydra.Ext.Dsl.Pg.Mappings
import Hydra.Ext.Org.Json.Coder
import Hydra.Ext.Staging.Pg.Coder
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Json.Model as Json
import qualified Hydra.Pg.Model as PG
import qualified Hydra.Pg.Mapping as PGM

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y


type Result a = Either (InContext OtherError) a

err :: Context -> String -> Result a
err cx msg = Left (InContext (OtherError msg) cx)

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

examplePgSchema :: PGM.Schema Graph () String
examplePgSchema = PGM.Schema {
    PGM.schemaVertexIdTypes = mkSimpleCoder "encode vertex id type" encodeType decodeType,
    PGM.schemaVertexIds = mkSimpleCoder "encode vertex id" expString (\_cx -> Right . Terms.string),
    PGM.schemaEdgeIdTypes = mkSimpleCoder "encode edge id type" encodeType decodeType,
    PGM.schemaEdgeIds = mkSimpleCoder "encode edge id" expString (\_cx -> Right . Terms.string),
    PGM.schemaPropertyTypes = mkSimpleCoder "encode property type" encodeType decodeType,
    PGM.schemaPropertyValues = mkSimpleCoder "encode property value" expString (\_cx -> Right . Terms.string),
    PGM.schemaAnnotations = defaultTinkerpopAnnotations,
    PGM.schemaDefaultVertexId = "defaultVertexId",
    PGM.schemaDefaultEdgeId = "defaultEdgeId"}
  where
    mkSimpleCoder _lab encode decode = Coder encode decode
    encodeType _cx _ = Right ()
    decodeType _cx _ = Right Types.unit

expString :: Context -> Term -> Result String
expString cx term = ExtractCore.string cx emptyGraph term

propertyGraphElements :: PG.Graph v -> [PG.Element v]
propertyGraphElements g = fmap PG.ElementVertex (M.elems $ PG.graphVertices g)
  ++ fmap PG.ElementEdge (M.elems $ PG.graphEdges g)

typeApplicationTermToPropertyGraph :: (Show t, Show v) => PGM.Schema Graph t v -> Type -> t -> t -> Context -> Graph -> Result (Term -> Context -> Result [PG.Element v])
typeApplicationTermToPropertyGraph schema typ vidType eidType cx g = do
    adapter <- elementCoder Nothing schema typ vidType eidType cx g
    return $ \term cx' -> flattenTree <$> coderEncode (adapterCoder adapter) cx' term
  where
    flattenTree tree = (PG.elementTreeSelf tree):(L.concat $ (flattenTree <$> PG.elementTreeDependencies tree))

lazyGraphToElements :: PG.LazyGraph v -> [PG.Element v]
lazyGraphToElements (PG.LazyGraph vertices edges) = fmap PG.ElementVertex vertices ++ fmap PG.ElementEdge edges

pgElementToJson :: PGM.Schema Graph t v -> PG.Element v -> Context -> Result Json.Value
pgElementToJson schema el cx = case el of
    PG.ElementVertex vertex -> do
      term <- coderDecode (PGM.schemaVertexIds schema) cx (PG.vertexId vertex)
      idJson <- liftEither $ untypedTermToJson term
      propsJson <- propsToJson $ PG.vertexProperties vertex
      let labelJson = Json.ValueString $ PG.unVertexLabel $ PG.vertexLabel vertex
      return $ Json.ValueObject $ M.fromList $ Y.catMaybes [
        Just ("label", labelJson),
        Just ("id", idJson),
        propsJson]
    PG.ElementEdge edge -> do
      term <- coderDecode (PGM.schemaEdgeIds schema) cx (PG.edgeId edge)
      idJson <- liftEither $ untypedTermToJson term
      termOut <- coderDecode (PGM.schemaVertexIds schema) cx (PG.edgeOut edge)
      outJson <- liftEither $ untypedTermToJson termOut
      termIn <- coderDecode (PGM.schemaVertexIds schema) cx (PG.edgeIn edge)
      inJson <- liftEither $ untypedTermToJson termIn
      propsJson <- propsToJson $ PG.edgeProperties edge
      let labelJson = Json.ValueString $ PG.unEdgeLabel $ PG.edgeLabel edge
      return $ Json.ValueObject $ M.fromList $ Y.catMaybes [
        Just ("label", labelJson),
        Just ("id", idJson),
        Just ("out", outJson),
        Just ("in", inJson),
        propsJson]
  where
    liftEither (Left _) = Left $ InContext (OtherError "untypedTermToJson failed") cx
    liftEither (Right v) = Right v
    propsToJson pairs = if L.null pairs
        then pure Nothing
        else do
          p <- CM.mapM propToJson $ M.toList pairs
          return $ Just $ ("properties", Json.ValueObject $ M.fromList p)
      where
        propToJson (PG.PropertyKey key, v) = do
          term <- coderDecode (PGM.schemaPropertyValues schema) cx v
          json <- liftEither $ untypedTermToJson term
          return (key, json)

pgElementsToJson :: PGM.Schema Graph t v -> [PG.Element v] -> Context -> Result Json.Value
pgElementsToJson schema els cx = Json.ValueArray <$> CM.mapM (\el -> pgElementToJson schema el cx) els
