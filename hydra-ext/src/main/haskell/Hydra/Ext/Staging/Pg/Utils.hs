module Hydra.Ext.Staging.Pg.Utils where

import Hydra.Kernel
import qualified Hydra.Pg.Model as PG
import qualified Hydra.Pg.Mapping as PGM
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import qualified Hydra.Extract.Core as ExtractCore
import Hydra.Ext.Staging.Pg.Coder

import qualified Data.List as L
import qualified Data.Map as M


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

typedTermToPropertyGraph :: (Show t, Show v) => PGM.Schema Graph t v -> Type -> t -> t -> Flow Graph (Term -> Graph -> Flow Graph [PG.Element v])
typedTermToPropertyGraph schema typ vidType eidType = do
    adapter <- elementCoder Nothing schema typ vidType eidType
    return $ \term graph -> flattenTree <$> coderEncode (adapterCoder adapter) term
  where
    flattenTree tree = (PG.elementTreeSelf tree):(L.concat $ (flattenTree <$> PG.elementTreeDependencies tree))
