-- | A utility for transforming Avro-compliant JSON into other models

module Hydra.Tools.AvroWorkflows (
  TransformWorkflow(..),
  JsonPayloadFormat(..),
  TermEncoder(..),
  LastMile(..),
  defaultTinkerpopAnnotations,
  exampleGraphsonContext,
  examplePgSchema,
  executeAvroTransformWorkflow,
  propertyGraphGraphsonLastMile,
  propertyGraphJsonLastMile,
  rdfDescriptionsToNtriples,
  shaclRdfLastMile,
  typedTermToShaclRdf,
  transformAvroJsonDirectory,
) where

import Hydra.Kernel
import Hydra.Tools.Monads
import Hydra.Dsl.Annotations
import qualified Hydra.Ext.Org.Apache.Avro.Schema as Avro
import qualified Hydra.Json as Json
import Hydra.Ext.Json.Coder
import Hydra.Ext.Json.Eliminate
import Hydra.Ext.Json.Serde
import Hydra.Ext.Avro.Coder
import Hydra.Ext.Avro.SchemaJson
import Hydra.Ext.Pg.Graphson.Utils
import Hydra.Ext.Pg.Coder
import qualified Hydra.Ext.Shacl.Coder as Shacl
import qualified Hydra.Ext.Org.W3.Rdf.Syntax as Rdf
import qualified Hydra.Ext.Rdf.Utils as RdfUt
import qualified Hydra.Pg.Model as PG
import qualified Hydra.Pg.Mapping as PGM
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import qualified Hydra.Expect as Expect
import Hydra.Ext.Rdf.Serde
import Hydra.Sources.Tier0.Core
import Hydra.Ext.Pg.Graphson.Coder
import Hydra.Pg.Graphson.Syntax as G

import qualified Control.Monad as CM
import qualified Data.Either as E
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import System.IO
import System.FilePath
import System.FilePath.Posix
import System.Directory


data JsonPayloadFormat = Json | Jsonl

type TermEncoder x = Term -> Graph -> Flow Graph [x]

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

-- | A convenience for transformAvroJsonDirectory, bundling all of the input parameters together as a workflow
executeAvroTransformWorkflow :: LastMile Graph x -> TransformWorkflow -> IO ()
executeAvroTransformWorkflow lastMile (TransformWorkflow name schemaSpec srcDir destDir) = do
    schemaPath <- case schemaSpec of
      SchemaSpecFile p -> pure p
      _ -> fail "unsupported schema spec"
    putStrLn $ "Executing workflow " ++ show name ++ ":"
    transformAvroJsonDirectory lastMile schemaPath srcDir destDir

expString :: Term -> Flow s String
expString term = withEmptyGraph $ Expect.string term

-- Replace all lists with sets, for better query performance.
-- This is a last-mile step which breaks type/term conformance
-- (a more robust solution would modify the target language in the SHACL coder, so that list types are also transformed to set types).
listsToSets :: Term -> Term
listsToSets = rewriteTerm mapExpr
  where
    mapExpr recurse = recurse . replaceLists
    replaceLists term = case term of
      TermList els -> TermSet $ S.fromList els
      _ -> term

propertyGraphGraphsonLastMile :: (Ord v, Show t, Show v) => GraphsonContext Graph v -> PGM.Schema Graph t v -> t -> t -> LastMile Graph (PG.Element v)
propertyGraphGraphsonLastMile ctx schema vidType eidType =
  LastMile (\typ -> typedTermToPropertyGraph schema typ vidType eidType) (\els -> jsonValuesToString <$> pgElementsToGraphson ctx els) "jsonl"

propertyGraphJsonLastMile :: (Show t, Show v) => PGM.Schema Graph t v -> t -> t -> LastMile Graph (PG.Element v)
propertyGraphJsonLastMile schema vidType eidType =
  LastMile (\typ -> typedTermToPropertyGraph schema typ vidType eidType) (\els -> jsonValueToString <$> pgElementsToJson schema els) "json"

rdfDescriptionsToNtriples :: [Rdf.Description] -> String
rdfDescriptionsToNtriples = rdfGraphToNtriples . RdfUt.descriptionsToGraph

shaclRdfLastMile :: LastMile Graph Rdf.Description
shaclRdfLastMile = LastMile typedTermToShaclRdf (pure . rdfDescriptionsToNtriples) "nt"

typedTermToPropertyGraph :: (Show t, Show v) => PGM.Schema Graph t v -> Type -> t -> t -> Flow Graph (Term -> Graph -> Flow Graph [PG.Element v])
typedTermToPropertyGraph schema typ vidType eidType = do
    adapter <- elementCoder Nothing schema typ vidType eidType
    return $ \term graph -> flattenTree <$> coderEncode (adapterCoder adapter) term
  where
    flattenTree tree = (PG.elementTreeSelf tree):(L.concat $ (flattenTree <$> PG.elementTreeDependencies tree))

typedTermToShaclRdf :: Type -> Flow Graph (Term -> Graph -> Flow Graph [Rdf.Description])
typedTermToShaclRdf _ = pure encode
  where
    encode term graph = do
        elDescs <- CM.mapM encodeElement $ M.elems $ graphElements graph
        termDescs <- encodeBlankTerm
        return $ L.concat (termDescs:elDescs)
      where
        encodeElement el = do
          let subject = Rdf.ResourceIri $ RdfUt.nameToIri $ elementName el
          Shacl.encodeTerm subject $ listsToSets $ elementTerm el
        encodeBlankTerm = if notInGraph
          then do
            subject <- RdfUt.nextBlankNode
            Shacl.encodeTerm subject $ listsToSets term
          else pure []
        notInGraph = L.null $ L.filter (\e -> elementTerm e == term) $ M.elems $ graphElements graph

transformAvroJson :: JsonPayloadFormat -> AvroHydraAdapter -> LastMile Graph x -> FilePath -> FilePath -> IO ()
transformAvroJson format adapter lastMile inFile outFile = do
    putStr $ "\t" ++ inFile ++ " --> "
    contents <- readFile inFile
    let entities = case format of
          Json -> [contents]
          Jsonl -> L.filter (not . L.null) $ lines contents
    lmEncoder <- fromFlowIo hydraCoreGraph $ lastMileEncoder lastMile (adapterTarget adapter)
    descs <- L.concat <$> fromFlowIo hydraCoreGraph (CM.zipWithM (jsonToTarget inFile adapter lmEncoder) [1..] entities)
    result <- fromFlowIo hydraCoreGraph $ lastMileSerializer lastMile descs
    writeFile outFile result
    putStrLn $ outFile ++ " (" ++ descEntities entities ++ ")"
  where
    descEntities entities = if L.length entities == 1 then "1 entity" else show (L.length entities) ++ " entities"

    jsonToTarget inFile adapter lmEncoder index payload = case stringToJsonValue payload of
        Left msg -> fail $ "Failed to read JSON payload #" ++ show index ++ " in file " ++ inFile ++ ": " ++ msg
        Right json -> withState emptyAvroEnvironment $ do
          -- TODO; the core graph is neither the data nor the schema graph
          let dataGraph = hydraCoreGraph
          let schemaGraph = Just hydraCoreGraph

          term <- coderEncode (adapterCoder adapter) json
          env <- getState
          let graph = elementsToGraph dataGraph schemaGraph (M.elems $ avroEnvironmentElements env)
          withState hydraCoreGraph $ lmEncoder term graph

-- | Given a payload format (one JSON object per file, or one per line),
--   a path to an Avro *.avsc schema, a path to a source directory containing JSON files conforming to the schema,
--   and a path to a destination directory, map each input file to a corresponding output file in the
--   destination directory. This transformation is sensitive to Hydra-specific annotations (primaryKey/foreignKey)
--   in the Avro schema, which tell Hydra which objects to treat as elements and which fields are references to elements.
transformAvroJsonDirectory :: LastMile Graph x -> FilePath -> FilePath -> FilePath -> IO ()
transformAvroJsonDirectory lastMile schemaPath srcDir destDir = do
    createDirectoryIfMissing True destDir
    schemaStr <- readFile schemaPath
    adapter <- fromFlowIo () $ loadAdapter schemaStr
    paths <- getDirectoryContents srcDir
    conf <- CM.mapM (transformFile adapter) paths
    return ()
  where
    loadAdapter schemaStr = do
      avroSchema <- coderDecode avroSchemaStringCoder schemaStr
      withState emptyAvroEnvironment $ avroHydraAdapter avroSchema

    transformFile adapter srcFile = do
      case jsonPayloadFormat srcFile of
        Nothing -> return False
        Just format -> do
          let destFile = replaceExtension srcFile (lastMileFileExtension lastMile)
          transformAvroJson format adapter lastMile (combine srcDir srcFile) (combine destDir destFile)
          return True

    jsonPayloadFormat fileName = if ext == ".json"
        then Just Json
        else if ext == ".jsonl"
        then Just Jsonl
        else Nothing
      where
        ext = takeExtension fileName
