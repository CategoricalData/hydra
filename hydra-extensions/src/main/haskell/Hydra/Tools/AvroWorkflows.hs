-- | A utility for transforming Avro-compliant JSON to RDF

module Hydra.Tools.AvroWorkflows (
  TransformWorkflow(..),
  JsonPayloadFormat(..),
  TermEncoder(..),
  LastMile(..),
  executeAvroTransformWorkflow,
  propertyGraphLastMile,
  rdfDescriptionsToNtriples,
  shaclRdfLastMile,
  typedTermToShaclRdf,
  transformAvroJsonDirectory,
) where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Langs.Json.Serde
import qualified Hydra.Langs.Avro.Schema as Avro
import qualified Hydra.Langs.Json.Model as Json
import Hydra.Langs.Json.Eliminate
import Hydra.Langs.Avro.Coder
import Hydra.Langs.Avro.SchemaJson
import Hydra.Langs.Tinkerpop.Coder
import Hydra.Rewriting
import qualified Hydra.Langs.Shacl.Coder as Shacl
import qualified Hydra.Langs.Rdf.Syntax as Rdf
import qualified Hydra.Langs.Rdf.Utils as RdfUt
import qualified Hydra.Langs.Tinkerpop.PropertyGraph as PG
import qualified Hydra.Langs.Tinkerpop.Mappings as PGM
import qualified Hydra.Dsl.Expect as Expect
import Hydra.Langs.Rdf.Serde
import Hydra.Workflow

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import System.IO
import System.FilePath
import System.FilePath.Posix
import System.Directory


data JsonPayloadFormat = Json | Jsonl

type TermEncoder x = Term Kv -> Graph Kv -> GraphFlow Kv [x]


-- | A convenience for transformAvroJsonDirectory, bundling all of the input parameters together as a workflow
executeAvroTransformWorkflow :: LastMile x -> TransformWorkflow -> IO ()
executeAvroTransformWorkflow lastMile (TransformWorkflow name schemaSpec srcDir destDir) = do
    schemaPath <- case schemaSpec of
      SchemaSpecFile p -> pure p
      _ -> fail "unsupported schema spec"
    putStrLn $ "Executing workflow " ++ show name ++ ":"
    transformAvroJsonDirectory lastMile schemaPath srcDir destDir

-- Replace all lists with sets, for better query performance.
-- This is a last-mile step which breaks type/term conformance
-- (a more robust solution would modify the target language in the SHACL coder, so that list types are also transformed to set types).
listsToSets :: Term Kv -> Term Kv
listsToSets = rewriteTerm mapExpr id
  where
    mapExpr recurse = recurse . replaceLists
    replaceLists term = case term of
      TermList els -> TermSet $ S.fromList els
      _ -> term

pgElementsToJson :: [PG.Element String String String] -> String
pgElementsToJson els = jsonValueToString $ Json.ValueString "TODO"

propertyGraphLastMile :: LastMile (PG.Element String String String)
propertyGraphLastMile = LastMile typedTermToPropertyGraph pgElementsToJson "json"

rdfDescriptionsToNtriples :: [Rdf.Description] -> String
rdfDescriptionsToNtriples = rdfGraphToNtriples . RdfUt.descriptionsToGraph

shaclRdfLastMile :: LastMile Rdf.Description
shaclRdfLastMile = LastMile typedTermToShaclRdf rdfDescriptionsToNtriples "nt"

typedTermToPropertyGraph :: Type Kv -> GraphFlow Kv (Term Kv -> Graph Kv -> GraphFlow Kv [PG.Element String String String])
typedTermToPropertyGraph typ = do
    adapter <- elementCoder schema typ
    return $ \term graph -> do
      el <- coderEncode (adapterCoder adapter) term
      return [el]
  where
    schema = PGM.Schema {
        PGM.schemaVertexIds = mkCoder "encode vertex id" Expect.string,
        PGM.schemaEdgeIds = mkCoder "encode edge id" Expect.string,
        PGM.schemaPropertyTypes = mkCoder "encode property type" $ \t -> pure (),
        PGM.schemaPropertyValues = mkCoder "encode property value" Expect.string}
      where
        mkCoder lab encode = Coder (withTrace lab . encode) noDecode
          where
            noDecode = \_ -> fail "property graph schema decoding not yet supported"

typedTermToShaclRdf :: Type Kv -> GraphFlow Kv (Term Kv -> Graph Kv -> GraphFlow Kv [Rdf.Description])
typedTermToShaclRdf _ = pure encode
  where
    encode term graph = do
        elDescs <- CM.mapM encodeElement $ M.elems $ graphElements graph
        termDescs <- encodeBlankTerm
        return $ L.concat (termDescs:elDescs)
      where
        encodeElement el = do
          let subject = Rdf.ResourceIri $ RdfUt.nameToIri $ elementName el
          Shacl.encodeTerm subject $ listsToSets $ elementData el
        encodeBlankTerm = if notInGraph
          then do
            subject <- RdfUt.nextBlankNode
            Shacl.encodeTerm subject $ listsToSets term
          else pure []
        notInGraph = L.null $ L.filter (\e -> elementData e == term) $ M.elems $ graphElements graph

transformAvroJson :: JsonPayloadFormat -> AvroHydraAdapter Kv -> LastMile x -> FilePath -> FilePath -> IO ()
transformAvroJson format adapter lastMile inFile outFile = do
    putStr $ "\t" ++ inFile ++ " --> "
    contents <- readFile inFile
    let entities = case format of
          Json -> [contents]
          Jsonl -> L.filter (not . L.null) $ lines contents
    lmEncoder <- fromFlowIo hydraCore $ lastMileEncoder lastMile (adapterTarget adapter)
    descs <- L.concat <$> fromFlowIo hydraCore (CM.zipWithM (jsonToTarget inFile adapter lmEncoder) [1..] entities)
    writeFile outFile $ (lastMileSerializer lastMile) descs
    putStrLn $ outFile ++ " (" ++ descEntities entities ++ ")"
  where
    descEntities entities = if L.length entities == 1 then "1 entity" else show (L.length entities) ++ " entities"

    jsonToTarget inFile adapter lmEncoder index payload = case stringToJsonValue payload of
        Left msg -> fail $ "Failed to read JSON payload #" ++ show index ++ " in file " ++ inFile ++ ": " ++ msg
        Right json -> withState emptyEnv $ do
          -- TODO; the core graph is neither the data nor the schema graph
          let dataGraph = hydraCore
          let schemaGraph = Just hydraCore

          term <- coderEncode (adapterCoder adapter) json
          env <- getState
          let graph = elementsToGraph dataGraph schemaGraph (M.elems $ avroEnvironmentElements env)
          withState hydraCore $ lmEncoder term graph

-- | Given a payload format (one JSON object per file, or one per line),
--   a path to an Avro *.avsc schema, a path to a source directory containing JSON files conforming to the schema,
--   and a path to a destination directory, map each input file to a corresponding output file in the
--   destination directory. This transformation is sensitive to Hydra-specific annotations (primaryKey/foreignKey)
--   in the Avro schema, which tell Hydra which objects to treat as elements and which fields are references to elements.
transformAvroJsonDirectory :: LastMile x -> FilePath -> FilePath -> FilePath -> IO ()
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
      withState emptyEnv $ avroHydraAdapter avroSchema

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

emptyEnv = emptyAvroEnvironment createAnn
  where
    createAnn = Kv
