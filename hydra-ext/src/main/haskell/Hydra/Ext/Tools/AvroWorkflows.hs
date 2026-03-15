-- | A utility for transforming Avro-compliant JSON into other models

module Hydra.Ext.Tools.AvroWorkflows (
  TransformWorkflow(..),
  JsonPayloadFormat(..),
  TermEncoder(..),
  LastMile(..),
  defaultTinkerpopAnnotations,
  encodeStringValue,
  examplePgSchema,
  executeAvroTransformWorkflow,
  rdfDescriptionsToNtriples,
  shaclRdfLastMile,
  typeApplicationTermToShaclRdf,
  transformAvroJsonDirectory,
) where

import Hydra.Kernel
import Hydra.Ext.Workflow
import qualified Hydra.Show.Error as ShowError
import Hydra.Dsl.Annotations
import qualified Hydra.Ext.Org.Apache.Avro.Schema as Avro
import qualified Hydra.Json.Model as Json
import Hydra.Extract.Json
import Hydra.Parsing (ParseResult(..), ParseSuccess(..), ParseError(..))
import qualified Hydra.Json.Parser as JsonParser
import Hydra.Ext.Avro.Coder
import Hydra.Ext.Avro.Environment
import Hydra.Ext.Avro.SchemaJson hiding (Result)
import Hydra.Pg.Graphson.Utils
import qualified Hydra.Ext.Shacl.Coder as Shacl
import qualified Hydra.Ext.Org.W3.Rdf.Syntax as Rdf
import qualified Hydra.Ext.Rdf.Utils as RdfUt
import qualified Hydra.Pg.Model as PG
import qualified Hydra.Pg.Mapping as PGM
import Hydra.Ext.Rdf.Serde
import Hydra.Sources.Kernel.Types.Core
import Hydra.Pg.Graphson.Coder
import Hydra.Pg.Graphson.Syntax as G
import Hydra.Pg.Utils (defaultTinkerpopAnnotations, examplePgSchema)

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


type Result a = Either (InContext Error) a

-- | The last mile of a transformation, which encodes and serializes terms to a file
data LastMile a =
  LastMile {
    -- | An encoder for terms to a list of output objects
    lastMileEncoder :: (Type -> Context -> Graph -> Result (Term -> Graph -> Context -> Result [a])),
    -- | A function which serializes a list of output objects to a string representation
    lastMileSerializer :: ([a] -> Result String),
    -- | A file extension for the generated file(s)
    lastMileFileExtension :: String}

-- | Parse a JSON string, returning Either for compatibility
parseJsonEither :: String -> Either String Json.Value
parseJsonEither s = case JsonParser.parseJson s of
  ParseResultSuccess success -> Right (parseSuccessValue success)
  ParseResultFailure err -> Left (parseErrorMessage err)

eitherToIo :: Result a -> IO a
eitherToIo (Left ic) = fail (ShowError.error (inContextObject ic))
eitherToIo (Right v) = return v

data JsonPayloadFormat = Json | Jsonl

type TermEncoder x = Term -> Graph -> Context -> Result [x]

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
listsToSets :: Term -> Term
listsToSets = rewriteTerm mapExpr
  where
    mapExpr recurse = recurse . replaceLists
    replaceLists term = case term of
      TermList els -> TermSet $ S.fromList els
      _ -> term

rdfDescriptionsToNtriples :: [Rdf.Description] -> String
rdfDescriptionsToNtriples = rdfGraphToNtriples . RdfUt.descriptionsToGraph

shaclRdfLastMile :: LastMile Rdf.Description
shaclRdfLastMile = LastMile typeApplicationTermToShaclRdf (Right . rdfDescriptionsToNtriples) "nt"

typeApplicationTermToShaclRdf :: Type -> Context -> Graph -> Result (Term -> Graph -> Context -> Result [Rdf.Description])
typeApplicationTermToShaclRdf _ _cx _g = Right encode
  where
    encode term graf cx = do
        elDescs <- CM.mapM (encodeElement cx) $ graphToBindings graf
        termDescs <- encodeBlankTerm cx
        return $ L.concat (termDescs:elDescs)
      where
        encodeElement cx' el = do
          let subject = Rdf.ResourceIri $ RdfUt.nameToIri $ bindingName el
          fst <$> Shacl.encodeTerm subject (listsToSets $ bindingTerm el) cx' graf
        encodeBlankTerm cx' = if notInGraph
          then do
            let (subject, cx'') = RdfUt.nextBlankNode cx'
            fst <$> Shacl.encodeTerm subject (listsToSets term) cx'' graf
          else pure []
        notInGraph = L.null $ L.filter (\e -> bindingTerm e == term) $ graphToBindings graf

transformAvroJson :: JsonPayloadFormat -> Adapter Avro.Schema Type Json.Value Term -> LastMile x -> FilePath -> FilePath -> IO ()
transformAvroJson format adapter lastMile inFile outFile = do
    putStr $ "\t" ++ inFile ++ " --> "
    contents <- readFile inFile
    let cx = emptyContext
    let entities = case format of
          Json -> [contents]
          Jsonl -> L.filter (not . L.null) $ lines contents
    lmEncoder <- eitherToIo $ lastMileEncoder lastMile (adapterTarget adapter) cx hydraCoreGraph
    descs <- L.concat <$> CM.mapM (jsonToTarget inFile adapter lmEncoder cx) (L.zip [1..] entities)
    result <- eitherToIo $ lastMileSerializer lastMile descs
    writeFile outFile result
    putStrLn $ outFile ++ " (" ++ descEntities entities ++ ")"
  where
    descEntities entities = if L.length entities == 1 then "1 entity" else show (L.length entities) ++ " entities"

    jsonToTarget inFile' adapter' lmEncoder cx (index, payload) = case parseJsonEither payload of
        Left msg -> fail $ "Failed to read JSON payload #" ++ show index ++ " in file " ++ inFile' ++ ": " ++ msg
        Right json -> do
          -- TODO; the core graph is neither the data nor the schema graph
          let dataGraph = hydraCoreGraph
          term <- eitherToIo $ coderEncode (adapterCoder adapter') cx json
          let graph = dataGraph  -- Note: elements from AvroEnvironment are no longer dynamically tracked
          eitherToIo $ lmEncoder term graph cx

-- | Given a payload format (one JSON object per file, or one per line),
--   a path to an Avro *.avsc schema, a path to a source directory containing JSON files conforming to the schema,
--   and a path to a destination directory, map each input file to a corresponding output file in the
--   destination directory. This transformation is sensitive to Hydra-specific annotations (primaryKey/foreignKey)
--   in the Avro schema, which tell Hydra which objects to treat as elements and which fields are references to elements.
transformAvroJsonDirectory :: LastMile x -> FilePath -> FilePath -> FilePath -> IO ()
transformAvroJsonDirectory lastMile schemaPath srcDir destDir = do
    createDirectoryIfMissing True destDir
    schemaStr <- readFile schemaPath
    let cx = emptyContext
    adapter <- eitherToIo $ loadAdapter cx schemaStr
    paths <- getDirectoryContents srcDir
    conf <- CM.mapM (transformFile adapter) paths
    return ()
  where
    loadAdapter cx schemaStr = do
      avroSchema <- coderDecode (avroSchemaStringCoder cx) cx schemaStr
      fst <$> avroHydraAdapter cx avroSchema emptyAvroEnvironment

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
