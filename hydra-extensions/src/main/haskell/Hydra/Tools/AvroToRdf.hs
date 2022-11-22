-- | A utility for transforming Avro-compliant JSON to RDF

module Hydra.Tools.AvroToRdf (
  TransformWorkflow(..),
  JsonPayloadFormat(..),
  avroJsonDirectoryToNtriples,
  executeAvroToRdfWorkflow,
) where

import Hydra.All
import Hydra.Impl.Haskell.Dsl.Standard
import qualified Hydra.Impl.Haskell.Dsl.Terms as Terms
import Hydra.Impl.Haskell.Ext.Json.Serde
import qualified Hydra.Ext.Avro.Schema as Avro
import qualified Hydra.Ext.Json.Model as Json
import Hydra.Ext.Json.Eliminate
import Hydra.Ext.Avro.Coder
import Hydra.Ext.Avro.SchemaJson
import Hydra.Rewriting
import qualified Hydra.Ext.Shacl.Coder as Shacl
import qualified Hydra.Ext.Rdf.Syntax as Rdf
import Hydra.Ext.Rdf.Serde
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


data JsonPayloadFormat = OnePerFile | OnePerLine

-- | Given a payload format (one JSON object per file, or one per line),
--   a path to an Avro *.avsc schema, a path to a source directory containing JSON files conforming to the schema,
--   and a path to a destination directory, map each input file to a corresponding output file in the
--   destination directory. This transformation is sensitive to Hydra-specific annotations (primaryKey/foreignKey)
--   in the Avro schema, which tell Hydra which objects to treat as elements and which fields are references to elements.
avroJsonDirectoryToNtriples :: JsonPayloadFormat -> FilePath -> FilePath -> FilePath -> IO ()
avroJsonDirectoryToNtriples format schemaPath srcDir destDir = do
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
      if takeExtension srcFile /= ".json"
        then return False
        else do
          let destFile = replaceExtension srcFile "nt"
          avroJsonToNtriples format adapter (combine srcDir srcFile) (combine destDir destFile)
          return True

avroJsonToNtriples :: JsonPayloadFormat -> AvroHydraAdapter Meta -> FilePath -> FilePath -> IO ()
avroJsonToNtriples format adapter inFile outFile = do
    putStr $ "\t" ++ inFile ++ " --> "
    contents <- readFile inFile
    let entities = case format of
          OnePerFile -> [contents]
          OnePerLine -> L.filter (not . L.null) $ lines contents
    descs <- L.concat <$> fromFlowIo coreContext (CM.zipWithM jsonToRdfDescription [1..] entities)
    writeFile outFile $ rdfGraphToString $ Shacl.descriptionsToGraph descs
    putStrLn $ outFile ++ " (" ++ descEntities entities ++ ")"
  where
    descEntities entities = if L.length entities == 1 then "1 entity" else show (L.length entities) ++ " entities"
    jsonToRdfDescription index payload = case stringToValue payload of
      Left msg -> fail $ "Failed to read JSON payload #" ++ show index ++ " in file " ++ inFile ++ ": " ++ msg
      Right json -> withState emptyEnv $ do
        term <- coderEncode (adapterCoder adapter) json
        env <- getState
        let graph = Graph (avroEnvironmentElements env) Nothing -- TODO; schema graph is not the core graph
        withState coreContext $ encodeTerms term graph

    encodeTerms term graph = do
        elDescs <- CM.mapM encodeElement $ M.elems $ graphElements graph
        termDescs <- encodeBlankTerm
        return $ L.concat (termDescs:elDescs)
      where
        encodeElement el = do
          let subject = Rdf.ResourceIri $ Shacl.nameToIri $ elementName el
          Shacl.encodeTerm subject $ listsToSets $ elementData el
        encodeBlankTerm = if notInGraph
          then do
            subject <- Shacl.nextBlankNode
            Shacl.encodeTerm subject $ listsToSets term
          else pure []
        notInGraph = L.null $ L.filter (\e -> elementData e == term) $ M.elems $ graphElements graph

emptyEnv :: AvroEnvironment Meta
emptyEnv = emptyEnvironment

-- | A convenience for avroJsonDirectoryToNtriples, bundling all of the input parameters together as a workflow
executeAvroToRdfWorkflow :: TransformWorkflow -> IO ()
executeAvroToRdfWorkflow (TransformWorkflow name schemaSpec srcDir destDir) = do
    schemaPath <- case schemaSpec of
      SchemaSpecFile p -> pure p
      _ -> fail "unsupported schema spec"
    putStrLn $ "Executing workflow " ++ show name ++ ":"
    avroJsonDirectoryToNtriples OnePerLine schemaPath srcDir destDir

-- Replace all lists with sets, for better query performance.
-- This is a last-mile step which breaks type/term conformance
-- (a more robust solution would modify the target language in the SHACL coder, so that list types are also transformed to set types).
listsToSets :: Term Meta -> Term Meta
listsToSets = rewriteTerm mapExpr id
  where
    mapExpr recurse = recurse . replaceLists
    replaceLists term = case term of
      TermList els -> TermSet $ S.fromList els
      _ -> term
