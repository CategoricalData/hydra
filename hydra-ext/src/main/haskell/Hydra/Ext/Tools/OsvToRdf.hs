-- | A utility for transforming OSV JSON to RDF. See https://osv.dev

module Hydra.Ext.Tools.OsvToRdf (
  executeOsvToRdfWorkflow,
  osvJsonDirectoryToNtriples,
) where

import Hydra.Kernel
import Hydra.Parsing (ParseResult(..), ParseSuccess(..), ParseError(..))
import qualified Hydra.Json.Parser as JsonParser
import Hydra.Ext.Org.Json.Coder
import Hydra.Generation
import Hydra.Formatting
import qualified Hydra.Json.Model as Json
import qualified Hydra.Ext.Shacl.Coder as Shacl
import Hydra.Ext.Rdf.Serde
import Hydra.Workflow
import qualified Hydra.Ext.Rdf.Utils as RdfUt

import qualified Hydra.Ext.Sources.Other.Osv as OsvSource
import qualified Hydra.Ext.Dev.Osv.Schema as Osv

import System.IO
import System.FilePath
import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import System.FilePath.Posix
import System.Directory


-- | Parse a JSON string, returning Either for compatibility
parseJsonEither :: String -> Either String Json.Value
parseJsonEither s = case JsonParser.parseJson s of
  ParseResultSuccess success -> Right (parseSuccessValue success)
  ParseResultFailure err -> Left (parseErrorMessage err)

emptyInstanceContext :: Graph -> Graph
emptyInstanceContext scx = elementsToGraph scx M.empty []

-- | A convenience for osvJsonDirectoryToNtriples, bundling all of the input parameters together as a workflow
executeOsvToRdfWorkflow :: TransformWorkflow -> IO ()
executeOsvToRdfWorkflow (TransformWorkflow name schemaSpec srcDir destDir) = do
    case schemaSpec of
      SchemaSpecProvided -> pure ()
      _ -> fail "unsupported schema spec"
    putStrLn $ "Executing workflow " ++ show name ++ ":"
    osvJsonDirectoryToNtriples srcDir destDir

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

-- | Given a source directory full of OSV JSON files, and a destination directory,
--   transform the content of each file to a corresponding RDF (N-Triples) file in the destination directory.
osvJsonDirectoryToNtriples :: FilePath -> FilePath -> IO ()
osvJsonDirectoryToNtriples srcDir destDir = do
    let cx = emptyContext
    entryType <- case requireType cx osvContext Osv._Entry of
      Left (InContext (OtherError msg) _) -> fail msg
      Right t -> return t
    coder <- case jsonCoder entryType cx osvInstanceContext of
      Left (InContext (OtherError msg) _) -> fail msg
      Right c -> return c
    paths <- getDirectoryContents srcDir
    conf <- CM.mapM (transformFile coder) paths
    let count = L.length $ L.filter id conf
    putStrLn $ "Transformed " ++ show count ++ " JSON files to N-triples"
  where
    transformFile coder srcFile = do
      if takeExtension srcFile /= ".json"
        then return False
        else do
          let destFile = replaceExtension srcFile "nt"
          osvJsonToNtriples coder (combine srcDir srcFile) (combine destDir destFile)
          return True

osvContext = modulesToGraph [OsvSource.module_] [OsvSource.module_]

osvInstanceContext = emptyInstanceContext osvContext

osvJsonToNtriples :: Coder (Term) Json.Value -> FilePath -> FilePath -> IO ()
osvJsonToNtriples coder inFile outFile = do
    contents <- readFile inFile
    case parseJsonEither contents of
      Left msg -> fail $ "Failed to read JSON value in file " ++ inFile ++ ": " ++ msg
      Right v -> do
        let v' = rewriteJsonFieldCase v
        let cx = emptyContext
        term0 <- case coderDecode coder cx v' of
          Left (InContext (OtherError msg) _) -> fail msg
          Right t -> return t
        let term1 = listsToSets term0
        let (node, _cx') = RdfUt.nextBlankNode emptyContext
        graph <- case Shacl.encodeTerm node term1 emptyContext osvContext of
          Left (InContext (OtherError msg) _) -> fail msg
          Right (descs, _) -> return $ RdfUt.descriptionsToGraph descs
        let graphStr = rdfGraphToNtriples graph
        writeFile outFile graphStr

rewriteJsonFieldCase :: Json.Value -> Json.Value
rewriteJsonFieldCase v = case v of
  Json.ValueArray values -> Json.ValueArray (rewriteJsonFieldCase <$> values)
  Json.ValueObject m -> Json.ValueObject $ M.fromList (rewriteEntry <$> M.toList m)
    where
      rewriteEntry (k, v) = (rewriteKey k, rewriteJsonFieldCase v)
      rewriteKey = convertCase CaseConventionLowerSnake CaseConventionCamel
  _ -> v
