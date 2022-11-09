-- | A utility for transforming OSV JSON to RDF. See https://osv.dev

module Hydra.Tools.OsvToRdf (osvJsonDirectoryToNtriples) where

import Hydra.All
import Hydra.Impl.Haskell.Ext.Json.Serde
import Hydra.Ext.Json.Coder
import Hydra.CoreDecoding
import Hydra.Impl.Haskell.GraphIO
import Hydra.Util.Formatting
import qualified Hydra.Ext.Json.Model as Json
import qualified Hydra.Ext.Shacl.Coder as Shacl
import Hydra.Ext.Rdf.Serde

import Hydra.Models.Osv
import qualified Dev.Osv.Schema as Osv

import System.IO
import System.FilePath
import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import System.FilePath.Posix
import System.Directory


emptyInstanceContext :: Context Meta -> Context Meta
emptyInstanceContext scx = scx {contextGraph = Graph M.empty (Just $ contextGraph scx)}

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

-- | Given a source directory full of OSV JSON files, and a destination directory,
--   transform the content of each file to a corresponding RDF (N-Triples) file in the destination directory.
osvJsonDirectoryToNtriples :: FilePath -> FilePath -> IO ()
osvJsonDirectoryToNtriples srcDir destDir = do
    entryType <- fromFlowIo osvContext (requireType Osv._Entry)
    coder <- fromFlowIo osvInstanceContext $ jsonCoder entryType
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

osvContext = modulesToContext [osvSchemaModule]

osvInstanceContext = emptyInstanceContext osvContext

osvJsonToNtriples :: Coder (Context Meta) (Context Meta) (Term Meta) Json.Value -> FilePath -> FilePath -> IO ()
osvJsonToNtriples coder inFile outFile = do
    contents <- readFile inFile
    case stringToValue contents of
      Left msg -> fail $ "Failed to read JSON value in file " ++ inFile ++ ": " ++ msg
      Right v -> do
        let v' = rewriteJsonFieldCase v
        term0 <- fromFlowIo osvInstanceContext $ coderDecode coder v'
        let term1 = listsToSets term0
        node <- fromFlowIo osvInstanceContext $ Shacl.nextBlankNode
        graph <- fromFlowIo osvInstanceContext $ (Shacl.descriptionsToGraph <$> Shacl.encodeTerm node term1)
        let graphStr = rdfGraphToString graph
        writeFile outFile graphStr

rewriteJsonFieldCase :: Json.Value -> Json.Value
rewriteJsonFieldCase v = case v of
  Json.ValueArray values -> Json.ValueArray (rewriteJsonFieldCase <$> values)
  Json.ValueObject m -> Json.ValueObject $ M.fromList (rewriteEntry <$> M.toList m)
    where
      rewriteEntry (k, v) = (rewriteKey k, rewriteJsonFieldCase v)
      rewriteKey = convertCase CaseConventionLowerSnake CaseConventionCamel
  _ -> v
