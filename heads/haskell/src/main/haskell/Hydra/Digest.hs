-- | Per-module source digests for skipping inference when nothing has changed.
--
-- The cache is intentionally all-or-nothing: if every module's DSL source
-- hash matches the stored digest and every expected JSON output exists,
-- the caller short-circuits. Otherwise it falls through to full inference
-- and overwrites the digest on success.

module Hydra.Digest (
    DigestMap,
    discoverNamespaceFiles,
    hashFile,
    hashUniverse,
    readDigest,
    writeDigest,
    digestPath,
) where

import Hydra.Packaging (Module(..), Namespace(..))

import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Maybe as Y
import qualified System.Directory as SD
import qualified System.FilePath as FP
import qualified Text.Regex.TDFA as RE
import qualified Control.Exception as E
import qualified Control.Monad as CM


type DigestMap = M.Map Namespace String


-- | Root directory where DSL source files live. Relative paths are resolved
-- from the Haskell head's working directory ("heads/haskell"), which matches
-- the existing hs-source-dirs in package.yaml.
packagesRoot :: FilePath
packagesRoot = ".." FP.</> ".." FP.</> "packages"


-- | Walk packages/*/src/main/haskell/Hydra/Sources/ to build a namespace →
-- file map. Each source file must declare its namespace with a top-level
-- line of the form: ns = Namespace "hydra.foo.bar"
--
-- Files without a recognizable ns declaration are silently skipped.
discoverNamespaceFiles :: IO (M.Map Namespace FilePath)
discoverNamespaceFiles = do
    exists <- SD.doesDirectoryExist packagesRoot
    if not exists then return M.empty else do
      pkgs <- SD.listDirectory packagesRoot
      pairs <- L.concat <$> mapM scanPackage pkgs
      return $ M.fromList pairs
  where
    scanPackage pkg = do
      let srcDir = packagesRoot FP.</> pkg FP.</> "src" FP.</> "main"
                               FP.</> "haskell" FP.</> "Hydra" FP.</> "Sources"
      isDir <- SD.doesDirectoryExist srcDir
      if not isDir then return [] else do
        files <- listHaskellFiles srcDir
        Y.catMaybes <$> mapM extractNs files

    listHaskellFiles dir = do
      entries <- SD.listDirectory dir
      subResults <- CM.forM entries $ \e -> do
        let p = dir FP.</> e
        isDir <- SD.doesDirectoryExist p
        if isDir
          then listHaskellFiles p
          else if ".hs" `L.isSuffixOf` e then return [p] else return []
      return $ concat subResults

    extractNs :: FilePath -> IO (Maybe (Namespace, FilePath))
    extractNs fp = do
      content <- E.try (readFile fp) :: IO (Either E.SomeException String)
      case content of
        Left _ -> return Nothing
        Right s ->
          let pattern = "^ns = Namespace \"([^\"]+)\"" :: String
              matches = s RE.=~ pattern :: [[String]]
          in case matches of
               ([_, nsName]:_) -> return $ Just (Namespace nsName, fp)
               _               -> return Nothing


-- | SHA-256 hex digest of the raw bytes of a file. Fails loudly if the file
-- cannot be read — callers treat failures as "cache miss" upstream.
hashFile :: FilePath -> IO String
hashFile fp = do
    bytes <- BL.readFile fp
    return $ SHA.showDigest (SHA.sha256 bytes)


-- | Hash every module in the given list against the namespace → file map.
-- Modules whose source file isn't found (because discovery missed them, or
-- they have no DSL source — e.g. generated coder modules loaded from JSON)
-- are absent from the returned map. The caller treats that as "cannot verify
-- freshness," which always falls through to full inference.
hashUniverse :: M.Map Namespace FilePath -> [Module] -> IO DigestMap
hashUniverse nsFiles mods = do
    let namespaces = map moduleNamespace mods
    pairs <- CM.forM namespaces $ \ns ->
      case M.lookup ns nsFiles of
        Nothing -> return Nothing
        Just fp -> do
          result <- E.try (hashFile fp) :: IO (Either E.SomeException String)
          case result of
            Left _  -> return Nothing
            Right h -> return $ Just (ns, h)
    return $ M.fromList (Y.catMaybes pairs)


-- | Digest path for a single-tree writer: sits next to the json/ subdir.
-- Input `<pkg>/src/main/json` produces `<pkg>/src/main/digest.json`, and
-- `<pkg>/src/test/json` produces `<pkg>/src/test/digest.json`. This keeps
-- main-tree and test-tree caches distinct.
digestPath :: FilePath -> FilePath
digestPath basePath = FP.takeDirectory basePath FP.</> "digest.json"


-- | Read a digest file. Absent or malformed → empty map.
readDigest :: FilePath -> IO DigestMap
readDigest path = do
    exists <- SD.doesFileExist path
    if not exists then return M.empty else do
      result <- E.try (readFile path) :: IO (Either E.SomeException String)
      case result of
        Left _  -> return M.empty
        Right s -> return $ parseDigest s


-- | Write a digest file. Format: a minimal JSON object
-- { "version": 1, "hashes": { "<namespace>": "<hex>", ... } }
-- Keys are written in sorted order for deterministic output.
writeDigest :: FilePath -> DigestMap -> IO ()
writeDigest path digest = do
    SD.createDirectoryIfMissing True (FP.takeDirectory path)
    writeFile path (serializeDigest digest)


-- | Minimal JSON parser for the digest file. We deliberately avoid pulling
-- in aeson's full machinery here because the format is trivial and we want
-- tolerant parsing (a malformed digest silently becomes an empty map).
-- The regex only matches `"key": "quoted_value"` pairs, so it naturally
-- skips the `"version": 1` and `"hashes": { ... }` scaffolding.
parseDigest :: String -> DigestMap
parseDigest s =
    let kvPattern = "\"([^\"]+)\"[[:space:]]*:[[:space:]]*\"([^\"]+)\"" :: String
        matches   = s RE.=~ kvPattern :: [[String]]
    in M.fromList [(Namespace k, v) | (_:k:v:_) <- matches]


serializeDigest :: DigestMap -> String
serializeDigest digest = unlines $
    ["{"
    ,"  \"version\": 1,"
    ,"  \"hashes\": {"]
    ++ hashLines ++
    ["  }"
    ,"}"]
  where
    entries = L.sortBy (\a b -> compare (fst a) (fst b)) (M.toList digest)
    hashLines = zipWith renderEntry [0..] entries
    renderEntry i (Namespace ns, h) =
      let sep = if i == length entries - 1 then "" else ","
      in "    \"" ++ ns ++ "\": \"" ++ h ++ "\"" ++ sep


