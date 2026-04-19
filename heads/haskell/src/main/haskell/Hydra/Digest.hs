-- | Per-module source digests for skipping inference when nothing has changed.
--
-- The cache is intentionally all-or-nothing: if every module's DSL source
-- hash matches the stored digest and every expected JSON output exists,
-- the caller short-circuits. Otherwise it falls through to full inference
-- and overwrites the digest on success.

module Hydra.Digest (
    -- v1 API (backwards-compatible namespace → hash map)
    DigestMap,
    discoverNamespaceFiles,
    hashFile,
    hashUniverse,
    readDigest,
    writeDigest,
    digestPath,
    -- v2 API (richer digest with inputs, outputs, generator stamp)
    Digest(..),
    DigestEntry(..),
    DigestKind(..),
    emptyDigest,
    readDigestV2,
    writeDigestV2,
    hashFileV2,
    digestsMatch,
    verifyOutputsExist,
    generatorStamp,
) where

import Hydra.Packaging (Module(..), Namespace(..))

import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Maybe as Y
import qualified System.Directory as SD
import qualified System.Environment as SE
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


----------------------------------------------------------------------
-- v2 API: per-package, per-target digest with inputs + outputs +
-- generator stamp.
----------------------------------------------------------------------
-- A v2 digest records:
--   * inputs:  every file whose content determines the output of a sync
--              step (DSL sources, JSON files consumed by code generators,
--              hand-written runtime files copied in by post-processing).
--   * outputs: every file the sync step is responsible for producing.
--   * generator: a stamp identifying the version of the generator that
--                produced these outputs, so generator changes invalidate
--                downstream caches.
--
-- A freshness check is "all input hashes match recorded inputs AND all
-- output files exist with matching hashes AND generator stamp matches."
-- Any mismatch falls through to a regen of the affected step.

-- | What kind of artifact is being recorded. Lets a single Digest mix
-- typed entries (DSL source, JSON file, target source, runtime support
-- file) without losing the discriminator.
data DigestKind
    = KindDslSource     -- A .hs source file under packages/*/src/main/haskell/Hydra/Sources/
    | KindJsonFile      -- A .json file under dist/json/
    | KindTargetFile    -- A generated source file under dist/<lang>/
    | KindRuntimeFile   -- A hand-written file under heads/*/src/ that gets copied in
    | KindOther         -- Anything else (escape hatch)
    deriving (Eq, Ord, Show, Read)

-- | One file's hash + classification. The path is the canonical key
-- (relative to the worktree root for portability).
data DigestEntry = DigestEntry
    { entryKind :: DigestKind
    , entryHash :: String  -- SHA-256 hex
    } deriving (Eq, Show)

-- | A versioned digest for one sync step. Indexed by file path so that
-- callers can mix file types freely.
data Digest = Digest
    { digestInputs    :: M.Map FilePath DigestEntry
    , digestOutputs   :: M.Map FilePath DigestEntry
    , digestGenerator :: String  -- generator stamp; see 'generatorStamp'
    } deriving (Eq, Show)

emptyDigest :: Digest
emptyDigest = Digest M.empty M.empty ""

-- | Hash any file by content. Returns a DigestEntry with the given kind
-- attached. Fails loudly if the file is missing — callers handle by
-- treating absent inputs as cache miss upstream.
hashFileV2 :: DigestKind -> FilePath -> IO DigestEntry
hashFileV2 kind fp = do
    h <- hashFile fp
    return (DigestEntry kind h)

-- | A stamp identifying the generator version. Right now just the
-- HYDRA_GENERATOR_STAMP env var if set, falling back to a fixed
-- placeholder. Future work: hash the bootstrap-from-json executable
-- itself, or read a commit SHA at build time.
--
-- The stamp is treated as opaque by the freshness check: any change
-- invalidates downstream digests across the board. This is a coarse
-- but safe trigger for "the code that produced these files changed."
generatorStamp :: IO String
generatorStamp = do
    mEnv <- E.try (SE.getEnv "HYDRA_GENERATOR_STAMP") :: IO (Either E.SomeException String)
    case mEnv of
      Right s | not (null s) -> return s
      _                      -> return "v0-unstamped"

-- | Read a v2 digest from disk. Absent or malformed → emptyDigest.
readDigestV2 :: FilePath -> IO Digest
readDigestV2 path = do
    exists <- SD.doesFileExist path
    if not exists then return emptyDigest else do
      result <- E.try (readFile path) :: IO (Either E.SomeException String)
      case result of
        Left _  -> return emptyDigest
        Right s -> return $ parseDigestV2 s

-- | Write a v2 digest to disk. Format is JSON-ish, sorted for
-- determinism, parseable by parseDigestV2 (regex-based, tolerant).
writeDigestV2 :: FilePath -> Digest -> IO ()
writeDigestV2 path d = do
    SD.createDirectoryIfMissing True (FP.takeDirectory path)
    writeFile path (serializeDigestV2 d)

-- | Two digests are equivalent for freshness purposes if their input,
-- output, and generator fields all match. Output hashes are NOT
-- compared against the filesystem here — see 'verifyOutputsExist'
-- for that.
digestsMatch :: Digest -> Digest -> Bool
digestsMatch a b =
    digestInputs a == digestInputs b
      && digestOutputs a == digestOutputs b
      && digestGenerator a == digestGenerator b

-- | For each output file recorded in the digest, verify the file
-- exists on disk and hashes to the recorded value. Returns True iff
-- every output is present and content-matched.
--
-- This catches "user deleted some files" or "files were partially
-- regenerated and corrupted." A digest match alone is not sufficient
-- proof of freshness because the dist/ tree could have been mutated
-- after the last write.
verifyOutputsExist :: Digest -> IO Bool
verifyOutputsExist d = do
    let outputs = M.toList (digestOutputs d)
    fmap and $ CM.forM outputs $ \(fp, entry) -> do
      exists <- SD.doesFileExist fp
      if not exists then return False else do
        result <- E.try (hashFile fp) :: IO (Either E.SomeException String)
        case result of
          Left _  -> return False
          Right h -> return (h == entryHash entry)


----------------------------------------------------------------------
-- v2 serialization.
--
-- File layout (deliberately readable + tolerant):
--
--   {
--     "version": 2,
--     "generator": "<stamp>",
--     "inputs": {
--       "<path>": { "kind": "DslSource", "hash": "<hex>" },
--       ...
--     },
--     "outputs": {
--       "<path>": { "kind": "JsonFile", "hash": "<hex>" },
--       ...
--     }
--   }
--
-- Parser is regex-based and recovers from formatting variations.
-- Unknown kinds round-trip as KindOther.
----------------------------------------------------------------------

serializeDigestV2 :: Digest -> String
serializeDigestV2 d = unlines $
    [ "{"
    , "  \"version\": 2,"
    , "  \"generator\": " ++ jsonString (digestGenerator d) ++ ","
    , "  \"inputs\": {"
    ] ++ entries (digestInputs d) ++
    [ "  },"
    , "  \"outputs\": {"
    ] ++ entries (digestOutputs d) ++
    [ "  }"
    , "}"
    ]
  where
    entries m =
      let kvs = L.sortBy (\a b -> compare (fst a) (fst b)) (M.toList m)
      in zipWith (renderEntry (length kvs)) [0..] kvs

    renderEntry total i (path, DigestEntry k h) =
      let sep = if i == total - 1 then "" else ","
      in "    " ++ jsonString path ++ ": { \"kind\": "
           ++ jsonString (kindToString k) ++ ", \"hash\": "
           ++ jsonString h ++ " }" ++ sep

    jsonString s = "\"" ++ concatMap escape s ++ "\""
    escape '\\' = "\\\\"
    escape '"'  = "\\\""
    escape c    = [c]

kindToString :: DigestKind -> String
kindToString KindDslSource   = "DslSource"
kindToString KindJsonFile    = "JsonFile"
kindToString KindTargetFile  = "TargetFile"
kindToString KindRuntimeFile = "RuntimeFile"
kindToString KindOther       = "Other"

stringToKind :: String -> DigestKind
stringToKind "DslSource"   = KindDslSource
stringToKind "JsonFile"    = KindJsonFile
stringToKind "TargetFile"  = KindTargetFile
stringToKind "RuntimeFile" = KindRuntimeFile
stringToKind _             = KindOther

-- Tolerant regex-based parser; ignores anything outside the recognized
-- shapes. The shapes we look for:
--   "generator": "<stamp>"           → captures generator stamp
--   "<path>": { "kind": "<k>", "hash": "<h>" } in inputs/outputs sections
parseDigestV2 :: String -> Digest
parseDigestV2 s =
    let -- Generator stamp is a top-level string.
        genPat = "\"generator\"[[:space:]]*:[[:space:]]*\"([^\"]*)\"" :: String
        gen    = case s RE.=~ genPat :: [[String]] of
                   ((_:g:_):_) -> g
                   _           -> ""
        -- Split on "outputs": to give us two halves; the inputs half is
        -- everything before, outputs half is everything after. This is
        -- coarse but lets us assign entries to the right map without
        -- proper JSON parsing.
        -- We anchor on the literal text "\"outputs\"" preceded by a
        -- closing brace + comma + whitespace to disambiguate from the
        -- (theoretical) word "outputs" appearing in a path.
        (inHalf, outHalf) = splitOnOutputs s
        entryPat = "\"([^\"]+)\"[[:space:]]*:[[:space:]]*\\{[[:space:]]*\"kind\"[[:space:]]*:[[:space:]]*\"([^\"]*)\"[[:space:]]*,[[:space:]]*\"hash\"[[:space:]]*:[[:space:]]*\"([^\"]*)\"" :: String
        parseEntries :: String -> M.Map FilePath DigestEntry
        parseEntries half =
          let ms = (half RE.=~ entryPat) :: [[String]]
          in M.fromList [ (path, DigestEntry (stringToKind k) h)
                        | (_:path:k:h:_) <- ms
                        ]
    in Digest
        { digestInputs    = parseEntries inHalf
        , digestOutputs   = parseEntries outHalf
        , digestGenerator = gen
        }

-- Split the digest text into the inputs region (everything up to and
-- including the first `"outputs"` key) and the outputs region (after).
-- If we can't find the boundary, treat everything as inputs.
splitOnOutputs :: String -> (String, String)
splitOnOutputs s =
    case findIndex "\"outputs\"" s of
      Just i  -> (take i s, drop i s)
      Nothing -> (s, "")
  where
    findIndex needle hay =
      let n = length needle
          go ix rest
            | length rest < n = Nothing
            | take n rest == needle = Just ix
            | otherwise = go (ix + 1) (drop 1 rest)
      in go 0 hay
