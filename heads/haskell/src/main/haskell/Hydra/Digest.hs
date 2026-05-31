-- | Per-module source digests for skipping inference when nothing has changed.
--
-- The cache is intentionally all-or-nothing: if every module's DSL source
-- hash matches the stored digest and every expected JSON output exists,
-- the caller short-circuits. Otherwise it falls through to full inference
-- and overwrites the digest on success.

module Hydra.Digest (
    -- v1 API (backwards-compatible namespace → hash map)
    DigestMap,
    discoverModuleNameFiles,
    hashFile,
    hashUniverse,
    readDigest,
    writeDigest,
    digestPath,
    -- Per-package input digest (v1 + selfHash + deps; see PerPackageDigest)
    PerPackageDigest(..),
    emptyPerPackageDigest,
    computeSelfHash,
    readPerPackageDigest,
    writePerPackageDigest,
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

import Hydra.Packaging (Module(..), ModuleName(..))

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
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


type DigestMap = M.Map ModuleName String


-- | Root directory where DSL source files live. Relative paths are resolved
-- from the Haskell head's working directory ("heads/haskell"), which matches
-- the existing hs-source-dirs in package.yaml.
packagesRoot :: FilePath
packagesRoot = ".." FP.</> ".." FP.</> "packages"


-- | Walk packages/*/src/main/haskell/Hydra/Sources/ (Haskell DSL sources) and
-- packages/*/src/main/{java,python}/hydra/sources/ (native coder sources) to
-- build a namespace → file map. Each source file declares its namespace with
-- one of the recognized idioms (see 'extractNs' / 'extractNativeNs').
--
-- Native (.java/.py) sources are scanned because hydra-java/hydra-python are
-- self-hosted: their canonical hydra.<lang>.* modules are authored natively
-- (#344), not as Haskell DSL. Without scanning them, a change to e.g.
-- Coder.java would never invalidate the per-package input digest, so the
-- freshness gate would skip regeneration even though the coder changed (#400).
--
-- Files without a recognizable namespace declaration are silently skipped.
discoverModuleNameFiles :: IO (M.Map ModuleName FilePath)
discoverModuleNameFiles = do
    exists <- SD.doesDirectoryExist packagesRoot
    if not exists then return M.empty else do
      pkgs <- SD.listDirectory packagesRoot
      hsPairs     <- L.concat <$> mapM scanPackage pkgs
      nativePairs <- L.concat <$> mapM scanNativePackage pkgs
      -- Native sources are the authoritative owners of hydra.<lang>.* (#344),
      -- so they take precedence over any stale legacy Haskell DSL copy of the
      -- same namespace. M.union is left-biased, so list native pairs first.
      return $ M.union (M.fromList nativePairs) (M.fromList hsPairs)
  where
    scanPackage pkg = do
      let srcDir = packagesRoot FP.</> pkg FP.</> "src" FP.</> "main"
                               FP.</> "haskell" FP.</> "Hydra" FP.</> "Sources"
      isDir <- SD.doesDirectoryExist srcDir
      if not isDir then return [] else do
        files <- listFilesWithSuffix ".hs" srcDir
        Y.catMaybes <$> mapM extractNs files

    -- Scan a package's native (.java/.py) self-host coder sources, which live
    -- under packages/<pkg>/src/main/<lang>/hydra/sources/. Only hydra-java and
    -- hydra-python currently have these; other packages have no such dir and
    -- yield [].
    scanNativePackage pkg = do
      let javaDir = packagesRoot FP.</> pkg FP.</> "src" FP.</> "main"
                                FP.</> "java" FP.</> "hydra" FP.</> "sources"
          pyDir   = packagesRoot FP.</> pkg FP.</> "src" FP.</> "main"
                                FP.</> "python" FP.</> "hydra" FP.</> "sources"
      javaPairs <- scanNativeDir ".java" extractNativeNs javaDir
      pyPairs   <- scanNativeDir ".py"   extractNativeNs pyDir
      return $ javaPairs ++ pyPairs

    scanNativeDir suffix extract dir = do
      isDir <- SD.doesDirectoryExist dir
      if not isDir then return [] else do
        files <- listFilesWithSuffix suffix dir
        Y.catMaybes <$> mapM extract files

    listFilesWithSuffix suffix dir = do
      entries <- SD.listDirectory dir
      subResults <- CM.forM entries $ \e -> do
        let p = dir FP.</> e
        isDir <- SD.doesDirectoryExist p
        if isDir
          then listFilesWithSuffix suffix p
          else if suffix `L.isSuffixOf` e then return [p] else return []
      return $ concat subResults

    extractNs :: FilePath -> IO (Maybe (ModuleName, FilePath))
    extractNs fp = do
      content <- E.try (readFile fp) :: IO (Either E.SomeException String)
      case content of
        Left _ -> return Nothing
        Right s ->
          -- Three namespace declaration idioms appear across the source tree:
          --   1. Top-level `ns = ModuleName "..."` (kernel + most term-level sources).
          --   2. Inline `moduleName = (ModuleName "...")` inside a Module
          --      record (~half of non-kernel sources, e.g. hydra-pg, hydra-ext).
          --   3. Indented `ns = ModuleName "..."` inside a where clause
          --      (e.g. packages/hydra-haskell/.../Sources/Haskell/Coder.hs).
          -- We accept all three. Without cases 2 and 3, those files are
          -- absent from the per-package digest, which causes silent cache hits
          -- in Phase 3 when those sources change.
          let pat1 = "^[[:space:]]*ns = ModuleName \"([^\"]+)\"" :: String
              pat2 = "moduleName = .ModuleName \"([^\"]+)\"" :: String
              try1 = (s RE.=~ pat1 :: [[String]])
              try2 = (s RE.=~ pat2 :: [[String]])
          in case (try1, try2) of
               (([_, nsName]:_), _) -> return $ Just (ModuleName nsName, fp)
               (_, ([_, nsName]:_)) -> return $ Just (ModuleName nsName, fp)
               _                    -> return Nothing

    -- Extract the namespace a native (.java/.py) coder source defines for
    -- itself. Two idioms, one per host language:
    --   * Java:   `ModuleName NS = new ModuleName("hydra.java.<x>")`
    --   * Python: `NS = ModuleName("hydra.python.<x>")` (optionally `_NS`),
    --             at column 0.
    -- Both files also reference OTHER modules via `new ModuleName("...")`
    -- (Java) or `<NAME>_NS = ModuleName("...")` (Python) as dependency
    -- declarations; the patterns below are anchored to the file's own `NS`
    -- field so those dependency references are not mistaken for the owner.
    extractNativeNs :: FilePath -> IO (Maybe (ModuleName, FilePath))
    extractNativeNs fp = do
      content <- E.try (readFile fp) :: IO (Either E.SomeException String)
      case content of
        Left _ -> return Nothing
        Right s ->
          -- `ModuleName NS = new ModuleName("...")` — the space before `NS`
          -- (in `ModuleName NS`) ensures we don't match the dependency fields,
          -- whose names end in `_NS` (e.g. `ModuleName SYNTAX_NS = ...`,
          -- `ModuleName CORE_NS = ...`).
          let javaPat = "ModuleName NS = new ModuleName\\(\"([^\"]+)\"\\)" :: String
              -- `^_?NS = ModuleName("...")` — top-level, optional leading
              -- underscore (e.g. language.py uses `_NS`).
              pyPat   = "^_?NS = ModuleName\\(\"([^\"]+)\"\\)" :: String
              tryJava = (s RE.=~ javaPat :: [[String]])
              tryPy   = (s RE.=~ pyPat   :: [[String]])
          in case (tryJava, tryPy) of
               (([_, nsName]:_), _) -> return $ Just (ModuleName nsName, fp)
               (_, ([_, nsName]:_)) -> return $ Just (ModuleName nsName, fp)
               _                    -> return Nothing


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
hashUniverse :: M.Map ModuleName FilePath -> [Module] -> IO DigestMap
hashUniverse nsFiles mods = do
    let namespaces = map moduleName mods
    pairs <- CM.forM namespaces $ \ns ->
      case M.lookup ns nsFiles of
        Nothing -> return Nothing
        Just fp -> do
          result <- E.try (hashFile fp) :: IO (Either E.SomeException String)
          case result of
            Left _  -> return Nothing
            Right h -> return $ Just (ns, h)
    return $ M.fromList (Y.catMaybes pairs)


-- | Digest path for a single-tree writer: lives under the package's
-- build/ subdir, partitioned by source set.
-- Input `<pkg>/src/main/json` produces `<pkg>/build/main/digest.json`, and
-- `<pkg>/src/test/json` produces `<pkg>/build/test/digest.json`. The
-- per-source-set partitioning keeps main-tree and test-tree caches
-- distinct; the build/ root keeps the whole cache subtree gitignored
-- under a single .gitignore pattern (see #379).
digestPath :: FilePath -> FilePath
digestPath basePath =
    let srcSetDir = FP.takeDirectory basePath           -- <pkg>/src/<set>
        sourceSet = FP.takeFileName srcSetDir            -- <set>
        pkgRoot   = FP.takeDirectory (FP.takeDirectory srcSetDir)  -- <pkg>
    in pkgRoot FP.</> "build" FP.</> sourceSet FP.</> "digest.json"


-- | Read a digest file. Absent or malformed → empty map.
--
-- The "encoderId" key (legacy from pre-#347 universe digests) is
-- silently ignored if present, for backward compatibility with old
-- on-disk digests — see retirement note above 'writeDigest'.
readDigest :: FilePath -> IO DigestMap
readDigest path = do
    exists <- SD.doesFileExist path
    if not exists then return M.empty else do
      result <- E.try (readFile path) :: IO (Either E.SomeException String)
      case result of
        Left _  -> return M.empty
        Right s -> return $ parseDigest s


-- | Write a digest file. Format: a minimal JSON object
-- { "formatVersion": 1, "version": 1, "hashes": { "<namespace>": "<hex>", ... } }
-- Keys are written in sorted order for deterministic output.
--
-- The two version fields are deliberately distinct:
--
--   * "formatVersion" describes the JSON encoding of sibling module files
--     (dist/json/<package>/.../*.json). See docs/json-format.md.
--     Bumped only when a parser for version N would mis-parse version N+1.
--   * "version" describes this digest file's own internal schema (v1 = simple
--     hash map, v2 = inputs/outputs/generator). It is not meant for consumers
--     gating on the module-JSON encoding.
--
-- Pre-#347 universe digests also carried a top-level "encoderId" field
-- (a content hash of four JSON-coder DSL source files). That mechanism
-- was retired when 'compute_generator_stamp' in bin/lib/assemble-common.sh
-- subsumed it as a per-target transform identity. Legacy on-disk
-- encoderIds are tolerated by 'parseDigest' (silently ignored) but no
-- longer produced or compared.
writeDigest :: FilePath -> DigestMap -> IO ()
writeDigest path digest = do
    SD.createDirectoryIfMissing True (FP.takeDirectory path)
    writeFile path (serializeDigest digest)


-- | Minimal JSON parser for the digest file. We deliberately avoid pulling
-- in aeson's full machinery here because the format is trivial and we want
-- tolerant parsing (a malformed digest silently becomes an empty map).
-- The regex only matches `"key": "quoted_value"` pairs, so it naturally
-- skips the `"formatVersion"`, `"version"`, and `"hashes": { ... }` scaffolding.
--
-- Top-level non-namespace keys are filtered out:
--   * "encoderId" — legacy from pre-#347 universe digests (retired).
--   * "selfHash"  — per-package digest's package-level hash (#347).
--   * "depHash:<pkg>" — per-package digest's recorded dep self-hashes (#347).
--     The "depHash:" prefix makes them syntactically distinct from
--     namespace entries (namespaces use "." as separator; packages use "-"
--     and the prefix removes any chance of collision).
parseDigest :: String -> DigestMap
parseDigest s =
    let kvPattern = "\"([^\"]+)\"[[:space:]]*:[[:space:]]*\"([^\"]+)\"" :: String
        matches   = s RE.=~ kvPattern :: [[String]]
    in M.fromList [ (ModuleName k, v)
                  | (_:k:v:_) <- matches
                  , k /= "encoderId"
                  , k /= "selfHash"
                  , not ("depHash:" `L.isPrefixOf` k)
                  ]


-- | Serialize a digest. Schema:
-- { "formatVersion": 1, "version": 1, "hashes": { "<ns>": "<hex>", ... } }
serializeDigest :: DigestMap -> String
serializeDigest digest = unlines $
    ["{"
    ,"  \"formatVersion\": 1,"
    ,"  \"version\": 1,"
    ,"  \"hashes\": {"]
    ++ hashLines ++
    ["  }"
    ,"}"]
  where
    entries = L.sortBy (\a b -> compare (fst a) (fst b)) (M.toList digest)
    hashLines = zipWith renderEntry [0..] entries
    renderEntry i (ModuleName ns, h) =
      let sep = if i == length entries - 1 then "" else ","
      in "    \"" ++ ns ++ "\": \"" ++ h ++ "\"" ++ sep


----------------------------------------------------------------------
-- Per-package input digest (#347 transitive A-side invalidation).
----------------------------------------------------------------------
-- Extends the v1 namespace→hash map with two extra fields:
--
--   * selfHash — SHA-256 over this package's own namespace hashes
--     (sorted by namespace name). A single string that summarizes "what
--     does this package's source content look like." Recorded into each
--     per-target output digest's transform-identity slot so that any
--     edit to any module in the package invalidates downstream regen.
--
--   * deps — map of declared-dependency-package-name → that dep's
--     selfHash, captured at the time this digest was written. When a
--     dep package's selfHash changes (because someone edited a module
--     in it), this package's recorded deps entry no longer matches,
--     and downstream caches invalidate transitively.
--
-- On-disk schema (a v1 file with selfHash + depHash:<pkg> top-level
-- entries, plus the existing hashes block):
--
--   {
--     "formatVersion": 1,
--     "version": 1,
--     "selfHash": "<hex>",
--     "depHash:hydra-kernel": "<hex>",
--     "depHash:hydra-rdf":    "<hex>",
--     "hashes": { "<ns>": "<hex>", ... }
--   }
--
-- The "depHash:" prefix keeps dep entries syntactically distinct from
-- namespace entries (namespaces don't contain ":") and lets the legacy
-- regex-based parseDigest reject them cleanly.
--
-- Backward compat: a pre-#347 on-disk digest has no selfHash field;
-- readPerPackageDigest returns empty strings/maps for those, and the
-- next regen will rewrite the file with the new fields populated.
data PerPackageDigest = PerPackageDigest
  { ppHashes   :: DigestMap          -- per-namespace source hashes
  , ppSelfHash :: String             -- hash over ppHashes (empty if legacy)
  , ppDeps     :: M.Map String String  -- depPkgName → that pkg's selfHash
  } deriving (Show, Eq)

emptyPerPackageDigest :: PerPackageDigest
emptyPerPackageDigest = PerPackageDigest M.empty "" M.empty

-- | Compute the package's selfHash from its own namespace hashes.
-- Deterministic: entries sorted lex by namespace, joined with explicit
-- separators so a hash collision can't be engineered by clever naming.
computeSelfHash :: DigestMap -> String
computeSelfHash digest =
    let entries = L.sortBy (\(a,_) (b,_) -> compare a b) (M.toList digest)
        rendered = concatMap (\(ModuleName ns, h) -> ns ++ "\t" ++ h ++ "\n") entries
    in SHA.showDigest (SHA.sha256 (BLC.pack rendered))

readPerPackageDigest :: FilePath -> IO PerPackageDigest
readPerPackageDigest path = do
    exists <- SD.doesFileExist path
    if not exists then return emptyPerPackageDigest else do
      result <- E.try (readFile path) :: IO (Either E.SomeException String)
      case result of
        Left _  -> return emptyPerPackageDigest
        -- Force the string fully before returning so the underlying file
        -- handle is closed promptly. Without this, lazy I/O can hold the
        -- handle open across a subsequent writeFile to the same path,
        -- producing "resource busy (file is locked)" errors (#347 #11).
        Right s -> length s `seq` return (parsePerPackageDigest s)

parsePerPackageDigest :: String -> PerPackageDigest
parsePerPackageDigest s =
    let kvPattern = "\"([^\"]+)\"[[:space:]]*:[[:space:]]*\"([^\"]+)\"" :: String
        matches   = s RE.=~ kvPattern :: [[String]]
        pairs     = [(k, v) | (_:k:v:_) <- matches]
        hashesMap = M.fromList
          [ (ModuleName k, v) | (k, v) <- pairs
          , k /= "encoderId"
          , k /= "selfHash"
          , not ("depHash:" `L.isPrefixOf` k)
          ]
        depsMap   = M.fromList
          [ (drop (length ("depHash:" :: String)) k, v)
          | (k, v) <- pairs, "depHash:" `L.isPrefixOf` k
          ]
        selfH     = Y.fromMaybe "" (L.lookup "selfHash" pairs)
    in PerPackageDigest hashesMap selfH depsMap

writePerPackageDigest :: FilePath -> PerPackageDigest -> IO ()
writePerPackageDigest path ppd = do
    SD.createDirectoryIfMissing True (FP.takeDirectory path)
    writeFile path (serializePerPackageDigest ppd)

serializePerPackageDigest :: PerPackageDigest -> String
serializePerPackageDigest (PerPackageDigest hashes selfH deps) = unlines $
    ["{"
    ,"  \"formatVersion\": 1,"
    ,"  \"version\": 1,"]
    ++ selfHashLines
    ++ depLines
    ++ ["  \"hashes\": {"]
    ++ hashLines ++
    ["  }"
    ,"}"]
  where
    selfHashLines =
      if null selfH
        then []
        else ["  \"selfHash\": \"" ++ selfH ++ "\","]
    depEntries = L.sortBy (\(a,_) (b,_) -> compare a b) (M.toList deps)
    depLines = map (\(pkg, h) ->
      "  \"depHash:" ++ pkg ++ "\": \"" ++ h ++ "\",") depEntries
    hashEntries = L.sortBy (\a b -> compare (fst a) (fst b)) (M.toList hashes)
    hashLines = zipWith renderEntry [0..] hashEntries
    renderEntry i (ModuleName ns, h) =
      let sep = if i == length hashEntries - 1 then "" else ","
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
    -- #347 transitive-invalidation fields, recorded at refresh time and
    -- compared at freshness-check time alongside per-namespace inputs:
    , digestRecordedSelfHash :: String  -- input package's selfHash
    , digestRecordedDeps     :: M.Map String String  -- depPkg → selfHash
    } deriving (Eq, Show)

emptyDigest :: Digest
emptyDigest = Digest M.empty M.empty "" "" M.empty

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
      && digestRecordedSelfHash a == digestRecordedSelfHash b
      && digestRecordedDeps a == digestRecordedDeps b

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
    ]
    ++ selfHashLine
    ++ depHashLines
    ++ [ "  \"inputs\": {" ]
    ++ entries (digestInputs d) ++
    [ "  },"
    , "  \"outputs\": {"
    ] ++ entries (digestOutputs d) ++
    [ "  }"
    , "}"
    ]
  where
    -- #347 transitive fields: emit only when non-empty so legacy on-disk
    -- digests (no selfHash, no deps) and current ones round-trip cleanly.
    selfHashLine =
      if null (digestRecordedSelfHash d)
        then []
        else ["  \"selfHash\": " ++ jsonString (digestRecordedSelfHash d) ++ ","]
    depHashLines =
      [ "  \"depHash:" ++ pkg ++ "\": " ++ jsonString h ++ ","
      | (pkg, h) <- L.sortBy (\(a,_) (b,_) -> compare a b)
                      (M.toList (digestRecordedDeps d))
      ]

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
        -- selfHash is also a top-level string (added by #347). Absent
        -- on legacy digests; default to "".
        selfPat = "\"selfHash\"[[:space:]]*:[[:space:]]*\"([^\"]*)\"" :: String
        selfH   = case s RE.=~ selfPat :: [[String]] of
                    ((_:h:_):_) -> h
                    _           -> ""
        -- depHash:<pkg> entries are flat top-level strings (the prefix
        -- keeps them syntactically distinct from filepath inputs/outputs).
        -- Restrict to the header region (before "inputs") so we don't
        -- accidentally match anything appearing later.
        headerEnd = case findIndex "\"inputs\"" s of
                      Just i  -> i
                      Nothing -> length s
        header = take headerEnd s
        depPat = "\"depHash:([^\"]+)\"[[:space:]]*:[[:space:]]*\"([^\"]*)\"" :: String
        depEntries = (header RE.=~ depPat) :: [[String]]
        deps    = M.fromList [(pkg, h) | (_:pkg:h:_) <- depEntries]
        -- Split on "outputs": to give us two halves; the inputs half is
        -- everything before, outputs half is everything after.
        (inHalf, outHalf) = splitOnOutputs s
        entryPat = "\"([^\"]+)\"[[:space:]]*:[[:space:]]*\\{[[:space:]]*\"kind\"[[:space:]]*:[[:space:]]*\"([^\"]*)\"[[:space:]]*,[[:space:]]*\"hash\"[[:space:]]*:[[:space:]]*\"([^\"]*)\"" :: String
        parseEntries :: String -> M.Map FilePath DigestEntry
        parseEntries half =
          let ms = (half RE.=~ entryPat) :: [[String]]
          in M.fromList [ (path, DigestEntry (stringToKind k) h)
                        | (_:path:k:h:_) <- ms
                        ]
    in Digest
        { digestInputs           = parseEntries inHalf
        , digestOutputs          = parseEntries outHalf
        , digestGenerator        = gen
        , digestRecordedSelfHash = selfH
        , digestRecordedDeps     = deps
        }

-- Split the digest text into the inputs region (everything up to and
-- including the first `"outputs"` key) and the outputs region (after).
-- If we can't find the boundary, treat everything as inputs.
splitOnOutputs :: String -> (String, String)
splitOnOutputs s =
    case findIndex "\"outputs\"" s of
      Just i  -> (take i s, drop i s)
      Nothing -> (s, "")

-- | First index at which 'needle' starts in 'hay', or Nothing.
-- Naive O(n*m) search; the strings are short (digest files are < 100KB).
findIndex :: String -> String -> Maybe Int
findIndex needle hay =
    let n = length needle
        go ix rest
          | length rest < n = Nothing
          | take n rest == needle = Just ix
          | otherwise = go (ix + 1) (drop 1 rest)
    in go 0 hay
