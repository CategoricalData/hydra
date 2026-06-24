-- | Per-module source digests for skipping inference when nothing has changed.
--
-- The cache is intentionally all-or-nothing: if every module's DSL source
-- hash matches the stored digest and every expected JSON output exists,
-- the caller short-circuits. Otherwise it falls through to full inference
-- and overwrites the digest on success.

{-# LANGUAGE ScopedTypeVariables #-}
module Hydra.Digest (
    -- v1 API (backwards-compatible namespace → hash map)
    DigestMap,
    discoverModuleNameFiles,
    hashFile,
    hashUniverse,
    hashPackageJsonContent,
    jsonContentKeyPrefix,
    readDigest,
    writeDigest,
    digestPath,
    -- Per-package input digest (v1 + selfHash + deps; see PerPackageDigest)
    PerPackageDigest(..),
    emptyPerPackageDigest,
    computeSelfHash,
    readPerPackageDigest,
    writePerPackageDigest,
    -- v2 API (richer digest with inputs, outputs, generation record)
    Digest(..),
    DigestEntry(..),
    DigestKind(..),
    GenerationMode(..),
    GenerationRecord(..),
    emptyDigest,
    emptyGenerationRecord,
    readDigestV2,
    writeDigestV2,
    hashFileV2,
    digestsMatch,
    verifyOutputsExist,
    generatorStamp,
    generationRecord,
    serializeDigestV2,
    parseDigestV2,
    -- Shared orphan-reconcile helpers (used by digest-check and the JSON
    -- write path; see #393 / #405)
    listFilesRecursive,
    pruneEmptyDirs,
    makeRelativeTo,
    reconcileOrphans,
) where

import Hydra.Packaging (Module(..), ModuleName(..))

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Maybe as Y
import qualified System.Directory as SD
import qualified System.Environment as SE
import qualified System.FilePath as FP
import qualified System.Process as SP
import qualified Data.Time.Clock as TC
import qualified Data.Time.Format as TF
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


-- | Prefix used to namespace JSON-content entries inside a per-package
-- input digest. Entries look like @jsonContent:<rel-path>@ where
-- @<rel-path>@ is the JSON file's path relative to
-- @<distJsonRoot>/<pkg>/src/main/json@.
--
-- The prefix keeps these entries syntactically distinct from real
-- @<namespace>@ keys (namespaces use @.@ as separator and never contain
-- @:@) and from the @depHash:@ prefix used by 'PerPackageDigest' for
-- transitive dep selfHashes (#347). 'parseDigest' and
-- 'parsePerPackageDigest' both treat unknown prefixes as opaque
-- key-value pairs that round-trip through 'hashes' — so no parser
-- change is needed to read or write them.
jsonContentKeyPrefix :: String
jsonContentKeyPrefix = "jsonContent:"

-- | Hash every @*.json@ file under @<distJsonRoot>/<pkg>/src/main/json@
-- and return them as digest entries keyed by
-- @jsonContent:<rel-path>@.
--
-- For native-coder packages (hydra-java, hydra-python), the JSON
-- content is the product of a *published coder runtime* applied to the
-- @.java@/@.py@ sources. The runtime can change behavior independently
-- of the sources (#398 reordered fields without touching any source),
-- and the existing source-hashing in 'hashUniverse' cannot see that.
-- Folding the JSON content into the per-package input digest closes
-- the gap: any change to the JSON the assembler is about to consume
-- invalidates the render gate, regardless of which writer produced it
-- (Phase-1 Haskell DSL, Phase-5 native driver, hand-edit, future
-- writers).
--
-- Returns an empty map if the JSON tree is absent (cold checkout).
-- Callers should fold this into the existing digest with
-- @M.union jsonHashes pkgDigest@ before writing.
hashPackageJsonContent :: FilePath -> String -> IO DigestMap
hashPackageJsonContent distJsonRoot pkg = do
    let jsonRoot = distJsonRoot FP.</> pkg FP.</> "src" FP.</> "main" FP.</> "json"
    exists <- SD.doesDirectoryExist jsonRoot
    if not exists then return M.empty else do
      files <- listFilesWithExt ".json" jsonRoot
      pairs <- CM.forM files $ \fp -> do
        result <- E.try (hashFile fp) :: IO (Either E.SomeException String)
        case result of
          Left _  -> return Nothing
          Right h -> do
            let rel = makeRelativeTo jsonRoot fp
                key = ModuleName (jsonContentKeyPrefix ++ rel)
            return $ Just (key, h)
      return $ M.fromList (Y.catMaybes pairs)
  where
    listFilesWithExt suffix dir = do
      entries <- SD.listDirectory dir
      subResults <- CM.forM entries $ \e -> do
        let p = dir FP.</> e
        isDir <- SD.doesDirectoryExist p
        if isDir
          then listFilesWithExt suffix p
          else if suffix `L.isSuffixOf` e then return [p] else return []
      return $ concat subResults


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
-- { "digestFormatVersion": 1, "moduleFormatVersion": 1, "hashes": { "<namespace>": "<hex>", ... } }
-- Keys are written in sorted order for deterministic output.
--
-- The two version fields are deliberately distinct (both reset to 1 for 0.16.0):
--
--   * "moduleFormatVersion" describes the JSON encoding of sibling module files
--     (dist/json/<package>/.../*.json). See docs/json-format.md.
--     Bumped only when a parser for version N would mis-parse version N+1.
--   * "digestFormatVersion" describes this digest file's own internal schema
--     (v1 = simple hash map, the inputs/outputs/generator layout = the v2
--     serializer below). It is not meant for consumers gating on the
--     module-JSON encoding.
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
-- skips the integer `"digestFormatVersion"` / `"moduleFormatVersion"` fields
-- and the `"hashes": { ... }` scaffolding.
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
-- { "digestFormatVersion": 1, "moduleFormatVersion": 1, "hashes": { "<ns>": "<hex>", ... } }
serializeDigest :: DigestMap -> String
serializeDigest digest = unlines $
    ["{"
    ,"  \"digestFormatVersion\": 1,"
    ,"  \"moduleFormatVersion\": 1,"
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
--     "digestFormatVersion": 1,
--     "moduleFormatVersion": 1,
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
    ,"  \"digestFormatVersion\": 1,"
    ,"  \"moduleFormatVersion\": 1,"]
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
-- generation provenance record (#413).
----------------------------------------------------------------------
-- A v2 digest records:
--   * inputs:  every file whose content determines the output of a sync
--              step (DSL sources, JSON files consumed by code generators,
--              hand-written runtime files copied in by post-processing).
--   * outputs: every file the sync step is responsible for producing.
--   * generation: a structured provenance record. The 'generationId' sub-field
--                 is the sole gating key (the cache-invalidation stamp); all
--                 other fields are informational and never gate freshness.
--
-- A freshness check is "all input hashes match recorded inputs AND all
-- output files exist with matching hashes AND generationId matches."
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

-- | Whether this artifact was produced by a published, versioned host or
-- by a locally-built migration shim. See #413 and #370.
--
-- 'GenerationModePublished': the host was consumed from a package registry
-- (Maven / PyPI / Hackage). The 'generationId' is a version pin such as
-- "hydra-java:0.16.0". The 'generationHydraVersion' field carries the
-- release version; 'generationRevision' is optional (the version pin
-- already identifies the commit).
--
-- 'GenerationModeShim': the host was built locally from source (the #370
-- fallback for backward-incompatible kernel changes). The 'generationId'
-- is a content hash. 'generationRevision' is required — the working-tree
-- SHA (with "-dirty" if uncommitted) is the shim's only precise identity.
-- 'generationHydraVersion' is omitted to avoid falsely stamping an
-- unreleased build with a release version.
data GenerationMode = GenerationModePublished | GenerationModeShim
    deriving (Eq, Show)

-- | Structured provenance record for a generated artifact (#413).
--
-- GATING vs INFORMATIONAL: only 'generationId' gates freshness checks.
-- All other fields are purely informational provenance. They MUST NOT
-- be added to 'digestsMatch' — see the invariant comment there.
data GenerationRecord = GenerationRecord
    { generationId           :: String             -- GATING: cache key; host-independent
    , generationMode         :: Maybe GenerationMode -- informational: published vs shim
    , generationHost         :: Maybe String         -- informational: "haskell"|"java"|...
    , generationHydraVersion :: Maybe String         -- informational: release version (published only)
    , generationRevision     :: Maybe String         -- informational: git SHA[-dirty] (required for shim)
    , generationTimestamp    :: Maybe String         -- informational: ISO-8601 wall time
    } deriving (Eq, Show)

emptyGenerationRecord :: GenerationRecord
emptyGenerationRecord = GenerationRecord "" Nothing Nothing Nothing Nothing Nothing

-- | A versioned digest for one sync step. Indexed by file path so that
-- callers can mix file types freely.
data Digest = Digest
    { digestInputs     :: M.Map FilePath DigestEntry
    , digestOutputs    :: M.Map FilePath DigestEntry
    , digestGeneration :: GenerationRecord  -- see GenerationRecord; only generationId gates
    -- #347 transitive-invalidation fields, recorded at refresh time and
    -- compared at freshness-check time alongside per-namespace inputs:
    , digestRecordedSelfHash :: String  -- input package's selfHash
    , digestRecordedDeps     :: M.Map String String  -- depPkg → selfHash
    } deriving (Eq, Show)

emptyDigest :: Digest
emptyDigest = Digest M.empty M.empty emptyGenerationRecord "" M.empty

-- | Hash any file by content. Returns a DigestEntry with the given kind
-- attached. Fails loudly if the file is missing — callers handle by
-- treating absent inputs as cache miss upstream.
hashFileV2 :: DigestKind -> FilePath -> IO DigestEntry
hashFileV2 kind fp = do
    h <- hashFile fp
    return (DigestEntry kind h)

-- | The sole gating component of the generation record: the
-- HYDRA_GENERATOR_STAMP env var if set, falling back to a fixed
-- placeholder. The stamp is treated as opaque by the freshness check;
-- any change invalidates downstream digests. Host-independent by design.
generatorStamp :: IO String
generatorStamp = do
    mEnv <- E.try (SE.getEnv "HYDRA_GENERATOR_STAMP") :: IO (Either E.SomeException String)
    case mEnv of
      Right s | not (null s) -> return s
      _                      -> return "v0-unstamped"

-- | Build the full GenerationRecord for the current invocation.
-- 'generationId' comes from 'generatorStamp' (the gating key).
-- Informational fields are gathered from env vars set by the calling
-- assembler script (HYDRA_GENERATOR_HOST, HYDRA_GENERATOR_MODE) plus
-- git and the system clock. Any field that cannot be determined is
-- left as Nothing and omitted from the serialized digest.
--
-- IMPORTANT: the informational fields are NEVER added to 'digestsMatch'.
-- They are recorded for human debuggers; they do not gate freshness.
generationRecord :: IO GenerationRecord
generationRecord = do
    stamp <- generatorStamp
    mHost <- getEnvMaybe "HYDRA_GENERATOR_HOST"
    mMode <- getEnvMaybe "HYDRA_GENERATOR_MODE"
    let mode = case mMode of
                 Just "published" -> Just GenerationModePublished
                 Just "shim"      -> Just GenerationModeShim
                 _                -> Nothing
    mVersion <- case mode of
      Just GenerationModePublished -> getEnvMaybe "HYDRA_GENERATOR_VERSION"
      _                            -> return Nothing
    mRevision <- gitRevision
    mTimestamp <- currentTimestamp
    return GenerationRecord
        { generationId           = stamp
        , generationMode         = mode
        , generationHost         = mHost
        , generationHydraVersion = mVersion
        , generationRevision     = mRevision
        , generationTimestamp    = mTimestamp
        }
  where
    getEnvMaybe var = do
      result <- E.try (SE.getEnv var) :: IO (Either E.SomeException String)
      case result of
        Right s | not (null s) -> return (Just s)
        _                      -> return Nothing

-- | Current git revision as "<short-sha>" or "<short-sha>-dirty" if there
-- are uncommitted changes. Returns Nothing if git is unavailable or the
-- working directory is not in a repo.
gitRevision :: IO (Maybe String)
gitRevision = do
    sha <- runGit ["rev-parse", "--short", "HEAD"]
    case sha of
      Nothing -> return Nothing
      Just s  -> do
        status <- runGit ["status", "--porcelain"]
        let dirty = case status of
              Just st | not (null (filter (/= '\n') st)) -> "-dirty"
              _                                           -> ""
        return (Just (trim s ++ dirty))
  where
    runGit args = do
      result <- E.try (SP.readProcess "git" args "") :: IO (Either E.SomeException String)
      case result of
        Left _  -> return Nothing
        Right s -> return (Just s)
    trim = L.dropWhileEnd (\c -> c == '\n' || c == '\r' || c == ' ')

-- | Current UTC time as an ISO-8601 string (e.g. "2026-06-23T14:05:00Z").
-- Returns Nothing if the clock cannot be read.
currentTimestamp :: IO (Maybe String)
currentTimestamp = do
    result <- E.try TC.getCurrentTime :: IO (Either E.SomeException TC.UTCTime)
    case result of
      Left _  -> return Nothing
      Right t -> return (Just (TF.formatTime TF.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" t))

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
-- output, and generationId fields all match. Output hashes are NOT
-- compared against the filesystem here — see 'verifyOutputsExist'
-- for that.
--
-- INVARIANT: this is an explicit ALLOWLIST of the fields that gate
-- freshness. It must never become a whole-'Digest' equality (@a == b@) or
-- a hash of the serialized digest file. The informational fields in
-- 'GenerationRecord' (mode, host, hydraVersion, revision, timestamp)
-- MUST be omitted here. A timestamp varies across byte-identical rebuilds;
-- 'host' varies across hosts that — by the self-hosting contract — produce
-- identical output. Gating on either causes spurious cache misses and
-- punishes self-hosting. Only 'generationId' is host-independent and
-- content-determining, so only it gates. A field gates iff it appears
-- below; add a field here only if it is deterministic AND content-determining.
digestsMatch :: Digest -> Digest -> Bool
digestsMatch a b =
    digestInputs a == digestInputs b
      && digestOutputs a == digestOutputs b
      && generationId (digestGeneration a) == generationId (digestGeneration b)
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
-- v2 serialization (#413: flat "generator" → structured "generation").
--
-- File layout (deliberately readable + tolerant):
--
--   {
--     "digestFormatVersion": 1,
--     "moduleFormatVersion": 1,
--     "generation": {
--       "generatorId": "<stamp>",          ← GATING; only this field gates freshness
--       "mode":        "published"|"shim", ← informational
--       "host":        "<lang>",           ← informational
--       "hydraVersion": "<ver>",           ← informational; published mode only
--       "revision":    "<sha>[-dirty]",    ← informational; required for shim
--       "timestamp":   "<iso8601>"         ← informational
--     },
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
-- Backward compat: digests written before #413 carry a flat
-- "generator": "<stamp>" at the top level. The parser reads that as
-- generatorId with all other generation fields absent (Nothing). The
-- serializer never writes the old flat form.
-- Unknown kinds round-trip as KindOther.
----------------------------------------------------------------------

serializeDigestV2 :: Digest -> String
serializeDigestV2 d = unlines $
    [ "{"
    , "  \"digestFormatVersion\": 1,"
    , "  \"moduleFormatVersion\": 1,"
    ]
    ++ generationLines (digestGeneration d)
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
    generationLines gr =
      let fields = concat
            [ [("generatorId", generationId gr)]
            , maybe [] (\m -> [("mode", modeToString m)]) (generationMode gr)
            , maybe [] (\h -> [("host", h)]) (generationHost gr)
            , maybe [] (\v -> [("hydraVersion", v)]) (generationHydraVersion gr)
            , maybe [] (\r -> [("revision", r)]) (generationRevision gr)
            , maybe [] (\t -> [("timestamp", t)]) (generationTimestamp gr)
            ]
          fieldLines = zipWith renderField [0..] fields
          renderField i (k, v) =
            let sep = if i == length fields - 1 then "" else ","
            in "    " ++ jsonString k ++ ": " ++ jsonString v ++ sep
      in [ "  \"generation\": {" ] ++ fieldLines ++ [ "  }," ]

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

modeToString :: GenerationMode -> String
modeToString GenerationModePublished = "published"
modeToString GenerationModeShim      = "shim"

stringToMode :: String -> Maybe GenerationMode
stringToMode "published" = Just GenerationModePublished
stringToMode "shim"      = Just GenerationModeShim
stringToMode _           = Nothing

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

-- Tolerant regex-based parser. Shapes recognised:
--   Structured (post-#413):
--     "generation": { ... "generatorId": "<stamp>", ... }
--   Legacy (pre-#413):
--     "generator": "<stamp>"
--   In both cases only generatorId (or the legacy stamp) is used as
--   the gating field; all other generation fields are parsed for
--   informational display but never compared in digestsMatch.
--   "<path>": { "kind": "<k>", "hash": "<h>" } in inputs/outputs sections.
parseDigestV2 :: String -> Digest
parseDigestV2 s =
    let -- Find the header region (before "inputs") for top-level fields.
        headerEnd = case findIndex "\"inputs\"" s of
                      Just i  -> i
                      Nothing -> length s
        header = take headerEnd s

        -- Try structured "generation" block first (post-#413).
        -- Locate the generation object: everything between "generation": { and
        -- the matching }.  We use a simple bracket scan; digest files are small.
        genBlock = extractGenerationBlock s
        genId = lookupInBlock "generatorId" genBlock
        genMode = stringToMode =<< lookupInBlock "mode" genBlock
        genHost = lookupInBlock "host" genBlock
        genVer  = lookupInBlock "hydraVersion" genBlock
        genRev  = lookupInBlock "revision" genBlock
        genTs   = lookupInBlock "timestamp" genBlock

        -- Fallback: legacy flat "generator" key (pre-#413 digests). Used only
        -- when the generation block is absent / generatorId not found there.
        legacyGenPat = "\"generator\"[[:space:]]*:[[:space:]]*\"([^\"]*)\"" :: String
        legacyGen = case (header RE.=~ legacyGenPat :: [[String]]) of
                      ((_:g:_):_) -> g
                      _           -> ""

        finalGenId = case genId of
          Just g | not (null g) -> g
          _                     -> legacyGen

        gr = GenerationRecord
               { generationId           = finalGenId
               , generationMode         = genMode
               , generationHost         = genHost
               , generationHydraVersion = genVer
               , generationRevision     = genRev
               , generationTimestamp    = genTs
               }

        -- selfHash is a top-level string (added by #347).
        selfPat = "\"selfHash\"[[:space:]]*:[[:space:]]*\"([^\"]*)\"" :: String
        selfH   = case (header RE.=~ selfPat :: [[String]]) of
                    ((_:h:_):_) -> h
                    _           -> ""
        -- depHash:<pkg> entries are flat top-level strings.
        depPat = "\"depHash:([^\"]+)\"[[:space:]]*:[[:space:]]*\"([^\"]*)\"" :: String
        depEntries = (header RE.=~ depPat) :: [[String]]
        deps    = M.fromList [(pkg, h) | (_:pkg:h:_) <- depEntries]
        -- Split on "outputs" to get inputs/outputs halves.
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
        , digestGeneration       = gr
        , digestRecordedSelfHash = selfH
        , digestRecordedDeps     = deps
        }

-- | Extract the text content of the "generation": { ... } block from a
-- digest string. Returns "" if not found.
extractGenerationBlock :: String -> String
extractGenerationBlock s =
    case findIndex "\"generation\"" s of
      Nothing -> ""
      Just i  ->
        let after = drop i s
        in case findIndex "{" after of
             Nothing -> ""
             Just j  ->
               let inner = drop (j + 1) after
               in takeUntilClose 0 inner
  where
    -- Collect characters until the closing '}', respecting nested braces.
    takeUntilClose _ [] = []
    takeUntilClose depth (c:cs)
      | c == '{' = c : takeUntilClose (depth + 1) cs
      | c == '}' = if depth == 0 then [] else c : takeUntilClose (depth - 1) cs
      | otherwise = c : takeUntilClose depth cs

-- | Look up a string field by key inside a JSON object fragment (the
-- text between the outer braces, as returned by 'extractGenerationBlock').
lookupInBlock :: String -> String -> Maybe String
lookupInBlock key block =
    let pat = "\"" ++ key ++ "\"[[:space:]]*:[[:space:]]*\"([^\"]*)\"" :: String
    in case (block RE.=~ pat :: [[String]]) of
         ((_:v:_):_) -> Just v
         _           -> Nothing

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


----------------------------------------------------------------------
-- Shared orphan-reconcile helpers (#393 / #405).
--
-- A keep-set-based prune: given an output directory and the set of files
-- that legitimately belong in it, delete everything else (the orphans).
-- Used by:
--   * digest-check fresh — the per-language target trees, keyed on the
--     recorded output digest (#393).
--   * the JSON write path — the dist/json/<pkg> trees, keyed on the
--     in-memory module emission set (#405).
-- Factored here so the two callers share one implementation.
----------------------------------------------------------------------

-- | Recursively list every regular file under a directory.
-- Skips dotfiles and dot-directories.
listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive root = do
    exists <- SD.doesDirectoryExist root
    if not exists then return [] else go root
  where
    go dir = do
      entries <- SD.listDirectory dir
      fmap concat $ CM.forM entries $ \e ->
        if "." `L.isPrefixOf` e
          then return []
          else do
            let p = dir FP.</> e
            isDir <- SD.doesDirectoryExist p
            if isDir
              then go p
              else do
                isFile <- SD.doesFileExist p
                return (if isFile then [p] else [])

-- | Remove any empty subdirectories under 'dir' (depth-first). 'dir'
-- itself is left alone; only its descendants are pruned. Used by the
-- orphan reconcile to clean up directories emptied by orphan deletion
-- (e.g. a renamed-away namespace dir). Best-effort: failures (e.g. a
-- directory that isn't actually empty due to a race) are ignored.
pruneEmptyDirs :: FilePath -> IO ()
pruneEmptyDirs dir = do
    entries <- SD.listDirectory dir `E.catch` \(_ :: E.IOException) -> return []
    CM.forM_ entries $ \e -> do
      let p = dir FP.</> e
      isDir <- SD.doesDirectoryExist p
      CM.when isDir $ do
        pruneEmptyDirs p
        children <- SD.listDirectory p `E.catch` \(_ :: E.IOException) -> return []
        CM.when (null children) $
          SD.removeDirectory p `E.catch` \(_ :: E.IOException) -> return ()

-- | Compute 'path' relative to 'base'. If 'path' isn't under 'base',
-- returns 'path' unchanged (callers should guard against that, but the
-- fallback keeps us from producing absolute paths accidentally).
makeRelativeTo :: FilePath -> FilePath -> FilePath
makeRelativeTo base path =
    let prefix = if not (null base) && last base == '/' then base else base ++ "/"
    in if prefix `L.isPrefixOf` path
         then drop (length prefix) path
         else path

-- | Delete every regular file under 'outputDir' whose path (relative to
-- 'outputDir', normalised) is not in 'keepRel', then prune any emptied
-- subdirectories. Files listed in 'protectRel' (relative, normalised) are
-- never deleted even if absent from the keep-set — used to shield a digest
-- file or other sidecar living inside the output dir. Returns the list of
-- deleted (absolute) paths so the caller can report them.
--
-- Deletion is best-effort (IOExceptions are swallowed) so a transient
-- failure on one file doesn't abort the whole reconcile.
reconcileOrphans :: FilePath -> S.Set FilePath -> S.Set FilePath -> IO [FilePath]
reconcileOrphans outputDir keepRel protectRel = do
    onDiskAbs <- listFilesRecursive outputDir
    let orphans =
          [ p
          | p <- onDiskAbs
          , let rel = FP.normalise (makeRelativeTo outputDir p)
          , not (S.member rel keepRel)
          , not (S.member rel protectRel)
          ]
    CM.forM_ orphans $ \p ->
      SD.removeFile p `E.catch` \(_ :: E.IOException) -> return ()
    CM.unless (null orphans) $ pruneEmptyDirs outputDir
    return orphans
