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
    digestPath,
    -- Per-package input digest (v1 + selfHash + deps; see PerPackageDigest)
    PerPackageDigest(..),
    emptyPerPackageDigest,
    computeSelfHash,
    -- v2 API (richer digest with inputs, outputs, generator stamp)
    Digest(..),
    DigestEntry(..),
    DigestKind(..),
    Generation(..),
    GenerationMode(..),
    emptyDigest,
    emptyGeneration,
    hashFileV2,
    digestsMatch,
    verifyOutputsExist,
    generatorStamp,
    generationRecord,
    -- Shared orphan-reconcile helpers (used by digest-check and the JSON
    -- write path; see #393 / #405)
    listFilesRecursive,
    pruneEmptyDirs,
    makeRelativeTo,
    reconcileOrphans,
) where

import Hydra.Packaging (Module(..), ModuleName(..))
import qualified Hydra.Build.Walk as GenWalk

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
        files <- GenWalk.filterByExtension "hs" <$> listFilesRecursive srcDir
        Y.catMaybes <$> mapM extractNs files

    -- Scan a package's native (.java/.py) self-host coder sources, which live
    -- under packages/<pkg>/src/main/<lang>/hydra/sources/. hydra-jvm, hydra-java,
    -- and hydra-python currently have these; other packages have no such dir and
    -- yield [].
    scanNativePackage pkg = do
      let javaDir = packagesRoot FP.</> pkg FP.</> "src" FP.</> "main"
                                FP.</> "java" FP.</> "hydra" FP.</> "sources"
          pyDir   = packagesRoot FP.</> pkg FP.</> "src" FP.</> "main"
                                FP.</> "python" FP.</> "hydra" FP.</> "sources"
      javaPairs <- scanNativeDir "java" extractNativeNs javaDir
      pyPairs   <- scanNativeDir "py"   extractNativeNs pyDir
      return $ javaPairs ++ pyPairs

    scanNativeDir ext extract dir = do
      isDir <- SD.doesDirectoryExist dir
      if not isDir then return [] else do
        files <- GenWalk.filterByExtension ext <$> listFilesRecursive dir
        Y.catMaybes <$> mapM extract files

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
    --   * Java:   `ModuleName NS = new ModuleName("hydra.{jvm,java}.<x>")` (#344, #505)
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
-- @:@). Since #512 these entries persist through the typed
-- hydra.build.format codec (Hydra.DigestFormat) as ordinary
-- moduleHashes keys; the prefix remains purely a naming convention.
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
      files <- GenWalk.filterByExtension "json" <$> listFilesRecursive jsonRoot
      pairs <- CM.forM files $ \fp -> do
        result <- E.try (hashFile fp) :: IO (Either E.SomeException String)
        case result of
          Left _  -> return Nothing
          Right h -> do
            let rel = makeRelativeTo jsonRoot fp
                key = ModuleName (jsonContentKeyPrefix ++ rel)
            return $ Just (key, h)
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
-- On-disk form (#512): the canonical JSON encoding of
-- hydra.build.format.InputDigest, written and read via Hydra.DigestFormat
-- (selfHash as an optional field, deps as the dependencyHashes map, module
-- hashes as the moduleHashes map). Pre-#512 legacy files fail the typed
-- decode and degrade to a cache miss; the next regen rewrites them.
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

-- | Provenance class of a generated artifact (#413 / #523).
--
--   * 'ModePublished' — produced by a published, versioned host (the normal
--     post-#370 path). 'genHydraVersion' carries the release version.
--   * 'ModeShim' — produced by a locally-built migration shim (a
--     backward-incompatible change the published host can't yet handle).
--     'genHydraVersion' is omitted (a shim has no release); 'genRevision'
--     is REQUIRED — the working-tree SHA is the shim's only precise identity.
--
-- The discriminator lets a debugger answer "did this come from a known
-- release, or an unreleased local build?" and drives the #415 skew
-- exemption: a 'ModeShim' artifact is legitimately ahead of the last
-- published host's moduleFormatVersion.
data GenerationMode = ModePublished | ModeShim
    deriving (Eq, Ord, Show, Read)

-- | Structured generation-provenance record (#413 / #523), replacing the
-- flat generator stamp. Separates IDENTITY (gating) from PROVENANCE
-- (informational).
--
-- INVARIANT (load-bearing): only 'genGeneratorId' gates freshness — it is
-- the sole field consulted by 'digestsMatch' (via 'digestGenerator', which
-- holds the same value). It is host-INDEPENDENT by design: self-hosting
-- requires every host to produce byte-identical output, so the artifact's
-- identity cannot depend on 'genHost'. Every OTHER field here is purely
-- informational and MUST NOT gate: 'genTimestamp' varies across
-- byte-identical rebuilds and 'genHost' varies across hosts that legitimately
-- produce identical output, so gating on either would punish self-hosting.
-- A second invariant ties the modes honest: @'genMode' == 'ModeShim'@ ⇒
-- 'genRevision' is present (non-empty).
--
-- Field names match #413's table verbatim so a later promotion to a Hydra
-- DSL type under @hydra.build.*@ (#416) is a mechanical lift.
--
-- 'genRevision' format: @<short-sha>@, with @-dirty@ appended when the
-- worktree has uncommitted changes (e.g. @a6d4f26@ / @a6d4f26-dirty@).
-- 'genTimestamp' format: ISO-8601 UTC.
data Generation = Generation
    { genGeneratorId  :: String            -- GATING: the cache key; = 'digestGenerator'
    , genMode         :: GenerationMode    -- informational: published | shim
    , genHost         :: String            -- informational: producing host ("haskell"|"java"|…)
    , genHydraVersion :: Maybe String      -- informational: release version; omitted for shim
    , genRevision     :: Maybe String      -- informational: <short-sha>[-dirty]; REQUIRED for shim
    , genTimestamp    :: Maybe String       -- informational: ISO-8601 UTC; NON-deterministic
    } deriving (Eq, Show)

emptyGeneration :: Generation
emptyGeneration = Generation "" ModePublished "" Nothing Nothing Nothing

-- | A versioned digest for one sync step. Indexed by file path so that
-- callers can mix file types freely.
data Digest = Digest
    { digestInputs    :: M.Map FilePath DigestEntry
    , digestOutputs   :: M.Map FilePath DigestEntry
    , digestGenerator :: String  -- generator stamp (= 'genGeneratorId'); see 'generatorStamp'.
                                  -- This is the SOLE gating identity field (see 'digestsMatch').
    , digestGeneration :: Generation  -- informational provenance (#413/#523); MUST NOT gate
    -- #347 transitive-invalidation fields, recorded at refresh time and
    -- compared at freshness-check time alongside per-namespace inputs:
    , digestRecordedSelfHash :: String  -- input package's selfHash
    , digestRecordedDeps     :: M.Map String String  -- depPkg → selfHash
    } deriving (Eq, Show)

emptyDigest :: Digest
emptyDigest = Digest M.empty M.empty "" emptyGeneration "" M.empty

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

-- | Gather the full 'Generation' provenance record from the environment
-- (#413 / #523). Extends the 'HYDRA_GENERATOR_STAMP' handshake: the shell
-- assembler exports the informational fields alongside the gating stamp:
--
--   HYDRA_GENERATOR_STAMP          → 'genGeneratorId' (also 'digestGenerator')
--   HYDRA_GENERATION_MODE          → 'genMode'         ("published" | "shim")
--   HYDRA_GENERATION_HOST          → 'genHost'
--   HYDRA_GENERATION_HYDRA_VERSION → 'genHydraVersion' (omitted when empty)
--   HYDRA_GENERATION_REVISION      → 'genRevision'     (omitted when empty)
--   HYDRA_GENERATION_TIMESTAMP     → 'genTimestamp'    (omitted when empty)
--
-- Fallbacks keep the legacy/unstamped path honest: unset MODE defaults to
-- 'ModePublished'. The invariant @shim ⇒ revision present@ is enforced here:
-- a 'ModeShim' record with no revision fails loudly rather than writing a
-- dishonest artifact.
generationRecord :: IO Generation
generationRecord = do
    gid  <- generatorStamp
    -- Unset MODE defaults to published; any set value goes through the shared
    -- 'stringToMode' (so "shim" ⇒ ModeShim, everything else ⇒ ModePublished).
    mode <- fmap (maybe ModePublished stringToMode) (lookupEnv "HYDRA_GENERATION_MODE")
    host <- fmap (Y.fromMaybe "") (lookupEnv "HYDRA_GENERATION_HOST")
    ver  <- fmap nonEmpty (lookupEnv "HYDRA_GENERATION_HYDRA_VERSION")
    rev  <- fmap nonEmpty (lookupEnv "HYDRA_GENERATION_REVISION")
    ts   <- fmap nonEmpty (lookupEnv "HYDRA_GENERATION_TIMESTAMP")
    CM.when (mode == ModeShim && Y.isNothing rev) $
      error "generationRecord: mode=shim requires HYDRA_GENERATION_REVISION (invariant: shim ⇒ revision present)"
    return Generation
      { genGeneratorId  = gid
      , genMode         = mode
      , genHost         = host
      , genHydraVersion = ver
      , genRevision     = rev
      , genTimestamp    = ts
      }
  where
    lookupEnv k = do
      r <- E.try (SE.getEnv k) :: IO (Either E.SomeException String)
      return $ case r of
        Right s -> Just s
        Left _  -> Nothing
    nonEmpty (Just s) | not (null s) = Just s
    nonEmpty _                       = Nothing

-- | Render a 'GenerationMode' to its wire string.
stringToMode :: String -> GenerationMode
stringToMode "shim" = ModeShim
stringToMode _      = ModePublished

-- | Two digests are equivalent for freshness purposes if their input,
-- output, and generator fields all match. Output hashes are NOT
-- compared against the filesystem here — see 'verifyOutputsExist'
-- for that.
--
-- INVARIANT: this is an explicit ALLOWLIST of the fields that gate
-- freshness. It must never become a whole-'Digest' equality (@a == b@) or
-- a hash of the serialized digest file. The 'digestGeneration' record's
-- informational fields ('genMode', 'genHost', 'genHydraVersion',
-- 'genRevision', 'genTimestamp') are DELIBERATELY absent below — only its
-- 'genGeneratorId' gates, and that value already lives in 'digestGenerator'
-- (compared here). Gating on any informational field would be wrong: a
-- timestamp varies across byte-identical rebuilds, and 'genHost' varies
-- across hosts that — by the self-hosting contract — produce identical
-- output, so gating on either causes spurious cache misses and punishes
-- self-hosting. A field gates iff it appears below; add a field here only
-- if it is deterministic AND content-determining.
digestsMatch :: Digest -> Digest -> Bool
digestsMatch a b =
    digestInputs a == digestInputs b
      && digestOutputs a == digestOutputs b
      && digestGenerator a == digestGenerator b  -- gating id (= genGeneratorId)
      && digestRecordedSelfHash a == digestRecordedSelfHash b
      && digestRecordedDeps a == digestRecordedDeps b
      -- NOTE: digestGeneration is intentionally NOT compared (informational).

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
          -- protectRel entries are package-relative names (e.g. "manifest.json",
          -- "languages.json"). Match on the basename too, so protection holds even
          -- when makeRelativeTo cannot strip the prefix (outputDir relative but the
          -- listed path absolute), which would otherwise leave rel as a full path
          -- that never matches a protect entry (#416: languages.json was pruned).
          , not (S.member rel protectRel)
          , not (S.member (FP.takeFileName rel) protectRel)
          ]
    CM.forM_ orphans $ \p ->
      SD.removeFile p `E.catch` \(_ :: E.IOException) -> return ()
    CM.unless (null orphans) $ pruneEmptyDirs outputDir
    return orphans
