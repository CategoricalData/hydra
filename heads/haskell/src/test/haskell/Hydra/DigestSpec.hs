-- | Regression tests for the per-package input digest: source-namespace
-- discovery (#400), per-package digest finalization (#606), and the
-- per-module Merkle-over-dependency-closure freshness key (#701).
--
-- #400: the digest that gates regeneration of dist/haskell/hydra-{java,python}
-- must cover the native-generator-owned hydra.<lang>.* modules (coder,
-- environment, language, names, serde, syntax, testing, utils), whose
-- canonical sources are authored natively in .java/.py (#344). Before #400,
-- 'discoverModuleNameFiles' scanned only the Haskell DSL tree, so editing e.g.
-- Coder.java never invalidated the digest and the freshness gate silently
-- skipped regeneration.
--
-- #701: a module's freshness key must be a Merkle hash over its transitive
-- dependency closure, not its own content hash alone -- and the closure must
-- be derived from actual term references, not just the (routinely
-- incomplete) hand-declared 'moduleDependencies' list. See
-- 'moduleReferenceEdges', 'closeDirtySet', and 'Digest.computeMerkleHashes'.
--
-- These tests run from the Haskell head's working directory (heads/haskell),
-- which is where 'discoverModuleNameFiles' resolves its relative packagesRoot
-- (../../packages) — the same cwd stack uses for 'stack test'.

module Hydra.DigestSpec where

import qualified Hydra.Digest as Digest
import qualified Hydra.DigestFormat as DigestFormat
import Hydra.Digest
  ( Digest(..), DigestEntry(..), DigestKind(..)
  , Generation(..), GenerationMode(..)
  , PerPackageDigest(..) )
import Hydra.Generation
  ( ensurePerPackageDigests, finalizePerPackageDigests, perPackageDigestPath
  , closeDirtySet, moduleReferenceEdges )
import Hydra.Packaging (Module(..), ModuleName(..), ModuleDependency(..), Definition(..), TermDefinition(..))
import Hydra.PackageRouting (buildRoutingMap)
import Hydra.Core (Term(..), Application(..), Name(..))

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified System.Directory as SD
import System.FilePath ((</>), takeDirectory)
import qualified Test.Hspec as H


-- | A name-only Module. 'hashUniverse' reads only 'moduleName', so the other
-- fields are irrelevant for digest computation.
nameOnly :: String -> Module
nameOnly ns = Module (ModuleName ns) Nothing [] []

-- | A Module with declared dependencies but no term bodies (for exercising
-- the declared-'moduleDependencies' half of 'moduleReferenceEdges').
withDeclaredDeps :: String -> [String] -> Module
withDeclaredDeps ns deps =
  Module (ModuleName ns) Nothing [ModuleDependency (ModuleName d) Nothing | d <- deps] []

-- | A Module with ONE term definition whose body is a bare reference
-- (TermVariable) to each of the given fully-qualified names, and NO declared
-- 'moduleDependencies' -- the #701 reproduction shape: a module that USES
-- another module's definition (e.g. a primitive, via a term-level
-- reference) without DECLARING it as a dependency.
withUndeclaredTermRef :: String -> String -> [String] -> Module
withUndeclaredTermRef ns defName refs =
  Module (ModuleName ns) Nothing []
    [DefinitionTerm (TermDefinition (Name defName) Nothing Nothing body)]
  where
    -- A single reference for one ref, or a nested application chain so
    -- every name in 'refs' appears somewhere in the term tree.
    body = case [TermVariable (Name r) | r <- refs] of
      []     -> TermVariable (Name defName)  -- degenerate: self-reference only
      [t]    -> t
      (t:ts) -> foldl (\acc t' -> TermApplication (Application acc t')) t ts


spec :: H.Spec
spec = do
  nsFiles <- H.runIO Digest.discoverModuleNameFiles

  H.describe "discoverModuleNameFiles covers native (.java/.py) coder sources (#400)" $ do

    -- The one native-owned JVM common module (#505). Must be discovered so
    -- edits to Serde.java invalidate the hydra-jvm per-package digest.
    let jvmModules =
          [ "hydra.jvm.serde" ]
        -- The eight native-owned Java coder modules. All must be discovered, or
        -- a change to that module's .java source would not invalidate the
        -- hydra-java per-package digest.
        javaModules =
          [ "hydra.java.coder", "hydra.java.environment", "hydra.java.language"
          , "hydra.java.names", "hydra.java.serde", "hydra.java.syntax"
          , "hydra.java.testing", "hydra.java.utils" ]
        -- The seven native-owned Python coder modules.
        pythonModules =
          [ "hydra.python.coder", "hydra.python.environment", "hydra.python.language"
          , "hydra.python.names", "hydra.python.serde", "hydra.python.syntax"
          , "hydra.python.testing", "hydra.python.utils" ]

    H.it "finds every hydra.jvm.* native module" $
      mapM_ (\ns -> M.member (ModuleName ns) nsFiles `H.shouldBe` True) jvmModules

    H.it "finds every hydra.java.* native module" $
      mapM_ (\ns -> M.member (ModuleName ns) nsFiles `H.shouldBe` True) javaModules

    H.it "finds every hydra.python.* native module" $
      mapM_ (\ns -> M.member (ModuleName ns) nsFiles `H.shouldBe` True) pythonModules

    H.it "routes hydra.java.coder to its .java source" $
      case M.lookup (ModuleName "hydra.java.coder") nsFiles of
        Just fp -> (".java" `L.isSuffixOf` fp) `H.shouldBe` True
        Nothing -> H.expectationFailure "hydra.java.coder not discovered"

    H.it "routes hydra.python.coder to its .py source" $
      case M.lookup (ModuleName "hydra.python.coder") nsFiles of
        Just fp -> (".py" `L.isSuffixOf` fp) `H.shouldBe` True
        Nothing -> H.expectationFailure "hydra.python.coder not discovered"

    -- The dependency-reference lines inside the native sources (e.g.
    -- SYNTAX_NS / CORE_NS in Java, LEXICAL_NS in Python) must NOT be mistaken
    -- for a file's own namespace. hydra.lexical is referenced by language.py's
    -- LEXICAL_NS but is defined elsewhere (and is not a hydra.python.* module),
    -- so discovery must not map it to a native .py file.
    H.it "does not misattribute a dependency reference as a native namespace" $
      case M.lookup (ModuleName "hydra.lexical") nsFiles of
        Just fp -> (".py" `L.isSuffixOf` fp || ".java" `L.isSuffixOf` fp)
                     `H.shouldBe` False
        Nothing -> return ()  -- absent is fine; it just must not be a native file

  H.describe "hashUniverse folds native modules into the digest (#400)" $ do

    H.it "produces a hash entry for hydra.java.coder" $ do
      d <- Digest.hashUniverse nsFiles [nameOnly "hydra.java.coder"]
      M.member (ModuleName "hydra.java.coder") d `H.shouldBe` True

    H.it "produces a hash entry for hydra.python.coder" $ do
      d <- Digest.hashUniverse nsFiles [nameOnly "hydra.python.coder"]
      M.member (ModuleName "hydra.python.coder") d `H.shouldBe` True

  H.describe "structured generation provenance record (#413/#523)" $ do

    -- A published-mode digest with a full generation record.
    let publishedGen = Generation
          { genGeneratorId  = "gid-abc123"
          , genMode         = ModePublished
          , genHost         = "haskell"
          , genHydraVersion = Just "0.17.0"
          , genRevision     = Just "a6d4f26"
          , genTimestamp    = Just "2026-07-03T14:00:00Z" }
        shimGen = Generation
          { genGeneratorId  = "gid-shimhash"
          , genMode         = ModeShim
          , genHost         = "java"
          , genHydraVersion = Nothing              -- omitted for shim
          , genRevision     = Just "a6d4f26-dirty" -- required for shim
          , genTimestamp    = Just "2026-07-03T14:05:00Z" }
        withGen g = (Digest.emptyDigest)
          { digestInputs     = M.fromList [("in/a.hs", DigestEntry KindDslSource "h1")]
          , digestOutputs    = M.fromList [("out/A.hs", DigestEntry KindTargetFile "h2")]
          , digestGenerator  = genGeneratorId g
          , digestGeneration = g }

    -- #512: the on-disk codec is the typed hydra.build.format encoding
    -- (DigestFormat); a round-trip goes value → canonical JSON string →
    -- parsed value. The legacy hand-rolled parse/serialize is gone; legacy
    -- files simply fail the typed decode and cache-miss.
    let typedRoundTrip d =
          either error id $ do
            s <- DigestFormat.outputDigestToJsonString DigestFormat.defaultFormatContext (DigestFormat.fromDigestV2 d)
            v <- DigestFormat.parseJsonString s
            DigestFormat.toDigestV2 <$> DigestFormat.outputDigestFromJsonString DigestFormat.defaultFormatContext v

    H.it "round-trips a published-mode generation record through the typed codec" $ do
      let d = withGen publishedGen
          d' = typedRoundTrip d
      digestGeneration d' `H.shouldBe` publishedGen

    H.it "round-trips a shim-mode record (hydraVersion omitted, revision present)" $ do
      let d = withGen shimGen
          d' = typedRoundTrip d
      digestGeneration d' `H.shouldBe` shimGen

    -- (Legacy-format tolerance tests retired with the legacy parser, #512:
    -- a pre-#512 on-disk digest fails the typed decode and degrades to a
    -- cache miss — the digest contract for any unreadable state.)

    -- The gating contract survives the codec change: two digests with the
    -- same gating id but different INFORMATIONAL provenance compare equal
    -- under digestsMatch (which gates on generatorId only).
    H.it "digestsMatch ignores informational provenance differences with same gating id" $ do
      let a = withGen publishedGen
          b = a
            { digestGeneration = (digestGeneration a)
                { genMode = ModeShim, genHost = "java"
                , genRevision = Just "deadbee-dirty", genTimestamp = Just "2026-07-03T00:00:00Z" } }
      Digest.digestsMatch a b `H.shouldBe` True

    -- digestsMatch must still MISS when the gating id differs.
    H.it "digestsMatch misses when the gating generatorId differs" $ do
      let a = withGen publishedGen
          b = a { digestGenerator = "gid-different", digestGeneration = publishedGen { genGeneratorId = "gid-different" } }
      Digest.digestsMatch a b `H.shouldBe` False

    -- The flat "generator" field survives in the TYPED schema (an explicit
    -- OutputDigest field, canonically always emitted) — the gating id remains
    -- readable without descending into the generation object. String-level
    -- assert, mirroring the old compat test: the encoded JSON carries a
    -- "generator" member distinct from generation.generatorId.
    H.it "typed encoding emits the flat \"generator\" member (gating id at top level)" $ do
      let out = either error id $ DigestFormat.outputDigestToJsonString
            DigestFormat.defaultFormatContext (DigestFormat.fromDigestV2 (withGen publishedGen))
          hasFlatGenerator = any (\ln -> L.isInfixOf "\"generator\"" ln
                                         && not (L.isInfixOf "\"generatorId\"" ln))
                                 (lines out)
      hasFlatGenerator `H.shouldBe` True

    -- Optional-field semantics through the typed codec: absent optionals
    -- (hydraVersion/revision/timestamp all Nothing) are omitted on encode and
    -- decode back to Nothing. (The legacy mode-defaults-to-published rule
    -- retired with the legacy parser; mode is a required field of the typed
    -- schema.)
    H.it "absent optionals round-trip as Nothing through the typed codec" $ do
      let bare = publishedGen { genHydraVersion = Nothing, genRevision = Nothing, genTimestamp = Nothing }
          d' = typedRoundTrip (withGen bare)
          g = digestGeneration d'
      genHydraVersion g `H.shouldBe` Nothing
      genRevision g `H.shouldBe` Nothing
      genTimestamp g `H.shouldBe` Nothing
      genMode g `H.shouldBe` ModePublished

  -- #606: a native-driver JSON change (e.g. #398's coder-runtime field
  -- reorder, which touches no .java/.py SOURCE) updates a native-owned
  -- package's jsonContent: hashes WITHOUT changing hashUniverse's
  -- universe-wide hash (hashUniverse only reads DSL SOURCE, never native
  -- driver JSON OUTPUT -- see hashPackageJsonContent's #398/#469 doc
  -- comment above). So a subsequent Haskell-side sync sees a cache HIT and
  -- takes writeModulesJsonPackageSplit's cache-hit branch:
  -- 'ensurePerPackageDigests' correctly detects the jsonContent mismatch
  -- (stored /= recomputed) and rewrites the package's digest -- but via the
  -- v1 'Digest.writeDigest', which silently drops selfHash/depHash:* (parse/
  -- serializeDigest never round-trip those keys). Before the #606 fix,
  -- NOTHING downstream ever repaired the dropped fields on this path, so
  -- Phase 3's digest-check could compare two independently-stale-but-equal
  -- selfHash/depHash pairs and report a false cache hit. This is latent, not
  -- live, for DSL-authored packages like hydra-build: their JSON content is
  -- driven entirely by the SAME DSL-source hash that gates the universe-wide
  -- check, so a real content change there always forces a miss instead
  -- (confirmed by a live repro, see the #606 branch plan / issue).
  --
  -- The fix: writeModulesJsonPackageSplit's cache-hit branch now calls
  -- 'finalizePerPackageDigests' right after 'ensurePerPackageDigests', so
  -- whatever the v1 writer just dropped gets correctly recomputed in the
  -- same pass. These tests exercise 'ensurePerPackageDigests' directly (the
  -- actual buggy function), not just 'finalizePerPackageDigests' in
  -- isolation, so the trigger condition is encoded precisely.
  H.describe "ensurePerPackageDigests + finalizePerPackageDigests (#606)" $ do
    H.it "ensurePerPackageDigests alone drops selfHash/depHash on a native-package content change (pre-#606-fix behavior)" $ do
      tmpRoot <- (</> "hydra-606-spec-bare") <$> SD.getTemporaryDirectory
      SD.removePathForcibly tmpRoot
      SD.createDirectoryIfMissing True tmpRoot

      -- hydra-java's real declared deps (packages/hydra-java/package.json):
      -- hydra-kernel + hydra-jvm. Seed both with correct, already-finalized
      -- digests (as a real prior sync would have left them).
      let writeFinalizedMap pkg hmap deps = do
            let dpath = perPackageDigestPath tmpRoot pkg
                selfH = Digest.computeSelfHash hmap
            SD.createDirectoryIfMissing True (takeDirectory dpath)
            DigestFormat.writePerPackageDigestFile dpath (PerPackageDigest hmap selfH (M.fromList deps))
          writeFinalized pkg hashes deps =
            writeFinalizedMap pkg (M.fromList [(ModuleName k, v) | (k, v) <- hashes]) deps
          -- The REAL native-driver JSON file 'hashPackageJsonContent' reads
          -- from disk -- writing THIS (not just a digest key) is what makes
          -- the jsonContent-only-change trigger faithful.
          javaJsonPath = tmpRoot </> "hydra-java" </> "src" </> "main" </> "json" </> "hydra" </> "java" </> "coder.json"

      writeFinalized "hydra-kernel" [("hydra.core", "kernel-hash-1")] []
      writeFinalized "hydra-jvm"    [("hydra.jvm.serde", "jvm-hash-1")] []
      kernelBefore <- DigestFormat.readPerPackageDigestFile (perPackageDigestPath tmpRoot "hydra-kernel")
      jvmBefore <- DigestFormat.readPerPackageDigestFile (perPackageDigestPath tmpRoot "hydra-jvm")
      SD.createDirectoryIfMissing True (takeDirectory javaJsonPath)
      writeFile javaJsonPath "{\"v\": \"old\"}"
      oldJsonDigest <- Digest.hashPackageJsonContent tmpRoot "hydra-java"
      writeFinalizedMap "hydra-java" oldJsonDigest
        [("hydra-kernel", Digest.ppSelfHash kernelBefore), ("hydra-jvm", Digest.ppSelfHash jvmBefore)]

      -- The native-driver regen: rewrite the JSON file's bytes with no
      -- accompanying .java source edit.
      writeFile javaJsonPath "{\"v\": \"new\"}"

      -- 'universeMods': a single synthetic hydra.java.coder module, routed
      -- to hydra-java via a real RoutingMap. hashUniverse resolves its DSL
      -- source file via 'Digest.discoverModuleNameFiles' (reads the REAL
      -- packages/hydra-java/.../coder.java on disk, since these tests run
      -- from heads/haskell's cwd) -- unchanged, so the SOURCE-side hash
      -- matches. The jsonContent file is the only thing that changed
      -- (simulating a native-driver JSON regen with no .java source edit).
      let routingMap = buildRoutingMap [("hydra-java", [ModuleName "hydra.java.coder"])]
          universeMods = [nameOnly "hydra.java.coder"]
      ensurePerPackageDigests routingMap tmpRoot universeMods

      after <- DigestFormat.readPerPackageDigestFile (perPackageDigestPath tmpRoot "hydra-java")
      newJsonDigest <- Digest.hashPackageJsonContent tmpRoot "hydra-java"
      let jsonKey = ModuleName "jsonContent:hydra/java/coder.json"
      -- Confirm the mismatch really was a jsonContent-only change, not some
      -- other accident: the recorded hash for the JSON file moved from old
      -- to new content (the source-side hydra.java.coder entry is also
      -- present but unaffected -- not asserted on here).
      M.lookup jsonKey (Digest.ppHashes after) `H.shouldBe` M.lookup jsonKey newJsonDigest
      M.lookup jsonKey (Digest.ppHashes after) `H.shouldNotBe` M.lookup jsonKey oldJsonDigest
      -- ... but selfHash/depHash were dropped by the v1 writer underneath
      -- ensurePerPackageDigests -- this is the bug, pinned so a future
      -- regression in ensurePerPackageDigests's own writer is caught even if
      -- the surrounding finalize-on-every-path fix is ever removed.
      Digest.ppSelfHash after `H.shouldBe` ""
      Digest.ppDeps after `H.shouldBe` M.empty

      SD.removePathForcibly tmpRoot

    H.it "the #606 fix (ensurePerPackageDigests + finalizePerPackageDigests) preserves selfHash/depHash across the same trigger" $ do
      tmpRoot <- (</> "hydra-606-spec-fixed") <$> SD.getTemporaryDirectory
      SD.removePathForcibly tmpRoot
      SD.createDirectoryIfMissing True tmpRoot

      let writeFinalizedMap pkg hmap deps = do
            let dpath = perPackageDigestPath tmpRoot pkg
                selfH = Digest.computeSelfHash hmap
            SD.createDirectoryIfMissing True (takeDirectory dpath)
            DigestFormat.writePerPackageDigestFile dpath (PerPackageDigest hmap selfH (M.fromList deps))
          writeFinalized pkg hashes deps =
            writeFinalizedMap pkg (M.fromList [(ModuleName k, v) | (k, v) <- hashes]) deps
          -- The REAL native-driver JSON file that hashPackageJsonContent
          -- reads from disk (dist/json/hydra-java/src/main/json/hydra/
          -- java/coder.json). Writing this -- not just a digest key -- is
          -- what makes the jsonContent-only-change trigger faithful: a
          -- real native-driver regen changes THIS file's bytes with no
          -- accompanying .java source edit.
          javaJsonPath = tmpRoot </> "hydra-java" </> "src" </> "main" </> "json" </> "hydra" </> "java" </> "coder.json"

      writeFinalized "hydra-kernel" [("hydra.core", "kernel-hash-2")] []
      writeFinalized "hydra-jvm"    [("hydra.jvm.serde", "jvm-hash-2")] []
      kernelBefore <- DigestFormat.readPerPackageDigestFile (perPackageDigestPath tmpRoot "hydra-kernel")
      jvmBefore <- DigestFormat.readPerPackageDigestFile (perPackageDigestPath tmpRoot "hydra-jvm")
      SD.createDirectoryIfMissing True (takeDirectory javaJsonPath)
      writeFile javaJsonPath "{\"v\": \"old\"}"
      oldJsonDigest <- Digest.hashPackageJsonContent tmpRoot "hydra-java"
      writeFinalizedMap "hydra-java" oldJsonDigest
        [("hydra-kernel", Digest.ppSelfHash kernelBefore), ("hydra-jvm", Digest.ppSelfHash jvmBefore)]

      -- The native-driver regen: rewrite the JSON file's bytes with no
      -- accompanying .java source edit.
      writeFile javaJsonPath "{\"v\": \"new\"}"

      let routingMap = buildRoutingMap [("hydra-java", [ModuleName "hydra.java.coder"])]
          universeMods = [nameOnly "hydra.java.coder"]
      -- Exactly the sequence writeModulesJsonPackageSplit's cache-hit branch
      -- now runs (Generation.hs, #606 fix).
      ensurePerPackageDigests routingMap tmpRoot universeMods
      finalizePerPackageDigests tmpRoot

      after <- DigestFormat.readPerPackageDigestFile (perPackageDigestPath tmpRoot "hydra-java")
      kernelAfter <- DigestFormat.readPerPackageDigestFile (perPackageDigestPath tmpRoot "hydra-kernel")
      jvmAfter <- DigestFormat.readPerPackageDigestFile (perPackageDigestPath tmpRoot "hydra-jvm")
      newJsonDigest <- Digest.hashPackageJsonContent tmpRoot "hydra-java"

      -- selfHash is populated and correctly derived from the (updated)
      -- jsonContent hash -- the mismatch was still detected and written.
      let jsonKey = ModuleName "jsonContent:hydra/java/coder.json"
      Digest.ppSelfHash after `H.shouldNotBe` ""
      Digest.ppSelfHash after `H.shouldBe` Digest.computeSelfHash (Digest.ppHashes after)
      M.lookup jsonKey (Digest.ppHashes after) `H.shouldBe` M.lookup jsonKey newJsonDigest
      M.lookup jsonKey (Digest.ppHashes after) `H.shouldNotBe` M.lookup jsonKey oldJsonDigest
      -- depHash:hydra-kernel / depHash:hydra-jvm correctly carry those
      -- packages' selfHashes -- the transitive-invalidation edge that a
      -- dropped depHash would otherwise silently break, letting Phase 3
      -- compare two stale-but-equal digests and report a false cache hit.
      M.lookup "hydra-kernel" (Digest.ppDeps after) `H.shouldBe` Just (Digest.ppSelfHash kernelAfter)
      M.lookup "hydra-jvm" (Digest.ppDeps after) `H.shouldBe` Just (Digest.ppSelfHash jvmAfter)

      SD.removePathForcibly tmpRoot

    H.it "is idempotent: a second run is a no-op over an already-finalized tree" $ do
      tmpRoot <- (</> "hydra-606-spec-idempotent") <$> SD.getTemporaryDirectory
      SD.removePathForcibly tmpRoot
      SD.createDirectoryIfMissing True tmpRoot
      let writeHashesOnly pkg hashes = do
            let dpath = perPackageDigestPath tmpRoot pkg
            SD.createDirectoryIfMissing True (takeDirectory dpath)
            DigestFormat.writeDigestMapFile dpath (M.fromList [(ModuleName k, v) | (k, v) <- hashes])
      writeHashesOnly "hydra-kernel" [("hydra.core", "kernel-hash-2")]
      writeHashesOnly "hydra-jvm"    [("hydra.jvm.serde", "jvm-hash-2")]
      writeHashesOnly "hydra-java"   [("jsonContent:hydra/java/coder.json", "java-json-hash-2")]

      finalizePerPackageDigests tmpRoot
      once <- DigestFormat.readPerPackageDigestFile (perPackageDigestPath tmpRoot "hydra-java")
      finalizePerPackageDigests tmpRoot
      twice <- DigestFormat.readPerPackageDigestFile (perPackageDigestPath tmpRoot "hydra-java")

      twice `H.shouldBe` once
      SD.removePathForcibly tmpRoot

  -- #701: the intra-package module-freshness cache must be a Merkle hash
  -- over its dependency closure, not a module's own content hash alone.
  -- These tests reproduce the exact mechanism behind the setOf/encodeMap
  -- staleness from the 0.17 breaking batch (#508): a module (like
  -- hydra.extract.core) that references another module's definition (like
  -- hydra.lib.eithers.mapSet) via a bare term-level reference, WITHOUT
  -- declaring that module in 'moduleDependencies'.
  H.describe "moduleReferenceEdges derives edges term references miss from declared deps (#701)" $ do

    H.it "captures a declared moduleDependencies edge" $ do
      let mods = [withDeclaredDeps "pkg.a" ["pkg.b"], nameOnly "pkg.b"]
          edges = moduleReferenceEdges mods
      M.lookup (ModuleName "pkg.a") edges `H.shouldBe` Just (S.singleton (ModuleName "pkg.b"))

    H.it "captures an UNDECLARED edge reached only via a term-level reference (the setOf/mapSet shape)" $ do
      -- hydra.extract.core's setOf calls Eithers.mapSet (hydra.lib.eithers.mapSet)
      -- but never lists hydra.lib.eithers in moduleDependencies.
      let extractCore = withUndeclaredTermRef "hydra.extract.core" "setOf" ["hydra.lib.eithers.mapSet"]
          libEithers = nameOnly "hydra.lib.eithers"
          mods = [extractCore, libEithers]
          edges = moduleReferenceEdges mods
      -- The declared side is empty (reproducing the real gap)...
      moduleDependencies extractCore `H.shouldBe` []
      -- ...but the DERIVED edge set still finds it, because the term body
      -- references hydra.lib.eithers.mapSet and moduleNameOf resolves that
      -- to hydra.lib.eithers.
      M.lookup (ModuleName "hydra.extract.core") edges `H.shouldBe` Just (S.singleton (ModuleName "hydra.lib.eithers"))

    H.it "unions declared and derived edges when both are present" $ do
      let m = (withDeclaredDeps "pkg.a" ["pkg.b"])
                { moduleDefinitions =
                    [DefinitionTerm (TermDefinition (Name "f") Nothing Nothing (TermVariable (Name "pkg.c.g")))] }
          mods = [m, nameOnly "pkg.b", nameOnly "pkg.c"]
          edges = moduleReferenceEdges mods
      M.lookup (ModuleName "pkg.a") edges `H.shouldBe`
        Just (S.fromList [ModuleName "pkg.b", ModuleName "pkg.c"])

    H.it "does not self-reference (a module's own definitions don't create a self-edge)" $ do
      let m = withUndeclaredTermRef "pkg.a" "f" ["pkg.a.g"]
          edges = moduleReferenceEdges [m]
      M.lookup (ModuleName "pkg.a") edges `H.shouldBe` Just S.empty

  H.describe "closeDirtySet closes over derived (not just declared) edges (#701)" $ do

    H.it "marks a module dirty when its UNDECLARED term-level dependency is dirty (setOf/mapSet repro)" $ do
      let extractCore = withUndeclaredTermRef "hydra.extract.core" "setOf" ["hydra.lib.eithers.mapSet"]
          libEithers = nameOnly "hydra.lib.eithers"
          mods = [extractCore, libEithers]
          -- hydra.lib.eithers changed (e.g. mapSet gained an 'ordering'
          -- constraint); nothing about hydra.extract.core's OWN source
          -- changed.
          initialDirty = S.singleton (ModuleName "hydra.lib.eithers")
          closure = closeDirtySet mods initialDirty
      S.member (ModuleName "hydra.extract.core") closure `H.shouldBe` True

    H.it "does not mark an unrelated module dirty" $ do
      let extractCore = withUndeclaredTermRef "hydra.extract.core" "setOf" ["hydra.lib.eithers.mapSet"]
          libEithers = nameOnly "hydra.lib.eithers"
          unrelated = nameOnly "hydra.unrelated"
          mods = [extractCore, libEithers, unrelated]
          closure = closeDirtySet mods (S.singleton (ModuleName "hydra.lib.eithers"))
      S.member (ModuleName "hydra.unrelated") closure `H.shouldBe` False

  H.describe "computeMerkleHashes folds dependency hashes into each module's stored key (#701)" $ do

    H.it "a dependent's Merkle hash changes when its dependency's own-hash changes, even though the dependent's own-hash is unchanged" $ do
      let edges = M.fromList [(ModuleName "a", S.singleton (ModuleName "b")), (ModuleName "b", S.empty)]
          sccs = [[ModuleName "b"], [ModuleName "a"]]  -- topological: b before a
          ownV1 = M.fromList [(ModuleName "a", "a-content"), (ModuleName "b", "b-content-v1")]
          ownV2 = M.fromList [(ModuleName "a", "a-content"), (ModuleName "b", "b-content-v2")]
          merkleV1 = Digest.computeMerkleHashes ownV1 edges sccs
          merkleV2 = Digest.computeMerkleHashes ownV2 edges sccs
      -- 'a's own content hash is IDENTICAL between v1 and v2 (the exact
      -- #701 scenario: a's DSL source never changed) -- but its recorded
      -- Merkle hash must still differ, because its dependency 'b' changed.
      M.lookup (ModuleName "a") merkleV1 `H.shouldNotBe` M.lookup (ModuleName "a") merkleV2

    H.it "a module's Merkle hash is unaffected by an unrelated module's change" $ do
      let edges = M.fromList [(ModuleName "a", S.singleton (ModuleName "b")), (ModuleName "b", S.empty), (ModuleName "c", S.empty)]
          sccs = [[ModuleName "b"], [ModuleName "c"], [ModuleName "a"]]
          ownV1 = M.fromList [(ModuleName "a", "a-content"), (ModuleName "b", "b-content"), (ModuleName "c", "c-content-v1")]
          ownV2 = M.fromList [(ModuleName "a", "a-content"), (ModuleName "b", "b-content"), (ModuleName "c", "c-content-v2")]
          merkleV1 = Digest.computeMerkleHashes ownV1 edges sccs
          merkleV2 = Digest.computeMerkleHashes ownV2 edges sccs
      M.lookup (ModuleName "a") merkleV1 `H.shouldBe` M.lookup (ModuleName "a") merkleV2

    H.it "a cyclic (mutually-recursive) SCC gets one shared hash, invalidated by any member" $ do
      let edges = M.fromList
            [ (ModuleName "a", S.singleton (ModuleName "b"))
            , (ModuleName "b", S.singleton (ModuleName "a")) ]
          sccs = [[ModuleName "a", ModuleName "b"]]  -- one SCC, both members
          ownV1 = M.fromList [(ModuleName "a", "a-v1"), (ModuleName "b", "b-content")]
          ownV2 = M.fromList [(ModuleName "a", "a-v2"), (ModuleName "b", "b-content")]
          merkleV1 = Digest.computeMerkleHashes ownV1 edges sccs
          merkleV2 = Digest.computeMerkleHashes ownV2 edges sccs
      -- Both members share the same hash within one computation...
      M.lookup (ModuleName "a") merkleV1 `H.shouldBe` M.lookup (ModuleName "b") merkleV1
      -- ...and changing EITHER member (here 'a', while 'b' is unchanged)
      -- perturbs BOTH members' recorded hash, since they're one SCC.
      M.lookup (ModuleName "b") merkleV1 `H.shouldNotBe` M.lookup (ModuleName "b") merkleV2

    H.it "is deterministic: recomputing over the same inputs yields the same hashes" $ do
      let edges = M.fromList [(ModuleName "a", S.singleton (ModuleName "b")), (ModuleName "b", S.empty)]
          sccs = [[ModuleName "b"], [ModuleName "a"]]
          own = M.fromList [(ModuleName "a", "a-content"), (ModuleName "b", "b-content")]
      Digest.computeMerkleHashes own edges sccs `H.shouldBe` Digest.computeMerkleHashes own edges sccs

    H.it "a module absent from the SCC list keeps its own-content hash unchanged (totality over ownHashes)" $ do
      let own = M.fromList [(ModuleName "orphan", "orphan-content")]
      Digest.computeMerkleHashes own M.empty [] `H.shouldBe` own
