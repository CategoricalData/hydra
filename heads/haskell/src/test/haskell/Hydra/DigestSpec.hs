-- | Regression tests for source-namespace discovery used by the per-package
-- input digest. See #400: the digest that gates regeneration of
-- dist/haskell/hydra-{java,python} must cover the native-generator-owned
-- hydra.<lang>.* modules (coder, environment, language, names, serde, syntax,
-- testing, utils), whose canonical sources are authored natively in .java/.py
-- (#344). Before #400, 'discoverModuleNameFiles' scanned only the Haskell DSL
-- tree, so editing e.g. Coder.java never invalidated the digest and the
-- freshness gate silently skipped regeneration.
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
import Hydra.Generation (ensurePerPackageDigests, finalizePerPackageDigests, perPackageDigestPath)
import Hydra.Packaging (Module(..), ModuleName(..))
import Hydra.PackageRouting (buildRoutingMap)

import qualified Data.List as L
import qualified Data.Map as M
import qualified System.Directory as SD
import System.FilePath ((</>), takeDirectory)
import qualified Test.Hspec as H


-- | A name-only Module. 'hashUniverse' reads only 'moduleName', so the other
-- fields are irrelevant for digest computation.
nameOnly :: String -> Module
nameOnly ns = Module (ModuleName ns) Nothing [] []


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
