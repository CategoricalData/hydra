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
import Hydra.Digest
  ( Digest(..), DigestEntry(..), DigestKind(..)
  , Generation(..), GenerationMode(..) )
import Hydra.Packaging (Module(..), ModuleName(..))

import qualified Data.List as L
import qualified Data.Map as M
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

    H.it "round-trips a published-mode generation record through v2 serialize/parse" $ do
      let d = withGen publishedGen
          d' = Digest.parseDigestV2 (Digest.serializeDigestV2 d)
      digestGeneration d' `H.shouldBe` publishedGen

    H.it "round-trips a shim-mode record (hydraVersion omitted, revision present)" $ do
      let d = withGen shimGen
          d' = Digest.parseDigestV2 (Digest.serializeDigestV2 d)
      digestGeneration d' `H.shouldBe` shimGen

    -- Compat window: a digest written by the PRE-#523 version has only the
    -- flat "generator" string and NO "generation" object. The new parser must
    -- recover the gating id from the flat field so on-disk digests stay green.
    H.it "parses a legacy flat-generator digest (no generation object) — gating id recovered" $ do
      let legacy = unlines
            [ "{"
            , "  \"digestFormatVersion\": 1,"
            , "  \"moduleFormatVersion\": 1,"
            , "  \"generator\": \"legacy-stamp-xyz\","
            , "  \"inputs\": {"
            , "    \"in/a.hs\": { \"kind\": \"DslSource\", \"hash\": \"h1\" }"
            , "  },"
            , "  \"outputs\": {"
            , "    \"out/A.hs\": { \"kind\": \"TargetFile\", \"hash\": \"h2\" }"
            , "  }"
            , "}" ]
          d = Digest.parseDigestV2 legacy
      digestGenerator d `H.shouldBe` "legacy-stamp-xyz"
      genGeneratorId (digestGeneration d) `H.shouldBe` "legacy-stamp-xyz"
      genMode (digestGeneration d) `H.shouldBe` ModePublished  -- default when absent

    -- The compat contract that keeps digest-check green over a previously-written
    -- tree: a legacy flat digest and a new-format digest carrying the SAME gating
    -- id must compare equal under digestsMatch (which gates on generatorId only).
    H.it "digestsMatch treats legacy flat and new-format digests with same gating id as equal" $ do
      let legacy = Digest.parseDigestV2 $ unlines
            [ "{", "  \"generator\": \"same-stamp\","
            , "  \"inputs\": {", "    \"in/a.hs\": { \"kind\": \"DslSource\", \"hash\": \"h1\" }", "  },"
            , "  \"outputs\": {", "    \"out/A.hs\": { \"kind\": \"TargetFile\", \"hash\": \"h2\" }", "  }", "}" ]
          modern = legacy
            { digestGeneration = (digestGeneration legacy)
                { genMode = ModeShim, genHost = "java"
                , genRevision = Just "deadbee-dirty", genTimestamp = Just "2026-07-03T00:00:00Z" } }
      -- Same inputs/outputs/generator; differing ONLY in informational fields.
      Digest.digestsMatch legacy modern `H.shouldBe` True

    -- digestsMatch must still MISS when the gating id differs.
    H.it "digestsMatch misses when the gating generatorId differs" $ do
      let a = withGen publishedGen
          b = a { digestGenerator = "gid-different", digestGeneration = publishedGen { genGeneratorId = "gid-different" } }
      Digest.digestsMatch a b `H.shouldBe` False

    -- (gap a) Precedence: when BOTH a flat "generator" and a "generation" object
    -- with a DIFFERENT generatorId are present, the STRUCTURED id must win. This
    -- pins the orElse order in parseDigestV2 (Digest.hs) — a silent flip would
    -- otherwise regress the gating id to the stale flat value on any digest that
    -- carries both (i.e. every digest this version writes).
    H.it "prefers generation.generatorId over the flat generator when both are present" $ do
      let both = Digest.parseDigestV2 $ unlines
            [ "{"
            , "  \"generator\": \"flat-stale\","
            , "  \"generation\": {"
            , "    \"generatorId\": \"object-wins\","
            , "    \"mode\": \"published\","
            , "    \"host\": \"haskell\""
            , "  },"
            , "  \"inputs\": {}, \"outputs\": {}"
            , "}" ]
      digestGenerator both `H.shouldBe` "object-wins"
      genGeneratorId (digestGeneration both) `H.shouldBe` "object-wins"

    -- (gap b) Fallback + floor: a "generation" object present but WITHOUT a
    -- generatorId falls back to the flat "generator"; with neither, the gating
    -- id floors to "".
    H.it "falls back to flat generator when the generation object omits generatorId" $ do
      let noId = Digest.parseDigestV2 $ unlines
            [ "{"
            , "  \"generator\": \"flat-fallback\","
            , "  \"generation\": { \"mode\": \"shim\", \"host\": \"java\", \"revision\": \"beef-dirty\" },"
            , "  \"inputs\": {}, \"outputs\": {}"
            , "}" ]
      digestGenerator noId `H.shouldBe` "flat-fallback"
      genMode (digestGeneration noId) `H.shouldBe` ModeShim  -- object's other fields still read

    H.it "floors the gating id to empty when neither flat nor object id is present" $ do
      let neither = Digest.parseDigestV2 "{ \"inputs\": {}, \"outputs\": {} }"
      digestGenerator neither `H.shouldBe` ""
      genGeneratorId (digestGeneration neither) `H.shouldBe` ""

    -- (gap c) THE compat guarantee, at the string level: serializeDigestV2 must
    -- still emit the flat "generator" line so a digest written by THIS version
    -- remains parseable by the PREVIOUS (flat-only) reader. A round-trip test
    -- cannot catch removal of the flat line (the new reader would still recover
    -- the id from the object), so assert the literal line is present.
    H.it "serializeDigestV2 still emits the flat \"generator\" line (compat guarantee)" $ do
      let out = Digest.serializeDigestV2 (withGen publishedGen)
          hasFlatGenerator = any (\ln -> L.isInfixOf "\"generator\":" ln
                                         && not (L.isInfixOf "\"generatorId\"" ln))
                                 (lines out)
      hasFlatGenerator `H.shouldBe` True

    -- (gap d) Optional-field semantics through the public parser: an absent
    -- optional reads as Nothing; the mode defaults to published when unset. This
    -- exercises the same nonEmpty/mode-default logic generationRecord relies on.
    H.it "absent optionals parse as Nothing and mode defaults to published" $ do
      let minimal = Digest.parseDigestV2 $ unlines
            [ "{"
            , "  \"generation\": { \"generatorId\": \"g\", \"host\": \"haskell\" },"
            , "  \"inputs\": {}, \"outputs\": {}"
            , "}" ]
          g = digestGeneration minimal
      genMode g `H.shouldBe` ModePublished
      genHydraVersion g `H.shouldBe` Nothing
      genRevision g `H.shouldBe` Nothing
      genTimestamp g `H.shouldBe` Nothing
