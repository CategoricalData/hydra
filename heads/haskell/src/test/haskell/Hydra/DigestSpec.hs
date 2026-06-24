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
import Hydra.Packaging (Module(..), ModuleName(..))

import qualified Data.List as L
import qualified Data.Map as M
import qualified Test.Hspec as H


-- | A name-only Module. 'hashUniverse' reads only 'moduleName', so the other
-- fields are irrelevant for digest computation.
nameOnly :: String -> Module
nameOnly ns = Module (ModuleName ns) Nothing [] []

-- | Build a minimal Digest with the given generatorId and informational fields.
mkDigest :: String -> Maybe Digest.GenerationMode -> Maybe String -> Digest.Digest
mkDigest genId mode host = Digest.emptyDigest
    { Digest.digestGeneration = Digest.emptyGenerationRecord
        { Digest.generationId        = genId
        , Digest.generationMode      = mode
        , Digest.generationHost      = host
        , Digest.generationRevision  = Just "abc1234"
        , Digest.generationTimestamp = Just "2026-06-23T00:00:00Z"
        }
    }


spec :: H.Spec
spec = do
  nsFiles <- H.runIO Digest.discoverModuleNameFiles

  H.describe "discoverModuleNameFiles covers native (.java/.py) coder sources (#400)" $ do

    -- The eight native-owned Java coder modules. All must be discovered, or
    -- a change to that module's .java source would not invalidate the
    -- hydra-java per-package digest.
    let javaModules =
          [ "hydra.java.coder", "hydra.java.environment", "hydra.java.language"
          , "hydra.java.names", "hydra.java.serde", "hydra.java.syntax"
          , "hydra.java.testing", "hydra.java.utils" ]
        -- The seven native-owned Python coder modules.
        pythonModules =
          [ "hydra.python.coder", "hydra.python.environment", "hydra.python.language"
          , "hydra.python.names", "hydra.python.serde", "hydra.python.syntax"
          , "hydra.python.testing", "hydra.python.utils" ]

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

  H.describe "digestsMatch gates only on generatorId, not informational fields (#413)" $ do

    H.it "matches when generatorId is equal, ignoring mode" $
      Digest.digestsMatch
        (mkDigest "abc" (Just Digest.GenerationModePublished) (Just "haskell"))
        (mkDigest "abc" (Just Digest.GenerationModeShim)      (Just "java"))
        `H.shouldBe` True

    H.it "matches when generatorId is equal, ignoring host" $
      Digest.digestsMatch
        (mkDigest "abc" Nothing (Just "haskell"))
        (mkDigest "abc" Nothing (Just "java"))
        `H.shouldBe` True

    H.it "matches when generatorId is equal, ignoring revision and timestamp" $
      let d1 = mkDigest "abc" Nothing Nothing
          d2 = (mkDigest "abc" Nothing Nothing)
                 { Digest.digestGeneration =
                     (Digest.digestGeneration (mkDigest "abc" Nothing Nothing))
                       { Digest.generationRevision  = Just "deadbeef-dirty"
                       , Digest.generationTimestamp = Just "2099-01-01T00:00:00Z"
                       }
                 }
      in Digest.digestsMatch d1 d2 `H.shouldBe` True

    H.it "does not match when generatorId differs" $
      Digest.digestsMatch
        (mkDigest "abc" Nothing Nothing)
        (mkDigest "xyz" Nothing Nothing)
        `H.shouldBe` False

  H.describe "GenerationRecord serialization round-trip (#413)" $ do

    H.it "serializes and re-parses generatorId via the v2 digest format" $ do
      let gr = Digest.emptyGenerationRecord
                 { Digest.generationId       = "deadbeef12345678"
                 , Digest.generationMode     = Just Digest.GenerationModeShim
                 , Digest.generationHost     = Just "haskell"
                 , Digest.generationRevision = Just "abc1234-dirty"
                 , Digest.generationTimestamp = Just "2026-06-23T00:00:00Z"
                 }
          d  = Digest.emptyDigest { Digest.digestGeneration = gr }
          s  = Digest.serializeDigestV2 d
          d' = Digest.parseDigestV2 s
          gr' = Digest.digestGeneration d'
      Digest.generationId gr'           `H.shouldBe` "deadbeef12345678"
      Digest.generationMode gr'         `H.shouldBe` Just Digest.GenerationModeShim
      Digest.generationHost gr'         `H.shouldBe` Just "haskell"
      Digest.generationRevision gr'     `H.shouldBe` Just "abc1234-dirty"
      Digest.generationTimestamp gr'    `H.shouldBe` Just "2026-06-23T00:00:00Z"

    H.it "parses legacy flat 'generator' field as generatorId (#413 backward compat)" $ do
      let legacyDigest = "{ \"digestFormatVersion\": 1, \"moduleFormatVersion\": 1,\n"
                      ++ "  \"generator\": \"oldstamp\",\n"
                      ++ "  \"inputs\": {}, \"outputs\": {} }"
          d = Digest.parseDigestV2 legacyDigest
      Digest.generationId (Digest.digestGeneration d) `H.shouldBe` "oldstamp"
