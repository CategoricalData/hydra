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
