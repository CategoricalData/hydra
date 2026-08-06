-- | Regression tests for 'overlayLibSubs' (#568/#630): the on-disk overlay-directory
-- existence check that drives emission-time redirect of hydra.lib.<sub> references to
-- hydra.overlay.<lang>.lib.<sub>. #630 moved the redirect itself into each coder (Haskell,
-- TypeScript, Scala, Python, Lisp), consulting 'overlayLibSubs' directly at coding time;
-- the driver-level post-generation correction pass this file used to also test
-- ('correctHaskellLibRedirect'/'correctTypeScriptLibRedirect') is now dead code, deleted.
--
-- These tests run from the Haskell head's working directory (heads/haskell), which is
-- where 'overlayLibSubs' resolves its relative overlay directory paths — the same cwd
-- stack uses for 'stack test'.

module Hydra.GenerationLibSpec where

import qualified Hydra.Generation as Generation
import qualified Data.Set as S
import qualified Test.Hspec as H


spec :: H.Spec
spec = do
  haskellSubs <- H.runIO (Generation.overlayLibSubs Generation.haskellOverlayLibDir)
  typeScriptSubs <- H.runIO (Generation.overlayLibSubs Generation.typeScriptOverlayLibDir)

  H.describe "overlayLibSubs discovers the Haskell overlay lib directory (#568)" $ do
    H.it "finds a known overlay-backed sub (chars)" $
      S.member "chars" haskellSubs `H.shouldBe` True

    H.it "does NOT find hydra.lib.defaults (no overlay implementation)" $
      S.member "defaults" haskellSubs `H.shouldBe` False

    H.it "excludes the shared Libraries registry file" $
      S.member "libraries" haskellSubs `H.shouldBe` False

  H.describe "overlayLibSubs discovers the TypeScript overlay lib directory (#568)" $ do
    H.it "finds a known overlay-backed sub (math)" $
      S.member "math" typeScriptSubs `H.shouldBe` True

    H.it "does NOT find hydra.lib.defaults (no overlay implementation)" $
      S.member "defaults" typeScriptSubs `H.shouldBe` False
