-- | Regression tests for the #568 structural fix: coders route hydra.lib.*
-- overlay imports by overlay-module EXISTENCE, not by name-matching. Before
-- this fix, the Haskell and TypeScript coders unconditionally redirected any
-- 3-segment hydra.lib.<sub> reference to hydra.overlay.<lang>.lib.<sub>,
-- excluding "hydra.lib.defaults" by literal name (hand-patched three times:
-- #549, #565, and the #569 audit). That approach re-broke on every future
-- overlay-less hydra.lib.* module. These tests pin the replacement:
-- 'overlayLibSubs' derives the redirectable subs from the overlay directory's
-- actual contents, and 'correctHaskellLibRedirect'/'correctTypeScriptLibRedirect'
-- correct the DSL's shape-only rewrite back down to only those subs.
--
-- These tests run from the Haskell head's working directory (heads/haskell),
-- which is where 'overlayLibSubs' resolves its relative overlay directory
-- paths — the same cwd stack uses for 'stack test'.

module Hydra.GenerationLibRedirectSpec where

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

  H.describe "correctHaskellLibRedirect narrows the DSL's shape-only rewrite (#568)" $ do
    let known = S.fromList ["chars", "math"]

    H.it "rewrites an overlay-less sub's import back to the kernel module" $
      Generation.correctHaskellLibRedirect known
        "import qualified Hydra.Overlay.Haskell.Lib.Defaults as X\n"
        `H.shouldBe` "import qualified Hydra.Lib.Defaults as X\n"

    H.it "leaves a known sub's import redirected, unchanged" $
      Generation.correctHaskellLibRedirect known
        "import qualified Hydra.Overlay.Haskell.Lib.Chars as X\n"
        `H.shouldBe` "import qualified Hydra.Overlay.Haskell.Lib.Chars as X\n"

    H.it "corrects only the unknown sub when both appear in the same file" $
      Generation.correctHaskellLibRedirect known
        "import qualified Hydra.Overlay.Haskell.Lib.Defaults as X\nimport qualified Hydra.Overlay.Haskell.Lib.Chars as Y\n"
        `H.shouldBe` "import qualified Hydra.Lib.Defaults as X\nimport qualified Hydra.Overlay.Haskell.Lib.Chars as Y\n"

    -- Pins the on-the-wire casing directly: the Haskell coder's importName
    -- helper (Hydra.Haskell.Coder) capitalizes every dotted segment when
    -- building a qualified import, so the generated text is PascalCase
    -- ("Hydra.Overlay.Haskell.Lib.Defaults"), never the lowercase dotted
    -- form ("hydra.overlay.haskell.lib.defaults") that the DSL's namespace
    -- string itself uses internally. An early implementation pass matched
    -- on the lowercase form and silently no-op'd on real generated text
    -- (caught before any build, via a standalone runghc check). This test
    -- guards against that regression recurring: lowercase input must NOT
    -- be recognized as a redirect to correct, since it never occurs on the
    -- wire and treating it as one would be a false match against unrelated
    -- text (e.g. a comment or string literal containing the lowercase form).
    H.it "does not match the lowercase dotted form (never appears on the wire)" $
      Generation.correctHaskellLibRedirect known
        "-- see hydra.overlay.haskell.lib.defaults for context\n"
        `H.shouldBe` "-- see hydra.overlay.haskell.lib.defaults for context\n"

  H.describe "correctTypeScriptLibRedirect narrows the DSL's shape-only rewrite (#568)" $ do
    let known = S.fromList ["chars", "math"]

    H.it "rewrites an overlay-less sub's import path back to the def-module path" $
      Generation.correctTypeScriptLibRedirect known
        "from \"../overlay/typescript/lib/defaults.js\";\n"
        `H.shouldBe` "from \"../lib/defaults.js\";\n"

    H.it "leaves a known sub's import path redirected, unchanged" $
      Generation.correctTypeScriptLibRedirect known
        "from \"../overlay/typescript/lib/math.js\";\n"
        `H.shouldBe` "from \"../overlay/typescript/lib/math.js\";\n"

  H.describe "a hypothetical future overlay-less module needs no coder change (#568 acceptance criterion)" $
    -- Simulates adding a new hydra.lib.futuresub with no overlay file: since
    -- "futuresub" is absent from the on-disk-derived known-subs set (just as
    -- "defaults" is today), the correction routes it to the kernel module
    -- automatically -- no by-name exclusion needed anywhere.
    H.it "routes an unregistered future sub back to its kernel module, unmodified source" $
      Generation.correctHaskellLibRedirect haskellSubs
        "import qualified Hydra.Overlay.Haskell.Lib.Futuresub as X\n"
        `H.shouldBe` "import qualified Hydra.Lib.Futuresub as X\n"
