-- | Self-validation for the hydra.build.libraries registry (#533).
--
-- 'Hydra.Sources.Build.Libraries.expectedLibraryNames' is hand-authored (per #533's
-- ruling: no existing Haskell-side list is "every hydra.lib.* module," so aliasing
-- one would silently ship a wrong registry). Hand-authoring consolidates a dozen-plus
-- duplicated host lists into ONE place, but a single hand-authored source can still
-- drift from reality if a future hydra.lib.<sub> module is added and this list isn't
-- updated — exactly the silent-gap failure mode #533 exists to close, just moved to
-- one location instead of many. This spec closes that loop: it derives the actual set
-- of registered hydra.lib.<sub> modules from the kernel source tree (one file per
-- module, the ground truth every Lib.hs file in packages/hydra-kernel represents) and
-- fails loudly if the registry and the kernel source tree disagree.
--
-- Runs from heads/haskell's working directory (the 'stack test' cwd), matching
-- Hydra.GenerationLibRedirectSpec's convention for locating sibling package trees.

module Hydra.BuildLibrariesSpec where

import qualified Hydra.Sources.Build.Libraries as BuildLibraries

import Data.Char (toLower)
import qualified Data.List as L
import qualified Data.Set as S
import qualified System.Directory as SD
import qualified System.FilePath as FP
import qualified Test.Hspec as H


-- | The kernel's actual hydra.lib.<sub> module set: one Haskell source file per
-- module in Hydra/Sources/Kernel/Lib/, lowercased, minus "defaults" (a constants-only
-- module with no native per-host overlay or registration requirement -- see
-- Hydra.Generation.overlayLibSubs, which excludes it from overlay discovery for the
-- same reason).
kernelLibDir :: FilePath
kernelLibDir = "../../packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib"

actualKernelLibSubs :: IO (S.Set String)
actualKernelLibSubs = do
  entries <- SD.listDirectory kernelLibDir
  let subs = [ map toLower (FP.dropExtension entry)
             | entry <- entries
             , FP.takeExtension entry == ".hs"
             , FP.dropExtension entry /= "Defaults"
             ]
  return (S.fromList subs)

spec :: H.Spec
spec = do
  actual <- H.runIO actualKernelLibSubs
  let registered = S.fromList BuildLibraries.expectedLibraryNames

  H.describe "hydra.build.libraries.expectedLibraries (#533)" $ do
    H.it "has no duplicate entries" $
      L.length BuildLibraries.expectedLibraryNames
        `H.shouldBe` S.size registered

    H.it "is in alphabetical order" $
      BuildLibraries.expectedLibraryNames `H.shouldBe` L.sort BuildLibraries.expectedLibraryNames

    H.it "matches the kernel's actual hydra.lib.<sub> module set exactly" $
      registered `H.shouldBe` actual

    H.it "is not accidentally empty (a vacuous match would defeat the point of this spec)" $
      S.null actual `H.shouldBe` False
