-- | Regression tests for #789: 'Hydra.Generation.verifyWrittenFiles' must detect a
-- generation pass that silently left a truncated or missing file on disk (e.g. an
-- ENOSPC write cut short mid-flush), rather than letting the caller report success
-- on a partial tree. See the #789 issue and the plan doc on the
-- bug_789_silent_partial_dist_haskell_generation branch for the full incident
-- writeup (a sync-haskell.sh run that hit disk exhaustion partway through
-- generating dist/haskell/hydra-kernel, leaving 0 of several hundred expected
-- modules under Hydra/Core/ with no error at the point of failure).
--
-- These tests exercise 'verifyWrittenFiles' directly against a scratch temp
-- directory rather than going through the full 'generateSourcesWithTransform'
-- pipeline, since that pipeline needs a real module-inference universe to
-- produce its (path, content) pairs; 'verifyWrittenFiles' is factored out
-- exactly so the write-verification logic can be tested independently of
-- that machinery (see its Haddock in Hydra/Generation.hs).

module Hydra.GenerationWriteVerificationSpec where

import qualified Hydra.Generation as Generation
import qualified System.Directory as SD
import System.FilePath ((</>), takeDirectory)
import qualified Test.Hspec as H


spec :: H.Spec
spec = do
  H.describe "verifyWrittenFiles (#789)" $ do
    H.it "reports no missing files when every expected file is fully written" $ do
      tmpRoot <- (</> "hydra-789-spec-complete") <$> SD.getTemporaryDirectory
      SD.removePathForcibly tmpRoot
      SD.createDirectoryIfMissing True tmpRoot

      let files = [ ("Hydra/Core/Model.hs", "module Hydra.Core.Model where\n")
                  , ("Hydra/Core/Validation.hs", "module Hydra.Core.Validation where\n")
                  ]
      -- Write them exactly as generateSourcesWithTransform's writePair would
      -- (content as-is; verifyWrittenFiles itself appends the trailing
      -- newline when computing the expected size, mirroring writePair).
      mapM_ (\(path, content) -> do
          let full = tmpRoot </> path
          SD.createDirectoryIfMissing True (takeDirectory full)
          writeFile full content
        ) files

      missing <- Generation.verifyWrittenFiles tmpRoot id files
      missing `H.shouldBe` []

    H.it "reports a file that is missing entirely from disk" $ do
      tmpRoot <- (</> "hydra-789-spec-missing") <$> SD.getTemporaryDirectory
      SD.removePathForcibly tmpRoot
      SD.createDirectoryIfMissing True tmpRoot

      let present = ("Hydra/Core/Model.hs", "module Hydra.Core.Model where\n")
          absent  = ("Hydra/Core/Validation.hs", "module Hydra.Core.Validation where\n")
          files = [present, absent]
      let full = tmpRoot </> fst present
      SD.createDirectoryIfMissing True (takeDirectory full)
      writeFile full (snd present)
      -- 'absent' is deliberately never written -- simulates a module that
      -- generateSourceFiles computed but whose write never happened (e.g.
      -- the process died before reaching it).

      missing <- Generation.verifyWrittenFiles tmpRoot id files
      missing `H.shouldBe` [fst absent]

    H.it "reports a file that was truncated mid-write (short of its expected content)" $ do
      tmpRoot <- (</> "hydra-789-spec-truncated") <$> SD.getTemporaryDirectory
      SD.removePathForcibly tmpRoot
      SD.createDirectoryIfMissing True tmpRoot

      let truncated = ("Hydra/Core/Validation.hs", "module Hydra.Core.Validation where\n  -- a lot more content than this\n")
          complete  = ("Hydra/Core/Model.hs", "module Hydra.Core.Model where\n")
          files = [complete, truncated]
      mapM_ (\(path, content) -> do
          let full = tmpRoot </> path
          SD.createDirectoryIfMissing True (takeDirectory full)
          writeFile full content
        ) files
      -- Simulate an ENOSPC-style short write: overwrite the second file with
      -- only a prefix of its intended content, as a writer killed mid-flush
      -- would leave on disk.
      let (truncPath, truncContent) = truncated
          shortContent = take 10 truncContent
      writeFile (tmpRoot </> truncPath) shortContent

      missing <- Generation.verifyWrittenFiles tmpRoot id files
      missing `H.shouldBe` [truncPath]

    H.it "does not flag a file the writer skipped because it was already byte-identical" $ do
      -- Mirrors generateSourcesWithTransform's writePair skip-write optimization:
      -- if the on-disk content already equals the expected content, the writer
      -- never re-touches the file. verifyWrittenFiles must still treat that file
      -- as correctly written (final-state check, not "did a write happen").
      tmpRoot <- (</> "hydra-789-spec-skip") <$> SD.getTemporaryDirectory
      SD.removePathForcibly tmpRoot
      SD.createDirectoryIfMissing True tmpRoot

      let files = [("Hydra/Core/Model.hs", "module Hydra.Core.Model where\n")]
      mapM_ (\(path, content) -> do
          let full = tmpRoot </> path
          SD.createDirectoryIfMissing True (takeDirectory full)
          writeFile full content
        ) files

      missing <- Generation.verifyWrittenFiles tmpRoot id files
      missing `H.shouldBe` []
