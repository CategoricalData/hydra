module Main where

import Hydra.Sources.All (kernelModules, mainModules)
import Hydra.Sources.Test.All (testModules)
import Hydra.ExtGeneration (writeCoq)

import Control.Monad (forM_)
import Data.List (sort)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

-- | Copy hand-written test-support .v files (e.g. runner.v) into the
-- generated test tree so coqc can see them alongside the generated modules.
-- Source path is resolved relative to the current working directory, which
-- is heads/haskell/ when generate-coq-tests runs.
copyCoqTestFiles :: FilePath -> IO ()
copyCoqTestFiles baseDir = do
  let srcDir = "src/test/coq" </> "hydra" </> "test"
      dstDir = baseDir </> "hydra" </> "test"
  srcExists <- doesDirectoryExist srcDir
  if not srcExists
    then return ()
    else do
      createDirectoryIfMissing True dstDir
      files <- sort <$> listDirectory srcDir
      forM_ files $ \f -> do
        copyFile (srcDir </> f) (dstDir </> f)
        putStrLn ("  hydra/test/" ++ f)

main :: IO ()
main = do
  let outputDir = "../../dist/coq/hydra-kernel/src/test/coq"
      universe  = mainModules ++ testModules
  putStrLn "Generating Coq (.v) files from Hydra test modules..."
  n <- writeCoq outputDir universe testModules
  putStrLn $ "Generated " ++ show n ++ " test .v files"
  putStrLn "Copying hand-written test support files..."
  copyCoqTestFiles outputDir
  -- Kernel modules are in dist/coq/hydra-kernel/src/main/coq. The generated
  -- test files Require their dependencies via fully qualified `hydra.*`
  -- module paths, so both roots need to be on coqc's library path. See the
  -- _CoqProject in the test runner directory (written separately).
  putStrLn $ "Output: " ++ outputDir
  putStrLn $ "(Add a _CoqProject that unions the main and test trees to coqc.)"
