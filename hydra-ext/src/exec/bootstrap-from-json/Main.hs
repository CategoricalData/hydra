-- | Bootstrapping demo executable: loads Hydra kernel modules from JSON and
-- generates code for a target language. This demonstrates that Hydra can
-- regenerate its kernel from a language-independent representation (JSON with
-- System F type annotations).
--
-- Output is written to a standalone directory (default: /tmp/hydra-bootstrapping-demo)
-- with subdirectories for each bootstrapping path.

{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Hydra.Kernel
import Hydra.Ext.Generation

import Control.Exception (catch, IOException)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Directory (listDirectory, doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import qualified System.FilePath as FP


-- | Format elapsed time for display.
formatTime :: Double -> String
formatTime secs
  | secs < 1   = show (round (secs * 1000) :: Int) ++ "ms"
  | secs < 60  = show tenths ++ "s"
  | otherwise   = show mins ++ "m " ++ show remTenths ++ "s"
  where
    tenths = fromIntegral (round (secs * 10) :: Int) / 10.0 :: Double
    mins = floor secs `div` 60 :: Int
    remSecs = secs - fromIntegral (mins * 60)
    remTenths = fromIntegral (round (remSecs * 10) :: Int) / 10.0 :: Double

-- | Count files with a given extension in a directory tree.
countFiles :: FilePath -> String -> IO Int
countFiles dir ext = go dir
  where
    go d = do
      entries <- listDirectory d `catch` \(_ :: IOException) -> return []
      counts <- mapM (\e -> do
        let p = d FP.</> e
        isFile <- doesFileExist p
        if isFile
          then return (if FP.takeExtension p == ext then 1 else 0)
          else go p) entries
      return (sum counts)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs

  (target, outBase) <- case args of
    ["--target", t] -> return (t, "/tmp/hydra-bootstrapping-demo")
    ["--target", t, "--output", o] -> return (t, o)
    _ -> do
      putStrLn "Usage: bootstrap-from-json --target <haskell|java|python> [--output <dir>]"
      exitFailure

  let outDir = outBase FP.</> ("haskell-to-" ++ target)
  let ext = case target of { "haskell" -> ".hs"; "java" -> ".java"; "python" -> ".py"; _ -> "" }

  putStrLn "=========================================="
  putStrLn $ "Bootstrapping Hydra to " ++ target ++ " from JSON (via Haskell)"
  putStrLn $ "Output directory: " ++ outDir
  putStrLn "=========================================="
  putStrLn ""

  -- JSON directory (relative to hydra-ext working directory)
  let haskellMainJson = "../hydra-haskell/src/gen-main/json"

  putStrLn "Loading main modules from JSON..."
  putStrLn $ "  Source: " ++ haskellMainJson
  mainMods <- loadAllModulesFromJson False haskellMainJson kernelModules
  putStrLn $ "  Loaded " ++ show (length mainMods) ++ " main modules."
  putStrLn ""

  -- Generate code for the target language (kernel modules only)
  let outMain = outDir FP.</> ("src/gen-main/" ++ target)
  putStrLn $ "Generating " ++ target ++ " main code..."

  mainStart <- getCurrentTime
  case target of
    "haskell" -> writeHaskell outMain mainMods mainMods
    "java"    -> writeJava    outMain mainMods mainMods
    "python"  -> writePython  outMain mainMods mainMods
    _ -> do
      putStrLn $ "Unknown target: " ++ target
      exitFailure
  mainEnd <- getCurrentTime
  let mainSecs = realToFrac (diffUTCTime mainEnd mainStart) :: Double

  mainFileCount <- countFiles outMain ext
  putStrLn $ "  Generated " ++ show mainFileCount ++ " main " ++ target ++ " files."
  putStrLn $ "  Time: " ++ formatTime mainSecs
  putStrLn ""

  -- Load and generate test modules.
  let haskellTestJson = "../hydra-haskell/src/gen-test/json"
  putStrLn "Loading test modules from JSON..."
  putStrLn $ "  Source: " ++ haskellTestJson
  let mainModsClean = fmap stripModuleTypeSchemes mainMods
  testMods <- loadAllModulesFromJsonDirWith False haskellTestJson (kernelModules ++ mainModsClean)
  putStrLn $ "  Loaded " ++ show (length testMods) ++ " test modules."
  putStrLn ""

  let allUniverse = mainModsClean ++ testMods
  let outTest = outDir FP.</> ("src/gen-test/" ++ target)
  putStrLn $ "Generating " ++ target ++ " test code..."

  testStart <- getCurrentTime
  case target of
    "haskell" -> writeHaskell outTest allUniverse testMods
    "java"    -> writeJava    outTest allUniverse testMods
    "python"  -> writePython  outTest allUniverse testMods
    _ -> return ()
  testEnd <- getCurrentTime
  let testSecs = realToFrac (diffUTCTime testEnd testStart) :: Double

  testFileCount <- countFiles outTest ext
  putStrLn $ "  Generated " ++ show testFileCount ++ " test " ++ target ++ " files."
  putStrLn $ "  Time: " ++ formatTime testSecs
  putStrLn ""

  putStrLn "=========================================="
  putStrLn "Bootstrap complete!"
  putStrLn $ "  Output files: " ++ show mainFileCount ++ " main + " ++ show testFileCount ++ " test"
  putStrLn $ "  Output:       " ++ outDir
  putStrLn "=========================================="

