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
import Hydra.Ext.Haskell.Coder (moduleToHaskell)
import Hydra.Ext.Haskell.Language (haskellLanguage)
import Hydra.Ext.Java.Coder (moduleToJava)
import Hydra.Ext.Java.Language (javaLanguage)
import Hydra.Ext.Python.Coder (moduleToPython)
import Hydra.Ext.Python.Language (pythonLanguage)

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

  -- Read namespace lists from manifest.json
  mainNamespaces <- readManifestField haskellMainJson "mainModules"

  putStrLn "Loading main modules from JSON..."
  putStrLn $ "  Source: " ++ haskellMainJson
  mainMods <- loadModulesFromJson False haskellMainJson kernelModules mainNamespaces
  putStrLn $ "  Loaded " ++ show (length mainMods) ++ " main modules."
  putStrLn ""

  -- Generate code for the target language.
  -- doInfer=False because modules loaded from JSON already have type annotations.
  let outMain = outDir FP.</> ("src/gen-main/" ++ target)
  putStrLn $ "Generating " ++ target ++ " main code..."

  mainStart <- getCurrentTime
  case target of
    "haskell" -> generateSources moduleToHaskell haskellLanguage False False False False outMain mainMods mainMods
    "java"    -> generateSources moduleToJava    javaLanguage    False True False True   outMain mainMods mainMods
    "python"  -> generateSources moduleToPython  pythonLanguage  False True True False   outMain mainMods mainMods
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
  testNamespaces <- readManifestField haskellMainJson "testModules"

  putStrLn "Loading test modules from JSON..."
  putStrLn $ "  Source: " ++ haskellTestJson
  testMods <- loadModulesFromJson False haskellTestJson kernelModules testNamespaces
  putStrLn $ "  Loaded " ++ show (length testMods) ++ " test modules."
  putStrLn ""

  let allUniverse = mainMods ++ testMods
  let outTest = outDir FP.</> ("src/gen-test/" ++ target)
  putStrLn $ "Generating " ++ target ++ " test code..."

  testStart <- getCurrentTime
  case target of
    "haskell" -> generateSources moduleToHaskell haskellLanguage False False False False outTest allUniverse testMods
    "java"    -> generateSources moduleToJava    javaLanguage    False True False True   outTest allUniverse testMods
    "python"  -> generateSources moduleToPython  pythonLanguage  False True True False   outTest allUniverse testMods
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
