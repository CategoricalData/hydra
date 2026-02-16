-- | Bootstrapping demo executable: loads Hydra kernel modules from JSON and
-- generates code for a target language. This demonstrates that Hydra can
-- regenerate its kernel from a language-independent representation (JSON with
-- System F type annotations).
--
-- Output is written to a standalone directory (default: /tmp/hydra-bootstrapping-demo)
-- with subdirectories for each bootstrapping path.

module Main where

import Hydra.Kernel
import Hydra.Ext.Generation

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import qualified System.FilePath as FP


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

  putStrLn "=========================================="
  putStrLn $ "Bootstrapping Hydra kernel to " ++ target ++ " from JSON"
  putStrLn $ "Output directory: " ++ outDir
  putStrLn "=========================================="
  putStrLn ""

  -- JSON directory (relative to hydra-ext working directory)
  let haskellMainJson = "../hydra-haskell/src/gen-main/json"

  -- Load main modules from JSON. TypeSchemes are preserved; the adaptation
  -- pipeline will adapt types (e.g. bigfloat → float64) rather than stripping
  -- and re-inferring them. System F constructs in term bodies (type lambdas,
  -- type applications) are handled by termAlternatives during adaptation.
  putStrLn "Loading main modules from JSON..."
  putStrLn $ "  Source: " ++ haskellMainJson
  mainMods <- loadAllModulesFromJsonDirWith False haskellMainJson kernelModules
  putStrLn $ "  Loaded " ++ show (length mainMods) ++ " main modules."
  putStrLn ""

  -- Generate code for the target language (kernel modules only)
  let outMain = outDir FP.</> ("src/gen-main/" ++ target)
  putStrLn $ "Generating " ++ target ++ " code to " ++ outMain ++ " ..."
  putStrLn ""

  case target of
    "haskell" -> writeHaskell outMain mainMods mainMods
    "java"    -> writeJava    outMain mainMods mainMods
    "python"  -> writePython  outMain mainMods mainMods
    _ -> do
      putStrLn $ "Unknown target: " ++ target
      exitFailure

  putStrLn ""

  -- Load and generate test modules.
  -- Test modules need inference (many bindings intentionally lack types).
  -- For the test universe, strip TypeSchemes from main modules to avoid
  -- schema-name unification conflicts (e.g. hydra.core.Term vs hydra.core.Type).
  let haskellTestJson = "../hydra-haskell/src/gen-test/json"
  putStrLn "Loading test modules from JSON..."
  putStrLn $ "  Source: " ++ haskellTestJson
  let mainModsClean = fmap stripModuleTypeSchemes mainMods
  testMods <- loadAllModulesFromJsonDirWith False haskellTestJson (kernelModules ++ mainModsClean)
  putStrLn $ "  Loaded " ++ show (length testMods) ++ " test modules."
  putStrLn ""

  let allUniverse = mainModsClean ++ testMods
  let outTest = outDir FP.</> ("src/gen-test/" ++ target)
  putStrLn $ "Generating " ++ target ++ " test code to " ++ outTest ++ " ..."
  putStrLn ""

  case target of
    "haskell" -> writeHaskell outTest allUniverse testMods
    "java"    -> writeJava    outTest allUniverse testMods
    "python"  -> writePython  outTest allUniverse testMods
    _ -> return ()

  putStrLn ""
  putStrLn "=========================================="
  putStrLn "Bootstrap complete!"
  putStrLn $ "Output written to: " ++ outDir
  putStrLn "=========================================="

