module Main where

import Hydra.Generation (writeModulesJson, writeDslJson)
import Hydra.Sources.All (mainModules, dslSourceModules, kernelModules, kernelTypesModules, haskellModules, jsonModules, otherModules)
import Hydra.Sources.Eval.Lib.All (evalLibModules)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Exception (catch, SomeException)


main :: IO ()
main = do
  outputDir <- parseOutputDir "../../dist/json/hydra-kernel/src/main/json"
  let allMods = mainModules ++ evalLibModules ++ dslSourceModules
  putStrLn "=== Generate Hydra main modules JSON ==="
  putStrLn ""
  putStrLn $ "Generating " ++ show (length allMods) ++ " main + eval lib + dsl source modules to JSON..."
  putStrLn ""

  result <- catch (writeModulesJson True outputDir allMods allMods >> return True)
                  (\e -> do
                    putStrLn $ "Error: " ++ show (e :: SomeException)
                    return False)

  putStrLn ""
  putStrLn "Generating DSL modules to JSON..."
  -- Mirror writeDslHaskell's input set: generate DSL modules for the full
  -- kernel-side universe (kernel types + JSON + other + haskell coder), not
  -- just kernelTypesModules. This is needed so that DSL modules like
  -- Hydra.Dsl.Yaml.Model and Hydra.Dsl.Haskell.Syntax get exported to JSON
  -- alongside everything else.
  let dslInputMods = kernelModules ++ jsonModules ++ otherModules ++ haskellModules
  dslResult <- catch (writeDslJson outputDir mainModules dslInputMods >> return True)
                     (\e -> do
                       putStrLn $ "Error generating DSL JSON: " ++ show (e :: SomeException)
                       return False)

  if result && dslResult
    then do
      putStrLn ""
      putStrLn "=== Done! ==="
      putStrLn ""
      putStrLn $ "Generated files are in: " ++ outputDir ++ "/"
    else do
      putStrLn ""
      putStrLn "=== FAILED ==="
      exitFailure

parseOutputDir :: String -> IO String
parseOutputDir defaultDir = do
  args <- getArgs
  return $ go args
  where
    go ("--output-dir" : dir : _) = dir
    go (_ : rest) = go rest
    go [] = defaultDir
