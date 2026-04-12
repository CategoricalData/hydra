module Main where

import Hydra.Generation (writeModulesJson, writeDslJson)
import Hydra.Sources.All (mainModules, kernelTypesModules)
import Hydra.Sources.Eval.Lib.All (evalLibModules)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Exception (catch, SomeException)


main :: IO ()
main = do
  outputDir <- parseOutputDir "../../dist/json/hydra-kernel/src/main/json"
  let allMods = mainModules ++ evalLibModules
  putStrLn "=== Generate Hydra main modules JSON ==="
  putStrLn ""
  putStrLn $ "Generating " ++ show (length allMods) ++ " main + eval lib modules to JSON..."
  putStrLn ""

  result <- catch (writeModulesJson True outputDir allMods allMods >> return True)
                  (\e -> do
                    putStrLn $ "Error: " ++ show (e :: SomeException)
                    return False)

  putStrLn ""
  putStrLn "Generating DSL modules to JSON..."
  dslResult <- catch (writeDslJson outputDir mainModules kernelTypesModules >> return True)
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
