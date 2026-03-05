module Main where

import Hydra.Generation (writeModulesJson)
import Hydra.Sources.All (mainModules)
import Hydra.Sources.Eval.Lib.All (evalLibModules)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Exception (catch, SomeException)


main :: IO ()
main = do
  outputDir <- parseOutputDir "src/gen-main/json"
  let allMods = mainModules ++ evalLibModules
  putStrLn "=== Generate Hydra main modules JSON ==="
  putStrLn ""
  putStrLn $ "Generating " ++ show (length allMods) ++ " main + eval lib modules to JSON..."
  putStrLn ""

  result <- catch (writeModulesJson True outputDir allMods allMods >> return True)
                  (\e -> do
                    putStrLn $ "Error: " ++ show (e :: SomeException)
                    return False)

  if result
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
