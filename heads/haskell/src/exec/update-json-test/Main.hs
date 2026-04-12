module Main where

import Hydra.Generation (writeModulesJson)
import Hydra.Sources.All (mainModules)
import Hydra.Sources.Test.All (testModules)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Exception (catch, SomeException)


main :: IO ()
main = do
  outputDir <- parseOutputDir "../../dist/json/hydra-kernel/src/test/json"
  putStrLn "=== Generate Hydra test modules JSON ==="
  putStrLn ""
  putStrLn $ "Generating " ++ show (length testModules) ++ " test modules to JSON..."
  putStrLn ""

  let universeModules = mainModules ++ testModules
  result <- catch (writeModulesJson True outputDir universeModules testModules >> return True)
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
