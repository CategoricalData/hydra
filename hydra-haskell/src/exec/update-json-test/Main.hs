module Main where

import Hydra.Generation (writeModulesJson)
import Hydra.Sources.All (mainModules)
import Hydra.Sources.Test.All (testModules)
import System.Exit (exitFailure)
import Control.Exception (catch, SomeException)


main :: IO ()
main = do
  putStrLn "=== Generate Hydra test modules JSON ==="
  putStrLn ""
  putStrLn $ "Generating " ++ show (length testModules) ++ " test modules to JSON..."
  putStrLn ""

  let universeModules = mainModules ++ testModules
  result <- catch (writeModulesJson True "src/gen-test/json" universeModules testModules >> return True)
                  (\e -> do
                    putStrLn $ "Error: " ++ show (e :: SomeException)
                    return False)

  if result
    then do
      putStrLn ""
      putStrLn "=== Done! ==="
      putStrLn ""
      putStrLn "Generated files are in: src/gen-test/json/"
    else do
      putStrLn ""
      putStrLn "=== FAILED ==="
      exitFailure
