module Main where

import Hydra.Generation (writeModulesJson)
import Hydra.ExtGeneration (writeExtManifestJson)
import Hydra.Sources.Ext (hydraExtJsonModules)
import Hydra.Sources.All (mainModules)
import System.Exit (exitFailure)
import Control.Exception (catch, SomeException)


main :: IO ()
main = do
  putStrLn "=== Generate Hydra ext modules JSON ==="
  putStrLn ""
  putStrLn $ "Generating " ++ show (length hydraExtJsonModules) ++ " hydra-ext modules to JSON..."
  putStrLn ""

  let universeModules = mainModules ++ hydraExtJsonModules
  result <- catch (writeModulesJson True "../../dist/json/hydra-ext/src/main/json" universeModules hydraExtJsonModules >> return True)
                  (\e -> do
                    putStrLn $ "Error: " ++ show (e :: SomeException)
                    return False)

  if result
    then do
      putStrLn ""
      writeExtManifestJson "../../dist/json/hydra-ext/src/main/json"
      putStrLn ""
      putStrLn "=== Done! ==="
      putStrLn ""
      putStrLn "Generated files are in: ../../dist/json/hydra-ext/src/main/json/"
    else do
      putStrLn ""
      putStrLn "=== FAILED ==="
      exitFailure
