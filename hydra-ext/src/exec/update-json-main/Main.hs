module Main where

import Hydra.Ext.Generation (writeModulesJson)
import Hydra.Ext.Sources.All (hydraExtModules, mainModules)
import System.Exit (exitFailure)
import Control.Exception (catch, SomeException)


main :: IO ()
main = do
  putStrLn "=== Generate Hydra ext modules JSON ==="
  putStrLn ""
  putStrLn $ "Generating " ++ show (length hydraExtModules) ++ " hydra-ext modules to JSON..."
  putStrLn ""

  let universeModules = mainModules ++ hydraExtModules
  result <- catch (writeModulesJson True "src/gen-main/json" universeModules hydraExtModules >> return True)
                  (\e -> do
                    putStrLn $ "Error: " ++ show (e :: SomeException)
                    return False)

  if result
    then do
      putStrLn ""
      putStrLn "=== Done! ==="
      putStrLn ""
      putStrLn "Generated files are in: src/gen-main/json/"
    else do
      putStrLn ""
      putStrLn "=== FAILED ==="
      exitFailure
