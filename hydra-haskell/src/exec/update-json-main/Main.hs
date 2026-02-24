module Main where

import Hydra.Generation (writeModulesJson)
import Hydra.Sources.All (mainModules)
import Hydra.Sources.Eval.Lib.All (evalLibModules)
import System.Exit (exitFailure)
import Control.Exception (catch, SomeException)


main :: IO ()
main = do
  let allMods = mainModules ++ evalLibModules
  putStrLn "=== Generate Hydra main modules JSON ==="
  putStrLn ""
  putStrLn $ "Generating " ++ show (length allMods) ++ " main + eval lib modules to JSON..."
  putStrLn ""

  result <- catch (writeModulesJson True "src/gen-main/json" allMods allMods >> return True)
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
