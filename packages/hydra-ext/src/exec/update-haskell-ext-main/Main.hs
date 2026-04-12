module Main where

import Hydra.Ext.Generation

main :: IO ()
main = do
  putStrLn "=== Generate Hydra ext modules (Haskell) ==="
  putStrLn ""
  putStrLn "Generating ext modules to ../../dist/haskell/hydra-ext/src/main/haskell..."
  putStrLn ""

  let universeModules = mainModules ++ hydraExtModules
  writeHaskell "../../dist/haskell/hydra-ext/src/main/haskell" universeModules hydraExtModules

  putStrLn ""
  putStrLn "Generating ext DSL modules to ../../dist/haskell/hydra-ext/src/main/haskell..."
  putStrLn ""

  writeDslHaskell "../../dist/haskell/hydra-ext/src/main/haskell" universeModules hydraExtModules

  putStrLn ""
  putStrLn "=== Done! ==="
