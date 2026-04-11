module Main where

import Hydra.Ext.Generation

main :: IO ()
main = do
  putStrLn "=== Generate Hydra ext modules (Haskell) ==="
  putStrLn ""
  putStrLn "Generating ext modules to src/gen-main/haskell..."
  putStrLn ""

  let universeModules = mainModules ++ hydraExtModules
  writeHaskell "src/gen-main/haskell" universeModules hydraExtModules

  putStrLn ""
  putStrLn "Generating ext DSL modules to src/gen-main/haskell..."
  putStrLn ""

  writeDslHaskell "src/gen-main/haskell" universeModules hydraExtModules

  putStrLn ""
  putStrLn "=== Done! ==="
