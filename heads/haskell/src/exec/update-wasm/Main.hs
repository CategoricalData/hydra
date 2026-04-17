module Main where

import Hydra.ExtGeneration

main :: IO ()
main = do
  putStrLn "=== Generate WebAssembly text format (WAT) modules ==="
  let mods = mainModules <> hydraExtModules
  n <- writeWasm "../../dist/wasm/hydra-kernel/src/main/wat" mods mainModules
  putStrLn $ "Generated " ++ show n ++ " files"
  putStrLn "=== Done! ==="
