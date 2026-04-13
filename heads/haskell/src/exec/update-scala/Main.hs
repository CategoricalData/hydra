module Main where

import Hydra.ExtGeneration

main :: IO ()
main = do
  putStrLn "=== Generate Scala source modules ==="
  let mods = mainModules <> hydraBootstrapCoderModules
  n <- writeScala "../../dist/scala/hydra-kernel/src/main/scala" mods mods
  putStrLn $ "Generated " ++ show n ++ " files"
  putStrLn "=== Done! ==="
