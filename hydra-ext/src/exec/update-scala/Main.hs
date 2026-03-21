module Main where

import Hydra.Ext.Generation

main :: IO ()
main = do
  putStrLn "=== Generate Scala source modules ==="
  let mods = mainModules <> hydraExtEssentialModules
  n <- writeScala "../hydra-scala/src/gen-main/scala" mods mods
  putStrLn $ "Generated " ++ show n ++ " files"
  putStrLn "=== Done! ==="
