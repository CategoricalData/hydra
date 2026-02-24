module Main where

import Hydra.Generation (writeManifestJson)


main :: IO ()
main = do
  putStrLn "=== Generate JSON manifest ==="
  putStrLn ""
  writeManifestJson "src/gen-main/json"
  putStrLn ""
  putStrLn "=== Done! ==="
