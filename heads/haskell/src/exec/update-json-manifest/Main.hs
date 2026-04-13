module Main where

import Hydra.Generation (writeManifestJson)
import Hydra.Sources.All (kernelModules, kernelTypesModules, mainModules, testModules)


main :: IO ()
main = do
  putStrLn "=== Generate JSON manifest ==="
  putStrLn ""
  writeManifestJson "../../dist/json/hydra-kernel/src/main/json" kernelModules kernelTypesModules mainModules testModules
  putStrLn ""
  putStrLn "=== Done! ==="
