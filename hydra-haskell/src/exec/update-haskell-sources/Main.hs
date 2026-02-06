module Main where

import Hydra.Generation
import Hydra.Sources.All
import Hydra.Sources.Kernel.Types.All

main :: IO ()
main = do
  putStrLn "=== Generate encoder/decoder source modules (Haskell) ==="
  putStrLn ""
  putStrLn "Generating decoder source modules to src/gen-main/haskell..."
  writeDecoderSourceHaskell "src/gen-main/haskell" mainModules kernelTypesModules

  putStrLn ""
  putStrLn "Generating encoder source modules to src/gen-main/haskell..."
  writeEncoderSourceHaskell "src/gen-main/haskell" mainModules kernelTypesModules

  putStrLn ""
  putStrLn "=== Done! ==="
