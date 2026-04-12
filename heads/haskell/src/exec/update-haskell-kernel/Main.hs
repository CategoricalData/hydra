module Main where

import Hydra.Haskell.Generation
import Hydra.Sources.All

main :: IO ()
main = do
  putStrLn "=== Generate Hydra kernel (Haskell) ==="
  putStrLn ""

  -- Kernel-level modules (language-independent: kernel, JSON, test, yaml) go into hydra-kernel.
  let kernelOut = "../../packages/hydra-kernel/src/gen-main/haskell"
  putStrLn $ "Generating kernel-level modules to " ++ kernelOut ++ "..."
  putStrLn ""
  let kernelMain = kernelModules ++ jsonModules ++ otherModules
  writeHaskell kernelOut mainModules kernelMain

  -- Haskell coder modules stay in hydra-haskell.
  let haskellOut = "../../packages/hydra-haskell/src/gen-main/haskell"
  putStrLn ""
  putStrLn $ "Generating Haskell coder modules to " ++ haskellOut ++ "..."
  putStrLn ""
  writeHaskell haskellOut mainModules haskellModules

  putStrLn ""
  putStrLn $ "Generating DSL source module (hydra.dsls) to " ++ kernelOut ++ "..."
  putStrLn ""
  writeDslSourceHaskell kernelOut

  putStrLn ""
  putStrLn "=== Done! ==="
