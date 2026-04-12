module Main where

import Hydra.Haskell.Generation (writeDecoderSourceHaskell, writeEncoderSourceHaskell, writeDslHaskell)
import Hydra.Sources.All
import Hydra.Sources.Kernel.Types.All

import System.Directory (getCurrentDirectory, doesFileExist)
import System.Exit (exitFailure)

main :: IO ()
main = do
  -- Guard: this executable uses relative paths and must be run from hydra-haskell/
  cwd <- getCurrentDirectory
  let marker = "hydra.cabal"
  exists <- doesFileExist marker
  if not exists
    then do
      putStrLn $ "Error: this executable must be run from the hydra-haskell/ directory."
      putStrLn $ "  Current directory: " ++ cwd
      putStrLn $ "  Expected to find: " ++ marker
      putStrLn $ "  Hint: cd to hydra-haskell/ and re-run, or use: (cd ../hydra-haskell && stack exec update-haskell-sources)"
      exitFailure
    else do
      let kernelOut = "../../packages/hydra-kernel/src/gen-main/haskell"
      let haskellOut = "../../packages/hydra-haskell/src/gen-main/haskell"
      -- kernelSideMain = kernelModules ++ jsonModules ++ otherModules
      -- (same as mainModules without haskellModules)
      let kernelSideMain = kernelModules ++ jsonModules ++ otherModules
      putStrLn "=== Generate encoder/decoder source modules (Haskell) ==="
      putStrLn ""
      putStrLn $ "Generating decoder source modules to " ++ kernelOut ++ "..."
      writeDecoderSourceHaskell kernelOut mainModules kernelTypesModules

      putStrLn ""
      putStrLn $ "Generating encoder source modules to " ++ kernelOut ++ "..."
      writeEncoderSourceHaskell kernelOut mainModules kernelTypesModules

      putStrLn ""
      putStrLn $ "Generating DSL modules (kernel-side types) to " ++ kernelOut ++ "..."
      writeDslHaskell kernelOut mainModules kernelSideMain

      putStrLn ""
      putStrLn $ "Generating DSL modules (Haskell coder types) to " ++ haskellOut ++ "..."
      writeDslHaskell haskellOut mainModules haskellModules

      putStrLn ""
      putStrLn "=== Done! ==="
