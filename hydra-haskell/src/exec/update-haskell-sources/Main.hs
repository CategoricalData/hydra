module Main where

import Hydra.Generation
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
      putStrLn "=== Generate encoder/decoder source modules (Haskell) ==="
      putStrLn ""
      putStrLn "Generating decoder source modules to src/gen-main/haskell..."
      writeDecoderSourceHaskell "src/gen-main/haskell" mainModules kernelTypesModules

      putStrLn ""
      putStrLn "Generating encoder source modules to src/gen-main/haskell..."
      writeEncoderSourceHaskell "src/gen-main/haskell" mainModules kernelTypesModules

      putStrLn ""
      putStrLn "Generating DSL modules to src/gen-main/haskell..."
      writeDslHaskell "src/gen-main/haskell" mainModules mainModules

      putStrLn ""
      putStrLn "=== Done! ==="
