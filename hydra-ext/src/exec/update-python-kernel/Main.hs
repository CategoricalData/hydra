#!/usr/bin/env stack
{- stack script
   --resolver lts-22.28
   --package hydra-ext
-}

module Main where

import Hydra.Ext.Generation
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  putStrLn "=== Generate Python kernel ==="
  putStrLn ""
  putStrLn "Generating kernel modules to ../hydra-python/src/main/python..."
  putStrLn ""

  -- Universe provides all modules for dependency resolution
  -- modulesToGenerate specifies which modules to actually generate
  writePython "../hydra-python/src/main/python" kernelModules kernelModules

  putStrLn ""
  putStrLn "=== Done! ==="
  putStrLn ""
  putStrLn "To view the generated modules:"
  putStrLn "  ls -R ../hydra-python/src/main/python"
