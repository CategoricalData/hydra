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

  writePython "../hydra-python/src/main/python" kernelModules

  putStrLn ""
  putStrLn "=== Done! ==="
  putStrLn ""
  putStrLn "To view the generated modules:"
  putStrLn "  ls -R ../hydra-python/src/main/python"
