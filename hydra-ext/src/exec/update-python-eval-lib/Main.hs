#!/usr/bin/env stack
{- stack script
   --resolver lts-22.28
   --package hydra-ext
-}

module Main where

import Hydra.Ext.Generation
import Hydra.Sources.Eval.Lib.All (evalLibModules)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  putStrLn "=== Generate Python eval lib modules ==="
  putStrLn ""
  putStrLn "Generating eval lib modules to ../hydra-python/src/gen-main/python..."
  putStrLn ""

  -- Universe includes mainModules to provide all dependencies
  -- Generate only the eval lib modules
  writePython "../hydra-python/src/gen-main/python" mainModules evalLibModules

  putStrLn ""
  putStrLn "=== Done! ==="
  putStrLn ""
  putStrLn "To view the generated modules:"
  putStrLn "  ls -R ../hydra-python/src/gen-main/python/hydra/eval"
