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

  putStrLn "=== Generate Java eval lib modules ==="
  putStrLn ""
  putStrLn "Generating eval lib modules to ../hydra-java/src/gen-main/java..."
  putStrLn ""

  -- Universe includes mainModules to provide all dependencies
  -- Generate only the eval lib modules
  writeJava "../hydra-java/src/gen-main/java" mainModules evalLibModules

  putStrLn ""
  putStrLn "=== Done! ==="
  putStrLn ""
  putStrLn "To view the generated modules:"
  putStrLn "  ls -R ../hydra-java/src/gen-main/java/hydra/eval"
