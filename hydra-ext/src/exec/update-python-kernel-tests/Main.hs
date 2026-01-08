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

  putStrLn "=== Generate Python kernel tests ==="
  putStrLn ""
  putStrLn "Generating kernel tests to ../hydra-python/src/gen-test/python..."
  putStrLn ""

  -- Universe includes mainModules to provide all dependencies
  -- Generate the test modules
  writePython "../hydra-python/src/gen-test/python" mainModules testModules

  putStrLn ""
  putStrLn "=== Done! ==="
  putStrLn ""
  putStrLn "To view the generated tests:"
  putStrLn "  ls -R ../hydra-python/src/gen-test/python/hydra/test"
