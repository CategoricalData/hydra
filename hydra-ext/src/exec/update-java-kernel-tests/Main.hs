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

  putStrLn "=== Generate Java kernel tests ==="
  putStrLn ""
  putStrLn "Generating kernel tests to ../hydra-java/src/gen-test/java..."
  putStrLn ""

  -- Universe includes mainModules to provide all dependencies
  -- Generate the test modules
  writeJava "../hydra-java/src/gen-test/java" mainModules testModules

  putStrLn ""
  putStrLn "=== Done! ==="
  putStrLn ""
  putStrLn "To view the generated tests:"
  putStrLn "  ls -R ../hydra-java/src/gen-test/java/hydra/test"
