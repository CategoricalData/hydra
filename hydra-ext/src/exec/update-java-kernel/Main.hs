#!/usr/bin/env stack
{- stack script
   --resolver lts-22.28
   --package hydra-ext
-}

module Main where

import Hydra.Ext.Generation

main :: IO ()
main = do
  putStrLn "=== Generate Java kernel ==="
  putStrLn ""
  putStrLn "Generating kernel modules to ../hydra-java/src/gen-main/java..."
  putStrLn ""

  writeJava "../hydra-java/src/gen-main/java" kernelModules

  putStrLn ""
  putStrLn "=== Done! ==="
  putStrLn ""
  putStrLn "To view the generated modules:"
  putStrLn "  ls -R ../hydra-java/src/gen-main/java"
