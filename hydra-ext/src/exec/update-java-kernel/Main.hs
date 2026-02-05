#!/usr/bin/env stack
{- stack script
   --resolver lts-22.28
   --package hydra-ext
-}

module Main where

import Hydra.Ext.Generation
import qualified Hydra.Sources.Haskell.Ast as HaskellAst
import qualified Hydra.Sources.Haskell.Operators as HaskellOperators
import qualified Hydra.Sources.Haskell.Language as HaskellLanguage
import qualified Hydra.Sources.Yaml.Model as YamlModel

main :: IO ()
main = do
  putStrLn "=== Generate Java kernel ==="
  putStrLn ""
  putStrLn "Generating kernel modules to ../hydra-java/src/gen-main/java..."
  putStrLn ""

  -- Universe provides all modules for dependency resolution
  -- modulesToGenerate includes kernelModules plus type-only modules
  -- needed by the test suite (e.g. Haskell AST types, operators, YAML model)
  let extraModules = [
        HaskellAst.module_,
        HaskellOperators.module_,
        HaskellLanguage.module_,
        YamlModel.module_]
  writeJava "../hydra-java/src/gen-main/java" mainModules (kernelModules ++ extraModules)

  putStrLn ""
  putStrLn "=== Done! ==="
  putStrLn ""
  putStrLn "To view the generated modules:"
  putStrLn "  ls -R ../hydra-java/src/gen-main/java"
