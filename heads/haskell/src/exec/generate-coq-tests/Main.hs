module Main where

import Hydra.Sources.All (kernelModules, mainModules)
import Hydra.Sources.Test.All (testModules)
import Hydra.ExtGeneration (writeCoq)

main :: IO ()
main = do
  let outputDir = "../../dist/coq/hydra-kernel/src/test/coq"
      universe  = mainModules ++ testModules
  putStrLn "Generating Coq (.v) files from Hydra test modules..."
  n <- writeCoq outputDir universe testModules
  putStrLn $ "Generated " ++ show n ++ " test .v files"
  -- Kernel modules are in dist/coq/hydra-kernel/src/main/coq. The generated
  -- test files Require their dependencies via fully qualified `hydra.*`
  -- module paths, so both roots need to be on coqc's library path. See the
  -- _CoqProject in the test runner directory (written separately).
  putStrLn $ "Output: " ++ outputDir
  putStrLn $ "(Add a _CoqProject that unions the main and test trees to coqc.)"
