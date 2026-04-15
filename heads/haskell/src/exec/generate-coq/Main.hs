module Main where

import Hydra.Sources.All (kernelModules)
import Hydra.ExtGeneration (writeCoq)
import Hydra.Coq.GenerateDriver (writeCoqProject, copyCoqLibFiles)

main :: IO ()
main = do
  let outputDir = "../../dist/coq/hydra-kernel/src/main/coq"
  putStrLn "Generating Coq (.v) files from Hydra kernel modules..."
  writeCoqProject outputDir
  copyCoqLibFiles outputDir
  n <- writeCoq outputDir kernelModules kernelModules
  putStrLn $ "Generated " ++ show n ++ " .v files"
