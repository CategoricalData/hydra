module Main where

import Hydra.Sources.All (kernelModules)
import Hydra.Ext.Generation (writeCoq)
import Hydra.Ext.Coq.Generate (writeCoqProject)

main :: IO ()
main = do
  let outputDir = "../../dist/coq/hydra-kernel/src/main/coq"
  putStrLn "Generating Coq (.v) files from Hydra kernel modules..."
  writeCoqProject outputDir
  n <- writeCoq outputDir kernelModules kernelModules
  putStrLn $ "Generated " ++ show n ++ " .v files"
