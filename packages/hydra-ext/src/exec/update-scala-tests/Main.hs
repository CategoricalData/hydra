module Main where

import Hydra.Ext.Generation
import Hydra.Kernel (moduleNamespace)


main :: IO ()
main = do
  putStrLn "=== Generate Scala test modules ==="
  putStrLn ""

  let allMainModules = mainModules ++ hydraBootstrapCoderModules
  let universeModules = allMainModules ++ testModules

  let testOutputDir = "../../dist/scala/hydra-kernel/src/test/scala"
  putStrLn "Generating test suite modules..."
  n <- writeScala testOutputDir universeModules testModules
  putStrLn $ "  Generated " ++ show n ++ " test suite files"

  putStrLn ""
  putStrLn "=== Done! ==="
