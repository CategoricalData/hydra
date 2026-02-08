module Main where

import Hydra.Ext.Generation
import qualified Hydra.Sources.Kernel.Terms.Unification as Unification

main :: IO ()
main = do
  putStrLn "Testing unification module only..."
  writeJava "/tmp/test-java-unification" kernelModules [Unification.module_]
  putStrLn "Done!"
