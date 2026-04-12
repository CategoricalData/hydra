module RegenScheme where

import Hydra.Ext.Generation

main :: IO ()
main = do
  putStrLn "Generating Scheme..."
  n <- writeScheme "../../dist/scheme/hydra-kernel/src/main/scheme" kernelModules kernelModules
  putStrLn $ "Generated " ++ show n ++ " files"
