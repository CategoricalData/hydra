module RegenScheme where

import Hydra.Ext.Generation

main :: IO ()
main = do
  putStrLn "Generating Scheme..."
  n <- writeScheme "../hydra-scheme/src/gen-main/scheme" mainModules mainModules
  putStrLn $ "Generated " ++ show n ++ " files"
