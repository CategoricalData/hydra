module RegenScheme where

import Hydra.Ext.Generation

main :: IO ()
main = do
  putStrLn "Generating Scheme..."
  n <- writeScheme "../hydra-lisp/hydra-scheme/src/gen-main/scheme" kernelModules kernelModules
  putStrLn $ "Generated " ++ show n ++ " files"
