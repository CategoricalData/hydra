{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception (SomeException, catch)
import Hydra.Ext.Generation
import Hydra.Module (Module(..), Namespace(..))
import qualified Data.List as L

main :: IO ()
main = do
  putStrLn "Testing each kernel module for Java generation..."
  let universe = mainModules `L.union` hydraExtModules
  let testMod m = do
        let ns = unNamespace (moduleNamespace m)
        (writeJava "/tmp/test-java-kernel-single" universe [m] >> putStrLn ("OK: " ++ ns))
          `catch` (\(e :: SomeException) -> putStrLn ("FAIL: " ++ ns ++ "\n  " ++ show e))
  mapM_ testMod kernelModules
  putStrLn "Done!"
