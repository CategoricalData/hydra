-- | Export Hydra test-suite modules to JSON.
--
-- Migrated from the flat-universe writer ('writeModulesJson') to the
-- per-package incremental inference driver ('writeTestModulesJson', #395).
-- The flat path ran one Algorithm-W pass over the entire main+test
-- universe on every cache miss (~17 min wall, ~8 GB RSS); the incremental
-- path re-infers only the test modules against the already-typed main
-- universe loaded from dist/json/<pkg>/src/main/json. See #395 and #381.

module Main where

import Hydra.Generation (writeTestModulesJson)
import Hydra.PackageRouting (defaultDistJsonRoot)
import Hydra.Sources.All (mainModules)
import Hydra.Sources.Test.All (testModules)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Exception (catch, SomeException)
import qualified Data.List as L


main :: IO ()
main = do
  distRoot <- parseDistRoot defaultDistJsonRoot
  putStrLn "=== Generate Hydra test modules JSON ==="
  putStrLn ""
  putStrLn $ "Generating " ++ show (length testModules)
    ++ " test modules to JSON (per-package incremental inference)..."
  putStrLn $ "dist-json root: " ++ distRoot
  putStrLn ""

  result <- catch (writeTestModulesJson distRoot mainModules testModules >> return True)
                  (\e -> do
                    putStrLn $ "Error: " ++ show (e :: SomeException)
                    return False)

  if result
    then do
      putStrLn ""
      putStrLn "=== Done! ==="
    else do
      putStrLn ""
      putStrLn "=== FAILED ==="
      exitFailure

-- | Parse the dist-json root. Accepts the new --dist-root flag and, for
-- call-site compatibility, the legacy --output-dir flag — which older
-- scripts pass as a full <root>/hydra-kernel/src/test/json path. When a
-- src/test/json suffix is present it is stripped back to the dist root,
-- since the per-package writer routes its own per-package subdirectories.
parseDistRoot :: String -> IO String
parseDistRoot defaultRoot = do
  args <- getArgs
  return $ go args
  where
    go ("--dist-root" : dir : _)  = dir
    go ("--output-dir" : dir : _) = stripTestSuffix dir
    go (_ : rest)                 = go rest
    go []                         = defaultRoot
    -- Strip a trailing "/<pkg>/src/test/json" so a legacy --output-dir
    -- pointing at the old single-tree path resolves to the dist root.
    stripTestSuffix dir =
      let parts = splitOnSlash dir
      in case reverse parts of
           ("json" : "test" : "src" : _pkg : rest) ->
             joinSlash (reverse rest)
           _ -> dir
    splitOnSlash = foldr step [[]]
      where
        step '/' acc       = [] : acc
        step c   (cur:rest) = (c:cur) : rest
        step _   []         = []  -- unreachable; seed is [[]]
    joinSlash = L.intercalate "/"
