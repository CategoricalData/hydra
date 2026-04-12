module Main where

import Hydra.Ext.Generation
import Hydra.Kernel (Module, moduleNamespace)
import qualified Hydra.Sources.Haskell.Operators as HaskellOperators
import System.Directory (createDirectoryIfMissing, copyFile, listDirectory, doesFileExist)
import System.FilePath ((</>), takeExtension)

-- | Extra non-kernel modules needed by the test suite
testDependencyModules :: [Module]
testDependencyModules = [HaskellOperators.module_]

main :: IO ()
main = do
  let allMainModules = mainModules ++ hydraBootstrapCoderModules
  let universeModules = allMainModules ++ testModules
  -- Ext coder modules only (excluding kernel modules already in mainModules)
  let kernelNamespaces = map moduleNamespace mainModules
  let coderOnlyModules = filter (\m -> moduleNamespace m `notElem` kernelNamespaces) hydraBootstrapCoderModules

  -- Scheme
  let schemeMainDir = "../../dist/scheme/hydra-kernel/src/main/scheme"
  let schemeTestDir = "../../dist/scheme/hydra-kernel/src/test/scheme"
  let libSrcDir = "../../heads/lisp/scheme/src/main/scheme/hydra/lib"
  let libDstDir = schemeMainDir </> "hydra" </> "lib"

  putStrLn "Generating Scheme..."
  n1 <- writeScheme schemeMainDir mainModules mainModules
  putStrLn $ "  " ++ show n1 ++ " kernel modules"
  c1 <- writeScheme schemeMainDir allMainModules coderOnlyModules
  putStrLn $ "  " ++ show c1 ++ " ext coder modules"
  e1 <- writeScheme schemeMainDir universeModules testDependencyModules
  putStrLn $ "  " ++ show e1 ++ " extra modules (test dependencies)"
  t1 <- writeScheme schemeTestDir universeModules testModules
  putStrLn $ "  " ++ show t1 ++ " test modules"

  -- Copy runtime library files for Scheme
  putStrLn "Copying Scheme runtime libraries..."
  createDirectoryIfMissing True libDstDir
  libFiles <- listDirectory libSrcDir
  let scmFiles = filter (\f -> takeExtension f == ".scm") libFiles
  mapM_ (\f -> copyFile (libSrcDir </> f) (libDstDir </> f)) scmFiles
  putStrLn $ "  " ++ show (length scmFiles) ++ " lib files"

  -- Create stub modules for Scheme
  let schemeStubs = [ schemeMainDir </> "hydra" </> "decode" </> "graph.scm"
                    , schemeMainDir </> "hydra" </> "decode" </> "compute.scm"
                    , schemeMainDir </> "hydra" </> "encode" </> "graph.scm"
                    , schemeMainDir </> "hydra" </> "encode" </> "compute.scm"
                    ]
  nSchemeStubs <- writeStubs schemeStubs
  putStrLn $ "  " ++ show nSchemeStubs ++ " stub files"

  -- Clojure
  let clojureMainDir = "../../dist/clojure/hydra-kernel/src/main/clojure"
  let clojureTestDir = "../../dist/clojure/hydra-kernel/src/test/clojure"
  putStrLn "Generating Clojure..."
  n2 <- writeClojure clojureMainDir mainModules mainModules
  putStrLn $ "  " ++ show n2 ++ " kernel modules"
  c2 <- writeClojure clojureMainDir allMainModules coderOnlyModules
  putStrLn $ "  " ++ show c2 ++ " ext coder modules"
  e2 <- writeClojure clojureMainDir universeModules testDependencyModules
  putStrLn $ "  " ++ show e2 ++ " extra modules (test dependencies)"
  t2 <- writeClojure clojureTestDir universeModules testModules
  putStrLn $ "  " ++ show t2 ++ " test modules"

  -- Common Lisp
  let commonLispMainDir = "../../dist/common-lisp/hydra-kernel/src/main/common-lisp"
  let commonLispTestDir = "../../dist/common-lisp/hydra-kernel/src/test/common-lisp"
  putStrLn "Generating Common Lisp..."
  n3 <- writeCommonLisp commonLispMainDir mainModules mainModules
  putStrLn $ "  " ++ show n3 ++ " kernel modules"
  c3 <- writeCommonLisp commonLispMainDir allMainModules coderOnlyModules
  putStrLn $ "  " ++ show c3 ++ " ext coder modules"
  e3 <- writeCommonLisp commonLispMainDir universeModules testDependencyModules
  putStrLn $ "  " ++ show e3 ++ " extra modules (test dependencies)"
  t3 <- writeCommonLisp commonLispTestDir universeModules testModules
  putStrLn $ "  " ++ show t3 ++ " test modules"

  -- Emacs Lisp
  let emacsLispMainDir = "../../dist/emacs-lisp/hydra-kernel/src/main/emacs-lisp"
  let emacsLispTestDir = "../../dist/emacs-lisp/hydra-kernel/src/test/emacs-lisp"
  putStrLn "Generating Emacs Lisp..."
  n4 <- writeEmacsLisp emacsLispMainDir mainModules mainModules
  putStrLn $ "  " ++ show n4 ++ " kernel modules"
  c4 <- writeEmacsLisp emacsLispMainDir allMainModules coderOnlyModules
  putStrLn $ "  " ++ show c4 ++ " ext coder modules"
  e4 <- writeEmacsLisp emacsLispMainDir universeModules testDependencyModules
  putStrLn $ "  " ++ show e4 ++ " extra modules (test dependencies)"
  t4 <- writeEmacsLisp emacsLispTestDir universeModules testModules
  putStrLn $ "  " ++ show t4 ++ " test modules"

  let totalMain = n1 + c1 + e1 + length scmFiles + nSchemeStubs + n2 + c2 + e2 + n3 + c3 + e3 + n4 + c4 + e4
  let totalTest = t1 + t2 + t3 + t4
  putStrLn $ "Total: " ++ show totalMain ++ " main files, " ++ show totalTest ++ " test files"

  putStrLn ""

writeStubs :: [FilePath] -> IO Int
writeStubs paths = do
  count <- mapM writeStub paths
  return (sum count)
  where
    writeStub path = do
      let modName = pathToModuleName path
      writeFile path $ "(define-library " ++ modName ++ "\n(import (scheme base))\n(export)\n(begin))\n"
      return 1

    pathToModuleName path =
      let parts = dropWhile (/= "hydra") $ splitPath path
      in "(" ++ unwords (map stripExt parts) ++ ")"

    splitPath [] = []
    splitPath p = case break (== '/') p of
      (a, []) -> [a]
      (a, _:rest) -> a : splitPath rest

    stripExt s = case break (== '.') s of
      (name, _) -> name
