module Main where

import Hydra.Ext.Generation
import Hydra.Generation (createTestGroupLookup)
import Hydra.Kernel (Module, moduleNamespace)
import qualified Hydra.Sources.Haskell.Operators as HaskellOperators
import qualified Hydra.Sources.Test.TestSuite as TestSuite
import qualified Hydra.Test.TestSuite as GenTests
import Hydra.Ext.Lisp.TestingIo
import System.Directory (createDirectoryIfMissing, copyFile, listDirectory, doesFileExist)
import System.FilePath ((</>), takeExtension)

-- | Extra non-kernel modules needed by the test suite
testDependencyModules :: [Module]
testDependencyModules = [HaskellOperators.module_]

main :: IO ()
main = do
  let universeModules = mainModules ++ testModules

  -- Scheme
  let schemeMainDir = "../hydra-lisp/hydra-scheme/src/gen-main/scheme"
  let schemeTestDir = "../hydra-lisp/hydra-scheme/src/gen-test/scheme"
  let libSrcDir = "../hydra-lisp/hydra-scheme/src/main/scheme/hydra/lib"
  let libDstDir = schemeMainDir </> "hydra" </> "lib"

  putStrLn "Generating Scheme..."
  n1 <- writeScheme schemeMainDir mainModules mainModules
  putStrLn $ "  " ++ show n1 ++ " kernel modules"
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
  let clojureMainDir = "../hydra-lisp/hydra-clojure/src/gen-main/clojure"
  let clojureTestDir = "../hydra-lisp/hydra-clojure/src/gen-test/clojure"
  putStrLn "Generating Clojure..."
  n2 <- writeClojure clojureMainDir mainModules mainModules
  putStrLn $ "  " ++ show n2 ++ " kernel modules"
  e2 <- writeClojure clojureMainDir universeModules testDependencyModules
  putStrLn $ "  " ++ show e2 ++ " extra modules (test dependencies)"
  t2 <- writeClojure clojureTestDir universeModules testModules
  putStrLn $ "  " ++ show t2 ++ " test modules"

  -- Common Lisp
  let commonLispMainDir = "../hydra-lisp/hydra-common-lisp/src/gen-main/common-lisp"
  let commonLispTestDir = "../hydra-lisp/hydra-common-lisp/src/gen-test/common-lisp"
  putStrLn "Generating Common Lisp..."
  n3 <- writeCommonLisp commonLispMainDir mainModules mainModules
  putStrLn $ "  " ++ show n3 ++ " kernel modules"
  e3 <- writeCommonLisp commonLispMainDir universeModules testDependencyModules
  putStrLn $ "  " ++ show e3 ++ " extra modules (test dependencies)"
  t3 <- writeCommonLisp commonLispTestDir universeModules testModules
  putStrLn $ "  " ++ show t3 ++ " test modules"

  -- Emacs Lisp
  let emacsLispMainDir = "../hydra-lisp/hydra-emacs-lisp/src/gen-main/emacs-lisp"
  let emacsLispTestDir = "../hydra-lisp/hydra-emacs-lisp/src/gen-test/emacs-lisp"
  putStrLn "Generating Emacs Lisp..."
  n4 <- writeEmacsLisp emacsLispMainDir mainModules mainModules
  putStrLn $ "  " ++ show n4 ++ " kernel modules"
  e4 <- writeEmacsLisp emacsLispMainDir universeModules testDependencyModules
  putStrLn $ "  " ++ show e4 ++ " extra modules (test dependencies)"
  t4 <- writeEmacsLisp emacsLispTestDir universeModules testModules
  putStrLn $ "  " ++ show t4 ++ " test modules"

  let totalMain = n1 + e1 + length scmFiles + nSchemeStubs + n2 + e2 + n3 + e3 + n4 + e4
  let totalTest = t1 + t2 + t3 + t4
  putStrLn $ "Total: " ++ show totalMain ++ " main files, " ++ show totalTest ++ " test files"

  -- Generation tests
  let testNamespaces = map moduleNamespace TestSuite.testSuiteModules
  let lookupFn = createTestGroupLookup testNamespaces GenTests.allTests

  putStrLn ""
  putStrLn "Generating generation tests..."
  g1 <- generateClojureGenerationTests
    (clojureTestDir </> "generation") TestSuite.testSuiteModules lookupFn
  putStrLn $ "  Clojure generation tests: " ++ if g1 then "OK" else "FAILED"

  g2 <- generateSchemeGenerationTests
    (schemeTestDir </> "generation") TestSuite.testSuiteModules lookupFn
  putStrLn $ "  Scheme generation tests: " ++ if g2 then "OK" else "FAILED"

  g3 <- generateCommonLispGenerationTests
    (commonLispTestDir </> "generation") TestSuite.testSuiteModules lookupFn
  putStrLn $ "  Common Lisp generation tests: " ++ if g3 then "OK" else "FAILED"

  g4 <- generateEmacsLispGenerationTests
    (emacsLispTestDir </> "generation") TestSuite.testSuiteModules lookupFn
  putStrLn $ "  Emacs Lisp generation tests: " ++ if g4 then "OK" else "FAILED"

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
