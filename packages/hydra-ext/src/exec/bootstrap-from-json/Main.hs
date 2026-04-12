-- | Unified executable for generating code from JSON modules.
--
-- Loads Hydra modules from pre-built JSON (no Haskell DSL compilation or type
-- inference needed) and generates code for a target language.
--
-- Used by both:
--   - sync-java.sh / sync-python.sh (repo sync, writes to hydra-java / hydra-python)
--   - haskell-to-*.sh (bootstrapping demo, writes to /tmp)
--
-- JSON sources:
--   dist/json/hydra-kernel/src/main/json/  — kernel, eval lib, and other modules
--   dist/json/hydra-kernel/src/test/json/  — test modules
--   hydra-ext/../../dist/json/hydra-ext/src/main/json/      — ext coder modules (Java/Python coders)
--
-- Usage:
--   bootstrap-from-json --target <haskell|java|python|clojure|scheme|common-lisp|emacs-lisp> [OPTIONS]
--
-- Options:
--   --output <dir>         Output base directory (default: repo target dir)
--   --include-coders       Also load and generate ext coder modules
--   --include-tests        Also load and generate kernel test modules
--   --kernel-only          Only generate kernel modules (exclude hydra.ext.*)
--   --types-only           Only generate type-defining modules
--   --ext-only             Only generate hydraExtDemoModules from ext manifest
--   --ext-java-only        Legacy alias for --ext-only
--   --json-dir <dir>       Override kernel JSON directory
--   --ext-json-dir <dir>   Override ext JSON directory (for --include-coders)

{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Hydra.Kernel
import Hydra.Ext.Generation
import Hydra.Ext.Haskell.Coder (moduleToHaskell)
import Hydra.Ext.Haskell.Language (haskellLanguage)
import Hydra.Ext.Java.Coder (moduleToJava)
import Hydra.Ext.Java.Language (javaLanguage)
import Hydra.Ext.Python.Coder (moduleToPython)
import Hydra.Ext.Python.Language (pythonLanguage)
import Hydra.Ext.Lisp.Language (lispLanguage)
import qualified Hydra.Ext.Lisp.Syntax as LispSyntax
import qualified Hydra.Sources.Test.TestSuite as TestSuite

import Control.Exception (catch, IOException)
import Control.Monad (when)
import Data.List (isPrefixOf, partition)
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime)
import System.Directory (listDirectory, doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import qualified System.FilePath as FP


-- | Format elapsed time for display.
formatTime :: Double -> String
formatTime secs
  | secs < 1   = show (round (secs * 1000) :: Int) ++ "ms"
  | secs < 60  = show tenths ++ "s"
  | otherwise   = show mins ++ "m " ++ show remTenths ++ "s"
  where
    tenths = fromIntegral (round (secs * 10) :: Int) / 10.0 :: Double
    mins = floor secs `div` 60 :: Int
    remSecs = secs - fromIntegral (mins * 60)
    remTenths = fromIntegral (round (remSecs * 10) :: Int) / 10.0 :: Double

-- | Count files with a given extension in a directory tree.
countFiles :: FilePath -> String -> IO Int
countFiles dir ext = go dir
  where
    go d = do
      entries <- listDirectory d `catch` \(_ :: IOException) -> return []
      counts <- mapM (\e -> do
        let p = d FP.</> e
        isFile <- doesFileExist p
        if isFile
          then return (if FP.takeExtension p == ext then 1 else 0)
          else go p) entries
      return (sum counts)

data Options = Options
  { optTarget          :: String
  , optOutput          :: Maybe FilePath
  , optIncludeCoders   :: Bool
  , optIncludeDsls     :: Bool
  , optIncludeTests    :: Bool
  , optIncludeGenTests :: Bool  -- deprecated; ignored
  , optKernelOnly      :: Bool
  , optTypesOnly       :: Bool
  , optExtJavaOnly     :: Bool
  , optJsonDir         :: Maybe FilePath
  , optExtJsonDir      :: Maybe FilePath
  }

defaultOptions :: Options
defaultOptions = Options
  { optTarget          = ""
  , optOutput          = Nothing
  , optIncludeCoders   = False
  , optIncludeDsls     = False
  , optIncludeTests    = False
  , optIncludeGenTests = False
  , optKernelOnly      = False
  , optTypesOnly       = False
  , optExtJavaOnly     = False
  , optJsonDir         = Nothing
  , optExtJsonDir      = Nothing
  }

parseArgs :: [String] -> Either String Options
parseArgs = go defaultOptions
  where
    go opts [] = if Prelude.null (optTarget opts)
      then Left "Missing --target"
      else Right opts
    go opts ("--target" : t : rest) = go (opts { optTarget = t }) rest
    go opts ("--output" : o : rest) = go (opts { optOutput = Just o }) rest
    go opts ("--include-coders" : rest) = go (opts { optIncludeCoders = True }) rest
    go opts ("--include-dsls" : rest) = go (opts { optIncludeDsls = True }) rest
    go opts ("--include-tests" : rest) = go (opts { optIncludeTests = True }) rest
    go opts ("--include-gentests" : rest) = go (opts { optIncludeGenTests = True }) rest
    go opts ("--kernel-only" : rest) = go (opts { optKernelOnly = True }) rest
    go opts ("--types-only" : rest) = go (opts { optTypesOnly = True }) rest
    go opts ("--ext-only" : rest) = go (opts { optExtJavaOnly = True }) rest
    go opts ("--ext-java-only" : rest) = go (opts { optExtJavaOnly = True }) rest  -- legacy alias
    go opts ("--json-dir" : d : rest) = go (opts { optJsonDir = Just d }) rest
    go opts ("--ext-json-dir" : d : rest) = go (opts { optExtJsonDir = Just d }) rest
    go _ (arg : _) = Left $ "Unknown argument: " ++ arg

usage :: String
usage = unlines
  [ "Usage: bootstrap-from-json --target <haskell|java|python|clojure|scheme|common-lisp|emacs-lisp> [OPTIONS]"
  , ""
  , "Options:"
  , "  --output <dir>         Output base directory"
  , "  --include-coders       Also generate ext coder modules (Java/Python coders)"
  , "  --include-dsls         Also generate DSL modules"
  , "  --include-tests        Also generate kernel test modules"
  , "  --include-gentests     (deprecated, ignored)"
  , "  --kernel-only          Only generate kernel modules (exclude hydra.ext.*)"
  , "  --types-only           Only generate type-defining modules"
  , "  --ext-only             Only generate hydraExtDemoModules from ext manifest"
  , "  --ext-java-only        Legacy alias for --ext-only"
  , "  --json-dir <dir>       Override kernel JSON directory"
  , "  --ext-json-dir <dir>   Override ext JSON directory (for --include-coders)"
  ]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs

  opts <- case parseArgs args of
    Left err -> do
      putStrLn $ "Error: " ++ err
      putStrLn ""
      putStrLn usage
      exitFailure
    Right o -> return o

  let target = optTarget opts
  let ext = case target of
        "haskell"     -> ".hs"
        "java"        -> ".java"
        "python"      -> ".py"
        "clojure"     -> ".clj"
        "scheme"      -> ".scm"
        "common-lisp" -> ".lisp"
        "emacs-lisp"  -> ".el"
        _             -> ""

  -- Determine output directories
  let defaultOutput = case target of
        "haskell"     -> "/tmp/hydra-bootstrapping-demo/haskell-to-haskell"
        "java"        -> "../../dist/java/hydra-kernel"
        "python"      -> "../../dist/python/hydra-kernel"
        "clojure"     -> "../../dist/clojure/hydra-kernel"
        "scheme"      -> "../../dist/scheme/hydra-kernel"
        "common-lisp" -> "../../dist/common-lisp/hydra-kernel"
        "emacs-lisp"  -> "../../dist/emacs-lisp/hydra-kernel"
        _             -> "/tmp/hydra-bootstrapping-demo/haskell-to-" ++ target
  let outBase = maybe defaultOutput id (optOutput opts)
  let outMain = outBase FP.</> ("src/main/" ++ target)
  let outTest = outBase FP.</> ("src/test/" ++ target)

  -- JSON directories (relative to hydra-ext working directory)
  let kernelJsonDir = maybe "../../dist/json/hydra-kernel/src/main/json" id (optJsonDir opts)
  let testJsonDir   = "../../dist/json/hydra-kernel/src/test/json"
  let extJsonDir    = maybe "../../dist/json/hydra-ext/src/main/json" id (optExtJsonDir opts)

  let targetCap = case target of
        "haskell"     -> "Haskell"
        "java"        -> "Java"
        "python"      -> "Python"
        "clojure"     -> "Clojure"
        "scheme"      -> "Scheme"
        "common-lisp" -> "Common Lisp"
        "emacs-lisp"  -> "Emacs Lisp"
        t             -> t

  putStrLn "=========================================="
  putStrLn $ "Mapping JSON to " ++ targetCap
  putStrLn "=========================================="
  putStrLn ""
  putStrLn $ "  Target:            " ++ targetCap
  putStrLn $ "  Output:            " ++ outBase
  putStrLn $ "  Include coders:    " ++ show (optIncludeCoders opts)
  putStrLn $ "  Include DSLs:      " ++ show (optIncludeDsls opts)
  putStrLn $ "  Include tests:     " ++ show (optIncludeTests opts)
  putStrLn $ "  Include gen tests: " ++ show (optIncludeGenTests opts)
  putStrLn ""

  -- Step 1: Load main + eval lib modules from kernel JSON
  putStrLn "Step 1: Loading main modules from JSON..."
  mainNamespaces <- readManifestField kernelJsonDir "mainModules"
  evalLibNamespaces <- readManifestField kernelJsonDir "evalLibModules"
  let allKernelNamespaces = mainNamespaces ++ evalLibNamespaces

  loadStart <- getCurrentTime
  mainMods <- loadModulesFromJson kernelJsonDir kernelModules allKernelNamespaces
  loadEnd <- getCurrentTime
  putStrLn $ "  Loaded " ++ show (length mainMods) ++ " modules."
  putStrLn $ "  Time: " ++ formatTime (elapsed loadEnd loadStart)
  putStrLn ""

  -- Step 2: Optionally load ext coder modules
  coderMods <- if optIncludeCoders opts
    then do
      putStrLn "Step 2: Loading hydra-ext coder modules from JSON..."
      coderNamespaces <- readManifestField extJsonDir "hydraBootstrapCoderModules"
      -- Filter out haskell coder modules (already loaded as part of mainModules)
      let kernelNsSet = fmap unNamespace allKernelNamespaces
          (_, extCoderNamespaces) = partition (\ns -> unNamespace ns `elem` kernelNsSet) coderNamespaces
      loadStart2 <- getCurrentTime
      mods <- loadModulesFromJson extJsonDir kernelModules extCoderNamespaces
      loadEnd2 <- getCurrentTime
      putStrLn $ "  Loaded " ++ show (length mods) ++ " modules."
      putStrLn $ "  Time: " ++ formatTime (elapsed loadEnd2 loadStart2)
      putStrLn ""
      return mods
    else do
      putStrLn "Step 2: Skipping ext coder modules"
      putStrLn ""
      return []

  -- Step 2b: Optionally load DSL modules from kernel JSON
  dslMods <- if optIncludeDsls opts
    then do
      putStrLn "Step 2b: Loading DSL modules from kernel JSON..."
      dslNamespaces <- readManifestField kernelJsonDir "dslModules"
      loadStart3 <- getCurrentTime
      mods <- loadModulesFromJson kernelJsonDir kernelModules dslNamespaces
      loadEnd3 <- getCurrentTime
      putStrLn $ "  Loaded " ++ show (length mods) ++ " DSL modules."
      putStrLn $ "  Time: " ++ formatTime (elapsed loadEnd3 loadStart3)
      putStrLn ""
      return mods
    else return []

  -- Apply filters
  let allMods = mainMods ++ coderMods ++ dslMods
  let filtered1 = if optKernelOnly opts
        then Prelude.filter (\m -> let ns = unNamespace (moduleNamespace m)
              in not (isPrefixOf "hydra.ext." ns) && not (isPrefixOf "hydra.json.yaml." ns)) allMods
        else allMods
  let filtered2 = if optTypesOnly opts
        then Prelude.filter (\m -> any isNativeType (moduleBindings m)) filtered1
        else filtered1
  let allMainMods = filtered2

  when (optKernelOnly opts) $ do
    putStrLn $ "Filtering to kernel modules: " ++ show (length allMainMods) ++ " of " ++ show (length allMods)
    putStrLn ""
  when (optTypesOnly opts) $ do
    putStrLn $ "Filtering to type modules: " ++ show (length allMainMods) ++ " of " ++ show (length filtered1)
    putStrLn ""

  -- When --ext-only (or legacy --ext-java-only) is used, load the ext demo modules
  -- from JSON and generate only those (using allMainMods as the universe for type resolution)
  (modsToGenerate, allModsFinal) <- if optExtJavaOnly opts
    then do
      extDemoNamespaces <- readManifestFieldWithFallback extJsonDir "hydraExtDemoModules" "hydraExtJavaModules"
      -- Filter out modules already loaded as kernel or coder modules
      let loadedNsSet = fmap (unNamespace . moduleNamespace) allMainMods
          toLoad = Prelude.filter (\ns -> unNamespace ns `notElem` loadedNsSet) extDemoNamespaces
      putStrLn $ "Loading " ++ show (length toLoad) ++ " ext demo modules from JSON..."
      extMods <- loadModulesFromJson extJsonDir kernelModules toLoad
      putStrLn $ "  Loaded " ++ show (length extMods) ++ " ext demo modules"
      putStrLn ""
      return (extMods, allMainMods ++ extMods)
    else return (allMainMods, allMainMods)


  -- Generate main modules
  let stepNum = if optIncludeCoders opts then "3" else "2"
  putStrLn $ "Step " ++ stepNum ++ ": Mapping " ++ show (length modsToGenerate) ++ " modules to " ++ targetCap ++ "..."

  genStart <- getCurrentTime
  let lispDialectAndExt = case target of
        "clojure"     -> Just (LispSyntax.DialectClojure,    "clj")
        "scheme"      -> Just (LispSyntax.DialectScheme,     "scm")
        "common-lisp" -> Just (LispSyntax.DialectCommonLisp, "lisp")
        "emacs-lisp"  -> Just (LispSyntax.DialectEmacsLisp,  "el")
        _             -> Nothing

  let lispGenerator = case lispDialectAndExt of
        Just (dialect, lispExt) -> Just (moduleToLispDialect dialect lispExt)
        Nothing -> Nothing

  mainFileCount <- case target of
    "haskell" -> generateSources moduleToHaskell haskellLanguage False False False False outMain allModsFinal modsToGenerate
    "java"    -> generateSources moduleToJava    javaLanguage    False True False True   outMain allModsFinal modsToGenerate
    "python"  -> generateSources moduleToPython  pythonLanguage  False True True False   outMain allModsFinal modsToGenerate
    _ | Just gen <- lispGenerator ->
          generateSources gen lispLanguage True False False False outMain allModsFinal modsToGenerate
    _ -> do
      putStrLn $ "Unknown target: " ++ target
      exitFailure
  genEnd <- getCurrentTime

  putStrLn $ "  Generated " ++ show mainFileCount ++ " files."
  putStrLn $ "  Time: " ++ formatTime (elapsed genEnd genStart)
  putStrLn ""

  -- Optionally generate test modules
  testFileCount <- if optIncludeTests opts
    then do
      putStrLn "Loading test modules from JSON..."
      testNamespaces <- readManifestField kernelJsonDir "testModules"
      testMods <- loadModulesFromJson testJsonDir kernelModules testNamespaces
      putStrLn $ "  Loaded " ++ show (length testMods) ++ " test modules"
      putStrLn ""

      let allUniverse = allMods ++ testMods

      -- When --kernel-only is active, ext modules are excluded from allMainMods.
      -- But test modules may depend on ext modules (e.g. hydra.test.serialization
      -- depends on hydra.ext.haskell.operators). Generate those ext modules to outMain
      -- so test code can reference them.
      when (optKernelOnly opts) $ do
        let testExtDeps = Prelude.filter (\ns -> isPrefixOf "hydra.ext." (unNamespace ns))
              $ concatMap moduleTermDependencies testMods
            extModsForTests = Prelude.filter (\m -> moduleNamespace m `elem` testExtDeps) allMods
        when (not (Prelude.null extModsForTests)) $ do
          putStrLn $ "Generating " ++ show (length extModsForTests) ++ " ext module(s) needed by tests..."
          case target of
            "haskell" -> generateSources moduleToHaskell haskellLanguage False False False False outMain allUniverse extModsForTests >> return ()
            "java"    -> generateSources     moduleToJava    javaLanguage    False True False True   outMain allUniverse extModsForTests >> return ()
            "python"  -> generateSources moduleToPython  pythonLanguage  False True True False   outMain allUniverse extModsForTests >> return ()
            _ | Just gen <- lispGenerator -> generateSources gen lispLanguage False False False False outMain allUniverse extModsForTests >> return ()
            _ -> return ()
          putStrLn ""

      putStrLn $ "Mapping test modules to " ++ targetCap ++ "..."

      testStart <- getCurrentTime
      count <- case target of
        "haskell" -> generateSources moduleToHaskell haskellLanguage False False False False outTest allUniverse testMods
        "java"    -> generateSources     moduleToJava    javaLanguage    False True False True   outTest allUniverse testMods
        "python"  -> generateSources moduleToPython  pythonLanguage  False True True False   outTest allUniverse testMods
        _ | Just gen <- lispGenerator -> generateSources gen lispLanguage False False False False outTest allUniverse testMods
        _ -> return 0
      testEnd <- getCurrentTime

      putStrLn $ "  Generated " ++ show count ++ " test files."
      putStrLn $ "  Time: " ++ formatTime (elapsed testEnd testStart)
      putStrLn ""
      return count
    else return 0

  let genTestSuccess = True

  putStrLn "=========================================="
  putStrLn $ "Done: " ++ show mainFileCount ++ " main"
    ++ (if optIncludeTests opts then " + " ++ show testFileCount ++ " test" else "")
    ++ " files"
  putStrLn $ "  Output: " ++ outBase
  putStrLn "=========================================="

  if not genTestSuccess
    then exitFailure
    else return ()

elapsed :: UTCTime -> UTCTime -> Double
elapsed end start = realToFrac (diffUTCTime end start)

