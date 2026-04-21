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
--   bootstrap-from-json --target <haskell|java|python|scala|typescript|clojure|scheme|common-lisp|emacs-lisp> [OPTIONS]
--
-- Options:
--   --output <dir>         Output base directory (default: repo target dir)
--   --include-coders       Also load and generate ext coder modules
--   --include-tests        Also load and generate kernel test modules
--   --kernel-only          Only generate kernel modules (exclude ext coder modules)
--   --types-only           Only generate type-defining modules
--   --ext-only             Only generate hydraExtDemoModules from ext manifest
--   --ext-java-only        Legacy alias for --ext-only
--   --json-dir <dir>       Override kernel JSON directory
--   --ext-json-dir <dir>   Override ext JSON directory (for --include-coders)

{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Hydra.Kernel
import Hydra.Generation
import Hydra.Sources.All (kernelModules)
import Hydra.ExtGeneration (moduleToLispDialect, wrapLongLinesInScalaTree)
import Hydra.Haskell.Coder (moduleToHaskell)
import Hydra.Haskell.Language (haskellLanguage)
import Hydra.Java.Coder (moduleToJava)
import Hydra.Java.Language (javaLanguage)
import Hydra.Python.Coder (moduleToPython)
import Hydra.Python.Language (pythonLanguage)
import Hydra.Scala.Coder (moduleToScala)
import Hydra.Scala.Language (scalaLanguage)
import Hydra.TypeScript.Coder (moduleToTypeScript)
import Hydra.TypeScript.Language (typeScriptLanguage)
import Hydra.Lisp.Language (lispLanguage)
import qualified Hydra.Lisp.Syntax as LispSyntax
import qualified Hydra.Sources.Test.TestSuite as TestSuite

import Control.Exception (catch, IOException)
import Control.Monad (when, forM)
import qualified Control.Monad as CM
import Data.List (isPrefixOf, partition, sortOn, groupBy)
import Data.Function (on)
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
  { optTarget             :: String
  , optOutput             :: Maybe FilePath
  , optIncludeCoders      :: Bool
  , optIncludeDsls        :: Bool
  , optIncludeTests       :: Bool
  , optIncludeGenTests    :: Bool  -- deprecated; ignored
  , optKernelOnly         :: Bool
  , optTypesOnly          :: Bool
  , optExtJavaOnly        :: Bool
  , optPackageSplit       :: Bool
  , optSynthesizeSources  :: Bool
  , optJsonDir            :: Maybe FilePath
  , optExtJsonDir         :: Maybe FilePath
  }

defaultOptions :: Options
defaultOptions = Options
  { optTarget             = ""
  , optOutput             = Nothing
  , optIncludeCoders      = False
  , optIncludeDsls        = False
  , optIncludeTests       = False
  , optIncludeGenTests    = False
  , optKernelOnly         = False
  , optTypesOnly          = False
  , optExtJavaOnly        = False
  , optPackageSplit       = False
  , optSynthesizeSources  = False
  , optJsonDir            = Nothing
  , optExtJsonDir         = Nothing
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
    go opts ("--package-split" : rest) = go (opts { optPackageSplit = True }) rest
    go opts ("--synthesize-sources" : rest) = go (opts { optSynthesizeSources = True }) rest
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
  , "  --kernel-only          Only generate kernel modules (exclude ext coder modules)"
  , "  --types-only           Only generate type-defining modules"
  , "  --ext-only             Only generate hydraExtDemoModules from ext manifest"
  , "  --ext-java-only        Legacy alias for --ext-only"
  , "  --package-split        Route each module to <output>/<package>/src/main/<lang>/"
  , "                         based on namespace prefix instead of a single output dir."
  , "  --synthesize-sources   Also synthesize decoder/encoder DSL source modules"
  , "                         (Hydra.Sources.Decode.*, Hydra.Sources.Encode.*) from"
  , "                         the loaded kernel type modules."
  , "  --json-dir <dir>       Override kernel JSON directory"
  , "  --ext-json-dir <dir>   Override ext JSON directory (for --include-coders)"
  ]

-- | Map a module namespace to the package that owns it.
--
-- This is a temporary hardcoded mapping for Step 1 of the DSL→JSON→Haskell
-- conversion. See feature_290_packaging-plan.md, "Sync system redesign /
-- Package manifests". Eventually this should be derived from each package's
-- package.json (or its Manifest.hs), but for now a prefix table is enough.
--
-- The ordering matters: more specific prefixes must come before less
-- specific ones. The fallback "hydra-kernel" covers all namespaces that
-- don't match any explicit prefix.
namespaceToPackage :: Namespace -> String
namespaceToPackage (Namespace ns) = go packagePrefixes
  where
    go []                 = "hydra-kernel"
    go ((prefix, pkg) : rest)
      | prefix `isPrefixOf` ns = pkg
      | otherwise              = go rest

packagePrefixes :: [(String, String)]
packagePrefixes =
  [ -- Coder packages (main runtime modules)
    ("hydra.haskell.",              "hydra-haskell")
  , ("hydra.java.",                 "hydra-java")
  , ("hydra.python.",               "hydra-python")
  , ("hydra.scala.",                "hydra-scala")
  , ("hydra.lisp.",                 "hydra-lisp")
  , ("hydra.coq.",                  "hydra-coq")
  , ("hydra.typeScript.",            "hydra-typescript")
    -- DSL wrapper modules for coder packages
  , ("hydra.dsl.haskell.",          "hydra-haskell")
  , ("hydra.dsl.java.",             "hydra-java")
  , ("hydra.dsl.python.",           "hydra-python")
  , ("hydra.dsl.scala.",            "hydra-scala")
  , ("hydra.dsl.lisp.",             "hydra-lisp")
  , ("hydra.dsl.coq.",              "hydra-coq")
  , ("hydra.dsl.typeScript.",        "hydra-typescript")
    -- Synthesized decoder source modules for coder packages
  , ("hydra.sources.decode.haskell.",    "hydra-haskell")
  , ("hydra.sources.decode.java.",       "hydra-java")
  , ("hydra.sources.decode.python.",     "hydra-python")
  , ("hydra.sources.decode.scala.",      "hydra-scala")
  , ("hydra.sources.decode.lisp.",       "hydra-lisp")
  , ("hydra.sources.decode.coq.",        "hydra-coq")
  , ("hydra.sources.decode.typeScript.", "hydra-typescript")
    -- Synthesized encoder source modules for coder packages
  , ("hydra.sources.encode.haskell.",    "hydra-haskell")
  , ("hydra.sources.encode.java.",       "hydra-java")
  , ("hydra.sources.encode.python.",     "hydra-python")
  , ("hydra.sources.encode.scala.",      "hydra-scala")
  , ("hydra.sources.encode.lisp.",       "hydra-lisp")
  , ("hydra.sources.encode.coq.",        "hydra-coq")
  , ("hydra.sources.encode.typeScript.", "hydra-typescript")
  ]

-- | Partition a list of modules by owning package, returning a list of
--   (packageName, modules) groups. The groups are sorted by package name
--   for deterministic output ordering.
groupByPackage :: [Module] -> [(String, [Module])]
groupByPackage mods =
    fmap collapse
      $ groupBy ((==) `on` fst)
      $ sortOn fst
      $ fmap (\m -> (namespaceToPackage (moduleNamespace m), m)) mods
  where
    collapse [] = ("", [])  -- unreachable; groupBy never returns empty inner lists
    collapse grp@((pkg, _) : _) = (pkg, fmap snd grp)

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
        "scala"       -> ".scala"
        "typescript"  -> ".ts"
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
        "scala"       -> "../../dist/scala/hydra-kernel"
        "typescript"  -> "../../dist/typescript/hydra-kernel"
        "clojure"     -> "../../dist/clojure/hydra-kernel"
        "scheme"      -> "../../dist/scheme/hydra-kernel"
        "common-lisp" -> "../../dist/common-lisp/hydra-kernel"
        "emacs-lisp"  -> "../../dist/emacs-lisp/hydra-kernel"
        _             -> "/tmp/hydra-bootstrapping-demo/haskell-to-" ++ target
  let outBase = maybe defaultOutput id (optOutput opts)
  let outMain = outBase FP.</> ("src/main/" ++ target)
  let outTest = outBase FP.</> ("src/test/" ++ target)

  -- When --package-split is set, compute a per-package (main, test) output path.
  -- Callers pass the parent directory of the package dirs as --output, e.g.
  -- --output ../../dist/haskell, and each module is routed to
  -- ../../dist/haskell/<package>/src/{main,test}/<lang>/.
  let packageOutMain pkg = outBase FP.</> pkg FP.</> ("src/main/" ++ target)
  let packageOutTest pkg = outBase FP.</> pkg FP.</> ("src/test/" ++ target)

  -- JSON directories (relative to hydra-ext working directory)
  let kernelJsonDir = maybe "../../dist/json/hydra-kernel/src/main/json" id (optJsonDir opts)
  let testJsonDir   = "../../dist/json/hydra-kernel/src/test/json"
  let extJsonDir    = maybe "../../dist/json/hydra-ext/src/main/json" id (optExtJsonDir opts)

  let targetCap = case target of
        "haskell"     -> "Haskell"
        "java"        -> "Java"
        "python"      -> "Python"
        "scala"       -> "Scala"
        "typescript"  -> "TypeScript"
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
  let kernelNsStrings = fmap unNamespace allKernelNamespaces
  let filtered1 = if optKernelOnly opts
        then Prelude.filter (\m -> unNamespace (moduleNamespace m) `elem` kernelNsStrings) allMods
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

  -- Optionally synthesize decoder/encoder DSL source modules over the loaded
  -- type modules, and add them to the modules-to-generate set. The
  -- synthesized modules carry no type annotations, so we run inference over
  -- them before handing them to the generator (which skips inference on the
  -- loaded modules).
  synthesizedSourceMods <- if optSynthesizeSources opts
    then do
      -- Decoder/encoder synthesis runs over a subset of the loaded kernel
      -- type modules. To produce the same output as the historical
      -- Sources.All.kernelTypesModules (a hand-curated list), we filter to
      -- modules that define native types and exclude any namespaces that are
      -- part of a coder package or the yaml runtime.
      let isSynthInput m =
            let nsStr = unNamespace (moduleNamespace m)
                isCoder = any (\(pfx, _) -> pfx `isPrefixOf` nsStr) packagePrefixes
                isYaml  = "hydra.yaml." `isPrefixOf` nsStr
                hasType = any isNativeType (moduleBindings m)
            in hasType && not isCoder && not isYaml
      let typeMods = Prelude.filter isSynthInput allMainMods
      putStrLn $ "Synthesizing decoder/encoder source modules from "
        ++ show (length typeMods) ++ " type modules..."
      decSrc <- generateDecoderSourceModules allMainMods typeMods
      encSrc <- generateEncoderSourceModules allMainMods typeMods
      putStrLn $ "  Synthesized " ++ show (length decSrc) ++ " decoder source modules"
      putStrLn $ "  Synthesized " ++ show (length encSrc) ++ " encoder source modules"
      -- Run inference on the synthesized modules; the generator call below uses
      -- doInfer=False, which would fail on these untyped bindings otherwise.
      let synthesized = decSrc ++ encSrc
      let inferUniverse = allMainMods ++ synthesized
      inferred <- inferModulesIO inferUniverse synthesized
      putStrLn $ "  Inferred types for " ++ show (length inferred) ++ " synthesized modules"
      putStrLn ""
      return inferred
    else return []

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

  -- Prepend synthesized source modules to modsToGenerate (deduping by namespace
  -- to keep ordering stable). They go into the same universe as the main modules.
  let modsToGenerate' = modsToGenerate ++ synthesizedSourceMods
  let allModsFinal'   = allModsFinal ++ synthesizedSourceMods

  -- Generate main modules
  let stepNum = if optIncludeCoders opts then "3" else "2"
  putStrLn $ "Step " ++ stepNum ++ ": Mapping " ++ show (length modsToGenerate') ++ " modules to " ++ targetCap ++ "..."

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

  -- Dispatch to the appropriate coder + language bindings.
  let genForDir :: FilePath -> [Module] -> IO Int
      genForDir dir mods = case target of
        "haskell" -> generateSources moduleToHaskell haskellLanguage False False False False dir allModsFinal' mods
        "java"       -> generateSources moduleToJava       javaLanguage       False True False True   dir allModsFinal' mods
        "python"     -> generateSources moduleToPython     pythonLanguage     False True True False   dir allModsFinal' mods
        "scala"      -> generateSources moduleToScala      scalaLanguage      False True False False  dir allModsFinal' mods
        "typescript" -> generateSources moduleToTypeScript typeScriptLanguage False True False False  dir allModsFinal' mods
        _ | Just gen <- lispGenerator ->
              generateSources gen lispLanguage True False False False dir allModsFinal' mods
        _ -> do
          putStrLn $ "Unknown target: " ++ target
          exitFailure

  mainFileCount <- if optPackageSplit opts
    then do
      -- Partition modules by owning package and generate each group to its own dir.
      let groups = groupByPackage modsToGenerate'
      counts <- CM.forM groups $ \(pkg, pkgMods) -> do
        let dir = packageOutMain pkg
        putStrLn $ "  " ++ pkg ++ ": " ++ show (length pkgMods) ++ " modules → " ++ dir
        genForDir dir pkgMods
      return (sum counts)
    else genForDir outMain modsToGenerate'
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

      -- When --kernel-only is active, non-kernel modules are excluded from allMainMods.
      -- But test modules may depend on non-kernel modules (e.g. hydra.test.serialization
      -- depends on hydra.haskell.operators). Generate those modules to outMain
      -- so test code can reference them.
      when (optKernelOnly opts) $ do
        let testExtraDeps = Prelude.filter (\ns -> unNamespace ns `notElem` kernelNsStrings)
              $ concatMap moduleTermDependencies testMods
            extModsForTests = Prelude.filter (\m -> moduleNamespace m `elem` testExtraDeps) allMods
        when (not (Prelude.null extModsForTests)) $ do
          putStrLn $ "Generating " ++ show (length extModsForTests) ++ " ext module(s) needed by tests..."
          case target of
            "haskell"    -> generateSources moduleToHaskell    haskellLanguage    False False False False outMain allUniverse extModsForTests >> return ()
            "java"       -> generateSources moduleToJava       javaLanguage       False True False True   outMain allUniverse extModsForTests >> return ()
            "python"     -> generateSources moduleToPython     pythonLanguage     False True True False   outMain allUniverse extModsForTests >> return ()
            "typescript" -> generateSources moduleToTypeScript typeScriptLanguage False True False False  outMain allUniverse extModsForTests >> return ()
            _ | Just gen <- lispGenerator -> generateSources gen lispLanguage False False False False outMain allUniverse extModsForTests >> return ()
            _ -> return ()
          putStrLn ""

      putStrLn $ "Mapping test modules to " ++ targetCap ++ "..."

      -- Dispatch helper for the test source set.
      let genTestForDir :: FilePath -> [Module] -> IO Int
          genTestForDir dir mods = case target of
            "haskell"    -> generateSources moduleToHaskell    haskellLanguage    False False False False dir allUniverse mods
            "java"       -> generateSources moduleToJava       javaLanguage       False True False True   dir allUniverse mods
            "python"     -> generateSources moduleToPython     pythonLanguage     False True True False   dir allUniverse mods
            "scala"      -> generateSources moduleToScala      scalaLanguage      False True False False  dir allUniverse mods
            "typescript" -> generateSources moduleToTypeScript typeScriptLanguage False True False False  dir allUniverse mods
            _ | Just gen <- lispGenerator -> generateSources gen lispLanguage False False False False dir allUniverse mods
            _ -> return 0

      testStart <- getCurrentTime
      count <- if optPackageSplit opts
        then do
          let groups = groupByPackage testMods
          counts <- CM.forM groups $ \(pkg, pkgMods) -> do
            let dir = packageOutTest pkg
            putStrLn $ "  " ++ pkg ++ ": " ++ show (length pkgMods) ++ " test modules → " ++ dir
            genTestForDir dir pkgMods
          return (sum counts)
        else genTestForDir outTest testMods
      testEnd <- getCurrentTime

      putStrLn $ "  Generated " ++ show count ++ " test files."
      putStrLn $ "  Time: " ++ formatTime (elapsed testEnd testStart)
      putStrLn ""
      return count
    else return 0

  let genTestSuccess = True

  -- Scala post-processing: wrap long lines in every generated .scala file.
  -- The Scala compiler hits stack/memory limits on extremely long single-line
  -- expressions; wrapping is the same pass that writeScala applies in the
  -- DSL-direct path, lifted here so the JSON pipeline produces identical output.
  when (target == "scala") $ do
    putStrLn "Post-processing: wrapping long Scala lines..."
    wrapLongLinesInScalaTree outBase
    putStrLn ""

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

