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
--   --kernel-only          Only generate kernel modules (exclude ext coder modules)
--   --types-only           Only generate type-defining modules
--   --ext-only             Only generate hydraExtDemoModules from ext manifest
--   --dist-json-root <dir> Override JSON root directory

{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Hydra.Kernel
import Hydra.Generation
import Hydra.PackageRouting (groupByPackage, namespaceToPackage, packagePrefixes)
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
import Hydra.Lisp.Language (lispLanguage)
import qualified Hydra.Lisp.Syntax as LispSyntax
import qualified Hydra.Sources.Test.TestSuite as TestSuite

import Control.Exception (catch, IOException)
import Control.Monad (when, forM)
import qualified Control.Monad as CM
import qualified Data.Char as C
import Data.List (isPrefixOf)
import qualified Data.List.Split as LS
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime)
import System.Directory (listDirectory, doesFileExist)
import qualified System.Directory as SD
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
  , optIncludeExt         :: Bool
  , optIncludeTests       :: Bool
  , optKernelOnly         :: Bool
  , optTypesOnly          :: Bool
  , optExtOnly            :: Bool
  , optSynthesizeSources  :: Bool
  , optDistJsonRoot       :: Maybe FilePath
  , optPackage            :: Maybe String  -- Layer 1: narrow generation to one package
  }

defaultOptions :: Options
defaultOptions = Options
  { optTarget             = ""
  , optOutput             = Nothing
  , optIncludeCoders      = False
  , optIncludeDsls        = False
  , optIncludeExt         = False
  , optIncludeTests       = False
  , optKernelOnly         = False
  , optTypesOnly          = False
  , optExtOnly            = False
  , optSynthesizeSources  = False
  , optDistJsonRoot       = Nothing
  , optPackage            = Nothing
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
    go opts ("--include-ext" : rest) = go (opts { optIncludeExt = True }) rest
    go opts ("--include-tests" : rest) = go (opts { optIncludeTests = True }) rest
    go opts ("--kernel-only" : rest) = go (opts { optKernelOnly = True }) rest
    go opts ("--types-only" : rest) = go (opts { optTypesOnly = True }) rest
    go opts ("--ext-only" : rest) = go (opts { optExtOnly = True }) rest
    go opts ("--synthesize-sources" : rest) = go (opts { optSynthesizeSources = True }) rest
    go opts ("--dist-json-root" : d : rest) = go (opts { optDistJsonRoot = Just d }) rest
    go opts ("--package" : p : rest) = go (opts { optPackage = Just p }) rest
    go _ (arg : _) = Left $ "Unknown argument: " ++ arg

usage :: String
usage = unlines
  [ "Usage: bootstrap-from-json --target <haskell|java|python|clojure|scheme|common-lisp|emacs-lisp> [OPTIONS]"
  , ""
  , "Options:"
  , "  --output <dir>           Output base directory"
  , "  --include-coders         Also load coder packages (hydra-java/python/scala/lisp)"
  , "  --include-dsls           Also load DSL wrapper modules"
  , "  --include-ext            Also load long-tail ext packages (hydra-coq,"
  , "                           hydra-javascript, hydra-ext)"
  , "  --include-tests          Also generate kernel test modules"
  , "  --kernel-only            Only generate kernel modules (exclude coder packages)"
  , "  --types-only             Only generate type-defining modules"
  , "  --ext-only               Only generate ext demo modules from hydra-pg / hydra-rdf"
  , "  --synthesize-sources     Also synthesize decoder/encoder DSL source modules"
  , "                           (Hydra.Sources.Decode.*, Hydra.Sources.Encode.*) from"
  , "                           the loaded kernel type modules."
  , "  --dist-json-root <dir>   Override the root dist/json directory (default:"
  , "                           ../../dist/json). The tool walks"
  , "                           <root>/<package>/src/main/json/ for each package it"
  , "                           needs to load, in dependency order."
  , "  --package <pkg>          Narrow generation to modules owned by <pkg>."
  , "                           The full universe is still loaded so cross-"
  , "                           package type references resolve."
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
        "scala"       -> ".scala"
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
        "clojure"     -> "../../dist/clojure/hydra-kernel"
        "scheme"      -> "../../dist/scheme/hydra-kernel"
        "common-lisp" -> "../../dist/common-lisp/hydra-kernel"
        "emacs-lisp"  -> "../../dist/emacs-lisp/hydra-kernel"
        _             -> "/tmp/hydra-bootstrapping-demo/haskell-to-" ++ target
  let outBase = maybe defaultOutput id (optOutput opts)
  -- Output path convention: callers pass the parent of per-package dirs as
  -- --output (e.g. --output ../../dist/haskell), and each module is routed
  -- to ../../dist/haskell/<package>/src/{main,test}/<lang>/.
  let packageOutMain pkg = outBase FP.</> pkg FP.</> ("src/main/" ++ target)
  let packageOutTest pkg = outBase FP.</> pkg FP.</> ("src/test/" ++ target)
  -- Flat outMain / outTest are used by the few legacy writers (test-mode
  -- ext injection below) that don't go through groupByPackage.
  let outMain = outBase FP.</> ("src/main/" ++ target)
  let outTest = outBase FP.</> ("src/test/" ++ target)

  -- Per-package JSON layout. Every package's main-side modules live at
  -- <root>/<pkg>/src/main/json/; the kernel's test modules live at
  -- <root>/hydra-kernel/src/test/json/.
  let distJsonRoot = maybe "../../dist/json" id (optDistJsonRoot opts)
  let pkgMainDir pkg = distJsonRoot FP.</> pkg FP.</> "src" FP.</> "main" FP.</> "json"
  let kernelJsonDir = pkgMainDir "hydra-kernel"
  let testJsonDir   = distJsonRoot FP.</> "hydra-kernel" FP.</> "src" FP.</> "test" FP.</> "json"

  -- Dependency order: baseline packages (hydra-kernel + hydra-haskell) are
  -- loaded individually in Step 1. Coder packages are loaded with
  -- --include-coders. Ext demo packages are loaded with --ext-only.
  -- Long-tail ext packages are loaded with --include-ext.
  let coderPackages   = ["hydra-java", "hydra-python", "hydra-scala", "hydra-lisp"]
  let extDemoPackages = ["hydra-pg", "hydra-rdf"]
  let extPackages     = ["hydra-coq", "hydra-javascript", "hydra-ext"]

  let targetCap = case target of
        "haskell"     -> "Haskell"
        "java"        -> "Java"
        "python"      -> "Python"
        "scala"       -> "Scala"
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
  putStrLn $ "  Include ext:       " ++ show (optIncludeExt opts)
  putStrLn $ "  Include tests:     " ++ show (optIncludeTests opts)
  putStrLn ""

  -- Load a single package's mainModules + evalLibModules from its per-package
  -- manifest. Returns the accumulated Modules; missing fields are treated as
  -- empty.
  let loadPackageMain :: String -> IO [Module]
      loadPackageMain pkg = do
        let pkgDir = pkgMainDir pkg
        mainNs <- readManifestFieldOrEmpty pkgDir "mainModules"
        evalNs <- readManifestFieldOrEmpty pkgDir "evalLibModules"
        let allNs = mainNs ++ evalNs
        if Prelude.null allNs
          then return []
          else do
            putStrLn $ "  " ++ pkg ++ ": " ++ show (length allNs) ++ " modules from " ++ pkgDir
            loadModulesFromJson pkgDir kernelModules allNs

  -- Load a single package's DSL wrapper modules.
  let loadPackageDsl :: String -> IO [Module]
      loadPackageDsl pkg = do
        let pkgDir = pkgMainDir pkg
        dslNs <- readManifestFieldOrEmpty pkgDir "dslModules"
        if Prelude.null dslNs
          then return []
          else do
            putStrLn $ "  " ++ pkg ++ ": " ++ show (length dslNs) ++ " DSL modules from " ++ pkgDir
            loadModulesFromJson pkgDir kernelModules dslNs

  -- Step 1: Load baseline main modules (hydra-kernel + hydra-haskell).
  -- Both packages are part of the bootstrap baseline: hydra-haskell provides
  -- the runtime AST modules (Hydra.Haskell.Syntax, Environment, Coder, Serde,
  -- ...) that the generated DSL source modules import, so it must pass
  -- --kernel-only filtering as part of the kernel namespace set.
  putStrLn "Step 1: Loading baseline main modules from JSON..."
  loadStart <- getCurrentTime
  kernelBaselineMods <- loadPackageMain "hydra-kernel"
  haskellBaselineMods <- loadPackageMain "hydra-haskell"
  let baselineMods = kernelBaselineMods ++ haskellBaselineMods
  loadEnd <- getCurrentTime
  putStrLn $ "  Loaded " ++ show (length baselineMods) ++ " baseline modules."
  putStrLn $ "  Time: " ++ formatTime (elapsed loadEnd loadStart)
  putStrLn ""

  let allKernelNamespaces = fmap moduleNamespace baselineMods

  -- Step 2: Optionally load coder-package main modules.
  coderMods <- if optIncludeCoders opts
    then do
      putStrLn "Step 2: Loading coder package modules from JSON..."
      loadStart2 <- getCurrentTime
      mods <- fmap concat $ CM.forM coderPackages loadPackageMain
      loadEnd2 <- getCurrentTime
      putStrLn $ "  Loaded " ++ show (length mods) ++ " coder modules."
      putStrLn $ "  Time: " ++ formatTime (elapsed loadEnd2 loadStart2)
      putStrLn ""
      return mods
    else do
      putStrLn "Step 2: Skipping coder packages"
      putStrLn ""
      return []

  -- Step 2b: Optionally load DSL wrapper modules from every loaded package.
  dslMods <- if optIncludeDsls opts
    then do
      putStrLn "Step 2b: Loading DSL wrapper modules from JSON..."
      loadStart3 <- getCurrentTime
      let dslPackages =
            ["hydra-kernel", "hydra-haskell"]
              ++ (if optIncludeCoders opts then coderPackages else [])
              ++ (if optIncludeExt opts then extPackages else [])
              ++ (if optExtOnly opts then extDemoPackages else [])
      mods <- fmap concat $ CM.forM dslPackages loadPackageDsl
      loadEnd3 <- getCurrentTime
      putStrLn $ "  Loaded " ++ show (length mods) ++ " DSL modules."
      putStrLn $ "  Time: " ++ formatTime (elapsed loadEnd3 loadStart3)
      putStrLn ""
      return mods
    else return []

  -- Step 2c: Optionally load long-tail ext packages (hydra-coq,
  -- hydra-javascript, hydra-ext) plus their type-resolution dependencies
  -- (hydra-pg, hydra-rdf). Ext modules reference pg decode/encode
  -- meta-sources and pg model types, so those must be in the universe.
  -- Already-loaded namespaces (via --include-coders) are skipped.
  extMods <- if optIncludeExt opts
    then do
      putStrLn "Step 2c: Loading ext package modules from JSON..."
      loadStart4 <- getCurrentTime
      let extAndDepPackages = extPackages ++ extDemoPackages
      allExtMods <- fmap concat $ CM.forM extAndDepPackages loadPackageMain
      -- Dedup against coder and baseline mods by namespace.
      let coderNsSet = fmap (unNamespace . moduleNamespace) (baselineMods ++ coderMods)
          mods = Prelude.filter
            (\m -> unNamespace (moduleNamespace m) `notElem` coderNsSet)
            allExtMods
      loadEnd4 <- getCurrentTime
      putStrLn $ "  Loaded " ++ show (length mods) ++ " ext modules."
      putStrLn $ "  Time: " ++ formatTime (elapsed loadEnd4 loadStart4)
      putStrLn ""
      return mods
    else return []

  -- Apply filters
  let allMods = baselineMods ++ coderMods ++ extMods ++ dslMods
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
      -- Decoder/encoder synthesis runs over a curated subset of loaded
      -- type modules. Kernel type modules produce the Sources.Decode.*
      -- and Sources.Encode.* coder-package meta-sources. hydra-pg's
      -- model and mapping modules produce the pg meta-sources that were
      -- historically generated by update-ext-sources.
      let pgSynthNs = ["hydra.pg.model", "hydra.pg.mapping"]
      let isSynthInput m =
            let nsStr = unNamespace (moduleNamespace m)
                isCoder = any (\(pfx, _) -> pfx `isPrefixOf` nsStr) packagePrefixes
                isYaml  = "hydra.yaml." `isPrefixOf` nsStr
                isPgSynthInput = nsStr `elem` pgSynthNs
                hasType = any isNativeType (moduleBindings m)
            in hasType && (not isCoder || isPgSynthInput) && not isYaml
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

  -- When --ext-only is used, load the ext demo packages (hydra-pg, hydra-rdf)
  -- and generate only those, using allMainMods plus the loaded ext demo
  -- modules as the universe for type resolution.
  (modsToGenerate, allModsFinal) <- if optExtOnly opts
    then do
      putStrLn "Loading ext demo packages from JSON..."
      extMods <- fmap concat $ CM.forM extDemoPackages loadPackageMain
      -- Filter out any namespace already present in the baseline/coder set
      -- so that downstream generation doesn't see duplicates.
      let loadedNsSet = fmap (unNamespace . moduleNamespace) allMainMods
          extMods' = Prelude.filter
            (\m -> unNamespace (moduleNamespace m) `notElem` loadedNsSet)
            extMods
      putStrLn $ "  Loaded " ++ show (length extMods') ++ " ext demo modules"
      putStrLn ""
      return (extMods', allMainMods ++ extMods')
    else return (allMainMods, allMainMods)

  -- Layer 1 per-package scoping: if --package <pkg> is set, narrow
  -- modsToGenerate to modules owned by that package (per namespaceToPackage).
  -- The universe (allModsFinal) is unchanged, so type references across
  -- packages still resolve. Applies AFTER all other filters.
  modsToGenerateScoped <- case optPackage opts of
    Nothing  -> return modsToGenerate
    Just pkg -> do
      let owned = Prelude.filter
            (\m -> namespaceToPackage (moduleNamespace m) == pkg)
            modsToGenerate
      putStrLn $ "Scoping to package " ++ pkg ++ ": "
        ++ show (length owned) ++ " of " ++ show (length modsToGenerate) ++ " modules"
      putStrLn ""
      return owned

  -- Prepend synthesized source modules to modsToGenerate (deduping by namespace
  -- to keep ordering stable). They go into the same universe as the main modules.
  let modsToGenerate' = modsToGenerateScoped ++ synthesizedSourceMods
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
  -- Expand a set of target modules to the transitive closure over their
  -- declared term and type dependencies, within the loaded universe.
  -- generateSourceFiles's internal schemaMods walk needs this expansion to
  -- resolve cross-module references when generating a subset of the
  -- universe; the DSL-direct path gets it for free by generating the full
  -- ext bag.
  let expandForSchemaContext :: [Module] -> [Module]
      expandForSchemaContext mods =
        let nsMap = Prelude.foldr
              (\m acc -> (unNamespace (moduleNamespace m), m) : acc)
              []
              allModsFinal'
            lookupMod nsv = lookup nsv nsMap
            stepDeps m =
              (fmap unNamespace (moduleTermDependencies m))
              ++ (fmap unNamespace (moduleTypeDependencies m))
            closeNs :: [String] -> [String] -> [String]
            closeNs visited [] = visited
            closeNs visited (n : rest)
              | n `elem` visited = closeNs visited rest
              | otherwise = case lookupMod n of
                  Nothing -> closeNs (n : visited) rest
                  Just m  -> closeNs (n : visited) (stepDeps m ++ rest)
            seed = fmap (unNamespace . moduleNamespace) mods
            closed = closeNs [] seed
        in Prelude.filter
             (\m -> unNamespace (moduleNamespace m) `elem` closed)
             allModsFinal'

  -- Helper to filter the generated output down to just the originally-
  -- requested modules, given the expanded set passed to generateSources.
  -- Called when we needed the expansion for schema context but only want
  -- files for the original scope on disk.
  let pruneExtraFiles :: FilePath -> [Module] -> [Module] -> IO ()
      pruneExtraFiles dir wanted expanded = do
        let wantedNs = fmap (unNamespace . moduleNamespace) wanted
            extra = Prelude.filter
              (\m -> unNamespace (moduleNamespace m) `notElem` wantedNs)
              expanded
            suffix = case target of
              "haskell" -> "hs"
              "java" -> "java"
              "python" -> "py"
              "scala" -> "scala"
              "clojure" -> "clj"
              "scheme" -> "scm"
              "common-lisp" -> "lisp"
              "emacs-lisp" -> "el"
              _ -> ""
        CM.forM_ extra $ \m -> do
          -- Files are laid out per target-language convention under dir.
          -- For Haskell: Hydra/Xxx/Yyy.hs from namespace hydra.xxx.yyy.
          -- The namespace-to-path is handled by the coder; we just unlink
          -- everything that shouldn't be present.
          let nsStr = unNamespace (moduleNamespace m)
              parts = LS.splitOn "." nsStr
              -- Haskell-style path: first letter of each part uppercased.
              capitalize (c:cs) = C.toUpper c : cs
              capitalize []     = []
              relPath = case target of
                "haskell" -> FP.joinPath (fmap capitalize parts) ++ "." ++ suffix
                _         -> FP.joinPath parts ++ "." ++ suffix
              fullPath = dir FP.</> relPath
          exists <- SD.doesFileExist fullPath
          CM.when exists $ SD.removeFile fullPath

  -- Only expand to transitive deps + prune when the caller scoped to a
  -- single package via --package. In that mode the transitive closure
  -- is needed for generateSourceFiles's internal schemaMods walk, and
  -- we prune the stray output files afterward.
  -- When --package is NOT set, the caller gave us the exact set to
  -- generate; expansion + prune would add stray output in the wrong
  -- paths (prune uses Haskell conventions and can't clean up Java/etc.).
  let useExpansion = case optPackage opts of
        Just _  -> True
        Nothing -> False

  let genForDir :: FilePath -> [Module] -> IO Int
      genForDir dir mods =
        let modsForGen = if useExpansion then expandForSchemaContext mods else mods
            gen ms = case target of
              "haskell" -> generateSources moduleToHaskell haskellLanguage False False False False dir allModsFinal' ms
              "java"    -> generateSources moduleToJava    javaLanguage    False True False True   dir allModsFinal' ms
              "python"  -> generateSources moduleToPython  pythonLanguage  False True True False   dir allModsFinal' ms
              "scala"   -> generateSources moduleToScala   scalaLanguage   False True False False  dir allModsFinal' ms
              _ | Just g <- lispGenerator ->
                    generateSources g lispLanguage True False False False dir allModsFinal' ms
              _ -> do
                putStrLn $ "Unknown target: " ++ target
                exitFailure
        in do
          count <- gen modsForGen
          if useExpansion
            then do
              pruneExtraFiles dir mods modsForGen
              countFiles dir ("." ++ case target of
                "haskell" -> "hs"
                "java" -> "java"
                "python" -> "py"
                "scala" -> "scala"
                "clojure" -> "clj"
                "scheme" -> "scm"
                "common-lisp" -> "lisp"
                "emacs-lisp" -> "el"
                _ -> "")
            else return count

  -- Partition modules by owning package and generate each group to its own dir.
  -- Routing via PackageRouting.groupByPackage is unconditional: every module
  -- lands at <output>/<pkg>/src/main/<lang>/... based on its namespace.
  mainFileCount <- do
    let groups = groupByPackage modsToGenerate'
    counts <- CM.forM groups $ \(pkg, pkgMods) -> do
      let dir = packageOutMain pkg
      putStrLn $ "  " ++ pkg ++ ": " ++ show (length pkgMods) ++ " modules → " ++ dir
      genForDir dir pkgMods
    return (sum counts)
  genEnd <- getCurrentTime

  putStrLn $ "  Generated " ++ show mainFileCount ++ " files."
  putStrLn $ "  Time: " ++ formatTime (elapsed genEnd genStart)
  putStrLn ""

  -- Optionally generate test modules
  testFileCount <- if optIncludeTests opts
    then do
      putStrLn "Loading test modules from JSON..."
      testNamespaces <- readManifestField kernelJsonDir "testModules"
      testModsAll <- loadModulesFromJson testJsonDir kernelModules testNamespaces
      putStrLn $ "  Loaded " ++ show (length testModsAll) ++ " test modules"

      -- Layer 1 per-package scoping for tests: if --package <pkg> is set,
      -- narrow testMods to modules owned by that package. The universe is
      -- unchanged so cross-package refs still resolve.
      let testMods = case optPackage opts of
            Nothing  -> testModsAll
            Just pkg ->
              Prelude.filter
                (\m -> namespaceToPackage (moduleNamespace m) == pkg)
                testModsAll
      case optPackage opts of
        Just pkg | length testMods /= length testModsAll ->
          putStrLn $ "  Scoping to package " ++ pkg ++ ": "
            ++ show (length testMods) ++ " of " ++ show (length testModsAll) ++ " test modules"
        _ -> return ()
      putStrLn ""

      let allUniverse = allMods ++ testModsAll

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
            "haskell" -> generateSources moduleToHaskell haskellLanguage False False False False outMain allUniverse extModsForTests >> return ()
            "java"    -> generateSources     moduleToJava    javaLanguage    False True False True   outMain allUniverse extModsForTests >> return ()
            "python"  -> generateSources moduleToPython  pythonLanguage  False True True False   outMain allUniverse extModsForTests >> return ()
            _ | Just gen <- lispGenerator -> generateSources gen lispLanguage False False False False outMain allUniverse extModsForTests >> return ()
            _ -> return ()
          putStrLn ""

      putStrLn $ "Mapping test modules to " ++ targetCap ++ "..."

      -- Dispatch helper for the test source set.
      let genTestForDir :: FilePath -> [Module] -> IO Int
          genTestForDir dir mods = case target of
            "haskell" -> generateSources moduleToHaskell haskellLanguage False False False False dir allUniverse mods
            "java"    -> generateSources moduleToJava    javaLanguage    False True False True   dir allUniverse mods
            "python"  -> generateSources moduleToPython  pythonLanguage  False True True False   dir allUniverse mods
            "scala"   -> generateSources moduleToScala   scalaLanguage   False True False False  dir allUniverse mods
            _ | Just gen <- lispGenerator -> generateSources gen lispLanguage False False False False dir allUniverse mods
            _ -> return 0

      testStart <- getCurrentTime
      count <- do
        let groups = groupByPackage testMods
        counts <- CM.forM groups $ \(pkg, pkgMods) -> do
          let dir = packageOutTest pkg
          putStrLn $ "  " ++ pkg ++ ": " ++ show (length pkgMods) ++ " test modules → " ++ dir
          genTestForDir dir pkgMods
        return (sum counts)
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

