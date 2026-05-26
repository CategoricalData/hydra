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
--   dist/json/hydra-kernel/src/main/json/  — kernel, default lib, and other modules
--   dist/json/hydra-kernel/src/test/json/  — test modules
--   hydra-ext/../../dist/json/hydra-ext/src/main/json/      — ext coder modules (Java/Python coders)
--
-- Usage:
--   bootstrap-from-json --target <haskell|java|python|scala|go|clojure|scheme|common-lisp|emacs-lisp> [OPTIONS]
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
import qualified Hydra.TargetFilePaths as TargetFilePaths
import qualified Hydra.Digest as Digest
import Hydra.Sources.All (kernelModules)
import Hydra.ExtGeneration (moduleToLispDialect, wrapLongScalaText, generateSourcesWithTransform)
import Hydra.Haskell.Coder (moduleToHaskell)
import Hydra.Haskell.Language (haskellLanguage)
import Hydra.Go.Coder (moduleToGo, goLanguage)
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
import Hydra.Sources.Test.All (testSkipEmitNamespaces)

import Control.Exception (catch, IOException)
import Control.Monad (when, forM)
import qualified Control.Monad as CM
import qualified Data.Char as C
import Data.IORef (IORef, newIORef, modifyIORef', readIORef)
import Data.List (isPrefixOf, partition)
import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.Map as M
import qualified Data.Maybe as Y
import qualified Data.Set as S
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime)
import System.Directory (listDirectory, doesFileExist)
import qualified System.Directory as SD
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import qualified System.FilePath as FP


-- | True if the module contains at least one type definition.
moduleHasTypeDefinition :: Module -> Bool
moduleHasTypeDefinition m = any isType (moduleDefinitions m)
  where
    isType (DefinitionType _) = True
    isType _ = False

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
  , optKernelOnly         :: Bool
  , optTypesOnly          :: Bool
  , optExtOnly            :: Bool
  , optSynthesizeSources  :: Bool
  , optDistJsonRoot       :: Maybe FilePath
  , optPackage            :: Maybe String  -- Layer 1: narrow generation to one package
  , optAllPackages        :: Bool          -- Batch mode: generate every package in one run
  , optPruneStale         :: Bool          -- #357: delete files in per-package output dirs not just-written
  , optKeepPathsFiles     :: [FilePath]    -- #357: each file lists "<sourceSetDir>\t<relPath>" pairs to keep
  }

defaultOptions :: Options
defaultOptions = Options
  { optTarget             = ""
  , optOutput             = Nothing
  , optIncludeCoders      = False
  , optIncludeDsls        = False
  , optIncludeTests       = False
  , optKernelOnly         = False
  , optTypesOnly          = False
  , optExtOnly            = False
  , optSynthesizeSources  = False
  , optDistJsonRoot       = Nothing
  , optPackage            = Nothing
  , optAllPackages        = False
  , optPruneStale         = False
  , optKeepPathsFiles     = []
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
    go opts ("--kernel-only" : rest) = go (opts { optKernelOnly = True }) rest
    go opts ("--types-only" : rest) = go (opts { optTypesOnly = True }) rest
    go opts ("--ext-only" : rest) = go (opts { optExtOnly = True }) rest
    go opts ("--synthesize-sources" : rest) = go (opts { optSynthesizeSources = True }) rest
    go opts ("--dist-json-root" : d : rest) = go (opts { optDistJsonRoot = Just d }) rest
    go opts ("--package" : p : rest) = go (opts { optPackage = Just p }) rest
    go opts ("--all-packages" : rest) = go (opts { optAllPackages = True }) rest
    go opts ("--prune-stale" : rest) = go (opts { optPruneStale = True }) rest
    go opts ("--keep-paths-from" : f : rest) =
      go (opts { optKeepPathsFiles = optKeepPathsFiles opts ++ [f] }) rest
    go _ (arg : _) = Left $ "Unknown argument: " ++ arg

usage :: String
usage = unlines
  [ "Usage: bootstrap-from-json --target <haskell|java|python|scala|go|clojure|scheme|common-lisp|emacs-lisp> [OPTIONS]"
  , ""
  , "Options:"
  , "  --output <dir>           Output base directory"
  , "  --include-coders         Also load coder packages (hydra-java/python/scala/lisp)"
  , "  --include-dsls           Also load DSL wrapper modules"
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
  , "  --prune-stale            After generation, delete files in each per-package"
  , "                           output dir that are not in the owned-module path set."
  , "                           Removes stale outputs left by deleted/renamed source"
  , "                           modules. See issue #357. Compatible with Stage 7"
  , "                           freshness filtering — the keep-set comes from"
  , "                           Hydra.TargetFilePaths.moduleFilePaths over the owned"
  , "                           module list, not from what was just written."
  , "  --keep-paths-from <file> Extend the prune keep-set from a manifest file. Each"
  , "                           line is '<sourceSetDir>\\t<relPath>'. Used to tell"
  , "                           the pruner about hand-written files (e.g. the"
  , "                           copy-kernel-runtime payload) that share the dir."
  , "                           May be passed multiple times."
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
        "go"          -> ".go"
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
        "go"          -> "../../dist/go/hydra-kernel"
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
  -- --include-coders. Ext demo packages are loaded with --ext-only. Other
  -- non-baseline non-coder packages (extPackages and extDemoPackages) are
  -- auto-loaded based on --package or --all-packages — see Step 2c.
  let coderPackages   = ["hydra-java", "hydra-python", "hydra-scala", "hydra-lisp", "hydra-go"]
  let extDemoPackages = ["hydra-pg", "hydra-rdf"]
  let extPackages     = ["hydra-coq", "hydra-typescript", "hydra-ext", "hydra-wasm", "hydra-bench"]

  let targetCap = case target of
        "haskell"     -> "Haskell"
        "java"        -> "Java"
        "python"      -> "Python"
        "scala"       -> "Scala"
        "go"          -> "Go"
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
  putStrLn ""

  -- Load a single package's mainModules + defaultLibModules from its per-package
  -- manifest. Returns the accumulated Modules; missing fields are treated as
  -- empty.
  let loadPackageMain :: String -> IO [Module]
      loadPackageMain pkg = do
        let pkgDir = pkgMainDir pkg
        mainNs <- readManifestFieldOrEmpty pkgDir "mainModules"
        defaultNs <- readManifestFieldOrEmpty pkgDir "defaultLibModules"
        let allNs = mainNs ++ defaultNs
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

  let allKernelNamespaces = fmap moduleName baselineMods

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
  -- Compute which non-baseline non-coder packages need to be loaded into
  -- the universe. Each of hydra-rdf, hydra-coq, hydra-typescript,
  -- hydra-ext, hydra-wasm is independent. hydra-pg depends on hydra-rdf
  -- (e.g. hydra.pg.rdf.environment references hydra.rdf.syntax.Iri), so
  -- loading hydra-pg implicitly loads hydra-rdf.
  --
  --   --package <p>  : load <p> + its package-level deps when it's an ext
  --                    or ext-demo package
  --   --ext-only     : load both ext-demo packages (legacy demo path)
  --   (otherwise)    : nothing extra. --all-packages alone does NOT auto-load
  --                    these — sync-haskell.sh uses --all-packages to regen
  --                    only baseline + dsl artifacts; the per-package
  --                    assemble-distribution.sh handles coder/ext packages
  --                    individually with --package <pkg>.
  let allExtPackages = extPackages ++ extDemoPackages
  let packageDeps p = case p of
        "hydra-pg"  -> ["hydra-pg", "hydra-rdf"]
        "hydra-ext" -> ["hydra-ext", "hydra-rdf"]
        _           -> [p]
  let extPackagesToLoad
        | optExtOnly opts                           = extDemoPackages
        | Just p <- optPackage opts, p `elem` allExtPackages = packageDeps p
        | otherwise                                 = []

  dslMods <- if optIncludeDsls opts
    then do
      putStrLn "Step 2b: Loading DSL wrapper modules from JSON..."
      loadStart3 <- getCurrentTime
      let dslPackages =
            ["hydra-kernel", "hydra-haskell"]
              ++ (if optIncludeCoders opts then coderPackages else [])
              ++ extPackagesToLoad
      mods <- fmap concat $ CM.forM dslPackages loadPackageDsl
      loadEnd3 <- getCurrentTime
      putStrLn $ "  Loaded " ++ show (length mods) ++ " DSL modules."
      putStrLn $ "  Time: " ++ formatTime (elapsed loadEnd3 loadStart3)
      putStrLn ""
      return mods
    else return []

  -- Step 2c: Load main modules for any ext / ext-demo packages selected above.
  -- Already-loaded namespaces (via --include-coders) are skipped to avoid
  -- duplicates.
  extMods <- if Prelude.null extPackagesToLoad
    then return []
    else do
      putStrLn "Step 2c: Loading ext package modules from JSON..."
      loadStart4 <- getCurrentTime
      allExtMods <- fmap concat $ CM.forM extPackagesToLoad loadPackageMain
      let coderNsSet = fmap (unModuleName . moduleName) (baselineMods ++ coderMods)
          mods = Prelude.filter
            (\m -> unModuleName (moduleName m) `notElem` coderNsSet)
            allExtMods
      loadEnd4 <- getCurrentTime
      putStrLn $ "  Loaded " ++ show (length mods) ++ " ext modules."
      putStrLn $ "  Time: " ++ formatTime (elapsed loadEnd4 loadStart4)
      putStrLn ""
      return mods

  -- Apply filters
  let allMods = baselineMods ++ coderMods ++ extMods ++ dslMods
  let kernelNsStrings = fmap unModuleName allKernelNamespaces
  let filtered1 = if optKernelOnly opts
        then Prelude.filter (\m -> unModuleName (moduleName m) `elem` kernelNsStrings) allMods
        else allMods
  let filtered2 = if optTypesOnly opts
        then Prelude.filter moduleHasTypeDefinition filtered1
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
      -- Synth is meaningful only for namespaces the hydra-kernel manifest
      -- claims, plus the two hydra-pg type modules (historical coverage
      -- of update-ext-sources). Long-tail ext types (hydra.xml.schema,
      -- hydra.avro.*, ...) produce synth output whose references can't
      -- be resolved in the generator's lexical env during batch-mode
      -- iteration.
      let kernelNsList = fmap unModuleName allKernelNamespaces
      let isSynthInput m =
            let nsStr = unModuleName (moduleName m)
                isCoder = any (\(pfx, _) -> pfx `isPrefixOf` nsStr) packagePrefixes
                isYaml  = "hydra.yaml." `isPrefixOf` nsStr
                isPgSynthInput = nsStr `elem` pgSynthNs
                hasType = moduleHasTypeDefinition m
                isKernel = (nsStr `elem` kernelNsList) && not isCoder && not isYaml
            in hasType && (isKernel || isPgSynthInput)
      let typeMods = Prelude.filter isSynthInput allMainMods
      putStrLn $ "Synthesizing decoder/encoder source modules from "
        ++ show (length typeMods) ++ " type modules..."
      decSrc <- generateDecoderSourceModules allMainMods typeMods
      encSrc <- generateEncoderSourceModules allMainMods typeMods
      putStrLn $ "  Synthesized " ++ show (length decSrc) ++ " decoder source modules"
      putStrLn $ "  Synthesized " ++ show (length encSrc) ++ " encoder source modules"
      -- Synthesized Source modules each contain one `module_` binding whose
      -- TermDefinition is annotated with TypeScheme [] (TypeVariable _Module),
      -- so the type is statically known. Skip the full-universe inference
      -- pass that historically ran here — it consumed multi-GB of memory on
      -- CI runners while solving for a type the synthesizer already provides.
      let synthesized = decSrc ++ encSrc
      putStrLn $ "  Synthesized modules carry static TypeScheme; skipping inference."
      putStrLn ""
      return synthesized
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
      let loadedNsSet = fmap (unModuleName . moduleName) allMainMods
          extMods' = Prelude.filter
            (\m -> unModuleName (moduleName m) `notElem` loadedNsSet)
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
            (\m -> namespaceToPackage (moduleName m) == pkg)
            modsToGenerate
      putStrLn $ "Scoping to package " ++ pkg ++ ": "
        ++ show (length owned) ++ " of " ++ show (length modsToGenerate) ++ " modules"
      putStrLn ""
      return owned

  -- Stage 7: per-module checksum skip. For each module M whose DSL source
  -- hash matches the hash recorded in the per-target digest at
  -- <outBase>/<owning-pkg>/digest.json's inputs section, exclude M from
  -- modsToGenerate. The output already on disk reflects exactly the same
  -- DSL source content, so regeneration would produce a byte-identical
  -- result. Excluding before generation skips inference work entirely.
  --
  -- Only applied when --package is set (scoped mode). In unscoped mode
  -- the per-module digest semantics get harder to reason about because
  -- multiple packages share the same outBase tree.
  -- Source set indicator: tests are generated when --include-tests is set
  -- AND we're scoped to a single package. (In --all-packages mode, tests
  -- are generated separately via testMods.)
  let sourceSetForFilter = if optIncludeTests opts then "test" else "main"

  -- Stage 7 freshness filtering stays active with or without --prune-stale
  -- (#357). The prune step's keep-set is derived from the *owned* module
  -- set (moduleFilePaths over modsToGenerateScoped), so Stage-7-skipped
  -- modules' files are protected without re-running the coder for them.
  modsToGenerateScopedFiltered <- case optPackage opts of
    Just pkg -> filterByTargetDigest outBase pkg sourceSetForFilter modsToGenerateScoped
    Nothing  -> return modsToGenerateScoped

  -- Prepend synthesized source modules to modsToGenerate (deduping by namespace
  -- to keep ordering stable). They go into the same universe as the main modules.
  let modsToGenerate' = modsToGenerateScopedFiltered ++ synthesizedSourceMods
  let allModsFinal'   = allModsFinal ++ synthesizedSourceMods

  -- Generate main modules
  let stepNum = if optIncludeCoders opts then "3" else "2"
  -- Stage 7: if every module was filtered out as fresh (and there's
  -- no synthesis to do), this is a no-op. The downstream loop over
  -- groupByPackage modsToGenerate' will naturally iterate zero times,
  -- but we log it for clarity.
  CM.when (Prelude.null modsToGenerate' && not (Prelude.null modsToGenerateScoped)) $ do
    putStrLn $ "Step " ++ stepNum ++ ": all "
      ++ show (length modsToGenerateScoped) ++ " main modules fresh; skipping generation."

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

  -- 'mods' is the set this scoped package wants written. The full
  -- universe is passed for typing context only; generateSourceFiles
  -- emits files only for the modsToGenerate argument (per the existing
  -- typeModulesToGenerate / termModulesToGenerate filters in
  -- Hydra.Codegen). No expansion, no prune.
  let genForDir :: FilePath -> [Module] -> IO [FilePath]
      genForDir dir mods = case target of
        "haskell"    -> generateSources moduleToHaskell    haskellLanguage    False False False False dir allModsFinal' mods
        "java"       -> generateSources moduleToJava       javaLanguage       False True  False True  dir allModsFinal' mods
        "python"     -> generateSources moduleToPython     pythonLanguage     False True  True  False dir allModsFinal' mods
        "scala"      -> generateSourcesWithTransform wrapLongScalaText moduleToScala scalaLanguage False True False False dir allModsFinal' mods
        "go"         -> generateSources moduleToGo  goLanguage         False False False False dir allModsFinal' mods
        "typescript" -> generateSources moduleToTypeScript typeScriptLanguage False True  True  False dir allModsFinal' mods
        _ | Just g <- lispGenerator ->
              generateSources g lispLanguage True False False False dir allModsFinal' mods
        _ -> do
          putStrLn $ "Unknown target: " ++ target
          exitFailure

  -- Per-dir keep-set for the #357 prune step. Keys are absolute output
  -- dirs; values are relative paths joinable with the key. Populated
  -- from moduleFilePaths over the *owned* module set (BEFORE Stage 7
  -- filtering) so that fresh-skipped modules' files are protected
  -- without re-running their coder. Flat-output mode is deliberately
  -- not recorded — pruning a /tmp demo dir would be surprising.
  keepRef <- newIORef (M.empty :: M.Map FilePath (S.Set FilePath))
  let recordKeep dir paths = modifyIORef' keepRef $
        M.insertWith S.union dir (S.fromList paths)
  let recordOwnedPaths dir mods =
        recordKeep dir [p | m <- mods, p <- TargetFilePaths.moduleFilePaths target m]

  -- Populate the main-set keep-set from the OWNED modules (pre-filter),
  -- grouped by package. This runs before generation so the keep-set is
  -- exhaustive whether or not Stage 7 ends up skipping individual modules
  -- as fresh.
  --
  -- Exception: in per-package mode AND --include-tests is set, the caller
  -- is a test-only invocation (see assemble-distribution.sh: the test
  -- pass omits --include-dsls, so the loaded universe is a subset of
  -- what's on disk under the main dir). Pruning main against that subset
  -- would delete legitimate DSL files. Batch mode (--all-packages) is OK
  -- because it loads the full universe.
  let testOnlyInvocation =
        optIncludeTests opts && Y.isJust (optPackage opts) && not (optAllPackages opts)
  let ownedMainGroups = groupByPackage (modsToGenerateScoped ++ synthesizedSourceMods)
  CM.unless testOnlyInvocation $
    case (optPackage opts, optAllPackages opts) of
      (Just pkgArg, _) ->
        CM.forM_ ownedMainGroups $ \(pkg, pkgMods) ->
          CM.when (pkg == pkgArg) $ recordOwnedPaths (packageOutMain pkg) pkgMods
      (Nothing, True)  ->
        CM.forM_ ownedMainGroups $ \(pkg, pkgMods) ->
          recordOwnedPaths (packageOutMain pkg) pkgMods
      (Nothing, False) -> return ()  -- flat-output mode: no prune

  -- Partition modules by owning package and generate each group to its own dir.
  -- Routing via PackageRouting.groupByPackage is unconditional: every module
  -- lands at <output>/<pkg>/src/main/<lang>/... based on its namespace.
  --
  -- Three routing modes:
  --   --package <pkg>   : one package, per-package dir (scoped sync path)
  --   --all-packages    : every package, per-package dirs (batch sync path)
  --   (neither)         : flat <outBase>/src/main/<target>/ (demo path)
  mainFileCount <- case (optPackage opts, optAllPackages opts) of
    (Just pkgArg, _) -> do
      let groups = groupByPackage modsToGenerate'
      let scopedGroups = Prelude.filter (\(pkg, _) -> pkg == pkgArg) groups
      counts <- CM.forM scopedGroups $ \(pkg, pkgMods) -> do
        let dir = packageOutMain pkg
        putStrLn $ "  " ++ pkg ++ ": " ++ show (length pkgMods) ++ " modules → " ++ dir
        paths <- genForDir dir pkgMods
        return (length paths)
      return (sum counts)
    (Nothing, True) -> do
      let groups = groupByPackage modsToGenerate'
      counts <- CM.forM groups $ \(pkg, pkgMods) -> do
        let dir = packageOutMain pkg
        putStrLn $ "  " ++ pkg ++ ": " ++ show (length pkgMods) ++ " modules → " ++ dir
        paths <- genForDir dir pkgMods
        return (length paths)
      return (sum counts)
    (Nothing, False) -> do
      putStrLn $ "  " ++ show (length modsToGenerate') ++ " modules → " ++ outMain
      paths <- genForDir outMain modsToGenerate'
      return (length paths)
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
      --
      -- Additionally, filter out skip-emit namespaces (e.g. hydra.test.testEnv).
      -- These are type-only stubs whose hand-written per-language
      -- counterparts are the source of truth; emitting them would
      -- overwrite hand-written code.
      let notSkipEmit m = moduleName m `notElem` testSkipEmitNamespaces
      -- Package-scoped test modules including skip-emit ones — used to
      -- populate the prune keep-set (skip-emit files are hand-written and
      -- must survive prune). Generation itself filters skip-emit out via
      -- testMods below.
      let testModsForKeep = case optPackage opts of
            Nothing  -> testModsAll
            Just pkg ->
              Prelude.filter
                (\m -> namespaceToPackage (moduleName m) == pkg)
                testModsAll
      let testMods = Prelude.filter notSkipEmit testModsForKeep
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
        let testExtraDeps = Prelude.filter (\ns -> unModuleName ns `notElem` kernelNsStrings)
              $ fmap moduleDependencyModule $ concatMap moduleDependencies testMods
            extModsForTests = Prelude.filter (\m -> moduleName m `elem` testExtraDeps) allMods
        when (not (Prelude.null extModsForTests)) $ do
          putStrLn $ "Generating " ++ show (length extModsForTests) ++ " ext module(s) needed by tests..."
          case target of
            "haskell"    -> generateSources moduleToHaskell    haskellLanguage    False False False False outMain allUniverse extModsForTests >> return ()
            "java"       -> generateSources moduleToJava       javaLanguage       False True  False True  outMain allUniverse extModsForTests >> return ()
            "python"     -> generateSources moduleToPython     pythonLanguage     False True  True  False outMain allUniverse extModsForTests >> return ()
            "go"         -> generateSources moduleToGo  goLanguage         False False False False outMain allUniverse extModsForTests >> return ()
            "typescript" -> generateSources moduleToTypeScript typeScriptLanguage False True  True  False outMain allUniverse extModsForTests >> return ()
            _ | Just gen <- lispGenerator -> generateSources gen lispLanguage False False False False outMain allUniverse extModsForTests >> return ()
            _ -> return ()
          putStrLn ""

      putStrLn $ "Mapping test modules to " ++ targetCap ++ "..."

      -- Populate the test-set keep-set from the OWNED test modules.
      -- Use testModsForKeep (package-scoped, NOT skip-emit-filtered) so
      -- that hand-written skip-emit files (e.g. Hydra/Test/TestEnv.hs)
      -- are protected from prune. See the main-set keep-set population
      -- earlier for the Stage-7 rationale.
      let ownedTestGroups = groupByPackage testModsForKeep
      case (optPackage opts, optAllPackages opts) of
        (Just pkgArg, _) ->
          CM.forM_ ownedTestGroups $ \(pkg, pkgMods) ->
            CM.when (pkg == pkgArg) $ recordOwnedPaths (packageOutTest pkg) pkgMods
        (Nothing, True)  ->
          CM.forM_ ownedTestGroups $ \(pkg, pkgMods) ->
            recordOwnedPaths (packageOutTest pkg) pkgMods
        (Nothing, False) -> return ()

      -- Dispatch helper for the test source set.
      let genTestForDir :: FilePath -> [Module] -> IO [FilePath]
          genTestForDir dir mods = case target of
            "haskell"    -> generateSources moduleToHaskell    haskellLanguage    False False False False dir allUniverse mods
            "java"       -> generateSources moduleToJava       javaLanguage       False True  False True  dir allUniverse mods
            "python"     -> generateSources moduleToPython     pythonLanguage     False True  True  False dir allUniverse mods
            "scala"      -> generateSourcesWithTransform wrapLongScalaText moduleToScala scalaLanguage False True False False dir allUniverse mods
            "go"         -> generateSources moduleToGo  goLanguage         False False False False dir allUniverse mods
            "typescript" -> generateSources moduleToTypeScript typeScriptLanguage False True  True  False dir allUniverse mods
            _ | Just gen <- lispGenerator -> generateSources gen lispLanguage False False False False dir allUniverse mods
            _ -> return []

      testStart <- getCurrentTime
      count <- case (optPackage opts, optAllPackages opts) of
        (Just pkgArg, _) -> do
          let groups = groupByPackage testMods
          let scopedGroups = Prelude.filter (\(pkg, _) -> pkg == pkgArg) groups
          counts <- CM.forM scopedGroups $ \(pkg, pkgMods) -> do
            let dir = packageOutTest pkg
            putStrLn $ "  " ++ pkg ++ ": " ++ show (length pkgMods) ++ " test modules → " ++ dir
            paths <- genTestForDir dir pkgMods
            return (length paths)
          return (sum counts)
        (Nothing, True) -> do
          let groups = groupByPackage testMods
          counts <- CM.forM groups $ \(pkg, pkgMods) -> do
            let dir = packageOutTest pkg
            putStrLn $ "  " ++ pkg ++ ": " ++ show (length pkgMods) ++ " test modules → " ++ dir
            paths <- genTestForDir dir pkgMods
            return (length paths)
          return (sum counts)
        (Nothing, False) -> do
          putStrLn $ "  " ++ show (length testMods) ++ " test modules → " ++ outTest
          paths <- genTestForDir outTest testMods
          return (length paths)
      testEnd <- getCurrentTime

      putStrLn $ "  Generated " ++ show count ++ " test files."
      putStrLn $ "  Time: " ++ formatTime (elapsed testEnd testStart)
      putStrLn ""
      return count
    else return 0

  let genTestSuccess = True

  -- (Scala line-wrap moved into the generation pipeline as a content
  -- transform on each file before write. See generateSourcesWithTransform
  -- in Hydra.Generation; the scala dispatch in genForDir/genTestForDir
  -- above passes wrapLongScalaText.)

  -- #357: After all generation passes, prune each recorded output dir.
  -- A file is considered current iff it is in the just-written set for
  -- that dir OR in a keep-paths manifest the caller passed for that dir.
  -- Anything else (a leftover from a deleted/renamed source module) is
  -- deleted. Skips the digest.json sibling in case it sits inside the
  -- source set (defensive: in the current layout it does not).
  CM.when (optPruneStale opts) $ do
    owned <- readIORef keepRef
    extra <- readKeepPathsFiles (optKeepPathsFiles opts)
    -- We walk only dirs that 'owned' (the path-set from moduleFilePaths
    -- over the owned modules) claims responsibility for. The keep-paths
    -- manifest extends what counts as "kept" within those dirs (e.g.
    -- hand-written runtime files), but it does NOT add new dirs to the
    -- walk-set. Adding manifest-only dirs would let an external caller
    -- accidentally trigger a prune of a dir whose owned-paths weren't
    -- populated — e.g. a test-only invocation whose manifest mentions
    -- the main dir, in which case the manifest is the only entry for
    -- main and prune would delete every generated main file.
    let merged = M.mapWithKey (\dir ks -> S.union ks (M.findWithDefault S.empty dir extra)) owned
    if M.null merged
      then putStrLn "Prune: no per-package output dirs recorded; skipping."
      else do
        putStrLn ""
        putStrLn "Pruning stale outputs (#357)..."
        totalDeleted <- CM.foldM (\acc (dir, keep) -> do
            n <- pruneDir dir keep
            return (acc + n)
          ) 0 (M.toList merged)
        putStrLn $ "  Pruned " ++ show totalDeleted ++ " stale file(s)."
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

-- | Stage 7: per-module target-side freshness filter.
--
-- For each module M in the input set, check whether its current DSL
-- source hash matches the hash recorded in the per-target digest
-- (<outBase>/<owning-pkg>/digest.json's "inputs" section). If so, the
-- target output already on disk reflects exactly the same DSL content,
-- so M can be skipped (no inference, no generation, no write).
--
-- The recorded digest is in v2 format (digest-check refresh writes it
-- after a successful regen). The "inputs" map's keys are namespace
-- strings (e.g. "hydra.core"); values are SHA-256 hex of the DSL
-- source file as of the last successful regen.
--
-- Modules with no recorded entry are kept (treated as dirty).
-- Modules with no current DSL source (synth modules etc.) are also
-- kept — we can't verify staleness, so we don't risk a false skip.
--
-- The per-target digest is read from
-- <outBase>/<pkg>/build/<sourceSet>/digest.json, where outBase is e.g.
-- "../../dist/java" (the parent of the per-package target dirs) and
-- sourceSet is "main" or "test".
filterByTargetDigest :: FilePath -> String -> String -> [Module] -> IO [Module]
filterByTargetDigest outBase pkg sourceSet mods = do
  let digestPath = outBase FP.</> pkg FP.</> "build" FP.</> sourceSet FP.</> "digest.json"
  exists <- SD.doesFileExist digestPath
  if not exists
    then do
      putStrLn $ "  Per-module skip: no target digest at " ++ digestPath
        ++ "; keeping all " ++ show (length mods) ++ " modules"
      return mods
    else do
      stored <- Digest.readDigestV2 digestPath
      let recordedInputs = Digest.digestInputs stored
      if M.null recordedInputs
        then do
          putStrLn $ "  Per-module skip: target digest empty; keeping all "
            ++ show (length mods) ++ " modules"
          return mods
        else do
          -- Compute current DSL source hashes.
          nsFiles <- Digest.discoverNamespaceFiles
          currentDigest <- Digest.hashUniverse nsFiles mods
          let isFresh m =
                let nsStr = unModuleName (moduleName m)
                in case (M.lookup nsStr recordedInputs, M.lookup (ModuleName nsStr) currentDigest) of
                     (Just rec, Just cur) -> Digest.entryHash rec == cur
                     _                    -> False
              (fresh, dirty) = partition isFresh mods
          if Prelude.null fresh
            then do
              putStrLn $ "  Per-module skip: 0 fresh / "
                ++ show (length dirty) ++ " dirty; processing all"
              return mods
            else if Prelude.null dirty
              then do
                putStrLn $ "  Per-module skip: " ++ show (length fresh)
                  ++ " fresh / 0 dirty; nothing to generate"
                return []
              else do
                putStrLn $ "  Per-module skip: " ++ show (length fresh)
                  ++ " fresh / " ++ show (length dirty)
                  ++ " dirty; excluding fresh from generation"
                return dirty

-- | #357: Load keep-paths manifests. Each manifest file holds lines of
-- the form '<sourceSetDir>\t<relPath>'. Lines that are empty, start with
-- '#', or lack a tab are skipped. The resulting map merges all manifest
-- contents keyed by source-set dir.
readKeepPathsFiles :: [FilePath] -> IO (M.Map FilePath (S.Set FilePath))
readKeepPathsFiles paths = do
  pairs <- fmap concat $ CM.forM paths $ \p -> do
    exists <- SD.doesFileExist p
    if not exists
      then do
        putStrLn $ "Warning: --keep-paths-from file not found: " ++ p
        return []
      else do
        contents <- readFile p
        return $ Y.mapMaybe parseLine (lines contents)
  return $ M.fromListWith S.union [(d, S.singleton r) | (d, r) <- pairs]
  where
    parseLine s = case dropWhile (== ' ') s of
      ""         -> Nothing
      '#' : _    -> Nothing
      s'         -> case break (== '\t') s' of
                      (_, [])    -> Nothing  -- no tab
                      (d, _ : r) -> Just (d, r)

-- | #357: Walk 'dir' recursively and delete every regular file whose path
-- (relative to 'dir') is not in 'keep'. The keep set holds relative paths
-- (e.g. "Hydra/Adapt.hs"), matching what 'generateSources' returns. Empty
-- directories left behind by deleted files are also removed. Returns the
-- number of files deleted.
--
-- Safety: the caller chose 'dir' (it's an output dir we just wrote to),
-- so we don't second-guess scope here. We do skip the 'digest.json'
-- sibling at the source-set root if it happens to live inside the
-- walked dir (defensive — in the current layout digest.json sits one
-- level up from the source set, so this is belt-and-suspenders).
pruneDir :: FilePath -> S.Set FilePath -> IO Int
pruneDir dir keep = do
  exists <- SD.doesDirectoryExist dir
  if not exists
    then return 0
    else do
      onDisk <- listFilesRecursivePrune dir
      let stale = Prelude.filter (\rel -> not (S.member rel keep)) onDisk
      CM.forM_ stale $ \rel -> do
        let p = dir FP.</> rel
        putStrLn $ "  - " ++ p
        SD.removeFile p
      -- Best-effort: remove empty directories left behind.
      pruneEmptyDirs dir
      return (length stale)

-- | List regular files under 'dir', returning paths relative to 'dir'.
-- Skips a top-level 'digest.json' (cf. note in pruneDir).
listFilesRecursivePrune :: FilePath -> IO [FilePath]
listFilesRecursivePrune root = go ""
  where
    go relDir = do
      let absDir = if Prelude.null relDir then root else root FP.</> relDir
      entries <- listDirectory absDir `catch` \(_ :: IOException) -> return []
      fmap concat $ CM.forM entries $ \e -> do
        let relP = if Prelude.null relDir then e else relDir FP.</> e
            absP = absDir FP.</> e
        isFile <- doesFileExist absP
        if isFile
          then return [relP | not (Prelude.null relDir) || e /= "digest.json"]
          else do
            isDir <- SD.doesDirectoryExist absP
            if isDir then go relP else return []

-- | Remove any empty subdirectories (depth-first). 'dir' itself is left
-- alone; only its descendants are pruned.
pruneEmptyDirs :: FilePath -> IO ()
pruneEmptyDirs dir = do
  entries <- listDirectory dir `catch` \(_ :: IOException) -> return []
  CM.forM_ entries $ \e -> do
    let p = dir FP.</> e
    isDir <- SD.doesDirectoryExist p
    CM.when isDir $ do
      pruneEmptyDirs p
      children <- listDirectory p `catch` \(_ :: IOException) -> return []
      CM.when (Prelude.null children) $
        SD.removeDirectory p `catch` \(_ :: IOException) -> return ()
