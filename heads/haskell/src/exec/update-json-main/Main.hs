-- | Export all Hydra DSL modules to JSON, fanned out across per-package
-- directories under dist/json/.
--
-- Replaces the older update-json-main + update-json-ext split. Every module
-- in the universe is routed to its owning package's dist/json/<pkg>/src/main/json/
-- directory via the routing table in Hydra.PackageRouting.
--
-- Universe (deduped by namespace before writing):
--   * hydra-kernel: mainModules + defaultLibModules + dslSourceModules
--   * hydra-haskell: haskellModules (already in mainModules; routed here)
--   * coder packages: hydraJavaModules, hydraPythonModules, hydraScalaModules,
--     hydraLispModules
--   * hydra-pg: hydraPgModules + pg decode/encode meta-sources + GenPGTransform
--   * hydra-rdf: hydraRdfModules
--
-- DSL wrapper modules (hydra.dsl.*) are generated in a second pass with the
-- same routing.

module Main where

import Hydra.Generation (writeModulesJsonPackageSplit, writeDslJsonPackageSplit, modulesToGraph)
import Hydra.PackageRouting (defaultDistJsonRoot)
import Hydra.Sources.Ext (
  mainModules, dslSourceModules, kernelModules, haskellModules, jsonModules, otherModules,
  hydraCoqModules, hydraGoModules, hydraJavaModules, hydraJavaScriptModules,
  hydraPythonModules, hydraScalaModules, hydraLispModules,
  hydraPgModules, hydraRdfModules, hydraWasmModules,
  hydraExtPackageModules,
  hydraExtDecodingModules, hydraExtEncodingModules)
import Hydra.Sources.Kernel.Lib.Defaults.All (defaultLibModules)

import qualified Hydra.Kernel as Kernel
import qualified Hydra.Core as Core
import qualified Hydra.Validate.Packaging as ValidatePackaging
import qualified Hydra.Validate.Core as ValidateCore
import qualified Hydra.Show.Error.Packaging as ShowErrorPackaging
import qualified Hydra.Show.Error.Core as ShowErrorCore
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Sources.Demos.GenPG.Transform as GenPGTransform

import Control.Exception (catch, SomeException)
import Control.Monad (when)
import qualified Data.List as L
import qualified Data.Set as S
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)


-- | Deduplicate a list of modules by namespace, keeping the first occurrence.
dedupByNamespace :: [Kernel.Module] -> [Kernel.Module]
dedupByNamespace = go S.empty
  where
    go _    []     = []
    go seen (m:ms)
      | ns `S.member` seen = go seen ms
      | otherwise          = m : go (S.insert ns seen) ms
      where ns = Kernel.moduleNamespace m

main :: IO ()
main = do
  distRoot <- parseDistRoot defaultDistJsonRoot
  includeJavaPython <- parseIncludeJavaPython

  let universe = dedupByNamespace $ L.concat
        [ mainModules
        , defaultLibModules
        , dslSourceModules
        , hydraCoqModules
        , hydraGoModules
        , hydraJavaModules
        , hydraJavaScriptModules
        , hydraPythonModules
        , hydraScalaModules
        , hydraLispModules
        , hydraPgModules
        , hydraRdfModules
        , hydraWasmModules
        , hydraExtPackageModules
        , hydraExtDecodingModules
        , hydraExtEncodingModules
        , [GenPGTransform.module_]
        ]

  putStrLn "=== Generate Hydra JSON modules ==="
  putStrLn ""

  -- Normative kernel-module validation. Runs BEFORE inference:
  --   * hydra.validate.packaging.kernelModule -- structural module checks
  --   * hydra.validate.core.type_   -- per-TypeDefinition type-tree checks
  --   * hydra.validate.core.term    -- per-TermDefinition term-tree checks
  -- For term-level checks we use the same graph the inferencer would
  -- build (modulesToGraph kernelModules kernelModules), so no extra
  -- graph construction is required beyond what inference already does.
  validateKernelModulesOrExit kernelModules

  -- Native hosts own the DSL→JSON path for hydra-java and hydra-python (#344).
  -- Their canonical hydra.java.* / hydra.python.* JSON is produced by
  -- bin/generate-hydra-{java,python}-from-{java,python}.sh (Phase 0 of sync.sh).
  -- We still pull in hydraJavaModules / hydraPythonModules above so they
  -- participate in the inference universe and in DSL-wrapper synthesis below,
  -- but we exclude their term-level modules from this write pass.
  --
  -- Legacy: the Haskell DSL copies at packages/hydra-{java,python}/src/main/haskell/
  -- remain as a historical reference through 0.15 but no longer drive
  -- dist/json/hydra-{java,python}/. To be deleted before 0.16.
  let isNativeOwned m =
        let ns = Packaging.unNamespace (Kernel.moduleNamespace m)
        in L.isPrefixOf "hydra.java." ns || L.isPrefixOf "hydra.python." ns
      writeUniverse
        | includeJavaPython = universe
        | otherwise         = filter (not . isNativeOwned) universe
      excluded = length universe - length writeUniverse

  putStrLn $ "Generating " ++ show (length writeUniverse) ++ " modules to JSON, routed per package..."
  when (excluded > 0) $
    putStrLn $ "  (excluded " ++ show excluded
      ++ " hydra.java.*/hydra.python.* modules — owned by native generators; see #344)"
  putStrLn $ "dist-json root: " ++ distRoot
  putStrLn ""

  result <- catch
    (writeModulesJsonPackageSplit True distRoot universe writeUniverse >> return True)
    (\e -> do
      putStrLn $ "Error: " ++ show (e :: SomeException)
      return False)

  putStrLn ""
  putStrLn "Generating DSL wrapper modules to JSON..."
  -- The DSL generator runs over the kernel-side type universe (kernel + json +
  -- other + haskell coder). This matches the old behavior of update-json-main:
  -- DSL modules are produced for every type in that universe.
  -- Extended per #358: include all coder-package type modules so
  -- DSL synthesis covers their syntax/environment types too.
  let dslInputMods = kernelModules ++ jsonModules ++ otherModules
        ++ haskellModules ++ hydraJavaModules ++ hydraPythonModules
        ++ hydraScalaModules ++ hydraLispModules
  dslResult <- catch
    (writeDslJsonPackageSplit distRoot universe dslInputMods >> return True)
    (\e -> do
      putStrLn $ "Error generating DSL JSON: " ++ show (e :: SomeException)
      return False)

  if result && dslResult
    then do
      putStrLn ""
      putStrLn "=== Done! ==="
    else do
      putStrLn ""
      putStrLn "=== FAILED ==="
      exitFailure

-- | Run packaging + core validation against every kernel module.
--
--   * hydra.validate.packaging.kernelModule -- structural rules on the
--     module (naming, ordering, namespaces, docs, etc.)
--   * hydra.validate.core.type_ -- per-TypeDefinition type-tree validity
--     (no empty annotations, no nested annotations, no duplicate fields,
--     etc.). Run with the type scheme's quantified variables as the
--     in-scope set, plus every kernel type-definition name.
--   * hydra.validate.core.term -- per-TermDefinition term-tree validity
--     (no empty term annotations, no duplicate let bindings, no shadowing,
--     no undefined term variables, etc.). Run with `typed = False` since
--     this validation precedes inference, and against the graph the
--     inferencer itself would build.
--
--   Reports all failures (per-module aggregated) and exits failure if any
--   module had any error.
validateKernelModulesOrExit :: [Kernel.Module] -> IO ()
validateKernelModulesOrExit mods = do
    putStrLn $ "Validating " ++ show (length mods)
               ++ " kernel modules (hydra.validate.packaging + hydra.validate.core)..."
    hFlush stdout
    let graph = modulesToGraph mods mods
    let pkgFailures  = [(m, e) | m <- mods, Just e <- [ValidatePackaging.kernelModule m]]
    let typeFailures = [(m, td, e) | m <- mods
                                  , td <- typeDefs m
                                  , Just e <- [validateTypeDef td]]
    let termFailures = [(m, td, e) | m <- mods
                                  , td <- termDefs m
                                  , Just e <- [validateTermDef graph td]]
    let pkgN  = length pkgFailures
    let typeN = length typeFailures
    let termN = length termFailures
    if pkgN == 0 && typeN == 0 && termN == 0
      then do
        putStrLn $ "  All " ++ show (length mods) ++ " kernel modules valid."
        putStrLn ""
      else do
        putStrLn $ "  " ++ show pkgN ++ " packaging failure(s), "
                       ++ show typeN ++ " core-type failure(s), "
                       ++ show termN ++ " core-term failure(s):"
        mapM_ reportPkgFailure  pkgFailures
        mapM_ reportTypeFailure typeFailures
        mapM_ reportTermFailure termFailures
        putStrLn ""
        putStrLn "=== FAILED: kernel module validation ==="
        exitFailure
  where
    -- Set of every type-definition name across all kernel modules. Used as
    -- the "in-scope" vocabulary when validating individual types: nominal
    -- references like `hydra.core.Name` look like TypeVariable nodes to
    -- Validate.Core.type_, which would otherwise flag them as "undefined".
    -- Forall-bound names are added on top per-definition.
    kernelTypeNames = S.fromList
      [Packaging.typeDefinitionName td
        | m <- mods
        , Packaging.DefinitionType td <- Packaging.moduleDefinitions m]
    -- Type definitions exempt from validation. UntypedLambdaError is
    -- currently `T.record []` and should migrate to `T.wrap T.unit`, but
    -- the change ripples through encode/decode/DSL helpers + every host
    -- language and is sized as its own change. Tracked separately.
    exemptTypeNames = S.fromList
      [Core.Name "hydra.error.checking.UntypedLambdaError"]
    -- Term definitions exempt from validation. etaExpandTypedTerm contains
    -- a deliberate `Logic.ifElse false ...` inference hack that lets the
    -- inferencer assign `list<Type>` to an otherwise-typeless empty list;
    -- excising it causes bootstrap-from-json (Java target) to fail with
    -- "expected list<hydra.core.Type> but found list<unit>". Validate.Core's
    -- constant-condition rule flags it but the hack is load-bearing; see
    -- the comment in Sources/Kernel/Terms/Reduction.hs.
    exemptTermNames = S.fromList
      [Core.Name "hydra.reduction.etaExpandTypedTerm"]
    typeDefs m =
      [td | Packaging.DefinitionType td <- Packaging.moduleDefinitions m
          , not (S.member (Packaging.typeDefinitionName td) exemptTypeNames)]
    termDefs m =
      [td | Packaging.DefinitionTerm td <- Packaging.moduleDefinitions m
          , not (S.member (Packaging.termDefinitionName td) exemptTermNames)]
    -- Empty ValidationResult, used as the starting accumulator for the
    -- profile-aware validators.
    emptyResult :: Kernel.ValidationResult e
    emptyResult = Kernel.ValidationResult [] []
    validateTypeDef td =
      let Core.TypeScheme vs body _ = Packaging.typeDefinitionTypeScheme td
          inScope = S.union kernelTypeNames (S.fromList vs)
          vr = ValidateCore.type_ ValidateCore.kernelDefaultCoreProfile emptyResult inScope body
      in case Kernel.validationResultErrors vr of
           []    -> Nothing
           err:_ -> Just err
    -- typed = False: pre-inference, lambdas have no domain annotations and
    -- term variables don't yet have known types, so type-variable-binding
    -- checks (System F mode) would fire spuriously.
    validateTermDef graph td =
      let vr = ValidateCore.term ValidateCore.kernelDefaultCoreProfile False graph (Packaging.termDefinitionTerm td)
      in case Kernel.validationResultErrors vr of
           []    -> Nothing
           err:_ -> Just err
    reportPkgFailure (m, err) = do
      putStrLn $ "  [packaging] " ++ Packaging.unNamespace (Kernel.moduleNamespace m)
                       ++ ": " ++ ShowErrorPackaging.invalidModuleError err
    reportTypeFailure (m, td, err) = do
      putStrLn $ "  [core/type] " ++ Packaging.unNamespace (Kernel.moduleNamespace m)
                       ++ "." ++ Core.unName (Packaging.typeDefinitionName td)
                       ++ ": " ++ ShowErrorCore.invalidTypeError err
    reportTermFailure (m, td, err) = do
      putStrLn $ "  [core/term] " ++ Packaging.unNamespace (Kernel.moduleNamespace m)
                       ++ "." ++ Core.unName (Packaging.termDefinitionName td)
                       ++ ": " ++ ShowErrorCore.invalidTermError err

-- | Parse an optional --dist-root argument. Retains the older --output-dir
-- flag name for call-site compatibility (it is interpreted as a dist-json
-- root now).
parseDistRoot :: String -> IO String
parseDistRoot defaultRoot = do
  args <- getArgs
  return $ go args
  where
    go ("--dist-root" : dir : _)  = dir
    go ("--output-dir" : dir : _) = dir
    go (_ : rest)                 = go rest
    go []                         = defaultRoot

-- | Parse --include-java-python (default False). When set, the legacy
-- Haskell DSL also writes hydra.java.*/hydra.python.* JSON (cold-start
-- bootstrap path). See #344.
parseIncludeJavaPython :: IO Bool
parseIncludeJavaPython = do
  args <- getArgs
  return $ "--include-java-python" `elem` args
