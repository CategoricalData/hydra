-- | Export all Hydra DSL modules to JSON, fanned out across per-package
-- directories under dist/json/.
--
-- Replaces the older update-json-main + update-json-ext split. Every module
-- in the universe is routed to its owning package's dist/json/<pkg>/src/main/json/
-- directory via the routing table in Hydra.PackageRouting.
--
-- Universe (deduped by namespace before writing):
--   * hydra-kernel: mainModules + dslSourceModules
--   * hydra-haskell: haskellModules (already in mainModules; routed here)
--   * coder packages: hydraJavaModules, hydraPythonModules, hydraScalaModules,
--     hydraLispModules
--   * hydra-pg: hydraPgModules + pg decode/encode meta-sources + GenPGTransform
--   * hydra-rdf: hydraRdfModules
--
-- DSL wrapper modules (hydra.dsl.*) are generated in a second pass with the
-- same routing.

module Main where

import Hydra.Generation (writeModulesJsonPackageSplit, writeDerivedJsonPackageSplit, modulesToGraph,
  loadModulesFromJson, readManifestField, loadNativePackageModulesTagged,
  generateEncoderModules, generateDecoderModules,
  isDerivedModule, packagesFromRouting, validatePackagesStructural, validatePackagesSemantic,
  ValidationFindings(..), validationFindingsNull)
import Hydra.PackageRouting (defaultDistJsonRoot, buildRoutingMap, groupByPackageIn)
import Hydra.Sources.Ext (
  mainModules, dslSourceModules, kernelModules, haskellModules, jsonModules, otherModules,
  hydraBenchModules,
  hydraCoqModules, hydraGoModules, hydraJvmModules, hydraJavaModules, hydraTypeScriptModules,
  hydraPythonModules, hydraScalaModules, hydraLispModules,
  hydraPgModules, hydraRdfModules, hydraWasmModules,
  hydraBuildModules,
  hydraExtPackageModules,
  allDslModules, allEncodingModules, extRoutingInput)

import qualified Hydra.Kernel as Kernel
import qualified Hydra.Core as Core
import qualified Hydra.Validate.Packaging as ValidatePackaging
import qualified Hydra.Validate.Core as ValidateCore
import qualified Hydra.Print.Error.Packaging as PrintErrorPackaging
import qualified Hydra.Print.Error.Core as PrintErrorCore
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Sources.Demos.GenPG.Transform as GenPGTransform

import Control.Exception (catch, SomeException)
import Control.Monad (when)
import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified System.Directory as DD
import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified System.FilePath as FP
import System.IO (hFlush, stdout)


-- 'loadNativePackageModules' moved to Hydra.Generation (#346) so that
-- transform-haskell-dsl-to-json shares the same native-JSON loader.

-- | Deduplicate a list of modules by namespace, keeping the first occurrence.
dedupByNamespace :: [Kernel.Module] -> [Kernel.Module]
dedupByNamespace = go S.empty
  where
    go _    []     = []
    go seen (m:ms)
      | ns `S.member` seen = go seen ms
      | otherwise          = m : go (S.insert ns seen) ms
      where ns = Kernel.moduleName m

-- | Type definitions exempt from #575 semantic validation. UntypedLambdaError
-- is currently `T.record []` and should migrate to `T.wrap T.unit`, but the
-- change ripples through encode/decode/DSL helpers + every host language and
-- is sized as its own change. Tracked separately. Applied globally (every
-- package, not just the kernel) since the name is fully-qualified.
kernelExemptTypeNames :: S.Set Core.Name
kernelExemptTypeNames = S.fromList
  [Core.Name "hydra.error.checking.UntypedLambdaError"]

-- | Term definitions exempt from #575 semantic validation. etaExpandTypedTerm
-- contains a deliberate `Logic.ifElse false ...` inference hack that lets the
-- inferencer assign `list<Type>` to an otherwise-typeless empty list; excising
-- it causes bootstrap-from-json (Java target) to fail with "expected
-- list<hydra.core.Type> but found list<unit>". Validate.Core's
-- constant-condition rule flags it but the hack is load-bearing; see the
-- comment in Sources/Kernel/Terms/Reduction.hs. Applied globally (every
-- package, not just the kernel) since the name is fully-qualified.
kernelExemptTermNames :: S.Set Core.Name
kernelExemptTermNames = S.fromList
  [Core.Name "hydra.reduction.etaExpandTypedTerm"]

main :: IO ()
main = do
  distRoot <- parseDistRoot defaultDistJsonRoot
  includeJavaPython <- parseIncludeJavaPython
  includeBench <- parseIncludeBench

  -- hydra-bench is opt-in: --include-bench must be passed by bin/sync-bench.sh
  -- to add the synthetic inference workloads to the universe. Default sync
  -- (bin/sync.sh) omits them so they don't balloon every host's codegen step.
  let extraBench = if includeBench then hydraBenchModules else []

  -- #346/#370/#505: hydra-jvm, hydra-java, and hydra-python have NO Haskell DSL
  -- sources, so their modules are absent from the compiled universe. But other
  -- packages may REFERENCE them at inference time — notably hydra-scala's serde
  -- calls hydra.jvm.serde.escapeJavaString. Load those modules from their
  -- already-generated dist/json (written by the native drivers) so cross-package
  -- references resolve. These are excluded from the write pass below by
  -- isNativeOwned, so loading them only seeds the inference universe.
  let baseUniverse = dedupByNamespace $ L.concat
        [ mainModules
        , dslSourceModules
        , extraBench
        , hydraCoqModules
        , hydraGoModules
        , hydraJvmModules
        , hydraJavaModules
        , hydraTypeScriptModules
        , hydraPythonModules
        , hydraScalaModules
        , hydraLispModules
        , hydraPgModules
        , hydraRdfModules
        , hydraWasmModules
        , hydraBuildModules
        , hydraExtPackageModules
        , [GenPGTransform.module_]
        ]

  -- Synthesize all encode/decode modules in-memory from their source type modules
  -- and add them to the universe so that other modules' inference can resolve
  -- hydra.encode.*/hydra.decode.* cross-references. These modules are no longer
  -- imported from dist/haskell/*/Sources/{Encode,Decode}/*.hs (#448); the
  -- JSON under dist/json is produced independently by writeDerivedJsonPackageSplit
  -- below. Same synthesizer, same content, no .hs needed.
  encMods <- generateEncoderModules baseUniverse allEncodingModules
  decMods <- generateDecoderModules baseUniverse allEncodingModules
  let synthesizedEncodeDecode = dedupByNamespace (encMods ++ decMods)

  nativeTagged <- loadNativePackageModulesTagged distRoot baseUniverse
  let nativeModules = L.concatMap snd nativeTagged
  let universe = dedupByNamespace (baseUniverse ++ synthesizedEncodeDecode ++ nativeModules)

  -- Routing map (#474), derived from each package's compiled mainModules plus
  -- the native (hydra-jvm / hydra-java / hydra-python) modules loaded from dist/json.
  -- Native modules are tagged by the package whose manifest they were LOADED from
  -- (#511), NOT by namespace prefix — prefix-tagging dropped modules without the
  -- package's prefix segment (e.g. hydra.gradle, owned by hydra-java). everything
  -- else comes from extRoutingInput.
  let nativeRoutingInput = [ (pkg, fmap Kernel.moduleName mods) | (pkg, mods) <- nativeTagged ]
      routingMap = buildRoutingMap (extRoutingInput ++ nativeRoutingInput)

  putStrLn "=== Generate Hydra JSON modules ==="
  putStrLn ""

  -- Native hosts own the DSL→JSON path for hydra-jvm, hydra-java, and hydra-python
  -- (#344, #505). Their canonical JSON is produced by the Java driver
  -- (bin/generate-hydra-java-from-java.sh, Phase 5 of sync.sh). We still load their
  -- modules so they participate in the inference universe and DSL-wrapper synthesis
  -- below, but we exclude their term-level modules from this write pass.
  let isNativeOwned m =
        let ns = Packaging.unModuleName (Kernel.moduleName m)
        in L.isPrefixOf "hydra.jvm." ns || L.isPrefixOf "hydra.java." ns || L.isPrefixOf "hydra.python." ns
      -- The native packages (#344) are loaded into the universe together with
      -- their ALREADY-DERIVED DSL wrapper modules (hydra.dsl.{java,python}.*),
      -- via loadNativePackageModules reading both mainModules and dslModules
      -- from JSON. Those wrappers are DERIVED modules: their phantom types
      -- (atomTrue : TypedTerm Atom) are declared at construction, not inferred.
      -- The main write pass runs per-package inference (doInfer=True), so if a
      -- wrapper module reaches writeUniverse it gets RE-INFERRED — and a nullary
      -- builder like atomTrue (body `inject Atom.true ()`, nothing forcing the
      -- result to Atom) generalizes to ∀t. TypedTerm t, emitting a poly/thunked
      -- wrapper the target coders mishandle. Inference must NEVER run on derived
      -- modules. So exclude the loaded native DSL wrappers from the main pass in
      -- BOTH branches; they were loaded only to seed the inference universe and
      -- their canonical JSON is already on disk. (The non-native packages' DSL
      -- wrappers are written by the derived pass below, not here.)
      isNativeDslWrapper m =
        let ns = Packaging.unModuleName (Kernel.moduleName m)
        in L.isPrefixOf "hydra.dsl.java." ns || L.isPrefixOf "hydra.dsl.python." ns
      -- Derived encode/decode modules (hydra.encode.*, hydra.decode.*) are compiled
      -- into mainModules and so reach this universe, but they are DERIVED: the
      -- synthesizer is authoritative for their in-term annotations and the derived
      -- pass below re-emits them with doInfer=False. They must NOT be re-inferred
      -- by this main (doInfer=True) pass — inference's occurs-check rejects the
      -- saturated nominal self-applications in polymorphic decoder signatures (e.g.
      -- hydra.decode.parsing.parseResult : ParseResult @ a). They are loaded only to
      -- seed the inference universe for the hand-written modules. Exclude them here;
      -- the derived pass is their sole writer. (#476)
      isDerivedEncodeDecode m =
        let ns = Packaging.unModuleName (Kernel.moduleName m)
        in L.isPrefixOf "hydra.encode." ns || L.isPrefixOf "hydra.decode." ns
      isDerived m = isNativeDslWrapper m || isDerivedEncodeDecode m
      writeUniverse
        | includeJavaPython = filter (not . isDerived) universe
        | otherwise         = filter (\m -> not (isNativeOwned m) && not (isDerived m)) universe
      excluded = length universe - length writeUniverse

  -- Undeclared cross-module dependency check (#574). Runs over the
  -- hand-written universe (not just kernelModules) since a referenced
  -- name's owning module may live in a different package -- the
  -- motivating incident (#555) was itself cross-package.
  --
  -- isExcludedFromUndeclaredDepsCheck excludes:
  --   * isNativeOwned (hydra.jvm.*/java.*/python.*): native-host packages
  --     with no Haskell DSL source; their JSON is loaded only to seed the
  --     inference universe and can embed literal cross-namespace variable
  --     references inside their own compiled bodies (observed: a
  --     hydra.python.coder.json definitions entry literally named
  --     "hydra.environment.reorderDefs", which corrupted the check's
  --     name-to-owner map when included in its universe).
  --   * isDerivedModule (hydra.dsl.*/encode.*/decode.*), the #575 GLOBAL
  --     derived-module policy (shared with the structural/semantic passes
  --     below): synthesized modules whose moduleDependencies is computed by
  --     Encoding.encodeModule/Decoding.decodeModule from the SOURCE module's
  --     declared deps, but never includes the source's own raw namespace --
  --     even though the generated body embeds literal type references into
  --     it (e.g. hydra.encode.paths references hydra.core.Field/.Name/.Term/
  --     etc. with hydra.core absent from its declared deps by construction,
  --     not by omission). Not a real gap. (#574 originally scoped this
  --     exclusion to isDerivedEncodeDecode specifically, pending the #575
  --     broader decision on derived-module validation policy -- now
  --     resolved: isDerivedModule is that decision, applied uniformly here
  --     and in the structural/semantic passes below. hydra.dsl.* wrappers
  --     are additionally covered now, not just encode/decode.)
  let isExcludedFromUndeclaredDepsCheck m = isNativeOwned m || isDerivedModule m
      undeclaredDepsUniverse = filter (not . isExcludedFromUndeclaredDepsCheck) universe
  checkUndeclaredDependenciesOrExit undeclaredDepsUniverse

  -- Pre-inference validation (#575): structural (packaging) + semantic
  -- (core, typed=False) checks need only module shape, so they run before
  -- any inference. Every package in the routing map is validated, not just
  -- the kernel -- packagesFromRouting groups 'universe' by the same routing
  -- that determines dist/json layout, so a package here can never disagree
  -- with where its own JSON lives. Native-owned packages
  -- (hydra-jvm/java/python) ARE included here since their DSL-side modules
  -- (loaded to seed inference, see isNativeOwned above) still have real
  -- structural shape worth checking; only their WRITE is skipped later, not
  -- their validation. Structural checks run ONLY here, not post-inference:
  -- module shape is authored, not affected by inference, so a second run
  -- would be pure redundancy. Distinct from checkUndeclaredDependenciesOrExit
  -- above (#574): that check resolves cross-module name references against
  -- a whole-universe owner map and is NOT part of kernelPackagingRuleNames /
  -- kernelDefaultPackagingProfile (by the #574 author's own design -- see
  -- Validate/Packaging.hs's kernelUniverseUndeclaredDependencies doc
  -- comment), so validatePackagesStructural below cannot and does not
  -- double-run it; the two checks are complementary, not overlapping.
  --
  -- validatePackagesStructural selects each package's own profile via
  -- packagingProfileFor/strictPackagingPackages (Hydra.Generation): only
  -- hydra-kernel is held to the fully-fatal kernelDefaultPackagingProfile
  -- for now; every other package gets kernelPackagingProfileWithDocWarnings
  -- (documentation-completeness demoted to a warning) while its own
  -- pre-existing documentation backlog is remediated. See #575 and #512.
  --
  -- exemptTypeNames/exemptTermNames carry forward the kernel's own
  -- inference/eta-expansion hacks (UntypedLambdaError, etaExpandTypedTerm --
  -- see their definitions below at kernelExemptTypeNames/kernelExemptTermNames)
  -- applied GLOBALLY across every package: both names are fully-qualified,
  -- so a same-name collision in a non-kernel package is not possible, and a
  -- single call across every package is simpler than splitting by package.
  let allPkgs = packagesFromRouting routingMap universe
  reportAndExitOnFailure "pre-inference (packaging)" $
    validatePackagesStructural allPkgs
  -- Native-owned packages (hydra-jvm/java/python) are excluded here for the
  -- same reason 'validatePackagesStructural' excludes them
  -- ('nativeOwnedPackagingPackages' doc comment): their on-disk JSON, loaded
  -- via isNativeOwned above only to seed the inference universe, is
  -- legitimately stale at this point in the pipeline -- sync.sh's Phase 1.5
  -- (auto-heal, #406) runs AFTER this driver. Without this filter, a broken
  -- synthesizer bug already fixed in the native DSL source (e.g. a variable-
  -- shadowing fix in hydra-java's Coder.java) still reports as a failure
  -- here every single sync, since this fatal gate never sees the healed
  -- JSON. Their DSL-shape is still worth checking -- see the doc comment on
  -- 'nativeOwnedPackagingPackages' -- just not as part of THIS fatal gate.
  let semanticPkgs = packagesFromRouting routingMap (filter (not . isNativeOwned) universe)
  reportAndExitOnFailure "pre-inference (core)" $
    validatePackagesSemantic ValidateCore.kernelDefaultCoreProfile False
      kernelExemptTypeNames kernelExemptTermNames semanticPkgs

  putStrLn $ "Generating " ++ show (length writeUniverse) ++ " modules to JSON, routed per package..."
  when (excluded > 0) $
    putStrLn $ "  (excluded " ++ show excluded
      ++ " native-owned (hydra.jvm.*/java.*/python.*) modules; see #344/#505)"
  putStrLn $ "dist-json root: " ++ distRoot
  putStrLn ""

  result <- catch
    (writeModulesJsonPackageSplit routingMap True distRoot universe writeUniverse >> return True)
    (\e -> do
      putStrLn $ "Error: " ++ show (e :: SomeException)
      return False)

  -- Post-inference validation (#575): semantic (core) checks ONLY, against
  -- INFERRED types. writeModulesJsonPackageSplit discards inferred modules
  -- in-memory to bound peak memory, so there is no return-value path for
  -- typed modules -- reload the just-written JSON from disk via
  -- loadModulesFromJson instead. loadModulesFromJson joins its basePath
  -- directly with each module's dotted path (<basePath>/<ns-as-path>.json),
  -- which is only correct for a single-package basePath (its other caller,
  -- loadNativePackageModulesTagged, passes a package-specific dir) -- so
  -- writeUniverse (spanning every package) must be reloaded PER PACKAGE, each
  -- with its own package-specific basePath, mirroring
  -- namespaceToPackageJsonDirIn. A prior version of this call passed the
  -- shared dist/json ROOT for every module regardless of owning package,
  -- which produced <root>/hydra/paths.json for hydra.paths (owned by
  -- hydra-kernel, correct location
  -- <root>/hydra-kernel/src/main/json/hydra/paths.json) -- a crash on every
  -- real sync, since hydra.paths always exists. Only meaningful if the write
  -- succeeded. Structural (packaging) checks do NOT rerun here -- see the
  -- pre-inference call site's comment above. typedUniverse is loaded only
  -- for writeUniverse's modules (the ones this driver actually wrote), so
  -- native-owned packages (hydra-jvm/java/python) are correctly absent from
  -- post-inference validation: their JSON is written by separate native
  -- drivers this file never touches, so there is nothing fresh to reload.
  --
  -- ALSO append synthesizedEncodeDecode (the in-memory hydra.encode.*/
  -- hydra.decode.* modules built at the top of main, doInfer=False) to the
  -- GRAPH CONTEXT (not the validated set -- see below): many non-derived
  -- modules legitimately call derived encode/decode functions (e.g.
  -- hydra.codegen.moduleToJson calls hydra.encode.packaging.module), and
  -- those derived modules' own JSON is written later by
  -- writeDerivedJsonPackageSplit (after this validation call), so it is not
  -- on disk yet to reload. universe already includes synthesizedEncodeDecode
  -- for the identical reason at the PRE-inference stage (see its own
  -- in-main comment) -- typedUniverse needs the same modules in scope for
  -- term-reference resolution to succeed at the POST-inference stage too.
  --
  -- But do NOT validate synthesizedEncodeDecode's own definitions under
  -- typed=True: these modules are doInfer=False by design (the synthesizer,
  -- not HM inference, populates their term annotations -- see the doc
  -- comment where synthesizedEncodeDecode is built), and their polymorphic
  -- functions' lambda domains are DELIBERATELY bare type-parameter
  -- references (e.g. decode<a> has a lambda domain literally `a`), never
  -- monomorphized. Checking them with typed=True (which T8 interprets as
  -- "every lambda domain must now be fully resolved") produces a false
  -- positive for every polymorphic derived function -- confirmed uniform
  -- across all of them, not isolated bugs. They are still validated with
  -- typed=False at the PRE-inference call site above, which is the correct
  -- mode for synthesizer-populated (not inferred) term annotations.
  CM.when result $
    do
      writtenTypedMods <- fmap L.concat $
        CM.forM (groupByPackageIn routingMap writeUniverse) $ \(pkg, pkgMods) ->
          loadModulesFromJson (distRoot FP.</> pkg FP.</> "src" FP.</> "main" FP.</> "json")
            universe (map Kernel.moduleName pkgMods)
      let typedUniverse = writtenTypedMods ++ synthesizedEncodeDecode
          synthesizedTypeNames = S.fromList
            [ Packaging.typeDefinitionName td
            | m <- synthesizedEncodeDecode, Packaging.DefinitionType td <- Packaging.moduleDefinitions m ]
          synthesizedTermNames = S.fromList
            [ Packaging.termDefinitionName td
            | m <- synthesizedEncodeDecode, Packaging.DefinitionTerm td <- Packaging.moduleDefinitions m ]
      reportAndExitOnFailure "post-inference (core)" $
        validatePackagesSemantic ValidateCore.kernelDefaultCoreProfile True
          (S.union kernelExemptTypeNames synthesizedTypeNames)
          (S.union kernelExemptTermNames synthesizedTermNames)
          (packagesFromRouting routingMap typedUniverse)

  putStrLn ""
  putStrLn "Generating derived modules (DSL + encode + decode) to JSON..."
  -- The derived-module generators run over each package's derivedMainModules
  -- list (see each package's Manifest.hs). For each source module we emit
  -- hydra.dsl.<x>, hydra.encode.<x>, and hydra.decode.<x>, routed to their
  -- owning package's dist/json/<pkg>/ via the derived RoutingMap. The
  -- encode/decode JSON is emitted DIRECTLY from the synthesized Modules — no
  -- per-host Hydra.Sources.{Encode,Decode}.*.hs source-as-data copy is needed
  -- for the JSON to exist (#474, #448-aligned). Per-package opt-in: add a
  -- module to a package's derivedMainModules list to start deriving for it.
  let dslSrcMods = allDslModules
      encSrcMods = allEncodingModules
  dslResult <- catch
    (writeDerivedJsonPackageSplit routingMap distRoot universe dslSrcMods encSrcMods >> return True)
    (\e -> do
      putStrLn $ "Error generating derived JSON: " ++ show (e :: SomeException)
      return False)

  if result && dslResult
    then do
      putStrLn ""
      putStrLn "=== Done! ==="
    else do
      putStrLn ""
      putStrLn "=== FAILED ==="
      exitFailure

-- | Check every module in the given universe for undeclared cross-module
-- dependencies (hydra.validate.packaging.checkUndeclaredDependencies /
-- kernelUniverseUndeclaredDependencies; see #574): a module referencing a
-- name owned by another module which is not among its declared
-- moduleDependencies. Runs over the WHOLE universe (all packages), not
-- scoped to a single 'Package' -- the motivating incident (#555) was itself
-- cross-package, so a single-package check would not have caught it. Wired
-- as its own call, independent of the #575 structural/semantic passes
-- (see the pre-inference call site's comment) -- this check resolves
-- cross-module name references against a whole-universe owner map, a
-- fundamentally different shape of check from the per-'Package' structural
-- rules.
checkUndeclaredDependenciesOrExit :: [Kernel.Module] -> IO ()
checkUndeclaredDependenciesOrExit mods = do
    putStrLn $ "Checking " ++ show (length mods)
               ++ " modules for undeclared cross-module dependencies (hydra.validate.packaging.checkUndeclaredDependencies)..."
    hFlush stdout
    let graph = modulesToGraph mods mods
    let primNames = S.fromList (M.keys (Kernel.graphPrimitives graph))
    let findings = ValidatePackaging.kernelUniverseUndeclaredDependencies mods primNames
    if null findings
      then do
        putStrLn $ "  No undeclared dependencies found."
        putStrLn ""
      else do
        putStrLn $ "  " ++ show (length findings) ++ " undeclared-dependency finding(s):"
        mapM_ (\e -> putStrLn $ "    " ++ PrintErrorPackaging.invalidPackageError e) findings
        putStrLn ""
        putStrLn "=== FAILED: undeclared cross-module dependencies ==="
        exitFailure

-- Kernel-only validateKernelModulesOrExit (disabled since 2def8d1d95,
-- misattributed to #368) replaced by the generic pre/post-inference
-- validatePackagesStructural/validatePackagesSemantic calls above, covering
-- every package. See #575.

-- | Report a 'ValidationFindings' result; exit failure if it is non-empty.
-- 'label' identifies the pass (e.g. "pre-inference (packaging)") in output.
reportAndExitOnFailure :: String -> ValidationFindings -> IO ()
reportAndExitOnFailure label findings = do
  hFlush stdout
  if validationFindingsNull findings
    then putStrLn $ "  Validation (" ++ label ++ "): all packages valid."
    else do
      let ValidationFindings pkgFailures typeFailures termFailures = findings
      putStrLn $ "  Validation (" ++ label ++ "): "
        ++ show (length pkgFailures) ++ " packaging failure(s), "
        ++ show (length typeFailures) ++ " core-type failure(s), "
        ++ show (length termFailures) ++ " core-term failure(s):"
      mapM_ (\(pkg, e) -> putStrLn $ "  [packaging] " ++ Packaging.unPackageName (Packaging.packageName pkg)
               ++ ": " ++ PrintErrorPackaging.invalidPackageError e) pkgFailures
      mapM_ (\(m, td, e) -> putStrLn $ "  [core/type] " ++ Packaging.unModuleName (Kernel.moduleName m)
               ++ "." ++ Core.unName (Packaging.typeDefinitionName td) ++ ": " ++ PrintErrorCore.invalidTypeError e) typeFailures
      mapM_ (\(m, td, e) -> putStrLn $ "  [core/term] " ++ Packaging.unModuleName (Kernel.moduleName m)
               ++ "." ++ Core.unName (Packaging.termDefinitionName td) ++ ": " ++ PrintErrorCore.invalidTermError e) termFailures
      putStrLn ""
      putStrLn $ "=== FAILED: " ++ label ++ " validation ==="
      exitFailure

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

-- | --include-bench appends the hydra-bench package's modules to the universe.
-- Off by default. Set by bin/sync-bench.sh, never by the default sync pipeline.
parseIncludeBench :: IO Bool
parseIncludeBench = do
  args <- getArgs
  return $ "--include-bench" `elem` args
