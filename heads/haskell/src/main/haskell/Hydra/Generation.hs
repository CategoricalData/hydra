-- | Entry point for Hydra code generation utilities

{-# LANGUAGE BangPatterns #-}

module Hydra.Generation (
  module Hydra.Generation,
) where

import Hydra.Kernel
import Hydra.Overlay.Haskell.Dsl.Annotations
import Hydra.Overlay.Haskell.Bootstrap
import Hydra.PackageRouting (RoutingMap, groupByPackageIn, namespaceToPackageIn, namespaceToPackageMaybeIn)
import Hydra.Packaging (_Module)
import Hydra.Testing (TestGroup(..))
import qualified Hydra.Json.Model as Json
import qualified Hydra.Json.Writer as JsonWriter
import qualified Hydra.Decoding as Decoding
import qualified Hydra.Digest as Digest
import qualified Hydra.Dsls as Dsls
import qualified Hydra.Encoding as Encoding
import qualified Hydra.Errors as Error
import qualified Hydra.Print.Errors as PrintError
import qualified Hydra.Codegen as CodeGeneration
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Inference as Inference
import qualified Hydra.Validate.Packaging as ValidatePackaging
import qualified Hydra.Validate.Core as ValidateCore
-- Hydra.Kernel re-exports Hydra.Error.Core (InvalidTypeError, InvalidTermError)
-- but NOT Hydra.Error.Packaging (InvalidPackageError) -- import it explicitly.
import Hydra.Error.Packaging (InvalidPackageError)

import qualified Control.Exception as E
import qualified Control.Monad as CM
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson.Key as AK
import qualified Data.ByteString.Lazy as BS
import qualified Data.Set as S
import qualified Data.Scientific as SC
import qualified Data.Vector as V
import qualified System.FilePath as FP
import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified System.Directory as SD
import qualified Data.Maybe as Y
import Data.Char (isAlphaNum, toUpper)



showError :: Error.Error -> String
showError = PrintError.error

-- | An initial InferenceContext (fresh-variable counter at 0, empty subterm-path trace).
-- Note: emptyInferenceContext is now re-exported from Hydra.Kernel via Hydra.Lexical.

-- | Generate source files and write them to disk.
-- Returns the relative paths the run wrote (joinable with the basePath argument
-- to get full paths). Callers that only care about the count can take 'length'
-- of the result; callers driving stale-output pruning (#357) use the path set
-- as the keep-set for their post-generation walk.
generateSources
  :: (Module -> [Definition] -> InferenceContext -> Graph -> Either Error.Error (M.Map FilePath String))
  -> Language
  -> Bool  -- ^ doInfer
  -> FilePath
  -> [Module]  -- ^ Universe
  -> [Module]  -- ^ Modules to generate
  -> IO [FilePath]  -- ^ Relative paths written (relative to basePath)
generateSources = generateSourcesWithTransform id

-- | Like 'generateSources' but applies a 'String -> String' transform to
-- each generated file's content before writing. The transform is part of
-- the generation pipeline (not a post-pass that reads back from disk),
-- which is the appropriate place for whole-file textual passes such as
-- the Scala line-wrap that 'writeScala' uses to keep individual lines
-- within scalac's stack-friendly threshold.
--
-- Emission flags (eta-expansion, case-hoisting, polymorphic-let-hoisting)
-- are now read from @lang@'s @supportedFeatures@ inside
-- @generateSourceFiles@; the caller only supplies @doInfer@.
generateSourcesWithTransform
  :: (String -> String)  -- ^ Per-file content transform (id for no-op)
  -> (Module -> [Definition] -> InferenceContext -> Graph -> Either Error.Error (M.Map FilePath String))
  -> Language
  -> Bool
  -> FilePath
  -> [Module]
  -> [Module]
  -> IO [FilePath]
generateSourcesWithTransform transform printDefinitions lang doInfer basePath universeModules modulesToGenerate = do
    let cx = emptyInferenceContext
    case CodeGeneration.generateSourceFiles printDefinitions lang doInfer bootstrapGraph universeModules modulesToGenerate cx of
      Left err -> do
        -- #474 diagnostic: on a batch failure, re-run each module individually
        -- against the full universe to name the culprit(s) before failing.
        CM.forM_ modulesToGenerate $ \m ->
          case CodeGeneration.generateSourceFiles printDefinitions lang doInfer bootstrapGraph universeModules [m] cx of
            Left e  -> putStrLn $ "  [gen-fail] " ++ unModuleName (moduleName m) ++ ": " ++ showError e
            Right _ -> return ()
        fail $ "Failed to generate source files: " ++ showError err
      Right files -> do
        mapM_ writePair files
        return $ map fst files
  where
    writePair (path, raw) = do
        let fullPath = FP.combine basePath path
        SD.createDirectoryIfMissing True $ FP.takeDirectory fullPath
        -- Skip writes when content is byte-identical. Rewriting the file with
        -- the same bytes bumps its mtime and triggers Stack (and every other
        -- mtime-based incremental build tool) to invalidate downstream
        -- artifacts, even though nothing actually changed. Hashing the file
        -- and comparing would be stricter but reading + comparing is
        -- adequate for generated source files under 1 MB.
        exists <- SD.doesFileExist fullPath
        skip <- if exists
                  then do old <- readFile fullPath
                          -- Force the whole string so the handle closes before
                          -- the subsequent writeFile reopens the path.
                          -- Comparing via == is lazy and can leave the handle
                          -- open, causing resource-busy errors on rewrite.
                          length old `seq` return (old == withNewline)
                  else return False
        CM.unless skip $ writeFile fullPath withNewline
      where
        s = transform raw
        -- Trailing whitespace is the coder's responsibility. The Hydra
        -- serialization layer in `hydra.serialization` and per-coder
        -- writers (e.g. the Haskell `toHaskellComments` formatter)
        -- avoid emitting trailing whitespace at the source. This
        -- writer just adds the final newline.
        withNewline = if L.isSuffixOf "\n" s then s else s ++ "\n"

-- | Build a graph from a list of modules using the Haskell bootstrapGraph.
-- Thin wrapper around modulesToGraphWith.
modulesToGraph :: [Module] -> [Module] -> Graph
modulesToGraph = CodeGeneration.modulesToGraph bootstrapGraph

-- ============================================================================
-- Package-level validation support (#575)
-- ============================================================================

-- | A derived module: one whose content is synthesizer-generated rather than
-- hand-authored, and which is therefore exempt from structural/packaging
-- validation rules (their definitions lists follow a semantic grouping, not
-- the alphabetical convention 'checkDefinitionOrdering' enforces on
-- hand-written modules). Covers the DSL wrapper (hydra.dsl.*) and
-- encode/decode (hydra.encode.*/hydra.decode.*) families. This is the single
-- source of truth for the derived-module exemption; callers that need to
-- distinguish structural exemption from semantic-check applicability should
-- use this predicate to gate 'ValidatePackaging' rules only, not
-- 'ValidateCore' rules (derived modules remain subject to those — a broken
-- synthesizer output should still surface as a validation failure).
isDerivedModule :: Module -> Bool
isDerivedModule m =
  L.isPrefixOf "hydra.dsl." ns || L.isPrefixOf "hydra.encode." ns || L.isPrefixOf "hydra.decode." ns
  where
    ns = unModuleName (moduleName m)

-- | Build a 'Package' value for each package in a routing map, wrapping the
-- given modules under that package's name. Metadata and dependencies are
-- left empty ('Nothing' / '[]'): no packaging-validation rule
-- ('checkConflictingModuleNames', 'checkDuplicateModuleNames',
-- 'checkPackageNameConvention') consults them, only 'packageName' and
-- 'packageModules'. Grouping is delegated to 'groupByPackageIn', the same
-- routing logic that determines where each module's JSON is written, so a
-- package here can never disagree with where that package's dist/json
-- directory lives.
packagesFromRouting :: RoutingMap -> [Module] -> [Package]
packagesFromRouting routingMap mods =
  [ Package (PackageName pkg) Nothing [] pkgMods
  | (pkg, pkgMods) <- groupByPackageIn routingMap mods ]

-- | Structural (packaging) findings for one package: the package and its
-- 'InvalidPackageError' (module-level findings are already lifted into
-- package-level findings by 'ValidatePackaging.package').
type PackageFailure = (Package, InvalidPackageError)

-- | Semantic (core) findings for one type definition.
type TypeFailure = (Module, TypeDefinition, InvalidTypeError)

-- | Semantic (core) findings for one term definition.
type TermFailure = (Module, TermDefinition, InvalidTermError)

-- | The full set of findings from one validation pass. Structural and
-- semantic findings are reported separately because they run as two
-- cleanly-separated passes at two different sync phases (#575): structural
-- (packaging) runs pre-inference only via 'validatePackagesStructural'
-- (module shape doesn't change across inference, so a post-inference rerun
-- would be pure redundancy); semantic (core) runs via
-- 'validatePackagesSemantic', called once pre-inference (typed=False) and
-- once post-inference (typed=True) against the reloaded, now-typed modules.
data ValidationFindings = ValidationFindings
  { validationPackageFailures :: [PackageFailure]
  , validationTypeFailures    :: [TypeFailure]
  , validationTermFailures    :: [TermFailure]
  } deriving (Eq, Show)

validationFindingsNull :: ValidationFindings -> Bool
validationFindingsNull (ValidationFindings pkgs types terms) =
  null pkgs && null types && null terms

-- | Packages held to the full 'ValidatePackaging.kernelDefaultPackagingProfile'
-- (every rule, including documentation completeness, is fatal). Every other
-- package uses 'ValidatePackaging.kernelPackagingProfileWithDocWarnings'
-- instead (#575): re-enabling comprehensive validation surfaced a large,
-- pre-existing documentation-completeness backlog outside the kernel, and
-- treating it as fatal everywhere at once would turn every sync fleet-wide
-- red until fully remediated. hydra-kernel is held to the full bar from the
-- start per policy; other packages are added here as their own backlogs are
-- remediated, expanding the fully-fatal set incrementally rather than
-- gating the whole fleet on the total backlog at once.
--
-- This is an INTERIM mechanism. The intended long-term home for per-package
-- validation configuration is a typed 'PackageValidationConfiguration' (see
-- issue #512) declared in each package's package.json -- richer than a
-- strict/relaxed toggle, supporting per-rule severities and parameters
-- (e.g. "allow underscores in names", "exempt deprecated definitions from
-- documentation"). This hardcoded list is deliberately the simplest thing
-- that implements today's actual policy without blocking on that design.
strictPackagingPackages :: [PackageName]
strictPackagingPackages = PackageName <$> ["hydra-kernel", "hydra-build", "hydra-haskell", "hydra-go", "hydra-lisp", "hydra-wasm", "hydra-typescript", "hydra-rdf", "hydra-pg", "hydra-ext"]

-- | Packages whose dist/json is written by a separate NATIVE driver
-- (bin/generate-hydra-java-from-java.sh, -python-from-python.sh), not by
-- this driver's own write pass (see 'isNativeOwned' in update-json-main's
-- Main.hs). Their on-disk JSON is loaded here only to seed the inference
-- universe ('loadNativePackageModulesTagged') and is legitimately stale at
-- this point in the pipeline: sync.sh's Phase 1.5 (auto-heal, #406) runs
-- AFTER this driver and is what brings their JSON current. Structurally
-- validating that stale JSON here would fail on staleness the pipeline
-- itself hasn't resolved yet, not on a real defect — so these packages are
-- excluded from 'validatePackagesStructural' entirely. Their own DSL
-- source's structural shape is a legitimate concern, but needs a hook in
-- the native drivers themselves (out of #575's scope; see #580).
nativeOwnedPackagingPackages :: [PackageName]
nativeOwnedPackagingPackages = PackageName <$> ["hydra-jvm", "hydra-java", "hydra-python"]

-- | Select the packaging 'ValidationProfile' for a package: the full,
-- fatal-on-everything profile for packages in 'strictPackagingPackages',
-- the documentation-relaxed profile for every other package.
packagingProfileFor :: Package -> ValidationProfile
packagingProfileFor pkg
  | packageName pkg `elem` strictPackagingPackages = ValidatePackaging.kernelDefaultPackagingProfile
  | otherwise = ValidatePackaging.kernelPackagingProfileWithDocWarnings

-- | Structural (packaging) validation against a list of packages. Runs
-- PRE-INFERENCE ONLY: module shape (definition names, ordering, docs,
-- conflicts) is authored and does not change across inference, so there is
-- no post-inference call for this — running it twice would be pure
-- redundant work with zero additional signal. Derived modules
-- (hydra.dsl.*/encode.*/decode.*) are exempted per 'isDerivedModule': their
-- definitions are synthesizer-ordered, not alphabetical, so packaging
-- convention rules would spuriously fail on them. Native-owned packages
-- ('nativeOwnedPackagingPackages') are excluded entirely — their JSON is
-- stale-by-design at this pipeline point, see that binding's doc comment.
--
-- Each package is validated under its OWN profile via 'packagingProfileFor'
-- rather than one profile shared by every package -- see
-- 'strictPackagingPackages' for the current fatal-vs-warning policy.
validatePackagesStructural :: [Package] -> ValidationFindings
validatePackagesStructural pkgs =
  ValidationFindings pkgFailures [] []
  where
    emptyPkgResult :: ValidationResult InvalidPackageError
    emptyPkgResult = ValidationResult [] []
    pkgFailures =
      [ (pkg, e)
      | pkg <- pkgs
      , packageName pkg `notElem` nativeOwnedPackagingPackages
      , let structuralPkg = pkg { packageModules = filter (not . isDerivedModule) (packageModules pkg) }
      , e <- validationResultErrors
               (ValidatePackaging.package (packagingProfileFor pkg) emptyPkgResult structuralPkg) ]

-- | Semantic (core) type/term-tree validation against a list of packages,
-- INCLUDING derived modules (hydra.dsl.*/encode.*/decode.*) — a broken
-- synthesizer output is a real bug and must still surface; #575's accepted
-- policy is structural-exempt, semantic-accountable.
--
-- 'typed' controls whether term validation runs in pre-inference mode
-- (lambdas lack domain annotations, term variables lack known types; pass
-- 'False') or post-inference mode against already-typed modules (pass
-- 'True'). Type-definition validation is unaffected by 'typed': a type
-- definition's own TypeScheme is authored, not inferred, so it is checked
-- identically in both phases. Callers typically run this pass at BOTH
-- phases (typed=False pre-inference, typed=True post-inference against
-- modules reloaded via 'loadModulesFromJson'), since each phase can surface
-- different findings: pre-inference catches authoring-shape defects
-- (duplicate fields, empty annotations) before inference can obscure them;
-- post-inference catches defects that only exist once types are resolved.
--
-- 'exemptTypeNames' / 'exemptTermNames' let a caller carry forward
-- definition-specific exemptions (e.g. the kernel's UntypedLambdaError /
-- etaExpandTypedTerm carve-outs) without hard-coding them here — this
-- module has no kernel-specific knowledge.
validatePackagesSemantic ::
     ValidationProfile
  -> Bool             -- ^ typed (post-inference term validation)
  -> S.Set Name        -- ^ exemptTypeNames
  -> S.Set Name        -- ^ exemptTermNames
  -> [Package]
  -> ValidationFindings
validatePackagesSemantic profile typed exemptTypeNames exemptTermNames pkgs =
  ValidationFindings [] typeFailures termFailures
  where
    allMods = L.concatMap packageModules pkgs
    graph = modulesToGraph allMods allMods
    typeDefsOf m =
      [ td | DefinitionType td <- moduleDefinitions m
           , not (S.member (typeDefinitionName td) exemptTypeNames) ]
    termDefsOf m =
      [ td | DefinitionTerm td <- moduleDefinitions m
           , not (S.member (termDefinitionName td) exemptTermNames) ]
    emptyCoreResult :: ValidationResult e
    emptyCoreResult = ValidationResult [] []
    validateTypeDef td =
      let TypeScheme vs body _ = typeDefinitionBody td
          inScope = S.union allTypeNames (S.fromList vs)
          vr = ValidateCore.type_ profile emptyCoreResult inScope body
      in case validationResultErrors vr of
           []    -> Nothing
           err:_ -> Just err
    validateTermDef td =
      let vr = ValidateCore.term profile typed graph (termDefinitionBody td)
      in case validationResultErrors vr of
           []    -> Nothing
           err:_ -> Just err
    -- Every type-definition name across all packages, used as the in-scope
    -- vocabulary so nominal references (e.g. hydra.core.Name) aren't
    -- mistaken for undefined type variables. Mirrors the kernel-only
    -- version's kernelTypeNames, generalized across every validated package.
    allTypeNames = S.fromList
      [ typeDefinitionName td
      | m <- allMods, DefinitionType td <- moduleDefinitions m ]
    typeFailures =
      [ (m, td, e) | m <- allMods, td <- typeDefsOf m, Just e <- [validateTypeDef td] ]
    termFailures =
      [ (m, td, e) | m <- allMods, td <- termDefsOf m, Just e <- [validateTermDef td] ]

-- | Convert a Definition to the Binding shape that elementsToGraph (and other
-- Binding-based kernel APIs) expects. A DefinitionTerm carries directly across;
-- a DefinitionType has its body re-encoded as a term and tagged with the
-- "type -> hydra.core.Type" annotation that downstream code uses to recognise
-- native types.
definitionAsBinding :: Definition -> Binding
definitionAsBinding (DefinitionTerm td) = Binding {
    bindingName = termDefinitionName td,
    bindingTerm = termDefinitionBody td,
    bindingTypeScheme = termSignatureToTypeScheme <$> termDefinitionSignature td}
definitionAsBinding (DefinitionType td) = Binding {
    bindingName = typeDefinitionName td,
    bindingTerm = TermAnnotated $ AnnotatedTerm {
      annotatedTermBody = EncodeCore.type_ (typeSchemeBody (typeDefinitionBody td)),
      annotatedTermAnnotation = TermMap $ M.fromList [
        (TermVariable (Name "type"), TermVariable (Name "hydra.core.Type"))]},
    bindingTypeScheme = Just (TypeScheme [] (TypeVariable (Name "hydra.core.Type")) Nothing)}
-- TODO(#156): Implement DefinitionPrimitive handling once primitive modules land. For now, primitives don't appear in modules that go through this function.
definitionAsBinding (DefinitionPrimitive pd) = Binding {
    bindingName = primitiveDefinitionName pd,
    bindingTerm = TermLiteral (LiteralString (maybe "" (\em -> maybe "" id (entityMetadataDescription em)) (primitiveDefinitionMetadata pd))),
    bindingTypeScheme = Just (termSignatureToTypeScheme (primitiveDefinitionSignature pd))}

-- | Extract a module's definitions in the legacy Binding view, suitable for
-- feeding elementsToGraph or any other API that still operates on Bindings.
moduleAsBindings :: Module -> [Binding]
moduleAsBindings = map definitionAsBinding . moduleDefinitions


-- | Generate and write the lexicon file (IO wrapper).
writeLexicon :: FilePath -> [Module] -> IO ()
writeLexicon path kernelModules = do
  case CodeGeneration.inferAndGenerateLexicon emptyInferenceContext bootstrapGraph kernelModules of
    Left err -> fail $ "Lexicon generation failed: " ++ showError err
    Right content -> do
      writeFile path content
      putStrLn $ "Lexicon written to " ++ path

----------------------------------------

-- | IO wrapper for generateCoderModules. Evaluates the Either and handles errors.
generateCoderModulesIO :: (InferenceContext -> Graph -> Module -> Either Error.Error (Maybe Module)) -> String -> [Module] -> [Module] -> IO [Module]
generateCoderModulesIO codec label universeModules typeModules = do
    let cx = emptyInferenceContext
    case CodeGeneration.generateCoderModules codec bootstrapGraph universeModules typeModules cx of
      Left err -> fail $ "Failed to generate " ++ label ++ " modules: " ++ showError err
      Right results -> return results

generateDecoderModules :: [Module] -> [Module] -> IO [Module]
generateDecoderModules = generateCoderModulesIO Decoding.decodeModule "decoder"

generateEncoderModules :: [Module] -> [Module] -> IO [Module]
generateEncoderModules = generateCoderModulesIO Encoding.encodeModule "encoder"

-- | Pure variants for use in contexts that cannot use IO (e.g. test utilities).
-- Fails with an error message on Left — only call where synthesis is expected to succeed.
generateDecoderModulesPure :: [Module] -> [Module] -> Either String [Module]
generateDecoderModulesPure universeModules typeModules =
  case CodeGeneration.generateCoderModules Decoding.decodeModule bootstrapGraph universeModules typeModules emptyInferenceContext of
    Left err -> Left ("Decoder synthesis failed: " ++ showError err)
    Right ms  -> Right ms

generateEncoderModulesPure :: [Module] -> [Module] -> Either String [Module]
generateEncoderModulesPure universeModules typeModules =
  case CodeGeneration.generateCoderModules Encoding.encodeModule bootstrapGraph universeModules typeModules emptyInferenceContext of
    Left err -> Left ("Encoder synthesis failed: " ++ showError err)
    Right ms  -> Right ms

----------------------------------------

-- | Generate encoder/decoder Source modules for a list of type modules.
-- These are Source modules that define `module_` bindings containing the encoder Modules as Terms.
generateCoderSourceModules :: ([Module] -> [Module] -> IO [Module]) -> [Module] -> [Module] -> IO [Module]
generateCoderSourceModules generate universeModules typeModules = do
  sourceMods <- generate universeModules typeModules
  return $ fmap CodeGeneration.moduleToSourceModule sourceMods

generateDecoderSourceModules :: [Module] -> [Module] -> IO [Module]
generateDecoderSourceModules = generateCoderSourceModules generateDecoderModules

generateEncoderSourceModules :: [Module] -> [Module] -> IO [Module]
generateEncoderSourceModules = generateCoderSourceModules generateEncoderModules

----------------------------------------
-- DSL Module Generation
----------------------------------------

generateDslModules :: [Module] -> [Module] -> IO [Module]
generateDslModules = generateCoderModulesIO Dsls.dslModule "DSL"

----------------------------------------
-- Module Inference
----------------------------------------

-- | IO wrapper for inferModules. Evaluates the Either and handles errors.
inferModulesIO :: [Module] -> [Module] -> IO [Module]
inferModulesIO universeMods targetMods = do
  case CodeGeneration.inferModules emptyInferenceContext bootstrapGraph universeMods targetMods of
    Left err -> fail $ "Type inference failed: " ++ showError err
    Right mods -> return mods

-- | IO wrapper for inferModulesGiven (incremental inference). The
-- universe modules already carry inferred TypeSchemes on their term
-- bindings (loaded from JSON); only the target modules are re-inferred,
-- using the typed universe as context.
inferModulesGivenIO :: [Module] -> [Module] -> IO [Module]
inferModulesGivenIO universeMods targetMods = do
  case CodeGeneration.inferModulesGiven emptyInferenceContext bootstrapGraph universeMods targetMods of
    Left err -> fail $ "Incremental type inference failed: " ++ showError err
    Right mods -> return mods

-- | Per-package iterative inference driver for #381.
--
-- Replaces the flat-universe 'inferModulesIO' fallback in the slow path
-- of 'writeModulesJsonPackageSplit'. Processes packages in dependency
-- order (topo sort over each package.json's "dependencies" field) and
-- runs 'inferModulesGiven' once per package, threading the typed-so-far
-- output of upstream packages through as the universe.
--
-- Peak memory per iteration is bounded by:
--    type-schemes of transitive deps + bindings of the focus package
-- rather than the universe-wide
--    bindings of every module + full substitution map + constraint set.
--
-- Returns the full set of inferred target modules (concatenated across
-- packages, in topo order). The caller's downstream JSON-write / digest
-- refresh code is unchanged — only the universe-load shape is different.
--
-- Each iteration writes its package's JSON to disk *immediately* via
-- 'writePackageSplitJson' before moving on. That side effect forces the
-- inferred modules through to NF, which is what dodges the lazy
-- thunk chain that wrecked the per-SCC attempt
-- (see docs/history/inferModules-per-scc-attempt.md on staging).
inferAndWriteByPackage :: RoutingMap -> FilePath -> [Module] -> [Module] -> IO ()
inferAndWriteByPackage routingMap distJsonRoot universeMods mods =
  inferAndWriteByPackageSeeded routingMap distJsonRoot M.empty M.empty [] universeMods mods

-- | Source-set-aware variant of 'inferAndWriteByPackageSeeded'. The main
-- path writes into src/main/json (srcSet = "main"); the test-side writer
-- passes srcSet = "test" so the same per-package driver routes inferred
-- modules into src/test/json. Everything else (seed maps, topo iteration,
-- bounded-memory accumulator) is identical.
inferAndWriteByPackageSeeded
  :: RoutingMap
  -> FilePath
  -> M.Map Name TypeScheme
  -> M.Map Name TypeScheme
  -> [Module]
  -> [Module]
  -> [Module]
  -> IO ()
inferAndWriteByPackageSeeded routingMap = inferAndWriteByPackageSeededFor routingMap "main"

-- | As 'inferAndWriteByPackage' but with a 'seedSchemes' map of
-- 'Name -> TypeScheme' for bindings that are already typed (e.g. clean
-- modules loaded from JSON with their TypeSchemes baked in). The seed
-- pre-populates the typed-so-far accumulator so downstream packages can
-- resolve cross-package type references against it without paying for
-- re-inference, and *without* retaining the full Module structure of
-- the upstream packages — only their (Name, TypeScheme) pairs.
--
-- Used by 'tryIncrementalInference' to thread the JSON-loaded clean
-- universe through as type-resolution context while running the
-- per-package re-inference only on the dirty subset.
--
-- Memory shape: the foldl accumulator is a 'Map Name TypeScheme', not
-- a '[Module]'. After each package writes its JSON, only the newly
-- inferred bindings' (Name, TypeScheme) pairs are folded into the
-- accumulator; the inferred Module values themselves are dropped on the
-- floor so GC can reclaim their term bodies. Cf. the prior shape, which
-- retained every prior package's full payload (term bodies, annotations,
-- everything) — fine for ~10 modules dirty, OOM at -M6G for ~250.
inferAndWriteByPackageSeededFor
  :: RoutingMap
  -> String                 -- ^ source set ("main" | "test"); selects src/<set>/json
  -> FilePath
  -> M.Map Name TypeScheme  -- ^ accumulated term-binding schemes from prior packages
  -> M.Map Name TypeScheme  -- ^ accumulated type-def schemes from prior packages
  -> [Module]               -- ^ schema-context-only modules (e.g. clean modules
                            --   loaded from JSON); used to build the JSON-write
                            --   schemaMap once and then dropped. Never iterated.
  -> [Module]               -- ^ universe modules that participate in grouping
                            --   and iteration (the "dirty" set in the warm
                            --   incremental path; the full set in the cold path).
  -> [Module]               -- ^ target subset to re-infer + write
  -> IO ()
inferAndWriteByPackageSeededFor
    routingMap srcSet distJsonRoot seedBindingSchemes seedSchemaSchemes schemaContextMods universeMods mods = do
  -- Build the JSON-write schemaMap ONCE, up front, from the full module
  -- universe (schemaContextMods + universeMods). The encoder needs every
  -- universe type reachable from this map — in particular
  -- hydra.packaging.Module, without which Maybe String fields encode as
  -- single-element arrays (see comment on writePackageSplitJson).
  --
  -- After this point, schemaContextMods is unreferenced and can be GC'd.
  let schemaMap = buildSchemaMap
        (modulesToGraph (schemaContextMods ++ universeMods)
                        (schemaContextMods ++ universeMods))
  -- Group both the target set (mods, what we write) and the full universe
  -- (universeMods, what participates in type resolution) by owning package.
  -- pkgsInScope is the union — every package whose modules appear in either
  -- set must take its turn in the iteration so that cross-package type
  -- references resolve. Packages that have universe modules but no target
  -- modules (e.g. hydra-java when #344 excludes its JSON from this write
  -- pass) still get inferred so their TypeSchemes seed the typed universe
  -- for downstream packages.
  let targetGroups    = groupByPackageIn routingMap mods
      universeGroups  = groupByPackageIn routingMap universeMods
      pkgToMods       = M.fromList targetGroups
      pkgToUniverse   = M.fromList universeGroups
      pkgsInScope     = L.nub (map fst universeGroups ++ map fst targetGroups)
  -- Cross-package inference-order edges derived from actual module
  -- dependencies (#546). package.json deps alone are insufficient when a module
  -- in package A references (via moduleDependencies) a module owned by package
  -- B that does NOT sit above A in the package.json graph. The canonical case:
  -- the kernel's hydra.test.testSuite aggregate references hydra.test.build.*
  -- (owned by hydra-build), yet hydra-build depends on hydra-kernel — so the
  -- package graph would infer hydra-kernel first and fail to resolve the
  -- build-test allTests bindings. Projecting each module's moduleDependencies
  -- through the routing map yields the true inference order (provider package
  -- before consumer). Namespaces not in the routing map (e.g. kernel type
  -- modules absent from the test universe) are skipped via the safe lookup.
  let routedPkg = namespaceToPackageMaybeIn routingMap
      -- Only modules actually iterated in THIS pass (universeMods) constrain the
      -- ordering; dependencies on modules outside it (e.g. kernel MAIN modules,
      -- which are pre-seeded via seedBindingSchemes in the test pass) are already
      -- satisfied and must NOT create edges — otherwise the genuine package-level
      -- cycle reappears (hydra-build test deps on kernel main vs. kernel test-suite
      -- deps on hydra-build test). Restricting the provider side to universeMods
      -- keeps only the real in-pass constraints (kernel test-suite -> hydra-build
      -- test), which are acyclic.
      universeNs = S.fromList (map moduleName universeMods)
      moduleDepEdges =
        [ (consumerPkg, providerPkg)
        | m <- universeMods
        , Just consumerPkg <- [routedPkg (moduleName m)]
        , dep <- moduleDependencies m
        , let depNs = moduleDependencyModule dep
        , S.member depNs universeNs
        , Just providerPkg <- [routedPkg depNs]
        , providerPkg /= consumerPkg
        , providerPkg `elem` pkgsInScope
        , consumerPkg `elem` pkgsInScope ]
  -- Build the package dep graph from each package's package.json, then union in
  -- the module-dependency-derived edges above. Conflict resolution: a
  -- module-dependency edge reflects an ACTUAL reference in THIS source set, so
  -- it wins over a contradicting package.json edge. Concretely, for the test
  -- pass the kernel test-suite references hydra-build's test modules
  -- (kernel -> hydra-build), while package.json records hydra-build -> kernel
  -- (a MAIN-linkage fact, irrelevant to test ordering here). Dropping the
  -- contradicted package.json edge for this pass avoids a spurious 2-cycle while
  -- preserving the true inference order; kernel main is already seeded, so the
  -- dropped edge costs nothing.
  let modDepEdgeSet = S.fromList moduleDepEdges
  pkgDeps <- CM.forM pkgsInScope $ \p -> do
    deps <- loadPackageDeps p
    -- Restrict deps to packages actually present in the in-scope set, dropping
    -- any package.json edge (p -> d) contradicted by a module-dep edge (d -> p).
    let jsonDeps = [ d | d <- filter (`elem` pkgsInScope) deps
                       , not (S.member (d, p) modDepEdgeSet) ]
        modDeps  = [ prov | (cons, prov) <- moduleDepEdges, cons == p ]
    return (p, L.nub (jsonDeps ++ modDeps))
  -- Topological sort: deps first, then dependents.
  topoResult <- case topologicalSort pkgDeps of
    Right ordered -> return ordered
    Left cycles -> fail $ "inferAndWriteByPackage: package dep graph has cycles: "
                       ++ show cycles
  putStrLn $ "  Per-package inference: " ++ show (length topoResult)
    ++ " packages in dep order: " ++ L.intercalate " -> " topoResult
  -- Iterate packages in topo order, accumulating typed-so-far bindings as
  -- a Map Name TypeScheme. Strictness: each iteration ends with a
  -- writePackageSplitJson call, which forces the inferred modules through
  -- JSON serialization. After the write, we extract (Name, TypeScheme)
  -- pairs into the accumulator and drop the inferred Modules so GC can
  -- reclaim their term bodies. The Map's spine and TypeScheme nodes stay
  -- in memory across iterations, but a TypeScheme is typically 1-3 orders
  -- of magnitude smaller than the term body it types.
  let processOne
        :: (M.Map Name TypeScheme, M.Map Name TypeScheme)
        -> String
        -> IO (M.Map Name TypeScheme, M.Map Name TypeScheme)
      processOne (accBindingSchemes, accSchemaSchemes) pkg = do
        let pkgTargets   = M.findWithDefault [] pkg pkgToMods
            pkgUniverse  = M.findWithDefault [] pkg pkgToUniverse
            targetNs     = S.fromList (map moduleName pkgTargets)
            -- Native-owned packages (#344: hydra-jvm, hydra-java, hydra-python) are NEVER
            -- inferred here. Their canonical JSON is produced by the native
            -- generators, and their full universe includes the native coder
            -- module (e.g. hydra.java.coder), which references kernel term
            -- bindings by name (hydra.annotations.commentsFromFieldType) that
            -- are not in this pass's by-name accumulator — re-inferring it
            -- fails with "no such binding". Their existing JSON TypeSchemes are
            -- still harvested below (free, no inference), so downstream by-name
            -- refs like hydra-scala -> hydra.jvm.serde.escapeJavaString still
            -- resolve (#470). Before #466 these packages had the dsl wrappers as
            -- write targets so inferTargets was non-empty and never hit the
            -- null-pkgTargets fallback; now that the wrappers are excluded from
            -- the write universe (they're derived modules; inference must not
            -- run on them) the fallback would otherwise infer the whole native
            -- universe — hence this explicit skip.
            isNativeOwnedPkg = pkg == "hydra-jvm" || pkg == "hydra-java" || pkg == "hydra-python"
            -- Infer only this package's write targets — re-inferring its whole
            -- universe (e.g. the full Java coder) blows the CI heap cap. The
            -- universe still participates as type-resolution context below, and
            -- its existing TypeSchemes (incl. native-JSON #344 signatures) are
            -- harvested into the accumulator below to seed downstream by-name
            -- refs like hydra-scala -> hydra.jvm.serde.escapeJavaString (#470).
            inferTargets
              | isNativeOwnedPkg = []
              | null pkgTargets  = pkgUniverse
              | otherwise        = pkgTargets
        putStrLn $ "  [" ++ pkg ++ "] "
          ++ show (length pkgTargets) ++ " write / "
          ++ show (length inferTargets) ++ " infer / "
          ++ show (M.size accBindingSchemes) ++ " typed-so-far term schemes / "
          ++ show (M.size accSchemaSchemes) ++ " type schemas"
        inferred <- if null inferTargets
          then return []
          else case inferModulesGivenSchemes
                    (InferenceContext 0 []) bootstrapGraph
                    accBindingSchemes accSchemaSchemes
                    pkgUniverse inferTargets of
                  Left (Error.ErrorResolution (Error.ResolutionErrorNoSuchBinding e))
                    | any (\pfx -> L.isPrefixOf pfx (unName (Error.noSuchBindingErrorName e)))
                           ["hydra.jvm.", "hydra.java.", "hydra.python."]
                    -> do
                      putStrLn $ "  WARNING: skipping inference for " ++ pkg
                               ++ " (missing native binding: "
                               ++ unName (Error.noSuchBindingErrorName e)
                               ++ "; cold tree — Phase 1.5 will seed it)"
                      return []
                  Left err -> fail $ "Per-package inference failed for "
                                  ++ pkg ++ ": " ++ showError err
                                  ++ " (raw: " ++ show err ++ ")"
                  Right ms -> return ms
        -- Filter inferred modules down to those in the original target set
        -- for the write step. Packages with no target mods (e.g. hydra-java
        -- under #344) write nothing; their inferred schemes still flow into
        -- the accumulators so dependents can resolve cross-package refs.
        let toWrite = filter (\m -> moduleName m `S.member` targetNs) inferred
        CM.when (not (null toWrite)) $ do
          -- Write each module using the pre-built schemaMap. The schemaMap
          -- covers the full universe (built once at the top of this
          -- function), so encoder lookups for hydra.packaging.Module and
          -- other cross-package schema types resolve correctly. Without
          -- this, prior packages' types are absent from the per-iteration
          -- schemaMap and Maybe String fields mis-serialize as arrays.
          let pkgDir = distJsonRoot FP.</> pkg FP.</> "src" FP.</> srcSet FP.</> "json"
          putStrLn $ "  " ++ pkg ++ ": " ++ show (length toWrite)
            ++ " modules -> " ++ pkgDir
          mapM_ (writeModuleJson schemaMap pkgDir) toWrite
        -- Force the writes to disk before folding the new schemes into
        -- the accumulators; this also forces 'inferred' to NF, breaking
        -- any lazy thunk chain across iterations.
        --
        -- Schemes are harvested from the package's FULL universe, not just the
        -- (re-)inferred targets. Universe modules excluded from inference still
        -- carry the TypeSchemes the native generators already emitted in their
        -- JSON signatures (#344/#505: hydra.{jvm,java,python}.* — produced by
        -- the native Java/Python generators). Harvesting those existing
        -- signatures is free (no inference), and it seeds bindings like
        -- hydra.jvm.serde.escapeJavaString so a downstream by-name reference
        -- (hydra-scala -> that binding) resolves. (#470)
        --
        -- A freshly (re-)inferred target's scheme must override the native
        -- signature for the same name, so 'inferred' is listed AFTER
        -- 'pkgUniverse': Data.Map.fromList keeps the LAST value for a duplicate
        -- key, so the inferred scheme wins.
        let schemeSources = pkgUniverse ++ inferred
            !newBindingSchemes = M.fromList
              [ (termDefinitionName td, ts)
              | m <- schemeSources
              , DefinitionTerm td <- moduleDefinitions m
              , Just ts <- [termSignatureToTypeScheme <$> termDefinitionSignature td]
              ]
            !newSchemaSchemes = M.fromList
              [ (typeDefinitionName td, normalizeTypeScheme (typeDefinitionBody td))
              | m <- schemeSources
              , DefinitionType td <- moduleDefinitions m
              ]
            !accBindingSchemes' = M.union newBindingSchemes accBindingSchemes
            !accSchemaSchemes'  = M.union newSchemaSchemes  accSchemaSchemes
        return (accBindingSchemes', accSchemaSchemes')
  _ <- L.foldl' (\ioAcc pkg -> ioAcc >>= \acc -> processOne acc pkg)
                (return (seedBindingSchemes, seedSchemaSchemes))
                topoResult
  return ()

-- | Normalize a TypeScheme by pulling any TypeForall wrappers from its
-- body into typeSchemeVariables. DSL-authored type definitions encode
-- polymorphism as nested TypeForalls in the body (with empty variables);
-- the kernel's 'schemaGraphToTypingEnvironment' applies this unwrapping
-- at schema-graph lookup time. When we feed a TypeScheme directly into
-- 'graphSchemaTypes' (bypassing the schema graph), we have to apply the
-- same normalization ourselves; otherwise downstream consumers that
-- pattern-match on the body shape (e.g. expecting `record{...}`) hit
-- an UnexpectedShape error against the raw `∀.∀.…record{...}` form.
normalizeTypeScheme :: TypeScheme -> TypeScheme
normalizeTypeScheme ts =
  let unwrapped = fTypeToTypeScheme (typeSchemeBody ts)
  in TypeScheme
       { typeSchemeVariables   = typeSchemeVariables ts ++ typeSchemeVariables unwrapped
       , typeSchemeBody        = typeSchemeBody unwrapped
       , typeSchemeConstraints = typeSchemeConstraints ts
       }

-- | Map-based variant of 'Hydra.Codegen.inferModulesGiven' that takes
-- pre-built maps of already-typed term bindings and schema types, and
-- merges them into the inference graph directly. This lets the caller
-- thread small Maps across iterations instead of full 'Module' values.
--
-- Used by 'inferAndWriteByPackageSeeded' to keep the per-package
-- accumulator small: only (Name, TypeScheme) pairs are retained across
-- iterations; the upstream packages' inferred Modules are dropped after
-- their JSON is written.
--
-- Closure / bindingsToInfer logic mirrors the kernel
-- 'inferModulesGiven'; the differences are (a) graphBoundTypes is
-- augmented with the caller's accumulated term schemes, (b)
-- graphSchemaTypes is augmented with the caller's accumulated type-def
-- schemes, and (c) the universe is sized to the current package only.
inferModulesGivenSchemes
  :: InferenceContext
  -> Graph
  -> M.Map Name TypeScheme  -- ^ accumulated term-binding schemes from prior packages
  -> M.Map Name TypeScheme  -- ^ accumulated type-def schemes (graphSchemaTypes)
  -> [Module]               -- ^ current-package universe (small)
  -> [Module]               -- ^ target subset to re-infer + write
  -> Either Error [Module]
inferModulesGivenSchemes cx bsGraph accBindingSchemes accSchemaSchemes universeMods targetMods =
    let g0 = CodeGeneration.modulesToGraph bsGraph universeMods universeMods
        g0Augmented = g0
          { graphBoundTypes  = M.union (graphBoundTypes g0)  accBindingSchemes
          , graphSchemaTypes = M.union (graphSchemaTypes g0) accSchemaSchemes
          }
        nsMap = M.fromList [(moduleName m, m) | m <- universeMods]
        closureMods = CodeGeneration.moduleDepsTransitive nsMap targetMods
        targetNamespaces = S.fromList (map moduleName targetMods)
        termBindingsOf m =
          [ Binding { bindingName = termDefinitionName td
                    , bindingTerm = termDefinitionBody td
                    , bindingTypeScheme = termSignatureToTypeScheme <$> termDefinitionSignature td
                    }
          | DefinitionTerm td <- moduleDefinitions m ]
        (bindingsToInfer, untouchedTypedBindings) =
          let go m =
                let isTarget = moduleName m `S.member` targetNamespaces
                    bs = termBindingsOf m
                in if isTarget
                     then (bs, [])
                     else ( filter (Y.isNothing . bindingTypeScheme) bs
                          , filter (Y.isJust . bindingTypeScheme) bs )
              parts = map go closureMods
          in (concatMap fst parts, concatMap snd parts)
    in case Inference.inferGraphTypes cx bindingsToInfer g0Augmented of
        Left e -> Left e
        Right ((_, newlyInferred), _) ->
          let allInferred = newlyInferred ++ untouchedTypedBindings
          in Right (map (CodeGeneration.refreshModule allInferred) targetMods)

----------------------------------------
-- JSON Module Export
----------------------------------------

-- | Build a schema map (Name -> Type) from a graph's schema types.
-- Used by the JSON encoder/decoder to resolve type variables.
buildSchemaMap :: Graph -> M.Map Name Type
buildSchemaMap g = M.map extractType (graphSchemaTypes g)
  where
    extractType (TypeScheme _ t _) = stripTop t
    stripTop (TypeAnnotated (AnnotatedType t _)) = stripTop t
    stripTop t = t

-- | Write a single module to a JSON file.
-- The file path is derived from the module namespace.
writeModuleJson :: M.Map Name Type -> FilePath -> Module -> IO ()
writeModuleJson schemaMap basePath mod = do
    case CodeGeneration.moduleToJson schemaMap mod of
      Left err -> fail $ "Failed to convert module to JSON: " ++ unModuleName (moduleName mod) ++ ": " ++ showError err
      Right jsonStr -> do
        let filePath = basePath FP.</> CodeGeneration.moduleNameToPath (moduleName mod) ++ ".json"
            newContent = jsonStr ++ "\n"
        SD.createDirectoryIfMissing True $ FP.takeDirectory filePath
        -- Skip the write (and the putStrLn spam) when the on-disk content
        -- is byte-identical. Important for DSL-wrapper generation, which
        -- runs unconditionally after a cache-hit main pass; without this,
        -- warm runs rewrite ~25 DSL-wrapper JSON files every time.
        exists <- SD.doesFileExist filePath
        skip <- if exists
                  then do old <- readFile filePath
                          -- Force the whole string so the handle closes
                          -- before the subsequent writeFile reopens the path.
                          length old `seq` return (old == newContent)
                  else return False
        CM.unless skip $ do
          writeFile filePath newContent
          putStrLn $ "Wrote: " ++ filePath

-- | Write multiple modules to JSON files.
-- Each module is written to basePath/<namespace-path>.json
-- If doInfer is True, type inference is performed on the modules first.
-- The universe modules are used for type inference context (may include more modules
-- than those being written). If not inferring, the universe is ignored.
--
-- When doInfer is True, this honors a content-hash cache at
-- <digestPath basePath>: if every universe module's DSL source hash matches
-- the stored digest and every target module's JSON file already exists,
-- inference and writes are skipped entirely. Otherwise the full path runs
-- and the digest is overwritten on success. The cache is all-or-nothing
-- (per the 2026-04-16 inferModulesGiven redesign).
writeModulesJson :: Bool -> FilePath -> [Module] -> [Module] -> IO ()
writeModulesJson doInfer basePath universeMods mods = do
  hit <- if doInfer then tryCacheHit basePath universeMods mods else return Nothing
  case hit of
    Just _ ->
      putStrLn $ "  Cache hit (" ++ show (length universeMods) ++ " modules clean); skipping inference and writes."
    Nothing -> do
      mods' <- if doInfer then inferModulesIO universeMods mods else return mods
      let graph = modulesToGraph universeMods universeMods
          schemaMap = buildSchemaMap graph
      mapM_ (writeModuleJson schemaMap basePath) mods'
      CM.when doInfer $ refreshDigest basePath universeMods

-- | Write multiple modules to JSON files, routing each module to the
-- dist/json/<package>/src/main/json/ directory of its owning package.
--
-- Like 'writeModulesJson', but fans the modules out across package
-- subdirectories based on 'namespaceToPackage'. Inference and schema-map
-- construction happen once over the full universe, so each per-module write
-- is as cheap as the single-directory version.
--
-- Cache layout (after 2026-04-18 split; build/ relocation in #379):
--
--   * Per-package digest at dist/json/<pkg>/build/main/digest.json
--     — covers the namespaces routed to <pkg>. Stage 3+ will exploit
--     per-package freshness; for now it's recorded so callers can rely
--     on it.
--   * Universe-wide digest at <distJsonRoot>/build/digest.json — kept
--     for backwards compatibility with the existing cache-hit semantics
--     (universe-wide all-or-nothing). Removed once per-package
--     freshness checks are wired in.
--
-- See 'writeModulesJson' for the (non-split) caching semantics.
writeModulesJsonPackageSplit :: RoutingMap -> Bool -> FilePath -> [Module] -> [Module] -> IO ()
writeModulesJsonPackageSplit routingMap doInfer distJsonRoot universeMods mods = do
  hit <- if doInfer then tryCacheHitSplit routingMap distJsonRoot universeMods mods else return Nothing
  case hit of
    Just _ -> do
      putStrLn $ "  Cache hit (" ++ show (length universeMods) ++ " modules clean); skipping inference and writes."
      -- Even on a cache hit, ensure per-package digests exist on disk
      -- so downstream tools (Stage 3 per-target freshness checks) have
      -- something to compare against. Cheap because hashing reads the
      -- DSL files but skips inference + JSON writes.
      -- Use the unfiltered universe (not the write-filtered 'mods'): the
      -- per-package input digest must cover native-generator-owned
      -- hydra.<lang>.* modules even though their JSON write is excluded
      -- (#344). See refreshPerPackageDigests for the full rationale (#400).
      CM.when doInfer $ ensurePerPackageDigests routingMap distJsonRoot universeMods
    Nothing -> do
      -- Try the incremental path: partition modules into clean
      -- (DSL hash unchanged) and dirty. Re-infer only the dirty ones
      -- against a typed universe loaded from JSON, and write JSON only
      -- for the dirty subset. Falls through to full inference if
      -- the incremental setup fails.
      result <- if doInfer
                  then tryIncrementalInference routingMap distJsonRoot universeMods mods
                  else return (Just (IncrementalFull mods))
      case result of
        Just (IncrementalFull allMods) -> do
          -- Full inference was needed; write JSON for every module.
          writePackageSplitJson routingMap distJsonRoot universeMods allMods allMods
          CM.when doInfer $ do
            refreshDigestAt (packageSplitDigestAnchor distJsonRoot) universeMods
            refreshPerPackageDigests routingMap distJsonRoot universeMods allMods
        Just (IncrementalPartial allMods dirtyMods) -> do
          -- Incremental inference succeeded; only rewrite JSON for the
          -- dirty modules. Clean modules' on-disk JSON is already
          -- up-to-date (we loaded them from there to build the typed
          -- universe).
          putStrLn $ "  Writing JSON for " ++ show (length dirtyMods)
            ++ " dirty modules (skipping " ++ show (length allMods - length dirtyMods)
            ++ " clean)"
          writePackageSplitJson routingMap distJsonRoot universeMods allMods dirtyMods
          CM.when doInfer $ do
            refreshDigestAt (packageSplitDigestAnchor distJsonRoot) universeMods
            refreshPerPackageDigests routingMap distJsonRoot universeMods allMods
        Just IncrementalPartialPreWritten -> do
          -- #381 follow-up: tryIncrementalInference routed through
          -- inferAndWriteByPackage, which already inferred and wrote JSON
          -- per package. Skip the write step here; refresh digests from
          -- the caller's own universeMods (the driver discards inferred
          -- modules to keep memory bounded — only their schemes flow
          -- through the foldl).
          CM.when doInfer $ do
            refreshDigestAt (packageSplitDigestAnchor distJsonRoot) universeMods
            refreshPerPackageDigests routingMap distJsonRoot universeMods mods
        Nothing -> do
          -- #381: per-package iterative inference. Replaces the
          -- universe-wide 'inferModulesIO universeMods mods' that runs
          -- one giant inference over every binding (peak >7 GB on CI).
          -- 'inferAndWriteByPackage' processes packages in topo order
          -- and writes each package's JSON before moving to the next,
          -- bounding peak memory by per-package size. JSON writes are
          -- emitted as a side effect inside the loop; we still need to
          -- refresh the universe-wide + per-package digests below.
          putStrLn "  Incremental inference unavailable; running per-package inference."
          inferAndWriteByPackage routingMap distJsonRoot universeMods mods
          CM.when doInfer $ do
            refreshDigestAt (packageSplitDigestAnchor distJsonRoot) universeMods
            refreshPerPackageDigests routingMap distJsonRoot universeMods mods

-- | Write test-suite modules to JSON via the per-package incremental
-- inference driver (#395), the test-side analogue of
-- 'writeModulesJsonPackageSplit'.
--
-- The flat-universe path this replaces ('writeModulesJson') ran one
-- Algorithm-W pass over the entire main+test universe (~210 + 58
-- modules) on every cache miss — ~17 min wall, ~8 GB RSS — because any
-- change anywhere in the kernel-types or coder DSL universe invalidates
-- the test digest. See #395.
--
-- Structurally the test path is simpler than the main path: a change to
-- test sources never re-infers main modules, so there's no dirty/clean
-- partitioning or transitive-closure step. The main universe is *always*
-- typed context. We therefore:
--
--   1. Load the already-typed main modules from their src/main/json
--      (they carry inferred TypeSchemes) and extract (Name, TypeScheme)
--      seed maps — the same maps 'tryIncrementalInference' builds for the
--      main path. Carrying Maps (not [Module]) lets GC reclaim the main
--      term bodies once inference has consulted the schemes.
--   2. Run the per-package driver over *only* the test modules, seeded
--      with those maps, writing into src/test/json and refreshing the
--      test digest.
--
-- On a cache hit (test universe hashes unchanged), skip entirely — the
-- ~0.8s fast path. If the main-JSON seed load fails for any reason, fall
-- back to the old flat 'writeModulesJson' so we never regress
-- correctness, only performance.
--
-- @mainMods@ is the typed context universe (mainModules); @testMods@ is
-- the set to infer + write. The cache key hashes the union, so a main
-- source change still invalidates the test cache (a test module may
-- reference the changed binding) — but the *re-inference* only runs over
-- the test modules, against main schemes loaded from JSON.
writeTestModulesJson :: RoutingMap -> FilePath -> [Module] -> [Module] -> IO ()
writeTestModulesJson routingMap distJsonRoot mainMods testMods = do
  let universeMods = mainMods ++ testMods
      testPaths    = [ distJsonRoot FP.</> pkg FP.</> "src" FP.</> "test" FP.</> "json"
                                     FP.</> CodeGeneration.moduleNameToPath (moduleName m) ++ ".json"
                     | (pkg, pkgMods) <- groupByPackageIn routingMap testMods, m <- pkgMods ]
      -- #551: every package owning test modules has its own build/test digest,
      -- and freshness must be checked against ALL of them, not just the
      -- (somewhat arbitrary) first one. Reading only one package's digest let
      -- a stale/missing per-package digest slip past undetected whenever that
      -- package's own file happened to still match the current universe hash
      -- while another test-owning package's digest did not — the existence
      -- check (every digest present) and the freshness check (every digest
      -- fresh) must span the same set. See #546 for the original
      -- existence-only guard this extends.
      perPkgTestDigests = [ perPackageDigestPathFor "test" distJsonRoot pkg
                          | (pkg, _) <- groupByPackageIn routingMap testMods ]
  allDigestsPresent <- Prelude.and <$> Prelude.mapM SD.doesFileExist perPkgTestDigests
  hit <- if allDigestsPresent
           then checkCacheHitAll perPkgTestDigests universeMods testPaths
           else return Nothing
  case hit of
    Just _ ->
      putStrLn $ "  Cache hit (" ++ show (length universeMods)
        ++ " modules clean); skipping test inference and writes."
    Nothing -> do
      -- Load the typed main universe from its src/main/json. These carry
      -- the inferred TypeSchemes that seed test-module inference.
      let mainNs = map moduleName mainMods
      loaded <- E.try (loadCleanFromJson routingMap distJsonRoot universeMods mainNs)
                :: IO (Either E.SomeException [Module])
      case loaded of
        Left e -> do
          putStrLn $ "  Test incremental seed-load failed (" ++ show e
            ++ "); falling back to flat-universe inference."
          flatFallback universeMods
        Right mainLoaded -> do
          let seedBindingSchemes = M.fromList
                [ (termDefinitionName td, ts)
                | m <- mainLoaded
                , DefinitionTerm td <- moduleDefinitions m
                , Just ts <- [termSignatureToTypeScheme <$> termDefinitionSignature td]
                ]
              seedSchemaSchemes = M.fromList
                [ (typeDefinitionName td, normalizeTypeScheme (typeDefinitionBody td))
                | m <- mainLoaded
                , DefinitionType td <- moduleDefinitions m
                ]
          putStrLn $ "  Per-package test inference: "
            ++ show (length testMods) ++ " test modules / "
            ++ show (M.size seedBindingSchemes) ++ " seeded term schemes / "
            ++ show (M.size seedSchemaSchemes) ++ " seeded type schemas"
          -- Driver iteration universe = testMods only; the seed Maps carry
          -- the main types so cross-package refs resolve. mainLoaded is the
          -- schema-context-only set so the JSON-write schemaMap (built once)
          -- covers main types like hydra.packaging.Module.
          inferAndWriteByPackageSeededFor routingMap "test" distJsonRoot
            seedBindingSchemes seedSchemaSchemes
            mainLoaded testMods testMods
          refreshTestDigests universeMods
  where
    -- #546: refresh a test digest for EVERY package that owns test modules,
    -- not just the single anchor package. Before hydra-build, all test modules
    -- belonged to hydra-kernel so one anchor sufficed; now the anchor
    -- (alphabetically-first package = hydra-build) would leave the kernel test
    -- digest unwritten, which the downstream per-package assemblers require.
    refreshTestDigests universeMods =
      CM.forM_ (groupByPackageIn routingMap testMods) $ \(pkg, _) ->
        refreshDigestAt (perPackageDigestPathFor "test" distJsonRoot pkg) universeMods
    -- Last-resort flat path: identical inference to the pre-#395 code,
    -- writing into src/test/json per package and refreshing the test
    -- digest. Only reached if the main-JSON seed load throws.
    flatFallback universeMods = do
      mods' <- inferModulesIO universeMods testMods
      writePackageSplitJsonFor routingMap "test" distJsonRoot universeMods universeMods mods'
      refreshTestDigests universeMods

-- | Incremental inference result. 'IncrementalFull mods' means all
-- modules need a fresh write; 'IncrementalPartial all dirty' means
-- only the dirty subset needs writing (the clean modules' on-disk
-- JSON is already correct). 'IncrementalPartialPreWritten all' means
-- inference and writes already happened inside tryIncrementalInference
-- (via the per-package driver); the caller refreshes digests from its
-- own 'universeMods' rather than receiving a returned module set, since
-- the driver discards inferred Module values to keep memory bounded
-- (only their (Name, TypeScheme) pairs flow through the foldl).
data IncrementalResult
  = IncrementalFull [Module]
  | IncrementalPartial [Module] [Module]
  -- ^ IncrementalPartial all-modules dirty-modules
  | IncrementalPartialPreWritten
  -- ^ Per-package driver inferred + wrote everything; caller refreshes
  --   digests from its own universeMods.

-- | Shared writer: build the schemaMap from the full module universe
-- and write the subset that needs to hit disk. 'universeForSchema' is
-- the complete post-inference module set used to seed the schema's
-- type-dependency closure; passing a narrow set (e.g. only DSL
-- wrappers, which don't declare hydra.packaging as a type dep) produces
-- a schemaMap missing hydra.packaging.Module and causes the encoder to
-- mis-serialize outer module frames (e.g. Maybe String as nested Maybe).
-- Callers should pass the full universe here for a complete schemaMap.
-- 'toWrite' is the subset that actually needs its JSON rewritten
-- (full set on a cache miss; dirty subset on an incremental hit).
writePackageSplitJson :: RoutingMap -> FilePath -> [Module] -> [Module] -> [Module] -> IO ()
writePackageSplitJson routingMap = writePackageSplitJsonFor routingMap "main"

-- | As 'writePackageSplitJson' but writes into the given source set's
-- tree (dist/json/<pkg>/src/<set>/json). Used by the test-side writer
-- to route into src/test/json; the main path uses srcSet = "main".
writePackageSplitJsonFor :: RoutingMap -> String -> FilePath -> [Module] -> [Module] -> [Module] -> IO ()
writePackageSplitJsonFor routingMap srcSet distJsonRoot universeMods universeForSchema toWrite = do
  -- Seed the graph's schema with the broader of the two inputs so
  -- hydra.packaging.Module (and every other universe type) is always
  -- reachable from the schemaMap, even when 'toWrite' is a narrow set
  -- like DSL wrappers whose declared type-deps omit packaging.
  let graph = modulesToGraph universeMods (universeMods ++ universeForSchema)
      schemaMap = buildSchemaMap graph
      groups = groupByPackageIn routingMap toWrite
  CM.forM_ groups $ \(pkg, pkgMods) -> do
    let pkgDir = distJsonRoot FP.</> pkg FP.</> "src" FP.</> srcSet FP.</> "json"
    putStrLn $ "  " ++ pkg ++ ": " ++ show (length pkgMods) ++ " modules -> " ++ pkgDir
    mapM_ (writeModuleJson schemaMap pkgDir) pkgMods

-- | Digest file for 'writeModulesJsonPackageSplit'. Single well-known
-- location at <distJsonRoot>/build/digest.json — the universe-wide
-- cache for Phase-1 freshness checks. Lives under build/ so the whole
-- build-cache subtree is gitignored as one unit (see #379).
packageSplitDigestAnchor :: FilePath -> FilePath
packageSplitDigestAnchor distJsonRoot = distJsonRoot FP.</> "build" FP.</> "digest.json"

-- | Per-package digest path for a given source set:
-- dist/json/<pkg>/build/<set>/digest.json. The digest covers the DSL
-- sources whose namespaces route to <pkg> and live in that source set.
-- The main path is build/main/digest.json; the parallel test path is
-- build/test/digest.json. See #379 for the build/ layout rationale.
perPackageDigestPathFor :: String -> FilePath -> String -> FilePath
perPackageDigestPathFor srcSet distJsonRoot pkg =
  distJsonRoot FP.</> pkg FP.</> "build" FP.</> srcSet FP.</> "digest.json"

-- | Per-package main-source-set digest path. Thin wrapper over
-- 'perPackageDigestPathFor' with srcSet = "main", preserving existing
-- main-path call sites.
perPackageDigestPath :: FilePath -> String -> FilePath
perPackageDigestPath = perPackageDigestPathFor "main"

-- | Read a package's declared dependencies from packages/<pkg>/package.json.
-- Returns the values of the top-level "dependencies" array, or [] if the
-- field is absent or the file can't be read.
--
-- Used by 'finalizePerPackageDigests' to populate each digest's depHash:<pkg>
-- entries, which carry that dep's selfHash for transitive invalidation.
loadPackageDeps :: String -> IO [String]
loadPackageDeps pkg = do
    let path = ".." FP.</> ".." FP.</> "packages" FP.</> pkg FP.</> "package.json"
    exists <- SD.doesFileExist path
    if not exists then return [] else do
      result <- E.try (BS.readFile path) :: IO (Either E.SomeException BS.ByteString)
      case result of
        Left _ -> return []
        Right bs -> case A.eitherDecode bs of
          Left _ -> return []
          Right (A.Object obj) -> case AKM.lookup (AK.fromString "dependencies") obj of
            Just (A.Array arr) -> return
              [ T.unpack t | A.String t <- V.toList arr ]
            _ -> return []
          Right _ -> return []

-- | After per-package digests have been written with their namespace
-- hashes (by 'refreshPerPackageDigests' / 'mergeDslJsonIntoPerPackageDigests'),
-- compute each package's selfHash and recorded dep selfHashes and rewrite
-- the digest with those fields populated.
--
-- Two in-memory passes over the packages on disk:
--
--   1. Read each package's existing digest; compute selfHash from its
--      hashes field. Collect into a map pkgName -> selfHash.
--   2. For each package, read its declared dependencies from package.json;
--      look up each dep's selfHash from the map; write back the digest
--      with selfHash + depHash:<dep> entries populated.
--
-- Packages whose declared deps haven't been written yet (e.g. test sources
-- before main) are skipped silently — the dep entry just gets dropped.
-- The next sync run will pick them up once all digests exist.
--
-- See #347 for the broader transitive-invalidation story.
finalizePerPackageDigests :: FilePath -> IO ()
finalizePerPackageDigests distJsonRoot = do
    -- Discover packages from on-disk digest files (don't rely on
    -- compile-time package lists; respects whatever was just written).
    pkgs <- discoverPackagesWithDigests distJsonRoot
    -- Pass 1: read each package's digest, compute selfHash.
    pkgPpds <- CM.forM pkgs $ \pkg -> do
      let dpath = perPackageDigestPath distJsonRoot pkg
      ppd <- Digest.readPerPackageDigest dpath
      let selfH = Digest.computeSelfHash (Digest.ppHashes ppd)
      return (pkg, ppd { Digest.ppSelfHash = selfH })
    let selfHashMap = M.fromList [(pkg, Digest.ppSelfHash ppd) | (pkg, ppd) <- pkgPpds]
    -- Pass 2: populate deps and write back.
    CM.forM_ pkgPpds $ \(pkg, ppd) -> do
      deps <- loadPackageDeps pkg
      let depHashes = M.fromList
            [ (d, h)
            | d <- deps
            , Just h <- [M.lookup d selfHashMap]
            ]
          finalPpd = ppd { Digest.ppDeps = depHashes }
          dpath = perPackageDigestPath distJsonRoot pkg
      Digest.writePerPackageDigest dpath finalPpd

-- | Enumerate packages that have a main-source-set digest on disk.
-- Walks dist/json/hydra-*/build/main/digest.json.
discoverPackagesWithDigests :: FilePath -> IO [String]
discoverPackagesWithDigests distJsonRoot = do
    exists <- SD.doesDirectoryExist distJsonRoot
    if not exists then return [] else do
      entries <- SD.listDirectory distJsonRoot
      fmap Y.catMaybes $ CM.forM entries $ \entry -> do
        let dpath = perPackageDigestPath distJsonRoot entry
        dExists <- SD.doesFileExist dpath
        return $ if dExists then Just entry else Nothing

-- | After a successful regen, write per-package digest files. Each
-- package's digest hashes its own source modules (the modules that route
-- to that package), letting Stage 3+ check freshness per package without
-- consulting the universe-wide digest.
--
-- We partition the *unfiltered universe* by owning package and hash each
-- package's modules. The universe is used deliberately rather than the
-- post-inference write set ('targetMods'): the write set excludes the
-- native-generator-owned hydra.<lang>.* modules (#344 — their JSON is
-- written by the native generators, not this pass), but the input digest
-- must still cover them, because edits to e.g. Coder.java DO feed
-- downstream generation. Hashing only the write set is exactly the bug
-- behind #400 — a native coder change never invalidated the digest, so
-- the freshness gate silently skipped regeneration. Discovery
-- ('discoverModuleNameFiles') now finds the native .java/.py sources, and
-- hashing the universe folds them into the right package's digest (routing
-- already maps hydra.<lang>.* → hydra-<lang>).
--
-- 'targetMods' (the post-inference write set) is retained in the signature
-- for call-site symmetry but is no longer used for digest computation.
refreshPerPackageDigests :: RoutingMap -> FilePath -> [Module] -> [Module] -> IO ()
refreshPerPackageDigests routingMap distJsonRoot universeMods _targetMods = do
  nsFiles <- Digest.discoverModuleNameFiles
  let groups = groupByPackageIn routingMap universeMods
  CM.forM_ groups $ \(pkg, pkgMods) -> do
    srcDigest <- Digest.hashUniverse nsFiles pkgMods
    -- Some packages (e.g. hydra-haskell with its synthesized coder
    -- modules, hydra-coq with no DSL sources at all) won't have any
    -- DSL files discoverable; skip writing an empty digest.
    CM.when (not (M.null srcDigest)) $ do
      -- #469: also fold in hashes of every *.json file under
      -- dist/json/<pkg>/src/main/json. For source-only packages, JSON
      -- content is a function of the source files, so this is
      -- redundant-but-safe; for native-coder packages (hydra-java /
      -- hydra-python), it's load-bearing: their JSON is produced by
      -- the published coder *runtime* on top of the .py/.java
      -- sources, and that runtime can change behavior independently
      -- (#398 reordered fields with no source change). Without
      -- jsonContent: entries, the freshness gate cannot detect that.
      jsonDigest <- Digest.hashPackageJsonContent distJsonRoot pkg
      let pkgDigest = M.union srcDigest jsonDigest
          dpath = perPackageDigestPath distJsonRoot pkg
      Digest.writeDigest dpath pkgDigest
      putStrLn $ "  Per-package digest: " ++ dpath
        ++ " (" ++ show (M.size srcDigest) ++ " src + "
        ++ show (M.size jsonDigest) ++ " json = "
        ++ show (M.size pkgDigest) ++ " entries)"

-- | Ensure per-package digest files exist on disk AND match current source
-- content. Called on cache hit so that Stage 3+ tooling has correct digests to
-- read even when no full regen ran.
--
-- For each package, compute the current input hash from its source files
-- (packages/<pkg>/.../*.hs and the native .java/.py self-host sources) and
-- compare against the on-disk per-package digest. Rewrite if missing or
-- stale. Without this, a universe-wide cache hit on top of an out-of-date
-- per-package digest leaves Phase 3 to silently believe its inputs haven't
-- changed when in fact they have — causing per-target dist regeneration to be
-- skipped.
--
-- 'universeMods' must be the *unfiltered* universe so the digest covers
-- native-generator-owned hydra.<lang>.* modules (#400); see
-- 'refreshPerPackageDigests'.
ensurePerPackageDigests :: RoutingMap -> FilePath -> [Module] -> IO ()
ensurePerPackageDigests routingMap distJsonRoot universeMods = do
  nsFiles <- Digest.discoverModuleNameFiles
  let groups = groupByPackageIn routingMap universeMods
  CM.forM_ groups $ \(pkg, pkgMods) -> do
    srcDigest <- Digest.hashUniverse nsFiles pkgMods
    CM.when (not (M.null srcDigest)) $ do
      -- #469: fold JSON content hashes in alongside source hashes,
      -- so an out-of-band JSON change (e.g. native coder runtime
      -- update) is reflected in the cache-hit-refreshed digest too.
      -- See 'refreshPerPackageDigests' for the rationale.
      jsonDigest <- Digest.hashPackageJsonContent distJsonRoot pkg
      let pkgDigest = M.union srcDigest jsonDigest
          dpath = perPackageDigestPath distJsonRoot pkg
      exists <- SD.doesFileExist dpath
      stored <- if exists then Digest.readDigest dpath else return M.empty
      CM.when (stored /= pkgDigest) $ do
        Digest.writeDigest dpath pkgDigest
        putStrLn $ "  Per-package digest refreshed: " ++ dpath
          ++ " (" ++ show (M.size srcDigest) ++ " src + "
          ++ show (M.size jsonDigest) ++ " json = "
          ++ show (M.size pkgDigest) ++ " entries)"

-- | Transitive closure over @moduleDependencies@: starting from an
-- initial dirty set of namespaces, repeatedly add any module whose
-- declared dependencies intersect the current dirty set, until fixed
-- point. Returns the closure (which always contains the initial set).
--
-- This is the reverse-edge walk: 'moduleDependencies' records what a
-- module imports; we want "modules that import any dirty thing." Built
-- as a fixed-point over the modules-with-some-dirty-dep predicate so
-- we don't have to materialize the inverted graph explicitly.
--
-- Note: @moduleDependencies@ reflects /declared/ deps (the
-- 'moduleDependencies' field on @Module@). Source files that use a
-- namespace without declaring it as a dep will not be picked up — but
-- such files would fail at inference time anyway, so the omission is
-- self-correcting in practice. See #347.
closeDirtySet :: [Module] -> S.Set ModuleName -> S.Set ModuleName
closeDirtySet universeMods initialDirty = fixedPoint initialDirty
  where
    fixedPoint d =
      let newlyDirty = S.fromList
            [ moduleName m
            | m <- universeMods
            , not (moduleName m `S.member` d)
            , any ((`S.member` d) . moduleDependencyModule) (moduleDependencies m)
            ]
          d' = S.union d newlyDirty
      in if S.size d' == S.size d then d else fixedPoint d'

-- | Try the incremental inference path: partition universeMods into
--   * cleanMods — DSL hash matches recorded digest AND existing JSON
--                 file is loadable (carries inferred TypeSchemes).
--   * dirtyMods — DSL hash mismatch OR no recorded hash OR JSON
--                 missing/unloadable. Plus the transitive closure of
--                 dependents via 'closeDirtySet' (#347).
--
-- If dirtyMods is empty, returns the loaded clean universe (no
-- inference needed; caller still writes JSON because we got here
-- via a cache miss on the universe-wide digest, meaning at least
-- one module's JSON is newer than the digest).
--
-- If dirtyMods is non-empty AND clean modules loaded successfully,
-- runs inferModulesGiven on (cleanLoaded ++ dirtyMods) targeting
-- dirtyMods, returns the full universe with refreshed dirty mods.
--
-- If anything goes wrong (no digest yet, JSON load failure, etc.),
-- returns Nothing — caller falls back to full inferModulesIO.
tryIncrementalInference :: RoutingMap -> FilePath -> [Module] -> [Module] -> IO (Maybe IncrementalResult)
tryIncrementalInference routingMap distJsonRoot universeMods targetMods = do
  -- Read the universe-wide digest to learn which sources were clean
  -- as of the last successful regen.
  let digestFile = packageSplitDigestAnchor distJsonRoot
  stored <- Digest.readDigest digestFile
  if M.null stored
    then return Nothing
    else do
      -- (Pre-#347 universe digests carried an encoderId field gating an
      -- additional universal-miss check here; retired in favor of
      -- HYDRA_GENERATOR_STAMP at Layer 2. The four JSON-coder DSL files
      -- previously fingerprinted by encoderId are namespaces in
      -- 'hashUniverse' below, so any change to them invalidates the
      -- cache through the standard per-namespace path.)
      nsFiles <- Digest.discoverModuleNameFiles
      currentDigest <- Digest.hashUniverse nsFiles universeMods
      if M.null currentDigest
        then return Nothing
        else do
          -- Partition: a module is "clean" if its current DSL source
          -- hash matches the stored hash for its namespace. Modules
          -- without a discoverable DSL source (e.g. demos under
          -- demos/src/, modules from heads/haskell/src/) are treated
          -- as clean — their definition is determined by code we
          -- don't have direct access to here, but if they aren't
          -- in stored AND aren't in currentDigest, no source change
          -- is detectable so they're effectively unchanged.
          let isSourceClean m =
                let ns = moduleName m
                in case (M.lookup ns currentDigest, M.lookup ns stored) of
                     (Just c, Just s) -> c == s
                     (Nothing, Nothing) -> True
                     _                -> False
              -- Initial dirty set: modules whose own source hash changed.
              initialDirty = L.filter (not . isSourceClean) universeMods
              initialDirtyNs = S.fromList (fmap moduleName initialDirty)
              -- Transitive expansion: add any module that imports a dirty
              -- module, by closure over moduleDependencies. Without this
              -- step a kernel-type rename invalidates the renamed module
              -- only — its consumers are loaded from the on-disk JSON
              -- (which reflects the old name) and the dependent .hs/.json
              -- on disk stays stale. See #347.
              allDirtyNs = closeDirtySet universeMods initialDirtyNs
              -- Two separate concerns:
              --
              -- 1) Which modules get re-inferred + re-written? Only those
              --    in targetMods (the caller is responsible for the rest;
              --    e.g. update-json-main excludes native-owned hydra.java.*
              --    and hydra.python.* per #344, leaving them to the native
              --    generators).
              --
              -- 2) Which modules get loaded from JSON to build the typed
              --    universe for inference? Everything in universeMods that
              --    isn't in dirtyMods, so cross-package type references
              --    still resolve.
              targetNs = S.fromList (fmap moduleName targetMods)
              isDirty m =
                let ns = moduleName m
                in ns `S.member` allDirtyNs && ns `S.member` targetNs
              dirtyMods = filter isDirty targetMods
              dirtyNs = S.fromList (fmap moduleName dirtyMods)
              cleanMods = filter (\m -> not (moduleName m `S.member` dirtyNs))
                                 universeMods
              addedByClosure = S.size (S.intersection allDirtyNs targetNs)
                             - S.size (S.intersection initialDirtyNs targetNs)

          CM.when (addedByClosure > 0) $
            putStrLn $ "  Transitive closure added " ++ show addedByClosure
              ++ " dependent modules to the dirty set."
          if Prelude.null dirtyMods
            then do
              -- Everything is clean per the digest, but tryCacheHitSplit
              -- said miss. That means a JSON file is missing or the
              -- digest is stale. Fall through to full inference.
              return Nothing
            else do
              putStrLn $ "  Incremental inference: "
                ++ show (length dirtyMods) ++ " dirty / "
                ++ show (length cleanMods) ++ " clean"
              -- Load clean modules from JSON (they carry inferred types).
              let cleanNs = fmap moduleName cleanMods
              loaded <- E.try (loadCleanFromJson routingMap distJsonRoot universeMods cleanNs)
                        :: IO (Either E.SomeException [Module])
              case loaded of
                Left e -> do
                  putStrLn $ "  Incremental load failed: " ++ show e
                  return Nothing
                Right cleanLoaded -> do
                  -- #381 follow-up: route the incremental dirty set through
                  -- the per-package driver, seeded with maps of the
                  -- JSON-loaded clean modules' (Name, TypeScheme) pairs.
                  -- Two maps: term-binding schemes (graphBoundTypes) and
                  -- type-def schemes (graphSchemaTypes). Carrying these as
                  -- Maps (not as [Module]) lets GC reclaim the cleanLoaded
                  -- term bodies and annotations once we've extracted what
                  -- inference actually consults. On a #369-style mass-
                  -- rename, dirtyMods ≈ 250 and cleanLoaded ≈ 70 with full
                  -- term ASTs; the prior [Module] accumulator blew past
                  -- -M6G even with per-package iteration.
                  let seedBindingSchemes = M.fromList
                        [ (termDefinitionName td, ts)
                        | m <- cleanLoaded
                        , DefinitionTerm td <- moduleDefinitions m
                        , Just ts <- [termSignatureToTypeScheme <$> termDefinitionSignature td]
                        ]
                      seedSchemaSchemes = M.fromList
                        [ (typeDefinitionName td, normalizeTypeScheme (typeDefinitionBody td))
                        | m <- cleanLoaded
                        , DefinitionType td <- moduleDefinitions m
                        ]
                  -- Driver iteration universe = dirtyMods only; seed Maps
                  -- carry the cleanLoaded types so cross-package refs
                  -- resolve during inference. cleanLoaded is also passed
                  -- as the schema-context-only set so the JSON-write
                  -- schemaMap (built once up front) covers prior-package
                  -- types like hydra.packaging.Module — without that,
                  -- Maybe String fields encode as single-element arrays.
                  inferAndWriteByPackageSeeded routingMap distJsonRoot
                    seedBindingSchemes seedSchemaSchemes
                    cleanLoaded dirtyMods dirtyMods
                  return (Just IncrementalPartialPreWritten)

-- | Load modules from per-package JSON paths. The dist-json-root
-- layout is dist/json/<pkg>/src/main/json/<ns-path>.json; we route
-- each namespace through namespaceToPackage to find its package
-- subdirectory.
loadCleanFromJson :: RoutingMap -> FilePath -> [Module] -> [ModuleName] -> IO [Module]
loadCleanFromJson routingMap distJsonRoot universeModules namespaces =
  CM.forM namespaces $ \ns -> do
    let pkg = namespaceToPackageIn routingMap ns
        pkgDir = distJsonRoot FP.</> pkg FP.</> "src" FP.</> "main" FP.</> "json"
        filePath = pkgDir FP.</> CodeGeneration.moduleNameToPath ns ++ ".json"
    parseResult <- parseJsonFile filePath
    case parseResult of
      Left err -> fail $ "Incremental: JSON parse error for "
        ++ unModuleName ns ++ " at " ++ filePath ++ ": " ++ err
      Right jsonVal -> case CodeGeneration.decodeModuleFromJson bootstrapGraph universeModules jsonVal of
        Left err -> fail $ "Incremental: module decode error for "
          ++ unModuleName ns ++ ": " ++ showError err
        Right m  -> return m

-- | Replace each DSL source module containing term definitions WITHOUT
-- signatures by its just-written dist/json counterpart, whose term definitions
-- carry the main pass's inferred signatures. The compiled source-as-data
-- modules always have termDefinitionSignature = Nothing (inference never runs
-- on the in-memory values), so without this read-back the DSL synthesizer's
-- term path (Hydra.Sources.Kernel.Terms.Dsls.generateRefBindings) would
-- silently skip every term definition and emit an empty module. Reading the
-- routed JSON back reuses the single main-pass inference — no re-inference,
-- which would not fit in memory — and holds on cache-hit paths too, since the
-- JSON on disk is current either way. Modules with no unsigned term
-- definitions (type modules, primitive hydra.lib.* modules) pass through
-- untouched. Fails if a reloaded module still lacks a term signature, rather
-- than regressing to silent omission. (#467)
reloadTermSignatureSources :: RoutingMap -> FilePath -> [Module] -> [Module] -> IO [Module]
reloadTermSignatureSources routingMap distJsonRoot universeModules mods = CM.forM mods $ \m ->
    if null (unsigned m)
      then return m
      else do
        loaded <- loadCleanFromJson routingMap distJsonRoot universeModules [moduleName m]
        case loaded of
          [m'] -> if null (unsigned m')
            then return m'
            else fail $ "DSL read-back: term definitions still lack signatures in "
              ++ unModuleName (moduleName m) ++ ": "
              ++ L.intercalate ", " (unName <$> unsigned m')
          _ -> fail $ "DSL read-back: expected exactly one module for "
            ++ unModuleName (moduleName m)
  where
    unsigned m = [ termDefinitionName td
                 | DefinitionTerm td <- moduleDefinitions m
                 , Y.isNothing (termDefinitionSignature td) ]

-- | If every universe module's DSL source hash matches the stored digest,
-- and every target module's JSON file already exists, return the current
-- digest (indicating a cache hit, so the caller can skip the slow path).
-- Otherwise return Nothing.
tryCacheHit :: FilePath -> [Module] -> [Module] -> IO (Maybe Digest.DigestMap)
tryCacheHit basePath universeMods targetMods = do
  let digestFile = Digest.digestPath basePath
      targetPaths = [basePath FP.</> CodeGeneration.moduleNameToPath (moduleName m) ++ ".json" | m <- targetMods]
  checkCacheHit digestFile universeMods targetPaths

tryCacheHitSplit :: RoutingMap -> FilePath -> [Module] -> [Module] -> IO (Maybe Digest.DigestMap)
tryCacheHitSplit routingMap distJsonRoot universeMods targetMods = do
  let digestFile = packageSplitDigestAnchor distJsonRoot
      targetPaths = [ distJsonRoot FP.</> pkg FP.</> "src" FP.</> "main" FP.</> "json"
                                    FP.</> CodeGeneration.moduleNameToPath (moduleName m) ++ ".json"
                    | (pkg, pkgMods) <- groupByPackageIn routingMap targetMods, m <- pkgMods]
  checkCacheHit digestFile universeMods targetPaths

-- | Shared logic: compare current source-file hashes against the stored
-- digest and check that every target JSON file exists on disk.
--
-- Only modules whose DSL source was discoverable contribute hashes; derived
-- modules without a `ns = ModuleName "..."` source file (e.g. kernel
-- decode/encode modules generated from 'Hydra.Sources.Kernel.Terms.*') are
-- transparently handled because their generator's source IS in the map, so
-- any change upstream invalidates the cache.
--
-- (Pre-#347 universe digests also carried an `encoderId` field that
-- fired here as an additional universal-miss trigger; retired in favor
-- of HYDRA_GENERATOR_STAMP at Layer 2. The four JSON-coder DSL files
-- previously fingerprinted by encoderId are namespaces in 'hashUniverse'
-- below, so changes propagate through the standard per-namespace path.)
checkCacheHit :: FilePath -> [Module] -> [FilePath] -> IO (Maybe Digest.DigestMap)
checkCacheHit digestFile universeMods targetPaths = do
  nsFiles <- Digest.discoverModuleNameFiles
  currentDigest <- Digest.hashUniverse nsFiles universeMods
  if M.null currentDigest
    then return Nothing  -- nothing to verify against; always recompute
    else do
      stored <- Digest.readDigest digestFile
      if stored /= currentDigest
        then return Nothing
        else do
          existFlags <- mapM SD.doesFileExist targetPaths
          if and existFlags then return (Just currentDigest) else return Nothing

-- | As 'checkCacheHit', but the stored digest is checked against EVERY file
-- in 'digestFiles' rather than a single one. A hit requires each of them to
-- match the current universe hash. #551: when several packages each keep
-- their own copy of what is logically one cache entry (e.g. the test-side
-- per-package digests, all written from the same 'universeMods' in
-- 'writeTestModulesJson'), checking only one of those copies can miss a
-- copy that fell out of sync with the others — a stale or orphaned
-- per-package digest could otherwise coincidentally match the current
-- hash while sitting alongside siblings that don't, or simply go
-- unnoticed because nothing ever re-reads it. Requiring all copies to
-- agree makes the multi-file redundancy actually load-bearing instead of
-- cosmetic.
checkCacheHitAll :: [FilePath] -> [Module] -> [FilePath] -> IO (Maybe Digest.DigestMap)
checkCacheHitAll digestFiles universeMods targetPaths = do
  nsFiles <- Digest.discoverModuleNameFiles
  currentDigest <- Digest.hashUniverse nsFiles universeMods
  if M.null currentDigest
    then return Nothing  -- nothing to verify against; always recompute
    else do
      storedDigests <- mapM Digest.readDigest digestFiles
      if any (/= currentDigest) storedDigests
        then return Nothing
        else do
          existFlags <- mapM SD.doesFileExist targetPaths
          if and existFlags then return (Just currentDigest) else return Nothing

-- | After a successful slow-path run, overwrite the digest with fresh hashes.
refreshDigest :: FilePath -> [Module] -> IO ()
refreshDigest basePath universeMods = refreshDigestAt (Digest.digestPath basePath) universeMods

refreshDigestAt :: FilePath -> [Module] -> IO ()
refreshDigestAt digestFile universeMods = do
  nsFiles <- Digest.discoverModuleNameFiles
  current <- Digest.hashUniverse nsFiles universeMods
  Digest.writeDigest digestFile current
  putStrLn $ "  Digest refreshed: " ++ digestFile ++ " (" ++ show (M.size current) ++ " entries)"

-- | Write DSL modules to JSON files.
writeDslJson :: FilePath -> [Module] -> [Module] -> IO ()
writeDslJson basePath universeModules typeModules = do
    dslMods <- generateDslModules universeModules typeModules
    let nonEmpty = filter (not . null . moduleDefinitions) dslMods
    writeModulesJson False basePath universeModules nonEmpty

-- | Write DSL modules to JSON files, routed per package. Like 'writeDslJson'
-- but uses 'writeModulesJsonPackageSplit' under the hood.
--
-- After writing, augment each touched package's input digest with one entry
-- per DSL namespace, keyed on the just-written JSON file's content hash. DSL
-- namespaces are synthesized and have no source file, so they cannot be
-- hashed via 'hashUniverse' (which looks them up in packages/.../Sources/).
-- Without these entries, downstream per-target caches (digest-check) cannot
-- detect when DSL JSON content changes — e.g. when the synthesizer in
-- Hydra/Sources/Kernel/Terms/Dsls.hs re-emits a binding under a new name,
-- the per-package input digest is unchanged and the .hs stays stale.
-- See feature_347_merkle_trees for the broader transform-fingerprint story.
writeDslJsonPackageSplit :: RoutingMap -> FilePath -> [Module] -> [Module] -> IO ()
writeDslJsonPackageSplit routingMap distJsonRoot universeModules typeModules = do
    dslSources <- reloadTermSignatureSources routingMap distJsonRoot universeModules typeModules
    dslMods <- generateDslModules universeModules dslSources
    let nonEmpty = filter (not . null . moduleDefinitions) dslMods
    writeModulesJsonPackageSplit routingMap False distJsonRoot universeModules nonEmpty
    mergeDslJsonIntoPerPackageDigests routingMap distJsonRoot nonEmpty
    -- After both type/term and DSL hash entries are settled, compute the
    -- per-package selfHash and depHashes that drive transitive
    -- invalidation. See finalizePerPackageDigests and #347.
    finalizePerPackageDigests distJsonRoot
    -- #405: prune orphaned JSON. This is the last JSON-write step in the
    -- update-json-main run, so every reconciled package's full emission
    -- set (type/term from the main pass + these DSL wrappers) is now known.
    -- The keep-set is the union of both passes' intended outputs; anything
    -- else under the package's src/main/json is an orphan (e.g. a DSL/type
    -- module dropped from the emission set, like the stale hydra.dsl.classes
    -- left by the #397 rename — see #405).
    reconcilePackageJsonOrphans routingMap distJsonRoot (writtenMainModules ++ nonEmpty)
  where
    -- The main pass writes every universe module EXCEPT native-generator-owned
    -- hydra.jvm.*/hydra.java.*/hydra.python.* (#344, #505). Mirror that exclusion
    -- so the keep-set matches what was actually written. (Those packages are skipped
    -- by the reconcile below anyway, but keeping the keep-set faithful avoids any
    -- accidental cross-package surprise.)
    writtenMainModules = filter (not . isNativeOwnedNs) universeModules
    isNativeOwnedNs m =
      let ns = unModuleName (moduleName m)
      in L.isPrefixOf "hydra.jvm." ns || L.isPrefixOf "hydra.java." ns || L.isPrefixOf "hydra.python." ns

-- | Generate ALL derived modules (DSL wrappers + term encoders + term
-- decoders) for a list of source type modules and write them, routed per
-- package, directly as JSON (#474). This replaces the DSL-only
-- 'writeDslJsonPackageSplit' and the historical two-cycle path where
-- encode/decode JSON came from compiled Hydra.Sources.{Encode,Decode}.*.hs
-- source-as-data modules: here the inner hydra.{encode,decode}.<x> Modules are
-- emitted straight to dist/json, so no per-host .hs source-as-data copy is
-- needed for the JSON to exist (#448-aligned).
--
-- DSL wrappers are derived from 'dslSourceModules' (broad: every type module);
-- encoders + decoders from 'encodingSourceModules' (narrower — only modules the
-- eta-expanding targets can compile, see #475). Each derived module is routed to
-- its owning package via the RoutingMap. The terminal digest/orphan steps run
-- ONCE over the union of all derived kinds plus the already-written main modules.
writeDerivedJsonPackageSplit :: RoutingMap -> FilePath -> [Module] -> [Module] -> [Module] -> IO ()
writeDerivedJsonPackageSplit routingMap distJsonRoot universeModules dslSourceModules encodingSourceModules = do
    dslSources <- reloadTermSignatureSources routingMap distJsonRoot universeModules dslSourceModules
    dslMods <- generateDslModules universeModules dslSources
    encMods <- generateEncoderModules universeModules encodingSourceModules
    decMods <- generateDecoderModules universeModules encodingSourceModules
    let derived = filter (not . null . moduleDefinitions) (dslMods ++ encMods ++ decMods)
    -- doInfer=False: the synthesizer is the sole source of truth for these derived
    -- modules' in-term annotations (lambda domains, type-applications, result-type
    -- signatures) — it fully populates them at construction, mirroring the DSL
    -- wrapper synthesis. Running inference over them is at best redundant and at
    -- worst incorrect: for polymorphic decoders (e.g. hydra.decode.parsing.parseResult,
    -- result type either<DecodingError, ParseResult @ a>) HM's occurs-check rejects
    -- the saturated nominal self-application that the synthesizer emits, so inference
    -- cannot even reproduce its own prior committed output. The synthesizer's nominal
    -- form is also the PREFERRED form: inference cannot re-produce a transparent type
    -- alias once expanded (type Vertex = int32 stays `Vertex`, not `literal<int32>`).
    -- (#476)
    writeModulesJsonPackageSplit routingMap False distJsonRoot universeModules derived
    mergeDslJsonIntoPerPackageDigests routingMap distJsonRoot derived
    finalizePerPackageDigests distJsonRoot
    reconcilePackageJsonOrphans routingMap distJsonRoot (writtenMainModules ++ derived)
  where
    writtenMainModules = filter (not . isNativeOwnedNs) universeModules
    isNativeOwnedNs m =
      let ns = unModuleName (moduleName m)
      in L.isPrefixOf "hydra.jvm." ns || L.isPrefixOf "hydra.java." ns || L.isPrefixOf "hydra.python." ns

-- | Packages whose dist/json tree is written by a NON-Haskell generator and
-- so must NOT be reconciled by the Haskell JSON write path. hydra-jvm, hydra-java,
-- and hydra-python receive their canonical JSON from the native Java/Python
-- generators (#344, #505); during the transition the Haskell DSL pass
-- ALSO writes their hydra.dsl.<lang>.* wrappers into the same dir, so neither
-- generator alone holds the complete keep-set. Rather than coordinate two
-- generators over one dir, the Haskell side simply skips them; the native
-- generators own their own reconcile. Once the legacy Haskell DSL sources for
-- these packages are deleted (before 0.16), they become cleanly single-writer
-- and this skip can be revisited. See #405.
jsonReconcileSkipPackages :: S.Set String
jsonReconcileSkipPackages = S.fromList ["hydra-jvm", "hydra-java", "hydra-python"]

-- | Files (relative to a package's src/main/json) that legitimately live in
-- the JSON tree but are NOT written by update-json-main, so the #405
-- reconcile must never delete them. manifest.json is written per package by
-- the separate update-json-manifest exe (one namespace listing per package);
-- it has no owning Module and would otherwise look like an orphan.
jsonReconcileProtect :: S.Set FilePath
jsonReconcileProtect = S.fromList [FP.normalise "manifest.json"]

-- | Delete orphaned .json files under each reconciled package's src/main/json.
--
-- For every package owning at least one written module (other than the
-- native-owned skip set), the keep-set is the set of files the JSON write
-- path emitted for it this run: one <moduleNameToPath ns>.json per routed
-- module across both the type/term and DSL passes. Any other .json under the
-- package's src/main/json is an orphan and is deleted; emptied directories
-- are pruned. Reuses the shared keep-set reconcile (Hydra.Digest, #393/#405).
--
-- 'writtenModules' must be the UNION of every module the write path emitted
-- this run (main write-universe + non-empty DSL wrappers), so one pass's
-- legitimate output is never mistaken for another pass's orphan.
reconcilePackageJsonOrphans :: RoutingMap -> FilePath -> [Module] -> IO ()
reconcilePackageJsonOrphans routingMap distJsonRoot writtenModules = do
    let groups = groupByPackageIn routingMap writtenModules
    CM.forM_ groups $ \(pkg, pkgMods) ->
      CM.unless (S.member pkg jsonReconcileSkipPackages) $ do
        let pkgJsonDir = distJsonRoot FP.</> pkg FP.</> "src" FP.</> "main" FP.</> "json"
            keepRel = S.fromList
              [ FP.normalise (CodeGeneration.moduleNameToPath (moduleName m) ++ ".json")
              | m <- pkgMods ]
        orphans <- Digest.reconcileOrphans pkgJsonDir keepRel jsonReconcileProtect
        CM.unless (null orphans) $ do
          putStrLn $ "  " ++ pkg ++ ": pruned " ++ show (length orphans)
            ++ " orphaned JSON file(s) (#405)"
          mapM_ (\p -> putStrLn $ "    - " ++ p) orphans

-- | For each package owning at least one of the given DSL modules, read its
-- per-package input digest, add an entry per DSL namespace whose value is
-- the SHA-256 of the just-written DSL JSON file, and write the merged
-- digest back. Type/term entries already present in the digest are
-- preserved — this is purely additive.
--
-- The JSON file path is derived the same way 'writeModuleJson' derives it:
-- <distJsonRoot>/<pkg>/src/main/json/<moduleNameToPath ns>.json.
mergeDslJsonIntoPerPackageDigests :: RoutingMap -> FilePath -> [Module] -> IO ()
mergeDslJsonIntoPerPackageDigests routingMap distJsonRoot dslMods = do
    let groups = groupByPackageIn routingMap dslMods
    CM.forM_ groups $ \(pkg, pkgMods) -> do
      let pkgJsonDir = distJsonRoot FP.</> pkg FP.</> "src" FP.</> "main" FP.</> "json"
      newEntries <- fmap (M.fromList . Y.catMaybes) $ CM.forM pkgMods $ \m -> do
        let ns      = moduleName m
            jsonFp  = pkgJsonDir FP.</> CodeGeneration.moduleNameToPath ns ++ ".json"
        exists <- SD.doesFileExist jsonFp
        if not exists
          then return Nothing
          else do
            h <- Digest.hashFile jsonFp
            return $ Just (ns, h)
      CM.when (not (M.null newEntries)) $ do
        let dpath = perPackageDigestPath distJsonRoot pkg
        existing <- do
          dExists <- SD.doesFileExist dpath
          if dExists then Digest.readDigest dpath else return M.empty
        let merged = M.union newEntries existing
        CM.when (merged /= existing) $ do
          Digest.writeDigest dpath merged
          putStrLn $ "  Per-package digest augmented with DSL entries: " ++ dpath
            ++ " (+" ++ show (M.size newEntries - M.size (M.intersection existing newEntries))
            ++ " new, " ++ show (M.size newEntries) ++ " total DSL)"

-- | Write a manifest.json listing module namespaces for kernelModules, mainModules, and testModules.
-- This allows Java and Python hosts to load the correct set of modules without directory scanning.
--
-- Takes the module sets as arguments because the set of "main modules" depends on
-- which host language is generating code.
writeManifestJson :: FilePath
                  -> [Module] -- ^ kernelModules
                  -> [Module] -- ^ kernelTypesModules (for DSL generation)
                  -> [Module] -- ^ mainModules
                  -> [Module] -- ^ testModules
                  -> IO ()
writeManifestJson basePath kernelModules kernelTypesModules mainModules testModules = do
    dslMods <- generateDslModules mainModules kernelTypesModules
    let nonEmptyDsls = filter (not . null . moduleDefinitions) dslMods
    -- Keep fields alphabetized so the emitted manifest.json byte order is unchanged
    -- by the switch to order-preserving JSON objects (see docs/json-format.md).
    let jsonVal = Json.ValueObject [
            ("dslModules", namespacesJson nonEmptyDsls),
            ("kernelModules", namespacesJson kernelModules),
            ("mainModules", namespacesJson mainModules),
            ("manifestFormatVersion", Json.ValueNumber 1),
            ("moduleFormatVersion", Json.ValueNumber currentModuleFormatVersion),
            ("testModules", namespacesJson testModules)]
        jsonStr = JsonWriter.printJson jsonVal
        filePath = basePath FP.</> "manifest.json"
    writeFile filePath (jsonStr ++ "\n")
    putStrLn $ "Wrote manifest: " ++ filePath
  where
    -- Sort namespace strings lexicographically for cross-host byte stability;
    -- see docs/json-format.md "Stability of byte order".
    namespacesJson mods = Json.ValueArray $ fmap Json.ValueString
      (L.sort (fmap (unModuleName . moduleName) mods))

-- | Write per-package manifest.json files at
-- <root>/<pkg>/src/main/json/manifest.json for every package owning at least
-- one module in the given lists.
--
-- Each per-package manifest has the same schema as the legacy monolithic
-- manifest, but the field values are scoped to modules owned by that package.
-- A package appears only if it owns at least one module in mainModules
-- (testModules alone aren't enough — test packages use their own
-- src/test/json/manifest.json path, not covered here).
--
-- The 'dslSourceModules' / 'encodingSourceModules' arguments are the per-package
-- lists of SOURCE modules from which dsl wrappers (broad) and encoders/decoders
-- (narrower — #475) are derived (#474). They are emitted directly as the
-- manifest's @mainDslModules@ / @mainEncodingModules@ fields (the source lists,
-- not the generated wrapper namespaces).
writePerPackageManifestsJson :: RoutingMap
                             -> FilePath
                             -> [Module] -- ^ dslSourceModules (broad: source modules for hydra.dsl.<x>)
                             -> [Module] -- ^ encodingSourceModules (narrower: source modules for hydra.{encode,decode}.<x>)
                             -> [Module] -- ^ mainModules (to partition)
                             -> [Module] -- ^ testModules (today always hydra-kernel)
                             -> IO ()
writePerPackageManifestsJson routingMap distJsonRoot dslSourceModules encodingSourceModules mainModules testModules = do
    let mainByPkg    = groupByPackageIn routingMap mainModules
    let dslByPkg     = M.fromList (groupByPackageIn routingMap dslSourceModules)
    let encByPkg     = M.fromList (groupByPackageIn routingMap encodingSourceModules)
    let testByPkg    = M.fromList (groupByPackageIn routingMap testModules)
    let packages = L.nub
          $ fmap fst mainByPkg
          ++ M.keys dslByPkg
          ++ M.keys encByPkg
          ++ M.keys testByPkg
    CM.forM_ (L.sort packages) $ \pkg -> do
      let mainForPkg    = Y.fromMaybe [] (lookup pkg mainByPkg)
          dslForPkg     = M.findWithDefault [] pkg dslByPkg
          encForPkg     = M.findWithDefault [] pkg encByPkg
          testForPkg    = M.findWithDefault [] pkg testByPkg
          -- Keep fields alphabetized so the emitted manifest.json byte order is unchanged
          -- by the switch to order-preserving JSON objects (see docs/json-format.md).
          jsonVal = Json.ValueObject [
              ("mainDslModules",      namespacesJson dslForPkg),
              ("mainEncodingModules", namespacesJson encForPkg),
              ("mainModules",    namespacesJson mainForPkg),
              ("manifestFormatVersion", Json.ValueNumber 1),
              ("moduleFormatVersion", Json.ValueNumber currentModuleFormatVersion),
              ("package",        Json.ValueString pkg),
              ("testModules",    namespacesJson testForPkg)]
          jsonStr = JsonWriter.printJson jsonVal
          pkgDir  = distJsonRoot FP.</> pkg FP.</> "src" FP.</> "main" FP.</> "json"
          filePath = pkgDir FP.</> "manifest.json"
      SD.createDirectoryIfMissing True pkgDir
      writeFile filePath (jsonStr ++ "\n")
      putStrLn $ "Wrote manifest: " ++ filePath
  where
    -- Sort namespace strings lexicographically for cross-host byte stability;
    -- see docs/json-format.md "Stability of byte order".
    namespacesJson mods = Json.ValueArray $ fmap Json.ValueString
      (L.sort (fmap (unModuleName . moduleName) mods))

----------------------------------------
-- JSON Module Import
----------------------------------------

-- | Convert an Aeson JSON value to a Hydra JSON value.
aesonToHydra :: A.Value -> Json.Value
aesonToHydra v = case v of
  A.Object km -> Json.ValueObject (mapPair <$> AKM.toList km)
    where
      mapPair (k, v') = (AK.toString k, aesonToHydra v')
  A.Array a -> Json.ValueArray (aesonToHydra <$> V.toList a)
  A.String t -> Json.ValueString $ T.unpack t
  A.Number s -> Json.ValueNumber s
  A.Bool b -> Json.ValueBoolean b
  A.Null -> Json.ValueNull

-- | Parse a JSON file using Aeson and convert to Hydra JSON.
-- Pre-processes the content to escape control characters that the Hydra JSON writer
-- doesn't escape (e.g. null bytes in string literals).
parseJsonFile :: FilePath -> IO (Either String Json.Value)
parseJsonFile fp = do
  content <- BS.readFile fp
  let escaped = escapeControlCharsInJson content
  return $ aesonToHydra <$> A.eitherDecode escaped

-- | Escape unescaped control characters (< 0x20) inside JSON string literals.
-- Thin ByteString wrapper around CodeGeneration.escapeControlCharsInJson (which operates on [Int]).
escapeControlCharsInJson :: BS.ByteString -> BS.ByteString
escapeControlCharsInJson input =
  BS.pack $ fmap fromIntegral $ CodeGeneration.escapeControlCharsInJson $ fmap fromIntegral $ BS.unpack input

-- | Read a field from manifest.json as a list of module names.
readManifestField :: FilePath -> String -> IO [ModuleName]
readManifestField basePath fieldName = do
    let manifestPath = basePath FP.</> "manifest.json"
    parseResult <- parseJsonFile manifestPath
    case parseResult of
      Left err -> fail $ "Failed to parse manifest.json: " ++ err
      Right jsonVal -> case jsonVal of
        Json.ValueObject obj -> case lookup fieldName obj of
          Nothing -> fail $ "manifest.json missing field: " ++ fieldName
          Just (Json.ValueArray arr) -> return $ fmap toNamespace arr
          Just _ -> fail $ "manifest.json field " ++ fieldName ++ " is not an array"
        _ -> fail "manifest.json is not a JSON object"
  where
    toNamespace (Json.ValueString s) = ModuleName s
    toNamespace _ = error $ "manifest.json: expected string in " ++ fieldName

-- | Read a manifest field or return an empty list if the field (or the
-- manifest itself) is missing. Differs from 'readManifestField', which
-- fails hard on a missing field.
readManifestFieldOrEmpty :: FilePath -> String -> IO [ModuleName]
readManifestFieldOrEmpty basePath fieldName = do
    let manifestPath = basePath FP.</> "manifest.json"
    exists <- SD.doesFileExist manifestPath
    if not exists
      then return []
      else do
        parseResult <- parseJsonFile manifestPath
        case parseResult of
          Left _ -> return []
          Right jsonVal -> case jsonVal of
            Json.ValueObject obj -> case lookup fieldName obj of
              Just (Json.ValueArray arr) -> return $ fmap toNamespace arr
              _                          -> return []
            _ -> return []
  where
    toNamespace (Json.ValueString s) = ModuleName s
    toNamespace _ = error $ "manifest.json: expected string in " ++ fieldName

-- | Read a manifest field, trying a primary name first and falling back to an alternative.
readManifestFieldWithFallback :: FilePath -> String -> String -> IO [ModuleName]
readManifestFieldWithFallback basePath primaryField fallbackField = do
    let manifestPath = basePath FP.</> "manifest.json"
    parseResult <- parseJsonFile manifestPath
    case parseResult of
      Left err -> fail $ "Failed to parse manifest.json: " ++ err
      Right jsonVal -> case jsonVal of
        Json.ValueObject obj -> case lookup primaryField obj of
          Just (Json.ValueArray arr) -> return $ fmap toNamespace arr
          _ -> case lookup fallbackField obj of
            Just (Json.ValueArray arr) -> return $ fmap toNamespace arr
            Nothing -> fail $ "manifest.json missing fields: " ++ primaryField ++ " and " ++ fallbackField
            Just _ -> fail $ "manifest.json field " ++ fallbackField ++ " is not an array"
        _ -> fail "manifest.json is not a JSON object"
  where
    toNamespace (Json.ValueString s) = ModuleName s
    toNamespace _ = error $ "manifest.json: expected string in " ++ primaryField ++ "/" ++ fallbackField

-- | The module wire-format version this build of the kernel reads and writes.
-- Bump this when a change to the JSON encoding of Module/Term would make a
-- parser for version N unable to correctly decode version N+1 output.
-- See docs/json-format.md §Format versioning.
currentModuleFormatVersion :: SC.Scientific
currentModuleFormatVersion = 1

-- | Extract moduleFormatVersion from a parsed manifest JSON value.
-- Returns Nothing if the field is absent (legacy manifest) or has an unexpected type.
parseManifestModuleFormatVersion :: Json.Value -> Maybe Int
parseManifestModuleFormatVersion jsonVal = case jsonVal of
    Json.ValueObject obj -> case lookup "moduleFormatVersion" obj of
      Just (Json.ValueNumber n) -> Just (round n)
      _                         -> Nothing
    _ -> Nothing

-- | Read the moduleFormatVersion from a package's manifest.json.
-- Returns Nothing if the manifest is absent, unparseable, or predates the field
-- (treat absence as version 1, the only version that has ever shipped).
-- Returns Just n on success.
readManifestModuleFormatVersion :: FilePath -> IO (Maybe Int)
readManifestModuleFormatVersion basePath = do
    let manifestPath = basePath FP.</> "manifest.json"
    exists <- SD.doesFileExist manifestPath
    if not exists
      then return Nothing
      else do
        parseResult <- parseJsonFile manifestPath
        case parseResult of
          Left _        -> return Nothing
          Right jsonVal -> return $ parseManifestModuleFormatVersion jsonVal

-- | Load modules from JSON files for a list of namespaces.
-- Uses the universe modules to build the graph for type resolution.
loadModulesFromJson :: FilePath -> [Module] -> [ModuleName] -> IO [Module]
loadModulesFromJson basePath universeModules namespaces = do
    CM.forM namespaces $ \ns -> do
      let filePath = basePath FP.</> CodeGeneration.moduleNameToPath ns ++ ".json"
      parseResult <- parseJsonFile filePath
      case parseResult of
        Left err -> fail $ "JSON parse error for " ++ unModuleName ns ++ ": " ++ err
        Right jsonVal -> case CodeGeneration.decodeModuleFromJson bootstrapGraph universeModules jsonVal of
          Left err -> fail $ "Module decode error for " ++ unModuleName ns ++ ": " ++ showError err
          Right mod -> do
            putStrLn $ "  Loaded: " ++ unModuleName ns
            return mod

-- | Load the hydra-jvm, hydra-java, and hydra-python modules from their
-- already-generated dist/json so they can seed the inference universe. Their
-- sources are native (Java/Python DSL, #344/#346/#370/#505); the Haskell generator
-- still needs these modules present to resolve cross-package references (e.g.
-- hydra-scala -> hydra.jvm.serde.escapeJavaString). Missing manifests (a truly
-- cold tree before Phase 1.5 seeds them) are tolerated: such a run cannot
-- reference them yet either. The decode context is the base universe (kernel etc.).
--
-- Lives here (not in a single exe's Main) so every JSON-writing driver shares
-- one loader: update-json-main AND transform-haskell-dsl-to-json must seed the
-- native packages identically, else the on-demand sync path (sync-packages.sh)
-- can't resolve hydra.jvm.serde.* for hydra-scala. (#346, #505)
-- | Load the native packages' modules from dist/json, TAGGED by owning package
-- (#511). Returns one @(package, [Module])@ row per native package so callers can
-- build routing input directly from where each module was LOADED, rather than
-- re-deriving the package from a namespace prefix — the latter drops modules whose
-- namespace lacks the package's prefix segment (e.g. hydra.gradle, owned by
-- hydra-java but with no @java@ segment).
--
-- Reads the current rich manifest schema (mainModules + mainDslModules +
-- mainEncodingModules) and, for forward/backward compatibility across the manifest
-- migration, the legacy flat @dslModules@ field too (#474). Each list is read
-- tolerantly: a package whose manifest omits a field contributes nothing for it.
loadNativePackageModulesTagged :: FilePath -> [Module] -> IO [(String, [Module])]
loadNativePackageModulesTagged distRoot baseUniverse =
    CM.forM ["hydra-jvm", "hydra-java", "hydra-python"] $ \pkg -> do
      let pkgJson = distRoot FP.</> pkg FP.</> "src" FP.</> "main" FP.</> "json"
          manifest = pkgJson FP.</> "manifest.json"
      exists <- SD.doesFileExist manifest
      if not exists
        then do
          putStrLn $ "  (skipping " ++ pkg ++ ": no manifest yet — cold tree)"
          return (pkg, [])
        else do
          mainNs   <- readManifestField pkgJson "mainModules"
          dslNs    <- readManifestFieldOrEmpty pkgJson "mainDslModules"
          encNs    <- readManifestFieldOrEmpty pkgJson "mainEncodingModules"
          legacyNs <- readManifestFieldOrEmpty pkgJson "dslModules"
          mods <- loadModulesFromJson pkgJson baseUniverse
            (L.nub (mainNs ++ dslNs ++ encNs ++ legacyNs))
          return (pkg, mods)

-- | Flattened form of 'loadNativePackageModulesTagged': all native modules in one
-- list, package tags discarded. Retained for callers that only need the module set
-- (not routing).
loadNativePackageModules :: FilePath -> [Module] -> IO [Module]
loadNativePackageModules distRoot baseUniverse =
    fmap (L.concatMap snd) (loadNativePackageModulesTagged distRoot baseUniverse)

-- | Like 'loadNativePackageModulesTagged' but returns only the module NAMES per
-- package, reading the manifest name lists directly WITHOUT decoding the modules
-- (so it needs no seeded universe). For callers that only build a routing map from
-- native packages (e.g. digest-check), where decoding would be wasted work and
-- would require a base universe. (#511)
loadNativePackageModuleNamesTagged :: FilePath -> IO [(String, [ModuleName])]
loadNativePackageModuleNamesTagged distRoot =
    CM.forM ["hydra-jvm", "hydra-java", "hydra-python"] $ \pkg -> do
      let pkgJson = distRoot FP.</> pkg FP.</> "src" FP.</> "main" FP.</> "json"
          manifest = pkgJson FP.</> "manifest.json"
      exists <- SD.doesFileExist manifest
      if not exists
        then return (pkg, [])
        else do
          mainNs   <- readManifestField pkgJson "mainModules"
          dslNs    <- readManifestFieldOrEmpty pkgJson "mainDslModules"
          encNs    <- readManifestFieldOrEmpty pkgJson "mainEncodingModules"
          legacyNs <- readManifestFieldOrEmpty pkgJson "dslModules"
          return (pkg, L.nub (mainNs ++ dslNs ++ encNs ++ legacyNs))

