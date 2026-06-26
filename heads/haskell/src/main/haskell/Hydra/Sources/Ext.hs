-- | Aggregates Hydra ext source modules via per-package manifest modules.
--
-- Top-level unions (`hydraExtModules`, `hydraExtJsonModules`,
-- `hydraExtDemoModules`, `genpgModules`, etc.) are defined in terms of
-- imports from each package's Manifest module plus a small set of
-- overrides for second-order meta-sources and demo-only modules. See
-- feature_290_packaging-plan.md, "Sync system redesign / Package manifests".
--
-- Overrides (things not in any package's physical manifest):
--
--   * `GenPGTransform` physically lives under `demos/` but today leaks into
--     `hydraExtModules` via `otherExtModules`. Preserved here to avoid
--     behavior change; flagged for future cleanup (it should move into a
--     demo-specific module list).
--   * The pg decode/encode meta-sources (`DecodePgMapping`, etc.) were previously
--     imported from dist/haskell/hydra-pg/; they are now synthesized at runtime
--     by the JSON-writing drivers and no longer imported here (#448).
--
-- Legacy compatibility: `hydraExtDemoModules` and `genpgModules` preserve
-- the exact content they had before the manifest refactor. In particular,
-- `pgLegacyModules` no longer includes the pg decode/encode meta-sources (they
-- are synthesized, not imported). `graphsonLegacyModules` is extracted from
-- PgManifest content for use by `genpgModules`. These legacy shapes will be
-- revisited when the demo build files move to per-package source lists.

module Hydra.Sources.Ext (
  module Hydra.Sources.Ext,
  module Hydra.Sources.All,
) where

import Hydra.Kernel
import Hydra.Sources.All

import qualified Hydra.Sources.Bench.Manifest as BenchManifest
import qualified Hydra.Sources.Coq.Manifest as CoqManifest
import qualified Hydra.Sources.Ext.Manifest as ExtManifest
import qualified Hydra.Sources.Go.Manifest as GoManifest
import qualified Hydra.Sources.TypeScript.Manifest as TypeScriptManifest
import qualified Hydra.Sources.Lisp.Manifest as LispManifest
import qualified Hydra.Sources.Pg.Manifest as PgManifest
import qualified Hydra.Sources.Rdf.Manifest as RdfManifest
import qualified Hydra.Sources.Scala.Manifest as ScalaManifest
import qualified Hydra.Sources.Wasm.Manifest as WasmManifest

-- Override: demo-only module that today leaks into hydraExtModules.
import qualified Hydra.Sources.Demos.GenPG.Transform as GenPGTransform

-- Legacy compatibility: explicit imports for the historical pgModules /
-- graphsonModules shape, used by hydraExtDemoModules and genpgModules.
import qualified Hydra.Sources.Cypher.Features as CypherFeatures
import qualified Hydra.Sources.Cypher.OpenCypher as OpenCypher
import qualified Hydra.Sources.Error.Pg as ErrorPg
import qualified Hydra.Sources.Pg.Coder as PgCoder
import qualified Hydra.Sources.Pg.Graphson.Coder as GraphsonCoder
import qualified Hydra.Sources.Pg.Graphson.Construct as GraphsonConstruct
import qualified Hydra.Sources.Pg.Graphson.Syntax as GraphsonSyntax
import qualified Hydra.Sources.Pg.Graphson.Utils as GraphsonUtils
import qualified Hydra.Sources.Pg.Mapping as PgMapping
import qualified Hydra.Sources.Pg.Model as PgModel
import qualified Hydra.Sources.Pg.Printing as PgPrinting
import qualified Hydra.Sources.Pg.Query as PgQuery
import qualified Hydra.Sources.Pg.Rdf.Environment as PgRdfEnvironment
import qualified Hydra.Sources.Pg.Rdf.Mappings as PgRdfMappings
import qualified Hydra.Sources.Pg.TermsToElements as PgTermsToElements
import qualified Hydra.Sources.Pg.Utils as PgUtils
import qualified Hydra.Sources.Tinkerpop.Features as TinkerpopFeatures
import qualified Hydra.Sources.Tinkerpop.Gremlin as Gremlin
import qualified Hydra.Sources.Tinkerpop.Language as TinkerpopLanguage
import qualified Hydra.Sources.Validate.Pg as ValidatePg

import qualified Data.List as L

-- ----------------------------------------------------------------------
-- Per-package unions (thin aliases over each package's manifest)
-- ----------------------------------------------------------------------

hydraBenchModules :: [Module]
hydraBenchModules = BenchManifest.mainModules

hydraCoqModules :: [Module]
hydraCoqModules = CoqManifest.mainModules

hydraExtPackageModules :: [Module]
hydraExtPackageModules = ExtManifest.mainModules

hydraGoModules :: [Module]
hydraGoModules = GoManifest.mainModules

-- | hydra-jvm is native-owned (#505): its canonical hydra.jvm.* JSON is produced
-- by the Java driver (UpdateJavaJson) alongside hydra-java. No Haskell DSL sources.
hydraJvmModules :: [Module]
hydraJvmModules = []

-- | hydra-java is no longer sourced from a Haskell DSL (#346/#370), exactly like
-- hydra-python. Its canonical hydra.java.* and hydra.dsl.java.* JSON is produced
-- solely by the native Java driver (hydra.UpdateJavaJson via
-- bin/update-java-json.sh) running against the published hydra-java host. The
-- Haskell DSL copy under packages/hydra-java/src/main/haskell/ has been deleted.
-- Kept as an empty binding to preserve the shape of the aggregations below.
hydraJavaModules :: [Module]
hydraJavaModules = []

hydraTypeScriptModules :: [Module]
hydraTypeScriptModules = TypeScriptManifest.mainModules

hydraLispModules :: [Module]
hydraLispModules = LispManifest.mainModules

hydraPgModules :: [Module]
hydraPgModules = PgManifest.mainModules

-- | hydra-python is no longer sourced from a Haskell DSL (#346/#370). Its
-- canonical hydra.python.* and hydra.dsl.python.* JSON is produced solely by the
-- native Python driver (bin/update-python-json.py) running against the published
-- hydra-python host. The Haskell DSL copy under
-- packages/hydra-python/src/main/haskell/ has been deleted, so there is nothing to
-- pull into the Haskell universe here. Kept as an empty binding to preserve the
-- shape of the aggregations below.
hydraPythonModules :: [Module]
hydraPythonModules = []

hydraRdfModules :: [Module]
hydraRdfModules = RdfManifest.mainModules

hydraScalaModules :: [Module]
hydraScalaModules = ScalaManifest.mainModules

hydraWasmModules :: [Module]
hydraWasmModules = WasmManifest.mainModules

-- ----------------------------------------------------------------------
-- Per-package derived-module source lists (#474)
-- ----------------------------------------------------------------------
-- Two lists per package (decoupled — #474/#475):
--   * mainDslModules      — broad: every type module gets a hydra.dsl.<x> wrapper.
--   * mainEncodingModules — narrower: source modules whose synthesized
--                           hydra.encode.<x>/hydra.decode.<x> compile across all
--                           eta-expanding targets (Java/Python). #475 will let
--                           this re-broaden toward mainDslModules.
-- hydra-java / hydra-python are native-owned, so both are empty here.

allDslModules :: [Module]
allDslModules =
  kernelDslSourceModules
  ++ haskellDslSourceModules
  ++ BenchManifest.mainDslModules
  ++ CoqManifest.mainDslModules
  ++ ExtManifest.mainDslModules
  ++ GoManifest.mainDslModules
  ++ TypeScriptManifest.mainDslModules
  ++ LispManifest.mainDslModules
  ++ PgManifest.mainDslModules
  ++ RdfManifest.mainDslModules
  ++ ScalaManifest.mainDslModules
  ++ WasmManifest.mainDslModules

allEncodingModules :: [Module]
allEncodingModules =
  kernelEncodingSourceModules
  ++ haskellEncodingSourceModules
  ++ BenchManifest.mainEncodingModules
  ++ CoqManifest.mainEncodingModules
  ++ ExtManifest.mainEncodingModules
  ++ GoManifest.mainEncodingModules
  ++ TypeScriptManifest.mainEncodingModules
  ++ LispManifest.mainEncodingModules
  ++ PgManifest.mainEncodingModules
  ++ RdfManifest.mainEncodingModules
  ++ ScalaManifest.mainEncodingModules
  ++ WasmManifest.mainEncodingModules

-- ----------------------------------------------------------------------
-- Package routing input (#474)
-- ----------------------------------------------------------------------

-- | One @(package, declaredModuleNames)@ row per package, built from the
-- compiled per-package manifest module lists. This is the source of truth
-- for 'Hydra.PackageRouting.buildRoutingMap' in the JSON-writing drivers
-- (update-json-main / update-json-manifest / transform-haskell-dsl-to-json /
-- diag-module): package ownership comes from each package's
-- @Manifest.mainModules@, never from a hardcoded name prefix.
--
-- hydra-java / hydra-python have empty compiled lists (their modules are
-- produced by native drivers and loaded from dist/json at runtime); callers
-- that need those routed should augment this list with the loaded native
-- modules tagged by package. The kernel baseline (kernel/json/other +
-- dslSourceModules + testModules) routes to hydra-kernel; haskellModules to
-- hydra-haskell.
--
-- hydra-pg additionally owns the GenPGTransform demo source, which lives
-- outside any package's Manifest.mainModules. The pg decode/encode modules
-- (hydra.{encode,decode}.pg.*) are synthesized at runtime (#448) and route
-- correctly via buildRoutingMap's derivedNames expansion from hydraPgModules.
extRoutingInput :: [(String, [ModuleName])]
extRoutingInput =
  [ ("hydra-kernel",     map moduleName (kernelModules ++ otherModules ++ dslSourceModules ++ testModules))
  , ("hydra-haskell",    map moduleName haskellModules)
  , ("hydra-bench",      map moduleName hydraBenchModules)
  , ("hydra-coq",        map moduleName hydraCoqModules)
  , ("hydra-ext",        map moduleName hydraExtPackageModules)
  , ("hydra-go",         map moduleName hydraGoModules)
  , ("hydra-jvm",        map moduleName hydraJvmModules)
  , ("hydra-java",       map moduleName hydraJavaModules)
  , ("hydra-typescript", map moduleName hydraTypeScriptModules)
  , ("hydra-lisp",       map moduleName hydraLispModules)
  , ("hydra-pg",         map moduleName (hydraPgModules
                           ++ [GenPGTransform.module_]))
  , ("hydra-python",     map moduleName hydraPythonModules)
  , ("hydra-rdf",        map moduleName hydraRdfModules)
  , ("hydra-scala",      map moduleName hydraScalaModules)
  , ("hydra-wasm",       map moduleName hydraWasmModules)
  ]

-- ----------------------------------------------------------------------
-- Overrides for second-order and demo-only modules
-- ----------------------------------------------------------------------

-- | Decode-side meta-sources for hydra-pg. Synthesized at runtime by the
--   JSON-writing drivers from hydra-pg's type modules (#448); no longer
--   imported from dist/haskell/hydra-pg/.
hydraExtDecodingModules :: [Module]
hydraExtDecodingModules = []

-- | Encode-side meta-sources for hydra-pg. Synthesized at runtime by the
--   JSON-writing drivers from hydra-pg's type modules (#448); no longer
--   imported from dist/haskell/hydra-pg/.
hydraExtEncodingModules :: [Module]
hydraExtEncodingModules = []

-- ----------------------------------------------------------------------
-- Legacy compatibility shapes
-- ----------------------------------------------------------------------

-- | Historical pgModules: the subset of PgManifest content plus the
--   decode/encode pg meta-sources, used by hydraExtDemoModules and
--   genpgModules. Narrower than PgManifest.mainModules (excludes Graphson,
--   Graphviz, Gql/PathAlgebra).
pgLegacyModules :: [Module]
pgLegacyModules = [
  CypherFeatures.module_,
  ErrorPg.module_,
  Gremlin.module_,
  OpenCypher.module_,
  PgCoder.module_,
  PgMapping.module_,
  PgModel.module_,
  PgPrinting.module_,
  PgQuery.module_,
  PgRdfEnvironment.module_,
  PgRdfMappings.module_,
  PgTermsToElements.module_,
  PgUtils.module_,
  TinkerpopFeatures.module_,
  TinkerpopLanguage.module_,
  ValidatePg.module_]

-- | Historical graphsonModules, used to compose genpgModules.
graphsonLegacyModules :: [Module]
graphsonLegacyModules = [
  GraphsonCoder.module_,
  GraphsonConstruct.module_,
  GraphsonSyntax.module_,
  GraphsonUtils.module_]

-- ----------------------------------------------------------------------
-- Top-level unions consumed by execs and demos
-- ----------------------------------------------------------------------

-- | Coder modules for the bootstrap-relevant languages: Haskell, Java, Python,
--   Scala, and Lisp. JVM common modules (#505) are included alongside Java.
hydraBootstrapCoderModules :: [Module]
hydraBootstrapCoderModules =
  haskellModules ++ hydraJvmModules ++ hydraJavaModules ++ hydraPythonModules ++ hydraScalaModules ++ hydraLispModules

-- | Essential hydra-ext modules: the JVM-common, Java, and Python coder families.
hydraExtEssentialModules :: [Module]
hydraExtEssentialModules = hydraJvmModules ++ hydraJavaModules ++ hydraPythonModules

-- | The full union of modules in the ext universe, as today.
--
-- Constructed from the per-package manifest unions plus the decode/encode
-- meta-source overrides and the demo-only GenPGTransform. The second-order
-- meta-sources and GenPGTransform are preserved here so that today's sync
-- output matches exactly; both are flagged for future cleanup.
hydraExtModules :: [Module]
hydraExtModules =
     hydraCoqModules
  ++ hydraExtPackageModules
  ++ hydraGoModules
  ++ hydraJvmModules
  ++ hydraJavaModules
  ++ hydraTypeScriptModules
  ++ hydraLispModules
  ++ hydraPgModules
  ++ hydraPythonModules
  ++ hydraRdfModules
  ++ hydraScalaModules
  ++ [GenPGTransform.module_]

-- | All modules that should be exported to JSON, including decode/encode modules
--   that are not part of hydraExtModules in all contexts.
hydraExtJsonModules :: [Module]
hydraExtJsonModules = hydraExtModules
  ++ hydraExtDecodingModules
  ++ hydraExtEncodingModules

-- | Ext modules whose generated code is checked into hydra-ext/src/gen-main/
--   for each target language (Haskell, Java, Python). These are the modules
--   needed by hydra-ext demos and other target-language code. Not to be
--   confused with language coder modules.
--
-- Uses the legacy pgLegacyModules shape so that the list of modules exposed
-- to `bootstrap-from-json --ext-only` is identical to pre-refactor behavior.
hydraExtDemoModules :: [Module]
hydraExtDemoModules = L.nub $ L.concat [pgLegacyModules, genpgModules, hydraRdfModules]

-- | Legacy alias.
hydraExtJavaModules :: [Module]
hydraExtJavaModules = hydraExtDemoModules

-- | All hydra-ext modules for the GenPG demo.
--
-- Historically: graphsonLegacyModules ++ pgLegacyModules ++ [GenPGTransform].
genpgModules :: [Module]
genpgModules = graphsonLegacyModules ++ pgLegacyModules ++ [GenPGTransform.module_]
