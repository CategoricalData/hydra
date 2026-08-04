
module Hydra.Sources.Build.PackagingProfile where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import qualified Hydra.Dsl.Paths        as Paths
import qualified Hydra.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core         as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Lib.Chars    as Chars
import qualified Hydra.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Literals as Literals
import qualified Hydra.Dsl.Lib.Logic    as Logic
import qualified Hydra.Dsl.Lib.Maps     as Maps
import qualified Hydra.Dsl.Lib.Math     as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Lib.Sets     as Sets
import qualified Hydra.Dsl.Lib.Strings  as Strings
import qualified Hydra.Overlay.Haskell.Dsl.Literals          as Literals
import qualified Hydra.Overlay.Haskell.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Base         as MetaBase
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Terms        as MetaTerms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Prims             as Prims
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Tabular           as Tabular
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Testing      as Testing
import qualified Hydra.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Overlay.Haskell.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Overlay.Haskell.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Variants     as Variants
import qualified Hydra.Dsl.Errors       as Error
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Validate.Packaging as ValidatePackaging


-- | Per-package packaging-validation policy for the Hydra packaging split (#559, #575).
--
-- A DSL-level, translingual port of the per-package profile-selection policy in
-- @Hydra.Generation@ (heads/haskell/src/main/haskell/Hydra/Generation.hs): which
-- packages are held to the full, fatal-on-everything packaging profile, which are
-- documentation-relaxed, and which are excluded from structural validation entirely
-- because their dist/json is written by a separate native driver.
--
-- The two hardcoded package lists here are an INTERIM mechanism (see #512): the
-- intended long-term home for per-package validation configuration is a typed
-- 'PackageValidationConfiguration' declared in each package's package.json, richer
-- than a strict/relaxed toggle. These lists are deliberately the simplest thing that
-- implements today's actual policy without blocking on that design.
--
-- The two profile constructors themselves ('kernelDefaultPackagingProfile',
-- 'kernelPackagingProfileWithDocWarnings') are NOT redefined here -- they are the
-- translingual kernel definitions from @hydra.validate.packaging@, referenced as-is.
ns :: ModuleName
ns = ModuleName "hydra.build.packagingProfile"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([ValidatePackaging.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Per-package packaging-validation policy: strict vs. doc-relaxed profile selection and native-owned exclusion")}
  where
   definitions = [
     toDefinition isNativeOwned,
     toDefinition nativeOwnedPackagingPackages,
     toDefinition packagingProfileFor,
     toDefinition strictPackagingPackages]

-- | Whether a package's dist/json is written by a separate NATIVE driver
-- (bin/generate-hydra-java-from-java.sh, -python-from-python.sh) rather than by
-- this driver's own write pass, i.e. whether its name is in
-- 'nativeOwnedPackagingPackages'. Native-owned packages are excluded from
-- structural (packaging) validation entirely: their on-disk JSON is stale-by-design
-- at the pipeline point where structural validation runs (sync.sh's Phase 1.5
-- auto-heal brings it current only AFTER this driver), so validating it here would
-- fail on staleness the pipeline itself has not resolved yet, not on a real defect.
--
-- This is the positive form of the @notElem nativeOwnedPackagingPackages@ guard used
-- at the structural-validation call site.
isNativeOwned :: TypedTermDefinition (Package -> Bool)
isNativeOwned = define "isNativeOwned" $
  doc "Whether a package's dist/json is written by a separate native driver, so it is excluded from structural validation" $
  "pkg" ~>
  Lists.member (Packaging.packageName $ var "pkg") (asTerm nativeOwnedPackagingPackages)

-- | Packages whose dist/json is written by a separate NATIVE driver
-- (bin/generate-hydra-java-from-java.sh, -python-from-python.sh), not by this
-- driver's own write pass. Their on-disk JSON is loaded only to seed the inference
-- universe and is legitimately stale at the structural-validation point in the
-- pipeline; validating that stale JSON here would fail on staleness the pipeline
-- itself has not resolved yet, so these packages are excluded from structural
-- (packaging) validation entirely (see 'isNativeOwned').
nativeOwnedPackagingPackages :: TypedTermDefinition [PackageName]
nativeOwnedPackagingPackages = define "nativeOwnedPackagingPackages" $
  doc "Packages whose dist/json is written by a separate native driver, hence excluded from structural validation" $
  list [
    wrap _PackageName (string "hydra-jvm"),
    wrap _PackageName (string "hydra-java"),
    wrap _PackageName (string "hydra-python")]

-- | Select the packaging 'ValidationProfile' for a package: the full,
-- fatal-on-everything 'kernelDefaultPackagingProfile' for packages in
-- 'strictPackagingPackages', the documentation-relaxed
-- 'kernelPackagingProfileWithDocWarnings' for every other package.
--
-- This per-package selection (rather than one profile shared by every package) is the
-- #575 incremental-remediation mechanism: re-enabling comprehensive validation
-- surfaced a large, pre-existing documentation-completeness backlog outside the
-- kernel, and treating it as fatal everywhere at once would turn every sync fleet-wide
-- red until fully remediated. Packages are added to 'strictPackagingPackages' as their
-- own backlogs are cleared, expanding the fully-fatal set incrementally.
packagingProfileFor :: TypedTermDefinition (Package -> ValidationProfile)
packagingProfileFor = define "packagingProfileFor" $
  doc "Select the packaging validation profile for a package: full-fatal for strict packages, doc-relaxed otherwise" $
  "pkg" ~>
  Logic.ifElse (Lists.member (Packaging.packageName $ var "pkg") (asTerm strictPackagingPackages))
    (asTerm ValidatePackaging.kernelDefaultPackagingProfile)
    (asTerm ValidatePackaging.kernelPackagingProfileWithDocWarnings)

-- | Packages held to the full 'kernelDefaultPackagingProfile' (every rule, including
-- documentation completeness, is fatal). Every other package uses the
-- documentation-relaxed 'kernelPackagingProfileWithDocWarnings' instead (#575).
-- hydra-kernel is held to the full bar from the start per policy; other packages are
-- added here as their own documentation-completeness backlogs are remediated,
-- expanding the fully-fatal set incrementally rather than gating the whole fleet on
-- the total backlog at once.
strictPackagingPackages :: TypedTermDefinition [PackageName]
strictPackagingPackages = define "strictPackagingPackages" $
  doc "Packages held to the full, fatal-on-everything packaging profile" $
  list [
    wrap _PackageName (string "hydra-kernel"),
    wrap _PackageName (string "hydra-build"),
    wrap _PackageName (string "hydra-haskell"),
    wrap _PackageName (string "hydra-go"),
    wrap _PackageName (string "hydra-lisp"),
    wrap _PackageName (string "hydra-wasm"),
    wrap _PackageName (string "hydra-typescript"),
    wrap _PackageName (string "hydra-rdf"),
    wrap _PackageName (string "hydra-pg"),
    wrap _PackageName (string "hydra-ext")]
