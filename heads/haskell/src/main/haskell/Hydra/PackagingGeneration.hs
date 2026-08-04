-- | Packaging-validation policy consumption for the full-sync driver.
--
-- #559 Step E: the per-package packaging-validation policy (which packages are
-- held to the full, fatal-on-everything profile, which are documentation-relaxed,
-- and which are excluded from structural validation entirely) was promoted into
-- the translingual @hydra.build.packagingProfile@ module. This module is the
-- native-side CONSUMER of that generated policy — a thin delegation, plus the
-- 'validatePackagesStructural' entry point that applies it.
--
-- It lives here, separate from "Hydra.Generation", specifically so that the
-- cold-seeder's headmods copy of Generation.hs stays free of any
-- @Hydra.Build.*@ import that is not confirmed published at the pinned
-- hydra-build version. 'Hydra.Generation' is copied into the cold-seeder's
-- headmods (json-driver's cold-seed-dist-haskell.sh) and compiled against the
-- PUBLISHED hydra lib; importing the newly-added, not-yet-published
-- @Hydra.Build.PackagingProfile@ from there would break cold-seed-from-json
-- (the #560/#607 revert-main class, enforced by cold-seed-dist-haskell.sh's
-- ALLOWED_BUILD_IMPORTS invariant). This mirrors the same reason
-- 'Hydra.ExtGeneration' is kept out of the cold-seeder's import graph
-- (see ColdSeedMain.hs). Only "Hydra.Generation"'s Build-free helpers
-- ('isDerivedModule', 'ValidationFindings', 'ValidationResult') are imported
-- here; the Build.* coupling stays on this (full-sync-only) side of the seam.
--
-- The sole consumer of this module is update-json-main/Main.hs (the full sync's
-- structural-validation gate). The cold-seed path does not run structural
-- validation, so nothing on that path needs these definitions.
module Hydra.PackagingGeneration (
  module Hydra.PackagingGeneration,
) where

import Hydra.Kernel
-- Hydra.Kernel re-exports most packaging/validation types (Package, PackageName,
-- ValidationProfile, ValidationResult, validationResultErrors) but NOT
-- Hydra.Error.Packaging (InvalidPackageError) -- import it explicitly, as
-- Hydra.Generation does.
import Hydra.Error.Packaging (InvalidPackageError)
import Hydra.Generation (ValidationFindings(..), isDerivedModule)
import qualified Hydra.Build.PackagingProfile as GenPackagingProfile
import qualified Hydra.Validate.Packaging as ValidatePackaging

-- | Packages held to the full 'ValidatePackaging.kernelDefaultPackagingProfile'
-- (every rule, including documentation completeness, is fatal). Every other
-- package uses 'ValidatePackaging.kernelPackagingProfileWithDocWarnings'
-- instead (#575).
--
-- #559 Step E: promoted to the translingual 'hydra.build.packagingProfile'
-- module; this is now a thin delegation to the generated
-- 'GenPackagingProfile.strictPackagingPackages', the single source of truth.
strictPackagingPackages :: [PackageName]
strictPackagingPackages = GenPackagingProfile.strictPackagingPackages

-- | Packages whose dist/json is written by a separate NATIVE driver
-- (bin/generate-hydra-java-from-java.sh, -python-from-python.sh), not by
-- this driver's own write pass. Their on-disk JSON is legitimately stale at
-- the structural-validation point in the pipeline, so they are excluded from
-- 'validatePackagesStructural' entirely.
--
-- #559 Step E: promoted to 'hydra.build.packagingProfile'; thin delegation to
-- the generated 'GenPackagingProfile.nativeOwnedPackagingPackages'.
nativeOwnedPackagingPackages :: [PackageName]
nativeOwnedPackagingPackages = GenPackagingProfile.nativeOwnedPackagingPackages

-- | Select the packaging 'ValidationProfile' for a package: the full,
-- fatal-on-everything profile for packages in 'strictPackagingPackages',
-- the documentation-relaxed profile for every other package.
--
-- #559 Step E: promoted to 'hydra.build.packagingProfile'; thin delegation to
-- the generated 'GenPackagingProfile.packagingProfileFor'.
packagingProfileFor :: Package -> ValidationProfile
packagingProfileFor = GenPackagingProfile.packagingProfileFor

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
