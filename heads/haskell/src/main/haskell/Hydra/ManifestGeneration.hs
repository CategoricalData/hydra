-- | Per-package manifest.json generation, split out of 'Hydra.Generation' (#622)
-- so the cold-seeder's headmods (which never call this) can drop their
-- 'Hydra.Build.ManifestWriter' coupling. Used only by the update-json-manifest
-- driver, which always runs against a local (non-cold-seed) build.
module Hydra.ManifestGeneration (
  writeCoderBaselineJson,
  writeExpectedLibrariesJson,
  writeLanguagesJson,
  writePerPackageManifestsJson,
) where

import Hydra.Kernel
import Hydra.Generation (currentModuleFormatVersion)
import Hydra.PackageRouting (RoutingMap, groupByPackageIn)
import qualified Hydra.Build.ManifestWriter as GenManifestWriter
import qualified Hydra.Json.Model as Json
import qualified Hydra.Json.Writer as JsonWriter
import qualified Hydra.Sources.Build.Libraries as Libraries
import qualified Hydra.Sources.Build.PublishSets as PublishSets
import qualified Hydra.Sources.Build.Registry as Registry

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y
import qualified System.Directory as SD
import qualified System.FilePath as FP

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
          -- #607: field assembly + serialization delegated to the generated
          -- hydra.build.manifestWriter (packageManifestJson), which reproduces this
          -- driver's alphabetized field order and sorted namespace arrays exactly.
          -- moduleFormatVersion is Haskell-specific, so it is spliced into the
          -- generated fields at its alphabetical position.
          Json.ValueObject genFields = GenManifestWriter.packageManifestJson
            pkg mainForPkg dslForPkg encForPkg testForPkg
          jsonVal = Json.ValueObject $ insertAfter "manifestFormatVersion"
            ("moduleFormatVersion", Json.ValueNumber currentModuleFormatVersion) genFields
          jsonStr = JsonWriter.printJson jsonVal
          pkgDir  = distJsonRoot FP.</> pkg FP.</> "src" FP.</> "main" FP.</> "json"
          filePath = pkgDir FP.</> "manifest.json"
      SD.createDirectoryIfMissing True pkgDir
      writeFile filePath (jsonStr ++ "\n")
      putStrLn $ "Wrote manifest: " ++ filePath
  where
    insertAfter key kv (f@(k, _):fs) = f : if k == key then kv : fs else insertAfter key kv fs
    insertAfter _ kv [] = [kv]

-- | Write the evaluated language scope lists to
-- @dist/json/hydra-build/src/main/json/languages.json@ (#416 piece 3 step 2).
--
-- This is the generated data artifact the build shell scripts read (via @jq@)
-- instead of hardcoding language enumerations: the canonical bash<->hydra bridge
-- (Josh-ratified Option A). The lists come straight from
-- 'Hydra.Sources.Build.Registry' raw @[String]@ bindings — the same single source
-- of truth the DSL defs wrap — so the artifact and the generated hydra.build.registry
-- module can never drift. Keys are the alias/scope names the scripts already use;
-- each array preserves the native per-script order exactly (byte-parity with the
-- retired hardcoded values). Emitted with the same 'Hydra.Json.Writer' the manifests
-- use, so it is a normal generated dist/json artifact (tracked, like manifest.json).
writeLanguagesJson :: FilePath -> IO ()
writeLanguagesJson distJsonRoot = do
    let jsonVal = Json.ValueObject [
          ("all",            arr Registry.allLanguageNameList),
          ("lisp",           arr Registry.lispDialectNameList),
          ("testMatrix",     arr Registry.testMatrixNameList),
          ("benchHosts",     arr Registry.benchHostNameList),
          ("inferenceBench", arr Registry.inferenceBenchHostNameList),
          ("benchDefault",   arr Registry.benchDefaultNameList)]
        jsonStr = JsonWriter.printJson jsonVal
        pkgDir  = distJsonRoot FP.</> "hydra-build" FP.</> "src" FP.</> "main" FP.</> "json"
        filePath = pkgDir FP.</> "languages.json"
    SD.createDirectoryIfMissing True pkgDir
    writeFile filePath (jsonStr ++ "\n")
    putStrLn $ "Wrote languages: " ++ filePath
  where
    arr names = Json.ValueArray (Json.ValueString <$> names)

-- | Write the expected @hydra.lib.\<sub>@ library set to
-- @dist/json/hydra-build/src/main/json/expected-libraries.json@ (#416/#533).
--
-- The Option-A emitted-artifact bridge for #533: every self-hosting host is
-- expected to register, load, and test the same set of @hydra.lib.*@ modules.
-- The canonical list lives once in 'Hydra.Sources.Build.Libraries.expectedLibraryNames'
-- (validated against the actual kernel @Lib/*.hs@ set by @Hydra.BuildLibrariesSpec@);
-- emitting it as a data artifact lets the non-Haskell host loaders (the CL/EL flat
-- loaders today) fail immediately, with the named culprit, if their native runtime
-- is missing a library the kernel provides — the whole point of #533 — WITHOUT
-- needing the (still-unpublished) hydra.build library linked into each host.
--
-- Names are emitted BARE (@"chars"@, @"effects"@, …), matching 'expectedLibraryNames'
-- and the @hydra.lib.\<sub>@ sub-namespace convention; each loader knows its own
-- @hydra.lib.@ prefix. Emitted with the same 'Hydra.Json.Writer' the manifests use,
-- so it is a normal tracked generated dist/json artifact.
writeExpectedLibrariesJson :: FilePath -> IO ()
writeExpectedLibrariesJson distJsonRoot = do
    let jsonVal = Json.ValueObject [
          ("expectedLibraries", arr Libraries.expectedLibraryNames)]
        jsonStr = JsonWriter.printJson jsonVal
        pkgDir  = distJsonRoot FP.</> "hydra-build" FP.</> "src" FP.</> "main" FP.</> "json"
        filePath = pkgDir FP.</> "expected-libraries.json"
    SD.createDirectoryIfMissing True pkgDir
    writeFile filePath (jsonStr ++ "\n")
    putStrLn $ "Wrote expected-libraries: " ++ filePath
  where
    arr names = Json.ValueArray (Json.ValueString <$> names)

-- | Write the coder-baseline package set to
-- @dist/json/hydra-build/src/main/json/coder-baseline.json@ (#416).
--
-- The Option-A data bridge for the per-package coder-load decision: the two packages
-- (@hydra-kernel@, @hydra-haskell@) that need NO coder modules loaded when their JSON
-- is transformed to a target. Every other package needs @--include-coders@. This
-- promotes the @case "$PACKAGE"@ that @transform-json-to-target.sh@ and
-- @cold-seed-dist-haskell.sh@ each hand-maintained (the latter literally commented
-- that it "mirror[s]" the former — the drift this removes). The names live once in
-- 'Hydra.Sources.Build.PublishSets.coderBaselineNames' (wrapped by the translingual
-- 'coderBaselinePackages' / 'requiresCoders'); the drivers read this artifact instead
-- of branching on name literals. Emitted with the same 'Hydra.Json.Writer' the
-- manifests use — a normal tracked generated dist/json artifact, like languages.json.
writeCoderBaselineJson :: FilePath -> IO ()
writeCoderBaselineJson distJsonRoot = do
    let jsonVal = Json.ValueObject [
          ("coderBaseline", arr PublishSets.coderBaselineNames)]
        jsonStr = JsonWriter.printJson jsonVal
        pkgDir  = distJsonRoot FP.</> "hydra-build" FP.</> "src" FP.</> "main" FP.</> "json"
        filePath = pkgDir FP.</> "coder-baseline.json"
    SD.createDirectoryIfMissing True pkgDir
    writeFile filePath (jsonStr ++ "\n")
    putStrLn $ "Wrote coder-baseline: " ++ filePath
  where
    arr names = Json.ValueArray (Json.ValueString <$> names)
