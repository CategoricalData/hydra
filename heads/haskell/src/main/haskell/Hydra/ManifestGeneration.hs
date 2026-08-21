-- | Per-package manifest.json generation, split out of 'Hydra.Generation' (#622)
-- so the cold-seeder's headmods (which never call this) can drop their
-- 'Hydra.Build.ManifestWriter' coupling. Used only by the update-json-manifest
-- driver, which always runs against a local (non-cold-seed) build.
module Hydra.ManifestGeneration (
  writeLanguagesJson,
  writePerPackageManifestsJson,
) where

import Hydra.Kernel
import Hydra.Generation (currentModuleFormatVersion)
import Hydra.PackageRouting (RoutingMap, groupByPackageIn)
import qualified Hydra.Build.ManifestWriter as GenManifestWriter
import qualified Hydra.Json.Model as Json
import qualified Hydra.Json.Writer as JsonWriter
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
