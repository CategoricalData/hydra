-- Note: this is an automatically generated file. Do not edit.

-- | Manifest-derived, fail-loud routing of modules to their owning packages

module Hydra.Build.Routing where

import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Decoding as Decoding
import qualified Hydra.Docs as Docs
import qualified Hydra.Dsls as Dsls
import qualified Hydra.Encoding as Encoding
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.File as ErrorFile
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Error.System as ErrorSystem
import qualified Hydra.Errors as Errors
import qualified Hydra.File as File
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Overlay.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Overlay.Haskell.Lib.Lists as Lists
import qualified Hydra.Overlay.Haskell.Lib.Maps as Maps
import qualified Hydra.Overlay.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Overlay.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Overlay.Haskell.Lib.Strings as Strings
import qualified Hydra.Names as Names
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.System as System
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Time as Time
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M

-- | Build a routing map from each package's declared modules
buildRoutingMap :: [(t0, [Packaging.ModuleName])] -> M.Map Packaging.ModuleName t0
buildRoutingMap pkgs =

      let declaredPairs = Lists.concat (Lists.map (\p -> Lists.map (\m -> (m, (Pairs.first p))) (Pairs.second p)) pkgs)
          derivedPairs =
                  Lists.concat (Lists.map (\p -> Lists.concat (Lists.map (\m -> Lists.map (\d -> (d, (Pairs.first p))) (derivedNames m)) (Pairs.second p))) pkgs)
      in (Maps.union (Maps.fromList declaredPairs) (Maps.fromList derivedPairs))

-- | The derived module names produced from a source module name
derivedNames :: Packaging.ModuleName -> [Packaging.ModuleName]
derivedNames m =

      let dsl = Dsls.dslModuleName m
          enc = Encoding.encodeModuleName m
          dec = Decoding.decodeModuleName m
      in [
        dsl,
        enc,
        dec,
        (sourceWrapperName enc),
        (sourceWrapperName dec)]

-- | Partition a list of modules by owning package, sorted by package name
groupByPackageIn :: Ord t0 => (M.Map Packaging.ModuleName t0 -> [Packaging.Module] -> Either Errors.Error [(t0, [Packaging.Module])])
groupByPackageIn rm mods =

      let step =
              \acc -> \m -> Eithers.bind acc (\grouped -> Eithers.bind (namespaceToPackageIn rm (Packaging.moduleName m)) (\pkg -> Right (Maps.alter (\mods0 -> Just (Lists.cons m (Optionals.fromOptional [] mods0))) pkg grouped)))
      in (Eithers.map (\grouped -> Maps.toList grouped) (Lists.foldl step (Right Maps.empty) mods))

-- | Map a module name to the package that owns it, failing loudly if unrouted
namespaceToPackageIn :: M.Map Packaging.ModuleName t0 -> Packaging.ModuleName -> Either Errors.Error t0
namespaceToPackageIn rm ns =
    Optionals.cases (Maps.lookup ns rm) (Left (Errors.ErrorOther (Errors.OtherError (Strings.cat [
      "unrouted module: ",
      (Packaging.unModuleName ns),
      " is not declared in any package's manifest (RoutingMap). Add it to the owning package's Manifest.mainModules."])))) (\p -> Right p)

-- | The hydra.sources.<...> wrapper name for a derived encode/decode module name
sourceWrapperName :: Packaging.ModuleName -> Packaging.ModuleName
sourceWrapperName ns =

      let parts = Strings.splitOn "." (Packaging.unModuleName ns)
          fallback =
                  Packaging.ModuleName (Strings.cat [
                    "hydra.sources.",
                    (Packaging.unModuleName ns)])
      in (Optionals.cases (Lists.uncons parts) fallback (\uc -> Packaging.ModuleName (Strings.cat [
        "hydra.sources.",
        (Strings.intercalate "." (Pairs.second uc))])))
