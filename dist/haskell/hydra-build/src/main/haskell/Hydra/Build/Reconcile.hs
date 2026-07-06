-- Note: this is an automatically generated file. Do not edit.

-- | The pure orphan-reconcile decision: which generated files to delete

module Hydra.Build.Reconcile where

import qualified Hydra.Ast as Ast
import qualified Hydra.Codegen as Codegen
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Docs as Docs
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.File as ErrorFile
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Error.System as ErrorSystem
import qualified Hydra.Errors as Errors
import qualified Hydra.File as File
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Overlay.Haskell.Lib.Equality as Equality
import qualified Hydra.Overlay.Haskell.Lib.Lists as Lists
import qualified Hydra.Overlay.Haskell.Lib.Logic as Logic
import qualified Hydra.Overlay.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Overlay.Haskell.Lib.Sets as Sets
import qualified Hydra.Overlay.Haskell.Lib.Strings as Strings
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
import qualified Data.Set as S

-- | The extension of a relative path, or none if the filename has no '.'
extensionOf :: String -> Maybe String
extensionOf path =

      let segs = Strings.splitOn "/" path
      in (Optionals.cases (Lists.maybeLast segs) Nothing (\fileName ->
        let dotParts = Strings.splitOn "." fileName
        in (Logic.ifElse (Equality.gt (Lists.length dotParts) 1) (Lists.maybeLast dotParts) Nothing)))

-- | Whether a relative path lies under at least one prefix, matched segment-wise
isUnderAnyPrefix :: S.Set String -> String -> Bool
isUnderAnyPrefix prefixes path =

      let pathSegs = Strings.splitOn "/" path
      in (Lists.foldl (\acc -> \prefix -> Logic.or acc (
        let prefixSegs = Strings.splitOn "/" prefix
        in (Equality.equal (Lists.take (Lists.length prefixSegs) pathSegs) prefixSegs))) False (Sets.toList prefixes))

-- | The keep-set of relative JSON paths (moduleNameToPath(ns) ++ .json) for written modules
keepPathsForModules :: [Packaging.ModuleName] -> S.Set String
keepPathsForModules mods =
    Sets.fromList (Lists.map (\m -> Strings.cat [
      Codegen.moduleNameToPath m,
      ".json"]) mods)

-- | Whether a path's extension satisfies the optional extension restriction
matchesExts :: Maybe (S.Set String) -> String -> Bool
matchesExts restrictExts path =
    Optionals.cases restrictExts True (\exts -> Optionals.cases (extensionOf path) False (\ext -> Sets.member ext exts))

-- | Whether a path satisfies the optional prefix restriction
matchesPrefixes :: Maybe (S.Set String) -> String -> Bool
matchesPrefixes restrictPrefixes path =
    Optionals.cases restrictPrefixes True (\prefixes -> isUnderAnyPrefix prefixes path)

-- | The owned second-level prefixes (e.g. hydra/java) derived from written module names
ownedPrefixes :: [Packaging.ModuleName] -> S.Set String
ownedPrefixes mods = Sets.fromList (Optionals.mapOptional (\m -> secondLevelDir (Codegen.moduleNameToPath m)) mods)

-- | The pure orphan-reconcile plan: which observed paths to delete
reconcilePlan :: S.Set String -> S.Set String -> Maybe (S.Set String) -> Maybe (S.Set String) -> [String] -> [String]
reconcilePlan keepSet protectSet restrictExts restrictPrefixes observed =
    Lists.filter (\path -> Logic.and (Logic.and (matchesExts restrictExts path) (matchesPrefixes restrictPrefixes path)) (Logic.and (Logic.not (Sets.member path keepSet)) (Logic.not (Sets.member path protectSet)))) observed

-- | The second-level directory of a '/'-separated path (e.g. hydra/java/coder -> hydra/java), or none
secondLevelDir :: String -> Maybe String
secondLevelDir path =

      let segs = Strings.splitOn "/" path
      in (Logic.ifElse (Equality.gte (Lists.length segs) 2) (Just (Strings.intercalate "/" (Lists.take 2 segs))) Nothing)
