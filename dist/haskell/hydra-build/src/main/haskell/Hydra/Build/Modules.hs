-- Note: this is an automatically generated file. Do not edit.

-- | Pure module-list utilities shared by the code-generation drivers

module Hydra.Build.Modules where

import qualified Hydra.Ast as Ast
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
import qualified Hydra.Overlay.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Overlay.Haskell.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Strip as Strip
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

-- | Deduplicate a list of strings, preserving first-occurrence order
dedupPreservingOrder :: Eq t0 => ([t0] -> [t0])
dedupPreservingOrder xs = Lists.nub xs

-- | Keep only modules outside the hydra.* and hydra.json.yaml.* namespaces
filterKernelModules :: [Packaging.Module] -> [Packaging.Module]
filterKernelModules mods =

      let startsWith =
              \prefix -> \s ->
                let parts = Strings.splitOn prefix s
                in (Optionals.cases (Lists.uncons parts) False (\uc -> Logic.and (Strings.null (Pairs.first uc)) (Logic.not (Lists.null (Pairs.second uc)))))
      in (Lists.filter (\m ->
        let name = Packaging.unModuleName (Packaging.moduleName m)
        in (Logic.and (Logic.not (startsWith "hydra." name)) (Logic.not (startsWith "hydra.json.yaml." name)))) mods)

-- | Keep only modules containing at least one type-defining binding
filterTypeModules :: [Packaging.Module] -> [Packaging.Module]
filterTypeModules mods =
    Lists.filter (\m -> Lists.foldl (\acc -> \d -> Logic.or acc (case d of
      Packaging.DefinitionType _ -> True
      _ -> False)) False (Packaging.moduleDefinitions m)) mods

-- | The first two forward-slash-separated segments of a relative path, joined by /
secondLevelDir :: String -> Maybe String
secondLevelDir rel =

      let parts = Strings.splitOn "/" rel
      in (Logic.ifElse (Equality.lt (Lists.length parts) 2) Nothing (Just (Strings.intercalate "/" (Lists.take 2 parts))))

-- | Strip System F type annotations from every module in a list
stripAllTermTypes :: [Packaging.Module] -> [Packaging.Module]
stripAllTermTypes mods = Lists.map (\m -> stripTermTypes m) mods

-- | Strip System F type annotations from all term bodies in a module
stripTermTypes :: Packaging.Module -> Packaging.Module
stripTermTypes m =
    Packaging.Module {
      Packaging.moduleName = (Packaging.moduleName m),
      Packaging.moduleMetadata = (Packaging.moduleMetadata m),
      Packaging.moduleDependencies = (Packaging.moduleDependencies m),
      Packaging.moduleDefinitions = (Lists.map (\d -> case d of
        Packaging.DefinitionType v0 -> Packaging.DefinitionType v0
        Packaging.DefinitionPrimitive v0 -> Packaging.DefinitionPrimitive v0
        Packaging.DefinitionTerm v0 -> Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Packaging.termDefinitionName v0),
          Packaging.termDefinitionMetadata = (Packaging.termDefinitionMetadata v0),
          Packaging.termDefinitionSignature = Nothing,
          Packaging.termDefinitionBody = (Strip.removeTypesFromTerm (Packaging.termDefinitionBody v0))})) (Packaging.moduleDefinitions m))}
