{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Json.Yaml.Decode where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import qualified Hydra.Dsl.Lib.Strings                as Strings
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms                   as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Annotations                     as Annotations
import qualified Hydra.Overlay.Haskell.Bootstrap                       as Bootstrap
import qualified Hydra.Overlay.Haskell.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Overlay.Haskell.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                  as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core                       as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Lib.Equality               as Equality
import qualified Hydra.Dsl.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Lib.Literals               as Literals
import qualified Hydra.Dsl.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Lib.Math                   as Math
import qualified Hydra.Dsl.Lib.Optionals                 as Optionals
import qualified Hydra.Dsl.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Packaging                     as Packaging
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Terms                      as MetaTerms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Variants                   as Variants
import qualified Hydra.Overlay.Haskell.Dsl.Prims                           as Prims
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Tabular                         as Tabular
import qualified Hydra.Overlay.Haskell.Dsl.Terms                           as Terms
import qualified Hydra.Overlay.Haskell.Dsl.Tests                           as Tests
import qualified Hydra.Overlay.Haskell.Dsl.Types                           as Types
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
--import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Print.Paths as PrintPaths
import qualified Hydra.Sources.Kernel.Terms.Print.Core      as PrintCore
import qualified Hydra.Sources.Kernel.Terms.Print.Graph     as PrintGraph
import qualified Hydra.Sources.Kernel.Terms.Print.Variants      as PrintVariants
import qualified Hydra.Sources.Kernel.Terms.Print.Typing    as PrintTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import Hydra.Json.Model
import qualified Hydra.Yaml.Model as YM
import qualified Hydra.Sources.Json.Decode as JsonDecode
import qualified Hydra.Sources.Yaml.Model as YamlModel


ns :: ModuleName
ns = ModuleName "hydra.json.yaml.decode"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([JsonDecode.ns, YamlModel.ns] L.++ (YamlModel.ns : KernelTypes.kernelTypesModuleNames)),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "YAML-to-JSON decoding. Converts YAML Nodes to JSON Values (may fail for non-JSON YAML), and YAML Nodes to Hydra Terms via JSON.")}
  where
    definitions = [
      toDefinition fromYaml,
      toDefinition yamlToJson]

-- | Decode a YAML Node to a Hydra Term via JSON.
fromYaml :: TypedTermDefinition (M.Map Name Type -> Name -> Type -> YM.Node -> Either String Term)
fromYaml = define "fromYaml" $
  doc "Decode a YAML node to a Hydra term via JSON decoding." $
  "types" ~> "tname" ~> "typ" ~> "node" ~>
  "jsonResult" <~ (yamlToJson @@ var "node") $
  Eithers.either
    ("err" ~> left $ var "err")
    ("json" ~> JsonDecode.fromJson @@ var "types" @@ var "tname" @@ var "typ" @@ var "json")
    (var "jsonResult")
-- | Convert a YAML Node to a JSON Value. Fails if the YAML uses non-JSON features
-- (e.g. non-string mapping keys, integer scalars without a JSON number equivalent).
yamlToJson :: TypedTermDefinition (YM.Node -> Either String Value)
yamlToJson = define "yamlToJson" $
  doc "Convert a YAML node to a JSON value. Fails for non-JSON YAML features (e.g. non-string mapping keys)." $
  "node" ~>
  cases YM._Node (var "node")
    Nothing [

    YM._Node_mapping>>: "m" ~>
      -- Convert each key-value pair; keys must be string scalars
      "convertEntry" <~ ("kv" ~>
        "keyNode" <~ (Pairs.first $ var "kv") $
        "valNode" <~ (Pairs.second $ var "kv") $
        -- Extract string key from the key node
        "keyResult" <~ (cases YM._Node (var "keyNode")
          (Just $ left $ string "non-scalar YAML mapping key") [
          YM._Node_scalar>>: "s" ~>
            cases YM._Scalar (var "s")
              (Just $ left $ string "non-string YAML mapping key") [
              YM._Scalar_str>>: "str" ~> right $ var "str"]]) $
        Eithers.either
          ("err" ~> left $ var "err")
          ("key" ~>
            "valResult" <~ (yamlToJson @@ var "valNode") $
            Eithers.map ("v" ~> pair (var "key") (var "v")) (var "valResult"))
          (var "keyResult")) $
      "entries" <~ (Eithers.mapList (var "convertEntry") (Maps.toList (var "m" :: TypedTerm (M.Map YM.Node YM.Node)))) $
      Eithers.map ("es" ~> Json.valueObject $ var "es") (var "entries"),

    YM._Node_scalar>>: "s" ~>
      cases YM._Scalar (var "s")
        Nothing [
        YM._Scalar_bool>>: "b" ~> right $ Json.valueBoolean $ var "b",
        YM._Scalar_decimal>>: "d" ~> right $ Json.valueNumber $ var "d",
        YM._Scalar_float>>: "f" ~> right $ Json.valueNumber $ Literals.float64ToDecimal $ var "f",
        YM._Scalar_int>>: "i" ~> right $ Json.valueNumber $ Literals.bigintToDecimal $ var "i",
        YM._Scalar_null>>: constant $ right Json.valueNull,
        YM._Scalar_str>>: "str" ~> right $ Json.valueString $ var "str"],

    YM._Node_sequence>>: "nodes" ~>
      "results" <~ (Eithers.mapList ("n" ~> yamlToJson @@ var "n") (var "nodes")) $
      Eithers.map ("vs" ~> Json.valueArray $ var "vs") (var "results")]

-- | Decode a YAML Node to a Hydra Term via JSON.
