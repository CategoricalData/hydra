
module Hydra.Sources.Json.Yaml.Decode where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.Grammars                        as Grammars
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Accessors                  as Accessors
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Grammar                    as Grammar
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Module                     as Module
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
--import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Grammars       as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Meta      as ShowMeta
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
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
import qualified Hydra.Ext.Org.Yaml.Model as YM
import qualified Hydra.Sources.Json.Decode as JsonDecode
import qualified Hydra.Sources.Yaml.Model as YamlModel


ns :: Namespace
ns = Namespace "hydra.json.yaml.decode"

define :: String -> TTerm a -> TBinding a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
    [JsonDecode.ns, YamlModel.ns]
    KernelTypes.kernelTypesNamespaces $
    Just "YAML-to-JSON decoding. Converts YAML Nodes to JSON Values (may fail for non-JSON YAML), and YAML Nodes to Hydra Terms via JSON."
  where
    elements = [
      toBinding yamlToJson,
      toBinding fromYaml]

-- | Convert a YAML Node to a JSON Value. Fails if the YAML uses non-JSON features
-- (e.g. non-string mapping keys, integer scalars without a JSON number equivalent).
yamlToJson :: TBinding (YM.Node -> Either String Value)
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
        Eithers.either_
          ("err" ~> left $ var "err")
          ("key" ~>
            "valResult" <~ (yamlToJson @@ var "valNode") $
            Eithers.map ("v" ~> pair (var "key") (var "v")) (var "valResult"))
          (var "keyResult")) $
      "entries" <~ (Eithers.mapList (var "convertEntry") (Maps.toList $ var "m")) $
      Eithers.map ("es" ~> Json.valueObject $ Maps.fromList $ var "es") (var "entries"),

    YM._Node_scalar>>: "s" ~>
      cases YM._Scalar (var "s")
        Nothing [
        YM._Scalar_bool>>: "b" ~> right $ Json.valueBoolean $ var "b",
        YM._Scalar_float>>: "f" ~> right $ Json.valueNumber $ var "f",
        YM._Scalar_int>>: "i" ~> right $ Json.valueNumber $ Literals.bigintToBigfloat $ var "i",
        YM._Scalar_null>>: constant $ right Json.valueNull,
        YM._Scalar_str>>: "str" ~> right $ Json.valueString $ var "str"],

    YM._Node_sequence>>: "nodes" ~>
      "results" <~ (Eithers.mapList ("n" ~> yamlToJson @@ var "n") (var "nodes")) $
      Eithers.map ("vs" ~> Json.valueArray $ var "vs") (var "results")]

-- | Decode a YAML Node to a Hydra Term via JSON.
fromYaml :: TBinding (M.Map Name Type -> Name -> Type -> YM.Node -> Either String Term)
fromYaml = define "fromYaml" $
  doc "Decode a YAML node to a Hydra term via JSON decoding." $
  "types" ~> "tname" ~> "typ" ~> "node" ~>
  "jsonResult" <~ (yamlToJson @@ var "node") $
  Eithers.either_
    ("err" ~> left $ var "err")
    ("json" ~> JsonDecode.fromJson @@ var "types" @@ var "tname" @@ var "typ" @@ var "json")
    (var "jsonResult")
