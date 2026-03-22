
module Hydra.Sources.Json.Yaml.Encode where

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
import qualified Hydra.Dsl.Ext.Org.Yaml.Model                       as Yaml
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
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
import qualified Hydra.Sources.Json.Encode as JsonEncode
import qualified Hydra.Sources.Yaml.Model as YamlModel


ns :: Namespace
ns = Namespace "hydra.json.yaml.encode"

define :: String -> TTerm a -> TBinding a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
    [JsonEncode.ns, YamlModel.ns]
    KernelTypes.kernelTypesNamespaces $
    Just "JSON-to-YAML encoding. Converts JSON Values to YAML Nodes (always succeeds), and Hydra Terms to YAML Nodes via JSON."
  where
    elements = [
      toBinding jsonToYaml,
      toBinding toYaml]

-- | Convert a JSON Value to a YAML Node. This always succeeds since YAML is a superset of JSON.
jsonToYaml :: TBinding (Value -> YM.Node)
jsonToYaml = define "jsonToYaml" $
  doc "Convert a JSON value to a YAML node. Always succeeds since YAML is a superset of JSON." $
  "value" ~>
  cases _Value (var "value")
    Nothing [
    _Value_array>>: "arr" ~>
      Yaml.nodeSequence $ Lists.map ("v" ~> jsonToYaml @@ var "v") (var "arr"),

    _Value_boolean>>: "b" ~>
      Yaml.nodeScalar $ Yaml.scalarBool $ var "b",

    _Value_null>>: constant $
      Yaml.nodeScalar Yaml.scalarNull,

    _Value_number>>: "n" ~>
      Yaml.nodeScalar $ Yaml.scalarFloat $ var "n",

    _Value_object>>: "obj" ~>
      Yaml.nodeMapping $ Maps.fromList $
        Lists.map
          ("kv" ~> pair
            (Yaml.nodeScalar $ Yaml.scalarStr $ Pairs.first $ var "kv")
            (jsonToYaml @@ (Pairs.second $ var "kv")))
          (Maps.toList $ var "obj"),

    _Value_string>>: "s" ~>
      Yaml.nodeScalar $ Yaml.scalarStr $ var "s"]

-- | Encode a Hydra Term to a YAML Node via JSON.
toYaml :: TBinding (Term -> Either String YM.Node)
toYaml = define "toYaml" $
  doc "Encode a Hydra term to a YAML node via JSON encoding." $
  "term" ~>
  Eithers.map ("v" ~> jsonToYaml @@ var "v") (JsonEncode.toJson @@ var "term")
