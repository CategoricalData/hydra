
module Hydra.Sources.Json.Yaml.Encode where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import qualified Hydra.Core.Dsl.Lib.Strings                as Strings
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms                   as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Annotations                     as Annotations
import qualified Hydra.Core.Overlay.Haskell.Bootstrap                       as Bootstrap
import qualified Hydra.Core.Overlay.Haskell.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Core.Overlay.Haskell.Dsl.Literals                        as Literals
import qualified Hydra.Core.Dsl.Paths                  as Paths
import qualified Hydra.Core.Dsl.Ast                        as Ast
import qualified Hydra.Core.Overlay.Haskell.Dsl.Base                       as MetaBase
import qualified Hydra.Core.Dsl.Coders                     as Coders
import qualified Hydra.Core.Dsl.Util                    as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core                       as Core
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Core.Dsl.Json.Model                       as Json
import qualified Hydra.Core.Dsl.Lib.Chars                  as Chars
import qualified Hydra.Core.Dsl.Lib.Eithers                as Eithers
import qualified Hydra.Core.Dsl.Lib.Equality               as Equality
import qualified Hydra.Core.Dsl.Lib.Lists                  as Lists
import qualified Hydra.Core.Dsl.Lib.Literals               as Literals
import qualified Hydra.Core.Dsl.Lib.Logic                  as Logic
import qualified Hydra.Core.Dsl.Lib.Maps                   as Maps
import qualified Hydra.Core.Dsl.Lib.Math                   as Math
import qualified Hydra.Core.Dsl.Lib.Optionals                 as Optionals
import qualified Hydra.Core.Dsl.Lib.Pairs                  as Pairs
import qualified Hydra.Core.Dsl.Lib.Sets                   as Sets
import qualified Hydra.Core.Dsl.Packaging                     as Packaging
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Core.Dsl.Topology                   as Topology
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Core.Dsl.Typing                     as Typing
import qualified Hydra.Core.Dsl.Util                       as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Core.Dsl.Yaml.Model                       as Yaml
import qualified Hydra.Core.Overlay.Haskell.Dsl.Prims                           as Prims
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Core.Overlay.Haskell.Dsl.Terms                           as Terms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Tests                           as Tests
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types                           as Types
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
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
import Hydra.Core.Json.Model
import qualified Hydra.Core.Yaml.Model as YM
import qualified Hydra.Sources.Json.Encode as JsonEncode
import qualified Hydra.Sources.Yaml.Model as YamlModel


ns :: ModuleName
ns = ModuleName "hydra.core.json.yaml.encode"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([JsonEncode.ns, YamlModel.ns] L.++ (YamlModel.ns : KernelTypes.kernelTypesModuleNames)),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "JSON-to-YAML encoding. Converts JSON Values to YAML Nodes (always succeeds), and Hydra Terms to YAML Nodes via JSON.")}
  where
    definitions = [
      toDefinition jsonToYaml,
      toDefinition toYaml]

-- | Convert a JSON Value to a YAML Node. This always succeeds since YAML is a superset of JSON.
jsonToYaml :: TypedTermDefinition (Value -> YM.Node)
jsonToYaml = define "jsonToYaml" $
  doc "Convert a JSON value to a YAML node. Always succeeds since YAML is a superset of JSON." $
  "value" ~>
  match _Value (var "value")
    Nothing [
    _Value_array>>: "arr" ~>
      Yaml.nodeSequence $ Lists.map ("v" ~> jsonToYaml @@ var "v") (var "arr"),

    _Value_boolean>>: "b" ~>
      Yaml.nodeScalar $ Yaml.scalarBool $ var "b",

    _Value_null>>: constant $
      Yaml.nodeScalar Yaml.scalarNull,

    _Value_number>>: "n" ~>
      Yaml.nodeScalar $ Yaml.scalarDecimal $ var "n",

    _Value_object>>: "obj" ~>
      Yaml.nodeMapping $ Maps.fromList $
        Lists.map
          ("kv" ~> pair
            (Yaml.nodeScalar $ Yaml.scalarStr $ Pairs.first $ var "kv")
            (jsonToYaml @@ (Pairs.second $ var "kv")))
          (var "obj"),

    _Value_string>>: "s" ~>
      Yaml.nodeScalar $ Yaml.scalarStr $ var "s"]

-- | Encode a Hydra Term to a YAML Node via JSON.
-- compactMaps = False: keeps YAML's array-of-entries map shape stable pending a deliberate
-- decision to opt this coder into #624's compact form (out of scope for #624 itself).
toYaml :: TypedTermDefinition (M.Map Name Type -> Name -> Type -> Term -> Either String YM.Node)
toYaml = define "toYaml" $
  doc "Encode a Hydra term to a YAML node via JSON encoding." $
  "types" ~> "tname" ~> "typ" ~> "term" ~>
  Eithers.map ("v" ~> jsonToYaml @@ var "v") (JsonEncode.toJson @@ var "types" @@ false @@ var "tname" @@ var "typ" @@ var "term")
