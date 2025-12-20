
module Hydra.Sources.Kernel.Terms.Templates where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (graphToSchema, instantiateTemplate)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Meta.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Hydra.Dsl.Meta.Compute       as Compute
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Json          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows     as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import           Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Meta.Module        as Module
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Decode.Core as DecodeCore
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore


ns :: Namespace
ns = Namespace "hydra.templates"

module_ :: Module
module_ = Module ns elements
    [DecodeCore.ns, ShowCore.ns]
    kernelTypesNamespaces $
    Just "A utility which instantiates a nonrecursive type with default values"
  where
   elements = [
     toBinding graphToSchema,
     toBinding instantiateTemplate]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

graphToSchema :: TBinding (Graph -> Flow Graph (M.Map Name Type))
graphToSchema = define "graphToSchema" $
  doc "Create a graph schema from a graph which contains nothing but encoded type definitions" $
  "g" ~>
  "toPair" <~ ("nameAndEl" ~>
    "name" <~ Pairs.first (var "nameAndEl") $
    "el" <~ Pairs.second (var "nameAndEl") $
    Flows.bind (trace (string "graph to schema") $ DecodeCore.type_ @@ (Core.bindingTerm (var "el"))) (
      "t" ~> Flows.pure (pair (var "name") (var "t")))) $
  Flows.bind (Flows.mapList (var "toPair") (Maps.toList (Graph.graphElements (var "g")))) (
    "pairs" ~> Flows.pure (Maps.fromList (var "pairs")))

instantiateTemplate :: TBinding (Bool -> M.Map Name Type -> Type -> Flow s Term)
instantiateTemplate = define "instantiateTemplate" $
  doc ("Given a graph schema and a nonrecursive type, instantiate it with default values."
    <> " If the minimal flag is set, the smallest possible term is produced; otherwise, exactly one subterm"
    <> " is produced for constructors which do not otherwise require one, e.g. in lists and optionals") $
  "minimal" ~> "schema" ~> "t" ~>
  "inst" <~ instantiateTemplate @@ var "minimal" @@ var "schema" $
  "noPoly" <~ Flows.fail (string "Polymorphic and function types are not currently supported") $
  "forFloat" <~ ("ft" ~> cases _FloatType (var "ft")
    Nothing [
    _FloatType_bigfloat>>: constant (Core.floatValueBigfloat (bigfloat 0.0)),
    _FloatType_float32>>: constant (Core.floatValueFloat32 (float32 0.0)),
    _FloatType_float64>>: constant (Core.floatValueFloat64 (float64 0.0))]) $
  "forInteger" <~ ("it" ~> cases _IntegerType (var "it")
    Nothing [
    _IntegerType_bigint>>: constant (Core.integerValueBigint (bigint 0)),
    _IntegerType_int8>>: constant (Core.integerValueInt8 (int8 0)),
    _IntegerType_int16>>: constant (Core.integerValueInt16 (int16 0)),
    _IntegerType_int32>>: constant (Core.integerValueInt32 (int32 0)),
    _IntegerType_int64>>: constant (Core.integerValueInt64 (int64 0)),
    _IntegerType_uint8>>: constant (Core.integerValueUint8 (uint8 0)),
    _IntegerType_uint16>>: constant (Core.integerValueUint16 (uint16 0)),
    _IntegerType_uint32>>: constant (Core.integerValueUint32 (uint32 0)),
    _IntegerType_uint64>>: constant (Core.integerValueUint64 (uint64 0))]) $
  "forLiteral" <~ ("lt" ~> cases _LiteralType (var "lt")
    Nothing [
    _LiteralType_binary>>: constant (Core.literalString (string "")),
    _LiteralType_boolean>>: constant (Core.literalBoolean false),
    _LiteralType_integer>>: "it" ~> Core.literalInteger (var "forInteger" @@ var "it"),
    _LiteralType_float>>: "ft" ~> Core.literalFloat (var "forFloat" @@ var "ft"),
    _LiteralType_string>>: constant (Core.literalString (string ""))]) $
  cases _Type (var "t")
    Nothing [
    _Type_annotated>>: "at" ~> var "inst" @@ (Core.annotatedTypeBody (var "at")),
    _Type_application>>: constant (var "noPoly"),
    _Type_function>>: constant (var "noPoly"),
    _Type_forall>>: constant (var "noPoly"),
    _Type_list>>: "et" ~> Logic.ifElse (var "minimal")
      (Flows.pure (Core.termList (list ([] :: [TTerm Term]))))
      (Flows.bind (var "inst" @@ var "et") (
        "e" ~> Flows.pure (Core.termList (list [var "e"])))),
    _Type_literal>>: "lt" ~> Flows.pure (Core.termLiteral (var "forLiteral" @@ var "lt")),
    _Type_map>>: "mt" ~>
      "kt" <~ Core.mapTypeKeys (var "mt") $
      "vt" <~ Core.mapTypeValues (var "mt") $
      Logic.ifElse (var "minimal")
        (Flows.pure (Core.termMap Maps.empty))
        (Flows.bind (var "inst" @@ var "kt") (
          "ke" ~>
          Flows.bind (var "inst" @@ var "vt") (
            "ve" ~> Flows.pure (Core.termMap (Maps.singleton (var "ke") (var "ve")))))),
    _Type_maybe>>: "ot" ~> Logic.ifElse (var "minimal")
      (Flows.pure (Core.termMaybe nothing))
      (Flows.bind (var "inst" @@ var "ot") (
        "e" ~> Flows.pure (Core.termMaybe (just (var "e"))))),
    _Type_record>>: "rt" ~>
      "tname" <~ Core.rowTypeTypeName (var "rt") $
      "fields" <~ Core.rowTypeFields (var "rt") $
      "toField" <~ ("ft" ~>
        Flows.bind (var "inst" @@ (Core.fieldTypeType (var "ft"))) (
          "e" ~> Flows.pure (Core.field (Core.fieldTypeName (var "ft")) (var "e")))) $
      Flows.bind (Flows.mapList (var "toField") (var "fields")) (
        "dfields" ~> Flows.pure (Core.termRecord (Core.record (var "tname") (var "dfields")))),
    _Type_set>>: "et" ~> Logic.ifElse (var "minimal")
      (Flows.pure (Core.termSet Sets.empty))
      (Flows.bind (var "inst" @@ var "et") (
        "e" ~> Flows.pure (Core.termSet (Sets.fromList (list [var "e"]))))),
    _Type_variable>>: "tname" ~>
      Maybes.maybe
        (Flows.fail (Strings.cat2 (string "Type variable ") (Strings.cat2 (ShowCore.term @@ (Core.termVariable (var "tname"))) (string " not found in schema"))))
        (var "inst")
        (Maps.lookup (var "tname") (var "schema")),
    _Type_wrap>>: "wt" ~>
      "tname" <~ Core.wrappedTypeTypeName (var "wt") $
      "t'" <~ Core.wrappedTypeBody (var "wt") $
      Flows.bind (var "inst" @@ var "t'") (
        "e" ~> Flows.pure (Core.termWrap (Core.wrappedTerm (var "tname") (var "e"))))]

{-

-- Example of type-to-term instantiation which creates a YAML-based template out of the OpenCypher feature model.

import Hydra.Staging.Yaml.Model as Yaml
import Hydra.Monads
import Data.Map as M
import Data.Maybe as Y

ff = flowToIo bootstrapGraph

schema <- ff $ graphToSchema $ modulesToGraph [openCypherFeaturesModule]

typ <- ff $ inlineType schema $ Y.fromJust $ M.lookup _CypherFeatures schema
term <- ff $ insantiateTemplate False schema typ

encoder <- ff (coderEncode <$> yamlCoder typ)
yaml <- ff $ encoder term
putStrLn $ hydraYamlToString yaml

-}
