
module Hydra.Sources.Kernel.Terms.Templates where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (graphToSchema, instantiateTemplate)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Dsl.Meta.Context      as Ctx
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Meta.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Meta.Lib.Chars    as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps     as Maps
import qualified Hydra.Dsl.Meta.Lib.Math     as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import           Hydra.Dsl.Meta.Lib.Strings  as Strings
import qualified Hydra.Dsl.Literals          as Literals
import qualified Hydra.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Dsl.Meta.Base         as MetaBase
import qualified Hydra.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Errors       as Error
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Decode.Core as DecodeCore
import qualified Hydra.Sources.Kernel.Terms.Constants as Constants
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore


ns :: Namespace
ns = Namespace "hydra.templates"

module_ :: Module
module_ = Module ns elements
    [Constants.ns, moduleNamespace DecodeCore.module_, ShowCore.ns]
    kernelTypesNamespaces $
    Just "A utility which instantiates a nonrecursive type with default values"
  where
   elements = [
     toDefinition graphToSchema,
     toDefinition instantiateTemplate]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

graphToSchema :: TTermDefinition (Context -> Graph -> [Binding] -> Either (InContext DecodingError) (M.Map Name Type))
graphToSchema = define "graphToSchema" $
  doc "Decode a list of type-encoding bindings into a map of named types" $
  "cx" ~> "graph" ~> "els" ~>
  "toPair" <~ ("el" ~>
    "name" <~ Core.bindingName (var "el") $
    Eithers.bind (Ctx.withContext (var "cx") (decoderFor _Type @@ var "graph" @@ (Core.bindingTerm (var "el")))) (
      "t" ~> right (pair (var "name") (var "t")))) $
  Eithers.bind (Eithers.mapList (var "toPair") (var "els")) (
    "pairs" ~> right (Maps.fromList (var "pairs")))

instantiateTemplate :: TTermDefinition (Context -> Bool -> M.Map Name Type -> Name -> Type -> Either (InContext Error) Term)
instantiateTemplate = define "instantiateTemplate" $
  doc ("Given a graph schema and a nonrecursive type, instantiate it with default values."
    <> " If the minimal flag is set, the smallest possible term is produced; otherwise, exactly one subterm"
    <> " is produced for constructors which do not otherwise require one, e.g. in lists and optionals."
    <> " The name parameter provides the element name for nominal type construction.") $
  "cx" ~> "minimal" ~> "schema" ~> "tname" ~> "t" ~>
  "inst" <~ ("tn" ~> instantiateTemplate @@ var "cx" @@ var "minimal" @@ var "schema" @@ var "tn") $
  "noPoly" <~ Ctx.failInContext (Error.errorOther $ Error.otherError (string "Polymorphic and function types are not currently supported")) (var "cx") $
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
    _Type_annotated>>: "at" ~> var "inst" @@ var "tname" @@ (Core.annotatedTypeBody (var "at")),
    _Type_application>>: constant (var "noPoly"),
    _Type_function>>: constant (var "noPoly"),
    _Type_forall>>: constant (var "noPoly"),
    _Type_list>>: "et" ~> Logic.ifElse (var "minimal")
      (right (Core.termList (list ([] :: [TTerm Term]))))
      (Eithers.bind (var "inst" @@ var "tname" @@ var "et") (
        "e" ~> right (Core.termList (list [var "e"])))),
    _Type_literal>>: "lt" ~> right (Core.termLiteral (var "forLiteral" @@ var "lt")),
    _Type_map>>: "mt" ~>
      "kt" <~ Core.mapTypeKeys (var "mt") $
      "vt" <~ Core.mapTypeValues (var "mt") $
      Logic.ifElse (var "minimal")
        (right (Core.termMap Maps.empty))
        (Eithers.bind (var "inst" @@ var "tname" @@ var "kt") (
          "ke" ~>
          Eithers.bind (var "inst" @@ var "tname" @@ var "vt") (
            "ve" ~> right (Core.termMap (Maps.singleton (var "ke") (var "ve")))))),
    _Type_maybe>>: "ot" ~> Logic.ifElse (var "minimal")
      (right (Core.termMaybe nothing))
      (Eithers.bind (var "inst" @@ var "tname" @@ var "ot") (
        "e" ~> right (Core.termMaybe (just (var "e"))))),
    _Type_record>>: "rt" ~>
      "toField" <~ ("ft" ~>
        Eithers.bind (var "inst" @@ var "tname" @@ (Core.fieldTypeType (var "ft"))) (
          "e" ~> right (Core.field (Core.fieldTypeName (var "ft")) (var "e")))) $
      Eithers.bind (Eithers.mapList (var "toField") (var "rt")) (
        "dfields" ~> right (Core.termRecord (Core.record (var "tname") (var "dfields")))),
    _Type_set>>: "et" ~> Logic.ifElse (var "minimal")
      (right (Core.termSet Sets.empty))
      (Eithers.bind (var "inst" @@ var "tname" @@ var "et") (
        "e" ~> right (Core.termSet (Sets.fromList (list [var "e"]))))),
    _Type_variable>>: "vname" ~>
      Maybes.maybe
        (Ctx.failInContext (Error.errorOther $ Error.otherError (Strings.cat2 (string "Type variable ") (Strings.cat2 (ShowCore.term @@ (Core.termVariable (var "vname"))) (string " not found in schema")))) (var "cx"))
        (var "inst" @@ var "vname")
        (Maps.lookup (var "vname") (var "schema")),
    _Type_wrap>>: "wt" ~>
      Eithers.bind (var "inst" @@ var "tname" @@ var "wt") (
        "e" ~> right (Core.termWrap (Core.wrappedTerm (var "tname") (var "e"))))]

