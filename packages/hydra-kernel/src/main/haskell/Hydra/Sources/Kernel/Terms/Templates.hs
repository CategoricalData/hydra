
module Hydra.Sources.Kernel.Terms.Templates where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (graphToSchema, instantiateTemplate)
import qualified Hydra.Core.Dsl.Paths    as Paths
import qualified Hydra.Core.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Core.Dsl.Ast          as Ast
import qualified Hydra.Core.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Core.Dsl.Coders       as Coders
import qualified Hydra.Core.Dsl.Util      as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core         as Core
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Graph        as Graph
import qualified Hydra.Core.Dsl.Json.Model         as Json
import qualified Hydra.Core.Dsl.Lib.Chars    as Chars
import qualified Hydra.Core.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Core.Dsl.Lib.Equality as Equality
import qualified Hydra.Core.Dsl.Lib.Lists    as Lists
import qualified Hydra.Core.Dsl.Lib.Literals as Literals
import qualified Hydra.Core.Dsl.Lib.Logic    as Logic
import qualified Hydra.Core.Dsl.Lib.Maps     as Maps
import qualified Hydra.Core.Dsl.Lib.Math     as Math
import qualified Hydra.Core.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Core.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Core.Dsl.Lib.Sets     as Sets
import qualified Hydra.Core.Dsl.Lib.Strings  as Strings
import qualified Hydra.Core.Overlay.Haskell.Dsl.Literals          as Literals
import qualified Hydra.Core.Overlay.Haskell.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Core.Overlay.Haskell.Dsl.Base         as MetaBase
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Core.Dsl.Packaging       as Packaging
import qualified Hydra.Core.Dsl.Parsing      as Parsing
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms     as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Prims             as Prims
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing      as Testing
import qualified Hydra.Core.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Tests             as Tests
import qualified Hydra.Core.Dsl.Topology     as Topology
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types             as Types
import qualified Hydra.Core.Dsl.Typing       as Typing
import qualified Hydra.Core.Dsl.Errors       as Error
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Constants as Constants
import qualified Hydra.Sources.Kernel.Terms.Print.Core as PrintCore


ns :: ModuleName
ns = ModuleName "hydra.core.templates"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Constants.ns, ModuleName "hydra.core.decode.model", PrintCore.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "A utility which instantiates a nonrecursive type with default values")}
  where
   definitions = [
     toDefinition graphToSchema,
     toDefinition instantiateTemplate]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

graphToSchema :: TypedTermDefinition (InferenceContext -> Graph -> [Binding] -> Either DecodingError (M.Map Name Type))
graphToSchema = define "graphToSchema" $
  doc "Decode a list of type-encoding bindings into a map of named types" $
  "cx" ~> "graph" ~> "els" ~>
  "toPair" <~ ("el" ~>
    "name" <~ Core.bindingName (var "el") $
    Eithers.bind (decoderFor _Type @@ var "graph" @@ (Core.bindingTerm (var "el"))) (
      "t" ~> right (pair (var "name") (var "t")))) $
  Eithers.bind (Eithers.mapList (var "toPair") (var "els")) (
    "pairs" ~> right (Maps.fromList (var "pairs") :: TypedTerm (M.Map Name Type)))

instantiateTemplate :: TypedTermDefinition (InferenceContext -> Bool -> M.Map Name Type -> Name -> Type -> Either Error Term)
instantiateTemplate = define "instantiateTemplate" $
  doc ("Given a graph schema and a nonrecursive type, instantiate it with default values."
    <> " If the minimal flag is set, the smallest possible term is produced; otherwise, exactly one subterm"
    <> " is produced for constructors which do not otherwise require one, e.g. in lists and optionals."
    <> " The name parameter provides the element name for nominal type construction.") $
  "cx" ~> "minimal" ~> "schema" ~> "tname" ~> "t" ~>
  "inst" <~ ("tn" ~> instantiateTemplate @@ var "cx" @@ var "minimal" @@ var "schema" @@ var "tn") $
  "noPoly" <~ left (Error.errorExtraction $ Error.extractionErrorUnexpectedShape $ Error.unexpectedShapeError (string "non-polymorphic type") (string "polymorphic or function type")) $
  "forFloat" <~ ("ft" ~> match _FloatType (var "ft")
    Nothing [
    _FloatType_float32>>: constant (Core.floatValueFloat32 (float32 0.0)),
    _FloatType_float64>>: constant (Core.floatValueFloat64 (float64 0.0))]) $
  "forInteger" <~ ("it" ~> match _IntegerType (var "it")
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
  "forLiteral" <~ ("lt" ~> match _LiteralType (var "lt")
    Nothing [
    _LiteralType_binary>>: constant (Core.literalString (string "")),
    _LiteralType_boolean>>: constant (Core.literalBoolean false),
    _LiteralType_decimal>>: constant (Core.literalDecimal (decimal 0)),
    _LiteralType_integer>>: "it" ~> Core.literalInteger (var "forInteger" @@ var "it"),
    _LiteralType_float>>: "ft" ~> Core.literalFloat (var "forFloat" @@ var "ft"),
    _LiteralType_string>>: constant (Core.literalString (string ""))]) $
  match _Type (var "t")
    Nothing [
    _Type_annotated>>: "at" ~> var "inst" @@ var "tname" @@ (Core.annotatedTypeBody (var "at")),
    _Type_application>>: constant (var "noPoly"),
    _Type_function>>: constant (var "noPoly"),
    _Type_forall>>: constant (var "noPoly"),
    _Type_list>>: "et" ~> Logic.ifElse (var "minimal")
      (right (Core.termList (list ([] :: [TypedTerm Term]))))
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
    _Type_optional>>: "ot" ~> Logic.ifElse (var "minimal")
      (right (Core.termOptional nothing))
      (Eithers.bind (var "inst" @@ var "tname" @@ var "ot") (
        "e" ~> right (Core.termOptional (just (var "e"))))),
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
      Optionals.match (Maps.lookup (var "vname" :: TypedTerm Name) (var "schema")) (left (Error.errorResolution $ Error.resolutionErrorUnexpectedShape $ Error.unexpectedShapeError (string "bound type variable") (Strings.concat2 (string "unbound variable ") (Core.unName (var "vname"))))) (var "inst" @@ var "vname"),
    _Type_wrap>>: "wt" ~>
      Eithers.bind (var "inst" @@ var "tname" @@ var "wt") (
        "e" ~> right (Core.termWrap (Core.wrappedTerm (var "tname") (var "e"))))]

