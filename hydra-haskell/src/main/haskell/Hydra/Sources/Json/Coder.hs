
module Hydra.Sources.Json.Coder where

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
import qualified Hydra.Dsl.Meta.Accessors                  as Accessors
import qualified Hydra.Dsl.Meta.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Meta.Coders                     as Coders
import qualified Hydra.Dsl.Meta.Compute                    as Compute
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Grammar                    as Grammar
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Meta.Json                       as Json
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
import qualified Hydra.Dsl.Meta.Module                     as Module
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Meta.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Meta.Typing                     as Typing
import qualified Hydra.Dsl.Meta.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt.Literals as AdaptLiterals
import qualified Hydra.Sources.Kernel.Terms.Adapt.Modules  as AdaptModules
import qualified Hydra.Sources.Kernel.Terms.Adapt.Simple   as AdaptSimple
import qualified Hydra.Sources.Kernel.Terms.Adapt.Terms    as AdaptTerms
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils    as AdaptUtils
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
import qualified Hydra.Sources.Kernel.Terms.Monads         as Monads
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
import qualified Hydra.Sources.Kernel.Terms.Tarjan         as Tarjan
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
import qualified Hydra.Dsl.Meta.Context      as Ctx
import qualified Hydra.Dsl.Meta.Error        as Error
import qualified Hydra.Sources.Json.Language as JsonLanguage
import qualified Hydra.Sources.Kernel.Terms.Literals as HydraLiterals

formatOtherError :: TTerm (InContext OtherError -> String)
formatOtherError = "ic" ~> Error.unOtherError @@ Ctx.inContextObject (var "ic")

-- | Lift Either String to Either (InContext OtherError) using a context
liftStringError :: TTerm Context -> TTerm (Either String a) -> TTerm (Either (InContext OtherError) a)
liftStringError cx = Eithers.bimap ("_s" ~> Ctx.inContext (Error.otherError $ var "_s") cx) ("_x" ~> var "_x")

ns :: Namespace
ns = Namespace "hydra.ext.org.json.coder"

define :: String -> TTerm a -> TBinding a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
    [AdaptModules.ns, AdaptTerms.ns, AdaptUtils.ns, moduleNamespace EncodeCore.module_,
     ExtractCore.ns, HydraLiterals.ns, JsonLanguage.ns, Monads.ns, Rewriting.ns]
    KernelTypes.kernelTypesNamespaces $
    Just "JSON encoding and decoding for Hydra terms"
  where
    elements = [
      toBinding jsonCoder,
      toBinding literalJsonCoder,
      toBinding recordCoder,
      toBinding encodeRecord,
      toBinding decodeRecord,
      toBinding termCoder,
      toBinding unitCoder,
      toBinding untypedTermToJson,
      toBinding readStringStub,
      toBinding showValue]

jsonCoder :: TBinding (Type -> Context -> Graph -> Either (InContext OtherError) (Coder Term Value))
jsonCoder = define "jsonCoder" $
  doc "Create a JSON coder for a given type" $
  "typ" ~> "cx" ~> "g" ~>
  "mkTermCoder" <~ ("t" ~> termCoder @@ var "t" @@ var "cx" @@ var "g") $
  "adapter" <<~ liftStringError (var "cx") (AdaptModules.languageAdapter @@ JsonLanguage.jsonLanguage @@ var "cx" @@ var "g" @@ var "typ") $
  "coder" <<~ var "mkTermCoder" @@ (Compute.adapterTarget $ var "adapter") $
  right $ AdaptUtils.composeCoders @@ (Compute.adapterCoder $ var "adapter") @@ var "coder"

literalJsonCoder :: TBinding (LiteralType -> Either (InContext OtherError) (Coder Literal Value))
literalJsonCoder = define "literalJsonCoder" $
  doc "Create a JSON coder for literal types" $
  "lt" ~>
  -- Define decoder functions that contain match expressions
  "decodeBool" <~ ("cx" ~> "s" ~>
    cases _Value (var "s")
      (Just $ Ctx.failInContext (Error.otherError (Strings.cat $ list [string "expected boolean, found: ", showValue @@ var "s"])) (var "cx")) [
      _Value_boolean>>: "b" ~> right (Core.literalBoolean $ var "b")]) $
  "decodeFloat" <~ ("cx" ~> "s" ~>
    cases _Value (var "s")
      (Just $ Ctx.failInContext (Error.otherError (Strings.cat $ list [string "expected number, found: ", showValue @@ var "s"])) (var "cx")) [
      _Value_number>>: "f" ~> right (Core.literalFloat $ Core.floatValueBigfloat $ var "f")]) $
  "decodeInteger" <~ ("cx" ~> "s" ~>
    cases _Value (var "s")
      (Just $ Ctx.failInContext (Error.otherError (Strings.cat $ list [string "expected number, found: ", showValue @@ var "s"])) (var "cx")) [
      _Value_number>>: "f" ~>
        "bi" <~ (Literals.bigfloatToBigint $ var "f") $
        right (Core.literalInteger $ Core.integerValueBigint $ var "bi")]) $
  "decodeString" <~ ("cx" ~> "s" ~>
    cases _Value (var "s")
      (Just $ Ctx.failInContext (Error.otherError (Strings.cat $ list [string "expected string, found: ", showValue @@ var "s"])) (var "cx")) [
      _Value_string>>: "s'" ~> right (Core.literalString $ var "s'")]) $
  "encoded" <~ (cases _LiteralType (var "lt") Nothing [
    _LiteralType_boolean>>: constant $ Compute.coder
      ("cx" ~> "lit" ~>
        "b" <<~ ExtractCore.booleanLiteral @@ var "cx" @@ var "lit" $
        right (Json.valueBoolean $ var "b"))
      (var "decodeBool"),
    _LiteralType_float>>: constant $ Compute.coder
      ("cx" ~> "lit" ~>
        "f" <<~ ExtractCore.floatLiteral @@ var "cx" @@ var "lit" $
        "bf" <<~ ExtractCore.bigfloatValue @@ var "cx" @@ var "f" $
        right (Json.valueNumber $ var "bf"))
      (var "decodeFloat"),
    _LiteralType_integer>>: constant $ Compute.coder
      ("cx" ~> "lit" ~>
        "i" <<~ ExtractCore.integerLiteral @@ var "cx" @@ var "lit" $
        "bi" <<~ ExtractCore.bigintValue @@ var "cx" @@ var "i" $
        right (Json.valueNumber $ Literals.bigintToBigfloat $ var "bi"))
      (var "decodeInteger"),
    _LiteralType_string>>: constant $ Compute.coder
      ("cx" ~> "lit" ~>
        "s" <<~ ExtractCore.stringLiteral @@ var "cx" @@ var "lit" $
        right (Json.valueString $ var "s"))
      (var "decodeString")]) $
  right $ var "encoded"

recordCoder :: TBinding (RowType -> Context -> Graph -> Either (InContext OtherError) (Coder Term Value))
recordCoder = define "recordCoder" $
  doc "Create a JSON coder for record types" $
  "rt" ~> "cx" ~> "g" ~>
  "fields" <~ (Core.rowTypeFields $ var "rt") $
  "getCoder" <~ ("f" ~>
    "coder" <<~ termCoder @@ (Core.fieldTypeType $ var "f") @@ var "cx" @@ var "g" $
    right $ pair (var "f") (var "coder")) $
  "coders" <<~ Eithers.mapList (var "getCoder") (var "fields") $
  right $ Compute.coder
    ("cx" ~> "term" ~> encodeRecord @@ var "coders" @@ var "cx" @@ var "g" @@ var "term")
    ("cx" ~> "val" ~> decodeRecord @@ var "rt" @@ var "coders" @@ var "cx" @@ var "val")

encodeRecord :: TBinding ([(FieldType, Coder Term Value)] -> Context -> Graph -> Term -> Either (InContext OtherError) Value)
encodeRecord = define "encodeRecord" $
  doc "Encode a record term to JSON" $
  "coders" ~> "cx" ~> "graph" ~> "term" ~>
  "stripped" <~ (Rewriting.deannotateTerm @@ var "term") $
  -- Lift case expressions out of the encodeField lambda
  "matchMaybeTerm" <~ ("fvalue" ~> "coder'" ~> "fname" ~> "dflt" ~>
    cases _Term (var "fvalue")
      (Just $ var "dflt") [
      _Term_maybe>>: "opt" ~> optCases (var "opt")
        (right nothing)
        ("v" ~>
          "encoded" <<~ Compute.coderEncode (var "coder'") @@ var "cx" @@ var "v" $
          right (just $ pair (Core.unName $ var "fname") (var "encoded")))]) $
  "matchTypeForMaybe" <~ ("ft" ~> "forMaybe" ~> "dflt" ~>
    cases _Type (Core.fieldTypeType $ var "ft")
      (Just $ var "dflt") [
      _Type_maybe>>: "ot" ~> var "forMaybe" @@ var "ot"]) $
  "encodeField" <~ ("coderAndField" ~>
    "coder" <~ (Pairs.first $ var "coderAndField") $
    "field" <~ (Pairs.second $ var "coderAndField") $
    "ft" <~ (Pairs.first $ var "coder") $
    "coder'" <~ (Pairs.second $ var "coder") $
    "fname" <~ (Core.fieldName $ var "field") $
    "fvalue" <~ (Core.fieldTerm $ var "field") $
    "forMaybe" <~ ("ot" ~>
      "dflt" <~ (
        "encoded" <<~ Compute.coderEncode (var "coder'") @@ var "cx" @@ var "fvalue" $
        right (just $ pair (Core.unName $ var "fname") (var "encoded"))) $
      var "matchMaybeTerm" @@ var "fvalue" @@ var "coder'" @@ var "fname" @@ var "dflt") $
    "dflt" <~ (
      "encoded" <<~ Compute.coderEncode (var "coder'") @@ var "cx" @@ var "fvalue" $
      right (just $ pair (Core.unName $ var "fname") (var "encoded"))) $
    var "matchTypeForMaybe" @@ var "ft" @@ var "forMaybe" @@ var "dflt") $
  "record" <<~ ExtractCore.termRecord @@ var "cx" @@ var "graph" @@ var "stripped" $
  "fields" <~ (Core.recordFields $ var "record") $
  "maybeFields" <<~ Eithers.mapList (var "encodeField") (Lists.zip (var "coders") (var "fields")) $
  right (Json.valueObject $ Maps.fromList $ Maybes.cat $ var "maybeFields")

decodeRecord :: TBinding (RowType -> [(FieldType, Coder Term Value)] -> Context -> Value -> Either (InContext OtherError) Term)
decodeRecord = define "decodeRecord" $
  doc "Decode a JSON value to a record term" $
  "rt" ~> "coders" ~> "cx" ~> "n" ~>
  -- Lift the object decoder body into a separate function to avoid lambdas with let-bindings inside cases
  "decodeObjectBody" <~ ("m" ~>
    "decodeField" <~ ("coder" ~>
      "ft" <~ (Pairs.first $ var "coder") $
      "coder'" <~ (Pairs.second $ var "coder") $
      "fname" <~ (Core.fieldTypeName $ var "ft") $
      "defaultValue" <~ Json.valueNull $
      "jsonValue" <~ (Maybes.fromMaybe (var "defaultValue") $ Maps.lookup (Core.unName $ var "fname") (var "m")) $
      "v" <<~ Compute.coderDecode (var "coder'") @@ var "cx" @@ var "jsonValue" $
      right (Core.field (var "fname") (var "v"))) $
    "fields" <<~ Eithers.mapList (var "decodeField") (var "coders") $
    right (Core.termRecord $ Core.record (Core.rowTypeTypeName $ var "rt") (var "fields"))) $
  cases _Value (var "n")
    (Just $ Ctx.failInContext (Error.otherError (Strings.cat $ list [string "expected object, found: ", showValue @@ var "n"])) (var "cx")) [
    _Value_object>>: var "decodeObjectBody"]

termCoder :: TBinding (Type -> Context -> Graph -> Either (InContext OtherError) (Coder Term Value))
termCoder = define "termCoder" $
  doc "Create a JSON coder for term types" $
  "typ" ~> "cx" ~> "g" ~>
  "stripped" <~ (Rewriting.deannotateType @@ var "typ") $
  -- Lift encoder/decoder functions that contain match expressions
  "encodeLiteral" <~ ("ac" ~> "cx" ~> "term" ~>
    cases _Term (var "term")
      (Just $ Ctx.failInContext (Error.otherError (Strings.cat $ list [string "expected literal term, found: ", ShowCore.term @@ var "term"])) (var "cx")) [
      _Term_literal>>: "av" ~> Compute.coderEncode (var "ac") @@ var "cx" @@ var "av"]) $
  "encodeList" <~ ("lc" ~> "cx" ~> "term" ~>
    cases _Term (var "term")
      (Just $ Ctx.failInContext (Error.otherError (Strings.cat $ list [string "expected list term, found: ", ShowCore.term @@ var "term"])) (var "cx")) [
      _Term_list>>: "els" ~>
        "encodedEls" <<~ Eithers.mapList ("el" ~> Compute.coderEncode (var "lc") @@ var "cx" @@ var "el") (var "els") $
        right (Json.valueArray $ var "encodedEls")]) $
  "decodeList" <~ ("lc" ~> "cx" ~> "n" ~>
    cases _Value (var "n")
      (Just $ Ctx.failInContext (Error.otherError (Strings.cat $ list [string "expected sequence, found: ", showValue @@ var "n"])) (var "cx")) [
      _Value_array>>: "nodes" ~>
        "decodedNodes" <<~ Eithers.mapList ("node" ~> Compute.coderDecode (var "lc") @@ var "cx" @@ var "node") (var "nodes") $
        right (Core.termList $ var "decodedNodes")]) $
  "matchLiteralString" <~ ("v" ~> "lit" ~>
    cases _Literal (var "lit")
      (Just $ ShowCore.term @@ var "v") [
      _Literal_string>>: "s" ~> var "s"]) $
  "matchTermLiteral" <~ ("v" ~>
    cases _Term (Rewriting.deannotateTerm @@ var "v")
      (Just $ ShowCore.term @@ var "v") [
      _Term_literal>>: "lit" ~> var "matchLiteralString" @@ var "v" @@ var "lit"]) $
  "encodeMaybe" <~ ("maybeElementCoder" ~> "cx" ~> "maybeTerm" ~>
    "strippedMaybeTerm" <~ (Rewriting.deannotateTerm @@ var "maybeTerm") $
    cases _Term (var "strippedMaybeTerm")
      (Just $ Ctx.failInContext (Error.otherError (Strings.cat $ list [string "expected optional term, found: ", ShowCore.term @@ var "maybeTerm"])) (var "cx")) [
      _Term_maybe>>: "maybeContents" ~>
        Logic.ifElse (Maybes.isNothing $ var "maybeContents")
          (right Json.valueNull)
          ("encodedInner" <<~ Compute.coderEncode (var "maybeElementCoder") @@ var "cx" @@ (Maybes.fromJust $ var "maybeContents") $
            right (var "encodedInner"))]) $
  "decodeMaybe" <~ ("maybeElementCoder" ~> "cx" ~> "jsonVal" ~>
    cases _Value (var "jsonVal")
      (Just $
        "decodedInner" <<~ Compute.coderDecode (var "maybeElementCoder") @@ var "cx" @@ var "jsonVal" $
        right (Core.termMaybe $ just $ var "decodedInner")) [
      _Value_null>>: constant $ right (Core.termMaybe nothing)]) $
  "result" <~ (cases _Type (var "stripped")
    (Just $ Ctx.failInContext (Error.otherError (Strings.cat $ list [
      string "unsupported type in JSON: ",
      ShowCore.type_ @@ var "typ"])) (var "cx")) [
    _Type_literal>>: "at" ~>
      "ac" <<~ literalJsonCoder @@ var "at" $
      right $ Compute.coder
        (var "encodeLiteral" @@ var "ac")
        ("cx" ~> "n" ~>
          "lit" <<~ Compute.coderDecode (var "ac") @@ var "cx" @@ var "n" $
          right (Core.termLiteral $ var "lit")),
    _Type_list>>: "lt" ~>
      "lc" <<~ termCoder @@ var "lt" @@ var "cx" @@ var "g" $
      right $ Compute.coder
        (var "encodeList" @@ var "lc")
        (var "decodeList" @@ var "lc"),
    _Type_map>>: "mt" ~>
      "kt" <~ (Core.mapTypeKeys $ var "mt") $
      "vt" <~ (Core.mapTypeValues $ var "mt") $
      "kc" <<~ termCoder @@ var "kt" @@ var "cx" @@ var "g" $
      "vc" <<~ termCoder @@ var "vt" @@ var "cx" @@ var "g" $
      "isStringKey" <~ Equality.equal (Rewriting.deannotateType @@ var "kt") (Core.typeLiteral Core.literalTypeString) $
      "toString" <~ ("v" ~>
        Logic.ifElse (var "isStringKey")
          (var "matchTermLiteral" @@ var "v")
          (ShowCore.term @@ var "v")) $
      "fromString" <~ ("s" ~> Logic.ifElse (var "isStringKey")
        (Core.termLiteral $ Core.literalString $ var "s")
        (readStringStub @@ var "s")) $
      "encodeEntry" <~ ("cx" ~> "kv" ~>
        "k" <~ (Pairs.first $ var "kv") $
        "v" <~ (Pairs.second $ var "kv") $
        "encodedV" <<~ Compute.coderEncode (var "vc") @@ var "cx" @@ var "v" $
        right (pair (var "toString" @@ var "k") (var "encodedV"))) $
      "decodeEntry" <~ ("cx" ~> "kv" ~>
        "k" <~ (Pairs.first $ var "kv") $
        "v" <~ (Pairs.second $ var "kv") $
        "decodedV" <<~ Compute.coderDecode (var "vc") @@ var "cx" @@ var "v" $
        right (pair (var "fromString" @@ var "k") (var "decodedV"))) $
      right $ Compute.coder
        ("cx" ~> "term" ~>
          cases _Term (var "term")
            (Just $ Ctx.failInContext (Error.otherError (Strings.cat $ list [string "expected map term, found: ", ShowCore.term @@ var "term"])) (var "cx")) [
            _Term_map>>: "m" ~>
              "entries" <<~ Eithers.mapList ("entry" ~> var "encodeEntry" @@ var "cx" @@ var "entry") (Maps.toList $ var "m") $
              right (Json.valueObject $ Maps.fromList $ var "entries")])
        ("cx" ~> "n" ~>
          cases _Value (var "n")
            (Just $ Ctx.failInContext (Error.otherError (Strings.cat $ list [string "expected mapping, found: ", showValue @@ var "n"])) (var "cx")) [
            _Value_object>>: "m" ~>
              "entries" <<~ Eithers.mapList ("entry" ~> var "decodeEntry" @@ var "cx" @@ var "entry") (Maps.toList $ var "m") $
              right (Core.termMap $ Maps.fromList $ var "entries")]),
    _Type_maybe>>: "maybeElementType" ~>
      "maybeElementCoder" <<~ termCoder @@ var "maybeElementType" @@ var "cx" @@ var "g" $
      right $ Compute.coder
        (var "encodeMaybe" @@ var "maybeElementCoder")
        (var "decodeMaybe" @@ var "maybeElementCoder"),
    _Type_record>>: "rt" ~> recordCoder @@ var "rt" @@ var "cx" @@ var "g",
    _Type_unit>>: constant $ right $ (var "hydra.ext.org.json.coder.unitCoder" :: TTerm (Coder Term Value)),
    _Type_variable>>: "name" ~> right $ Compute.coder
      ("_cx" ~> "term" ~> right (Json.valueString $ Strings.cat $ list [
        string "variable '",
        Core.unName $ var "name",
        string "' for: ",
        ShowCore.term @@ var "term"]))
      ("cx" ~> "_term" ~> Ctx.failInContext (Error.otherError (Strings.cat $ list [
        string "type variable ",
        Core.unName $ var "name",
        string " does not support decoding"])) (var "cx"))]) $
  var "result"

unitCoder :: TBinding (Coder Term Value)
unitCoder = define "unitCoder" $
  doc "JSON coder for unit values" $
  -- Lift encoder/decoder functions that contain match expressions
  "encodeUnit" <~ ("cx" ~> "term" ~>
    cases _Term (Rewriting.deannotateTerm @@ var "term")
      (Just $ Ctx.failInContext (Error.otherError (Strings.cat $ list [string "expected unit, found: ", ShowCore.term @@ var "term"])) (var "cx")) [
      _Term_unit>>: constant $ right Json.valueNull]) $
  "decodeUnit" <~ ("cx" ~> "n" ~>
    cases _Value (var "n")
      (Just $ Ctx.failInContext (Error.otherError (Strings.cat $ list [string "expected null, found: ", showValue @@ var "n"])) (var "cx")) [
      _Value_null>>: constant $ right Core.termUnit]) $
  Compute.coder (var "encodeUnit") (var "decodeUnit")

untypedTermToJson :: TBinding (Term -> Either (InContext OtherError) Value)
untypedTermToJson = define "untypedTermToJson" $
  doc "A simplistic, unidirectional encoding for terms as JSON values. Not type-aware; best used for human consumption." $
  "term" ~>
  "recurse" <~ ("t" ~> untypedTermToJson @@ var "t") $
  "unexp" <~ ("msg" ~> right $ Json.valueString $ Strings.cat2 (string "FAIL: ") (var "msg")) $
  "asRecord" <~ ("fields" ~> var "recurse" @@ (Core.termRecord $ Core.record (Core.name $ string "") (var "fields"))) $
  "asVariant" <~ ("name" ~> "term" ~> var "recurse" @@
    (Core.termUnion $ Core.injection (Core.name $ string "") $ Core.field (Core.name $ var "name") (var "term"))) $
  -- Lift case expressions out of nested lambdas
  "matchTermMaybe" <~ ("forTerm" ~> "t" ~>
    cases _Term (var "t")
      (Just $ Eithers.map (unaryFunction just) $ var "recurse" @@ var "t") [
      _Term_maybe>>: "mt" ~> Maybes.maybe
        (right nothing)
        (var "forTerm")
        (var "mt")]) $
  "matchElimination" <~ ("unexp" ~> "asVariant" ~> "elm" ~>
    cases _Elimination (var "elm")
      (Just $ var "unexp" @@ (Strings.cat $ list [
        string "unexpected elimination variant: ",
        ShowCore.elimination @@ var "elm"])) [
      _Elimination_record>>: "proj" ~> var "asVariant" @@ string "project" @@
        (Core.termVariable $ Core.projectionField $ var "proj")]) $
  "matchFunction" <~ ("unexp" ~> "asRecord" ~> "asVariant" ~> "f" ~>
    cases _Function (var "f") Nothing [
      _Function_elimination>>: "elm" ~>
        var "matchElimination" @@ var "unexp" @@ var "asVariant" @@ var "elm",
      _Function_lambda>>: "l" ~> var "asRecord" @@ list [
        Core.field (Core.name $ string "parameter") (Core.termVariable $ Core.lambdaParameter $ var "l"),
        Core.field (Core.name $ string "domain") (Core.termMaybe $
          Maybes.map (encoderFor _Type) (Core.lambdaDomain $ var "l")),
        Core.field (Core.name $ string "body") (Core.lambdaBody $ var "l")],
      _Function_primitive>>: "name" ~> right $ Json.valueString $ Core.unName $ var "name"]) $
  "matchLiteral" <~ ("lit" ~>
    cases _Literal (var "lit") Nothing [
      _Literal_binary>>: "b" ~> Json.valueString $ Literals.binaryToString $ var "b",
      _Literal_boolean>>: "b" ~> Json.valueBoolean $ var "b",
      _Literal_float>>: "f" ~> Json.valueNumber $ HydraLiterals.floatValueToBigfloat @@ var "f",
      _Literal_integer>>: "i" ~>
        "bf" <~ (HydraLiterals.integerValueToBigint @@ var "i") $
        "f" <~ (Literals.bigintToBigfloat $ var "bf") $
        Json.valueNumber $ var "f",
      _Literal_string>>: "s" ~> Json.valueString $ var "s"]) $
  "fieldToKeyval" <~ ("f" ~>
    "forTerm" <~ ("t" ~> var "matchTermMaybe" @@ var "forTerm" @@ var "t") $
    "mjson" <<~ var "forTerm" @@ (Core.fieldTerm $ var "f") $
    right $ Maybes.map
      ("j" ~> pair (Core.unName $ Core.fieldName $ var "f") (var "j"))
      (var "mjson")) $
  "result" <~ (cases _Term (var "term")
    (Just $ var "unexp" @@ (Strings.cat $ list [
      string "unsupported term variant: ",
      ShowCore.term @@ var "term"])) [
    _Term_annotated>>: "at" ~>
      "term1" <~ (Core.annotatedTermBody $ var "at") $
      "ann" <~ (Core.annotatedTermAnnotation $ var "at") $
      "encodePair" <~ ("kv" ~>
        "k" <~ (Core.unName $ Pairs.first $ var "kv") $
        "v" <~ (Pairs.second $ var "kv") $
        "json" <<~ var "recurse" @@ var "v" $
        right $ pair (var "k") (var "json")) $
      "json" <<~ var "recurse" @@ var "term1" $
      "pairs" <<~ Eithers.mapList (var "encodePair") (Maps.toList $ var "ann") $
      right $ Json.valueObject $ Maps.fromList $ list [
        pair (string "term") (var "json"),
        pair (string "annotations") (Json.valueObject $ Maps.fromList $ var "pairs")],
    _Term_application>>: "app" ~> var "asRecord" @@ list [
      Core.field (Core.name $ string "function") (Core.applicationFunction $ var "app"),
      Core.field (Core.name $ string "argument") (Core.applicationArgument $ var "app")],
    _Term_function>>: "f" ~>
      var "matchFunction" @@ var "unexp" @@ var "asRecord" @@ var "asVariant" @@ var "f",
    _Term_let>>: "lt" ~>
      "bindings" <~ (Core.letBindings $ var "lt") $
      "env" <~ (Core.letBody $ var "lt") $
      "fromBinding" <~ ("b" ~> Core.field
        (Core.bindingName $ var "b")
        (Core.bindingTerm $ var "b")) $
      var "asRecord" @@ list [
        Core.field (Core.name $ string "bindings") (Core.termRecord $ Core.record
          (Core.name $ string "")
          (Lists.map (var "fromBinding") (var "bindings"))),
        Core.field (Core.name $ string "environment") (var "env")],
    _Term_list>>: "terms" ~>
      "jsonTerms" <<~ Eithers.mapList (var "recurse") (var "terms") $
      right $ Json.valueArray $ var "jsonTerms",
    _Term_literal>>: "lit" ~>
      right $ var "matchLiteral" @@ var "lit",
    _Term_maybe>>: "mt" ~> Maybes.maybe
      (right Json.valueNull)
      (var "recurse")
      (var "mt"),
    _Term_record>>: "r" ~>
      "fields" <~ (Core.recordFields $ var "r") $
      "keyvals" <<~ Eithers.mapList (var "fieldToKeyval") (var "fields") $
      right $ Json.valueObject $ Maps.fromList $ Maybes.cat $ var "keyvals",
    _Term_set>>: "vals" ~> var "recurse" @@ (Core.termList $ Sets.toList $ var "vals"),
    _Term_typeLambda>>: "ta" ~> var "asRecord" @@ list [
      Core.field (Core.name $ string "parameter") (Core.termVariable $ Core.typeLambdaParameter $ var "ta"),
      Core.field (Core.name $ string "body") (Core.typeLambdaBody $ var "ta")],
    _Term_typeApplication>>: "tt" ~> var "asRecord" @@ list [
      Core.field (Core.name $ string "term") (Core.typeApplicationTermBody $ var "tt"),
      Core.field (Core.name $ string "type") (encoderFor _Type @@ (Core.typeApplicationTermType $ var "tt"))],
    _Term_union>>: "i" ~>
      "field" <~ (Core.injectionField $ var "i") $
      Logic.ifElse (Equality.equal (Core.fieldTerm $ var "field") Core.termUnit)
        (right $ Json.valueString $ Core.unName $ Core.fieldName $ var "field")
        ("mkeyval" <<~ var "fieldToKeyval" @@ var "field" $
          right $ Json.valueObject $ Maps.fromList $ Maybes.maybe
            (list ([] :: [TTerm (String, Value)]))
            ("keyval" ~> list [var "keyval"])
            (var "mkeyval")),
    _Term_variable>>: "v" ~> right $ Json.valueString $ Core.unName $ var "v",
    _Term_wrap>>: "wt" ~> var "recurse" @@ (Core.wrappedTermBody $ var "wt")]) $
  var "result"

readStringStub :: TBinding (String -> Term)
readStringStub = define "readStringStub" $
  doc "Placeholder for reading a string into a term (to be implemented)" $
  "s" ~> Core.termLiteral $ Core.literalString $ Strings.cat2 (string "TODO: read ") (var "s")

-- TODO: implement this function, and deduplicate with hydra.json.coder.showValue
showValue :: TBinding (Value -> String)
showValue = define "showValue" $
  doc "Show a JSON value as a string (placeholder implementation)" $
  "value" ~> string "TODO: implement showValue"
