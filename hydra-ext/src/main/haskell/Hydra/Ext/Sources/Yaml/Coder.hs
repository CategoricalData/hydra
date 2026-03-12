
module Hydra.Ext.Sources.Yaml.Coder where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Meta.Compute                    as Compute
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Error                      as Error
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Meta.Yaml                       as Yaml
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Literals       as HydraLiterals
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S

import qualified Hydra.Ext.Org.Yaml.Model as YM


ns :: Namespace
ns = Namespace "hydra.ext.org.yaml.coder"

define :: String -> TTerm a -> TBinding a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
    [ExtractCore.ns, HydraLiterals.ns, Rewriting.ns]
    (KernelTypes.kernelTypesNamespaces L.++ [Namespace "hydra.ext.org.yaml.model"]) $
    Just "YAML encoding and decoding for Hydra terms"
  where
    elements = [
      toBinding yamlCoder,
      toBinding literalYamlCoder,
      toBinding recordCoder,
      toBinding encodeRecord,
      toBinding decodeRecord,
      toBinding termCoder,
      toBinding unitCoder]

yamlCoder :: TBinding (Type -> Context -> Graph -> Either (InContext OtherError) (Coder Term YM.Node))
yamlCoder = define "yamlCoder" $
  doc "Create a YAML coder for a given type" $
  "typ" ~> "cx" ~> "g" ~>
  termCoder @@ var "typ" @@ var "cx" @@ var "g"

literalYamlCoder :: TBinding (LiteralType -> Either (InContext OtherError) (Coder Literal YM.Scalar))
literalYamlCoder = define "literalYamlCoder" $
  doc "Create a YAML coder for literal types" $
  "lt" ~>
  "decodeBool" <~ ("cx" ~> "s" ~>
    cases YM._Scalar (var "s")
      (Just $ Ctx.failInContext (Error.otherError (Strings.cat $ list [string "expected boolean, found scalar"])) (var "cx")) [
      YM._Scalar_bool>>: "b" ~> right (Core.literalBoolean $ var "b")]) $
  "decodeFloat" <~ ("ft" ~> "cx" ~> "s" ~>
    cases YM._Scalar (var "s")
      (Just $ Ctx.failInContext (Error.otherError (Strings.cat $ list [string "expected float, found scalar"])) (var "cx")) [
      YM._Scalar_float>>: "f" ~> right (Core.literalFloat $ HydraLiterals.bigfloatToFloatValue @@ var "ft" @@ var "f")]) $
  "decodeInteger" <~ ("it" ~> "cx" ~> "s" ~>
    cases YM._Scalar (var "s")
      (Just $ Ctx.failInContext (Error.otherError (Strings.cat $ list [string "expected integer, found scalar"])) (var "cx")) [
      YM._Scalar_int>>: "i" ~> right (Core.literalInteger $ HydraLiterals.bigintToIntegerValue @@ var "it" @@ var "i")]) $
  "decodeString" <~ ("cx" ~> "s" ~>
    cases YM._Scalar (var "s")
      (Just $ Ctx.failInContext (Error.otherError (Strings.cat $ list [string "expected string, found scalar"])) (var "cx")) [
      YM._Scalar_str>>: "s'" ~> right (Core.literalString $ var "s'")]) $
  "encoded" <~ (cases _LiteralType (var "lt") Nothing [
    _LiteralType_boolean>>: constant $ Compute.coder
      ("cx" ~> "lit" ~>
        "b" <<~ ExtractCore.booleanLiteral @@ var "cx" @@ var "lit" $
        right (Yaml.scalarBool $ var "b"))
      (var "decodeBool"),
    _LiteralType_float>>: "ft" ~> Compute.coder
      ("cx" ~> "lit" ~>
        "f" <<~ ExtractCore.floatLiteral @@ var "cx" @@ var "lit" $
        right (Yaml.scalarFloat $ HydraLiterals.floatValueToBigfloat @@ var "f"))
      (var "decodeFloat" @@ var "ft"),
    _LiteralType_integer>>: "it" ~> Compute.coder
      ("cx" ~> "lit" ~>
        "i" <<~ ExtractCore.integerLiteral @@ var "cx" @@ var "lit" $
        right (Yaml.scalarInt $ HydraLiterals.integerValueToBigint @@ var "i"))
      (var "decodeInteger" @@ var "it"),
    _LiteralType_string>>: constant $ Compute.coder
      ("cx" ~> "lit" ~>
        "s" <<~ ExtractCore.stringLiteral @@ var "cx" @@ var "lit" $
        right (Yaml.scalarStr $ var "s"))
      (var "decodeString")]) $
  right $ var "encoded"

recordCoder :: TBinding (RowType -> Context -> Graph -> Either (InContext OtherError) (Coder Term YM.Node))
recordCoder = define "recordCoder" $
  doc "Create a YAML coder for record types" $
  "rt" ~> "cx" ~> "g" ~>
  "fields" <~ (Core.rowTypeFields $ var "rt") $
  "getCoder" <~ ("f" ~>
    "coder" <<~ termCoder @@ (Core.fieldTypeType $ var "f") @@ var "cx" @@ var "g" $
    right $ pair (var "f") (var "coder")) $
  "coders" <<~ Eithers.mapList (var "getCoder") (var "fields") $
  right $ Compute.coder
    ("cx" ~> "term" ~> encodeRecord @@ var "coders" @@ var "cx" @@ var "g" @@ var "term")
    ("cx" ~> "val" ~> decodeRecord @@ var "rt" @@ var "coders" @@ var "cx" @@ var "val")

encodeRecord :: TBinding ([(FieldType, Coder Term YM.Node)] -> Context -> Graph -> Term -> Either (InContext OtherError) YM.Node)
encodeRecord = define "encodeRecord" $
  doc "Encode a record term to YAML" $
  "coders" ~> "cx" ~> "graph" ~> "term" ~>
  "stripped" <~ (Rewriting.deannotateTerm @@ var "term") $
  -- Check if a field should be omitted: type is Maybe and value is TermMaybe Nothing
  "isMaybeNothing" <~ ("ft" ~> "fvalue" ~>
    cases _Type (Core.fieldTypeType $ var "ft")
      (Just false) [
      _Type_maybe>>: constant $
        cases _Term (var "fvalue")
          (Just false) [
          _Term_maybe>>: "opt" ~> Maybes.isNothing (var "opt")]]) $
  "encodeField" <~ ("coderAndField" ~>
    "ftAndCoder" <~ (Pairs.first $ var "coderAndField") $
    "field" <~ (Pairs.second $ var "coderAndField") $
    "ft" <~ (Pairs.first $ var "ftAndCoder") $
    "coder'" <~ (Pairs.second $ var "ftAndCoder") $
    "fname" <~ (Core.fieldName $ var "field") $
    "fvalue" <~ (Core.fieldTerm $ var "field") $
    Logic.ifElse (var "isMaybeNothing" @@ var "ft" @@ var "fvalue")
      (right nothing)
      ("encoded" <<~ Compute.coderEncode (var "coder'") @@ var "cx" @@ var "fvalue" $
        right (just $ pair (Yaml.nodeScalar $ Yaml.scalarStr $ Core.unName $ var "fname") (var "encoded")))) $
  "record" <<~ ExtractCore.termRecord @@ var "cx" @@ var "graph" @@ var "stripped" $
  "fields" <~ (Core.recordFields $ var "record") $
  "maybeFields" <<~ Eithers.mapList (var "encodeField") (Lists.zip (var "coders") (var "fields")) $
  right (Yaml.nodeMapping $ Maps.fromList $ Maybes.cat $ var "maybeFields")

decodeRecord :: TBinding (RowType -> [(FieldType, Coder Term YM.Node)] -> Context -> YM.Node -> Either (InContext OtherError) Term)
decodeRecord = define "decodeRecord" $
  doc "Decode a YAML value to a record term" $
  "rt" ~> "coders" ~> "cx" ~> "n" ~>
  "decodeObjectBody" <~ ("m" ~>
    "decodeField" <~ ("coder" ~>
      "ft" <~ (Pairs.first $ var "coder") $
      "coder'" <~ (Pairs.second $ var "coder") $
      "fname" <~ (Core.fieldTypeName $ var "ft") $
      "defaultValue" <~ (Yaml.nodeScalar Yaml.scalarNull) $
      "yamlValue" <~ (Maybes.fromMaybe (var "defaultValue") $ Maps.lookup (Yaml.nodeScalar $ Yaml.scalarStr $ Core.unName $ var "fname") (var "m")) $
      "v" <<~ Compute.coderDecode (var "coder'") @@ var "cx" @@ var "yamlValue" $
      right (Core.field (var "fname") (var "v"))) $
    "fields" <<~ Eithers.mapList (var "decodeField") (var "coders") $
    right (Core.termRecord $ Core.record (Core.rowTypeTypeName $ var "rt") (var "fields"))) $
  cases YM._Node (var "n")
    (Just $ Ctx.failInContext (Error.otherError (string "expected mapping")) (var "cx")) [
    YM._Node_mapping>>: var "decodeObjectBody"]

termCoder :: TBinding (Type -> Context -> Graph -> Either (InContext OtherError) (Coder Term YM.Node))
termCoder = define "termCoder" $
  doc "Create a YAML coder for term types" $
  "typ" ~> "cx" ~> "g" ~>
  "stripped" <~ (Rewriting.deannotateType @@ var "typ") $
  "encodeLiteral" <~ ("ac" ~> "cx" ~> "term" ~>
    cases _Term (var "term")
      (Just $ Ctx.failInContext (Error.otherError (Strings.cat $ list [string "expected literal term, found: ", ShowCore.term @@ var "term"])) (var "cx")) [
      _Term_literal>>: "av" ~>
        "scalar" <<~ Compute.coderEncode (var "ac") @@ var "cx" @@ var "av" $
        right (Yaml.nodeScalar $ var "scalar")]) $
  "encodeList" <~ ("lc" ~> "cx" ~> "term" ~>
    cases _Term (var "term")
      (Just $ Ctx.failInContext (Error.otherError (Strings.cat $ list [string "expected list term, found: ", ShowCore.term @@ var "term"])) (var "cx")) [
      _Term_list>>: "els" ~>
        "encodedEls" <<~ Eithers.mapList ("el" ~> Compute.coderEncode (var "lc") @@ var "cx" @@ var "el") (var "els") $
        right (Yaml.nodeSequence $ var "encodedEls")]) $
  "decodeList" <~ ("lc" ~> "cx" ~> "n" ~>
    cases YM._Node (var "n")
      (Just $ Ctx.failInContext (Error.otherError (string "expected sequence")) (var "cx")) [
      YM._Node_sequence>>: "nodes" ~>
        "decodedNodes" <<~ Eithers.mapList ("node" ~> Compute.coderDecode (var "lc") @@ var "cx" @@ var "node") (var "nodes") $
        right (Core.termList $ var "decodedNodes")]) $
  "encodeMaybe" <~ ("maybeElementCoder" ~> "cx" ~> "maybeTerm" ~>
    "strippedMaybeTerm" <~ (Rewriting.deannotateTerm @@ var "maybeTerm") $
    cases _Term (var "strippedMaybeTerm")
      (Just $ Ctx.failInContext (Error.otherError (Strings.cat $ list [string "expected optional term, found: ", ShowCore.term @@ var "maybeTerm"])) (var "cx")) [
      _Term_maybe>>: "maybeContents" ~>
        Logic.ifElse (Maybes.isNothing $ var "maybeContents")
          (right $ Yaml.nodeScalar Yaml.scalarNull)
          ("encodedInner" <<~ Compute.coderEncode (var "maybeElementCoder") @@ var "cx" @@ (Maybes.fromJust $ var "maybeContents") $
            right (var "encodedInner"))]) $
  "decodeMaybe" <~ ("maybeElementCoder" ~> "cx" ~> "yamlVal" ~>
    cases YM._Node (var "yamlVal")
      (Just $
        "decodedInner" <<~ Compute.coderDecode (var "maybeElementCoder") @@ var "cx" @@ var "yamlVal" $
        right (Core.termMaybe $ just $ var "decodedInner")) [
      YM._Node_scalar>>: "s" ~>
        cases YM._Scalar (var "s")
          (Just $
            "decodedInner" <<~ Compute.coderDecode (var "maybeElementCoder") @@ var "cx" @@ var "yamlVal" $
            right (Core.termMaybe $ just $ var "decodedInner")) [
          YM._Scalar_null>>: constant $ right (Core.termMaybe nothing)]]) $
  "result" <~ (cases _Type (var "stripped")
    (Just $ Ctx.failInContext (Error.otherError (Strings.cat $ list [
      string "unsupported type in YAML: ",
      ShowCore.type_ @@ var "typ"])) (var "cx")) [
    _Type_literal>>: "at" ~>
      "ac" <<~ literalYamlCoder @@ var "at" $
      right $ Compute.coder
        (var "encodeLiteral" @@ var "ac")
        ("cx" ~> "n" ~>
          cases YM._Node (var "n")
            (Just $ Ctx.failInContext (Error.otherError (string "expected scalar node")) (var "cx")) [
            YM._Node_scalar>>: "s" ~>
              "lit" <<~ Compute.coderDecode (var "ac") @@ var "cx" @@ var "s" $
              right (Core.termLiteral $ var "lit")]),
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
      "encodeEntry" <~ ("cx" ~> "kv" ~>
        "k" <~ (Pairs.first $ var "kv") $
        "v" <~ (Pairs.second $ var "kv") $
        "encodedK" <<~ Compute.coderEncode (var "kc") @@ var "cx" @@ var "k" $
        "encodedV" <<~ Compute.coderEncode (var "vc") @@ var "cx" @@ var "v" $
        right (pair (var "encodedK") (var "encodedV"))) $
      "decodeEntry" <~ ("cx" ~> "kv" ~>
        "k" <~ (Pairs.first $ var "kv") $
        "v" <~ (Pairs.second $ var "kv") $
        "decodedK" <<~ Compute.coderDecode (var "kc") @@ var "cx" @@ var "k" $
        "decodedV" <<~ Compute.coderDecode (var "vc") @@ var "cx" @@ var "v" $
        right (pair (var "decodedK") (var "decodedV"))) $
      right $ Compute.coder
        ("cx" ~> "term" ~>
          cases _Term (var "term")
            (Just $ Ctx.failInContext (Error.otherError (Strings.cat $ list [string "expected map term, found: ", ShowCore.term @@ var "term"])) (var "cx")) [
            _Term_map>>: "m" ~>
              "entries" <<~ Eithers.mapList ("entry" ~> var "encodeEntry" @@ var "cx" @@ var "entry") (Maps.toList $ var "m") $
              right (Yaml.nodeMapping $ Maps.fromList $ var "entries")])
        ("cx" ~> "n" ~>
          cases YM._Node (var "n")
            (Just $ Ctx.failInContext (Error.otherError (string "expected mapping")) (var "cx")) [
            YM._Node_mapping>>: "m" ~>
              "entries" <<~ Eithers.mapList ("entry" ~> var "decodeEntry" @@ var "cx" @@ var "entry") (Maps.toList $ var "m") $
              right (Core.termMap $ Maps.fromList $ var "entries")]),
    _Type_maybe>>: "maybeElementType" ~>
      "isNestedMaybe" <~ (cases _Type (Rewriting.deannotateType @@ var "maybeElementType")
        (Just false) [
        _Type_maybe>>: constant true]) $
      Logic.ifElse (var "isNestedMaybe")
        -- Nested Maybe<Maybe<T>> uses list encoding to disambiguate Nothing from Just Nothing
        ("listCoder" <<~ termCoder @@ (Core.typeList $ var "maybeElementType") @@ var "cx" @@ var "g" $
          right $ Compute.coder
            ("cx" ~> "maybeTerm" ~>
              "strippedMaybeTerm" <~ (Rewriting.deannotateTerm @@ var "maybeTerm") $
              cases _Term (var "strippedMaybeTerm")
                (Just $ Ctx.failInContext (Error.otherError (Strings.cat $ list [string "expected optional term, found: ", ShowCore.term @@ var "maybeTerm"])) (var "cx")) [
                _Term_maybe>>: "maybeContents" ~>
                  Logic.ifElse (Maybes.isNothing $ var "maybeContents")
                    (Compute.coderEncode (var "listCoder") @@ var "cx" @@ (Core.termList $ list ([] :: [TTerm Term])))
                    (Compute.coderEncode (var "listCoder") @@ var "cx" @@ (Core.termList $ list [Maybes.fromJust $ var "maybeContents"]))])
            ("cx" ~> "n" ~>
              "listTerm" <<~ Compute.coderDecode (var "listCoder") @@ var "cx" @@ var "n" $
              cases _Term (var "listTerm")
                (Just $ Ctx.failInContext (Error.otherError (string "expected list term after decoding")) (var "cx")) [
                _Term_list>>: "l" ~>
                  Logic.ifElse (Lists.null $ var "l")
                    (right (Core.termMaybe nothing))
                    (right (Core.termMaybe $ just $ Lists.head $ var "l"))]))
        -- Simple Maybe<T> (non-nested) uses null encoding
        ("maybeElementCoder" <<~ termCoder @@ var "maybeElementType" @@ var "cx" @@ var "g" $
          right $ Compute.coder
            (var "encodeMaybe" @@ var "maybeElementCoder")
            (var "decodeMaybe" @@ var "maybeElementCoder")),
    _Type_record>>: "rt" ~> recordCoder @@ var "rt" @@ var "cx" @@ var "g",
    _Type_set>>: "st" ~>
      "lc" <<~ termCoder @@ var "st" @@ var "cx" @@ var "g" $
      right $ Compute.coder
        ("cx" ~> "term" ~>
          cases _Term (var "term")
            (Just $ Ctx.failInContext (Error.otherError (Strings.cat $ list [string "expected set term, found: ", ShowCore.term @@ var "term"])) (var "cx")) [
            _Term_set>>: "s" ~>
              var "encodeList" @@ var "lc" @@ var "cx" @@ (Core.termList $ Sets.toList $ var "s")])
        ("cx" ~> "n" ~>
          "listTerm" <<~ var "decodeList" @@ var "lc" @@ var "cx" @@ var "n" $
          cases _Term (var "listTerm")
            (Just $ Ctx.failInContext (Error.otherError (string "expected list term after decoding")) (var "cx")) [
            _Term_list>>: "l" ~> right (Core.termSet $ Sets.fromList $ var "l")]),
    _Type_union>>: "rt" ~>
      "nm" <~ (Core.rowTypeTypeName $ var "rt") $
      "sfields" <~ (Core.rowTypeFields $ var "rt") $
      "fieldCoders" <<~ Eithers.mapList ("f" ~>
        "fc" <<~ termCoder @@ (Core.fieldTypeType $ var "f") @@ var "cx" @@ var "g" $
        right $ pair (Core.unName $ Core.fieldTypeName $ var "f") (var "fc")) (var "sfields") $
      "coderMap" <~ (Maps.fromList $ var "fieldCoders") $
      right $ Compute.coder
        ("cx" ~> "term" ~>
          "field" <<~ ExtractCore.injection @@ var "cx" @@ var "nm" @@ var "g" @@ var "term" $
          "fn" <~ (Core.fieldName $ var "field") $
          "fterm" <~ (Core.fieldTerm $ var "field") $
          "fnStr" <~ (Core.unName $ var "fn") $
          Maybes.maybe
            (Ctx.failInContext (Error.otherError (Strings.cat $ list [string "no coder for union field: ", var "fnStr"])) (var "cx"))
            ("cdr" ~>
              "encodedVal" <<~ Compute.coderEncode (var "cdr") @@ var "cx" @@ var "fterm" $
              right (Yaml.nodeMapping $ Maps.fromList $ list [pair (Yaml.nodeScalar $ Yaml.scalarStr $ var "fnStr") (var "encodedVal")]))
            (Maps.lookup (var "fnStr") (var "coderMap")))
        ("cx" ~> "n" ~>
          cases YM._Node (var "n")
            (Just $ Ctx.failInContext (Error.otherError (string "expected mapping for union")) (var "cx")) [
            YM._Node_mapping>>: "m" ~>
              "entries" <~ (Maps.toList $ var "m") $
              Logic.ifElse (Lists.null $ var "entries")
                (Ctx.failInContext (Error.otherError (string "empty mapping for union")) (var "cx"))
                ("entry" <~ (Lists.head $ var "entries") $
                 "keyNode" <~ (Pairs.first $ var "entry") $
                 "valNode" <~ (Pairs.second $ var "entry") $
                 cases YM._Node (var "keyNode")
                   (Just $ Ctx.failInContext (Error.otherError (string "expected scalar key in union mapping")) (var "cx")) [
                   YM._Node_scalar>>: "s" ~>
                     cases YM._Scalar (var "s")
                       (Just $ Ctx.failInContext (Error.otherError (string "expected string key in union mapping")) (var "cx")) [
                       YM._Scalar_str>>: "fieldName" ~>
                         Maybes.maybe
                           (Ctx.failInContext (Error.otherError (Strings.cat $ list [string "unknown union field: ", var "fieldName"])) (var "cx"))
                           ("cdr" ~>
                             "decodedVal" <<~ Compute.coderDecode (var "cdr") @@ var "cx" @@ var "valNode" $
                             right (Core.termUnion $ Core.injection (var "nm") (Core.field (Core.name $ var "fieldName") (var "decodedVal"))))
                           (Maps.lookup (var "fieldName") (var "coderMap"))]])]),
    _Type_unit>>: constant $ right $ (var "hydra.ext.org.yaml.coder.unitCoder" :: TTerm (Coder Term YM.Node)),
    _Type_wrap>>: "wt" ~>
      "tname" <~ (Core.wrappedTypeTypeName $ var "wt") $
      "innerCoder" <<~ termCoder @@ (Core.wrappedTypeBody $ var "wt") @@ var "cx" @@ var "g" $
      right $ Compute.coder
        ("cx" ~> "term" ~>
          "inner" <<~ ExtractCore.wrap @@ var "cx" @@ var "tname" @@ var "g" @@ var "term" $
          Compute.coderEncode (var "innerCoder") @@ var "cx" @@ var "inner")
        ("cx" ~> "n" ~>
          "decoded" <<~ Compute.coderDecode (var "innerCoder") @@ var "cx" @@ var "n" $
          right (Core.termWrap $ Core.wrappedTerm (var "tname") (var "decoded")))]) $
  var "result"

unitCoder :: TBinding (Coder Term YM.Node)
unitCoder = define "unitCoder" $
  doc "YAML coder for unit values" $
  "encodeUnit" <~ ("cx" ~> "term" ~>
    cases _Term (Rewriting.deannotateTerm @@ var "term")
      (Just $ Ctx.failInContext (Error.otherError (Strings.cat $ list [string "expected unit, found: ", ShowCore.term @@ var "term"])) (var "cx")) [
      _Term_unit>>: constant $ right $ Yaml.nodeScalar Yaml.scalarNull]) $
  "decodeUnit" <~ ("cx" ~> "n" ~>
    cases YM._Node (var "n")
      (Just $ Ctx.failInContext (Error.otherError (string "expected null")) (var "cx")) [
      YM._Node_scalar>>: "s" ~>
        cases YM._Scalar (var "s")
          (Just $ Ctx.failInContext (Error.otherError (string "expected null scalar")) (var "cx")) [
          YM._Scalar_null>>: constant $ right Core.termUnit]]) $
  Compute.coder (var "encodeUnit") (var "decodeUnit")
