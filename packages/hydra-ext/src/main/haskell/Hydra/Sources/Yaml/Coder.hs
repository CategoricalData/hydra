
module Hydra.Sources.Yaml.Coder where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Errors                      as Error
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Yaml.Model                       as Yaml
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Literals       as HydraLiterals
import qualified Hydra.Sources.Kernel.Terms.Strip          as Strip
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import qualified Hydra.Sources.Yaml.Language           as YamlLanguage
import           Prelude hiding ((++))
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S

import qualified Hydra.Yaml.Model as YM


-- | Lift Either String to Either Error using a context
liftStringError :: TTerm Context -> TTerm (Either String a) -> TTerm (Either Error a)
liftStringError cx = Eithers.bimap ("_s" ~> Error.errorOther (Error.otherError $ var "_s")) ("_x" ~> var "_x")

ns :: Namespace
ns = Namespace "hydra.yaml.coder"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns definitions
    [Adapt.ns,
     ExtractCore.ns, HydraLiterals.ns, YamlLanguage.ns, Strip.ns]
    (KernelTypes.kernelTypesNamespaces L.++ [Namespace "hydra.yaml.model"]) $
    Just "YAML encoding and decoding for Hydra terms"
  where
    definitions = [
      toDefinition yamlCoder,
      toDefinition literalYamlCoder,
      toDefinition recordCoder,
      toDefinition encodeRecord,
      toDefinition decodeRecord,
      toDefinition termCoder,
      toDefinition unitCoder]

yamlCoder :: TTermDefinition (Type -> Context -> Graph -> Either Error (Coder Term YM.Node))
yamlCoder = define "yamlCoder" $
  doc "Create a YAML coder for a given type" $
  "typ" ~> "cx" ~> "g" ~>
  "mkTermCoder" <~ ("t" ~> termCoder @@ var "t" @@ var "cx" @@ var "g") $
  "adapter" <<~ (Adapt.simpleLanguageAdapter @@ YamlLanguage.yamlLanguage @@ var "cx" @@ var "g" @@ var "typ") $
  "coder" <<~ var "mkTermCoder" @@ (Coders.adapterTarget $ var "adapter") $
  right $ Adapt.composeCoders @@ (Coders.adapterCoder $ var "adapter") @@ var "coder"

literalYamlCoder :: TTermDefinition (LiteralType -> Either Error (Coder Literal YM.Scalar))
literalYamlCoder = define "literalYamlCoder" $
  doc "Create a YAML coder for literal types" $
  "lt" ~>
  "decodeBool" <~ ("cx" ~> "s" ~>
    cases YM._Scalar (var "s")
      (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError (Strings.cat $ list [string "expected boolean, found scalar"])) (var "cx")) [
      YM._Scalar_bool>>: "b" ~> right (Core.literalBoolean $ var "b")]) $
  "decodeFloat" <~ ("cx" ~> "s" ~>
    cases YM._Scalar (var "s")
      (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError (Strings.cat $ list [string "expected float, found scalar"])) (var "cx")) [
      YM._Scalar_float>>: "f" ~> right (Core.literalFloat $ Core.floatValueBigfloat $ var "f")]) $
  "decodeInteger" <~ ("cx" ~> "s" ~>
    cases YM._Scalar (var "s")
      (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError (Strings.cat $ list [string "expected integer, found scalar"])) (var "cx")) [
      YM._Scalar_int>>: "i" ~> right (Core.literalInteger $ Core.integerValueBigint $ var "i")]) $
  "decodeString" <~ ("cx" ~> "s" ~>
    cases YM._Scalar (var "s")
      (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError (Strings.cat $ list [string "expected string, found scalar"])) (var "cx")) [
      YM._Scalar_str>>: "s'" ~> right (Core.literalString $ var "s'")]) $
  "encoded" <~ (cases _LiteralType (var "lt") Nothing [
    _LiteralType_boolean>>: constant $ Coders.coder
      ("cx" ~> "lit" ~>
        "b" <<~ ExtractCore.booleanLiteral @@ var "lit" $
        right (Yaml.scalarBool $ var "b"))
      (var "decodeBool"),
    _LiteralType_float>>: constant $ Coders.coder
      ("cx" ~> "lit" ~>
        "f" <<~ ExtractCore.floatLiteral @@ var "lit" $
        "bf" <<~ ExtractCore.bigfloatValue @@ var "f" $
        right (Yaml.scalarFloat $ var "bf"))
      (var "decodeFloat"),
    _LiteralType_integer>>: constant $ Coders.coder
      ("cx" ~> "lit" ~>
        "i" <<~ ExtractCore.integerLiteral @@ var "lit" $
        "bi" <<~ ExtractCore.bigintValue @@ var "i" $
        right (Yaml.scalarInt $ var "bi"))
      (var "decodeInteger"),
    _LiteralType_string>>: constant $ Coders.coder
      ("cx" ~> "lit" ~>
        "s" <<~ ExtractCore.stringLiteral @@ var "lit" $
        right (Yaml.scalarStr $ var "s"))
      (var "decodeString")]) $
  right $ var "encoded"

recordCoder :: TTermDefinition (Name -> [FieldType] -> Context -> Graph -> Either Error (Coder Term YM.Node))
recordCoder = define "recordCoder" $
  doc "Create a YAML coder for record types" $
  "tname" ~> "rt" ~> "cx" ~> "g" ~>
  "getCoder" <~ ("f" ~>
    "coder" <<~ termCoder @@ (Core.fieldTypeType $ var "f") @@ var "cx" @@ var "g" $
    right $ pair (var "f") (var "coder")) $
  "coders" <<~ Eithers.mapList (var "getCoder") (var "rt") $
  right $ Coders.coder
    ("cx" ~> "term" ~> encodeRecord @@ var "coders" @@ var "cx" @@ var "g" @@ var "term")
    ("cx" ~> "val" ~> decodeRecord @@ var "tname" @@ var "coders" @@ var "cx" @@ var "val")

encodeRecord :: TTermDefinition ([(FieldType, Coder Term YM.Node)] -> Context -> Graph -> Term -> Either Error YM.Node)
encodeRecord = define "encodeRecord" $
  doc "Encode a record term to YAML" $
  "coders" ~> "cx" ~> "graph" ~> "term" ~>
  "stripped" <~ (Strip.deannotateTerm @@ var "term") $
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
      ("encoded" <<~ Coders.coderEncode (var "coder'") @@ var "cx" @@ var "fvalue" $
        right (just $ pair (Yaml.nodeScalar $ Yaml.scalarStr $ Core.unName $ var "fname") (var "encoded")))) $
  "record" <<~ ExtractCore.termRecord @@ var "graph" @@ var "stripped" $
  "fields" <~ (Core.recordFields $ var "record") $
  "maybeFields" <<~ Eithers.mapList (var "encodeField") (Lists.zip (var "coders") (var "fields")) $
  right (Yaml.nodeMapping $ Maps.fromList $ Maybes.cat $ var "maybeFields")

decodeRecord :: TTermDefinition (Name -> [(FieldType, Coder Term YM.Node)] -> Context -> YM.Node -> Either Error Term)
decodeRecord = define "decodeRecord" $
  doc "Decode a YAML value to a record term" $
  "tname" ~> "coders" ~> "cx" ~> "n" ~>
  "decodeObjectBody" <~ ("m" ~>
    "decodeField" <~ ("coder" ~>
      "ft" <~ (Pairs.first $ var "coder") $
      "coder'" <~ (Pairs.second $ var "coder") $
      "fname" <~ (Core.fieldTypeName $ var "ft") $
      "defaultValue" <~ (Yaml.nodeScalar Yaml.scalarNull) $
      "yamlValue" <~ (Maybes.fromMaybe (var "defaultValue") $ Maps.lookup (Yaml.nodeScalar $ Yaml.scalarStr $ Core.unName $ var "fname") (var "m")) $
      "v" <<~ Coders.coderDecode (var "coder'") @@ var "cx" @@ var "yamlValue" $
      right (Core.field (var "fname") (var "v"))) $
    "fields" <<~ Eithers.mapList (var "decodeField") (var "coders") $
    right (Core.termRecord $ Core.record (var "tname") (var "fields"))) $
  cases YM._Node (var "n")
    (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError (string "expected mapping")) (var "cx")) [
    YM._Node_mapping>>: var "decodeObjectBody"]

termCoder :: TTermDefinition (Type -> Context -> Graph -> Either Error (Coder Term YM.Node))
termCoder = define "termCoder" $
  doc "Create a YAML coder for term types" $
  "typ" ~> "cx" ~> "g" ~>
  "stripped" <~ (Strip.deannotateType @@ var "typ") $
  "encodeLiteral" <~ ("ac" ~> "cx" ~> "term" ~>
    cases _Term (var "term")
      (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError (Strings.cat $ list [string "expected literal term, found: ", ShowCore.term @@ var "term"])) (var "cx")) [
      _Term_literal>>: "av" ~>
        "scalar" <<~ Coders.coderEncode (var "ac") @@ var "cx" @@ var "av" $
        right (Yaml.nodeScalar $ var "scalar")]) $
  "encodeList" <~ ("lc" ~> "cx" ~> "term" ~>
    cases _Term (var "term")
      (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError (Strings.cat $ list [string "expected list term, found: ", ShowCore.term @@ var "term"])) (var "cx")) [
      _Term_list>>: "els" ~>
        "encodedEls" <<~ Eithers.mapList ("el" ~> Coders.coderEncode (var "lc") @@ var "cx" @@ var "el") (var "els") $
        right (Yaml.nodeSequence $ var "encodedEls")]) $
  "decodeList" <~ ("lc" ~> "cx" ~> "n" ~>
    cases YM._Node (var "n")
      (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError (string "expected sequence")) (var "cx")) [
      YM._Node_sequence>>: "nodes" ~>
        "decodedNodes" <<~ Eithers.mapList ("node" ~> Coders.coderDecode (var "lc") @@ var "cx" @@ var "node") (var "nodes") $
        right (Core.termList $ var "decodedNodes")]) $
  "encodeMaybe" <~ ("maybeElementCoder" ~> "cx" ~> "maybeTerm" ~>
    "strippedMaybeTerm" <~ (Strip.deannotateTerm @@ var "maybeTerm") $
    cases _Term (var "strippedMaybeTerm")
      (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError (Strings.cat $ list [string "expected optional term, found: ", ShowCore.term @@ var "maybeTerm"])) (var "cx")) [
      _Term_maybe>>: "maybeContents" ~>
        Maybes.maybe
          (right $ Yaml.nodeScalar Yaml.scalarNull)
          ("innerTerm" ~>
            "encodedInner" <<~ Coders.coderEncode (var "maybeElementCoder") @@ var "cx" @@ var "innerTerm" $
            right (var "encodedInner"))
          (var "maybeContents")]) $
  "decodeMaybe" <~ ("maybeElementCoder" ~> "cx" ~> "yamlVal" ~>
    cases YM._Node (var "yamlVal")
      (Just $
        "decodedInner" <<~ Coders.coderDecode (var "maybeElementCoder") @@ var "cx" @@ var "yamlVal" $
        right (Core.termMaybe $ just $ var "decodedInner")) [
      YM._Node_scalar>>: "s" ~>
        cases YM._Scalar (var "s")
          (Just $
            "decodedInner" <<~ Coders.coderDecode (var "maybeElementCoder") @@ var "cx" @@ var "yamlVal" $
            right (Core.termMaybe $ just $ var "decodedInner")) [
          YM._Scalar_null>>: constant $ right (Core.termMaybe nothing)]]) $
  "result" <~ (cases _Type (var "stripped")
    (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError (Strings.cat $ list [
      string "unsupported type in YAML: ",
      ShowCore.type_ @@ var "typ"])) (var "cx")) [
    _Type_literal>>: "at" ~>
      "ac" <<~ literalYamlCoder @@ var "at" $
      right $ Coders.coder
        (var "encodeLiteral" @@ var "ac")
        ("cx" ~> "n" ~>
          cases YM._Node (var "n")
            (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError (string "expected scalar node")) (var "cx")) [
            YM._Node_scalar>>: "s" ~>
              "lit" <<~ Coders.coderDecode (var "ac") @@ var "cx" @@ var "s" $
              right (Core.termLiteral $ var "lit")]),
    _Type_list>>: "lt" ~>
      "lc" <<~ termCoder @@ var "lt" @@ var "cx" @@ var "g" $
      right $ Coders.coder
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
        "encodedK" <<~ Coders.coderEncode (var "kc") @@ var "cx" @@ var "k" $
        "encodedV" <<~ Coders.coderEncode (var "vc") @@ var "cx" @@ var "v" $
        right (pair (var "encodedK") (var "encodedV"))) $
      "decodeEntry" <~ ("cx" ~> "kv" ~>
        "k" <~ (Pairs.first $ var "kv") $
        "v" <~ (Pairs.second $ var "kv") $
        "decodedK" <<~ Coders.coderDecode (var "kc") @@ var "cx" @@ var "k" $
        "decodedV" <<~ Coders.coderDecode (var "vc") @@ var "cx" @@ var "v" $
        right (pair (var "decodedK") (var "decodedV"))) $
      right $ Coders.coder
        ("cx" ~> "term" ~>
          cases _Term (var "term")
            (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError (Strings.cat $ list [string "expected map term, found: ", ShowCore.term @@ var "term"])) (var "cx")) [
            _Term_map>>: "m" ~>
              "entries" <<~ Eithers.mapList ("entry" ~> var "encodeEntry" @@ var "cx" @@ var "entry") (Maps.toList $ var "m") $
              right (Yaml.nodeMapping $ Maps.fromList $ var "entries")])
        ("cx" ~> "n" ~>
          cases YM._Node (var "n")
            (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError (string "expected mapping")) (var "cx")) [
            YM._Node_mapping>>: "m" ~>
              "entries" <<~ Eithers.mapList ("entry" ~> var "decodeEntry" @@ var "cx" @@ var "entry") (Maps.toList $ var "m") $
              right (Core.termMap $ Maps.fromList $ var "entries")]),
    _Type_maybe>>: "maybeElementType" ~>
      "maybeElementCoder" <<~ termCoder @@ var "maybeElementType" @@ var "cx" @@ var "g" $
      right $ Coders.coder
        (var "encodeMaybe" @@ var "maybeElementCoder")
        (var "decodeMaybe" @@ var "maybeElementCoder"),
    _Type_record>>: "rt" ~> recordCoder @@ Core.name (string "yaml") @@ var "rt" @@ var "cx" @@ var "g",
    _Type_unit>>: constant $ right $ (var "hydra.yaml.coder.unitCoder" :: TTerm (Coder Term YM.Node))]) $
  var "result"

unitCoder :: TTermDefinition (Coder Term YM.Node)
unitCoder = define "unitCoder" $
  doc "YAML coder for unit values" $
  "encodeUnit" <~ ("cx" ~> "term" ~>
    cases _Term (Strip.deannotateTerm @@ var "term")
      (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError (Strings.cat $ list [string "expected unit, found: ", ShowCore.term @@ var "term"])) (var "cx")) [
      _Term_unit>>: constant $ right $ Yaml.nodeScalar Yaml.scalarNull]) $
  "decodeUnit" <~ ("cx" ~> "n" ~>
    cases YM._Node (var "n")
      (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError (string "expected null")) (var "cx")) [
      YM._Node_scalar>>: "s" ~>
        cases YM._Scalar (var "s")
          (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError (string "expected null scalar")) (var "cx")) [
          YM._Scalar_null>>: constant $ right Core.termUnit]]) $
  Coders.coder (var "encodeUnit") (var "decodeUnit")
