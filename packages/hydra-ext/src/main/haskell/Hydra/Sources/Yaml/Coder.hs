{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Yaml.Coder where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import           Hydra.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import qualified Hydra.Dsl.Lib.Strings                as Strings
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core                       as Core
import qualified Hydra.Dsl.Errors                      as Error
import qualified Hydra.Dsl.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Lib.Equality               as Equality
import qualified Hydra.Dsl.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Lib.Literals               as Literals
import qualified Hydra.Dsl.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Lib.Optionals                 as Optionals
import qualified Hydra.Dsl.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Yaml.Model                       as Yaml
import qualified Hydra.Overlay.Haskell.Dsl.Terms                           as Terms
import qualified Hydra.Overlay.Haskell.Dsl.Types                           as Types
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Literals       as HydraLiterals
import qualified Hydra.Sources.Kernel.Terms.Strip          as Strip
import qualified Hydra.Sources.Kernel.Terms.Print.Core      as PrintCore
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import qualified Hydra.Sources.Yaml.Language           as YamlLanguage
import           Prelude hiding ((++))
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S

import qualified Hydra.Yaml.Model as YM


ns :: ModuleName
ns = ModuleName "hydra.yaml.coder"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([Adapt.ns, ExtractCore.ns, HydraLiterals.ns, YamlLanguage.ns, Strip.ns, ModuleName "hydra.print.core"] L.++ (KernelTypes.kernelTypesModuleNames L.++ [ModuleName "hydra.yaml.model"])),
            moduleMetadata = descriptionMetadata (Just "YAML encoding and decoding for Hydra terms")}
  where
    definitions = [
      toDefinition decodeRecord,
      toDefinition encodeRecord,
      toDefinition literalYamlCoder,
      toDefinition recordCoder,
      toDefinition requiresYamlStringSentinel,
      toDefinition termCoder,
      toDefinition unitCoder,
      toDefinition yamlCoder]

decodeRecord :: TypedTermDefinition (Name -> [(FieldType, Coder Term YM.Node Error)] -> YM.Node -> Either Error Term)
decodeRecord = define "decodeRecord" $
  doc "Decode a YAML value to a record term" $
  "tname" ~> "coders" ~> "n" ~>
  "decodeObjectBody" <~ ("m" ~>
    "decodeField" <~ ("coder" ~>
      "ft" <~ (Pairs.first $ var "coder") $
      "coder'" <~ (Pairs.second $ var "coder") $
      "fname" <~ (Core.fieldTypeName $ var "ft") $
      "defaultValue" <~ (Yaml.nodeScalar Yaml.scalarNull) $
      "yamlValue" <~ (Optionals.fromOptional (var "defaultValue") $ Maps.lookup (Yaml.nodeScalar $ Yaml.scalarStr $ Core.unName $ var "fname") (var "m")) $
      "v" <<~ Coders.coderDecode (var "coder'") @@ var "yamlValue" $
      right (Core.field (var "fname") (var "v"))) $
    "fields" <<~ Eithers.mapList (var "decodeField") (var "coders") $
    right (Core.termRecord $ Core.record (var "tname") (var "fields"))) $
  cases YM._Node (var "n")
    (Just $ left (Error.errorOther $ Error.otherError (string "expected mapping"))) [
    YM._Node_mapping>>: var "decodeObjectBody"]

encodeRecord :: TypedTermDefinition ([(FieldType, Coder Term YM.Node Error)] -> Graph -> Term -> Either Error YM.Node)
encodeRecord = define "encodeRecord" $
  doc "Encode a record term to YAML" $
  "coders" ~> "graph" ~> "term" ~>
  "stripped" <~ (Strip.deannotateTerm @@ var "term") $
  -- Check if a field should be omitted: type is Maybe and value is TermOptional Nothing
  "isMaybeNothing" <~ ("ft" ~> "fvalue" ~>
    cases _Type (Core.fieldTypeType $ var "ft")
      (Just false) [
      _Type_optional>>: constant $
        cases _Term (var "fvalue")
          (Just false) [
          _Term_optional>>: "opt" ~> Optionals.isNone (var "opt")]]) $
  "encodeField" <~ ("coderAndField" ~>
    "ftAndCoder" <~ (Pairs.first $ var "coderAndField") $
    "field" <~ (Pairs.second $ var "coderAndField") $
    "ft" <~ (Pairs.first $ var "ftAndCoder") $
    "coder'" <~ (Pairs.second $ var "ftAndCoder") $
    "fname" <~ (Core.fieldName $ var "field") $
    "fvalue" <~ (Core.fieldTerm $ var "field") $
    Logic.ifElse (var "isMaybeNothing" @@ var "ft" @@ var "fvalue")
      (right nothing)
      ("encoded" <<~ Coders.coderEncode (var "coder'") @@ var "fvalue" $
        right (just $ pair (Yaml.nodeScalar $ Yaml.scalarStr $ Core.unName $ var "fname") (var "encoded")))) $
  "record" <<~ ExtractCore.termRecord @@ var "graph" @@ var "stripped" $
  "fields" <~ (Core.recordFields $ var "record") $
  "maybeFields" <<~ Eithers.mapList (var "encodeField") (Lists.zip (var "coders") (var "fields")) $
  right (Yaml.nodeMapping $ Maps.fromList $ Optionals.cat $ var "maybeFields")

-- | Lift Either String to Either Error using a context
liftStringError :: TypedTerm InferenceContext -> TypedTerm (Either String a) -> TypedTerm (Either Error a)
liftStringError cx = Eithers.bimap ("_s" ~> Error.errorOther (Error.otherError $ var "_s")) ("_x" ~> var "_x")

literalYamlCoder :: TypedTermDefinition (LiteralType -> Either Error (Coder Literal YM.Scalar Error))
literalYamlCoder = define "literalYamlCoder" $
  doc "Create a YAML coder for literal types" $
  "lt" ~>
  "decodeBool" <~ ("s" ~>
    cases YM._Scalar (var "s")
      (Just $ left (Error.errorOther $ Error.otherError (Strings.cat $ list [string "expected boolean, found scalar"]))) [
      YM._Scalar_bool>>: "b" ~> right (Core.literalBoolean $ var "b")]) $
  "decodeDecimal" <~ ("s" ~>
    cases YM._Scalar (var "s")
      (Just $ left (Error.errorOther $ Error.otherError (Strings.cat $ list [string "expected decimal, found scalar"]))) [
      YM._Scalar_decimal>>: "d" ~> right (Core.literalDecimal $ var "d"),
      YM._Scalar_float>>: "f" ~> right (Core.literalDecimal $ Literals.float64ToDecimal $ var "f"),
      YM._Scalar_int>>: "i" ~> right (Core.literalDecimal $ Literals.bigintToDecimal $ var "i")]) $
  "decodeFloat" <~ ("s" ~>
    cases YM._Scalar (var "s")
      (Just $ left (Error.errorOther $ Error.otherError (Strings.cat $ list [string "expected float, found scalar"]))) [
      YM._Scalar_decimal>>: "d" ~> right (Core.literalFloat $ Core.floatValueFloat64 $ Literals.decimalToFloat64 $ var "d"),
      YM._Scalar_float>>: "f" ~> right (Core.literalFloat $ Core.floatValueFloat64 $ var "f")]) $
  "decodeInteger" <~ ("s" ~>
    cases YM._Scalar (var "s")
      (Just $ left (Error.errorOther $ Error.otherError (Strings.cat $ list [string "expected integer, found scalar"]))) [
      YM._Scalar_int>>: "i" ~> right (Core.literalInteger $ Core.integerValueBigint $ var "i")]) $
  "decodeString" <~ ("s" ~>
    cases YM._Scalar (var "s")
      (Just $ left (Error.errorOther $ Error.otherError (Strings.cat $ list [string "expected string, found scalar"]))) [
      YM._Scalar_str>>: "s'" ~> right (Core.literalString $ var "s'")]) $
  "encoded" <~ (cases _LiteralType (var "lt") Nothing [
    _LiteralType_boolean>>: constant $ Coders.coder
      ("lit" ~>
        "b" <<~ ExtractCore.booleanLiteral @@ var "lit" $
        right (Yaml.scalarBool $ var "b"))
      (var "decodeBool"),
    _LiteralType_decimal>>: constant $ Coders.coder
      ("lit" ~>
        "d" <<~ ExtractCore.decimalLiteral @@ var "lit" $
        right (Yaml.scalarDecimal $ var "d"))
      (var "decodeDecimal"),
    _LiteralType_float>>: constant $ Coders.coder
      ("lit" ~>
        "f" <<~ ExtractCore.floatLiteral @@ var "lit" $
        "bf" <~ (cases _FloatValue (var "f") Nothing [
          _FloatValue_float32>>: "v" ~> Literals.float32ToFloat64 (var "v"),
          _FloatValue_float64>>: "v" ~> var "v"]) $
        "shown" <~ (Literals.showFloat64 $ var "bf") $
        Logic.ifElse (requiresYamlStringSentinel @@ var "shown")
          (left (Error.errorOther $ Error.otherError (Strings.cat $ list [string "YAML cannot represent float value: ", var "shown"])))
          (right (Yaml.scalarFloat $ var "bf")))
      (var "decodeFloat"),
    _LiteralType_integer>>: constant $ Coders.coder
      ("lit" ~>
        "i" <<~ ExtractCore.integerLiteral @@ var "lit" $
        "bi" <<~ ExtractCore.bigintValue @@ var "i" $
        right (Yaml.scalarInt $ var "bi"))
      (var "decodeInteger"),
    _LiteralType_string>>: constant $ Coders.coder
      ("lit" ~>
        "s" <<~ ExtractCore.stringLiteral @@ var "lit" $
        right (Yaml.scalarStr $ var "s"))
      (var "decodeString")]) $
  right $ var "encoded"

recordCoder :: TypedTermDefinition (Name -> [FieldType] -> InferenceContext -> Graph -> Either Error (Coder Term YM.Node Error))
recordCoder = define "recordCoder" $
  doc "Create a YAML coder for record types" $
  "tname" ~> "rt" ~> "cx" ~> "g" ~>
  "getCoder" <~ ("f" ~>
    "coder" <<~ termCoder @@ (Core.fieldTypeType $ var "f") @@ var "cx" @@ var "g" $
    right $ pair (var "f") (var "coder")) $
  "coders" <<~ Eithers.mapList (var "getCoder") (var "rt") $
  right $ Coders.coder
    ("term" ~> encodeRecord @@ var "coders" @@ var "g" @@ var "term")
    ("val" ~> decodeRecord @@ var "tname" @@ var "coders" @@ var "val")

-- | Check whether a float's string form is an IEEE value that Hydra YAML cannot represent
-- as a plain scalar: NaN, Infinity, -Infinity, or -0.0. Hydra YAML's float scalar deliberately
-- excludes these (see Yaml.Model) so the encoder must refuse them rather than silently coerce.
requiresYamlStringSentinel :: TypedTermDefinition (String -> Bool)
requiresYamlStringSentinel = define "requiresYamlStringSentinel" $
  doc "True for IEEE sentinel strings that Hydra YAML cannot represent as a float scalar." $
  "s" ~> Logic.or (Equality.equal (var "s") (string "NaN")) $
         Logic.or (Equality.equal (var "s") (string "Infinity")) $
         Logic.or (Equality.equal (var "s") (string "-Infinity"))
                  (Equality.equal (var "s") (string "-0.0"))

termCoder :: TypedTermDefinition (Type -> InferenceContext -> Graph -> Either Error (Coder Term YM.Node Error))
termCoder = define "termCoder" $
  doc "Create a YAML coder for term types" $
  "typ" ~> "cx" ~> "g" ~>
  "stripped" <~ (Strip.deannotateType @@ var "typ") $
  "encodeLiteral" <~ ("ac" ~> "term" ~>
    cases _Term (var "term")
      (Just $ left (Error.errorOther $ Error.otherError (Strings.cat $ list [string "expected literal term, found: ", PrintCore.term @@ var "term"]))) [
      _Term_literal>>: "av" ~>
        "scalar" <<~ Coders.coderEncode (var "ac") @@ var "av" $
        right (Yaml.nodeScalar $ var "scalar")]) $
  "encodeList" <~ ("lc" ~> "term" ~>
    cases _Term (var "term")
      (Just $ left (Error.errorOther $ Error.otherError (Strings.cat $ list [string "expected list term, found: ", PrintCore.term @@ var "term"]))) [
      _Term_list>>: "els" ~>
        "encodedEls" <<~ Eithers.mapList ("el" ~> Coders.coderEncode (var "lc") @@ var "el") (var "els") $
        right (Yaml.nodeSequence $ var "encodedEls")]) $
  "decodeList" <~ ("lc" ~> "n" ~>
    cases YM._Node (var "n")
      (Just $ left (Error.errorOther $ Error.otherError (string "expected sequence"))) [
      YM._Node_sequence>>: "nodes" ~>
        "decodedNodes" <<~ Eithers.mapList ("node" ~> Coders.coderDecode (var "lc") @@ var "node") (var "nodes") $
        right (Core.termList $ var "decodedNodes")]) $
  "encodeMaybe" <~ ("maybeElementCoder" ~> "maybeTerm" ~>
    "strippedMaybeTerm" <~ (Strip.deannotateTerm @@ var "maybeTerm") $
    cases _Term (var "strippedMaybeTerm")
      (Just $ left (Error.errorOther $ Error.otherError (Strings.cat $ list [string "expected optional term, found: ", PrintCore.term @@ var "maybeTerm"]))) [
      _Term_optional>>: "maybeContents" ~>
        Optionals.cases (var "maybeContents") (right $ Yaml.nodeScalar Yaml.scalarNull) ("innerTerm" ~>
            "encodedInner" <<~ Coders.coderEncode (var "maybeElementCoder") @@ var "innerTerm" $
            right (var "encodedInner"))]) $
  "decodeMaybe" <~ ("maybeElementCoder" ~> "yamlVal" ~>
    cases YM._Node (var "yamlVal")
      (Just $
        "decodedInner" <<~ Coders.coderDecode (var "maybeElementCoder") @@ var "yamlVal" $
        right (Core.termOptional $ just $ var "decodedInner")) [
      YM._Node_scalar>>: "s" ~>
        cases YM._Scalar (var "s")
          (Just $
            "decodedInner" <<~ Coders.coderDecode (var "maybeElementCoder") @@ var "yamlVal" $
            right (Core.termOptional $ just $ var "decodedInner")) [
          YM._Scalar_null>>: constant $ right (Core.termOptional nothing)]]) $
  "result" <~ (cases _Type (var "stripped")
    (Just $ left (Error.errorOther $ Error.otherError (Strings.cat $ list [
      string "unsupported type in YAML: ",
      PrintCore.type_ @@ var "typ"]))) [
    _Type_literal>>: "at" ~>
      "ac" <<~ literalYamlCoder @@ var "at" $
      right $ Coders.coder
        (var "encodeLiteral" @@ var "ac")
        ("n" ~>
          cases YM._Node (var "n")
            (Just $ left (Error.errorOther $ Error.otherError (string "expected scalar node"))) [
            YM._Node_scalar>>: "s" ~>
              "lit" <<~ Coders.coderDecode (var "ac") @@ var "s" $
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
      "encodeEntry" <~ ("kv" ~>
        "k" <~ (Pairs.first $ var "kv") $
        "v" <~ (Pairs.second $ var "kv") $
        "encodedK" <<~ Coders.coderEncode (var "kc") @@ var "k" $
        "encodedV" <<~ Coders.coderEncode (var "vc") @@ var "v" $
        right (pair (var "encodedK") (var "encodedV"))) $
      "decodeEntry" <~ ("kv" ~>
        "k" <~ (Pairs.first $ var "kv") $
        "v" <~ (Pairs.second $ var "kv") $
        "decodedK" <<~ Coders.coderDecode (var "kc") @@ var "k" $
        "decodedV" <<~ Coders.coderDecode (var "vc") @@ var "v" $
        right (pair (var "decodedK") (var "decodedV"))) $
      right $ Coders.coder
        ("term" ~>
          cases _Term (var "term")
            (Just $ left (Error.errorOther $ Error.otherError (Strings.cat $ list [string "expected map term, found: ", PrintCore.term @@ var "term"]))) [
            _Term_map>>: "m" ~>
              "entries" <<~ Eithers.mapList ("entry" ~> var "encodeEntry" @@ var "entry") (Maps.toList (var "m" :: TypedTerm (M.Map Term Term))) $
              right (Yaml.nodeMapping $ Maps.fromList $ var "entries")])
        ("n" ~>
          cases YM._Node (var "n")
            (Just $ left (Error.errorOther $ Error.otherError (string "expected mapping"))) [
            YM._Node_mapping>>: "m" ~>
              "entries" <<~ Eithers.mapList ("entry" ~> var "decodeEntry" @@ var "entry") (Maps.toList (var "m" :: TypedTerm (M.Map YM.Node YM.Node))) $
              right (Core.termMap $ Maps.fromList $ var "entries")]),
    _Type_optional>>: "maybeElementType" ~>
      "maybeElementCoder" <<~ termCoder @@ var "maybeElementType" @@ var "cx" @@ var "g" $
      right $ Coders.coder
        (var "encodeMaybe" @@ var "maybeElementCoder")
        (var "decodeMaybe" @@ var "maybeElementCoder"),
    _Type_record>>: "rt" ~> recordCoder @@ Core.name (string "yaml") @@ var "rt" @@ var "cx" @@ var "g",
    _Type_unit>>: constant $ right $ (var "hydra.yaml.coder.unitCoder" :: TypedTerm (Coder Term YM.Node Error))]) $
  var "result"

unitCoder :: TypedTermDefinition (Coder Term YM.Node Error)
unitCoder = define "unitCoder" $
  doc "YAML coder for unit values" $
  "encodeUnit" <~ ("term" ~>
    cases _Term (Strip.deannotateTerm @@ var "term")
      (Just $ left (Error.errorOther $ Error.otherError (Strings.cat $ list [string "expected unit, found: ", PrintCore.term @@ var "term"]))) [
      _Term_unit>>: constant $ right $ Yaml.nodeScalar Yaml.scalarNull]) $
  "decodeUnit" <~ ("n" ~>
    cases YM._Node (var "n")
      (Just $ left (Error.errorOther $ Error.otherError (string "expected null"))) [
      YM._Node_scalar>>: "s" ~>
        cases YM._Scalar (var "s")
          (Just $ left (Error.errorOther $ Error.otherError (string "expected null scalar"))) [
          YM._Scalar_null>>: constant $ right Core.termUnit]]) $
  Coders.coder (var "encodeUnit") (var "decodeUnit")

yamlCoder :: TypedTermDefinition (Type -> InferenceContext -> Graph -> Either Error (Coder Term YM.Node Error))
yamlCoder = define "yamlCoder" $
  doc "Create a YAML coder for a given type" $
  "typ" ~> "cx" ~> "g" ~>
  "mkTermCoder" <~ ("t" ~> termCoder @@ var "t" @@ var "cx" @@ var "g") $
  "adapter" <<~ (Adapt.simpleLanguageAdapter @@ YamlLanguage.yamlLanguage @@ var "cx" @@ var "g" @@ var "typ") $
  "coder" <<~ var "mkTermCoder" @@ (Coders.adapterTarget $ var "adapter") $
  right $ Adapt.composeCoders @@ (Coders.adapterCoder $ var "adapter") @@ var "coder"
