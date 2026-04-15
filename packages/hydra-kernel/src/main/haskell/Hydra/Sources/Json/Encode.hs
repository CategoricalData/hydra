
module Hydra.Sources.Json.Encode where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                  as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Core                       as Core
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
import qualified Hydra.Dsl.Packaging                     as Packaging
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
import qualified Hydra.Sources.Kernel.Terms.Strip          as Strip
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Paths as ShowPaths
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Variants      as ShowVariants
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++), encodeFloat)
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import Hydra.Json.Model


ns :: Namespace
ns = Namespace "hydra.json.encode"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns definitions
    [Strip.ns, moduleNamespace Literals.module_, moduleNamespace ExtractCore.module_]
    KernelTypes.kernelTypesNamespaces $
    Just "JSON encoding for Hydra terms. Converts Terms to JSON Values using Either for error handling."
  where
    definitions = [
      toDefinition toJson,
      toDefinition toJsonUntyped,
      toDefinition encodeLiteral,
      toDefinition encodeFloat,
      toDefinition encodeInteger,
      toDefinition requiresJsonStringSentinel]

-- | Encode a Term to a JSON Value, given a type and type lookup table.
-- Returns Left with an error message for unsupported term constructs.
-- The type is used to determine idiomatic encoding for optional (Maybe) fields:
--   Maybe(T) where T is not Maybe: Nothing -> null, Just v -> v (plain value)
--   Maybe(Maybe(T)): Nothing -> null, Just v -> [v] (array-wrapped, for round-trip fidelity)
-- In record context, Nothing fields of simple Maybe type are omitted entirely.
toJson :: TTermDefinition (M.Map Name Type -> Name -> Type -> Term -> Either String Value)
toJson = define "toJson" $
  doc "Encode a Hydra term to a JSON value given a type and type name. Returns Left for unsupported constructs." $
  "types" ~> "tname" ~> "typ" ~> "term" ~>
  "stripped" <~ (Strip.deannotateType @@ var "typ") $
  "strippedTerm" <~ (Strip.deannotateTerm @@ var "term") $
  cases _Type (var "stripped")
    (Just $ left $ Strings.cat $ list [
      string "unsupported type for JSON encoding: ",
      ShowCore.type_ @@ var "typ"]) [

    -- Literals
    _Type_literal>>: constant $
      cases _Term (var "strippedTerm")
        (Just $ left $ string "expected literal term") [
        _Term_literal>>: "lit" ~> encodeLiteral @@ var "lit"],

    -- Lists
    _Type_list>>: "elemType" ~>
      cases _Term (var "strippedTerm")
        (Just $ left $ string "expected list term") [
        _Term_list>>: "terms" ~>
          "results" <~ (Eithers.mapList ("t" ~> toJson @@ var "types" @@ var "tname" @@ var "elemType" @@ var "t") (var "terms")) $
          Eithers.map ("vs" ~> Json.valueArray $ var "vs") (var "results")],

    -- Sets (encode as arrays)
    _Type_set>>: "elemType" ~>
      cases _Term (var "strippedTerm")
        (Just $ left $ string "expected set term") [
        _Term_set>>: "vals" ~>
          "terms" <~ (Sets.toList $ var "vals") $
          "results" <~ (Eithers.mapList ("t" ~> toJson @@ var "types" @@ var "tname" @@ var "elemType" @@ var "t") (var "terms")) $
          Eithers.map ("vs" ~> Json.valueArray $ var "vs") (var "results")],

    -- Maybe: encoding depends on whether the inner type is itself Maybe
    _Type_maybe>>: "innerType" ~>
      "innerStripped" <~ (Strip.deannotateType @@ var "innerType") $
      "isNestedMaybe" <~ (cases _Type (var "innerStripped") (Just false) [
        _Type_maybe>>: constant true]) $
      cases _Term (var "strippedTerm")
        (Just $ left $ string "expected maybe term") [
        _Term_maybe>>: "opt" ~> optCases (var "opt")
          -- Nothing: always null
          (right Json.valueNull)
          -- Just v: plain value for simple Maybe, array-wrapped for nested Maybe
          ("v" ~>
            "encoded" <~ (toJson @@ var "types" @@ var "tname" @@ var "innerType" @@ var "v") $
            Logic.ifElse (var "isNestedMaybe")
              (Eithers.map ("ev" ~> Json.valueArray $ list [var "ev"]) (var "encoded"))
              (var "encoded"))],

    -- Records
    _Type_record>>: "rt" ~>
      cases _Term (var "strippedTerm")
        (Just $ left $ string "expected record term") [
        _Term_record>>: "r" ~>
          -- Helper to check if a field type is simple Maybe (i.e. Maybe(T) where T is not Maybe)
          "isSimpleMaybe" <~ ("ftype" ~>
            cases _Type (Strip.deannotateType @@ var "ftype") (Just false) [
              _Type_maybe>>: "innerT" ~>
                cases _Type (Strip.deannotateType @@ var "innerT") (Just true) [
                  _Type_maybe>>: constant false]]) $
          -- Encode a (fieldType, field) pair. For simple Maybe fields, omit Nothing and encode Just as plain value.
          "encodeFieldWithType" <~ ("ft" ~> "f" ~>
            "fname" <~ (Core.unName $ Core.fieldName $ var "f") $
            "fterm" <~ (Core.fieldTerm $ var "f") $
            "ftype" <~ (Core.fieldTypeType $ var "ft") $
            Logic.ifElse (var "isSimpleMaybe" @@ var "ftype")
              -- Simple Maybe field: omit Nothing, encode Just as plain value
              (cases _Term (Strip.deannotateTerm @@ var "fterm")
                (Just $ left $ string "expected maybe term for optional field") [
                _Term_maybe>>: "opt" ~> optCases (var "opt")
                  (right nothing)  -- Nothing -> omit field (signal with Nothing)
                  ("v" ~>
                    "innerType" <~ (cases _Type (Strip.deannotateType @@ var "ftype") (Just $ var "ftype") [
                      _Type_maybe>>: "it" ~> var "it"]) $
                    "encoded" <~ (toJson @@ var "types" @@ var "tname" @@ var "innerType" @@ var "v") $
                    Eithers.map ("ev" ~> just $ pair (var "fname") (var "ev")) (var "encoded"))])
              -- Non-optional field: encode normally
              ("encoded" <~ (toJson @@ var "types" @@ var "tname" @@ var "ftype" @@ var "fterm") $
                Eithers.map ("ev" ~> just $ pair (var "fname") (var "ev")) (var "encoded"))) $
          -- Zip field types with field terms and encode
          "fieldTypes" <~ (var "rt") $
          "fields" <~ (Core.recordFields $ var "r") $
          "encodedPairs" <~ (Eithers.mapList
            ("ftf" ~> var "encodeFieldWithType" @@ (Pairs.first $ var "ftf") @@ (Pairs.second $ var "ftf"))
            (Lists.zip (var "fieldTypes") (var "fields"))) $
          -- Filter out Nothing entries (omitted optional fields) and build object
          Eithers.map ("pairs" ~>
            Json.valueObject $ Maps.fromList $ Maybes.cat $ var "pairs")
            (var "encodedPairs")],

    -- Unions (single-key object)
    _Type_union>>: "rt" ~>
      cases _Term (var "strippedTerm")
        (Just $ left $ string "expected union term") [
        _Term_inject>>: "inj" ~>
          "field" <~ (Core.injectionField $ var "inj") $
          "fname" <~ (Core.unName $ Core.fieldName $ var "field") $
          "fterm" <~ (Core.fieldTerm $ var "field") $
          -- Find the field type that matches this variant
          "findFieldType" <~ ("fts" ~>
            Logic.ifElse (Lists.null $ var "fts")
              (left $ Strings.cat $ list [string "unknown variant: ", var "fname"])
              (Logic.ifElse (Equality.equal (Core.unName $ Core.fieldTypeName $ Lists.head $ var "fts") (var "fname"))
                (right $ Core.fieldTypeType $ Lists.head $ var "fts")
                (var "findFieldType" @@ (Lists.tail $ var "fts")))) $
          "ftypeResult" <~ (var "findFieldType" @@ var "rt") $
          Eithers.either_
            ("err" ~> left $ var "err")
            ("ftype" ~>
              "encodedUnion" <~ (toJson @@ var "types" @@ var "tname" @@ var "ftype" @@ var "fterm") $
              Eithers.map
                ("v" ~> Json.valueObject $ Maps.fromList $ list [pair (var "fname") (var "v")])
                (var "encodedUnion"))
            (var "ftypeResult")],

    -- Unit (empty object)
    _Type_unit>>: constant $
      right $ Json.valueObject $ Maps.empty,

    -- Wrapped types (look up inner type and recurse)
    _Type_wrap>>: "wn" ~>
      cases _Term (var "strippedTerm")
        (Just $ left $ string "expected wrapped term") [
        _Term_wrap>>: "wt" ~>
          toJson @@ var "types" @@ var "tname" @@ var "wn" @@ (Core.wrappedTermBody $ var "wt")],

    -- Maps -> array of {\"@key\": k, \"@value\": v}
    _Type_map>>: "mt" ~>
      "keyType" <~ (Core.mapTypeKeys $ var "mt") $
      "valType" <~ (Core.mapTypeValues $ var "mt") $
      cases _Term (var "strippedTerm")
        (Just $ left $ string "expected map term") [
        _Term_map>>: "m" ~>
          "encodeEntry" <~ ("kv" ~>
            "k" <~ (Pairs.first $ var "kv") $
            "v" <~ (Pairs.second $ var "kv") $
            "encodedK" <~ (toJson @@ var "types" @@ var "tname" @@ var "keyType" @@ var "k") $
            "encodedV" <~ (toJson @@ var "types" @@ var "tname" @@ var "valType" @@ var "v") $
            Eithers.either_
              ("err" ~> left $ var "err")
              ("ek" ~> Eithers.map
                ("ev" ~> Json.valueObject $ Maps.fromList $ list [
                  pair (string "@key") (var "ek"),
                  pair (string "@value") (var "ev")])
                (var "encodedV"))
              (var "encodedK")) $
          "entries" <~ (Eithers.mapList (var "encodeEntry") (Maps.toList $ var "m")) $
          Eithers.map ("es" ~> Json.valueArray $ var "es") (var "entries")],

    -- Pairs -> {\"@first\": ..., \"@second\": ...}
    _Type_pair>>: "pt" ~>
      "firstType" <~ (Core.pairTypeFirst $ var "pt") $
      "secondType" <~ (Core.pairTypeSecond $ var "pt") $
      cases _Term (var "strippedTerm")
        (Just $ left $ string "expected pair term") [
        _Term_pair>>: "p" ~>
          "first" <~ (Pairs.first $ var "p") $
          "second" <~ (Pairs.second $ var "p") $
          "encodedFirst" <~ (toJson @@ var "types" @@ var "tname" @@ var "firstType" @@ var "first") $
          "encodedSecond" <~ (toJson @@ var "types" @@ var "tname" @@ var "secondType" @@ var "second") $
          Eithers.either_
            ("err" ~> left $ var "err")
            ("ef" ~> Eithers.map
              ("es" ~> Json.valueObject $ Maps.fromList $ list [
                pair (string "@first") (var "ef"),
                pair (string "@second") (var "es")])
              (var "encodedSecond"))
            (var "encodedFirst")],

    -- Either -> {\"@left\": ...} or {\"@right\": ...}
    _Type_either>>: "et" ~>
      "leftType" <~ (Core.eitherTypeLeft $ var "et") $
      "rightType" <~ (Core.eitherTypeRight $ var "et") $
      cases _Term (var "strippedTerm")
        (Just $ left $ string "expected either term") [
        _Term_either>>: "e" ~>
          Eithers.either_
            ("l" ~>
              "encodedL" <~ (toJson @@ var "types" @@ var "tname" @@ var "leftType" @@ var "l") $
              Eithers.map
                ("v" ~> Json.valueObject $ Maps.fromList $ list [pair (string "@left") (var "v")])
                (var "encodedL"))
            ("r" ~>
              "encodedR" <~ (toJson @@ var "types" @@ var "tname" @@ var "rightType" @@ var "r") $
              Eithers.map
                ("v" ~> Json.valueObject $ Maps.fromList $ list [pair (string "@right") (var "v")])
                (var "encodedR"))
            (var "e")],

    -- Type variables (look up in type table and recurse; fall back to untyped encoding)
    _Type_variable>>: "name" ~>
      "lookedUp" <~ (Maps.lookup (var "name") (var "types")) $
      Maybes.maybe
        (toJsonUntyped @@ var "term")
        ("resolvedType" ~> toJson @@ var "types" @@ var "name" @@ var "resolvedType" @@ var "term")
        (var "lookedUp")]

-- | Encode a Term to a JSON Value without type information.
-- This is a structural fallback used when type information is unavailable (e.g. unresolved
-- type variables). It encodes terms based on their structure alone, using the legacy encoding
-- for Maybe (null/[value]) since without type info we cannot determine if idiomatic encoding
-- is safe.
toJsonUntyped :: TTermDefinition (Term -> Either String Value)
toJsonUntyped = define "toJsonUntyped" $
  doc "Encode a Hydra term to a JSON value without type information. Falls back to array-wrapped Maybe encoding." $
  "term" ~>
  "stripped" <~ (Strip.deannotateTerm @@ var "term") $
  cases _Term (var "stripped")
    (Just $ left $ Strings.cat $ list [
      string "unsupported term variant for JSON encoding: ",
      ShowCore.term @@ var "term"]) [
    -- Literals
    _Term_literal>>: "lit" ~> encodeLiteral @@ var "lit",

    -- Lists
    _Term_list>>: "terms" ~>
      "results" <~ (Eithers.mapList ("t" ~> toJsonUntyped @@ var "t") (var "terms")) $
      Eithers.map ("vs" ~> Json.valueArray $ var "vs") (var "results"),

    -- Sets (encode as arrays)
    _Term_set>>: "vals" ~>
      "terms" <~ (Sets.toList $ var "vals") $
      "results" <~ (Eithers.mapList ("t" ~> toJsonUntyped @@ var "t") (var "terms")) $
      Eithers.map ("vs" ~> Json.valueArray $ var "vs") (var "results"),

    -- Maybe (legacy encoding: null/[value], since we don't know if it's nested)
    _Term_maybe>>: "opt" ~> optCases (var "opt")
      (right Json.valueNull)
      ("v" ~>
        "encodedMaybe" <~ (toJsonUntyped @@ var "v") $
        Eithers.map ("encoded" ~> Json.valueArray $ list [var "encoded"]) (var "encodedMaybe")),

    -- Records
    _Term_record>>: "r" ~>
      "encodeField" <~ ("f" ~>
        "fname" <~ (Core.unName $ Core.fieldName $ var "f") $
        "fterm" <~ (Core.fieldTerm $ var "f") $
        "encodedField" <~ (toJsonUntyped @@ var "fterm") $
        Eithers.map ("v" ~> pair (var "fname") (var "v")) (var "encodedField")) $
      "fields" <~ (Core.recordFields $ var "r") $
      "encodedFields" <~ (Eithers.mapList (var "encodeField") (var "fields")) $
      Eithers.map ("fs" ~> Json.valueObject $ Maps.fromList $ var "fs") (var "encodedFields"),

    -- Unions (single-key object)
    _Term_inject>>: "inj" ~>
      "field" <~ (Core.injectionField $ var "inj") $
      "fname" <~ (Core.unName $ Core.fieldName $ var "field") $
      "fterm" <~ (Core.fieldTerm $ var "field") $
      "encodedUnion" <~ (toJsonUntyped @@ var "fterm") $
      Eithers.map
        ("v" ~> Json.valueObject $ Maps.fromList $ list [pair (var "fname") (var "v")])
        (var "encodedUnion"),

    -- Unit
    _Term_unit>>: constant $ right $ Json.valueObject $ Maps.empty,

    -- Wrapped terms (transparent)
    _Term_wrap>>: "wt" ~> toJsonUntyped @@ (Core.wrappedTermBody $ var "wt"),

    -- Maps -> array of {\"@key\": k, \"@value\": v}
    _Term_map>>: "m" ~>
      "encodeEntry" <~ ("kv" ~>
        "k" <~ (Pairs.first $ var "kv") $
        "v" <~ (Pairs.second $ var "kv") $
        "encodedK" <~ (toJsonUntyped @@ var "k") $
        "encodedV" <~ (toJsonUntyped @@ var "v") $
        Eithers.either_
          ("err" ~> left $ var "err")
          ("ek" ~> Eithers.map
            ("ev" ~> Json.valueObject $ Maps.fromList $ list [
              pair (string "@key") (var "ek"),
              pair (string "@value") (var "ev")])
            (var "encodedV"))
          (var "encodedK")) $
      "entries" <~ (Eithers.mapList (var "encodeEntry") (Maps.toList $ var "m")) $
      Eithers.map ("es" ~> Json.valueArray $ var "es") (var "entries"),

    -- Pairs -> {\"@first\": ..., \"@second\": ...}
    _Term_pair>>: "p" ~>
      "first" <~ (Pairs.first $ var "p") $
      "second" <~ (Pairs.second $ var "p") $
      "encodedFirst" <~ (toJsonUntyped @@ var "first") $
      "encodedSecond" <~ (toJsonUntyped @@ var "second") $
      Eithers.either_
        ("err" ~> left $ var "err")
        ("ef" ~> Eithers.map
          ("es" ~> Json.valueObject $ Maps.fromList $ list [
            pair (string "@first") (var "ef"),
            pair (string "@second") (var "es")])
          (var "encodedSecond"))
        (var "encodedFirst"),

    -- Either -> {\"@left\": ...} or {\"@right\": ...}
    _Term_either>>: "e" ~>
      Eithers.either_
        ("l" ~>
          "encodedL" <~ (toJsonUntyped @@ var "l") $
          Eithers.map
            ("v" ~> Json.valueObject $ Maps.fromList $ list [pair (string "@left") (var "v")])
            (var "encodedL"))
        ("r" ~>
          "encodedR" <~ (toJsonUntyped @@ var "r") $
          Eithers.map
            ("v" ~> Json.valueObject $ Maps.fromList $ list [pair (string "@right") (var "v")])
            (var "encodedR"))
        (var "e")]

-- | Encode a literal value to JSON
encodeLiteral :: TTermDefinition (Literal -> Either String Value)
encodeLiteral = define "encodeLiteral" $
  doc "Encode a Hydra literal to a JSON value" $
  "lit" ~> cases _Literal (var "lit") Nothing [
    _Literal_binary>>: "b" ~> right $ Json.valueString $ Literals.binaryToString $ var "b",
    _Literal_boolean>>: "b" ~> right $ Json.valueBoolean $ var "b",
    _Literal_decimal>>: "d" ~> right $ Json.valueNumber $ var "d",
    _Literal_float>>: "f" ~> encodeFloat @@ var "f",
    _Literal_integer>>: "i" ~> encodeInteger @@ var "i",
    _Literal_string>>: "s" ~> right $ Json.valueString $ var "s"]

-- | Encode a float value to JSON.
-- Bigfloat uses native JSON numbers (lossless for finite values via Scientific). Bigfloat
-- rejects every IEEE value that cannot be represented by its target types (Java BigDecimal,
-- Python Decimal cannot hold NaN, ±Infinity, or -0.0 at all), so those produce an error.
-- Float32 always uses a JSON string to preserve exact source precision. Float64 encodes via
-- a JSON number for finite, non-negative-zero values and via a JSON string for the values
-- that the JSON grammar cannot express (NaN, ±Infinity, -0.0) — keeping those IEEE values
-- round-trippable through JSON.
encodeFloat :: TTermDefinition (FloatValue -> Either String Value)
encodeFloat = define "encodeFloat" $
  doc "Encode a float value to JSON. Bigfloat rejects anything the decimal space can't hold; Float64 uses string sentinels for NaN/Inf/-0.0; Float32 always strings." $
  "fv" ~> cases _FloatValue (var "fv") Nothing [
    _FloatValue_bigfloat>>: "bf" ~>
      "s" <~ (Literals.showBigfloat $ var "bf") $
      Logic.ifElse (requiresJsonStringSentinel @@ var "s")
        (left $ Strings.cat $ list [string "JSON cannot represent bigfloat value: ", var "s"])
        (right $ Json.valueNumber $ Literals.float64ToDecimal $ Literals.bigfloatToFloat64 $ var "bf"),
    _FloatValue_float32>>: "f" ~> right $ Json.valueString $ Literals.showFloat32 $ var "f",
    _FloatValue_float64>>: "f" ~>
      "s" <~ (Literals.showFloat64 $ var "f") $
      Logic.ifElse (requiresJsonStringSentinel @@ var "s")
        (right $ Json.valueString $ var "s")
        (right $ Json.valueNumber $ Literals.float64ToDecimal $ var "f")]

-- | Check whether a float's string form is an IEEE value that the JSON number grammar or
-- Scientific-backed decoding cannot round-trip: NaN, Infinity, -Infinity, or -0.0.
requiresJsonStringSentinel :: TTermDefinition (String -> Bool)
requiresJsonStringSentinel = define "requiresJsonStringSentinel" $
  doc "True for IEEE sentinel strings that JSON must escape as a string to preserve." $
  "s" ~> Logic.or (Equality.equal (var "s") (string "NaN")) $
         Logic.or (Equality.equal (var "s") (string "Infinity")) $
         Logic.or (Equality.equal (var "s") (string "-Infinity"))
                  (Equality.equal (var "s") (string "-0.0"))

-- | Encode an integer value to JSON
-- Small integers (int8, int16, int32, uint8, uint16) use native JSON numbers
-- Large integers (int64, uint32, uint64, bigint) use strings to preserve precision
encodeInteger :: TTermDefinition (IntegerValue -> Either String Value)
encodeInteger = define "encodeInteger" $
  doc "Encode an integer value to JSON. Small ints use native numbers; large ints use strings." $
  "iv" ~> cases _IntegerValue (var "iv") Nothing [
    -- Large integers: use strings to preserve precision
    _IntegerValue_bigint>>: "bi" ~> right $ Json.valueString $ Literals.showBigint $ var "bi",
    _IntegerValue_int64>>: "i" ~> right $ Json.valueString $ Literals.showInt64 $ var "i",
    _IntegerValue_uint32>>: "i" ~> right $ Json.valueString $ Literals.showUint32 $ var "i",
    _IntegerValue_uint64>>: "i" ~> right $ Json.valueString $ Literals.showUint64 $ var "i",
    -- Small integers: use native JSON numbers (convert to decimal for JSON)
    _IntegerValue_int8>>: "i" ~> right $ Json.valueNumber $ Literals.bigintToDecimal $ Literals.int8ToBigint $ var "i",
    _IntegerValue_int16>>: "i" ~> right $ Json.valueNumber $ Literals.bigintToDecimal $ Literals.int16ToBigint $ var "i",
    _IntegerValue_int32>>: "i" ~> right $ Json.valueNumber $ Literals.bigintToDecimal $ Literals.int32ToBigint $ var "i",
    _IntegerValue_uint8>>: "i" ~> right $ Json.valueNumber $ Literals.bigintToDecimal $ Literals.uint8ToBigint $ var "i",
    _IntegerValue_uint16>>: "i" ~> right $ Json.valueNumber $ Literals.bigintToDecimal $ Literals.uint16ToBigint $ var "i"]
