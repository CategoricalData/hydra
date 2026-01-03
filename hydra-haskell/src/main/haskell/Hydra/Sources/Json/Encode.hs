
module Hydra.Sources.Json.Encode where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
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
import qualified Hydra.Sources.Kernel.Terms.All             as KernelTerms
import qualified Hydra.Sources.Kernel.Types.All             as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.Rewriting       as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Show.Core       as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Core    as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Literals        as HydraLiterals
import           Prelude hiding ((++), encodeFloat)
import qualified Data.Int                                   as I
import qualified Data.List                                  as L
import qualified Data.Map                                   as M
import qualified Data.Set                                   as S
import qualified Data.Maybe                                 as Y

-- Additional imports
import Hydra.Json


ns :: Namespace
ns = Namespace "hydra.json.encode"

define :: String -> TTerm a -> TBinding a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
    [Rewriting.ns, moduleNamespace HydraLiterals.module_, moduleNamespace ExtractCore.module_]
    KernelTypes.kernelTypesNamespaces $
    Just "JSON encoding for Hydra terms. Converts Terms to JSON Values using Either for error handling."
  where
    elements = [
      toBinding toJson,
      toBinding encodeLiteral,
      toBinding encodeFloat,
      toBinding encodeInteger]

-- | Encode a Term to a JSON Value.
-- Returns Left with an error message for unsupported term constructs.
toJson :: TBinding (Term -> Either String Value)
toJson = define "toJson" $
  doc "Encode a Hydra term to a JSON value. Returns Left for unsupported constructs." $
  "term" ~>
  "stripped" <~ (Rewriting.deannotateTerm @@ var "term") $
  cases _Term (var "stripped")
    (Just $ left $ Strings.cat $ list [
      string "unsupported term variant for JSON encoding: ",
      ShowCore.term @@ var "term"]) [
    -- Literals
    _Term_literal>>: "lit" ~> encodeLiteral @@ var "lit",

    -- Lists
    _Term_list>>: "terms" ~>
      "results" <~ (Eithers.mapList ("t" ~> toJson @@ var "t") (var "terms")) $
      Eithers.map ("vs" ~> Json.valueArray $ var "vs") (var "results"),

    -- Sets (encode as arrays)
    _Term_set>>: "vals" ~>
      "terms" <~ (Sets.toList $ var "vals") $
      "results" <~ (Eithers.mapList ("t" ~> toJson @@ var "t") (var "terms")) $
      Eithers.map ("vs" ~> Json.valueArray $ var "vs") (var "results"),

    -- Maybe
    _Term_maybe>>: "opt" ~> optCases (var "opt")
      (right Json.valueNull)
      ("v" ~>
        "encodedMaybe" <~ (toJson @@ var "v") $
        Eithers.map ("encoded" ~> Json.valueArray $ list [var "encoded"]) (var "encodedMaybe")),

    -- Records
    _Term_record>>: "r" ~>
      "encodeField" <~ ("f" ~>
        "fname" <~ (Core.unName $ Core.fieldName $ var "f") $
        "fterm" <~ (Core.fieldTerm $ var "f") $
        "encodedField" <~ (toJson @@ var "fterm") $
        Eithers.map ("v" ~> pair (var "fname") (var "v")) (var "encodedField")) $
      "fields" <~ (Core.recordFields $ var "r") $
      "encodedFields" <~ (Eithers.mapList (var "encodeField") (var "fields")) $
      Eithers.map ("fs" ~> Json.valueObject $ Maps.fromList $ var "fs") (var "encodedFields"),

    -- Unions (single-key object)
    _Term_union>>: "inj" ~>
      "field" <~ (Core.injectionField $ var "inj") $
      "fname" <~ (Core.unName $ Core.fieldName $ var "field") $
      "fterm" <~ (Core.fieldTerm $ var "field") $
      "encodedUnion" <~ (toJson @@ var "fterm") $
      Eithers.map
        ("v" ~> Json.valueObject $ Maps.fromList $ list [pair (var "fname") (var "v")])
        (var "encodedUnion"),

    -- Unit
    _Term_unit>>: constant $ right $ Json.valueObject $ Maps.empty,

    -- Wrapped terms (transparent)
    _Term_wrap>>: "wt" ~> toJson @@ (Core.wrappedTermBody $ var "wt"),

    -- Maps -> array of {\"@key\": k, \"@value\": v}
    _Term_map>>: "m" ~>
      "encodeEntry" <~ ("kv" ~>
        "k" <~ (Pairs.first $ var "kv") $
        "v" <~ (Pairs.second $ var "kv") $
        "encodedK" <~ (toJson @@ var "k") $
        "encodedV" <~ (toJson @@ var "v") $
        -- Using Eithers.either to flatten the nested Either
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
      "encodedFirst" <~ (toJson @@ var "first") $
      "encodedSecond" <~ (toJson @@ var "second") $
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
          "encodedL" <~ (toJson @@ var "l") $
          Eithers.map
            ("v" ~> Json.valueObject $ Maps.fromList $ list [pair (string "@left") (var "v")])
            (var "encodedL"))
        ("r" ~>
          "encodedR" <~ (toJson @@ var "r") $
          Eithers.map
            ("v" ~> Json.valueObject $ Maps.fromList $ list [pair (string "@right") (var "v")])
            (var "encodedR"))
        (var "e")]

-- | Encode a literal value to JSON
encodeLiteral :: TBinding (Literal -> Either String Value)
encodeLiteral = define "encodeLiteral" $
  doc "Encode a Hydra literal to a JSON value" $
  "lit" ~> cases _Literal (var "lit") Nothing [
    _Literal_binary>>: "b" ~> right $ Json.valueString $ Literals.binaryToString $ var "b",
    _Literal_boolean>>: "b" ~> right $ Json.valueBoolean $ var "b",
    _Literal_float>>: "f" ~> encodeFloat @@ var "f",
    _Literal_integer>>: "i" ~> encodeInteger @@ var "i",
    _Literal_string>>: "s" ~> right $ Json.valueString $ var "s"]

-- | Encode a float value to JSON
-- Float64 and Bigfloat use native JSON numbers; Float32 uses string to preserve exact precision
encodeFloat :: TBinding (FloatValue -> Either String Value)
encodeFloat = define "encodeFloat" $
  doc "Encode a float value to JSON. Float64/Bigfloat use native numbers; Float32 uses string." $
  "fv" ~> cases _FloatValue (var "fv") Nothing [
    _FloatValue_bigfloat>>: "bf" ~> right $ Json.valueNumber $ var "bf",
    _FloatValue_float32>>: "f" ~> right $ Json.valueString $ Literals.showFloat32 $ var "f",
    _FloatValue_float64>>: "f" ~> right $ Json.valueNumber $ Literals.float64ToBigfloat $ var "f"]

-- | Encode an integer value to JSON
-- Small integers (int8, int16, int32, uint8, uint16) use native JSON numbers
-- Large integers (int64, uint32, uint64, bigint) use strings to preserve precision
encodeInteger :: TBinding (IntegerValue -> Either String Value)
encodeInteger = define "encodeInteger" $
  doc "Encode an integer value to JSON. Small ints use native numbers; large ints use strings." $
  "iv" ~> cases _IntegerValue (var "iv") Nothing [
    -- Large integers: use strings to preserve precision
    _IntegerValue_bigint>>: "bi" ~> right $ Json.valueString $ Literals.showBigint $ var "bi",
    _IntegerValue_int64>>: "i" ~> right $ Json.valueString $ Literals.showInt64 $ var "i",
    _IntegerValue_uint32>>: "i" ~> right $ Json.valueString $ Literals.showUint32 $ var "i",
    _IntegerValue_uint64>>: "i" ~> right $ Json.valueString $ Literals.showUint64 $ var "i",
    -- Small integers: use native JSON numbers (convert to bigfloat for JSON)
    _IntegerValue_int8>>: "i" ~> right $ Json.valueNumber $ Literals.bigintToBigfloat $ Literals.int8ToBigint $ var "i",
    _IntegerValue_int16>>: "i" ~> right $ Json.valueNumber $ Literals.bigintToBigfloat $ Literals.int16ToBigint $ var "i",
    _IntegerValue_int32>>: "i" ~> right $ Json.valueNumber $ Literals.bigintToBigfloat $ Literals.int32ToBigint $ var "i",
    _IntegerValue_uint8>>: "i" ~> right $ Json.valueNumber $ Literals.bigintToBigfloat $ Literals.uint8ToBigint $ var "i",
    _IntegerValue_uint16>>: "i" ~> right $ Json.valueNumber $ Literals.bigintToBigfloat $ Literals.uint16ToBigint $ var "i"]
