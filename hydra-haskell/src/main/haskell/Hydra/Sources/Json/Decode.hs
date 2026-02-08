
module Hydra.Sources.Json.Decode where

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
import qualified Hydra.Dsl.Meta.Lib.Flows                  as Flows
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
import           Prelude hiding ((++), decodeFloat)
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import Hydra.Json.Model


ns :: Namespace
ns = Namespace "hydra.json.decode"

define :: String -> TTerm a -> TBinding a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
    [Rewriting.ns, moduleNamespace Literals.module_, moduleNamespace ExtractCore.module_]
    KernelTypes.kernelTypesNamespaces $
    Just "JSON decoding for Hydra terms. Converts JSON Values to Terms using Either for error handling."
  where
    elements = [
      toBinding fromJson,
      toBinding decodeLiteral,
      toBinding decodeFloat,
      toBinding decodeInteger,
      toBinding expectString,
      toBinding expectArray,
      toBinding expectObject,
      toBinding expectNumber]

-- | Decode a JSON Value to a Term given a Type and type lookup table.
-- Returns Left with an error message for type mismatches or invalid JSON.
fromJson :: TBinding (M.Map Name Type -> Type -> Value -> Either String Term)
fromJson = define "fromJson" $
  doc "Decode a JSON value to a Hydra term given a type. Returns Left for type mismatches." $
  "types" ~> "typ" ~> "value" ~>
  "stripped" <~ (Rewriting.deannotateType @@ var "typ") $
  cases _Type (var "stripped")
    (Just $ left $ Strings.cat $ list [
      string "unsupported type for JSON decoding: ",
      ShowCore.type_ @@ var "typ"]) [

    -- Literals
    _Type_literal>>: "lt" ~> decodeLiteral @@ var "lt" @@ var "value",

    -- Lists
    _Type_list>>: "elemType" ~>
      "decodeElem" <~ ("v" ~> fromJson @@ var "types" @@ var "elemType" @@ var "v") $
      "arrResult" <~ (expectArray @@ var "value") $
      Eithers.either_
        ("err" ~> left $ var "err")
        ("arr" ~>
          "decoded" <~ (Eithers.mapList (var "decodeElem") (var "arr")) $
          Eithers.map ("ts" ~> Core.termList $ var "ts") (var "decoded"))
        (var "arrResult"),

    -- Sets
    _Type_set>>: "elemType" ~>
      "decodeElem" <~ ("v" ~> fromJson @@ var "types" @@ var "elemType" @@ var "v") $
      "arrResult" <~ (expectArray @@ var "value") $
      Eithers.either_
        ("err" ~> left $ var "err")
        ("arr" ~>
          "decoded" <~ (Eithers.mapList (var "decodeElem") (var "arr")) $
          Eithers.map ("elems" ~> Core.termSet $ Sets.fromList $ var "elems") (var "decoded"))
        (var "arrResult"),

    -- Maybe (null -> Nothing, [v] -> Just v)
    _Type_maybe>>: "innerType" ~>
      -- Helper to decode Just case (single element array)
      "decodeJust" <~ ("arr" ~>
        Eithers.map ("v" ~> Core.termMaybe $ just $ var "v")
          (fromJson @@ var "types" @@ var "innerType" @@ (Lists.head $ var "arr"))) $
      -- Decode array based on length: 0 -> Nothing, 1 -> Just, else error
      "decodeMaybeArray" <~ ("arr" ~>
        "len" <~ (Lists.length $ var "arr") $
        Logic.ifElse (Equality.equal (var "len") (int32 0))
          (right $ Core.termMaybe nothing)
          (Logic.ifElse (Equality.equal (var "len") (int32 1))
            (var "decodeJust" @@ var "arr")
            (left $ string "expected single-element array for Just"))) $
      cases _Value (var "value")
        (Just $ left $ string "expected null or single-element array for Maybe") [
        _Value_null>>: constant $ right $ Core.termMaybe nothing,
        _Value_array>>: "arr" ~> var "decodeMaybeArray" @@ var "arr"],

    -- Records
    _Type_record>>: "rt" ~>
      "objResult" <~ (expectObject @@ var "value") $
      Eithers.either_
        ("err" ~> left $ var "err")
        ("obj" ~>
          "decodeField" <~ ("ft" ~>
            "fname" <~ (Core.fieldTypeName $ var "ft") $
            "ftype" <~ (Core.fieldTypeType $ var "ft") $
            "mval" <~ (Maps.lookup (Core.unName $ var "fname") (var "obj")) $
            -- Use empty object as default for missing optional fields
            "defaultVal" <~ Json.valueNull $
            "jsonVal" <~ (Maybes.fromMaybe (var "defaultVal") (var "mval")) $
            "decoded" <~ (fromJson @@ var "types" @@ var "ftype" @@ var "jsonVal") $
            Eithers.map ("v" ~> Core.field (var "fname") (var "v")) (var "decoded")) $
          "fields" <~ (Core.rowTypeFields $ var "rt") $
          "decodedFields" <~ (Eithers.mapList (var "decodeField") (var "fields")) $
          Eithers.map
            ("fs" ~> Core.termRecord $ Core.record (Core.rowTypeTypeName $ var "rt") (var "fs"))
            (var "decodedFields"))
        (var "objResult"),

    -- Unions (single-key object)
    _Type_union>>: "rt" ~>
      -- Helper to decode a field once found
      "decodeVariant" <~ ("key" ~> "val" ~> "ftype" ~>
        "jsonVal" <~ (Maybes.fromMaybe Json.valueNull (var "val")) $
        "decoded" <~ (fromJson @@ var "types" @@ var "ftype" @@ var "jsonVal") $
        Eithers.map
          ("v" ~> Core.termUnion $ Core.injection
            (Core.rowTypeTypeName $ var "rt")
            (Core.field (Core.name $ var "key") (var "v")))
          (var "decoded")) $
      -- Helper to check if a field matches and decode it
      "tryField" <~ ("key" ~> "val" ~> "ft" ~>
        Logic.ifElse (Equality.equal (Core.unName $ Core.fieldTypeName $ var "ft") (var "key"))
          (just $ var "decodeVariant" @@ var "key" @@ var "val" @@ (Core.fieldTypeType $ var "ft"))
          nothing) $
      -- Find matching field and decode (uses tryField which returns Maybe, then either unwraps or recurses)
      "findAndDecode" <~ ("key" ~> "val" ~> "fts" ~>
        Logic.ifElse (Lists.null $ var "fts")
          (left $ Strings.cat $ list [string "unknown variant: ", var "key"])
          (Maybes.maybe
            (var "findAndDecode" @@ var "key" @@ var "val" @@ (Lists.tail $ var "fts"))
            ("r" ~> var "r")
            (var "tryField" @@ var "key" @@ var "val" @@ (Lists.head $ var "fts")))) $
      -- Helper to decode a single-key object
      "decodeSingleKey" <~ ("obj" ~>
        var "findAndDecode"
          @@ (Lists.head $ Maps.keys $ var "obj")
          @@ (Maps.lookup (Lists.head $ Maps.keys $ var "obj") (var "obj"))
          @@ (Core.rowTypeFields $ var "rt")) $
      -- Process the union object
      "processUnion" <~ ("obj" ~>
        Logic.ifElse (Equality.equal (Lists.length $ Maps.keys $ var "obj") (int32 1))
          (var "decodeSingleKey" @@ var "obj")
          (left $ string "expected single-key object for union")) $
      "objResult" <~ (expectObject @@ var "value") $
      Eithers.either_
        ("err" ~> left $ var "err")
        ("obj" ~> var "processUnion" @@ var "obj")
        (var "objResult"),

    -- Unit (empty object)
    _Type_unit>>: constant $
      "objResult" <~ (expectObject @@ var "value") $
      Eithers.map (constant Core.termUnit) (var "objResult"),

    -- Wrapped types (look up in type table and extract inner type if needed)
    _Type_wrap>>: "wn" ~>
      -- Extract inner type from a looked-up type (handles nested wraps)
      "extractInnerType" <~ ("lt" ~>
        cases _Type (var "lt") (Just $ var "lt") [
          _Type_wrap>>: "wt" ~> Core.wrappedTypeBody $ var "wt"]) $
      -- Decode using the inner type and wrap the result
      "decodeAndWrap" <~ ("lt" ~>
        "innerType" <~ (var "extractInnerType" @@ var "lt") $
        "decoded" <~ (fromJson @@ var "types" @@ var "innerType" @@ var "value") $
        Eithers.map
          ("v" ~> Core.termWrap $ Core.wrappedTerm (Core.wrappedTypeTypeName $ var "wn") (var "v"))
          (var "decoded")) $
      "lookedUp" <~ (Maps.lookup (Core.wrappedTypeTypeName $ var "wn") (var "types")) $
      Maybes.maybe
        (left $ Strings.cat $ list [
          string "unknown wrapped type: ",
          Core.unName $ Core.wrappedTypeTypeName $ var "wn"])
        ("lt" ~> var "decodeAndWrap" @@ var "lt")
        (var "lookedUp"),

    -- Map -> array of {@key, @value}
    _Type_map>>: "mt" ~>
      "keyType" <~ (Core.mapTypeKeys $ var "mt") $
      "valType" <~ (Core.mapTypeValues $ var "mt") $
      "arrResult" <~ (expectArray @@ var "value") $
      Eithers.either_
        ("err" ~> left $ var "err")
        ("arr" ~>
          "decodeEntry" <~ ("entryJson" ~>
            "objResult" <~ (expectObject @@ var "entryJson") $
            Eithers.either_
              ("err" ~> left $ var "err")
              ("entryObj" ~>
                "keyJson" <~ (Maps.lookup (string "@key") (var "entryObj")) $
                "valJson" <~ (Maps.lookup (string "@value") (var "entryObj")) $
                Maybes.maybe
                  (left $ string "missing @key in map entry")
                  ("kj" ~> Maybes.maybe
                    (left $ string "missing @value in map entry")
                    ("vj" ~>
                      "decodedKey" <~ (fromJson @@ var "types" @@ var "keyType" @@ var "kj") $
                      "decodedVal" <~ (fromJson @@ var "types" @@ var "valType" @@ var "vj") $
                      Eithers.either_
                        ("err" ~> left $ var "err")
                        ("k" ~> Eithers.map ("v" ~> pair (var "k") (var "v")) (var "decodedVal"))
                        (var "decodedKey"))
                    (var "valJson"))
                  (var "keyJson"))
              (var "objResult")) $
          "entries" <~ (Eithers.mapList (var "decodeEntry") (var "arr")) $
          Eithers.map ("es" ~> Core.termMap $ Maps.fromList $ var "es") (var "entries"))
        (var "arrResult"),

    -- Pair -> {@first, @second}
    _Type_pair>>: "pt" ~>
      "firstType" <~ (Core.pairTypeFirst $ var "pt") $
      "secondType" <~ (Core.pairTypeSecond $ var "pt") $
      "objResult" <~ (expectObject @@ var "value") $
      Eithers.either_
        ("err" ~> left $ var "err")
        ("obj" ~>
          "firstJson" <~ (Maps.lookup (string "@first") (var "obj")) $
          "secondJson" <~ (Maps.lookup (string "@second") (var "obj")) $
          Maybes.maybe
            (left $ string "missing @first in pair")
            ("fj" ~> Maybes.maybe
              (left $ string "missing @second in pair")
              ("sj" ~>
                "decodedFirst" <~ (fromJson @@ var "types" @@ var "firstType" @@ var "fj") $
                "decodedSecond" <~ (fromJson @@ var "types" @@ var "secondType" @@ var "sj") $
                Eithers.either_
                  ("err" ~> left $ var "err")
                  ("f" ~> Eithers.map ("s" ~> Core.termPair $ pair (var "f") (var "s")) (var "decodedSecond"))
                  (var "decodedFirst"))
              (var "secondJson"))
            (var "firstJson"))
        (var "objResult"),

    -- Either -> {@left} or {@right}
    _Type_either>>: "et" ~>
      "leftType" <~ (Core.eitherTypeLeft $ var "et") $
      "rightType" <~ (Core.eitherTypeRight $ var "et") $
      "objResult" <~ (expectObject @@ var "value") $
      Eithers.either_
        ("err" ~> left $ var "err")
        ("obj" ~>
          "leftJson" <~ (Maps.lookup (string "@left") (var "obj")) $
          "rightJson" <~ (Maps.lookup (string "@right") (var "obj")) $
          Maybes.maybe
            (Maybes.maybe
              (left $ string "expected @left or @right in Either")
              ("rj" ~>
                "decoded" <~ (fromJson @@ var "types" @@ var "rightType" @@ var "rj") $
                Eithers.map ("v" ~> Core.termEither $ right $ var "v") (var "decoded"))
              (var "rightJson"))
            ("lj" ~>
              "decoded" <~ (fromJson @@ var "types" @@ var "leftType" @@ var "lj") $
              Eithers.map ("v" ~> Core.termEither $ left $ var "v") (var "decoded"))
            (var "leftJson"))
        (var "objResult"),

    -- Type variables (look up in type table and recurse)
    _Type_variable>>: "name" ~>
      "lookedUp" <~ (Maps.lookup (var "name") (var "types")) $
      Maybes.maybe
        (left $ Strings.cat $ list [
          string "unknown type variable: ",
          Core.unName $ var "name"])
        ("resolvedType" ~> fromJson @@ var "types" @@ var "resolvedType" @@ var "value")
        (var "lookedUp")]

-- | Decode a JSON value to a literal term given a literal type
decodeLiteral :: TBinding (LiteralType -> Value -> Either String Term)
decodeLiteral = define "decodeLiteral" $
  doc "Decode a JSON value to a literal term" $
  "lt" ~> "value" ~>
  cases _LiteralType (var "lt") Nothing [
    _LiteralType_binary>>: constant $
      "strResult" <~ (expectString @@ var "value") $
      Eithers.map ("s" ~> Core.termLiteral $ Core.literalBinary $ Literals.stringToBinary $ var "s") (var "strResult"),

    _LiteralType_boolean>>: constant $
      cases _Value (var "value")
        (Just $ left $ string "expected boolean") [
        _Value_boolean>>: "b" ~> right $ Core.termLiteral $ Core.literalBoolean $ var "b"],

    _LiteralType_float>>: "ft" ~> decodeFloat @@ var "ft" @@ var "value",

    _LiteralType_integer>>: "it" ~> decodeInteger @@ var "it" @@ var "value",

    _LiteralType_string>>: constant $
      "strResult" <~ (expectString @@ var "value") $
      Eithers.map ("s" ~> Core.termLiteral $ Core.literalString $ var "s") (var "strResult")]

-- | Decode a JSON value to a float term
-- Float64 and Bigfloat are decoded from JSON numbers; Float32 from string
decodeFloat :: TBinding (FloatType -> Value -> Either String Term)
decodeFloat = define "decodeFloat" $
  doc "Decode a JSON value to a float term. Float64/Bigfloat from numbers; Float32 from string." $
  "ft" ~> "value" ~>
  cases _FloatType (var "ft") Nothing [
    -- Bigfloat: JSON number (Double) -> bigfloat
    _FloatType_bigfloat>>: constant $
      "numResult" <~ (expectNumber @@ var "value") $
      Eithers.map
        ("n" ~> Core.termLiteral $ Core.literalFloat $ Core.floatValueBigfloat $ var "n")
        (var "numResult"),
    -- Float32: JSON string -> parse as float32 (preserves exact precision)
    _FloatType_float32>>: constant $
      "strResult" <~ (expectString @@ var "value") $
      Eithers.either_
        ("err" ~> left $ var "err")
        ("s" ~>
          "parsed" <~ (Literals.readFloat32 $ var "s") $
          Maybes.maybe
            (left $ Strings.cat $ list [string "invalid float32: ", var "s"])
            ("v" ~> right $ Core.termLiteral $ Core.literalFloat $ Core.floatValueFloat32 $ var "v")
            (var "parsed"))
        (var "strResult"),
    -- Float64: JSON number (Double) -> float64
    _FloatType_float64>>: constant $
      "numResult" <~ (expectNumber @@ var "value") $
      Eithers.map
        ("n" ~> Core.termLiteral $ Core.literalFloat $ Core.floatValueFloat64 $ Literals.bigfloatToFloat64 $ var "n")
        (var "numResult")]

-- | Decode a JSON value to an integer term
-- Small integers (int8, int16, int32, uint8, uint16) are decoded from JSON numbers
-- Large integers (int64, uint32, uint64, bigint) are decoded from JSON strings
decodeInteger :: TBinding (IntegerType -> Value -> Either String Term)
decodeInteger = define "decodeInteger" $
  doc "Decode a JSON value to an integer term. Small ints from numbers; large ints from strings." $
  "it" ~> "value" ~>
  cases _IntegerType (var "it") Nothing [
    -- Large integers: decode from JSON string
    _IntegerType_bigint>>: constant $
      "strResult" <~ (expectString @@ var "value") $
      Eithers.either_
        ("err" ~> left $ var "err")
        ("s" ~>
          "parsed" <~ (Literals.readBigint $ var "s") $
          Maybes.maybe
            (left $ Strings.cat $ list [string "invalid bigint: ", var "s"])
            ("v" ~> right $ Core.termLiteral $ Core.literalInteger $ Core.integerValueBigint $ var "v")
            (var "parsed"))
        (var "strResult"),
    _IntegerType_int64>>: constant $
      "strResult" <~ (expectString @@ var "value") $
      Eithers.either_
        ("err" ~> left $ var "err")
        ("s" ~>
          "parsed" <~ (Literals.readInt64 $ var "s") $
          Maybes.maybe
            (left $ Strings.cat $ list [string "invalid int64: ", var "s"])
            ("v" ~> right $ Core.termLiteral $ Core.literalInteger $ Core.integerValueInt64 $ var "v")
            (var "parsed"))
        (var "strResult"),
    _IntegerType_uint32>>: constant $
      "strResult" <~ (expectString @@ var "value") $
      Eithers.either_
        ("err" ~> left $ var "err")
        ("s" ~>
          "parsed" <~ (Literals.readUint32 $ var "s") $
          Maybes.maybe
            (left $ Strings.cat $ list [string "invalid uint32: ", var "s"])
            ("v" ~> right $ Core.termLiteral $ Core.literalInteger $ Core.integerValueUint32 $ var "v")
            (var "parsed"))
        (var "strResult"),
    _IntegerType_uint64>>: constant $
      "strResult" <~ (expectString @@ var "value") $
      Eithers.either_
        ("err" ~> left $ var "err")
        ("s" ~>
          "parsed" <~ (Literals.readUint64 $ var "s") $
          Maybes.maybe
            (left $ Strings.cat $ list [string "invalid uint64: ", var "s"])
            ("v" ~> right $ Core.termLiteral $ Core.literalInteger $ Core.integerValueUint64 $ var "v")
            (var "parsed"))
        (var "strResult"),
    -- Small integers: decode from JSON number
    _IntegerType_int8>>: constant $
      "numResult" <~ (expectNumber @@ var "value") $
      Eithers.map
        ("n" ~> Core.termLiteral $ Core.literalInteger $ Core.integerValueInt8 $
          Literals.bigintToInt8 $ Literals.bigfloatToBigint $ var "n")
        (var "numResult"),
    _IntegerType_int16>>: constant $
      "numResult" <~ (expectNumber @@ var "value") $
      Eithers.map
        ("n" ~> Core.termLiteral $ Core.literalInteger $ Core.integerValueInt16 $
          Literals.bigintToInt16 $ Literals.bigfloatToBigint $ var "n")
        (var "numResult"),
    _IntegerType_int32>>: constant $
      "numResult" <~ (expectNumber @@ var "value") $
      Eithers.map
        ("n" ~> Core.termLiteral $ Core.literalInteger $ Core.integerValueInt32 $
          Literals.bigintToInt32 $ Literals.bigfloatToBigint $ var "n")
        (var "numResult"),
    _IntegerType_uint8>>: constant $
      "numResult" <~ (expectNumber @@ var "value") $
      Eithers.map
        ("n" ~> Core.termLiteral $ Core.literalInteger $ Core.integerValueUint8 $
          Literals.bigintToUint8 $ Literals.bigfloatToBigint $ var "n")
        (var "numResult"),
    _IntegerType_uint16>>: constant $
      "numResult" <~ (expectNumber @@ var "value") $
      Eithers.map
        ("n" ~> Core.termLiteral $ Core.literalInteger $ Core.integerValueUint16 $
          Literals.bigintToUint16 $ Literals.bigfloatToBigint $ var "n")
        (var "numResult")]

-- | Extract a string from a JSON value
expectString :: TBinding (Value -> Either String String)
expectString = define "expectString" $
  doc "Extract a string from a JSON value" $
  "value" ~> cases _Value (var "value")
    (Just $ left $ string "expected string") [
    _Value_string>>: "s" ~> right $ var "s"]

-- | Extract an array from a JSON value
expectArray :: TBinding (Value -> Either String [Value])
expectArray = define "expectArray" $
  doc "Extract an array from a JSON value" $
  "value" ~> cases _Value (var "value")
    (Just $ left $ string "expected array") [
    _Value_array>>: "arr" ~> right $ var "arr"]

-- | Extract an object from a JSON value
expectObject :: TBinding (Value -> Either String (M.Map String Value))
expectObject = define "expectObject" $
  doc "Extract an object from a JSON value" $
  "value" ~> cases _Value (var "value")
    (Just $ left $ string "expected object") [
    _Value_object>>: "obj" ~> right $ var "obj"]

-- | Extract a number from a JSON value
expectNumber :: TBinding (Value -> Either String Double)
expectNumber = define "expectNumber" $
  doc "Extract a number from a JSON value" $
  "value" ~> cases _Value (var "value")
    (Just $ left $ string "expected number") [
    _Value_number>>: "n" ~> right $ var "n"]
