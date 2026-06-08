-- Note: this is an automatically generated file. Do not edit.
-- | JSON decoding for Hydra terms. Converts JSON Values to Terms using Either for error handling.

module Hydra.Json.Decode where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Haskell.Lib.Equality as Equality
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Literals as LibLiterals
import qualified Hydra.Haskell.Lib.Logic as Logic
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Haskell.Lib.Sets as Sets
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Literals as Literals
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Strip as Strip
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
-- | Decode a JSON value to a float term. Finite values arrive as JSON numbers; NaN/Inf/-0.0 arrive as JSON string sentinels. Float32 and Float64 are symmetric.
decodeFloat :: Core.FloatType -> Model.Value -> Either String Core.Term
decodeFloat ft value =
    case ft of
      Core.FloatTypeFloat32 -> case value of
        Model.ValueNumber v1 -> Right (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 (LibLiterals.decimalToFloat32 v1))))
        Model.ValueString v1 -> Optionals.cases (parseSpecialFloat32 v1) (Left (Strings.cat [
          "invalid float32 sentinel: ",
          v1])) (\v -> Right (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 v))))
        _ -> Left "expected number or special float string for float32"
      Core.FloatTypeFloat64 -> case value of
        Model.ValueNumber v1 -> Right (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (LibLiterals.decimalToFloat64 v1))))
        Model.ValueString v1 -> Optionals.cases (parseSpecialFloat v1) (Left (Strings.cat [
          "invalid float64 sentinel: ",
          v1])) (\v -> Right (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 v))))
        _ -> Left "expected number or special float string for float64"
-- | Decode a JSON value to an integer term. Small ints from numbers; large ints from strings.
decodeInteger :: Core.IntegerType -> Model.Value -> Either String Core.Term
decodeInteger it value =
    case it of
      Core.IntegerTypeBigint ->
        let strResult = expectString value
        in (Eithers.either (\err -> Left err) (\s ->
          let parsed = LibLiterals.readBigint s
          in (Optionals.cases parsed (Left (Strings.cat [
            "invalid bigint: ",
            s])) (\v -> Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint v)))))) strResult)
      Core.IntegerTypeInt64 ->
        let strResult = expectString value
        in (Eithers.either (\err -> Left err) (\s ->
          let parsed = LibLiterals.readInt64 s
          in (Optionals.cases parsed (Left (Strings.cat [
            "invalid int64: ",
            s])) (\v -> Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 v)))))) strResult)
      Core.IntegerTypeUint64 ->
        let strResult = expectString value
        in (Eithers.either (\err -> Left err) (\s ->
          let parsed = LibLiterals.readUint64 s
          in (Optionals.cases parsed (Left (Strings.cat [
            "invalid uint64: ",
            s])) (\v -> Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint64 v)))))) strResult)
      Core.IntegerTypeInt8 ->
        let numResult = expectNumber value
        in (Eithers.map (\n -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 (LibLiterals.bigintToInt8 (LibLiterals.decimalToBigint n))))) numResult)
      Core.IntegerTypeInt16 ->
        let numResult = expectNumber value
        in (Eithers.map (\n -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt16 (LibLiterals.bigintToInt16 (LibLiterals.decimalToBigint n))))) numResult)
      Core.IntegerTypeInt32 ->
        let numResult = expectNumber value
        in (Eithers.map (\n -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (LibLiterals.bigintToInt32 (LibLiterals.decimalToBigint n))))) numResult)
      Core.IntegerTypeUint8 ->
        let numResult = expectNumber value
        in (Eithers.map (\n -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint8 (LibLiterals.bigintToUint8 (LibLiterals.decimalToBigint n))))) numResult)
      Core.IntegerTypeUint16 ->
        let numResult = expectNumber value
        in (Eithers.map (\n -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint16 (LibLiterals.bigintToUint16 (LibLiterals.decimalToBigint n))))) numResult)
      Core.IntegerTypeUint32 -> case value of
        Model.ValueNumber v1 -> Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint32 (LibLiterals.bigintToUint32 (LibLiterals.decimalToBigint v1)))))
        Model.ValueString v1 ->
          let parsed = LibLiterals.readUint32 v1
          in (Optionals.cases parsed (Left (Strings.cat [
            "invalid uint32: ",
            v1])) (\v -> Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint32 v)))))
        _ -> Left "expected number or string for uint32"
-- | Decode a JSON value to a literal term
decodeLiteral :: Core.LiteralType -> Model.Value -> Either String Core.Term
decodeLiteral lt value =
    case lt of
      Core.LiteralTypeBinary ->
        let strResult = expectString value
        in (Eithers.map (\s -> Core.TermLiteral (Core.LiteralBinary (LibLiterals.stringToBinary s))) strResult)
      Core.LiteralTypeBoolean -> case value of
        Model.ValueBoolean v1 -> Right (Core.TermLiteral (Core.LiteralBoolean v1))
        _ -> Left "expected boolean"
      Core.LiteralTypeDecimal -> case value of
        Model.ValueNumber v1 -> Right (Core.TermLiteral (Core.LiteralDecimal v1))
        _ -> Left "expected number for decimal"
      Core.LiteralTypeFloat v0 -> decodeFloat v0 value
      Core.LiteralTypeInteger v0 -> decodeInteger v0 value
      Core.LiteralTypeString ->
        let strResult = expectString value
        in (Eithers.map (\s -> Core.TermLiteral (Core.LiteralString s)) strResult)
-- | Extract an array from a JSON value
expectArray :: Model.Value -> Either String [Model.Value]
expectArray value =
    case value of
      Model.ValueArray v0 -> Right v0
      _ -> Left "expected array"
-- | Extract a number from a JSON value
expectNumber :: Model.Value -> Either String Sci.Scientific
expectNumber value =
    case value of
      Model.ValueNumber v0 -> Right v0
      _ -> Left "expected number"
-- | Extract an object from a JSON value
expectObject :: Model.Value -> Either String (M.Map String Model.Value)
expectObject value =
    case value of
      Model.ValueObject v0 -> Right v0
      _ -> Left "expected object"
-- | Extract a string from a JSON value
expectString :: Model.Value -> Either String String
expectString value =
    case value of
      Model.ValueString v0 -> Right v0
      _ -> Left "expected string"
-- | Decode a JSON value to a Hydra term given a type and type name. Returns Left for type mismatches.
fromJson :: M.Map Core.Name Core.Type -> Core.Name -> Core.Type -> Model.Value -> Either String Core.Term
fromJson types tname typ value =

      let stripped = Strip.deannotateType typ
      in case stripped of
        Core.TypeLiteral v0 -> decodeLiteral v0 value
        Core.TypeList v0 ->
          let decodeElem = \v -> fromJson types tname v0 v
              arrResult = expectArray value
          in (Eithers.either (\err -> Left err) (\arr ->
            let decoded = Eithers.mapList decodeElem arr
            in (Eithers.map (\ts -> Core.TermList ts) decoded)) arrResult)
        Core.TypeSet v0 ->
          let decodeElem = \v -> fromJson types tname v0 v
              arrResult = expectArray value
          in (Eithers.either (\err -> Left err) (\arr ->
            let decoded = Eithers.mapList decodeElem arr
            in (Eithers.map (\elems -> Core.TermSet (Sets.fromList elems)) decoded)) arrResult)
        Core.TypeOptional v0 ->
          let innerStripped = Strip.deannotateType v0
              isNestedMaybe =
                      case innerStripped of
                        Core.TypeOptional _ -> True
                        _ -> False
          in (Logic.ifElse isNestedMaybe (
            let decodeJust =
                    \arr -> Optionals.cases (Lists.maybeHead arr) (Left "expected single-element array for Just") (\firstVal -> Eithers.map (\v -> Core.TermOptional (Just v)) (fromJson types tname v0 firstVal))
                decodeMaybeArray =
                        \arr ->
                          let len = Lists.length arr
                          in (Logic.ifElse (Equality.equal len 0) (Right (Core.TermOptional Nothing)) (Logic.ifElse (Equality.equal len 1) (decodeJust arr) (Left "expected single-element array for Just")))
            in case value of
              Model.ValueNull -> Right (Core.TermOptional Nothing)
              Model.ValueArray v1 -> decodeMaybeArray v1
              _ -> Left "expected null or single-element array for nested Maybe") (case value of
            Model.ValueNull -> Right (Core.TermOptional Nothing)
            _ -> Eithers.map (\v -> Core.TermOptional (Just v)) (fromJson types tname v0 value)))
        Core.TypeRecord v0 ->
          let objResult = expectObject value
          in (Eithers.either (\err -> Left err) (\obj ->
            let decodeField =
                    \ft ->
                      let fname = Core.fieldTypeName ft
                          ftype = Core.fieldTypeType ft
                          mval = Maps.lookup (Core.unName fname) obj
                          defaultVal = Model.ValueNull
                          jsonVal = Optionals.fromOptional defaultVal mval
                          decoded = fromJson types tname ftype jsonVal
                      in (Eithers.map (\v -> Core.Field {
                        Core.fieldName = fname,
                        Core.fieldTerm = v}) decoded)
                decodedFields = Eithers.mapList decodeField v0
            in (Eithers.map (\fs -> Core.TermRecord (Core.Record {
              Core.recordTypeName = tname,
              Core.recordFields = fs})) decodedFields)) objResult)
        Core.TypeUnion v0 ->
          let decodeVariant =
                  \key -> \val -> \ftype ->
                    let jsonVal = Optionals.fromOptional Model.ValueNull val
                        decoded = fromJson types tname ftype jsonVal
                    in (Eithers.map (\v -> Core.TermInject (Core.Injection {
                      Core.injectionTypeName = tname,
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name key),
                        Core.fieldTerm = v}})) decoded)
              findAndDecode =
                      \key -> \val -> \fts -> Optionals.cases (Lists.find (\ft -> Equality.equal (Core.unName (Core.fieldTypeName ft)) key) fts) (Left (Strings.cat [
                        "unknown variant: ",
                        key])) (\ft -> decodeVariant key val (Core.fieldTypeType ft))
              decodeSingleKey =
                      \obj -> Optionals.cases (Lists.maybeHead (Maps.keys obj)) (Left "expected single-key object for union") (\k -> findAndDecode k (Maps.lookup k obj) v0)
              processUnion =
                      \obj -> Logic.ifElse (Equality.equal (Lists.length (Maps.keys obj)) 1) (decodeSingleKey obj) (Left "expected single-key object for union")
              objResult = expectObject value
          in (Eithers.either (\err -> Left err) (\obj -> processUnion obj) objResult)
        Core.TypeUnit ->
          let objResult = expectObject value
          in (Eithers.map (\_2 -> Core.TermUnit) objResult)
        Core.TypeWrap v0 ->
          let decoded = fromJson types tname v0 value
          in (Eithers.map (\v -> Core.TermWrap (Core.WrappedTerm {
            Core.wrappedTermTypeName = tname,
            Core.wrappedTermBody = v})) decoded)
        Core.TypeMap v0 ->
          let keyType = Core.mapTypeKeys v0
              valType = Core.mapTypeValues v0
              arrResult = expectArray value
          in (Eithers.either (\err -> Left err) (\arr ->
            let decodeEntry =
                    \entryJson ->
                      let objResult = expectObject entryJson
                      in (Eithers.either (\err -> Left err) (\entryObj ->
                        let keyJson = Maps.lookup "key" entryObj
                            valJson = Maps.lookup "value" entryObj
                        in (Optionals.cases keyJson (Left "missing key in map entry") (\kj -> Optionals.cases valJson (Left "missing value in map entry") (\vj ->
                          let decodedKey = fromJson types tname keyType kj
                              decodedVal = fromJson types tname valType vj
                          in (Eithers.either (\err -> Left err) (\k -> Eithers.map (\v -> (k, v)) decodedVal) decodedKey))))) objResult)
                entries = Eithers.mapList decodeEntry arr
            in (Eithers.map (\es -> Core.TermMap (Maps.fromList es)) entries)) arrResult)
        Core.TypePair v0 ->
          let firstType = Core.pairTypeFirst v0
              secondType = Core.pairTypeSecond v0
              objResult = expectObject value
          in (Eithers.either (\err -> Left err) (\obj ->
            let firstJson = Maps.lookup "first" obj
                secondJson = Maps.lookup "second" obj
            in (Optionals.cases firstJson (Left "missing first in pair") (\fj -> Optionals.cases secondJson (Left "missing second in pair") (\sj ->
              let decodedFirst = fromJson types tname firstType fj
                  decodedSecond = fromJson types tname secondType sj
              in (Eithers.either (\err -> Left err) (\f -> Eithers.map (\s -> Core.TermPair (f, s)) decodedSecond) decodedFirst))))) objResult)
        Core.TypeEither v0 ->
          let leftType = Core.eitherTypeLeft v0
              rightType = Core.eitherTypeRight v0
              objResult = expectObject value
          in (Eithers.either (\err -> Left err) (\obj ->
            let leftJson = Maps.lookup "left" obj
                rightJson = Maps.lookup "right" obj
            in (Optionals.cases leftJson (Optionals.cases rightJson (Left "expected left or right in Either") (\rj ->
              let decoded = fromJson types tname rightType rj
              in (Eithers.map (\v -> Core.TermEither (Right v)) decoded))) (\lj ->
              let decoded = fromJson types tname leftType lj
              in (Eithers.map (\v -> Core.TermEither (Left v)) decoded)))) objResult)
        Core.TypeVariable v0 ->
          let lookedUp = Maps.lookup v0 types
          in (Optionals.cases lookedUp (Left (Strings.cat [
            "unknown type variable: ",
            (Core.unName v0)])) (\resolvedType -> fromJson types v0 resolvedType value))
        _ -> Left (Strings.cat [
          "unsupported type for JSON decoding: ",
          (ShowCore.type_ typ)])
-- | Parse an IEEE sentinel string (NaN, Infinity, -Infinity, -0.0) to a float64. Returns Nothing for unrecognized strings.
parseSpecialFloat :: String -> Maybe Double
parseSpecialFloat s =
    Logic.ifElse (Logic.or (Equality.equal s "NaN") (Logic.or (Equality.equal s "Infinity") (Logic.or (Equality.equal s "-Infinity") (Equality.equal s "-0.0")))) (LibLiterals.readFloat64 s) Nothing
-- | Parse an IEEE sentinel string (NaN, Infinity, -Infinity, -0.0) to a float32. Returns Nothing for unrecognized strings.
parseSpecialFloat32 :: String -> Maybe Float
parseSpecialFloat32 s =
    Logic.ifElse (Logic.or (Equality.equal s "NaN") (Logic.or (Equality.equal s "Infinity") (Logic.or (Equality.equal s "-Infinity") (Equality.equal s "-0.0")))) (LibLiterals.readFloat32 s) Nothing
