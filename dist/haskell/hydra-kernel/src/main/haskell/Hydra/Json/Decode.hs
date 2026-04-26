-- Note: this is an automatically generated file. Do not edit.
-- | JSON decoding for Hydra terms. Converts JSON Values to Terms using Either for error handling.

module Hydra.Json.Decode where
import qualified Hydra.Core as Core
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Strip as Strip
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
-- | Decode a JSON value to a float term. Numbers for Bigfloat/Float64; strings for Float32 and NaN/Inf sentinels.
decodeFloat :: Core.FloatType -> Model.Value -> Either String Core.Term
decodeFloat ft value =
    case ft of
      Core.FloatTypeBigfloat -> case value of
        Model.ValueNumber v1 -> Right (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat (Literals.float64ToBigfloat (Literals.decimalToFloat64 v1)))))
        Model.ValueString v1 -> Maybes.maybe (Left (Strings.cat [
          "invalid bigfloat sentinel: ",
          v1])) (\v -> Right (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat (Literals.float64ToBigfloat v))))) (parseSpecialFloat v1)
        _ -> Left "expected number or special float string for bigfloat"
      Core.FloatTypeFloat32 ->
        let strResult = expectString value
        in (Eithers.either (\err -> Left err) (\s ->
          let parsed = Literals.readFloat32 s
          in (Maybes.maybe (Left (Strings.cat [
            "invalid float32: ",
            s])) (\v -> Right (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 v)))) parsed)) strResult)
      Core.FloatTypeFloat64 -> case value of
        Model.ValueNumber v1 -> Right (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (Literals.decimalToFloat64 v1))))
        Model.ValueString v1 -> Maybes.maybe (Left (Strings.cat [
          "invalid float64 sentinel: ",
          v1])) (\v -> Right (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 v)))) (parseSpecialFloat v1)
        _ -> Left "expected number or special float string for float64"
-- | Decode a JSON value to an integer term. Small ints from numbers; large ints from strings.
decodeInteger :: Core.IntegerType -> Model.Value -> Either String Core.Term
decodeInteger it value =
    case it of
      Core.IntegerTypeBigint ->
        let strResult = expectString value
        in (Eithers.either (\err -> Left err) (\s ->
          let parsed = Literals.readBigint s
          in (Maybes.maybe (Left (Strings.cat [
            "invalid bigint: ",
            s])) (\v -> Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint v)))) parsed)) strResult)
      Core.IntegerTypeInt64 ->
        let strResult = expectString value
        in (Eithers.either (\err -> Left err) (\s ->
          let parsed = Literals.readInt64 s
          in (Maybes.maybe (Left (Strings.cat [
            "invalid int64: ",
            s])) (\v -> Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 v)))) parsed)) strResult)
      Core.IntegerTypeUint32 ->
        let strResult = expectString value
        in (Eithers.either (\err -> Left err) (\s ->
          let parsed = Literals.readUint32 s
          in (Maybes.maybe (Left (Strings.cat [
            "invalid uint32: ",
            s])) (\v -> Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint32 v)))) parsed)) strResult)
      Core.IntegerTypeUint64 ->
        let strResult = expectString value
        in (Eithers.either (\err -> Left err) (\s ->
          let parsed = Literals.readUint64 s
          in (Maybes.maybe (Left (Strings.cat [
            "invalid uint64: ",
            s])) (\v -> Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint64 v)))) parsed)) strResult)
      Core.IntegerTypeInt8 ->
        let numResult = expectNumber value
        in (Eithers.map (\n -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 (Literals.bigintToInt8 (Literals.decimalToBigint n))))) numResult)
      Core.IntegerTypeInt16 ->
        let numResult = expectNumber value
        in (Eithers.map (\n -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt16 (Literals.bigintToInt16 (Literals.decimalToBigint n))))) numResult)
      Core.IntegerTypeInt32 ->
        let numResult = expectNumber value
        in (Eithers.map (\n -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Literals.bigintToInt32 (Literals.decimalToBigint n))))) numResult)
      Core.IntegerTypeUint8 ->
        let numResult = expectNumber value
        in (Eithers.map (\n -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint8 (Literals.bigintToUint8 (Literals.decimalToBigint n))))) numResult)
      Core.IntegerTypeUint16 ->
        let numResult = expectNumber value
        in (Eithers.map (\n -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint16 (Literals.bigintToUint16 (Literals.decimalToBigint n))))) numResult)
-- | Decode a JSON value to a literal term
decodeLiteral :: Core.LiteralType -> Model.Value -> Either String Core.Term
decodeLiteral lt value =
    case lt of
      Core.LiteralTypeBinary ->
        let strResult = expectString value
        in (Eithers.map (\s -> Core.TermLiteral (Core.LiteralBinary (Literals.stringToBinary s))) strResult)
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
        Core.TypeMaybe v0 ->
          let innerStripped = Strip.deannotateType v0
              isNestedMaybe =
                      case innerStripped of
                        Core.TypeMaybe _ -> True
                        _ -> False
          in (Logic.ifElse isNestedMaybe (
            let decodeJust =
                    \arr -> Maybes.maybe (Left "expected single-element array for Just") (\firstVal -> Eithers.map (\v -> Core.TermMaybe (Just v)) (fromJson types tname v0 firstVal)) (Lists.maybeHead arr)
                decodeMaybeArray =
                        \arr ->
                          let len = Lists.length arr
                          in (Logic.ifElse (Equality.equal len 0) (Right (Core.TermMaybe Nothing)) (Logic.ifElse (Equality.equal len 1) (decodeJust arr) (Left "expected single-element array for Just")))
            in case value of
              Model.ValueNull -> Right (Core.TermMaybe Nothing)
              Model.ValueArray v1 -> decodeMaybeArray v1
              _ -> Left "expected null or single-element array for nested Maybe") (case value of
            Model.ValueNull -> Right (Core.TermMaybe Nothing)
            _ -> Eithers.map (\v -> Core.TermMaybe (Just v)) (fromJson types tname v0 value)))
        Core.TypeRecord v0 ->
          let objResult = expectObject value
          in (Eithers.either (\err -> Left err) (\obj ->
            let decodeField =
                    \ft ->
                      let fname = Core.fieldTypeName ft
                          ftype = Core.fieldTypeType ft
                          mval = Maps.lookup (Core.unName fname) obj
                          defaultVal = Model.ValueNull
                          jsonVal = Maybes.fromMaybe defaultVal mval
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
                    let jsonVal = Maybes.fromMaybe Model.ValueNull val
                        decoded = fromJson types tname ftype jsonVal
                    in (Eithers.map (\v -> Core.TermInject (Core.Injection {
                      Core.injectionTypeName = tname,
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name key),
                        Core.fieldTerm = v}})) decoded)
              findAndDecode =
                      \key -> \val -> \fts -> Maybes.maybe (Left (Strings.cat [
                        "unknown variant: ",
                        key])) (\ft -> decodeVariant key val (Core.fieldTypeType ft)) (Lists.find (\ft -> Equality.equal (Core.unName (Core.fieldTypeName ft)) key) fts)
              decodeSingleKey =
                      \obj -> Maybes.maybe (Left "expected single-key object for union") (\k -> findAndDecode k (Maps.lookup k obj) v0) (Lists.maybeHead (Maps.keys obj))
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
                        in (Maybes.maybe (Left "missing key in map entry") (\kj -> Maybes.maybe (Left "missing value in map entry") (\vj ->
                          let decodedKey = fromJson types tname keyType kj
                              decodedVal = fromJson types tname valType vj
                          in (Eithers.either (\err -> Left err) (\k -> Eithers.map (\v -> (k, v)) decodedVal) decodedKey)) valJson) keyJson)) objResult)
                entries = Eithers.mapList decodeEntry arr
            in (Eithers.map (\es -> Core.TermMap (Maps.fromList es)) entries)) arrResult)
        Core.TypePair v0 ->
          let firstType = Core.pairTypeFirst v0
              secondType = Core.pairTypeSecond v0
              objResult = expectObject value
          in (Eithers.either (\err -> Left err) (\obj ->
            let firstJson = Maps.lookup "first" obj
                secondJson = Maps.lookup "second" obj
            in (Maybes.maybe (Left "missing first in pair") (\fj -> Maybes.maybe (Left "missing second in pair") (\sj ->
              let decodedFirst = fromJson types tname firstType fj
                  decodedSecond = fromJson types tname secondType sj
              in (Eithers.either (\err -> Left err) (\f -> Eithers.map (\s -> Core.TermPair (f, s)) decodedSecond) decodedFirst)) secondJson) firstJson)) objResult)
        Core.TypeEither v0 ->
          let leftType = Core.eitherTypeLeft v0
              rightType = Core.eitherTypeRight v0
              objResult = expectObject value
          in (Eithers.either (\err -> Left err) (\obj ->
            let leftJson = Maps.lookup "left" obj
                rightJson = Maps.lookup "right" obj
            in (Maybes.maybe (Maybes.maybe (Left "expected left or right in Either") (\rj ->
              let decoded = fromJson types tname rightType rj
              in (Eithers.map (\v -> Core.TermEither (Right v)) decoded)) rightJson) (\lj ->
              let decoded = fromJson types tname leftType lj
              in (Eithers.map (\v -> Core.TermEither (Left v)) decoded)) leftJson)) objResult)
        Core.TypeVariable v0 ->
          let lookedUp = Maps.lookup v0 types
          in (Maybes.maybe (Left (Strings.cat [
            "unknown type variable: ",
            (Core.unName v0)])) (\resolvedType -> fromJson types v0 resolvedType value) lookedUp)
        _ -> Left (Strings.cat [
          "unsupported type for JSON decoding: ",
          (ShowCore.type_ typ)])
-- | Parse an IEEE sentinel string (NaN, Infinity, -Infinity, -0.0) to a float64. Returns Nothing for unrecognized strings.
parseSpecialFloat :: String -> Maybe Double
parseSpecialFloat s =
    Logic.ifElse (Logic.or (Equality.equal s "NaN") (Logic.or (Equality.equal s "Infinity") (Logic.or (Equality.equal s "-Infinity") (Equality.equal s "-0.0")))) (Literals.readFloat64 s) Nothing
