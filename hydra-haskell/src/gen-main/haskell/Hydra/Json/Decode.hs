-- Note: this is an automatically generated file. Do not edit.

-- | JSON decoding for Hydra terms. Converts JSON Values to Terms using Either for error handling.

module Hydra.Json.Decode where

import qualified Hydra.Core as Core
import qualified Hydra.Json as Json
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Show.Core as Core_
import Prelude hiding  (decodeFloat, Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Decode a JSON value to a Hydra term given a type. Returns Left for type mismatches.
fromJson :: (M.Map Core.Name Core.Type -> Core.Type -> Json.Value -> Either String Core.Term)
fromJson types typ value =  
  let stripped = (Rewriting.deannotateType typ)
  in ((\x -> case x of
    Core.TypeLiteral v1 -> (decodeLiteral v1 value)
    Core.TypeList v1 ->  
      let decodeElem = (\v -> fromJson types v1 v)
      in  
        let arrResult = (expectArray value)
        in (Eithers.either (\err -> Left err) (\arr ->  
          let decoded = (Eithers.mapList decodeElem arr)
          in (Eithers.map (\ts -> Core.TermList ts) decoded)) arrResult)
    Core.TypeSet v1 ->  
      let decodeElem = (\v -> fromJson types v1 v)
      in  
        let arrResult = (expectArray value)
        in (Eithers.either (\err -> Left err) (\arr ->  
          let decoded = (Eithers.mapList decodeElem arr)
          in (Eithers.map (\elems -> Core.TermSet (Sets.fromList elems)) decoded)) arrResult)
    Core.TypeMaybe v1 -> ((\x -> case x of
      Json.ValueNull -> (Right (Core.TermMaybe Nothing))
      Json.ValueArray v2 -> (Logic.ifElse (Equality.equal (Lists.length v2) 0) (Right (Core.TermMaybe Nothing)) (Logic.ifElse (Equality.equal (Lists.length v2) 1) ( 
        let decoded = (fromJson types v1 (Lists.head v2))
        in (Eithers.map (\v -> Core.TermMaybe (Just v)) decoded)) (Left "expected single-element array for Just")))
      _ -> (Left "expected null or single-element array for Maybe")) value)
    Core.TypeRecord v1 ->  
      let objResult = (expectObject value)
      in (Eithers.either (\err -> Left err) (\obj ->  
        let decodeField = (\ft ->  
                let fname = (Core.fieldTypeName ft)
                in  
                  let ftype = (Core.fieldTypeType ft)
                  in  
                    let mval = (Maps.lookup (Core.unName fname) obj)
                    in  
                      let defaultVal = Json.ValueNull
                      in  
                        let jsonVal = (Maybes.fromMaybe defaultVal mval)
                        in  
                          let decoded = (fromJson types ftype jsonVal)
                          in (Eithers.map (\v -> Core.Field {
                            Core.fieldName = fname,
                            Core.fieldTerm = v}) decoded))
        in  
          let fields = (Core.rowTypeFields v1)
          in  
            let decodedFields = (Eithers.mapList decodeField fields)
            in (Eithers.map (\fs -> Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.rowTypeTypeName v1),
              Core.recordFields = fs})) decodedFields)) objResult)
    Core.TypeUnion v1 ->  
      let objResult = (expectObject value)
      in (Eithers.either (\err -> Left err) (\obj ->  
        let keys = (Maps.keys obj)
        in (Logic.ifElse (Equality.equal (Lists.length keys) 1) ( 
          let key = (Lists.head keys)
          in  
            let val = (Maps.lookup key obj)
            in  
              let fields = (Core.rowTypeFields v1)
              in  
                let findField = (\fts -> Logic.ifElse (Lists.null fts) (Left (Strings.cat [
                        "unknown variant: ",
                        key])) ( 
                        let ft = (Lists.head fts)
                        in (Logic.ifElse (Equality.equal (Core.unName (Core.fieldTypeName ft)) key) ( 
                          let ftype = (Core.fieldTypeType ft)
                          in  
                            let jsonVal = (Maybes.fromMaybe Json.ValueNull val)
                            in  
                              let decoded = (fromJson types ftype jsonVal)
                              in (Eithers.map (\v -> Core.TermUnion (Core.Injection {
                                Core.injectionTypeName = (Core.rowTypeTypeName v1),
                                Core.injectionField = Core.Field {
                                  Core.fieldName = (Core.Name key),
                                  Core.fieldTerm = v}})) decoded)) (findField (Lists.tail fts)))))
                in (findField fields)) (Left "expected single-key object for union"))) objResult)
    Core.TypeUnit ->  
      let objResult = (expectObject value)
      in (Eithers.map (\_ -> Core.TermUnit) objResult)
    Core.TypeWrap v1 ->  
      let innerType = (Maps.lookup (Core.wrappedTypeTypeName v1) types)
      in (Maybes.maybe (Left (Strings.cat [
        "unknown wrapped type: ",
        (Core.unName (Core.wrappedTypeTypeName v1))])) (\it ->  
        let decoded = (fromJson types it value)
        in (Eithers.map (\v -> Core.TermWrap (Core.WrappedTerm {
          Core.wrappedTermTypeName = (Core.wrappedTypeTypeName v1),
          Core.wrappedTermBody = v})) decoded)) innerType)
    Core.TypeMap v1 ->  
      let keyType = (Core.mapTypeKeys v1)
      in  
        let valType = (Core.mapTypeValues v1)
        in  
          let arrResult = (expectArray value)
          in (Eithers.either (\err -> Left err) (\arr ->  
            let decodeEntry = (\entryJson ->  
                    let objResult = (expectObject entryJson)
                    in (Eithers.either (\err -> Left err) (\entryObj ->  
                      let keyJson = (Maps.lookup "@key" entryObj)
                      in  
                        let valJson = (Maps.lookup "@value" entryObj)
                        in (Maybes.maybe (Left "missing @key in map entry") (\kj -> Maybes.maybe (Left "missing @value in map entry") (\vj ->  
                          let decodedKey = (fromJson types keyType kj)
                          in  
                            let decodedVal = (fromJson types valType vj)
                            in (Eithers.either (\err -> Left err) (\k -> Eithers.map (\v -> (k, v)) decodedVal) decodedKey)) valJson) keyJson)) objResult))
            in  
              let entries = (Eithers.mapList decodeEntry arr)
              in (Eithers.map (\es -> Core.TermMap (Maps.fromList es)) entries)) arrResult)
    Core.TypePair v1 ->  
      let firstType = (Core.pairTypeFirst v1)
      in  
        let secondType = (Core.pairTypeSecond v1)
        in  
          let objResult = (expectObject value)
          in (Eithers.either (\err -> Left err) (\obj ->  
            let firstJson = (Maps.lookup "@first" obj)
            in  
              let secondJson = (Maps.lookup "@second" obj)
              in (Maybes.maybe (Left "missing @first in pair") (\fj -> Maybes.maybe (Left "missing @second in pair") (\sj ->  
                let decodedFirst = (fromJson types firstType fj)
                in  
                  let decodedSecond = (fromJson types secondType sj)
                  in (Eithers.either (\err -> Left err) (\f -> Eithers.map (\s -> Core.TermPair (f, s)) decodedSecond) decodedFirst)) secondJson) firstJson)) objResult)
    Core.TypeEither v1 ->  
      let leftType = (Core.eitherTypeLeft v1)
      in  
        let rightType = (Core.eitherTypeRight v1)
        in  
          let objResult = (expectObject value)
          in (Eithers.either (\err -> Left err) (\obj ->  
            let leftJson = (Maps.lookup "@left" obj)
            in  
              let rightJson = (Maps.lookup "@right" obj)
              in (Maybes.maybe (Maybes.maybe (Left "expected @left or @right in Either") (\rj ->  
                let decoded = (fromJson types rightType rj)
                in (Eithers.map (\v -> Core.TermEither (Right v)) decoded)) rightJson) (\lj ->  
                let decoded = (fromJson types leftType lj)
                in (Eithers.map (\v -> Core.TermEither (Left v)) decoded)) leftJson)) objResult)
    _ -> (Left (Strings.cat [
      "unsupported type for JSON decoding: ",
      (Core_.type_ typ)]))) stripped)

-- | Decode a JSON value to a literal term
decodeLiteral :: (Core.LiteralType -> Json.Value -> Either String Core.Term)
decodeLiteral lt value = ((\x -> case x of
  Core.LiteralTypeBinary ->  
    let strResult = (expectString value)
    in (Eithers.map (\s -> Core.TermLiteral (Core.LiteralBinary (Literals.stringToBinary s))) strResult)
  Core.LiteralTypeBoolean -> ((\x -> case x of
    Json.ValueBoolean v2 -> (Right (Core.TermLiteral (Core.LiteralBoolean v2)))
    _ -> (Left "expected boolean")) value)
  Core.LiteralTypeFloat v1 -> (decodeFloat v1 value)
  Core.LiteralTypeInteger v1 -> (decodeInteger v1 value)
  Core.LiteralTypeString ->  
    let strResult = (expectString value)
    in (Eithers.map (\s -> Core.TermLiteral (Core.LiteralString s)) strResult)) lt)

-- | Decode a JSON value to a float term. Float64/Bigfloat from numbers; Float32 from string.
decodeFloat :: (Core.FloatType -> Json.Value -> Either String Core.Term)
decodeFloat ft value = ((\x -> case x of
  Core.FloatTypeBigfloat ->  
    let numResult = (expectNumber value)
    in (Eithers.map (\n -> Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat n))) numResult)
  Core.FloatTypeFloat32 ->  
    let strResult = (expectString value)
    in (Eithers.either (\err -> Left err) (\s ->  
      let parsed = (Literals.readFloat32 s)
      in (Maybes.maybe (Left (Strings.cat [
        "invalid float32: ",
        s])) (\v -> Right (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 v)))) parsed)) strResult)
  Core.FloatTypeFloat64 ->  
    let numResult = (expectNumber value)
    in (Eithers.map (\n -> Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (Literals.bigfloatToFloat64 n)))) numResult)) ft)

-- | Decode a JSON value to an integer term. Small ints from numbers; large ints from strings.
decodeInteger :: (Core.IntegerType -> Json.Value -> Either String Core.Term)
decodeInteger it value = ((\x -> case x of
  Core.IntegerTypeBigint ->  
    let strResult = (expectString value)
    in (Eithers.either (\err -> Left err) (\s ->  
      let parsed = (Literals.readBigint s)
      in (Maybes.maybe (Left (Strings.cat [
        "invalid bigint: ",
        s])) (\v -> Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint v)))) parsed)) strResult)
  Core.IntegerTypeInt64 ->  
    let strResult = (expectString value)
    in (Eithers.either (\err -> Left err) (\s ->  
      let parsed = (Literals.readInt64 s)
      in (Maybes.maybe (Left (Strings.cat [
        "invalid int64: ",
        s])) (\v -> Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 v)))) parsed)) strResult)
  Core.IntegerTypeUint32 ->  
    let strResult = (expectString value)
    in (Eithers.either (\err -> Left err) (\s ->  
      let parsed = (Literals.readUint32 s)
      in (Maybes.maybe (Left (Strings.cat [
        "invalid uint32: ",
        s])) (\v -> Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint32 v)))) parsed)) strResult)
  Core.IntegerTypeUint64 ->  
    let strResult = (expectString value)
    in (Eithers.either (\err -> Left err) (\s ->  
      let parsed = (Literals.readUint64 s)
      in (Maybes.maybe (Left (Strings.cat [
        "invalid uint64: ",
        s])) (\v -> Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint64 v)))) parsed)) strResult)
  Core.IntegerTypeInt8 ->  
    let numResult = (expectNumber value)
    in (Eithers.map (\n -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 (Literals.bigintToInt8 (Literals.bigfloatToBigint n))))) numResult)
  Core.IntegerTypeInt16 ->  
    let numResult = (expectNumber value)
    in (Eithers.map (\n -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt16 (Literals.bigintToInt16 (Literals.bigfloatToBigint n))))) numResult)
  Core.IntegerTypeInt32 ->  
    let numResult = (expectNumber value)
    in (Eithers.map (\n -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Literals.bigintToInt32 (Literals.bigfloatToBigint n))))) numResult)
  Core.IntegerTypeUint8 ->  
    let numResult = (expectNumber value)
    in (Eithers.map (\n -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint8 (Literals.bigintToUint8 (Literals.bigfloatToBigint n))))) numResult)
  Core.IntegerTypeUint16 ->  
    let numResult = (expectNumber value)
    in (Eithers.map (\n -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint16 (Literals.bigintToUint16 (Literals.bigfloatToBigint n))))) numResult)) it)

-- | Extract a string from a JSON value
expectString :: (Json.Value -> Either String String)
expectString value = ((\x -> case x of
  Json.ValueString v1 -> (Right v1)
  _ -> (Left "expected string")) value)

-- | Extract an array from a JSON value
expectArray :: (Json.Value -> Either String [Json.Value])
expectArray value = ((\x -> case x of
  Json.ValueArray v1 -> (Right v1)
  _ -> (Left "expected array")) value)

-- | Extract an object from a JSON value
expectObject :: (Json.Value -> Either String (M.Map String Json.Value))
expectObject value = ((\x -> case x of
  Json.ValueObject v1 -> (Right v1)
  _ -> (Left "expected object")) value)

-- | Extract a number from a JSON value
expectNumber :: (Json.Value -> Either String Double)
expectNumber value = ((\x -> case x of
  Json.ValueNumber v1 -> (Right v1)
  _ -> (Left "expected number")) value)
