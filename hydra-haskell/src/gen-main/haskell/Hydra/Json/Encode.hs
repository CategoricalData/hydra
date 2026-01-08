-- Note: this is an automatically generated file. Do not edit.

-- | JSON encoding for Hydra terms. Converts Terms to JSON Values using Either for error handling.

module Hydra.Json.Encode where

import qualified Hydra.Core as Core
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Show.Core as Core_
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Encode a Hydra term to a JSON value. Returns Left for unsupported constructs.
toJson :: (Core.Term -> Either String Model.Value)
toJson term =  
  let stripped = (Rewriting.deannotateTerm term)
  in ((\x -> case x of
    Core.TermLiteral v1 -> (encodeLiteral v1)
    Core.TermList v1 ->  
      let results = (Eithers.mapList (\t -> toJson t) v1)
      in (Eithers.map (\vs -> Model.ValueArray vs) results)
    Core.TermSet v1 ->  
      let terms = (Sets.toList v1)
      in  
        let results = (Eithers.mapList (\t -> toJson t) terms)
        in (Eithers.map (\vs -> Model.ValueArray vs) results)
    Core.TermMaybe v1 -> (Maybes.maybe (Right Model.ValueNull) (\v ->  
      let encodedMaybe = (toJson v)
      in (Eithers.map (\encoded -> Model.ValueArray [
        encoded]) encodedMaybe)) v1)
    Core.TermRecord v1 ->  
      let encodeField = (\f ->  
              let fname = (Core.unName (Core.fieldName f))
              in  
                let fterm = (Core.fieldTerm f)
                in  
                  let encodedField = (toJson fterm)
                  in (Eithers.map (\v -> (fname, v)) encodedField))
      in  
        let fields = (Core.recordFields v1)
        in  
          let encodedFields = (Eithers.mapList encodeField fields)
          in (Eithers.map (\fs -> Model.ValueObject (Maps.fromList fs)) encodedFields)
    Core.TermUnion v1 ->  
      let field = (Core.injectionField v1)
      in  
        let fname = (Core.unName (Core.fieldName field))
        in  
          let fterm = (Core.fieldTerm field)
          in  
            let encodedUnion = (toJson fterm)
            in (Eithers.map (\v -> Model.ValueObject (Maps.fromList [
              (fname, v)])) encodedUnion)
    Core.TermUnit -> (Right (Model.ValueObject Maps.empty))
    Core.TermWrap v1 -> (toJson (Core.wrappedTermBody v1))
    Core.TermMap v1 ->  
      let encodeEntry = (\kv ->  
              let k = (Pairs.first kv)
              in  
                let v = (Pairs.second kv)
                in  
                  let encodedK = (toJson k)
                  in  
                    let encodedV = (toJson v)
                    in (Eithers.either (\err -> Left err) (\ek -> Eithers.map (\ev -> Model.ValueObject (Maps.fromList [
                      ("@key", ek),
                      ("@value", ev)])) encodedV) encodedK))
      in  
        let entries = (Eithers.mapList encodeEntry (Maps.toList v1))
        in (Eithers.map (\es -> Model.ValueArray es) entries)
    Core.TermPair v1 ->  
      let first = (Pairs.first v1)
      in  
        let second = (Pairs.second v1)
        in  
          let encodedFirst = (toJson first)
          in  
            let encodedSecond = (toJson second)
            in (Eithers.either (\err -> Left err) (\ef -> Eithers.map (\es -> Model.ValueObject (Maps.fromList [
              ("@first", ef),
              ("@second", es)])) encodedSecond) encodedFirst)
    Core.TermEither v1 -> (Eithers.either (\l ->  
      let encodedL = (toJson l)
      in (Eithers.map (\v -> Model.ValueObject (Maps.fromList [
        ("@left", v)])) encodedL)) (\r ->  
      let encodedR = (toJson r)
      in (Eithers.map (\v -> Model.ValueObject (Maps.fromList [
        ("@right", v)])) encodedR)) v1)
    _ -> (Left (Strings.cat [
      "unsupported term variant for JSON encoding: ",
      (Core_.term term)]))) stripped)

encodeLiteral :: (Core.Literal -> Either t0 Model.Value)
encodeLiteral lit = ((\x -> case x of
  Core.LiteralBinary v1 -> (Right (Model.ValueString (Literals.binaryToString v1)))
  Core.LiteralBoolean v1 -> (Right (Model.ValueBoolean v1))
  Core.LiteralFloat v1 -> (encodeFloat v1)
  Core.LiteralInteger v1 -> (encodeInteger v1)
  Core.LiteralString v1 -> (Right (Model.ValueString v1))) lit)

encodeFloat :: (Core.FloatValue -> Either t0 Model.Value)
encodeFloat fv = ((\x -> case x of
  Core.FloatValueBigfloat v1 -> (Right (Model.ValueNumber v1))
  Core.FloatValueFloat32 v1 -> (Right (Model.ValueString (Literals.showFloat32 v1)))
  Core.FloatValueFloat64 v1 -> (Right (Model.ValueNumber (Literals.float64ToBigfloat v1)))) fv)

encodeInteger :: (Core.IntegerValue -> Either t0 Model.Value)
encodeInteger iv = ((\x -> case x of
  Core.IntegerValueBigint v1 -> (Right (Model.ValueString (Literals.showBigint v1)))
  Core.IntegerValueInt64 v1 -> (Right (Model.ValueString (Literals.showInt64 v1)))
  Core.IntegerValueUint32 v1 -> (Right (Model.ValueString (Literals.showUint32 v1)))
  Core.IntegerValueUint64 v1 -> (Right (Model.ValueString (Literals.showUint64 v1)))
  Core.IntegerValueInt8 v1 -> (Right (Model.ValueNumber (Literals.bigintToBigfloat (Literals.int8ToBigint v1))))
  Core.IntegerValueInt16 v1 -> (Right (Model.ValueNumber (Literals.bigintToBigfloat (Literals.int16ToBigint v1))))
  Core.IntegerValueInt32 v1 -> (Right (Model.ValueNumber (Literals.bigintToBigfloat (Literals.int32ToBigint v1))))
  Core.IntegerValueUint8 v1 -> (Right (Model.ValueNumber (Literals.bigintToBigfloat (Literals.uint8ToBigint v1))))
  Core.IntegerValueUint16 v1 -> (Right (Model.ValueNumber (Literals.bigintToBigfloat (Literals.uint16ToBigint v1))))) iv)
