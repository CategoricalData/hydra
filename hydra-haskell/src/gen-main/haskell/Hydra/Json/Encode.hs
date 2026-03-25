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

-- | Encode a float value to JSON. Float64/Bigfloat use native numbers; Float32 uses string.
encodeFloat :: Core.FloatValue -> Either t0 Model.Value
encodeFloat fv =
    case fv of
      Core.FloatValueBigfloat v0 -> Right (Model.ValueNumber v0)
      Core.FloatValueFloat32 v0 -> Right (Model.ValueString (Literals.showFloat32 v0))
      Core.FloatValueFloat64 v0 -> Right (Model.ValueNumber (Literals.float64ToBigfloat v0))

-- | Encode an integer value to JSON. Small ints use native numbers; large ints use strings.
encodeInteger :: Core.IntegerValue -> Either t0 Model.Value
encodeInteger iv =
    case iv of
      Core.IntegerValueBigint v0 -> Right (Model.ValueString (Literals.showBigint v0))
      Core.IntegerValueInt64 v0 -> Right (Model.ValueString (Literals.showInt64 v0))
      Core.IntegerValueUint32 v0 -> Right (Model.ValueString (Literals.showUint32 v0))
      Core.IntegerValueUint64 v0 -> Right (Model.ValueString (Literals.showUint64 v0))
      Core.IntegerValueInt8 v0 -> Right (Model.ValueNumber (Literals.bigintToBigfloat (Literals.int8ToBigint v0)))
      Core.IntegerValueInt16 v0 -> Right (Model.ValueNumber (Literals.bigintToBigfloat (Literals.int16ToBigint v0)))
      Core.IntegerValueInt32 v0 -> Right (Model.ValueNumber (Literals.bigintToBigfloat (Literals.int32ToBigint v0)))
      Core.IntegerValueUint8 v0 -> Right (Model.ValueNumber (Literals.bigintToBigfloat (Literals.uint8ToBigint v0)))
      Core.IntegerValueUint16 v0 -> Right (Model.ValueNumber (Literals.bigintToBigfloat (Literals.uint16ToBigint v0)))

-- | Encode a Hydra literal to a JSON value
encodeLiteral :: Core.Literal -> Either t0 Model.Value
encodeLiteral lit =
    case lit of
      Core.LiteralBinary v0 -> Right (Model.ValueString (Literals.binaryToString v0))
      Core.LiteralBoolean v0 -> Right (Model.ValueBoolean v0)
      Core.LiteralFloat v0 -> encodeFloat v0
      Core.LiteralInteger v0 -> encodeInteger v0
      Core.LiteralString v0 -> Right (Model.ValueString v0)

-- | Encode a Hydra term to a JSON value. Returns Left for unsupported constructs.
toJson :: Core.Term -> Either String Model.Value
toJson term =

      let stripped = Rewriting.deannotateTerm term
      in case stripped of
        Core.TermLiteral v0 -> encodeLiteral v0
        Core.TermList v0 ->
          let results = Eithers.mapList (\t -> toJson t) v0
          in (Eithers.map (\vs -> Model.ValueArray vs) results)
        Core.TermSet v0 ->
          let terms = Sets.toList v0
              results = Eithers.mapList (\t -> toJson t) terms
          in (Eithers.map (\vs -> Model.ValueArray vs) results)
        Core.TermMaybe v0 -> Maybes.maybe (Right Model.ValueNull) (\v ->
          let encodedMaybe = toJson v
          in (Eithers.map (\encoded -> Model.ValueArray [
            encoded]) encodedMaybe)) v0
        Core.TermRecord v0 ->
          let encodeField =
                  \f ->
                    let fname = Core.unName (Core.fieldName f)
                        fterm = Core.fieldTerm f
                        encodedField = toJson fterm
                    in (Eithers.map (\v -> (fname, v)) encodedField)
              fields = Core.recordFields v0
              encodedFields = Eithers.mapList encodeField fields
          in (Eithers.map (\fs -> Model.ValueObject (Maps.fromList fs)) encodedFields)
        Core.TermUnion v0 ->
          let field = Core.injectionField v0
              fname = Core.unName (Core.fieldName field)
              fterm = Core.fieldTerm field
              encodedUnion = toJson fterm
          in (Eithers.map (\v -> Model.ValueObject (Maps.fromList [
            (fname, v)])) encodedUnion)
        Core.TermUnit -> Right (Model.ValueObject Maps.empty)
        Core.TermWrap v0 -> toJson (Core.wrappedTermBody v0)
        Core.TermMap v0 ->
          let encodeEntry =
                  \kv ->
                    let k = Pairs.first kv
                        v = Pairs.second kv
                        encodedK = toJson k
                        encodedV = toJson v
                    in (Eithers.either (\err -> Left err) (\ek -> Eithers.map (\ev -> Model.ValueObject (Maps.fromList [
                      ("@key", ek),
                      ("@value", ev)])) encodedV) encodedK)
              entries = Eithers.mapList encodeEntry (Maps.toList v0)
          in (Eithers.map (\es -> Model.ValueArray es) entries)
        Core.TermPair v0 ->
          let first = Pairs.first v0
              second = Pairs.second v0
              encodedFirst = toJson first
              encodedSecond = toJson second
          in (Eithers.either (\err -> Left err) (\ef -> Eithers.map (\es -> Model.ValueObject (Maps.fromList [
            ("@first", ef),
            ("@second", es)])) encodedSecond) encodedFirst)
        Core.TermEither v0 -> Eithers.either (\l ->
          let encodedL = toJson l
          in (Eithers.map (\v -> Model.ValueObject (Maps.fromList [
            ("@left", v)])) encodedL)) (\r ->
          let encodedR = toJson r
          in (Eithers.map (\v -> Model.ValueObject (Maps.fromList [
            ("@right", v)])) encodedR)) v0
        _ -> Left (Strings.cat [
          "unsupported term variant for JSON encoding: ",
          (Core_.term term)])
