-- Note: this is an automatically generated file. Do not edit.

-- | JSON encoding for Hydra terms. Converts Terms to JSON Values using Either for error handling.

module Hydra.Json.Encode where

import qualified Hydra.Core as Core
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Strip as Strip
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M

-- | Encode a float value to JSON. Bigfloat rejects anything the decimal space can't hold; Float64 uses string sentinels for NaN/Inf/-0.0; Float32 always strings.
encodeFloat :: Core.FloatValue -> Either String Model.Value
encodeFloat fv =
    case fv of
      Core.FloatValueBigfloat v0 ->
        let s = Literals.showBigfloat v0
        in (Logic.ifElse (requiresJsonStringSentinel s) (Left (Strings.cat [
          "JSON cannot represent bigfloat value: ",
          s])) (Right (Model.ValueNumber (Literals.float64ToDecimal (Literals.bigfloatToFloat64 v0)))))
      Core.FloatValueFloat32 v0 -> Right (Model.ValueString (Literals.showFloat32 v0))
      Core.FloatValueFloat64 v0 ->
        let s = Literals.showFloat64 v0
        in (Logic.ifElse (requiresJsonStringSentinel s) (Right (Model.ValueString s)) (Right (Model.ValueNumber (Literals.float64ToDecimal v0))))

-- | Encode an integer value to JSON. Small ints use native numbers; large ints use strings.
encodeInteger :: Core.IntegerValue -> Either t0 Model.Value
encodeInteger iv =
    case iv of
      Core.IntegerValueBigint v0 -> Right (Model.ValueString (Literals.showBigint v0))
      Core.IntegerValueInt64 v0 -> Right (Model.ValueString (Literals.showInt64 v0))
      Core.IntegerValueUint32 v0 -> Right (Model.ValueString (Literals.showUint32 v0))
      Core.IntegerValueUint64 v0 -> Right (Model.ValueString (Literals.showUint64 v0))
      Core.IntegerValueInt8 v0 -> Right (Model.ValueNumber (Literals.bigintToDecimal (Literals.int8ToBigint v0)))
      Core.IntegerValueInt16 v0 -> Right (Model.ValueNumber (Literals.bigintToDecimal (Literals.int16ToBigint v0)))
      Core.IntegerValueInt32 v0 -> Right (Model.ValueNumber (Literals.bigintToDecimal (Literals.int32ToBigint v0)))
      Core.IntegerValueUint8 v0 -> Right (Model.ValueNumber (Literals.bigintToDecimal (Literals.uint8ToBigint v0)))
      Core.IntegerValueUint16 v0 -> Right (Model.ValueNumber (Literals.bigintToDecimal (Literals.uint16ToBigint v0)))

-- | Encode a Hydra literal to a JSON value
encodeLiteral :: Core.Literal -> Either String Model.Value
encodeLiteral lit =
    case lit of
      Core.LiteralBinary v0 -> Right (Model.ValueString (Literals.binaryToString v0))
      Core.LiteralBoolean v0 -> Right (Model.ValueBoolean v0)
      Core.LiteralDecimal v0 -> Right (Model.ValueNumber v0)
      Core.LiteralFloat v0 -> encodeFloat v0
      Core.LiteralInteger v0 -> encodeInteger v0
      Core.LiteralString v0 -> Right (Model.ValueString v0)

-- | True for IEEE sentinel strings that JSON must escape as a string to preserve.
requiresJsonStringSentinel :: String -> Bool
requiresJsonStringSentinel s =
    Logic.or (Equality.equal s "NaN") (Logic.or (Equality.equal s "Infinity") (Logic.or (Equality.equal s "-Infinity") (Equality.equal s "-0.0")))

-- | Encode a Hydra term to a JSON value given a type and type name. Returns Left for unsupported constructs.
toJson :: M.Map Core.Name Core.Type -> Core.Name -> Core.Type -> Core.Term -> Either String Model.Value
toJson types tname typ term =

      let stripped = Strip.deannotateType typ
          strippedTerm = Strip.deannotateTerm term
      in case stripped of
        Core.TypeLiteral _ -> case strippedTerm of
          Core.TermLiteral v1 -> encodeLiteral v1
          _ -> Left "expected literal term"
        Core.TypeList v0 -> case strippedTerm of
          Core.TermList v1 ->
            let results = Eithers.mapList (\t -> toJson types tname v0 t) v1
            in (Eithers.map (\vs -> Model.ValueArray vs) results)
          _ -> Left "expected list term"
        Core.TypeSet v0 -> case strippedTerm of
          Core.TermSet v1 ->
            let terms = Sets.toList v1
                results = Eithers.mapList (\t -> toJson types tname v0 t) terms
            in (Eithers.map (\vs -> Model.ValueArray vs) results)
          _ -> Left "expected set term"
        Core.TypeMaybe v0 ->
          let innerStripped = Strip.deannotateType v0
              isNestedMaybe =
                      case innerStripped of
                        Core.TypeMaybe _ -> True
                        _ -> False
          in case strippedTerm of
            Core.TermMaybe v1 -> Maybes.maybe (Right Model.ValueNull) (\v ->
              let encoded = toJson types tname v0 v
              in (Logic.ifElse isNestedMaybe (Eithers.map (\ev -> Model.ValueArray [
                ev]) encoded) encoded)) v1
            _ -> Left "expected maybe term"
        Core.TypeRecord v0 -> case strippedTerm of
          Core.TermRecord v1 ->
            let isSimpleMaybe =
                    \ftype -> case (Strip.deannotateType ftype) of
                      Core.TypeMaybe v2 -> case (Strip.deannotateType v2) of
                        Core.TypeMaybe _ -> False
                        _ -> True
                      _ -> False
                encodeFieldWithType =
                        \ft -> \f ->
                          let fname = Core.unName (Core.fieldName f)
                              fterm = Core.fieldTerm f
                              ftype = Core.fieldTypeType ft
                          in (Logic.ifElse (isSimpleMaybe ftype) (case (Strip.deannotateTerm fterm) of
                            Core.TermMaybe v2 -> Maybes.maybe (Right Nothing) (\v ->
                              let innerType =
                                      case (Strip.deannotateType ftype) of
                                        Core.TypeMaybe v3 -> v3
                                        _ -> ftype
                                  encoded = toJson types tname innerType v
                              in (Eithers.map (\ev -> Just (fname, ev)) encoded)) v2
                            _ -> Left "expected maybe term for optional field") (
                            let encoded = toJson types tname ftype fterm
                            in (Eithers.map (\ev -> Just (fname, ev)) encoded)))
                fieldTypes = v0
                fields = Core.recordFields v1
                encodedPairs =
                        Eithers.mapList (\ftf -> encodeFieldWithType (Pairs.first ftf) (Pairs.second ftf)) (Lists.zip fieldTypes fields)
            in (Eithers.map (\pairs -> Model.ValueObject (Maps.fromList (Maybes.cat pairs))) encodedPairs)
          _ -> Left "expected record term"
        Core.TypeUnion v0 -> case strippedTerm of
          Core.TermInject v1 ->
            let field = Core.injectionField v1
                fname = Core.unName (Core.fieldName field)
                fterm = Core.fieldTerm field
                ftypeResult =
                        Maybes.maybe (Left (Strings.cat [
                          "unknown variant: ",
                          fname])) (\ft -> Right (Core.fieldTypeType ft)) (Lists.find (\ft -> Equality.equal (Core.unName (Core.fieldTypeName ft)) fname) v0)
            in (Eithers.either (\err -> Left err) (\ftype ->
              let encodedUnion = toJson types tname ftype fterm
              in (Eithers.map (\v -> Model.ValueObject (Maps.fromList [
                (fname, v)])) encodedUnion)) ftypeResult)
          _ -> Left "expected union term"
        Core.TypeUnit -> Right (Model.ValueObject Maps.empty)
        Core.TypeWrap v0 -> case strippedTerm of
          Core.TermWrap v1 -> toJson types tname v0 (Core.wrappedTermBody v1)
          _ -> Left "expected wrapped term"
        Core.TypeMap v0 ->
          let keyType = Core.mapTypeKeys v0
              valType = Core.mapTypeValues v0
          in case strippedTerm of
            Core.TermMap v1 ->
              let encodeEntry =
                      \kv ->
                        let k = Pairs.first kv
                            v = Pairs.second kv
                            encodedK = toJson types tname keyType k
                            encodedV = toJson types tname valType v
                        in (Eithers.either (\err -> Left err) (\ek -> Eithers.map (\ev -> Model.ValueObject (Maps.fromList [
                          ("@key", ek),
                          ("@value", ev)])) encodedV) encodedK)
                  entries = Eithers.mapList encodeEntry (Maps.toList v1)
              in (Eithers.map (\es -> Model.ValueArray es) entries)
            _ -> Left "expected map term"
        Core.TypePair v0 ->
          let firstType = Core.pairTypeFirst v0
              secondType = Core.pairTypeSecond v0
          in case strippedTerm of
            Core.TermPair v1 ->
              let first = Pairs.first v1
                  second = Pairs.second v1
                  encodedFirst = toJson types tname firstType first
                  encodedSecond = toJson types tname secondType second
              in (Eithers.either (\err -> Left err) (\ef -> Eithers.map (\es -> Model.ValueObject (Maps.fromList [
                ("@first", ef),
                ("@second", es)])) encodedSecond) encodedFirst)
            _ -> Left "expected pair term"
        Core.TypeEither v0 ->
          let leftType = Core.eitherTypeLeft v0
              rightType = Core.eitherTypeRight v0
          in case strippedTerm of
            Core.TermEither v1 -> Eithers.either (\l ->
              let encodedL = toJson types tname leftType l
              in (Eithers.map (\v -> Model.ValueObject (Maps.fromList [
                ("@left", v)])) encodedL)) (\r ->
              let encodedR = toJson types tname rightType r
              in (Eithers.map (\v -> Model.ValueObject (Maps.fromList [
                ("@right", v)])) encodedR)) v1
            _ -> Left "expected either term"
        Core.TypeVariable v0 ->
          let lookedUp = Maps.lookup v0 types
          in (Maybes.maybe (toJsonUntyped term) (\resolvedType -> toJson types v0 resolvedType term) lookedUp)
        _ -> Left (Strings.cat [
          "unsupported type for JSON encoding: ",
          (ShowCore.type_ typ)])

-- | Encode a Hydra term to a JSON value without type information. Falls back to array-wrapped Maybe encoding.
toJsonUntyped :: Core.Term -> Either String Model.Value
toJsonUntyped term =

      let stripped = Strip.deannotateTerm term
      in case stripped of
        Core.TermLiteral v0 -> encodeLiteral v0
        Core.TermList v0 ->
          let results = Eithers.mapList (\t -> toJsonUntyped t) v0
          in (Eithers.map (\vs -> Model.ValueArray vs) results)
        Core.TermSet v0 ->
          let terms = Sets.toList v0
              results = Eithers.mapList (\t -> toJsonUntyped t) terms
          in (Eithers.map (\vs -> Model.ValueArray vs) results)
        Core.TermMaybe v0 -> Maybes.maybe (Right Model.ValueNull) (\v ->
          let encodedMaybe = toJsonUntyped v
          in (Eithers.map (\encoded -> Model.ValueArray [
            encoded]) encodedMaybe)) v0
        Core.TermRecord v0 ->
          let encodeField =
                  \f ->
                    let fname = Core.unName (Core.fieldName f)
                        fterm = Core.fieldTerm f
                        encodedField = toJsonUntyped fterm
                    in (Eithers.map (\v -> (fname, v)) encodedField)
              fields = Core.recordFields v0
              encodedFields = Eithers.mapList encodeField fields
          in (Eithers.map (\fs -> Model.ValueObject (Maps.fromList fs)) encodedFields)
        Core.TermInject v0 ->
          let field = Core.injectionField v0
              fname = Core.unName (Core.fieldName field)
              fterm = Core.fieldTerm field
              encodedUnion = toJsonUntyped fterm
          in (Eithers.map (\v -> Model.ValueObject (Maps.fromList [
            (fname, v)])) encodedUnion)
        Core.TermUnit -> Right (Model.ValueObject Maps.empty)
        Core.TermWrap v0 -> toJsonUntyped (Core.wrappedTermBody v0)
        Core.TermMap v0 ->
          let encodeEntry =
                  \kv ->
                    let k = Pairs.first kv
                        v = Pairs.second kv
                        encodedK = toJsonUntyped k
                        encodedV = toJsonUntyped v
                    in (Eithers.either (\err -> Left err) (\ek -> Eithers.map (\ev -> Model.ValueObject (Maps.fromList [
                      ("@key", ek),
                      ("@value", ev)])) encodedV) encodedK)
              entries = Eithers.mapList encodeEntry (Maps.toList v0)
          in (Eithers.map (\es -> Model.ValueArray es) entries)
        Core.TermPair v0 ->
          let first = Pairs.first v0
              second = Pairs.second v0
              encodedFirst = toJsonUntyped first
              encodedSecond = toJsonUntyped second
          in (Eithers.either (\err -> Left err) (\ef -> Eithers.map (\es -> Model.ValueObject (Maps.fromList [
            ("@first", ef),
            ("@second", es)])) encodedSecond) encodedFirst)
        Core.TermEither v0 -> Eithers.either (\l ->
          let encodedL = toJsonUntyped l
          in (Eithers.map (\v -> Model.ValueObject (Maps.fromList [
            ("@left", v)])) encodedL)) (\r ->
          let encodedR = toJsonUntyped r
          in (Eithers.map (\v -> Model.ValueObject (Maps.fromList [
            ("@right", v)])) encodedR)) v0
        _ -> Left (Strings.cat [
          "unsupported term variant for JSON encoding: ",
          (ShowCore.term term)])
