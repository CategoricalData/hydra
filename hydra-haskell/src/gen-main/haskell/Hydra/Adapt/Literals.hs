-- Note: this is an automatically generated file. Do not edit.

-- | Adapter framework for literal types and terms

module Hydra.Adapt.Literals where

import qualified Hydra.Adapt.Utils as Utils
import qualified Hydra.Coders as Coders
import qualified Hydra.Compute as Compute
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Reflect as Reflect
import qualified Hydra.Show.Core as Core__
import qualified Hydra.Util as Util
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Compare two precision values
comparePrecision :: (Util.Precision -> Util.Precision -> Util.Comparison)
comparePrecision p1 p2 = ((\x -> case x of
  Util.PrecisionArbitrary -> ((\x -> case x of
    Util.PrecisionArbitrary -> Util.ComparisonEqualTo
    Util.PrecisionBits _ -> Util.ComparisonGreaterThan) p2)
  Util.PrecisionBits v0 -> ((\x -> case x of
    Util.PrecisionArbitrary -> Util.ComparisonLessThan
    Util.PrecisionBits v1 -> (Logic.ifElse (Equality.lt v0 v1) Util.ComparisonLessThan Util.ComparisonGreaterThan)) p2)) p1)

-- | Convert a float value to a different float type
convertFloatValue :: (Core.FloatType -> Core.FloatValue -> Core.FloatValue)
convertFloatValue target fv =  
  let decoder = (\fv -> (\x -> case x of
          Core.FloatValueBigfloat v0 -> v0
          Core.FloatValueFloat32 v0 -> (Literals.float32ToBigfloat v0)
          Core.FloatValueFloat64 v0 -> (Literals.float64ToBigfloat v0)) fv)
  in  
    let encoder = (\d -> (\x -> case x of
            Core.FloatTypeBigfloat -> (Core.FloatValueBigfloat d)
            Core.FloatTypeFloat32 -> (Core.FloatValueFloat32 (Literals.bigfloatToFloat32 d))
            Core.FloatTypeFloat64 -> (Core.FloatValueFloat64 (Literals.bigfloatToFloat64 d))) target)
    in (encoder (decoder fv))

-- | Convert an integer value to a different integer type
convertIntegerValue :: (Core.IntegerType -> Core.IntegerValue -> Core.IntegerValue)
convertIntegerValue target iv =  
  let decoder = (\iv -> (\x -> case x of
          Core.IntegerValueBigint v0 -> v0
          Core.IntegerValueInt8 v0 -> (Literals.int8ToBigint v0)
          Core.IntegerValueInt16 v0 -> (Literals.int16ToBigint v0)
          Core.IntegerValueInt32 v0 -> (Literals.int32ToBigint v0)
          Core.IntegerValueInt64 v0 -> (Literals.int64ToBigint v0)
          Core.IntegerValueUint8 v0 -> (Literals.uint8ToBigint v0)
          Core.IntegerValueUint16 v0 -> (Literals.uint16ToBigint v0)
          Core.IntegerValueUint32 v0 -> (Literals.uint32ToBigint v0)
          Core.IntegerValueUint64 v0 -> (Literals.uint64ToBigint v0)) iv)
  in  
    let encoder = (\d -> (\x -> case x of
            Core.IntegerTypeBigint -> (Core.IntegerValueBigint d)
            Core.IntegerTypeInt8 -> (Core.IntegerValueInt8 (Literals.bigintToInt8 d))
            Core.IntegerTypeInt16 -> (Core.IntegerValueInt16 (Literals.bigintToInt16 d))
            Core.IntegerTypeInt32 -> (Core.IntegerValueInt32 (Literals.bigintToInt32 d))
            Core.IntegerTypeInt64 -> (Core.IntegerValueInt64 (Literals.bigintToInt64 d))
            Core.IntegerTypeUint8 -> (Core.IntegerValueUint8 (Literals.bigintToUint8 d))
            Core.IntegerTypeUint16 -> (Core.IntegerValueUint16 (Literals.bigintToUint16 d))
            Core.IntegerTypeUint32 -> (Core.IntegerValueUint32 (Literals.bigintToUint32 d))
            Core.IntegerTypeUint64 -> (Core.IntegerValueUint64 (Literals.bigintToUint64 d))) target)
    in (encoder (decoder iv))

-- | Generate a disclaimer message for type conversions
disclaimer :: (Bool -> String -> String -> String)
disclaimer lossy source target = (Strings.cat [
  "replace ",
  source,
  " with ",
  target,
  (Logic.ifElse lossy " (lossy)" "")])

-- | Create an adapter for literal types
literalAdapter :: (Coders.AdapterContext -> Core.LiteralType -> Either String (Compute.Adapter Core.LiteralType Core.LiteralType Core.Literal Core.Literal))
literalAdapter cx lt =  
  let forBinary = (\t ->  
          let matchBinary = (\_cx -> \lit -> (\x -> case x of
                  Core.LiteralBinary v0 -> (Right (Core.LiteralString (Literals.binaryToString v0)))) lit)
          in  
            let matchString = (\_cx -> \lit -> (\x -> case x of
                    Core.LiteralString v0 -> (Right (Core.LiteralBinary (Literals.stringToBinary v0)))) lit)
            in  
              let step = Compute.Coder {
                      Compute.coderEncode = matchBinary,
                      Compute.coderDecode = matchString}
              in (Right [
                Compute.Adapter {
                  Compute.adapterIsLossy = False,
                  Compute.adapterSource = t,
                  Compute.adapterTarget = Core.LiteralTypeString,
                  Compute.adapterCoder = step}]))
  in  
    let forBoolean = (\t ->  
            let matchBoolean = (\step_ -> \cx -> \lit -> (\x -> case x of
                    Core.LiteralBoolean v0 -> (Eithers.bind (Compute.coderEncode step_ cx (Core.IntegerValueUint8 (Logic.ifElse v0 1 0))) (\iv -> Right (Core.LiteralInteger iv)))) lit)
            in  
              let matchInteger = (\step_ -> \cx -> \lit ->  
                      let forValue = (\val -> (\x -> case x of
                              Core.IntegerValueUint8 v0 -> (Core.LiteralBoolean (Equality.equal v0 1))) val)
                      in ((\x -> case x of
                        Core.LiteralInteger v0 -> (Eithers.bind (Compute.coderDecode step_ cx v0) (\val -> Right (forValue val)))) lit))
              in  
                let constraints = (Coders.languageConstraints (Coders.adapterContextLanguage cx))
                in  
                  let hasIntegers = (Logic.not (Sets.null (Coders.languageConstraintsIntegerTypes constraints)))
                  in  
                    let hasStrings = (Sets.member Variants.LiteralVariantString (Coders.languageConstraintsLiteralVariants constraints))
                    in  
                      let withIntegers =  
                              let withAdapter = (\adapter ->  
                                      let step_ = (Compute.adapterCoder adapter)
                                      in  
                                        let step = Compute.Coder {
                                                Compute.coderEncode = (matchBoolean step_),
                                                Compute.coderDecode = (matchInteger step_)}
                                        in (Right [
                                          Compute.Adapter {
                                            Compute.adapterIsLossy = False,
                                            Compute.adapterSource = t,
                                            Compute.adapterTarget = (Core.LiteralTypeInteger (Compute.adapterTarget adapter)),
                                            Compute.adapterCoder = step}]))
                              in (Eithers.bind (integerAdapter cx Core.IntegerTypeUint8) (\adapter -> withAdapter adapter))
                      in  
                        let withStrings =  
                                let encode = (\cx -> \lit -> Eithers.bind (Core_.booleanLiteral cx lit) (\b -> Right (Core.LiteralString (Logic.ifElse b "true" "false"))))
                                in  
                                  let decode = (\cx -> \lit -> Eithers.bind (Core_.stringLiteral cx lit) (\s -> Logic.ifElse (Equality.equal s "true") (Right (Core.LiteralBoolean True)) (Logic.ifElse (Equality.equal s "false") (Right (Core.LiteralBoolean False)) (Left (Context.InContext {
                                          Context.inContextObject = (Error.OtherError (Strings.cat2 "expected boolean literal, found " s)),
                                          Context.inContextContext = cx})))))
                                  in [
                                    Compute.Adapter {
                                      Compute.adapterIsLossy = False,
                                      Compute.adapterSource = t,
                                      Compute.adapterTarget = Core.LiteralTypeString,
                                      Compute.adapterCoder = Compute.Coder {
                                        Compute.coderEncode = encode,
                                        Compute.coderDecode = decode}}]
                        in (Logic.ifElse hasIntegers withIntegers (Logic.ifElse hasStrings (Right withStrings) (Left "no alternatives available for boolean encoding"))))
    in  
      let forFloat = (\t -> \ft ->  
              let withFloats =  
                      let adapt = (\adapter -> \dir -> \cx -> \l -> (\x -> case x of
                              Core.LiteralFloat v0 -> (Eithers.map (\x -> Core.LiteralFloat x) (Utils.encodeDecode dir (Compute.adapterCoder adapter) cx v0))
                              _ -> (Left (Context.InContext {
                                Context.inContextObject = (Error.OtherError (Strings.cat2 "expected floating-point literal, found " (Core__.literal l))),
                                Context.inContextContext = cx}))) l)
                      in (Eithers.bind (floatAdapter cx ft) (\adapter ->  
                        let step = (Utils.bidirectional (adapt adapter))
                        in (Right [
                          Compute.Adapter {
                            Compute.adapterIsLossy = (Compute.adapterIsLossy adapter),
                            Compute.adapterSource = t,
                            Compute.adapterTarget = (Core.LiteralTypeFloat (Compute.adapterTarget adapter)),
                            Compute.adapterCoder = step}])))
              in  
                let constraints = (Coders.languageConstraints (Coders.adapterContextLanguage cx))
                in  
                  let hasFloats = (Logic.not (Sets.null (Coders.languageConstraintsFloatTypes constraints)))
                  in (Logic.ifElse hasFloats withFloats (Left "no float types available")))
      in  
        let forInteger = (\t -> \it ->  
                let withIntegers =  
                        let adapt = (\adapter -> \dir -> \cx -> \lit -> (\x -> case x of
                                Core.LiteralInteger v0 -> (Eithers.map (\x -> Core.LiteralInteger x) (Utils.encodeDecode dir (Compute.adapterCoder adapter) cx v0))
                                _ -> (Left (Context.InContext {
                                  Context.inContextObject = (Error.OtherError (Strings.cat2 "expected integer literal, found " (Core__.literal lit))),
                                  Context.inContextContext = cx}))) lit)
                        in (Eithers.bind (integerAdapter cx it) (\adapter ->  
                          let step = (Utils.bidirectional (adapt adapter))
                          in (Right [
                            Compute.Adapter {
                              Compute.adapterIsLossy = (Compute.adapterIsLossy adapter),
                              Compute.adapterSource = t,
                              Compute.adapterTarget = (Core.LiteralTypeInteger (Compute.adapterTarget adapter)),
                              Compute.adapterCoder = step}])))
                in  
                  let constraints = (Coders.languageConstraints (Coders.adapterContextLanguage cx))
                  in  
                    let hasIntegers = (Logic.not (Sets.null (Coders.languageConstraintsIntegerTypes constraints)))
                    in (Logic.ifElse hasIntegers withIntegers (Left "no integer types available")))
        in  
          let alts = (\t -> (\x -> case x of
                  Core.LiteralTypeBinary -> (forBinary t)
                  Core.LiteralTypeBoolean -> (forBoolean t)
                  Core.LiteralTypeFloat v0 -> (forFloat t v0)
                  Core.LiteralTypeInteger v0 -> (forInteger t v0)
                  Core.LiteralTypeString -> (Left "no substitute for the literal string type")) t)
          in  
            let supported = (Utils.literalTypeIsSupported (Coders.languageConstraints (Coders.adapterContextLanguage cx)))
            in (Utils.chooseAdapter alts supported Core__.literalType Core__.literalType lt)

-- | Create an adapter for float types
floatAdapter :: (Coders.AdapterContext -> Core.FloatType -> Either String (Compute.Adapter Core.FloatType Core.FloatType Core.FloatValue Core.FloatValue))
floatAdapter cx ft =  
  let makeAdapter = (\source -> \target ->  
          let lossy = (Equality.equal (comparePrecision (Reflect.floatTypePrecision source) (Reflect.floatTypePrecision target)) Util.ComparisonGreaterThan)
          in  
            let step = Compute.Coder {
                    Compute.coderEncode = (\_cx -> \fv -> Right (convertFloatValue target fv)),
                    Compute.coderDecode = (\_cx -> \fv -> Right (convertFloatValue source fv))}
            in (Right (Compute.Adapter {
              Compute.adapterIsLossy = lossy,
              Compute.adapterSource = source,
              Compute.adapterTarget = target,
              Compute.adapterCoder = step})))
  in  
    let altTypes = (\t -> (\x -> case x of
            Core.FloatTypeBigfloat -> [
              Core.FloatTypeFloat64,
              Core.FloatTypeFloat32]
            Core.FloatTypeFloat32 -> [
              Core.FloatTypeFloat64,
              Core.FloatTypeBigfloat]
            Core.FloatTypeFloat64 -> [
              Core.FloatTypeBigfloat,
              Core.FloatTypeFloat32]) t)
    in  
      let alts = (\t -> Eithers.mapList (makeAdapter t) (altTypes t))
      in  
        let supported = (Utils.floatTypeIsSupported (Coders.languageConstraints (Coders.adapterContextLanguage cx)))
        in (Utils.chooseAdapter alts supported Core__.floatType Core__.floatType ft)

-- | Create an adapter for integer types
integerAdapter :: (Coders.AdapterContext -> Core.IntegerType -> Either String (Compute.Adapter Core.IntegerType Core.IntegerType Core.IntegerValue Core.IntegerValue))
integerAdapter cx it =  
  let interleave = (\xs -> \ys -> Lists.concat (Lists.transpose [
          xs,
          ys]))
  in  
    let signedOrdered = (Lists.filter (\v -> Logic.and (Reflect.integerTypeIsSigned v) (Logic.not (Equality.equal (Reflect.integerTypePrecision v) Util.PrecisionArbitrary))) Reflect.integerTypes)
    in  
      let unsignedOrdered = (Lists.filter (\v -> Logic.and (Logic.not (Reflect.integerTypeIsSigned v)) (Logic.not (Equality.equal (Reflect.integerTypePrecision v) Util.PrecisionArbitrary))) Reflect.integerTypes)
      in  
        let signedPref = (interleave signedOrdered unsignedOrdered)
        in  
          let unsignedPref = (interleave unsignedOrdered signedOrdered)
          in  
            let signedNonPref = (Lists.reverse unsignedPref)
            in  
              let unsignedNonPref = (Lists.reverse signedPref)
              in  
                let signed = (\i -> Lists.concat [
                        Lists.drop (Math.mul i 2) signedPref,
                        [
                          Core.IntegerTypeBigint],
                        (Lists.drop (Math.add (Math.sub 8 (Math.mul i 2)) 1) signedNonPref)])
                in  
                  let unsigned = (\i -> Lists.concat [
                          Lists.drop (Math.mul i 2) unsignedPref,
                          [
                            Core.IntegerTypeBigint],
                          (Lists.drop (Math.add (Math.sub 8 (Math.mul i 2)) 1) unsignedNonPref)])
                  in  
                    let makeAdapter = (\source -> \target ->  
                            let lossy = (Logic.not (Equality.equal (comparePrecision (Reflect.integerTypePrecision source) (Reflect.integerTypePrecision target)) Util.ComparisonLessThan))
                            in  
                              let step = Compute.Coder {
                                      Compute.coderEncode = (\_cx -> \iv -> Right (convertIntegerValue target iv)),
                                      Compute.coderDecode = (\_cx -> \iv -> Right (convertIntegerValue source iv))}
                              in (Right (Compute.Adapter {
                                Compute.adapterIsLossy = lossy,
                                Compute.adapterSource = source,
                                Compute.adapterTarget = target,
                                Compute.adapterCoder = step})))
                    in  
                      let altTypes = (\t -> (\x -> case x of
                              Core.IntegerTypeBigint -> (Lists.reverse unsignedPref)
                              Core.IntegerTypeInt8 -> (signed 1)
                              Core.IntegerTypeInt16 -> (signed 2)
                              Core.IntegerTypeInt32 -> (signed 3)
                              Core.IntegerTypeInt64 -> (signed 4)
                              Core.IntegerTypeUint8 -> (unsigned 1)
                              Core.IntegerTypeUint16 -> (unsigned 2)
                              Core.IntegerTypeUint32 -> (unsigned 3)
                              Core.IntegerTypeUint64 -> (unsigned 4)) t)
                      in  
                        let alts = (\t -> Eithers.mapList (makeAdapter t) (altTypes t))
                        in  
                          let supported = (Utils.integerTypeIsSupported (Coders.languageConstraints (Coders.adapterContextLanguage cx)))
                          in (Utils.chooseAdapter alts supported Core__.integerType Core__.integerType it)
