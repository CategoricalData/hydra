-- | Adapter framework for literal types and terms

module Hydra.LiteralAdapters where

import qualified Hydra.AdapterUtils as AdapterUtils
import qualified Hydra.Coders as Coders
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Describe.Core as Core_
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as Core__
import qualified Hydra.Flows as Flows
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows_
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Show.Core as Core___
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Compare two precision values
comparePrecision :: (Mantle.Precision -> Mantle.Precision -> Graph.Comparison)
comparePrecision p1 p2 = ((\x -> case x of
  Mantle.PrecisionArbitrary -> ((\x -> case x of
    Mantle.PrecisionArbitrary -> Graph.ComparisonEqualTo
    Mantle.PrecisionBits _ -> Graph.ComparisonGreaterThan) p2)
  Mantle.PrecisionBits v1 -> ((\x -> case x of
    Mantle.PrecisionArbitrary -> Graph.ComparisonLessThan
    Mantle.PrecisionBits v2 -> (Logic.ifElse (Equality.ltInt32 v1 v2) Graph.ComparisonLessThan Graph.ComparisonGreaterThan)) p2)) p1)

-- | Convert a float value to a different float type
convertFloatValue :: (Core.FloatType -> Core.FloatValue -> Core.FloatValue)
convertFloatValue target fv =  
  let decoder = (\fv -> (\x -> case x of
          Core.FloatValueBigfloat v1 -> v1
          Core.FloatValueFloat32 v1 -> (Literals.float32ToBigfloat v1)
          Core.FloatValueFloat64 v1 -> (Literals.float64ToBigfloat v1)) fv) 
      encoder = (\d -> (\x -> case x of
              Core.FloatTypeBigfloat -> (Core.FloatValueBigfloat d)
              Core.FloatTypeFloat32 -> (Core.FloatValueFloat32 (Literals.bigfloatToFloat32 d))
              Core.FloatTypeFloat64 -> (Core.FloatValueFloat64 (Literals.bigfloatToFloat64 d))) target)
  in (encoder (decoder fv))

-- | Convert an integer value to a different integer type
convertIntegerValue :: (Core.IntegerType -> Core.IntegerValue -> Core.IntegerValue)
convertIntegerValue target iv =  
  let decoder = (\iv -> (\x -> case x of
          Core.IntegerValueBigint v1 -> v1
          Core.IntegerValueInt8 v1 -> (Literals.int8ToBigint v1)
          Core.IntegerValueInt16 v1 -> (Literals.int16ToBigint v1)
          Core.IntegerValueInt32 v1 -> (Literals.int32ToBigint v1)
          Core.IntegerValueInt64 v1 -> (Literals.int64ToBigint v1)
          Core.IntegerValueUint8 v1 -> (Literals.uint8ToBigint v1)
          Core.IntegerValueUint16 v1 -> (Literals.uint16ToBigint v1)
          Core.IntegerValueUint32 v1 -> (Literals.uint32ToBigint v1)
          Core.IntegerValueUint64 v1 -> (Literals.uint64ToBigint v1)) iv) 
      encoder = (\d -> (\x -> case x of
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

literalAdapter :: (Core.LiteralType -> Compute.Flow Coders.AdapterContext (Compute.Adapter t0 t0 Core.LiteralType Core.LiteralType Core.Literal Core.Literal))
literalAdapter lt =  
  let alts = (\t -> (\x -> case x of
          Core.LiteralTypeBinary ->  
            let step = Compute.Coder {
                    Compute.coderEncode = (\lit -> (\x -> case x of
                      Core.LiteralBinary v2 -> (Flows_.pure (Core.LiteralString (Literals.binaryToString v2)))) lit),
                    Compute.coderDecode = (\lit -> (\x -> case x of
                      Core.LiteralString v2 -> (Flows_.pure (Core.LiteralBinary (Literals.stringToBinary v2)))) lit)}
            in (Flows_.pure [
              Compute.Adapter {
                Compute.adapterIsLossy = False,
                Compute.adapterSource = t,
                Compute.adapterTarget = Core.LiteralTypeString,
                Compute.adapterCoder = step}])
          Core.LiteralTypeBoolean -> (Flows_.bind Errors.getState (\cx ->  
            let constraints = (Coders.languageConstraints (Coders.adapterContextLanguage cx)) 
                hasIntegers = (Logic.not (Sets.null (Coders.languageConstraintsIntegerTypes constraints)))
                hasStrings = (Sets.member Mantle.LiteralVariantString (Coders.languageConstraintsLiteralVariants constraints))
            in (Logic.ifElse hasIntegers (Flows_.bind (integerAdapter Core.IntegerTypeUint8) (\adapter ->  
              let step_ = (Compute.adapterCoder adapter) 
                  step = Compute.Coder {
                          Compute.coderEncode = (\lit -> (\x -> case x of
                            Core.LiteralBoolean v2 -> (Flows_.bind (Compute.coderEncode step_ (Core.IntegerValueUint8 (Logic.ifElse v2 1 0))) (\iv -> Flows_.pure (Core.LiteralInteger iv)))) lit),
                          Compute.coderDecode = (\lit -> (\x -> case x of
                            Core.LiteralInteger v2 -> (Flows_.bind (Compute.coderDecode step_ v2) (\val -> (\x -> case x of
                              Core.IntegerValueUint8 v3 -> (Flows_.pure (Core.LiteralBoolean (Equality.equal v3 1)))) val))) lit)}
              in (Flows_.pure [
                Compute.Adapter {
                  Compute.adapterIsLossy = False,
                  Compute.adapterSource = t,
                  Compute.adapterTarget = (Core.LiteralTypeInteger (Compute.adapterTarget adapter)),
                  Compute.adapterCoder = step}]))) (Logic.ifElse hasStrings (Flows_.pure ( 
              let encode = (\lit -> Flows_.bind (Core__.booleanLiteral lit) (\b -> Flows_.pure (Core.LiteralString (Logic.ifElse b "true" "false")))) 
                  decode = (\lit -> Flows_.bind (Core__.stringLiteral lit) (\s -> Logic.ifElse (Equality.equalString s "true") (Flows_.pure (Core.LiteralBoolean True)) (Logic.ifElse (Equality.equalString s "false") (Flows_.pure (Core.LiteralBoolean False)) (Errors.unexpected "boolean literal" s))))
              in [
                Compute.Adapter {
                  Compute.adapterIsLossy = False,
                  Compute.adapterSource = t,
                  Compute.adapterTarget = Core.LiteralTypeString,
                  Compute.adapterCoder = Compute.Coder {
                    Compute.coderEncode = encode,
                    Compute.coderDecode = decode}}])) (Flows_.fail "no alternatives available for boolean encoding")))))
          Core.LiteralTypeFloat v1 -> (Flows_.bind Errors.getState (\cx ->  
            let constraints = (Coders.languageConstraints (Coders.adapterContextLanguage cx)) 
                hasFloats = (Logic.not (Sets.null (Coders.languageConstraintsFloatTypes constraints)))
            in (Logic.ifElse hasFloats (Flows_.bind (floatAdapter v1) (\adapter ->  
              let step = (AdapterUtils.bidirectional (\dir -> \l -> (\x -> case x of
                      Core.LiteralFloat v2 -> (Flows_.map (\x -> Core.LiteralFloat x) (AdapterUtils.encodeDecode dir (Compute.adapterCoder adapter) v2))
                      _ -> (Errors.unexpected "floating-point literal" (Core___.showLiteral l))) l))
              in (Flows_.pure [
                Compute.Adapter {
                  Compute.adapterIsLossy = (Compute.adapterIsLossy adapter),
                  Compute.adapterSource = t,
                  Compute.adapterTarget = (Core.LiteralTypeFloat (Compute.adapterTarget adapter)),
                  Compute.adapterCoder = step}]))) (Flows_.fail "no float types available"))))
          Core.LiteralTypeInteger v1 -> (Flows_.bind Errors.getState (\cx ->  
            let constraints = (Coders.languageConstraints (Coders.adapterContextLanguage cx)) 
                hasIntegers = (Logic.not (Sets.null (Coders.languageConstraintsIntegerTypes constraints)))
            in (Logic.ifElse hasIntegers (Flows_.bind (integerAdapter v1) (\adapter ->  
              let step = (AdapterUtils.bidirectional (\dir -> \lit -> (\x -> case x of
                      Core.LiteralInteger v2 -> (Flows_.map (\x -> Core.LiteralInteger x) (AdapterUtils.encodeDecode dir (Compute.adapterCoder adapter) v2))
                      _ -> (Errors.unexpected "integer literal" (Core___.showLiteral lit))) lit))
              in (Flows_.pure [
                Compute.Adapter {
                  Compute.adapterIsLossy = (Compute.adapterIsLossy adapter),
                  Compute.adapterSource = t,
                  Compute.adapterTarget = (Core.LiteralTypeInteger (Compute.adapterTarget adapter)),
                  Compute.adapterCoder = step}]))) (Flows_.fail "no integer types available"))))
          Core.LiteralTypeString -> (Flows_.fail "no substitute for the literal string type")) t)
  in (Flows_.bind Errors.getState (\cx ->  
    let supported = (AdapterUtils.literalTypeIsSupported (Coders.languageConstraints (Coders.adapterContextLanguage cx)))
    in (AdapterUtils.chooseAdapter alts supported Core___.showLiteralType Core_.literalType lt)))

floatAdapter :: (Core.FloatType -> Compute.Flow Coders.AdapterContext (Compute.Adapter t0 t1 Core.FloatType Core.FloatType Core.FloatValue Core.FloatValue))
floatAdapter ft =  
  let alts = (\t -> Flows_.mapList (makeAdapter t) ((\x -> case x of
          Core.FloatTypeBigfloat -> [
            Core.FloatTypeFloat64,
            Core.FloatTypeFloat32]
          Core.FloatTypeFloat32 -> [
            Core.FloatTypeFloat64,
            Core.FloatTypeBigfloat]
          Core.FloatTypeFloat64 -> [
            Core.FloatTypeBigfloat,
            Core.FloatTypeFloat32]) t)) 
      makeAdapter = (\source -> \target ->  
              let lossy = (Equality.equal (comparePrecision (Variants.floatTypePrecision source) (Variants.floatTypePrecision target)) Graph.ComparisonGreaterThan) 
                  step = Compute.Coder {
                          Compute.coderEncode = (\fv -> Flows_.pure (convertFloatValue target fv)),
                          Compute.coderDecode = (\fv -> Flows_.pure (convertFloatValue source fv))}
                  msg = (disclaimer lossy (Core_.floatType source) (Core_.floatType target))
              in (Flows.warn msg (Flows_.pure (Compute.Adapter {
                Compute.adapterIsLossy = lossy,
                Compute.adapterSource = source,
                Compute.adapterTarget = target,
                Compute.adapterCoder = step}))))
  in (Flows_.bind Errors.getState (\cx ->  
    let supported = (AdapterUtils.floatTypeIsSupported (Coders.languageConstraints (Coders.adapterContextLanguage cx)))
    in (AdapterUtils.chooseAdapter alts supported Core___.showFloatType Core_.floatType ft)))

integerAdapter :: (Core.IntegerType -> Compute.Flow Coders.AdapterContext (Compute.Adapter t0 t1 Core.IntegerType Core.IntegerType Core.IntegerValue Core.IntegerValue))
integerAdapter it =  
  let interleave = (\xs -> \ys -> Lists.concat (Lists.transpose [
          xs,
          ys])) 
      signedOrdered = (Lists.filter (\v -> Logic.and (Variants.integerTypeIsSigned v) (Logic.not (Equality.equal (Variants.integerTypePrecision v) Mantle.PrecisionArbitrary))) Variants.integerTypes)
      unsignedOrdered = (Lists.filter (\v -> Logic.and (Logic.not (Variants.integerTypeIsSigned v)) (Logic.not (Equality.equal (Variants.integerTypePrecision v) Mantle.PrecisionArbitrary))) Variants.integerTypes)
      signedPref = (interleave signedOrdered unsignedOrdered)
      unsignedPref = (interleave unsignedOrdered signedOrdered)
      signedNonPref = (Lists.reverse unsignedPref)
      unsignedNonPref = (Lists.reverse signedPref)
      signed = (\i -> Lists.concat [
              Lists.drop (Math.mul i 2) signedPref,
              [
                Core.IntegerTypeBigint],
              (Lists.drop (Math.add (Math.sub 8 (Math.mul i 2)) 1) signedNonPref)])
      unsigned = (\i -> Lists.concat [
              Lists.drop (Math.mul i 2) unsignedPref,
              [
                Core.IntegerTypeBigint],
              (Lists.drop (Math.add (Math.sub 8 (Math.mul i 2)) 1) unsignedNonPref)])
      alts = (\t -> Flows_.mapList (makeAdapter t) ((\x -> case x of
              Core.IntegerTypeBigint -> (Lists.reverse unsignedPref)
              Core.IntegerTypeInt8 -> (signed 1)
              Core.IntegerTypeInt16 -> (signed 2)
              Core.IntegerTypeInt32 -> (signed 3)
              Core.IntegerTypeInt64 -> (signed 4)
              Core.IntegerTypeUint8 -> (unsigned 1)
              Core.IntegerTypeUint16 -> (unsigned 2)
              Core.IntegerTypeUint32 -> (unsigned 3)
              Core.IntegerTypeUint64 -> (unsigned 4)) t))
      makeAdapter = (\source -> \target ->  
              let lossy = (Logic.not (Equality.equal (comparePrecision (Variants.integerTypePrecision source) (Variants.integerTypePrecision target)) Graph.ComparisonLessThan)) 
                  step = Compute.Coder {
                          Compute.coderEncode = (\iv -> Flows_.pure (convertIntegerValue target iv)),
                          Compute.coderDecode = (\iv -> Flows_.pure (convertIntegerValue source iv))}
                  msg = (disclaimer lossy (Core_.integerType source) (Core_.integerType target))
              in (Flows.warn msg (Flows_.pure (Compute.Adapter {
                Compute.adapterIsLossy = lossy,
                Compute.adapterSource = source,
                Compute.adapterTarget = target,
                Compute.adapterCoder = step}))))
  in (Flows_.bind Errors.getState (\cx ->  
    let supported = (AdapterUtils.integerTypeIsSupported (Coders.languageConstraints (Coders.adapterContextLanguage cx)))
    in (AdapterUtils.chooseAdapter alts supported Core___.showIntegerType Core_.integerType it)))
