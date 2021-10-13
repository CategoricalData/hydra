module Hydra.Prototyping.Adapters.Atomic (
  atomicAdapter,
  floatAdapter,
  integerAdapter,
) where

import Hydra.Core
import Hydra.Prototyping.Basics
import Hydra.Prototyping.Steps
import Hydra.Impl.Haskell.Extras
import Hydra.Adapter
import Hydra.Prototyping.Adapters.Utils

import qualified Data.List as L
import qualified Data.Set as S


atomicAdapter :: AdapterContext -> AtomicType -> Qualified (Adapter AtomicType AtomicValue)
atomicAdapter context = chooseAdapter alts supported describeAtomicType
  where
    alts t = case t of
        AtomicTypeBinary -> pure $ fallbackAdapter t
        AtomicTypeBoolean -> pure $ if noIntegerVars
            then fallbackAdapter t
            else do
              adapter <- integerAdapter context IntegerTypeUint8
              let step' = adapterStep adapter
              let step = Step encode decode
                    where
                      encode (AtomicValueBoolean bv) = AtomicValueInteger <$> stepOut step' (toInt bv)
                        where
                          toInt bv = IntegerValueUint8 $ if bv == BooleanValueFalse then 0 else 1
                      decode (AtomicValueInteger iv) = AtomicValueBoolean <$> do
                        (IntegerValueUint8 v) <- stepIn step' iv
                        return $ if v == 0 then BooleanValueFalse else BooleanValueTrue
              return $ Adapter False t (AtomicTypeInteger $ adapterTarget adapter) step
        AtomicTypeFloat ft -> pure $ if noFloatVars
          then fallbackAdapter t
          else do
            adapter <- floatAdapter context ft
            let step = bidirectional
                  $ \dir (AtomicValueFloat fv) -> AtomicValueFloat
                    <$> stepBoth dir (adapterStep adapter) fv
            return $ Adapter (adapterIsLossy adapter) t (AtomicTypeFloat $ adapterTarget adapter) step
        AtomicTypeInteger it -> pure $ if noIntegerVars
          then fallbackAdapter t
          else do
            adapter <- integerAdapter context it
            let step = bidirectional
                  $ \dir (AtomicValueInteger iv) -> AtomicValueInteger
                    <$> stepBoth dir (adapterStep adapter) iv
            return $ Adapter (adapterIsLossy adapter) t (AtomicTypeInteger $ adapterTarget adapter) step
        AtomicTypeString -> pure $ fail "no substitute for the atomic string type"
    supported = atomicTypeIsSupported constraints
    constraints = languageConstraints $ adapterContextTarget context
    noFloatVars = not (S.member AtomicVariantFloat $ languageConstraintsAtomicVariants constraints)
      || S.null (languageConstraintsFloatVariants constraints)
    noIntegerVars = not (S.member AtomicVariantInteger $ languageConstraintsAtomicVariants constraints)
      || S.null (languageConstraintsIntegerVariants constraints)
    noStrings = not $ supported AtomicTypeString
    fallbackAdapter t = if noStrings
        then fail "cannot serialize unsupported type; strings are unsupported"
        else qualify msg $ Adapter False t AtomicTypeString step
      where
        msg = disclaimer False (describeAtomicType t) (describeAtomicType AtomicTypeString)
        step = Step encode decode
          where
            -- TODO: this format is tied to Haskell
            encode av = pure $ AtomicValueString $ case av of
              AtomicValueBinary s -> s
              AtomicValueBoolean b -> if b == BooleanValueTrue then "true" else "false"
              _ -> show av
            decode (AtomicValueString s) = pure $ case t of
              AtomicTypeBinary -> AtomicValueBinary s
              AtomicTypeBoolean -> AtomicValueBoolean $ if s == "true" then BooleanValueTrue else BooleanValueFalse
              _ -> read s

disclaimer :: Bool -> String -> String -> String
disclaimer lossy source target = "replace " ++ source ++ " with " ++ target
  ++ if lossy then " (lossy)" else ""

floatAdapter :: AdapterContext -> FloatType -> Qualified (Adapter FloatType FloatValue)
floatAdapter context = chooseAdapter alts supported describeFloatType
  where
    alts t = makeAdapter t <$> case t of
        FloatTypeBigfloat -> [FloatTypeFloat64, FloatTypeFloat32]
        FloatTypeFloat32 -> [FloatTypeFloat64, FloatTypeBigfloat]
        FloatTypeFloat64 -> [FloatTypeBigfloat, FloatTypeFloat32]
      where
        makeAdapter source target = qualify msg $ Adapter lossy source target step
          where
            lossy = comparePrecision (floatTypePrecision source) (floatTypePrecision target) == GT
            step = Step (pure . convertFloatValue target) (pure . convertFloatValue source)
            msg = disclaimer lossy (describeFloatType source) (describeFloatType target)

    supported = floatTypeIsSupported $ languageConstraints $ adapterContextTarget context

integerAdapter :: AdapterContext -> IntegerType -> Qualified (Adapter IntegerType IntegerValue)
integerAdapter context = chooseAdapter alts supported describeIntegerType
  where
    alts t = makeAdapter t <$> case t of
        IntegerTypeBigint -> L.reverse unsignedPref
        IntegerTypeInt8 -> signed 1
        IntegerTypeInt16 -> signed 2
        IntegerTypeInt32 -> signed 3
        IntegerTypeInt64 -> signed 4
        IntegerTypeUint8 -> unsigned 1
        IntegerTypeUint16 -> unsigned 2
        IntegerTypeUint32 -> unsigned 3
        IntegerTypeUint64 -> unsigned 4
      where
        signed i = L.drop (i*2) signedPref ++ [IntegerTypeBigint] ++ L.drop (8-(i*2)+1) signedNonPref
        unsigned i = L.drop (i*2) unsignedPref ++ [IntegerTypeBigint] ++ L.drop (8-(i*2)+1) unsignedNonPref
        signedPref = interleave signedOrdered unsignedOrdered
        unsignedPref = interleave unsignedOrdered signedOrdered
        signedNonPref = L.reverse unsignedPref
        unsignedNonPref = L.reverse signedPref
        interleave xs ys = L.concat (L.transpose [xs, ys])
        signedOrdered = L.filter
          (\v -> integerTypeIsSigned v && integerTypePrecision v /= PrecisionArbitrary) integerTypes
        unsignedOrdered = L.filter
          (\v -> not (integerTypeIsSigned v) && integerTypePrecision v /= PrecisionArbitrary) integerTypes
        makeAdapter source target = qualify msg $ Adapter lossy source target step
          where
            lossy = comparePrecision (integerTypePrecision source) (integerTypePrecision target) /= LT
            step = Step (pure . convertIntegerValue target) (pure . convertIntegerValue source)
            msg = disclaimer lossy (describeIntegerType source) (describeIntegerType target)

    supported = integerTypeIsSupported $ languageConstraints $ adapterContextTarget context
