module Hydra.Adapters.Literal (
  literalAdapter,
  floatAdapter,
  integerAdapter,
) where

import Hydra.Core
import Hydra.Basics
import Hydra.Monads
import Hydra.Compute
import Hydra.Adapters.Utils
import Hydra.Adapters.UtilsEtc
import Hydra.Lexical

import qualified Data.List as L
import qualified Data.Set as S


literalAdapter :: LiteralType -> Flow (AdapterContext m) (SymmetricAdapter (Context m) LiteralType Literal)
literalAdapter lt = do
    acx <- getState
    chooseAdapter (alts acx) (supported acx) describeLiteralType lt
  where
    supported acx = literalTypeIsSupported (constraints acx)
    constraints acx = languageConstraints $ adapterContextTarget acx

    alts acx t = case t of
        LiteralTypeBinary -> pure $ fallbackAdapter t
        LiteralTypeBoolean -> pure $ if noIntegerVars
            then fallbackAdapter t
            else do
              adapter <- integerAdapter IntegerTypeUint8
              let step' = adapterCoder adapter
              let step = Coder encode decode
                    where
                      encode (LiteralBoolean bv) = LiteralInteger <$> coderEncode step' (toInt bv)
                        where
                          toInt bv = IntegerValueUint8 $ if bv then 1 else 0
                      decode (LiteralInteger iv) = LiteralBoolean <$> do
                        (IntegerValueUint8 v) <- coderDecode step' iv
                        return $ v == 1
              return $ Adapter False t (LiteralTypeInteger $ adapterTarget adapter) step
        LiteralTypeFloat ft -> pure $ if noFloatVars
          then fallbackAdapter t
          else do
            adapter <- floatAdapter ft
            let step = bidirectional
                  $ \dir l -> case l of
                    LiteralFloat fv -> LiteralFloat <$> encodeDecode dir (adapterCoder adapter) fv
                    _ -> unexpected "floating-point literal" (show l)
            return $ Adapter (adapterIsLossy adapter) t (LiteralTypeFloat $ adapterTarget adapter) step
        LiteralTypeInteger it -> pure $ if noIntegerVars
          then fallbackAdapter t
          else do
            adapter <- integerAdapter it
            let step = bidirectional
                  $ \dir (LiteralInteger iv) -> LiteralInteger
                    <$> encodeDecode dir (adapterCoder adapter) iv
            return $ Adapter (adapterIsLossy adapter) t (LiteralTypeInteger $ adapterTarget adapter) step
        LiteralTypeString -> pure $ fail "no substitute for the literal string type"
      where
        noFloatVars = not (S.member LiteralVariantFloat $ languageConstraintsLiteralVariants $ constraints acx)
          || S.null (languageConstraintsFloatTypes $ constraints acx)
        noIntegerVars = not (S.member LiteralVariantInteger $ languageConstraintsLiteralVariants $ constraints acx)
          || S.null (languageConstraintsIntegerTypes $ constraints acx)
        noStrings = not $ supported acx LiteralTypeString

        fallbackAdapter t = if noStrings
            then fail "cannot serialize unsupported type; strings are unsupported"
            else withWarning msg $ Adapter False t LiteralTypeString step
          where
            msg = disclaimer False (describeLiteralType t) (describeLiteralType LiteralTypeString)
            step = Coder encode decode
              where
                -- TODO: this format is tied to Haskell
                encode av = pure $ LiteralString $ case av of
                  LiteralBinary s -> s
                  LiteralBoolean b -> if b then "true" else "false"
                  _ -> show av
                decode (LiteralString s) = pure $ case t of
                  LiteralTypeBinary -> LiteralBinary s
                  LiteralTypeBoolean -> LiteralBoolean $ s == "true"
                  _ -> read s

comparePrecision :: Precision -> Precision -> Ordering
comparePrecision p1 p2 = if p1 == p2 then EQ else case (p1, p2) of
  (PrecisionArbitrary, _) -> GT
  (_, PrecisionArbitrary) -> LT
  (PrecisionBits b1, PrecisionBits b2) -> compare b1 b2

disclaimer :: Bool -> String -> String -> String
disclaimer lossy source target = "replace " ++ source ++ " with " ++ target
  ++ if lossy then " (lossy)" else ""

floatAdapter :: FloatType -> Flow (AdapterContext m) (SymmetricAdapter (Context m) FloatType FloatValue)
floatAdapter ft = do
    acx <- getState
    let supported = floatTypeIsSupported $ languageConstraints $ adapterContextTarget acx
    chooseAdapter alts supported describeFloatType ft
  where
    alts t = makeAdapter t <$> case t of
        FloatTypeBigfloat -> [FloatTypeFloat64, FloatTypeFloat32]
        FloatTypeFloat32 -> [FloatTypeFloat64, FloatTypeBigfloat]
        FloatTypeFloat64 -> [FloatTypeBigfloat, FloatTypeFloat32]
      where
        makeAdapter source target = withWarning msg $ Adapter lossy source target step
          where
            lossy = comparePrecision (floatTypePrecision source) (floatTypePrecision target) == GT
            step = Coder (pure . convertFloatValue target) (pure . convertFloatValue source)
            msg = disclaimer lossy (describeFloatType source) (describeFloatType target)

integerAdapter :: IntegerType -> Flow (AdapterContext m) (SymmetricAdapter (Context m) IntegerType IntegerValue)
integerAdapter it = do
    acx <- getState
    let supported = integerTypeIsSupported $ languageConstraints $ adapterContextTarget acx
    chooseAdapter alts supported describeIntegerType it
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

        makeAdapter source target = withWarning msg $ Adapter lossy source target step
          where
            lossy = comparePrecision (integerTypePrecision source) (integerTypePrecision target) /= LT
            step = Coder (pure . convertIntegerValue target) (pure . convertIntegerValue source)
            msg = disclaimer lossy (describeIntegerType source) (describeIntegerType target)
