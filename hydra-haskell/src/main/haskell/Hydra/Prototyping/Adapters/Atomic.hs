module Hydra.Prototyping.Adapters.Atomic (
  Mutator(..),
  atomicAdapter,
  atomicMutators,
  floatMutators,
  integerMutators,
  mutateFloatValue,
  mutateIntegerValue,
) where

import Hydra.Core
import Hydra.Prototyping.Basics
import Hydra.Prototyping.Extras

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y
import qualified Data.Set as S


type Mutator v = Qualified (v -> v)

atomicAdapter = ()
--atomicAdapter :: TranslationContext -> AtomicType -> Either String (Step AtomicValue AtomicValue)
--atomicAdapter context at = ...


atomicMutators :: S.Set AtomicVariant -> Qualified (M.Map AtomicVariant (AtomicValue -> AtomicValue))
atomicMutators = mutators atomicVariants subst descriptions buildMap
  where
    subst _ = [] -- no substitution across atomic variants (for now)
    descriptions = M.fromList [
      (AtomicVariantBinary, "binary strings"),
      (AtomicVariantBoolean, "boolean values"),
      (AtomicVariantFloat, "floating-point numbers"),
      (AtomicVariantInteger, "integers"),
      (AtomicVariantString, "strings")]
    buildMap :: AtomicVariant -> AtomicVariant -> Qualified (AtomicValue -> AtomicValue)
    buildMap source target = pure id

floatMutators :: S.Set FloatVariant -> Qualified (M.Map FloatVariant (FloatValue -> FloatValue))
floatMutators = mutators floatVariants subst descriptions buildMap
  where
    subst v = case v of
       FloatVariantBigfloat -> [FloatVariantFloat64, FloatVariantFloat32]
       FloatVariantFloat32 -> [FloatVariantFloat64, FloatVariantBigfloat]
       FloatVariantFloat64 -> [FloatVariantBigfloat, FloatVariantFloat32]
    descriptions = M.fromList $ describe <$> floatVariants
      where
        describe v = (v, precision v ++ " floating-point numbers")
        precision v = case floatVariantPrecision v of
          PrecisionArbitrary -> "arbitrary-precision"
          PrecisionBits bits -> show bits ++ "-bit"
    buildMap :: FloatVariant -> FloatVariant -> Qualified (FloatValue -> FloatValue)
    buildMap _ target = pure $ encoder . decoder
      where
        decoder fv = case fv of
          FloatValueBigfloat d -> d
          FloatValueFloat32 f -> realToFrac f
          FloatValueFloat64 d -> d
        encoder d = case target of
          FloatVariantBigfloat -> FloatValueBigfloat d
          FloatVariantFloat32 -> FloatValueFloat32 $ realToFrac d
          FloatVariantFloat64 -> FloatValueFloat64 d

integerMutators :: S.Set IntegerVariant -> Qualified (M.Map IntegerVariant (IntegerValue -> IntegerValue))
integerMutators = mutators integerVariants subst descriptions buildMap
  where
    subst v = case v of
        IntegerVariantBigint -> L.reverse unsignedPref
        IntegerVariantInt8 -> signed 1
        IntegerVariantInt16 -> signed 2
        IntegerVariantInt32 -> signed 3
        IntegerVariantInt64 -> signed 4
        IntegerVariantUint8 -> unsigned 1
        IntegerVariantUint16 -> unsigned 2
        IntegerVariantUint32 -> unsigned 3
        IntegerVariantUint64 -> unsigned 4
      where
        signed i = L.drop (i*2) signedPref ++ [IntegerVariantBigint] ++ L.drop (8-(i*2)+1) signedNonPref
        unsigned i = L.drop (i*2) unsignedPref ++ [IntegerVariantBigint] ++ L.drop (8-(i*2)+1) unsignedNonPref
        signedPref = interleave signedOrdered unsignedOrdered
        unsignedPref = interleave unsignedOrdered signedOrdered
        signedNonPref = L.reverse unsignedPref
        unsignedNonPref = L.reverse signedPref
        interleave xs ys = L.concat (L.transpose [xs, ys])
        signedOrdered = L.filter
          (\v -> integerVariantIsSigned v && integerVariantPrecision v /= PrecisionArbitrary) integerVariants
        unsignedOrdered = L.filter
          (\v -> not (integerVariantIsSigned v) && integerVariantPrecision v /= PrecisionArbitrary) integerVariants
    descriptions = M.fromList $ describe <$> integerVariants
      where
        describe v = (v, precision v ++ " integers")
        precision v = case integerVariantPrecision v of
          PrecisionArbitrary -> "arbitrary-precision"
          PrecisionBits bits -> show bits ++ "-bit"
    buildMap :: IntegerVariant -> IntegerVariant -> Qualified (IntegerValue -> IntegerValue)
    buildMap _ target = pure $ encoder . decoder
      where
        decoder :: IntegerValue -> Integer
        decoder iv = case iv of
          IntegerValueBigint v -> v
          IntegerValueInt8 v -> fromIntegral v
          IntegerValueInt16 v -> fromIntegral v
          IntegerValueInt32 v -> fromIntegral v
          IntegerValueInt64 v -> fromIntegral v
          IntegerValueUint8 v -> fromIntegral v
          IntegerValueUint16 v -> fromIntegral v
          IntegerValueUint32 v -> fromIntegral v
          IntegerValueUint64 v -> fromIntegral v
        encoder :: Integer -> IntegerValue
        encoder d = case target of
          IntegerVariantBigint -> IntegerValueBigint d
          IntegerVariantInt8 -> IntegerValueInt8 $ fromIntegral d
          IntegerVariantInt16 -> IntegerValueInt16 $ fromIntegral d
          IntegerVariantInt32 -> IntegerValueInt32 $ fromIntegral d
          IntegerVariantInt64 -> IntegerValueInt64 $ fromIntegral d
          IntegerVariantUint8 -> IntegerValueUint8 $ fromIntegral d
          IntegerVariantUint16 -> IntegerValueUint16 $ fromIntegral d
          IntegerVariantUint32 -> IntegerValueUint32 $ fromIntegral d
          IntegerVariantUint64 -> IntegerValueUint64 $ fromIntegral d

mutateFloatValue :: M.Map FloatVariant (FloatValue -> FloatValue) -> FloatValue -> FloatValue
mutateFloatValue muts fv = Y.fromMaybe id (M.lookup (floatValueVariant fv) muts) fv

mutateIntegerValue :: M.Map IntegerVariant (IntegerValue -> IntegerValue) -> IntegerValue -> IntegerValue
mutateIntegerValue muts iv = Y.fromMaybe id (M.lookup (integerValueVariant iv) muts) iv

mutator :: (Ord v, Show v) =>
     (v -> [v])
  -> M.Map v String
  -> (v -> v -> Qualified (a -> a))
  -> S.Set v
  -> v
  -> Qualified (a -> a)
mutator subst descriptions buildMap supported source
  | S.member source supported = pure id
  | L.null candidates = fail $ "no acceptable substitute for " ++ show source
  | otherwise = do
      mapping <- buildMap source target
      Qualified (Just mapping) ["replace " ++ describe source ++ " with " ++ describe target]
  where
      target = L.head candidates
      candidates = L.filter (`S.member` supported) $ subst source
      describe var = Y.fromMaybe "unknown" $ M.lookup var descriptions

mutators :: (Ord v, Show v) =>
     [v]
  -> (v -> [v])
  -> M.Map v String
  -> (v -> v -> Qualified (a -> a))
  -> S.Set v
  -> Qualified (M.Map v (a -> a))
mutators variants subst descriptions buildMap supported = M.fromList <$> CM.mapM toPair variants
  where
    toPair v = do
      m <- mutator subst descriptions buildMap supported v
      return (v, m)
