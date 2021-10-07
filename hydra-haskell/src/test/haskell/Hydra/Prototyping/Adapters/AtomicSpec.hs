module Hydra.Prototyping.Adapters.AtomicSpec where

import Hydra.Core
import Hydra.Prototyping.Adapters.Atomic

import qualified Data.Either as E
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC


testFloatMutators :: H.SpecWith ()
testFloatMutators = do
  H.describe "Test floating-point mutators" $ do

    H.it "upgrade float32 to bigfloat, since float32 and float64 are unsupported" $
      QC.property $ \f -> mutateFloatValue m3 (FloatValueFloat32 f) == FloatValueBigfloat (realToFrac f)

    H.it "downgrade bigfloat to float64" $
      QC.property $ \d -> mutateFloatValue m1 (FloatValueBigfloat d) == FloatValueFloat64 d

    H.it "downgrade bigfloat to float32, since float64 is unsupported" $
      QC.property $ \d -> mutateFloatValue m4 (FloatValueBigfloat d) == FloatValueFloat32 (realToFrac d)

    H.it "bigfloat is supported and remains unchanged" $
      QC.property $ \d -> mutateFloatValue m2 (FloatValueBigfloat d) == FloatValueBigfloat d
  where
    muts = E.fromRight M.empty . floatMutators . S.fromList
    m1 = muts [FloatVariantFloat32, FloatVariantFloat64]
    m2 = muts [FloatVariantFloat32, FloatVariantBigfloat]
    m3 = muts [FloatVariantBigfloat]
    m4 = muts [FloatVariantFloat32]

testIntegerMutators :: H.SpecWith ()
testIntegerMutators = do
  H.describe "Test integer mutators" $ do

    H.it "upgrade uint8 to uint16, not int16" $
      QC.property $ \b -> mutateIntegerValue m1 (IntegerValueUint8 b) == IntegerValueUint16 (fromIntegral b)

    H.it "upgrade int8 to int16, not uint16" $
      QC.property $ \b -> mutateIntegerValue m1 (IntegerValueInt8 b) == IntegerValueInt16 (fromIntegral b)

    H.it "upgrade uint8 to int16 when uint16 is not supported" $
      QC.property $ \b -> mutateIntegerValue m2 (IntegerValueUint8 b) == IntegerValueInt16 (fromIntegral b)

    H.it "cross-convert uint32 to int32, even when uint16 is supported" $
      QC.property $ \i -> mutateIntegerValue m3 (IntegerValueUint32 i) == IntegerValueInt32 (fromIntegral i)

    H.it "downgrade bigint to int32, not uint32" $
      QC.property $ \i -> mutateIntegerValue m4 (IntegerValueBigint i) == IntegerValueInt32 (fromIntegral i)
  where
    muts = E.fromRight M.empty . integerMutators . S.fromList
    m1 = muts [IntegerVariantInt16, IntegerVariantUint16, IntegerVariantBigint]
    m2 = muts [IntegerVariantInt16, IntegerVariantInt32, IntegerVariantBigint]
    m3 = muts [IntegerVariantUint16, IntegerVariantInt32]
    m4 = muts [IntegerVariantInt16, IntegerVariantUint16, IntegerVariantInt32, IntegerVariantUint32]

spec :: H.Spec
spec = do
  testFloatMutators
  testIntegerMutators
--  testAtomicMutators
--  testTermMutators
