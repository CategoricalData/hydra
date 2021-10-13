module Hydra.Prototyping.Adapters.AtomicSpec where

import Hydra.Core
import Hydra.Errors
import Hydra.Adapter
import Hydra.Prototyping.Basics
import Hydra.Prototyping.Steps
import Hydra.Prototyping.Adapters.Atomic

import Hydra.TestUtils

import qualified Data.Set as S
import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC


testFloatAdapter :: H.SpecWith ()
testFloatAdapter = H.describe "Test floating-point adapter" $ do

  H.it "upgrade float32 to bigfloat, since float32 and float64 are unsupported" $
    QC.property $ \f -> checkFloatAdapter
      [FloatVariantBigfloat]
      FloatTypeFloat32 FloatTypeBigfloat False
      (FloatValueFloat32 f) (FloatValueBigfloat $ realToFrac f)

  H.it "downgrade bigfloat to float64" $
    QC.property $ \d -> checkFloatAdapter
      [FloatVariantFloat32, FloatVariantFloat64]
      FloatTypeBigfloat FloatTypeFloat64 True
      (FloatValueBigfloat d) (FloatValueFloat64 $ realToFrac d)

  H.it "downgrade bigfloat to float32, since float64 is unsupported" $
    QC.property $ \d -> checkFloatAdapter
      [FloatVariantFloat32]
      FloatTypeBigfloat FloatTypeFloat32 True
      (FloatValueBigfloat d) (FloatValueFloat32 $ realToFrac d)
  
  H.it "bigfloat is supported and remains unchanged" $
    QC.property $ \d -> checkFloatAdapter
      [FloatVariantFloat32, FloatVariantBigfloat]
      FloatTypeBigfloat FloatTypeBigfloat False
      (FloatValueBigfloat d) (FloatValueBigfloat d)

testIntegerAdapter :: H.SpecWith ()
testIntegerAdapter = H.describe "Test integer adapter" $ do

  H.it "upgrade uint8 to uint16, not int16" $
    QC.property $ \b -> checkIntegerAdapter
      [IntegerVariantInt16, IntegerVariantUint16, IntegerVariantBigint]
      IntegerTypeUint8 IntegerTypeUint16 False
      (IntegerValueUint8 b) (IntegerValueUint16 $ fromIntegral b)

  H.it "upgrade int8 to int16, not uint16" $
    QC.property $ \b -> checkIntegerAdapter
      [IntegerVariantInt16, IntegerVariantUint16, IntegerVariantBigint]
      IntegerTypeInt8 IntegerTypeInt16 False
      (IntegerValueInt8 b) (IntegerValueInt16 $ fromIntegral b)

  H.it "upgrade uint8 to int16 when uint16 is not supported" $
    QC.property $ \b -> checkIntegerAdapter
      [IntegerVariantInt16, IntegerVariantInt32, IntegerVariantBigint]
      IntegerTypeUint8 IntegerTypeInt16 False
      (IntegerValueUint8 b) (IntegerValueInt16 $ fromIntegral b)

  H.it "cross-convert uint32 to int32, even when uint16 is supported" $
    QC.property $ \b -> checkIntegerAdapter
      [IntegerVariantUint16, IntegerVariantInt32]
      IntegerTypeUint32 IntegerTypeInt32 True
      (IntegerValueUint32 b) (IntegerValueInt32 $ fromIntegral b)

  H.it "downgrade bigint to int32, not uint32" $
    QC.property $ \b -> checkIntegerAdapter
      [IntegerVariantInt16, IntegerVariantUint16, IntegerVariantInt32, IntegerVariantUint32]
      IntegerTypeBigint IntegerTypeInt32 True
      (IntegerValueBigint b) (IntegerValueInt32 $ fromIntegral b)

testAtomicAdapter :: H.SpecWith ()
testAtomicAdapter = H.describe "Test atomic adapter" $ do

  H.it "encode binary data as strings" $
    QC.property $ \b -> checkAtomicAdapter
      [AtomicVariantString]
      AtomicTypeBinary AtomicTypeString False
      (AtomicValueBinary b) (AtomicValueString b)

  H.it "encode booleans as strings" $
    QC.property $ \b -> checkAtomicAdapter
      [AtomicVariantString]
      AtomicTypeBoolean AtomicTypeString False
      (AtomicValueBoolean b) (AtomicValueString $ if b == BooleanValueTrue then "true" else "false")

  H.it "encode booleans as integers" $
    QC.property $ \b -> checkAtomicAdapter
      [AtomicVariantInteger]
      AtomicTypeBoolean (AtomicTypeInteger IntegerTypeInt16) False
      (AtomicValueBoolean b) (AtomicValueInteger $ IntegerValueInt16 $ if b == BooleanValueTrue then 1 else 0)

  H.it "floating-point encoding is delegated to the float adapter" $
    QC.property $ \f -> checkAtomicAdapter
      [AtomicVariantFloat]
      (AtomicTypeFloat FloatTypeBigfloat) (AtomicTypeFloat FloatTypeFloat32) True
      (AtomicValueFloat $ FloatValueBigfloat f) (AtomicValueFloat $ FloatValueFloat32 $ realToFrac f)

  H.it "integer encoding is delegated to the integer adapter" $
    QC.property $ \i -> checkAtomicAdapter
      [AtomicVariantInteger]
      (AtomicTypeInteger IntegerTypeBigint) (AtomicTypeInteger IntegerTypeInt32) True
      (AtomicValueInteger $ IntegerValueBigint i) (AtomicValueInteger $ IntegerValueInt32 $ fromIntegral i)

  H.it "strings are unchanged" $
    QC.property $ \s -> checkAtomicAdapter
      [AtomicVariantString]
      AtomicTypeString AtomicTypeString False
      (AtomicValueString s) (AtomicValueString s)

spec :: H.Spec
spec = do
  testFloatAdapter
  testIntegerAdapter
  testAtomicAdapter
