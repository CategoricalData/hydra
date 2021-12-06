module Hydra.Prototyping.Adapters.AtomicSpec where

import Hydra.Core

import Hydra.TestUtils

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC


testFloatAdapter :: H.SpecWith ()
testFloatAdapter = H.describe "Test floating-point adapter" $ do

  H.it "upgrade float32 to bigfloat, since float32 and float64 are unsupported" $
    QC.property $ \f -> checkFloatAdapter
      [FloatTypeBigfloat]
      FloatTypeFloat32 FloatTypeBigfloat False
      (FloatValueFloat32 f) (FloatValueBigfloat $ realToFrac f)

  H.it "downgrade bigfloat to float64" $
    QC.property $ \d -> checkFloatAdapter
      [FloatTypeFloat32, FloatTypeFloat64]
      FloatTypeBigfloat FloatTypeFloat64 True
      (FloatValueBigfloat d) (FloatValueFloat64 $ realToFrac d)

  H.it "downgrade bigfloat to float32, since float64 is unsupported" $
    QC.property $ \d -> checkFloatAdapter
      [FloatTypeFloat32]
      FloatTypeBigfloat FloatTypeFloat32 True
      (FloatValueBigfloat d) (FloatValueFloat32 $ realToFrac d)
  
  H.it "bigfloat is supported and remains unchanged" $
    QC.property $ \d -> checkFloatAdapter
      [FloatTypeFloat32, FloatTypeBigfloat]
      FloatTypeBigfloat FloatTypeBigfloat False
      (FloatValueBigfloat d) (FloatValueBigfloat d)

testIntegerAdapter :: H.SpecWith ()
testIntegerAdapter = H.describe "Test integer adapter" $ do

  H.it "upgrade uint8 to uint16, not int16" $
    QC.property $ \b -> checkIntegerAdapter
      [IntegerTypeInt16, IntegerTypeUint16, IntegerTypeBigint]
      IntegerTypeUint8 IntegerTypeUint16 False
      (IntegerValueUint8 b) (IntegerValueUint16 $ fromIntegral b)

  H.it "upgrade int8 to int16, not uint16" $
    QC.property $ \b -> checkIntegerAdapter
      [IntegerTypeInt16, IntegerTypeUint16, IntegerTypeBigint]
      IntegerTypeInt8 IntegerTypeInt16 False
      (IntegerValueInt8 b) (IntegerValueInt16 $ fromIntegral b)

  H.it "upgrade uint8 to int16 when uint16 is not supported" $
    QC.property $ \b -> checkIntegerAdapter
      [IntegerTypeInt16, IntegerTypeInt32, IntegerTypeBigint]
      IntegerTypeUint8 IntegerTypeInt16 False
      (IntegerValueUint8 b) (IntegerValueInt16 $ fromIntegral b)

  H.it "cross-convert uint32 to int32, even when uint16 is supported" $
    QC.property $ \b -> checkIntegerAdapter
      [IntegerTypeUint16, IntegerTypeInt32]
      IntegerTypeUint32 IntegerTypeInt32 True
      (IntegerValueUint32 b) (IntegerValueInt32 $ fromIntegral b)

  H.it "downgrade bigint to int32, not uint32" $
    QC.property $ \b -> checkIntegerAdapter
      [IntegerTypeInt16, IntegerTypeUint16, IntegerTypeInt32, IntegerTypeUint32]
      IntegerTypeBigint IntegerTypeInt32 True
      (IntegerValueBigint b) (IntegerValueInt32 $ fromIntegral b)

testAtomicAdapter :: H.SpecWith ()
testAtomicAdapter = H.describe "Test atomic adapter" $ do

  H.it "encode binary data as strings" $
    QC.property $ \b -> checkAtomicAdapter
      [LiteralVariantString]
      LiteralTypeBinary LiteralTypeString False
      (LiteralBinary b) (LiteralString b)

  H.it "encode booleans as strings" $
    QC.property $ \b -> checkAtomicAdapter
      [LiteralVariantString]
      LiteralTypeBoolean LiteralTypeString False
      (LiteralBoolean b) (LiteralString $ if b == BooleanValueTrue then "true" else "false")

  H.it "encode booleans as integers" $
    QC.property $ \b -> checkAtomicAdapter
      [LiteralVariantInteger]
      LiteralTypeBoolean (LiteralTypeInteger IntegerTypeInt16) False
      (LiteralBoolean b) (LiteralInteger $ IntegerValueInt16 $ if b == BooleanValueTrue then 1 else 0)

  H.it "floating-point encoding is delegated to the float adapter" $
    QC.property $ \f -> checkAtomicAdapter
      [LiteralVariantFloat]
      (LiteralTypeFloat FloatTypeBigfloat) (LiteralTypeFloat FloatTypeFloat32) True
      (LiteralFloat $ FloatValueBigfloat f) (LiteralFloat $ FloatValueFloat32 $ realToFrac f)

  H.it "integer encoding is delegated to the integer adapter" $
    QC.property $ \i -> checkAtomicAdapter
      [LiteralVariantInteger]
      (LiteralTypeInteger IntegerTypeBigint) (LiteralTypeInteger IntegerTypeInt32) True
      (LiteralInteger $ IntegerValueBigint i) (LiteralInteger $ IntegerValueInt32 $ fromIntegral i)

  H.it "strings are unchanged" $
    QC.property $ \s -> checkAtomicAdapter
      [LiteralVariantString]
      LiteralTypeString LiteralTypeString False
      (LiteralString s) (LiteralString s)

spec :: H.Spec
spec = do
  testFloatAdapter
  testIntegerAdapter
  testAtomicAdapter
