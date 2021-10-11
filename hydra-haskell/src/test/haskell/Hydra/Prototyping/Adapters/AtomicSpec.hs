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


baseLanguage :: Language
baseLanguage = hydraCoreLanguage

baseContext :: AdapterContext
baseContext = AdapterContext testContext baseLanguage baseLanguage

withConstraints :: Language_Constraints -> AdapterContext
withConstraints c = baseContext { adapterContextTarget = baseLanguage { languageConstraints = c }}

checkAdapter :: (Eq t, Eq v)
  => (AdapterContext -> t -> Qualified (Adapter t v))
  -> (r -> AdapterContext)
  -> r -> t -> t -> Bool -> v -> v -> Bool
checkAdapter mkAdapter context variants source target lossy vs vt = 
    adapterSource adapter == source
    && adapterTarget adapter == target
    && adapterIsLossy adapter == lossy
    && stepOut step vs == ResultSuccess vt
    && if lossy then True else (stepOut step vs >>= stepIn step) == ResultSuccess vs
  where
    Qualified (Just adapter) _ = mkAdapter (context variants) source
    step = adapterMapping adapter

testFloatAdapters :: H.SpecWith ()
testFloatAdapters = do
    H.describe "Test floating-point mutators" $ do
  
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
  where
    context variants = withConstraints $ (languageConstraints baseLanguage) {
      languageConstraintsFloatVariants = S.fromList variants }
    checkFloatAdapter = checkAdapter floatAdapter context

testIntegerAdapters :: H.SpecWith ()
testIntegerAdapters = do
    H.describe "Test integer mutators" $ do
  
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
  where
    context variants = withConstraints $ (languageConstraints baseLanguage) {
      languageConstraintsIntegerVariants = S.fromList variants }
    checkIntegerAdapter = checkAdapter integerAdapter context

testAtomicAdapter :: H.SpecWith ()
testAtomicAdapter = do
    H.describe "Test atomic mutators" $ do
      
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

      H.it "floating point encoding is delegating to the float adapter" $
        QC.property $ \f -> checkAtomicAdapter
          [AtomicVariantFloat]
          (AtomicTypeFloat FloatTypeBigfloat) (AtomicTypeFloat FloatTypeFloat32) True
          (AtomicValueFloat $ FloatValueBigfloat f) (AtomicValueFloat $ FloatValueFloat32 $ realToFrac f)

      H.it "integer encoding is delegating to the integer adapter" $
        QC.property $ \i -> checkAtomicAdapter
          [AtomicVariantInteger]
          (AtomicTypeInteger IntegerTypeBigint) (AtomicTypeInteger IntegerTypeInt32) True
          (AtomicValueInteger $ IntegerValueBigint i) (AtomicValueInteger $ IntegerValueInt32 $ fromIntegral i)

      H.it "strings are unchanged" $
        QC.property $ \s -> checkAtomicAdapter
          [AtomicVariantString]
          AtomicTypeString AtomicTypeString False
          (AtomicValueString s) (AtomicValueString s)
  where
    context variants = withConstraints $ (languageConstraints baseLanguage) {
        languageConstraintsAtomicVariants = S.fromList variants,
        languageConstraintsFloatVariants = floatVars,
        languageConstraintsIntegerVariants = integerVars }
      where
        floatVars = S.fromList [FloatVariantFloat32]
        integerVars = S.fromList [IntegerVariantInt16, IntegerVariantInt32]
    checkAtomicAdapter = checkAdapter atomicAdapter context

spec :: H.Spec
spec = do
  testFloatAdapters
  testIntegerAdapters
  testAtomicAdapter
