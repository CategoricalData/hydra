module Hydra.ArbitraryCore where

import Hydra.Core

import qualified Test.QuickCheck as QC


-- Note: this will generally be a "bad" application
instance QC.Arbitrary Application
  where
    arbitrary = Application <$> QC.arbitrary <*> QC.arbitrary
    
instance QC.Arbitrary AtomicType
  where
    arbitrary = QC.oneof [
      pure AtomicTypeBinary,
      pure AtomicTypeBoolean,
      AtomicTypeFloat <$> QC.arbitrary,
      AtomicTypeInteger <$> QC.arbitrary,
      pure AtomicTypeString]
      
instance QC.Arbitrary AtomicValue
  where
    arbitrary = QC.oneof [
      AtomicValueBinary <$> QC.arbitrary,
      AtomicValueBoolean <$> QC.arbitrary,
      AtomicValueFloat <$> QC.arbitrary,
      AtomicValueInteger <$> QC.arbitrary,
      AtomicValueString <$> QC.arbitrary]

instance QC.Arbitrary BooleanValue
  where
    arbitrary = QC.oneof $ pure <$> [ BooleanValueFalse, BooleanValueTrue ]

-- Note: this will generally be a "bad" field
instance QC.Arbitrary Field
  where
    arbitrary = Field <$> QC.arbitrary <*> QC.arbitrary

instance QC.Arbitrary FloatType
  where
    arbitrary = QC.oneof $ pure <$> [
      FloatTypeBigfloat,
      FloatTypeFloat32,
      FloatTypeFloat64]
      
instance QC.Arbitrary FloatValue
  where
    arbitrary = QC.oneof [
      FloatValueBigfloat <$> QC.arbitrary,
      FloatValueFloat32 <$> QC.arbitrary,
      FloatValueFloat64 <$> QC.arbitrary]

instance QC.Arbitrary IntegerType
  where
    arbitrary = QC.oneof $ pure <$> [
      IntegerTypeBigint,
      IntegerTypeInt8,
      IntegerTypeInt16,
      IntegerTypeInt32,
      IntegerTypeInt64,
      IntegerTypeUint8,
      IntegerTypeUint16,
      IntegerTypeUint32,
      IntegerTypeUint64]

instance QC.Arbitrary IntegerValue
  where
    arbitrary = QC.oneof [
      IntegerValueBigint <$> QC.arbitrary,
      IntegerValueInt8 <$> QC.arbitrary,
      IntegerValueInt16 <$> QC.arbitrary,
      IntegerValueInt32 <$> QC.arbitrary,
      IntegerValueInt64 <$> QC.arbitrary,
      IntegerValueUint8 <$> QC.arbitrary,
      IntegerValueUint16 <$> QC.arbitrary,
      IntegerValueUint32 <$> QC.arbitrary,
      IntegerValueUint64 <$> QC.arbitrary]

-- Note: this will generally be a "bad" lambda
instance QC.Arbitrary Lambda
  where
    arbitrary = Lambda <$> QC.arbitrary <*> QC.arbitrary
    
-- Note: this will generally be a "bad" term
instance QC.Arbitrary Term
  where
    arbitrary = QC.oneof [
      TermApplication <$> QC.arbitrary,
      TermAtomic <$> QC.arbitrary,
      TermCases <$> QC.arbitrary,
      TermCompareTo <$> QC.arbitrary,
      pure TermData,
--      -- TermElement requires a graph
--      -- TermFunction requires a context
      TermLambda <$> QC.arbitrary,
      TermList <$> QC.arbitrary,
      TermMap <$> QC.arbitrary,
      TermProjection <$> QC.arbitrary,
      TermRecord <$> QC.arbitrary,
      TermSet <$> QC.arbitrary,
      TermUnion <$> QC.arbitrary,
      TermVariable <$> QC.arbitrary]
