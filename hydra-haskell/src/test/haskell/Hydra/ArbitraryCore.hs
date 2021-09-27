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
    
instance QC.Arbitrary Type where
  arbitrary = QC.sized arbitraryType

arbitraryFieldList :: Int -> QC.Gen [FieldType]
arbitraryFieldList n = do
  l <- QC.choose (0, div n 2)
  QC.vectorOf l (arbitraryFieldType (div n l))

arbitraryFieldType :: Int -> QC.Gen FieldType
arbitraryFieldType n = FieldType <$> QC.arbitrary <*> arbitraryType n

arbitraryFunctionType :: Int -> QC.Gen FunctionType
arbitraryFunctionType n = FunctionType <$> arbitraryType n' <*> arbitraryType n'
  where n' = div n 2

arbitraryMapType :: Int -> QC.Gen MapType
arbitraryMapType n = MapType <$> arbitraryType n' <*> arbitraryType n'
  where n' = div n 2

arbitraryType :: Int -> QC.Gen Type
arbitraryType n = QC.oneof [
    TypeAtomic <$> QC.arbitrary,
    TypeElement <$> arbitraryType n',
    TypeFunction <$> arbitraryFunctionType n',
    TypeList <$> arbitraryType n',
    TypeMap <$> arbitraryMapType n',
    TypeNominal <$> QC.arbitrary, -- note: this will generally be a "bad" nominal type
    TypeRecord <$> arbitraryFieldList n',
    TypeSet <$> arbitraryType n',
    TypeUnion <$> arbitraryFieldList n']   
  where n' = n-1
