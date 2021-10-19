module Hydra.ArbitraryCore where

import Hydra.Core

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Test.QuickCheck as QC


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

-- Note: this will generally be a "bad" term, as the type system is not taken into account,
--       and we are not generating a full graph (so element references will not be valid).
instance QC.Arbitrary Term where
  arbitrary = QC.sized arbitraryTerm

instance QC.Arbitrary Type where
  arbitrary = QC.sized arbitraryType

arbitraryField :: Int -> QC.Gen Field
arbitraryField n = Field <$> QC.arbitrary <*> arbitraryTerm n

arbitraryFieldType :: Int -> QC.Gen FieldType
arbitraryFieldType n = FieldType <$> QC.arbitrary <*> arbitraryType n

arbitraryList :: (Int -> QC.Gen a) -> Int -> QC.Gen [a]
arbitraryList g n = do
  l <- QC.choose (0, div n 2)
  QC.vectorOf l (g (div n l)) 

arbitraryPair :: (a -> a -> b) -> (Int -> QC.Gen a) -> Int -> QC.Gen b
arbitraryPair c g n = c <$> g n' <*> g n'
  where n' = div n 2

arbitraryFunction :: Int -> QC.Gen Function
arbitraryFunction n = QC.oneof [
    FunctionCases <$> arbitraryList arbitraryField n',
    FunctionCompareTo <$> arbitraryTerm n',
    FunctionLambda <$> (Lambda <$> QC.arbitrary <*> arbitraryTerm n'),
    pure FunctionData,
    -- FunctionPrimitive requires a context
    FunctionProjection <$> QC.arbitrary]
  where n' = n-1

arbitraryOptional :: (Int -> QC.Gen a) -> Int -> QC.Gen (Maybe a)
arbitraryOptional gen n = do
  b <- QC.arbitrary
  if b then pure Nothing else Just <$> gen n

arbitraryTerm :: Int -> QC.Gen Term
arbitraryTerm n = QC.oneof [
    TermApplication <$> arbitraryPair Application arbitraryTerm n',
    TermAtomic <$> QC.arbitrary,
    TermElement <$> QC.arbitrary,
    TermFunction <$> arbitraryFunction n',
    TermList <$> arbitraryList arbitraryTerm n',
    TermMap <$> (M.fromList <$>
      arbitraryList (arbitraryPair (,) arbitraryTerm) n'),
    TermOptional <$> arbitraryOptional arbitraryTerm n',
    TermRecord <$> arbitraryList arbitraryField n',
    TermSet <$> (S.fromList <$> arbitraryList arbitraryTerm n'),
    TermUnion <$> arbitraryField n',
    TermVariable <$> QC.arbitrary]
  where n' = n-1
  
arbitraryType :: Int -> QC.Gen Type
arbitraryType n = QC.oneof [
    TypeAtomic <$> QC.arbitrary,
    TypeElement <$> arbitraryType n',
    TypeFunction <$> arbitraryPair FunctionType arbitraryType n',
    TypeList <$> arbitraryType n',
    TypeMap <$> arbitraryPair MapType arbitraryType n',
    TypeNominal <$> QC.arbitrary, -- note: this will generally be a "bad" nominal type
    TypeOptional <$> arbitraryType n',
    TypeRecord <$> arbitraryList arbitraryFieldType n',
    TypeSet <$> arbitraryType n',
    TypeUnion <$> arbitraryList arbitraryFieldType n']   
  where n' = n-1
