module Hydra.ArbitraryCore where

import Hydra.Core
import Hydra.Impl.Haskell.Dsl

import qualified Control.Monad as CM
import qualified Data.List as L
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

instance QC.Arbitrary Type where
  arbitrary = QC.sized arbitraryType

instance QC.Arbitrary TypedTerm where
  arbitrary = QC.sized arbitraryTypedTerm
     
arbitraryAtomicValue :: AtomicType -> QC.Gen AtomicValue
arbitraryAtomicValue at = case at of
  AtomicTypeBinary -> AtomicValueBinary <$> QC.arbitrary
  AtomicTypeBoolean -> AtomicValueBoolean <$> QC.arbitrary
  AtomicTypeFloat ft -> AtomicValueFloat <$> arbitraryFloatValue ft         
  AtomicTypeInteger it -> AtomicValueInteger <$> arbitraryIntegerValue it
  AtomicTypeString -> AtomicValueString <$> QC.arbitrary

arbitraryField :: FieldType -> Int -> QC.Gen Field
arbitraryField (FieldType fn ft) n = Field fn <$> arbitraryTerm ft n

arbitraryFieldType :: Int -> QC.Gen FieldType
arbitraryFieldType n = FieldType <$> QC.arbitrary <*> arbitraryType n

arbitraryFloatValue :: FloatType -> QC.Gen FloatValue
arbitraryFloatValue ft = case ft of
  FloatTypeBigfloat -> FloatValueBigfloat <$> QC.arbitrary
  FloatTypeFloat32 -> FloatValueFloat32 <$> QC.arbitrary
  FloatTypeFloat64 -> FloatValueFloat64 <$> QC.arbitrary

-- Note: primitive functions and data terms are not currently generated, as they require a context.
--       Lambda terms are not yet generated, either.
arbitraryFunction :: FunctionType -> Int -> QC.Gen Function
arbitraryFunction (FunctionType dom cod) n = QC.oneof $ defaults ++ whenEqual ++ domainSpecific
  where
    n' = decr n
    defaults = [
      -- Note: this simple lambda is a bit of a cheat. We just have to make sure we can generate at least one term
      --       for any supported function type.
      FunctionLambda <$> (Lambda "x" <$> arbitraryTerm cod n')]
     -- Note: two random types will rarely be equal, but it will happen occasionally with simple types
    whenEqual = [FunctionCompareTo <$> arbitraryTerm dom n' | dom == cod]
    domainSpecific = case dom of
      TypeUnion sfields -> [FunctionCases <$> CM.mapM arbitraryCase sfields]
        where
          arbitraryCase (FieldType fn dom') = do
            term <- arbitraryFunction (FunctionType dom' cod) n2
            return $ Field fn $ TermFunction term
          n2 = div n' $ L.length sfields
      TypeRecord sfields -> [FunctionProjection <$> (fieldTypeName <$> QC.elements sfields) | not (L.null sfields)]
      _ -> []

arbitraryIntegerValue :: IntegerType -> QC.Gen IntegerValue
arbitraryIntegerValue it = case it of
  IntegerTypeBigint -> IntegerValueBigint <$> QC.arbitrary
  IntegerTypeInt8 -> IntegerValueInt8 <$> QC.arbitrary
  IntegerTypeInt16 -> IntegerValueInt16 <$> QC.arbitrary
  IntegerTypeInt32 -> IntegerValueInt32 <$> QC.arbitrary
  IntegerTypeInt64 -> IntegerValueInt64 <$> QC.arbitrary
  IntegerTypeUint8 -> IntegerValueUint8 <$> QC.arbitrary
  IntegerTypeUint16 -> IntegerValueUint16 <$> QC.arbitrary
  IntegerTypeUint32 -> IntegerValueUint32 <$> QC.arbitrary
  IntegerTypeUint64 -> IntegerValueUint64 <$> QC.arbitrary

arbitraryList :: Bool -> (Int -> QC.Gen a) -> Int -> QC.Gen [a]
arbitraryList nonempty g n = do
  l <- QC.choose (0, div n 2)
  if nonempty && l == 0
    then do
      x <- g (decr n)
      return [x]
    else QC.vectorOf l (g (div n l))

arbitraryOptional :: (Int -> QC.Gen a) -> Int -> QC.Gen (Maybe a)
arbitraryOptional gen n = do
  b <- QC.arbitrary
  if b || n == 0 then pure Nothing else Just <$> gen (decr n)

arbitraryPair :: (a -> a -> b) -> (Int -> QC.Gen a) -> Int -> QC.Gen b
arbitraryPair c g n = c <$> g n' <*> g n'
  where n' = div n 2

-- Note: variables and function applications are not (currently) generated
arbitraryTerm :: Type -> Int -> QC.Gen Term
arbitraryTerm typ n = case typ of
    TypeAtomic at -> TermAtomic <$> arbitraryAtomicValue at
    TypeFunction ft -> TermFunction <$> arbitraryFunction ft n'
    TypeList lt -> TermList <$> arbitraryList False (arbitraryTerm lt) n'
    TypeMap (MapType kt vt) -> TermMap <$> (M.fromList <$> arbitraryList False arbPair n')
      where
        arbPair n = do
            k <- arbitraryTerm kt n'
            v <- arbitraryTerm vt n'
            return (k, v)
          where
            n' = div n 2
    TypeOptional ot -> TermOptional <$> arbitraryOptional (arbitraryTerm ot) n'
    TypeRecord sfields -> TermRecord <$> arbitraryFields sfields
    TypeSet st -> TermSet <$> (S.fromList <$> arbitraryList False (arbitraryTerm st) n')
    TypeUnion sfields -> TermUnion <$> do
      f <- QC.elements sfields
      let fn = fieldTypeName f
      ft <- arbitraryTerm (fieldTypeType f) n'
      return $ Field fn ft
  where
    n' = decr n
    arbitraryFields sfields = if L.null sfields
        then pure []
        else CM.mapM (`arbitraryField` n2) sfields
      where
        n2 = div n' $ L.length sfields

-- Note: nominal types and element types are not currently generated, as instantiating them requires a context
arbitraryType :: Int -> QC.Gen Type
arbitraryType n = if n == 0 then pure unitType else QC.oneof [
    TypeAtomic <$> QC.arbitrary,
    TypeFunction <$> arbitraryPair FunctionType arbitraryType n',
    TypeList <$> arbitraryType n',
    TypeMap <$> arbitraryPair MapType arbitraryType n',
    TypeOptional <$> arbitraryType n',
    TypeRecord <$> arbitraryList False arbitraryFieldType n',
    TypeSet <$> arbitraryType n',
    TypeUnion <$> arbitraryList True arbitraryFieldType n']   
  where n' = decr n

arbitraryTypedTerm :: Int -> QC.Gen TypedTerm
arbitraryTypedTerm n = do
    typ <- arbitraryType n'
    term <- arbitraryTerm typ n'
    return $ TypedTerm typ term
  where
    n' = div n 2 -- TODO: a term is usually bigger than its type

decr :: Int -> Int
decr n = max 0 (n-1)
