module Hydra.ArbitraryCore where

import Hydra.Core
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Impl.Haskell.Extras
import qualified Hydra.Impl.Haskell.Dsl.Types as Types

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Test.QuickCheck as QC


instance QC.Arbitrary LiteralType
  where
    arbitrary = QC.oneof [
      pure LiteralTypeBinary,
      pure LiteralTypeBoolean,
      LiteralTypeFloat <$> QC.arbitrary,
      LiteralTypeInteger <$> QC.arbitrary,
      pure LiteralTypeString]

instance QC.Arbitrary Literal
  where
    arbitrary = QC.oneof [
      LiteralBinary <$> QC.arbitrary,
      LiteralBoolean <$> QC.arbitrary,
      LiteralFloat <$> QC.arbitrary,
      LiteralInteger <$> QC.arbitrary,
      LiteralString <$> QC.arbitrary]

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

instance (Default m, Eq m, Ord m, Read m, Show m) => QC.Arbitrary (Data m) where
  arbitrary = (\(TypedData _ term) -> term) <$> QC.sized arbitraryTypedData

instance Default m => QC.Arbitrary (Type m) where
  arbitrary = QC.sized arbitraryType
  shrink typ = case typeTerm typ of
    TypeTermLiteral at -> Types.literal <$> case at of
      LiteralTypeInteger _ -> [LiteralTypeBoolean]
      LiteralTypeFloat _ -> [LiteralTypeBoolean]
      _ -> []
    _ -> [] -- TODO

instance (Default m, Eq m, Ord m, Read m, Show m) => QC.Arbitrary (TypedData m) where
  arbitrary = QC.sized arbitraryTypedData
  shrink (TypedData typ term) = L.concat ((\(t, m) -> TypedData t <$> m term) <$> shrinkers typ)

arbitraryLiteral :: LiteralType -> QC.Gen Literal
arbitraryLiteral at = case at of
  LiteralTypeBinary -> LiteralBinary <$> QC.arbitrary
  LiteralTypeBoolean -> LiteralBoolean <$> QC.arbitrary
  LiteralTypeFloat ft -> LiteralFloat <$> arbitraryFloatValue ft
  LiteralTypeInteger it -> LiteralInteger <$> arbitraryIntegerValue it
  LiteralTypeString -> LiteralString <$> QC.arbitrary

arbitraryField :: (Default m, Eq m, Ord m, Read m, Show m) => FieldType m -> Int -> QC.Gen (Field m)
arbitraryField (FieldType fn ft) n = Field fn <$> arbitraryData ft n

arbitraryFieldType :: Default m => Int -> QC.Gen (FieldType m)
arbitraryFieldType n = FieldType <$> QC.arbitrary <*> arbitraryType n

arbitraryFloatValue :: FloatType -> QC.Gen FloatValue
arbitraryFloatValue ft = case ft of
  FloatTypeBigfloat -> FloatValueBigfloat <$> QC.arbitrary
  FloatTypeFloat32 -> FloatValueFloat32 <$> QC.arbitrary
  FloatTypeFloat64 -> FloatValueFloat64 <$> QC.arbitrary

-- Note: primitive functions and data terms are not currently generated, as they require a context.
arbitraryFunction :: (Default m, Eq m, Ord m, Read m, Show m) => FunctionType m -> Int -> QC.Gen (Function m)
arbitraryFunction (FunctionType dom cod) n = QC.oneof $ defaults ++ whenEqual ++ domainSpecific
  where
    n' = decr n
    defaults = [
      -- Note: this simple lambda is a bit of a cheat. We just have to make sure we can generate at least one term
      --       for any supported function type.
      FunctionLambda <$> (Lambda "x" <$> arbitraryData cod n')]
     -- Note: two random types will rarely be equal, but it will happen occasionally with simple types
    whenEqual = [FunctionCompareTo <$> arbitraryData dom n' | dom == cod]
    domainSpecific = case typeTerm dom of
      TypeTermUnion sfields -> [FunctionCases <$> CM.mapM arbitraryCase sfields]
        where
          arbitraryCase (FieldType fn dom') = do
            term <- arbitraryFunction (FunctionType dom' cod) n2
            return $ Field fn $ defaultData $ DataTermFunction term
          n2 = div n' $ L.length sfields
        -- Note: projections now require nominally-typed records
--      TypeTermRecord sfields -> [FunctionProjection <$> (fieldTypeName <$> QC.elements sfields) | not (L.null sfields)]
--      TypeTermOptional ot -> [FunctionOptionalCases <$> (
--        OptionalCases <$> arbitraryData cod n'
--          <*> (defaultData . DataTermFunction
--          <$> arbitraryFunction (FunctionType ot cod) n'))]
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
arbitraryData :: (Default m, Eq m, Ord m, Read m, Show m) => Type m -> Int -> QC.Gen (Data m)
arbitraryData typ n = case typeTerm typ of
    TypeTermLiteral at -> atomic <$> arbitraryLiteral at
    TypeTermFunction ft -> defaultData . DataTermFunction <$> arbitraryFunction ft n'
    TypeTermList lt -> list <$> arbitraryList False (arbitraryData lt) n'
    TypeTermMap (MapType kt vt) -> defaultData . DataTermMap <$> (M.fromList <$> arbitraryList False arbPair n')
      where
        arbPair n = do
            k <- arbitraryData kt n'
            v <- arbitraryData vt n'
            return (k, v)
          where
            n' = div n 2
    TypeTermOptional ot -> optional <$> arbitraryOptional (arbitraryData ot) n'
    TypeTermRecord sfields -> record <$> arbitraryFields sfields
    TypeTermSet st -> set <$> (S.fromList <$> arbitraryList False (arbitraryData st) n')
    TypeTermUnion sfields -> union <$> do
      f <- QC.elements sfields
      let fn = fieldTypeName f
      ft <- arbitraryData (fieldTypeType f) n'
      return $ Field fn ft
  where
    n' = decr n
    arbitraryFields sfields = if L.null sfields
        then pure []
        else CM.mapM (`arbitraryField` n2) sfields
      where
        n2 = div n' $ L.length sfields

-- Note: nominal types and element types are not currently generated, as instantiating them requires a context
arbitraryType :: Default m => Int -> QC.Gen (Type m)
arbitraryType n = if n == 0 then pure Types.unit else Types.defaultType <$> QC.oneof [
    TypeTermLiteral <$> QC.arbitrary,
    TypeTermFunction <$> arbitraryPair FunctionType arbitraryType n',
    TypeTermList <$> arbitraryType n',
    TypeTermMap <$> arbitraryPair MapType arbitraryType n',
    TypeTermOptional <$> arbitraryType n',
    TypeTermRecord <$> arbitraryList False arbitraryFieldType n', -- TODO: avoid duplicate field names
    TypeTermSet <$> arbitraryType n',
    TypeTermUnion <$> arbitraryList True arbitraryFieldType n'] -- TODO: avoid duplicate field names
  where n' = decr n

arbitraryTypedData :: (Default m, Eq m, Ord m, Read m, Show m) => Int -> QC.Gen (TypedData m)
arbitraryTypedData n = do
    typ <- arbitraryType n'
    term <- arbitraryData typ n'
    return $ TypedData typ term
  where
    n' = div n 2 -- TODO: a term is usually bigger than its type

decr :: Int -> Int
decr n = max 0 (n-1)

-- Note: shrinking currently discards any metadata
shrinkers :: (Default m, Eq m, Ord m, Read m, Show m) => Type m -> [(Type m, (Data m) -> [Data m])]
shrinkers typ = trivialShrinker ++ case typeTerm typ of
    TypeTermLiteral at -> case at of
      LiteralTypeBinary -> [(Types.binary, \(Data (DataTermLiteral (LiteralBinary s)) _) -> binaryData <$> QC.shrink s)]
      LiteralTypeBoolean -> []
      LiteralTypeFloat ft -> []
      LiteralTypeInteger it -> []
      LiteralTypeString -> [(Types.string, \(Data (DataTermLiteral (LiteralString s)) _) -> stringValue <$> QC.shrink s)]
  --  TypeTermElement et ->
  --  TypeTermFunction ft ->
    TypeTermList lt -> dropElements : promoteType : shrinkType
      where
        dropElements = (Types.list lt, \(Data (DataTermList els) _) -> list <$> dropAny els)
        promoteType = (lt, \(Data (DataTermList els) _) -> els)
        shrinkType = (\(t, m) -> (Types.list t, \(Data (DataTermList els) _) -> list <$> CM.mapM m els)) <$> shrinkers lt
    TypeTermMap (MapType kt vt) -> shrinkKeys ++ shrinkValues ++ dropPairs
      where
        shrinkKeys = (\(t, m) -> (Types.map t vt,
            \(Data (DataTermMap mp) _) -> defaultData . DataTermMap . M.fromList <$> (shrinkPair m <$> M.toList mp))) <$> shrinkers kt
          where
            shrinkPair m (km, vm) = (\km' -> (km', vm)) <$> m km
        shrinkValues = (\(t, m) -> (Types.map kt t,
            \(Data (DataTermMap mp) _) -> defaultData . DataTermMap . M.fromList <$> (shrinkPair m <$> M.toList mp))) <$> shrinkers vt
          where
            shrinkPair m (km, vm) = (\vm' -> (km, vm')) <$> m vm
        dropPairs = [(Types.map kt vt, \(Data (DataTermMap m) _) -> defaultData . DataTermMap . M.fromList <$> dropAny (M.toList m))]
  --  TypeTermNominal name ->
    TypeTermOptional ot -> toNothing : promoteType : shrinkType
      where
        toNothing = (Types.optional ot, \(Data (DataTermOptional m) _) -> optional <$> Y.maybe [] (const [Nothing]) m)
        promoteType = (ot, \(Data (DataTermOptional m) _) -> Y.maybeToList m)
        shrinkType = (\(t, m) -> (Types.optional t,
          \(Data (DataTermOptional mb) _) -> Y.maybe [] (fmap (optional . Just) . m) mb)) <$> shrinkers ot
    TypeTermRecord sfields -> dropFields
        ++ shrinkFieldNames Types.record record (\(Data (DataTermRecord dfields) _) -> dfields) sfields
        ++ promoteTypes ++ shrinkTypes
      where
        dropFields = dropField <$> indices
          where
            dropField i = (Types.record $ dropIth i sfields, \(Data (DataTermRecord dfields) _)
              -> [record $ dropIth i dfields])
        promoteTypes = promoteField <$> indices
          where
            promoteField i = (fieldTypeType $ sfields !! i, \(Data (DataTermRecord dfields) _)
              -> [fieldData $ dfields !! i])
        shrinkTypes = [] -- TODO
        indices = [0..(L.length sfields - 1)]
    TypeTermSet st -> dropElements : promoteType : shrinkType
      where
        dropElements = (Types.set st, \(Data (DataTermSet els) _) -> set . S.fromList <$> dropAny (S.toList els))
        promoteType = (st, \(Data (DataTermSet els) _) -> S.toList els)
        shrinkType = (\(t, m) -> (Types.set t, \(Data (DataTermSet els) _) -> set . S.fromList <$> CM.mapM m (S.toList els))) <$> shrinkers st
    TypeTermUnion sfields -> dropFields
        ++ shrinkFieldNames Types.union (union . L.head) (\(Data (DataTermUnion f) _) -> [f]) sfields
        ++ promoteTypes ++ shrinkTypes
      where
        dropFields = [] -- TODO
        promoteTypes = [] -- TODO
        shrinkTypes = [] -- TODO
    _ -> []
  where
    dropAny l = case l of
      [] -> []
      (h:r) -> [r] ++ ((h :) <$> dropAny r)
    dropIth i l = L.take i l ++ L.drop (i+1) l
    nodupes l = L.length (L.nub l) == L.length l
    trivialShrinker = [(Types.unit, const [unitData]) | typ /= Types.unit]
    shrinkFieldNames toType toData fromData sfields = forNames <$> altNames
      where
        forNames names = (toType $ withFieldTypeNames names sfields,
           \term -> [toData $ withFieldNames names $ fromData term])
        altNames = L.filter nodupes $ CM.mapM QC.shrink (fieldTypeName <$> sfields)
        withFieldTypeNames = L.zipWith (\n f -> FieldType n $ fieldTypeType f)
        withFieldNames = L.zipWith (\n f -> Field n $ fieldData f)

-- | A placeholder for a type name. Use in tests only, where a union term is needed but no type name is known.
untyped :: Name
untyped = show (dflt :: Meta)
