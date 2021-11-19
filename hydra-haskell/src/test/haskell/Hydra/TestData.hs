module Hydra.TestData where
  
import Hydra.V2.Core
import Hydra.Impl.Haskell.Dsl

import qualified Data.Map as M
import qualified Data.Set as S


concatType :: Type
concatType = functionType stringType $ functionType stringType stringType

compareStringsType :: Type
compareStringsType = functionType stringType stringType

eitherStringOrInt8Type :: Type
eitherStringOrInt8Type = TypeUnion [FieldType "left" stringType, FieldType "right" int8Type]

exampleProjectionType :: Type
exampleProjectionType = functionType latLonType int32Type

int32ElementType :: Type
int32ElementType = TypeElement int32Type

int32ElementDataType :: Type
int32ElementDataType = functionType int32ElementType int32Type

latlonRecord :: Int -> Int -> Term
latlonRecord lat lon = ExpressionRecord [Field "lat" $ int32Value lat, Field "lon" $ int32Value lon]

latLonType :: Type
latLonType = TypeRecord [FieldType "lat" int32Type, FieldType "lon" int32Type]

listOfInt8sType :: Type
listOfInt8sType = TypeList int8Type

listOfInt16sType :: Type
listOfInt16sType = TypeList int16Type

listOfListsOfStringsType :: Type
listOfListsOfStringsType = TypeList $ TypeList stringType

listOfSetOfInt32ElementReferencesType :: Type
listOfSetOfInt32ElementReferencesType = TypeList $ TypeSet $ TypeElement int32Type

listOfSetOfStringsType :: Type
listOfSetOfStringsType = TypeList $ TypeSet stringType

listOfStringsType :: Type
listOfStringsType = TypeList stringType

makeMap :: [(String, Int)] -> Term
makeMap keyvals = ExpressionMap $ M.fromList $ ((\(k, v) -> (stringValue k, int32Value v)) <$> keyvals)

mapOfStringsToIntsType :: Type
mapOfStringsToIntsType = mapType stringType int32Type

optionalInt8Type :: Type
optionalInt8Type = TypeOptional int8Type

optionalInt16Type :: Type
optionalInt16Type = TypeOptional int16Type

optionalStringType :: Type
optionalStringType = TypeOptional stringType

setOfStringsType :: Type
setOfStringsType = TypeSet stringType

stringAliasType :: Type
stringAliasType = TypeNominal "StringTypeAlias"

stringList :: [String] -> Term
stringList strings = ExpressionList $ stringValue <$> strings

stringOrIntType :: Type
stringOrIntType = TypeUnion [FieldType "left" stringType, FieldType "right" int32Type]

stringSet :: S.Set String -> Term
stringSet strings = ExpressionSet $ S.fromList $ stringValue <$> S.toList strings

unionTypeForFunctions :: Type -> Type
unionTypeForFunctions dom = TypeUnion [
  FieldType _Function_cases stringType, -- TODO (TypeRecord cases)
  FieldType _Function_compareTo dom,
  FieldType _Function_data unitType,
  FieldType _Function_lambda stringType, -- TODO (TypeRecord [FieldType _Lambda_parameter stringType, FieldType _Lambda_body cod]),
  FieldType _Function_primitive stringType,
  FieldType _Function_projection stringType,
  FieldType _Term_variable stringType] -- TODO
