module Hydra.TestData where
  
import Hydra.Core
import Hydra.Impl.Haskell.Dsl
import Hydra.TestGraph

import qualified Data.Map as M
import qualified Data.Set as S


concatType :: Type
concatType = functionType stringType $ functionType stringType stringType

compareStringsType :: Type
compareStringsType = functionType stringType stringType

eitherStringOrInt8Type :: Type
eitherStringOrInt8Type = TypeUnion [FieldType "left" stringType, FieldType "right" int8Type]

exampleProjectionFieldName :: QualifiedFieldName
exampleProjectionFieldName = QualifiedFieldName "firstName" "Person"

exampleProjectionType :: Type
exampleProjectionType = functionType testTypePerson stringType

int32ElementType :: Type
int32ElementType = TypeElement int32Type

int32ElementDataType :: Type
int32ElementDataType = functionType int32ElementType int32Type

latlonRecord :: (Default a, Eq a, Ord a, Read a, Show a) => Int -> Int -> Term a
latlonRecord lat lon = record [Field "lat" $ int32Value lat, Field "lon" $ int32Value lon]

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

makeMap :: (Default a, Eq a, Ord a, Read a, Show a) => [(String, Int)] -> Term a
makeMap keyvals = defaultTerm $ ExpressionMap $ M.fromList $ ((\(k, v) -> (stringValue k, int32Value v)) <$> keyvals)

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

stringList :: (Default a, Eq a, Ord a, Read a, Show a) => [String] -> Term a
stringList strings = list $ stringValue <$> strings

stringOrIntType :: Type
stringOrIntType = TypeUnion [FieldType "left" stringType, FieldType "right" int32Type]

stringSet :: (Default a, Eq a, Ord a, Read a, Show a) => S.Set String -> Term a
stringSet strings = set $ S.fromList $ stringValue <$> S.toList strings

unionTypeForFunctions :: Type -> Type
unionTypeForFunctions dom = TypeUnion [
  FieldType _Function_cases stringType, -- TODO (TypeRecord cases)
  FieldType _Function_compareTo dom,
  FieldType _Function_data unitType,
  FieldType _Function_lambda stringType, -- TODO (TypeRecord [FieldType _Lambda_parameter stringType, FieldType _Lambda_body cod]),
  FieldType _Function_primitive stringType,
  FieldType _Function_projection stringType,
  FieldType _Expression_variable stringType] -- TODO
