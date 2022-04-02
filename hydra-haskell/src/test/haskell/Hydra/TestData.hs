module Hydra.TestData where

import Hydra.Core
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Impl.Haskell.Extras
import Hydra.TestGraph
import qualified Hydra.Impl.Haskell.Dsl.Types as Types

import qualified Data.Map as M
import qualified Data.Set as S


concatType :: Type
concatType = Types.function Types.string $ Types.function Types.string Types.string

compareStringsType :: Type
compareStringsType = Types.function Types.string Types.string

eitherStringOrInt8Type :: Type
eitherStringOrInt8Type = Types.union [Types.field "left" Types.string, Types.field "right" Types.int8]

exampleProjectionType :: Type
exampleProjectionType = Types.function testTypePerson Types.string

int32ElementType :: Type
int32ElementType = TypeElement Types.int32

int32ElementDataType :: Type
int32ElementDataType = Types.function int32ElementType Types.int32

latlonRecord :: (Default a, Eq a, Ord a, Read a, Show a) => Int -> Int -> Term a
latlonRecord lat lon = record [Field "lat" $ int32Value lat, Field "lon" $ int32Value lon]

latLonType :: Type
latLonType = TypeRecord [Types.field "lat" Types.int32, Types.field "lon" Types.int32]

listOfInt8sType :: Type
listOfInt8sType = TypeList Types.int8

listOfInt16sType :: Type
listOfInt16sType = TypeList Types.int16

listOfListsOfStringsType :: Type
listOfListsOfStringsType = TypeList $ TypeList Types.string

listOfSetOfInt32ElementReferencesType :: Type
listOfSetOfInt32ElementReferencesType = TypeList $ TypeSet $ TypeElement Types.int32

listOfSetOfStringsType :: Type
listOfSetOfStringsType = TypeList $ TypeSet Types.string

listOfStringsType :: Type
listOfStringsType = TypeList Types.string

makeMap :: (Default a, Eq a, Ord a, Read a, Show a) => [(String, Int)] -> Term a
makeMap keyvals = defaultTerm $ ExpressionMap $ M.fromList $ ((\(k, v) -> (stringValue k, int32Value v)) <$> keyvals)

mapOfStringsToIntsType :: Type
mapOfStringsToIntsType = Types.map Types.string Types.int32

optionalInt8Type :: Type
optionalInt8Type = TypeOptional Types.int8

optionalInt16Type :: Type
optionalInt16Type = TypeOptional Types.int16

optionalStringType :: Type
optionalStringType = TypeOptional Types.string

setOfStringsType :: Type
setOfStringsType = TypeSet Types.string

stringAliasType :: Type
stringAliasType = Types.nominal "StringTypeAlias"

stringOrIntType :: Type
stringOrIntType = Types.union [Types.field "left" Types.string, Types.field "right" Types.int32]

unionTypeForFunctions :: Type -> Type
unionTypeForFunctions dom = Types.union [
  Types.field _Function_cases Types.string, -- TODO (TypeRecord cases)
  Types.field _Function_compareTo dom,
  Types.field _Function_data Types.unit,
  Types.field _Function_lambda Types.string, -- TODO (TypeRecord [Types.field _Lambda_parameter Types.string, Types.field _Lambda_body cod]),
  Types.field _Function_primitive Types.string,
  Types.field _Function_projection Types.string,
  Types.field _Expression_variable Types.string] -- TODO
