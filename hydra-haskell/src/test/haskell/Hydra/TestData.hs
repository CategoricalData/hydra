module Hydra.TestData where

import Hydra.Core
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Impl.Haskell.Extras
import Hydra.TestGraph
import qualified Hydra.Impl.Haskell.Dsl.Terms as Terms
import qualified Hydra.Impl.Haskell.Dsl.Types as Types

import qualified Data.Map as M


concatType :: Type m
concatType = Types.function Types.string $ Types.function Types.string Types.string

compareStringsType :: Type m
compareStringsType = Types.function Types.string Types.string

eitherStringOrInt8Type :: Type m
eitherStringOrInt8Type = Types.union [Types.field "left" Types.string, Types.field "right" Types.int8]

exampleProjectionType :: Type Meta
exampleProjectionType = Types.function testTypePerson Types.string

int32ElementType :: Type m
int32ElementType = Types.element Types.int32

int32ElementDataType :: Type m
int32ElementDataType = Types.function int32ElementType Types.int32

latlonRecord :: (Eq m, Ord m, Read m, Show m) => Int -> Int -> Term m
latlonRecord lat lon = record [Field (FieldName "lat") $ int32 lat, Field (FieldName "lon") $ int32 lon]

latLonType :: Type m
latLonType = Types.record [Types.field "lat" Types.int32, Types.field "lon" Types.int32]

listOfInt8sType :: Type m
listOfInt8sType = Types.list Types.int8

listOfInt16sType :: Type m
listOfInt16sType = Types.list Types.int16

listOfListsOfStringsType :: Type m
listOfListsOfStringsType = Types.list $ Types.list Types.string

listOfSetOfInt32ElementReferencesType :: Type m
listOfSetOfInt32ElementReferencesType = Types.list $ Types.set $ Types.element Types.int32

listOfSetOfStringsType :: Type m
listOfSetOfStringsType = Types.list $ Types.set Types.string

listOfStringsType :: Type m
listOfStringsType = Types.list Types.string

makeMap :: (Eq a, Ord a, Read a, Show a) => [(String, Int)] -> Term a
makeMap keyvals = Terms.map $ M.fromList $ ((\(k, v) -> (string k, int32 v)) <$> keyvals)

mapOfStringsToIntsType :: Type m
mapOfStringsToIntsType = Types.map Types.string Types.int32

optionalInt8Type :: Type m
optionalInt8Type = Types.optional Types.int8

optionalInt16Type :: Type m
optionalInt16Type = Types.optional Types.int16

optionalStringType :: Type m
optionalStringType = Types.optional Types.string

setOfStringsType :: Type m
setOfStringsType = Types.set Types.string

stringAliasType :: Type m
stringAliasType = Types.nominal $ Name "StringTypeAlias"

stringOrIntType :: Type m
stringOrIntType = Types.union [Types.field "left" Types.string, Types.field "right" Types.int32]

unionTypeForFunctions :: Type m -> Type m
unionTypeForFunctions dom = Types.union [
  FieldType _Elimination_element Types.unit,
  FieldType _Elimination_nominal Types.string,
  FieldType _Elimination_optional Types.string,
  FieldType _Elimination_record Types.string,
  FieldType _Elimination_union Types.string, -- TODO (TypeRecord cases)
  FieldType _Function_compareTo dom,
  FieldType _Function_lambda Types.string, -- TODO (TypeRecord [FieldType _Lambda_parameter Types.string, FieldType _Lambda_body cod]),
  FieldType _Function_primitive Types.string,
  FieldType _Term_variable Types.string]
