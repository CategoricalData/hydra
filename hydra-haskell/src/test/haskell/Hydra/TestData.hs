module Hydra.TestData where

import Hydra.Core
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Impl.Haskell.Extras
import Hydra.TestGraph
import qualified Hydra.Impl.Haskell.Dsl.Types as Types

import qualified Data.Map as M


concatType :: Default m => Type m
concatType = Types.function Types.string $ Types.function Types.string Types.string

compareStringsType :: Default m => Type m
compareStringsType = Types.function Types.string Types.string

eitherStringOrInt8Type :: Default m => Type m
eitherStringOrInt8Type = Types.union [Types.field "left" Types.string, Types.field "right" Types.int8]

exampleProjectionType :: Type Meta
exampleProjectionType = Types.function testTypePerson Types.string

int32ElementType :: Default m => Type m
int32ElementType = Types.element Types.int32

int32ElementDataType :: Default m => Type m
int32ElementDataType = Types.function int32ElementType Types.int32

latlonRecord :: (Default m, Eq m, Ord m, Read m, Show m) => Int -> Int -> Term m
latlonRecord lat lon = record [Field (FieldName "lat") $ int32Value lat, Field (FieldName "lon") $ int32Value lon]

latLonType :: Default m => Type m
latLonType = Types.record [Types.field "lat" Types.int32, Types.field "lon" Types.int32]

listOfInt8sType :: Default m => Type m
listOfInt8sType = Types.list Types.int8

listOfInt16sType :: Default m => Type m
listOfInt16sType = Types.list Types.int16

listOfListsOfStringsType :: Default m => Type m
listOfListsOfStringsType = Types.list $ Types.list Types.string

listOfSetOfInt32ElementReferencesType :: Default m => Type m
listOfSetOfInt32ElementReferencesType = Types.list $ Types.set $ Types.element Types.int32

listOfSetOfStringsType :: Default m => Type m
listOfSetOfStringsType = Types.list $ Types.set Types.string

listOfStringsType :: Default m => Type m
listOfStringsType = Types.list Types.string

makeMap :: (Default a, Eq a, Ord a, Read a, Show a) => [(String, Int)] -> Term a
makeMap keyvals = defaultTerm $ TermExprMap $ M.fromList $ ((\(k, v) -> (stringValue k, int32Value v)) <$> keyvals)

mapOfStringsToIntsType :: Default m => Type m
mapOfStringsToIntsType = Types.map Types.string Types.int32

optionalInt8Type :: Default m => Type m
optionalInt8Type = Types.optional Types.int8

optionalInt16Type :: Default m => Type m
optionalInt16Type = Types.optional Types.int16

optionalStringType :: Default m => Type m
optionalStringType = Types.optional Types.string

setOfStringsType :: Default m => Type m
setOfStringsType = Types.set Types.string

stringAliasType :: Default m => Type m
stringAliasType = Types.nominal $ Name "StringTypeAlias"

stringOrIntType :: Default m => Type m
stringOrIntType = Types.union [Types.field "left" Types.string, Types.field "right" Types.int32]

unionTypeForFunctions :: Default m => Type m -> Type m
unionTypeForFunctions dom = Types.union [
  FieldType _Elimination_element Types.unit,
  FieldType _Elimination_nominal Types.string,
  FieldType _Elimination_optional Types.string,
  FieldType _Elimination_record Types.string,
  FieldType _Elimination_union Types.string, -- TODO (TypeExprRecord cases)
  FieldType _Function_compareTo dom,
  FieldType _Function_lambda Types.string, -- TODO (TypeExprRecord [FieldType _Lambda_parameter Types.string, FieldType _Lambda_body cod]),
  FieldType _Function_primitive Types.string,
  FieldType _TermExpr_variable Types.string]
