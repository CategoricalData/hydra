module Hydra.TestData where

import Hydra.Kernel
import Hydra.Dsl.Terms
import Hydra.TestGraph
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import qualified Data.Map as M


concatType :: Type m
concatType = Types.function Types.string $ Types.function Types.string Types.string

compareStringsType :: Type m
compareStringsType = Types.function Types.string Types.string

eitherStringOrInt8Type :: Type m
eitherStringOrInt8Type = TypeUnion $ RowType eitherStringOrInt8TypeName Nothing
  [Types.field "left" Types.string, Types.field "right" Types.int8]

eitherStringOrInt8TypeName :: Name
eitherStringOrInt8TypeName = fromQname testNamespace "EitherStringOrInt8"

exampleProjectionType :: Type Meta
exampleProjectionType = Types.function testTypePerson Types.string

int32ElementType :: Type m
int32ElementType = Types.element Types.int32

int32ElementDataType :: Type m
int32ElementDataType = Types.function int32ElementType Types.int32

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
stringAliasType = Types.wrap $ Name "StringTypeAlias"

stringOrIntName :: Name
stringOrIntName = Name "StringOrInt"

stringOrIntType :: Type m
stringOrIntType = TypeUnion $ RowType stringOrIntName Nothing [Types.field "left" Types.string, Types.field "right" Types.int32]

testTypeName :: Name
testTypeName = fromQname testNamespace "TestType"
