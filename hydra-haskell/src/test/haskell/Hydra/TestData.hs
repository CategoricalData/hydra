module Hydra.TestData where

import Hydra.Kernel
import Hydra.Dsl.Terms
import Hydra.TestGraph
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import qualified Data.Map as M


concatType :: Type a
concatType = Types.function Types.string $ Types.function Types.string Types.string

compareStringsType :: Type a
compareStringsType = Types.function Types.string Types.string

eitherStringOrInt8Type :: Type a
eitherStringOrInt8Type = TypeUnion $ RowType eitherStringOrInt8TypeName Nothing
  [Types.field "left" Types.string, Types.field "right" Types.int8]

eitherStringOrInt8TypeName :: Name
eitherStringOrInt8TypeName = unqualifyName testNamespace "EitherStringOrInt8"

exampleProjectionType :: Type Kv
exampleProjectionType = Types.function testTypePerson Types.string

listOfInt8sType :: Type a
listOfInt8sType = Types.list Types.int8

listOfInt16sType :: Type a
listOfInt16sType = Types.list Types.int16

listOfListsOfStringsType :: Type a
listOfListsOfStringsType = Types.list $ Types.list Types.string

listOfSetOfStringsType :: Type a
listOfSetOfStringsType = Types.list $ Types.set Types.string

listOfStringsType :: Type a
listOfStringsType = Types.list Types.string

makeMap :: (Eq a, Ord a, Read a, Show a) => [(String, Int)] -> Term a
makeMap keyvals = Terms.map $ M.fromList $ ((\(k, v) -> (string k, int32 v)) <$> keyvals)

mapOfStringsToIntsType :: Type a
mapOfStringsToIntsType = Types.map Types.string Types.int32

optionalInt8Type :: Type a
optionalInt8Type = Types.optional Types.int8

optionalInt16Type :: Type a
optionalInt16Type = Types.optional Types.int16

optionalStringType :: Type a
optionalStringType = Types.optional Types.string

setOfStringsType :: Type a
setOfStringsType = Types.set Types.string

stringAliasType :: Type a
stringAliasType = Types.wrap $ Name "StringTypeAlias"

stringOrIntName :: Name
stringOrIntName = Name "StringOrInt"

stringOrIntType :: Type a
stringOrIntType = TypeUnion $ RowType stringOrIntName Nothing [Types.field "left" Types.string, Types.field "right" Types.int32]

testTypeName :: Name
testTypeName = unqualifyName testNamespace "TestType"
