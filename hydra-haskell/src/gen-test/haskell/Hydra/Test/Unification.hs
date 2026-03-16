-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for type unification operations

module Hydra.Test.Unification where

import qualified Hydra.Core as Core
import qualified Hydra.Testing as Testing
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Test cases for type unification operations
allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
  Testing.testGroupName = "unification",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "variableOccursInType",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable occurs in itself",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseVariableOccursInType (Testing.VariableOccursInTypeTestCase {
            Testing.variableOccursInTypeTestCaseVariable = (Core.Name "a"),
            Testing.variableOccursInTypeTestCaseType = (Core.TypeVariable (Core.Name "a")),
            Testing.variableOccursInTypeTestCaseExpected = True})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable does not occur in different variable",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseVariableOccursInType (Testing.VariableOccursInTypeTestCase {
            Testing.variableOccursInTypeTestCaseVariable = (Core.Name "a"),
            Testing.variableOccursInTypeTestCaseType = (Core.TypeVariable (Core.Name "b")),
            Testing.variableOccursInTypeTestCaseExpected = False})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable does not occur in int32",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseVariableOccursInType (Testing.VariableOccursInTypeTestCase {
            Testing.variableOccursInTypeTestCaseVariable = (Core.Name "a"),
            Testing.variableOccursInTypeTestCaseType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Testing.variableOccursInTypeTestCaseExpected = False})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable does not occur in string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseVariableOccursInType (Testing.VariableOccursInTypeTestCase {
            Testing.variableOccursInTypeTestCaseVariable = (Core.Name "a"),
            Testing.variableOccursInTypeTestCaseType = (Core.TypeLiteral Core.LiteralTypeString),
            Testing.variableOccursInTypeTestCaseExpected = False})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable occurs in list element type",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseVariableOccursInType (Testing.VariableOccursInTypeTestCase {
            Testing.variableOccursInTypeTestCaseVariable = (Core.Name "a"),
            Testing.variableOccursInTypeTestCaseType = (Core.TypeList (Core.TypeVariable (Core.Name "a"))),
            Testing.variableOccursInTypeTestCaseExpected = True})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable does not occur in list of different type",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseVariableOccursInType (Testing.VariableOccursInTypeTestCase {
            Testing.variableOccursInTypeTestCaseVariable = (Core.Name "a"),
            Testing.variableOccursInTypeTestCaseType = (Core.TypeList (Core.TypeVariable (Core.Name "b"))),
            Testing.variableOccursInTypeTestCaseExpected = False})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable occurs in function domain",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseVariableOccursInType (Testing.VariableOccursInTypeTestCase {
            Testing.variableOccursInTypeTestCaseVariable = (Core.Name "a"),
            Testing.variableOccursInTypeTestCaseType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
              Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
            Testing.variableOccursInTypeTestCaseExpected = True})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable occurs in function codomain",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseVariableOccursInType (Testing.VariableOccursInTypeTestCase {
            Testing.variableOccursInTypeTestCaseVariable = (Core.Name "a"),
            Testing.variableOccursInTypeTestCaseType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
            Testing.variableOccursInTypeTestCaseExpected = True})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable does not occur in function with different vars",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseVariableOccursInType (Testing.VariableOccursInTypeTestCase {
            Testing.variableOccursInTypeTestCaseVariable = (Core.Name "a"),
            Testing.variableOccursInTypeTestCaseType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "c"))})),
            Testing.variableOccursInTypeTestCaseExpected = False})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable occurs in optional type",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseVariableOccursInType (Testing.VariableOccursInTypeTestCase {
            Testing.variableOccursInTypeTestCaseVariable = (Core.Name "a"),
            Testing.variableOccursInTypeTestCaseType = (Core.TypeMaybe (Core.TypeVariable (Core.Name "a"))),
            Testing.variableOccursInTypeTestCaseExpected = True})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable occurs in pair first",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseVariableOccursInType (Testing.VariableOccursInTypeTestCase {
            Testing.variableOccursInTypeTestCaseVariable = (Core.Name "a"),
            Testing.variableOccursInTypeTestCaseType = (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeVariable (Core.Name "a")),
              Core.pairTypeSecond = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
            Testing.variableOccursInTypeTestCaseExpected = True})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable occurs in pair second",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseVariableOccursInType (Testing.VariableOccursInTypeTestCase {
            Testing.variableOccursInTypeTestCaseVariable = (Core.Name "a"),
            Testing.variableOccursInTypeTestCaseType = (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.pairTypeSecond = (Core.TypeVariable (Core.Name "a"))})),
            Testing.variableOccursInTypeTestCaseExpected = True})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable occurs in either left",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseVariableOccursInType (Testing.VariableOccursInTypeTestCase {
            Testing.variableOccursInTypeTestCaseVariable = (Core.Name "a"),
            Testing.variableOccursInTypeTestCaseType = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "a")),
              Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
            Testing.variableOccursInTypeTestCaseExpected = True})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable occurs in either right",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseVariableOccursInType (Testing.VariableOccursInTypeTestCase {
            Testing.variableOccursInTypeTestCaseVariable = (Core.Name "a"),
            Testing.variableOccursInTypeTestCaseType = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.eitherTypeRight = (Core.TypeVariable (Core.Name "a"))})),
            Testing.variableOccursInTypeTestCaseExpected = True})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable occurs in map key type",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseVariableOccursInType (Testing.VariableOccursInTypeTestCase {
            Testing.variableOccursInTypeTestCaseVariable = (Core.Name "a"),
            Testing.variableOccursInTypeTestCaseType = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeVariable (Core.Name "a")),
              Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
            Testing.variableOccursInTypeTestCaseExpected = True})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable occurs in map value type",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseVariableOccursInType (Testing.VariableOccursInTypeTestCase {
            Testing.variableOccursInTypeTestCaseVariable = (Core.Name "a"),
            Testing.variableOccursInTypeTestCaseType = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
              Core.mapTypeValues = (Core.TypeVariable (Core.Name "a"))})),
            Testing.variableOccursInTypeTestCaseExpected = True})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable occurs in set type",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseVariableOccursInType (Testing.VariableOccursInTypeTestCase {
            Testing.variableOccursInTypeTestCaseVariable = (Core.Name "a"),
            Testing.variableOccursInTypeTestCaseType = (Core.TypeSet (Core.TypeVariable (Core.Name "a"))),
            Testing.variableOccursInTypeTestCaseExpected = True})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable occurs in nested list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseVariableOccursInType (Testing.VariableOccursInTypeTestCase {
            Testing.variableOccursInTypeTestCaseVariable = (Core.Name "a"),
            Testing.variableOccursInTypeTestCaseType = (Core.TypeList (Core.TypeList (Core.TypeVariable (Core.Name "a")))),
            Testing.variableOccursInTypeTestCaseExpected = True})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable occurs in list of functions",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseVariableOccursInType (Testing.VariableOccursInTypeTestCase {
            Testing.variableOccursInTypeTestCaseVariable = (Core.Name "a"),
            Testing.variableOccursInTypeTestCaseType = (Core.TypeList (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))),
            Testing.variableOccursInTypeTestCaseExpected = True})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable does not occur in complex type without it",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseVariableOccursInType (Testing.VariableOccursInTypeTestCase {
            Testing.variableOccursInTypeTestCaseVariable = (Core.Name "a"),
            Testing.variableOccursInTypeTestCaseType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
              Core.functionTypeCodomain = (Core.TypeMaybe (Core.TypePair (Core.PairType {
                Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeString),
                Core.pairTypeSecond = (Core.TypeVariable (Core.Name "b"))})))})),
            Testing.variableOccursInTypeTestCaseExpected = False})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable occurs deep in complex type",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseVariableOccursInType (Testing.VariableOccursInTypeTestCase {
            Testing.variableOccursInTypeTestCaseVariable = (Core.Name "a"),
            Testing.variableOccursInTypeTestCaseType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
              Core.functionTypeCodomain = (Core.TypeMaybe (Core.TypePair (Core.PairType {
                Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeString),
                Core.pairTypeSecond = (Core.TypeVariable (Core.Name "a"))})))})),
            Testing.variableOccursInTypeTestCaseExpected = True})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable occurs in forAll body",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseVariableOccursInType (Testing.VariableOccursInTypeTestCase {
            Testing.variableOccursInTypeTestCaseVariable = (Core.Name "a"),
            Testing.variableOccursInTypeTestCaseType = (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "b"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))})),
            Testing.variableOccursInTypeTestCaseExpected = True})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable occurs in forAll bound position",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseVariableOccursInType (Testing.VariableOccursInTypeTestCase {
            Testing.variableOccursInTypeTestCaseVariable = (Core.Name "a"),
            Testing.variableOccursInTypeTestCaseType = (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "a"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))})),
            Testing.variableOccursInTypeTestCaseExpected = True})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "unifyTypes",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unify identical int32 types",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUnifyTypes (Testing.UnifyTypesTestCase {
            Testing.unifyTypesTestCaseSchemaTypes = [],
            Testing.unifyTypesTestCaseLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Testing.unifyTypesTestCaseRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Testing.unifyTypesTestCaseExpected = (Right (Typing.TypeSubst M.empty))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unify identical string types",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUnifyTypes (Testing.UnifyTypesTestCase {
            Testing.unifyTypesTestCaseSchemaTypes = [],
            Testing.unifyTypesTestCaseLeft = (Core.TypeLiteral Core.LiteralTypeString),
            Testing.unifyTypesTestCaseRight = (Core.TypeLiteral Core.LiteralTypeString),
            Testing.unifyTypesTestCaseExpected = (Right (Typing.TypeSubst M.empty))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unify identical variable types",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUnifyTypes (Testing.UnifyTypesTestCase {
            Testing.unifyTypesTestCaseSchemaTypes = [],
            Testing.unifyTypesTestCaseLeft = (Core.TypeVariable (Core.Name "a")),
            Testing.unifyTypesTestCaseRight = (Core.TypeVariable (Core.Name "a")),
            Testing.unifyTypesTestCaseExpected = (Right (Typing.TypeSubst M.empty))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unify variable with int32",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUnifyTypes (Testing.UnifyTypesTestCase {
            Testing.unifyTypesTestCaseSchemaTypes = [],
            Testing.unifyTypesTestCaseLeft = (Core.TypeVariable (Core.Name "a")),
            Testing.unifyTypesTestCaseRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Testing.unifyTypesTestCaseExpected = (Right (Typing.TypeSubst (M.fromList [
              (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))])))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unify int32 with variable",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUnifyTypes (Testing.UnifyTypesTestCase {
            Testing.unifyTypesTestCaseSchemaTypes = [],
            Testing.unifyTypesTestCaseLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Testing.unifyTypesTestCaseRight = (Core.TypeVariable (Core.Name "a")),
            Testing.unifyTypesTestCaseExpected = (Right (Typing.TypeSubst (M.fromList [
              (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))])))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unify two different variables",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUnifyTypes (Testing.UnifyTypesTestCase {
            Testing.unifyTypesTestCaseSchemaTypes = [],
            Testing.unifyTypesTestCaseLeft = (Core.TypeVariable (Core.Name "a")),
            Testing.unifyTypesTestCaseRight = (Core.TypeVariable (Core.Name "b")),
            Testing.unifyTypesTestCaseExpected = (Right (Typing.TypeSubst (M.fromList [
              (Core.Name "a", (Core.TypeVariable (Core.Name "b")))])))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unify list of variables with list of int32",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUnifyTypes (Testing.UnifyTypesTestCase {
            Testing.unifyTypesTestCaseSchemaTypes = [],
            Testing.unifyTypesTestCaseLeft = (Core.TypeList (Core.TypeVariable (Core.Name "a"))),
            Testing.unifyTypesTestCaseRight = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
            Testing.unifyTypesTestCaseExpected = (Right (Typing.TypeSubst (M.fromList [
              (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))])))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unify identical list types",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUnifyTypes (Testing.UnifyTypesTestCase {
            Testing.unifyTypesTestCaseSchemaTypes = [],
            Testing.unifyTypesTestCaseLeft = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)),
            Testing.unifyTypesTestCaseRight = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)),
            Testing.unifyTypesTestCaseExpected = (Right (Typing.TypeSubst M.empty))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unify function types with variables",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUnifyTypes (Testing.UnifyTypesTestCase {
            Testing.unifyTypesTestCaseSchemaTypes = [],
            Testing.unifyTypesTestCaseLeft = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "b"))})),
            Testing.unifyTypesTestCaseRight = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})),
            Testing.unifyTypesTestCaseExpected = (Right (Typing.TypeSubst (M.fromList [
              (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
              (Core.Name "b", (Core.TypeLiteral Core.LiteralTypeString))])))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unify identical function types",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUnifyTypes (Testing.UnifyTypesTestCase {
            Testing.unifyTypesTestCaseSchemaTypes = [],
            Testing.unifyTypesTestCaseLeft = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})),
            Testing.unifyTypesTestCaseRight = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})),
            Testing.unifyTypesTestCaseExpected = (Right (Typing.TypeSubst M.empty))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unify optional types",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUnifyTypes (Testing.UnifyTypesTestCase {
            Testing.unifyTypesTestCaseSchemaTypes = [],
            Testing.unifyTypesTestCaseLeft = (Core.TypeMaybe (Core.TypeVariable (Core.Name "a"))),
            Testing.unifyTypesTestCaseRight = (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
            Testing.unifyTypesTestCaseExpected = (Right (Typing.TypeSubst (M.fromList [
              (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))])))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unify pair types",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUnifyTypes (Testing.UnifyTypesTestCase {
            Testing.unifyTypesTestCaseSchemaTypes = [],
            Testing.unifyTypesTestCaseLeft = (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeVariable (Core.Name "a")),
              Core.pairTypeSecond = (Core.TypeVariable (Core.Name "b"))})),
            Testing.unifyTypesTestCaseRight = (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)})),
            Testing.unifyTypesTestCaseExpected = (Right (Typing.TypeSubst (M.fromList [
              (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
              (Core.Name "b", (Core.TypeLiteral Core.LiteralTypeString))])))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unify either types",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUnifyTypes (Testing.UnifyTypesTestCase {
            Testing.unifyTypesTestCaseSchemaTypes = [],
            Testing.unifyTypesTestCaseLeft = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "a")),
              Core.eitherTypeRight = (Core.TypeVariable (Core.Name "b"))})),
            Testing.unifyTypesTestCaseRight = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeString)})),
            Testing.unifyTypesTestCaseExpected = (Right (Typing.TypeSubst (M.fromList [
              (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
              (Core.Name "b", (Core.TypeLiteral Core.LiteralTypeString))])))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unify map types",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUnifyTypes (Testing.UnifyTypesTestCase {
            Testing.unifyTypesTestCaseSchemaTypes = [],
            Testing.unifyTypesTestCaseLeft = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
              Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))})),
            Testing.unifyTypesTestCaseRight = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
              Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
            Testing.unifyTypesTestCaseExpected = (Right (Typing.TypeSubst (M.fromList [
              (Core.Name "k", (Core.TypeLiteral Core.LiteralTypeString)),
              (Core.Name "v", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))])))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unify set types",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUnifyTypes (Testing.UnifyTypesTestCase {
            Testing.unifyTypesTestCaseSchemaTypes = [],
            Testing.unifyTypesTestCaseLeft = (Core.TypeSet (Core.TypeVariable (Core.Name "a"))),
            Testing.unifyTypesTestCaseRight = (Core.TypeSet (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
            Testing.unifyTypesTestCaseExpected = (Right (Typing.TypeSubst (M.fromList [
              (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))])))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unify unit types",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUnifyTypes (Testing.UnifyTypesTestCase {
            Testing.unifyTypesTestCaseSchemaTypes = [],
            Testing.unifyTypesTestCaseLeft = Core.TypeUnit,
            Testing.unifyTypesTestCaseRight = Core.TypeUnit,
            Testing.unifyTypesTestCaseExpected = (Right (Typing.TypeSubst M.empty))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "fail to unify int32 with string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUnifyTypes (Testing.UnifyTypesTestCase {
            Testing.unifyTypesTestCaseSchemaTypes = [],
            Testing.unifyTypesTestCaseLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Testing.unifyTypesTestCaseRight = (Core.TypeLiteral Core.LiteralTypeString),
            Testing.unifyTypesTestCaseExpected = (Left "cannot unify")})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "fail to unify list with function",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUnifyTypes (Testing.UnifyTypesTestCase {
            Testing.unifyTypesTestCaseSchemaTypes = [],
            Testing.unifyTypesTestCaseLeft = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
            Testing.unifyTypesTestCaseRight = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
            Testing.unifyTypesTestCaseExpected = (Left "cannot unify")})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "occur check: variable with list containing it",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUnifyTypes (Testing.UnifyTypesTestCase {
            Testing.unifyTypesTestCaseSchemaTypes = [],
            Testing.unifyTypesTestCaseLeft = (Core.TypeVariable (Core.Name "a")),
            Testing.unifyTypesTestCaseRight = (Core.TypeList (Core.TypeVariable (Core.Name "a"))),
            Testing.unifyTypesTestCaseExpected = (Left "appears free")})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "joinTypes",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "join identical int32",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJoinTypes (Testing.JoinTypesTestCase {
            Testing.joinTypesTestCaseLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Testing.joinTypesTestCaseRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Testing.joinTypesTestCaseExpected = (Right [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "join identical string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJoinTypes (Testing.JoinTypesTestCase {
            Testing.joinTypesTestCaseLeft = (Core.TypeLiteral Core.LiteralTypeString),
            Testing.joinTypesTestCaseRight = (Core.TypeLiteral Core.LiteralTypeString),
            Testing.joinTypesTestCaseExpected = (Right [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "join list types",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJoinTypes (Testing.JoinTypesTestCase {
            Testing.joinTypesTestCaseLeft = (Core.TypeList (Core.TypeVariable (Core.Name "a"))),
            Testing.joinTypesTestCaseRight = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
            Testing.joinTypesTestCaseExpected = (Right [
              Typing.TypeConstraint {
                Typing.typeConstraintLeft = (Core.TypeVariable (Core.Name "a")),
                Typing.typeConstraintRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Typing.typeConstraintComment = "join types; test"}])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "join function types",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJoinTypes (Testing.JoinTypesTestCase {
            Testing.joinTypesTestCaseLeft = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "b"))})),
            Testing.joinTypesTestCaseRight = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})),
            Testing.joinTypesTestCaseExpected = (Right [
              Typing.TypeConstraint {
                Typing.typeConstraintLeft = (Core.TypeVariable (Core.Name "a")),
                Typing.typeConstraintRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Typing.typeConstraintComment = "join types; test"},
              Typing.TypeConstraint {
                Typing.typeConstraintLeft = (Core.TypeVariable (Core.Name "b")),
                Typing.typeConstraintRight = (Core.TypeLiteral Core.LiteralTypeString),
                Typing.typeConstraintComment = "join types; test"}])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "join optional types",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJoinTypes (Testing.JoinTypesTestCase {
            Testing.joinTypesTestCaseLeft = (Core.TypeMaybe (Core.TypeVariable (Core.Name "a"))),
            Testing.joinTypesTestCaseRight = (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
            Testing.joinTypesTestCaseExpected = (Right [
              Typing.TypeConstraint {
                Typing.typeConstraintLeft = (Core.TypeVariable (Core.Name "a")),
                Typing.typeConstraintRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Typing.typeConstraintComment = "join types; test"}])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "join pair types",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJoinTypes (Testing.JoinTypesTestCase {
            Testing.joinTypesTestCaseLeft = (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeVariable (Core.Name "a")),
              Core.pairTypeSecond = (Core.TypeVariable (Core.Name "b"))})),
            Testing.joinTypesTestCaseRight = (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)})),
            Testing.joinTypesTestCaseExpected = (Right [
              Typing.TypeConstraint {
                Typing.typeConstraintLeft = (Core.TypeVariable (Core.Name "a")),
                Typing.typeConstraintRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Typing.typeConstraintComment = "join types; test"},
              Typing.TypeConstraint {
                Typing.typeConstraintLeft = (Core.TypeVariable (Core.Name "b")),
                Typing.typeConstraintRight = (Core.TypeLiteral Core.LiteralTypeString),
                Typing.typeConstraintComment = "join types; test"}])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "join either types",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJoinTypes (Testing.JoinTypesTestCase {
            Testing.joinTypesTestCaseLeft = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "a")),
              Core.eitherTypeRight = (Core.TypeVariable (Core.Name "b"))})),
            Testing.joinTypesTestCaseRight = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeString)})),
            Testing.joinTypesTestCaseExpected = (Right [
              Typing.TypeConstraint {
                Typing.typeConstraintLeft = (Core.TypeVariable (Core.Name "a")),
                Typing.typeConstraintRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Typing.typeConstraintComment = "join types; test"},
              Typing.TypeConstraint {
                Typing.typeConstraintLeft = (Core.TypeVariable (Core.Name "b")),
                Typing.typeConstraintRight = (Core.TypeLiteral Core.LiteralTypeString),
                Typing.typeConstraintComment = "join types; test"}])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "join map types",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJoinTypes (Testing.JoinTypesTestCase {
            Testing.joinTypesTestCaseLeft = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
              Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))})),
            Testing.joinTypesTestCaseRight = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
              Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
            Testing.joinTypesTestCaseExpected = (Right [
              Typing.TypeConstraint {
                Typing.typeConstraintLeft = (Core.TypeVariable (Core.Name "k")),
                Typing.typeConstraintRight = (Core.TypeLiteral Core.LiteralTypeString),
                Typing.typeConstraintComment = "join types; test"},
              Typing.TypeConstraint {
                Typing.typeConstraintLeft = (Core.TypeVariable (Core.Name "v")),
                Typing.typeConstraintRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Typing.typeConstraintComment = "join types; test"}])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "join set types",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJoinTypes (Testing.JoinTypesTestCase {
            Testing.joinTypesTestCaseLeft = (Core.TypeSet (Core.TypeVariable (Core.Name "a"))),
            Testing.joinTypesTestCaseRight = (Core.TypeSet (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
            Testing.joinTypesTestCaseExpected = (Right [
              Typing.TypeConstraint {
                Typing.typeConstraintLeft = (Core.TypeVariable (Core.Name "a")),
                Typing.typeConstraintRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Typing.typeConstraintComment = "join types; test"}])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "join unit types",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJoinTypes (Testing.JoinTypesTestCase {
            Testing.joinTypesTestCaseLeft = Core.TypeUnit,
            Testing.joinTypesTestCaseRight = Core.TypeUnit,
            Testing.joinTypesTestCaseExpected = (Right [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "fail to join int32 with string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJoinTypes (Testing.JoinTypesTestCase {
            Testing.joinTypesTestCaseLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Testing.joinTypesTestCaseRight = (Core.TypeLiteral Core.LiteralTypeString),
            Testing.joinTypesTestCaseExpected = (Left ())})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "fail to join list with function",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJoinTypes (Testing.JoinTypesTestCase {
            Testing.joinTypesTestCaseLeft = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
            Testing.joinTypesTestCaseRight = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
            Testing.joinTypesTestCaseExpected = (Left ())})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "fail to join pair with either",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseJoinTypes (Testing.JoinTypesTestCase {
            Testing.joinTypesTestCaseLeft = (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)})),
            Testing.joinTypesTestCaseRight = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeString)})),
            Testing.joinTypesTestCaseExpected = (Left ())})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}
