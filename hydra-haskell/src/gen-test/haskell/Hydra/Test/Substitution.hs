-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for type and term substitution operations

module Hydra.Test.Substitution where

import qualified Hydra.Core as Core
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Test cases for type and term substitution operations
allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
  Testing.testGroupName = "substitution",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "substInType",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty substitution returns type unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseSubstInType (Testing.SubstInTypeTestCase {
            Testing.substInTypeTestCaseSubstitution = [],
            Testing.substInTypeTestCaseInput = (Core.TypeLiteral Core.LiteralTypeString),
            Testing.substInTypeTestCaseOutput = (Core.TypeLiteral Core.LiteralTypeString)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "substitute type variable with int32",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseSubstInType (Testing.SubstInTypeTestCase {
            Testing.substInTypeTestCaseSubstitution = [
              (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))],
            Testing.substInTypeTestCaseInput = (Core.TypeVariable (Core.Name "a")),
            Testing.substInTypeTestCaseOutput = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "non-matching variable unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseSubstInType (Testing.SubstInTypeTestCase {
            Testing.substInTypeTestCaseSubstitution = [
              (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))],
            Testing.substInTypeTestCaseInput = (Core.TypeVariable (Core.Name "b")),
            Testing.substInTypeTestCaseOutput = (Core.TypeVariable (Core.Name "b"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "substitute in function domain",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseSubstInType (Testing.SubstInTypeTestCase {
            Testing.substInTypeTestCaseSubstitution = [
              (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))],
            Testing.substInTypeTestCaseInput = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})),
            Testing.substInTypeTestCaseOutput = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "substitute in function codomain",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseSubstInType (Testing.SubstInTypeTestCase {
            Testing.substInTypeTestCaseSubstitution = [
              (Core.Name "a", (Core.TypeLiteral Core.LiteralTypeString))],
            Testing.substInTypeTestCaseInput = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
            Testing.substInTypeTestCaseOutput = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "substitute in list element type",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseSubstInType (Testing.SubstInTypeTestCase {
            Testing.substInTypeTestCaseSubstitution = [
              (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))],
            Testing.substInTypeTestCaseInput = (Core.TypeList (Core.TypeVariable (Core.Name "a"))),
            Testing.substInTypeTestCaseOutput = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "substitute in optional type",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseSubstInType (Testing.SubstInTypeTestCase {
            Testing.substInTypeTestCaseSubstitution = [
              (Core.Name "a", (Core.TypeLiteral Core.LiteralTypeString))],
            Testing.substInTypeTestCaseInput = (Core.TypeMaybe (Core.TypeVariable (Core.Name "a"))),
            Testing.substInTypeTestCaseOutput = (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "substitute in pair type both sides",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseSubstInType (Testing.SubstInTypeTestCase {
            Testing.substInTypeTestCaseSubstitution = [
              (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))],
            Testing.substInTypeTestCaseInput = (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeVariable (Core.Name "a")),
              Core.pairTypeSecond = (Core.TypeVariable (Core.Name "a"))})),
            Testing.substInTypeTestCaseOutput = (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.pairTypeSecond = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "substitute in either type",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseSubstInType (Testing.SubstInTypeTestCase {
            Testing.substInTypeTestCaseSubstitution = [
              (Core.Name "a", (Core.TypeLiteral Core.LiteralTypeString))],
            Testing.substInTypeTestCaseInput = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "a")),
              Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
            Testing.substInTypeTestCaseOutput = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeLiteral Core.LiteralTypeString),
              Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "substitute in map key type",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseSubstInType (Testing.SubstInTypeTestCase {
            Testing.substInTypeTestCaseSubstitution = [
              (Core.Name "k", (Core.TypeLiteral Core.LiteralTypeString))],
            Testing.substInTypeTestCaseInput = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
              Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
            Testing.substInTypeTestCaseOutput = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
              Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "substitute in set type",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseSubstInType (Testing.SubstInTypeTestCase {
            Testing.substInTypeTestCaseSubstitution = [
              (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))],
            Testing.substInTypeTestCaseInput = (Core.TypeSet (Core.TypeVariable (Core.Name "a"))),
            Testing.substInTypeTestCaseOutput = (Core.TypeSet (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested substitution in list of pairs",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseSubstInType (Testing.SubstInTypeTestCase {
            Testing.substInTypeTestCaseSubstitution = [
              (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))],
            Testing.substInTypeTestCaseInput = (Core.TypeList (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeVariable (Core.Name "a")),
              Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)}))),
            Testing.substInTypeTestCaseOutput = (Core.TypeList (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple substitutions",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseSubstInType (Testing.SubstInTypeTestCase {
            Testing.substInTypeTestCaseSubstitution = [
              (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
              (Core.Name "b", (Core.TypeLiteral Core.LiteralTypeString))],
            Testing.substInTypeTestCaseInput = (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeVariable (Core.Name "a")),
              Core.pairTypeSecond = (Core.TypeVariable (Core.Name "b"))})),
            Testing.substInTypeTestCaseOutput = (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "forAll bound variable not substituted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseSubstInType (Testing.SubstInTypeTestCase {
            Testing.substInTypeTestCaseSubstitution = [
              (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))],
            Testing.substInTypeTestCaseInput = (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "a"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))})),
            Testing.substInTypeTestCaseOutput = (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "a"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "forAll free variable substituted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseSubstInType (Testing.SubstInTypeTestCase {
            Testing.substInTypeTestCaseSubstitution = [
              (Core.Name "b", (Core.TypeLiteral Core.LiteralTypeString))],
            Testing.substInTypeTestCaseInput = (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "a"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "b"))}))})),
            Testing.substInTypeTestCaseOutput = (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "a"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}
