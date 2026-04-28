-- Note: this is an automatically generated file. Do not edit.
-- | Test cases for type unification operations

module Hydra.Test.Unification where
import qualified Hydra.Core as Core
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Testing as Testing
import qualified Hydra.Typing as Typing
import qualified Hydra.Unification as Unification
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
-- | Test cases for type unification operations
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
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
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Literals.showBoolean (Unification.variableOccursInType (Core.Name "a") (Core.TypeVariable (Core.Name "a")))),
                Testing.universalTestCaseExpected = (Literals.showBoolean True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable does not occur in different variable",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Literals.showBoolean (Unification.variableOccursInType (Core.Name "a") (Core.TypeVariable (Core.Name "b")))),
                Testing.universalTestCaseExpected = (Literals.showBoolean False)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable does not occur in int32",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Literals.showBoolean (Unification.variableOccursInType (Core.Name "a") (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))),
                Testing.universalTestCaseExpected = (Literals.showBoolean False)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable does not occur in string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Literals.showBoolean (Unification.variableOccursInType (Core.Name "a") (Core.TypeLiteral Core.LiteralTypeString))),
                Testing.universalTestCaseExpected = (Literals.showBoolean False)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable occurs in list element type",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Literals.showBoolean (Unification.variableOccursInType (Core.Name "a") (Core.TypeList (Core.TypeVariable (Core.Name "a"))))),
                Testing.universalTestCaseExpected = (Literals.showBoolean True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable does not occur in list of different type",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Literals.showBoolean (Unification.variableOccursInType (Core.Name "a") (Core.TypeList (Core.TypeVariable (Core.Name "b"))))),
                Testing.universalTestCaseExpected = (Literals.showBoolean False)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable occurs in function domain",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Literals.showBoolean (Unification.variableOccursInType (Core.Name "a") (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                  Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))),
                Testing.universalTestCaseExpected = (Literals.showBoolean True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable occurs in function codomain",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Literals.showBoolean (Unification.variableOccursInType (Core.Name "a") (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})))),
                Testing.universalTestCaseExpected = (Literals.showBoolean True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable does not occur in function with different vars",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Literals.showBoolean (Unification.variableOccursInType (Core.Name "a") (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
                  Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "c"))})))),
                Testing.universalTestCaseExpected = (Literals.showBoolean False)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable occurs in optional type",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Literals.showBoolean (Unification.variableOccursInType (Core.Name "a") (Core.TypeMaybe (Core.TypeVariable (Core.Name "a"))))),
                Testing.universalTestCaseExpected = (Literals.showBoolean True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable occurs in pair first",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Literals.showBoolean (Unification.variableOccursInType (Core.Name "a") (Core.TypePair (Core.PairType {
                  Core.pairTypeFirst = (Core.TypeVariable (Core.Name "a")),
                  Core.pairTypeSecond = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))),
                Testing.universalTestCaseExpected = (Literals.showBoolean True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable occurs in pair second",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Literals.showBoolean (Unification.variableOccursInType (Core.Name "a") (Core.TypePair (Core.PairType {
                  Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.pairTypeSecond = (Core.TypeVariable (Core.Name "a"))})))),
                Testing.universalTestCaseExpected = (Literals.showBoolean True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable occurs in either left",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Literals.showBoolean (Unification.variableOccursInType (Core.Name "a") (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "a")),
                  Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))),
                Testing.universalTestCaseExpected = (Literals.showBoolean True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable occurs in either right",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Literals.showBoolean (Unification.variableOccursInType (Core.Name "a") (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.eitherTypeRight = (Core.TypeVariable (Core.Name "a"))})))),
                Testing.universalTestCaseExpected = (Literals.showBoolean True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable occurs in map key type",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Literals.showBoolean (Unification.variableOccursInType (Core.Name "a") (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeVariable (Core.Name "a")),
                  Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))),
                Testing.universalTestCaseExpected = (Literals.showBoolean True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable occurs in map value type",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Literals.showBoolean (Unification.variableOccursInType (Core.Name "a") (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
                  Core.mapTypeValues = (Core.TypeVariable (Core.Name "a"))})))),
                Testing.universalTestCaseExpected = (Literals.showBoolean True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable occurs in set type",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Literals.showBoolean (Unification.variableOccursInType (Core.Name "a") (Core.TypeSet (Core.TypeVariable (Core.Name "a"))))),
                Testing.universalTestCaseExpected = (Literals.showBoolean True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable occurs in nested list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Literals.showBoolean (Unification.variableOccursInType (Core.Name "a") (Core.TypeList (Core.TypeList (Core.TypeVariable (Core.Name "a")))))),
                Testing.universalTestCaseExpected = (Literals.showBoolean True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable occurs in list of functions",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Literals.showBoolean (Unification.variableOccursInType (Core.Name "a") (Core.TypeList (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))))),
                Testing.universalTestCaseExpected = (Literals.showBoolean True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable does not occur in complex type without it",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Literals.showBoolean (Unification.variableOccursInType (Core.Name "a") (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                  Core.functionTypeCodomain = (Core.TypeMaybe (Core.TypePair (Core.PairType {
                    Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.pairTypeSecond = (Core.TypeVariable (Core.Name "b"))})))})))),
                Testing.universalTestCaseExpected = (Literals.showBoolean False)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable occurs deep in complex type",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Literals.showBoolean (Unification.variableOccursInType (Core.Name "a") (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                  Core.functionTypeCodomain = (Core.TypeMaybe (Core.TypePair (Core.PairType {
                    Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.pairTypeSecond = (Core.TypeVariable (Core.Name "a"))})))})))),
                Testing.universalTestCaseExpected = (Literals.showBoolean True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable occurs in forAll body",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Literals.showBoolean (Unification.variableOccursInType (Core.Name "a") (Core.TypeForall (Core.ForallType {
                  Core.forallTypeParameter = (Core.Name "b"),
                  Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
                    Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))})))),
                Testing.universalTestCaseExpected = (Literals.showBoolean True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable occurs in forAll bound position",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Literals.showBoolean (Unification.variableOccursInType (Core.Name "a") (Core.TypeForall (Core.ForallType {
                  Core.forallTypeParameter = (Core.Name "a"),
                  Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                    Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))})))),
                Testing.universalTestCaseExpected = (Literals.showBoolean True)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "unifyTypes",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unify identical int32 types",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\ts -> Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst ts)))),
                  "}"]) (Unification.unifyTypes Lexical.emptyContext (Maps.fromList (Lists.map (\n -> (n, Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeVariable n),
                  Core.typeSchemeConstraints = Nothing})) [])) (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) "test")),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst (Typing.TypeSubst M.empty))))),
                  "}"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unify identical string types",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\ts -> Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst ts)))),
                  "}"]) (Unification.unifyTypes Lexical.emptyContext (Maps.fromList (Lists.map (\n -> (n, Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeVariable n),
                  Core.typeSchemeConstraints = Nothing})) [])) (Core.TypeLiteral Core.LiteralTypeString) (Core.TypeLiteral Core.LiteralTypeString) "test")),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst (Typing.TypeSubst M.empty))))),
                  "}"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unify identical variable types",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\ts -> Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst ts)))),
                  "}"]) (Unification.unifyTypes Lexical.emptyContext (Maps.fromList (Lists.map (\n -> (n, Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeVariable n),
                  Core.typeSchemeConstraints = Nothing})) [])) (Core.TypeVariable (Core.Name "a")) (Core.TypeVariable (Core.Name "a")) "test")),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst (Typing.TypeSubst M.empty))))),
                  "}"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unify variable with int32",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\ts -> Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst ts)))),
                  "}"]) (Unification.unifyTypes Lexical.emptyContext (Maps.fromList (Lists.map (\n -> (n, Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeVariable n),
                  Core.typeSchemeConstraints = Nothing})) [])) (Core.TypeVariable (Core.Name "a")) (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) "test")),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst (Typing.TypeSubst (M.fromList [
                    (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))])))))),
                  "}"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unify int32 with variable",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\ts -> Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst ts)))),
                  "}"]) (Unification.unifyTypes Lexical.emptyContext (Maps.fromList (Lists.map (\n -> (n, Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeVariable n),
                  Core.typeSchemeConstraints = Nothing})) [])) (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) (Core.TypeVariable (Core.Name "a")) "test")),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst (Typing.TypeSubst (M.fromList [
                    (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))])))))),
                  "}"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unify two different variables",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\ts -> Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst ts)))),
                  "}"]) (Unification.unifyTypes Lexical.emptyContext (Maps.fromList (Lists.map (\n -> (n, Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeVariable n),
                  Core.typeSchemeConstraints = Nothing})) [])) (Core.TypeVariable (Core.Name "a")) (Core.TypeVariable (Core.Name "b")) "test")),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst (Typing.TypeSubst (M.fromList [
                    (Core.Name "a", (Core.TypeVariable (Core.Name "b")))])))))),
                  "}"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unify list of variables with list of int32",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\ts -> Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst ts)))),
                  "}"]) (Unification.unifyTypes Lexical.emptyContext (Maps.fromList (Lists.map (\n -> (n, Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeVariable n),
                  Core.typeSchemeConstraints = Nothing})) [])) (Core.TypeList (Core.TypeVariable (Core.Name "a"))) (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))) "test")),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst (Typing.TypeSubst (M.fromList [
                    (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))])))))),
                  "}"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unify identical list types",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\ts -> Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst ts)))),
                  "}"]) (Unification.unifyTypes Lexical.emptyContext (Maps.fromList (Lists.map (\n -> (n, Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeVariable n),
                  Core.typeSchemeConstraints = Nothing})) [])) (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)) (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)) "test")),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst (Typing.TypeSubst M.empty))))),
                  "}"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unify function types with variables",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\ts -> Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst ts)))),
                  "}"]) (Unification.unifyTypes Lexical.emptyContext (Maps.fromList (Lists.map (\n -> (n, Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeVariable n),
                  Core.typeSchemeConstraints = Nothing})) [])) (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                  Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "b"))})) (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})) "test")),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst (Typing.TypeSubst (M.fromList [
                    (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                    (Core.Name "b", (Core.TypeLiteral Core.LiteralTypeString))])))))),
                  "}"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unify identical function types",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\ts -> Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst ts)))),
                  "}"]) (Unification.unifyTypes Lexical.emptyContext (Maps.fromList (Lists.map (\n -> (n, Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeVariable n),
                  Core.typeSchemeConstraints = Nothing})) [])) (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})) (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})) "test")),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst (Typing.TypeSubst M.empty))))),
                  "}"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unify optional types",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\ts -> Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst ts)))),
                  "}"]) (Unification.unifyTypes Lexical.emptyContext (Maps.fromList (Lists.map (\n -> (n, Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeVariable n),
                  Core.typeSchemeConstraints = Nothing})) [])) (Core.TypeMaybe (Core.TypeVariable (Core.Name "a"))) (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))) "test")),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst (Typing.TypeSubst (M.fromList [
                    (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))])))))),
                  "}"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unify pair types",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\ts -> Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst ts)))),
                  "}"]) (Unification.unifyTypes Lexical.emptyContext (Maps.fromList (Lists.map (\n -> (n, Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeVariable n),
                  Core.typeSchemeConstraints = Nothing})) [])) (Core.TypePair (Core.PairType {
                  Core.pairTypeFirst = (Core.TypeVariable (Core.Name "a")),
                  Core.pairTypeSecond = (Core.TypeVariable (Core.Name "b"))})) (Core.TypePair (Core.PairType {
                  Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)})) "test")),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst (Typing.TypeSubst (M.fromList [
                    (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                    (Core.Name "b", (Core.TypeLiteral Core.LiteralTypeString))])))))),
                  "}"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unify either types",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\ts -> Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst ts)))),
                  "}"]) (Unification.unifyTypes Lexical.emptyContext (Maps.fromList (Lists.map (\n -> (n, Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeVariable n),
                  Core.typeSchemeConstraints = Nothing})) [])) (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "a")),
                  Core.eitherTypeRight = (Core.TypeVariable (Core.Name "b"))})) (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeString)})) "test")),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst (Typing.TypeSubst (M.fromList [
                    (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                    (Core.Name "b", (Core.TypeLiteral Core.LiteralTypeString))])))))),
                  "}"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unify map types",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\ts -> Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst ts)))),
                  "}"]) (Unification.unifyTypes Lexical.emptyContext (Maps.fromList (Lists.map (\n -> (n, Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeVariable n),
                  Core.typeSchemeConstraints = Nothing})) [])) (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
                  Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))})) (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
                  Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})) "test")),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst (Typing.TypeSubst (M.fromList [
                    (Core.Name "k", (Core.TypeLiteral Core.LiteralTypeString)),
                    (Core.Name "v", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))])))))),
                  "}"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unify set types",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\ts -> Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst ts)))),
                  "}"]) (Unification.unifyTypes Lexical.emptyContext (Maps.fromList (Lists.map (\n -> (n, Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeVariable n),
                  Core.typeSchemeConstraints = Nothing})) [])) (Core.TypeSet (Core.TypeVariable (Core.Name "a"))) (Core.TypeSet (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))) "test")),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst (Typing.TypeSubst (M.fromList [
                    (Core.Name "a", (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))])))))),
                  "}"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unify unit types",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\ts -> Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst ts)))),
                  "}"]) (Unification.unifyTypes Lexical.emptyContext (Maps.fromList (Lists.map (\n -> (n, Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeVariable n),
                  Core.typeSchemeConstraints = Nothing})) [])) Core.TypeUnit Core.TypeUnit "test")),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst (Typing.TypeSubst M.empty))))),
                  "}"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "fail to unify int32 with string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\ts -> Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst ts)))),
                  "}"]) (Unification.unifyTypes Lexical.emptyContext (Maps.fromList (Lists.map (\n -> (n, Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeVariable n),
                  Core.typeSchemeConstraints = Nothing})) [])) (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) (Core.TypeLiteral Core.LiteralTypeString) "test")),
                Testing.universalTestCaseExpected = "failure"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "fail to unify list with function",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\ts -> Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst ts)))),
                  "}"]) (Unification.unifyTypes Lexical.emptyContext (Maps.fromList (Lists.map (\n -> (n, Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeVariable n),
                  Core.typeSchemeConstraints = Nothing})) [])) (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))) (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})) "test")),
                Testing.universalTestCaseExpected = "failure"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "occur check: variable with list containing it",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\ts -> Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                    Core.unName (Pairs.first p),
                    ": ",
                    (ShowCore.type_ (Pairs.second p))]) (Maps.toList (Typing.unTypeSubst ts)))),
                  "}"]) (Unification.unifyTypes Lexical.emptyContext (Maps.fromList (Lists.map (\n -> (n, Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeVariable n),
                  Core.typeSchemeConstraints = Nothing})) [])) (Core.TypeVariable (Core.Name "a")) (Core.TypeList (Core.TypeVariable (Core.Name "a"))) "test")),
                Testing.universalTestCaseExpected = "failure"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "joinTypes",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "join identical int32",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\cs -> Strings.cat [
                  "[",
                  (Strings.intercalate ", " (Lists.map (\c -> Strings.cat [
                    "(",
                    (ShowCore.type_ (Typing.typeConstraintLeft c)),
                    " ~ ",
                    (ShowCore.type_ (Typing.typeConstraintRight c)),
                    ")"]) cs)),
                  "]"]) (Unification.joinTypes Lexical.emptyContext (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) "test")),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "[",
                  (Strings.intercalate ", " (Lists.map (\c -> Strings.cat [
                    "(",
                    (ShowCore.type_ (Typing.typeConstraintLeft c)),
                    " ~ ",
                    (ShowCore.type_ (Typing.typeConstraintRight c)),
                    ")"]) [])),
                  "]"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "join identical string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\cs -> Strings.cat [
                  "[",
                  (Strings.intercalate ", " (Lists.map (\c -> Strings.cat [
                    "(",
                    (ShowCore.type_ (Typing.typeConstraintLeft c)),
                    " ~ ",
                    (ShowCore.type_ (Typing.typeConstraintRight c)),
                    ")"]) cs)),
                  "]"]) (Unification.joinTypes Lexical.emptyContext (Core.TypeLiteral Core.LiteralTypeString) (Core.TypeLiteral Core.LiteralTypeString) "test")),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "[",
                  (Strings.intercalate ", " (Lists.map (\c -> Strings.cat [
                    "(",
                    (ShowCore.type_ (Typing.typeConstraintLeft c)),
                    " ~ ",
                    (ShowCore.type_ (Typing.typeConstraintRight c)),
                    ")"]) [])),
                  "]"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "join list types",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\cs -> Strings.cat [
                  "[",
                  (Strings.intercalate ", " (Lists.map (\c -> Strings.cat [
                    "(",
                    (ShowCore.type_ (Typing.typeConstraintLeft c)),
                    " ~ ",
                    (ShowCore.type_ (Typing.typeConstraintRight c)),
                    ")"]) cs)),
                  "]"]) (Unification.joinTypes Lexical.emptyContext (Core.TypeList (Core.TypeVariable (Core.Name "a"))) (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))) "test")),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "[",
                  (Strings.intercalate ", " (Lists.map (\c -> Strings.cat [
                    "(",
                    (ShowCore.type_ (Typing.typeConstraintLeft c)),
                    " ~ ",
                    (ShowCore.type_ (Typing.typeConstraintRight c)),
                    ")"]) [
                    Typing.TypeConstraint {
                      Typing.typeConstraintLeft = (Core.TypeVariable (Core.Name "a")),
                      Typing.typeConstraintRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Typing.typeConstraintComment = "join types; test"}])),
                  "]"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "join function types",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\cs -> Strings.cat [
                  "[",
                  (Strings.intercalate ", " (Lists.map (\c -> Strings.cat [
                    "(",
                    (ShowCore.type_ (Typing.typeConstraintLeft c)),
                    " ~ ",
                    (ShowCore.type_ (Typing.typeConstraintRight c)),
                    ")"]) cs)),
                  "]"]) (Unification.joinTypes Lexical.emptyContext (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                  Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "b"))})) (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})) "test")),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "[",
                  (Strings.intercalate ", " (Lists.map (\c -> Strings.cat [
                    "(",
                    (ShowCore.type_ (Typing.typeConstraintLeft c)),
                    " ~ ",
                    (ShowCore.type_ (Typing.typeConstraintRight c)),
                    ")"]) [
                    Typing.TypeConstraint {
                      Typing.typeConstraintLeft = (Core.TypeVariable (Core.Name "a")),
                      Typing.typeConstraintRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Typing.typeConstraintComment = "join types; test"},
                    Typing.TypeConstraint {
                      Typing.typeConstraintLeft = (Core.TypeVariable (Core.Name "b")),
                      Typing.typeConstraintRight = (Core.TypeLiteral Core.LiteralTypeString),
                      Typing.typeConstraintComment = "join types; test"}])),
                  "]"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "join optional types",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\cs -> Strings.cat [
                  "[",
                  (Strings.intercalate ", " (Lists.map (\c -> Strings.cat [
                    "(",
                    (ShowCore.type_ (Typing.typeConstraintLeft c)),
                    " ~ ",
                    (ShowCore.type_ (Typing.typeConstraintRight c)),
                    ")"]) cs)),
                  "]"]) (Unification.joinTypes Lexical.emptyContext (Core.TypeMaybe (Core.TypeVariable (Core.Name "a"))) (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))) "test")),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "[",
                  (Strings.intercalate ", " (Lists.map (\c -> Strings.cat [
                    "(",
                    (ShowCore.type_ (Typing.typeConstraintLeft c)),
                    " ~ ",
                    (ShowCore.type_ (Typing.typeConstraintRight c)),
                    ")"]) [
                    Typing.TypeConstraint {
                      Typing.typeConstraintLeft = (Core.TypeVariable (Core.Name "a")),
                      Typing.typeConstraintRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Typing.typeConstraintComment = "join types; test"}])),
                  "]"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "join pair types",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\cs -> Strings.cat [
                  "[",
                  (Strings.intercalate ", " (Lists.map (\c -> Strings.cat [
                    "(",
                    (ShowCore.type_ (Typing.typeConstraintLeft c)),
                    " ~ ",
                    (ShowCore.type_ (Typing.typeConstraintRight c)),
                    ")"]) cs)),
                  "]"]) (Unification.joinTypes Lexical.emptyContext (Core.TypePair (Core.PairType {
                  Core.pairTypeFirst = (Core.TypeVariable (Core.Name "a")),
                  Core.pairTypeSecond = (Core.TypeVariable (Core.Name "b"))})) (Core.TypePair (Core.PairType {
                  Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)})) "test")),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "[",
                  (Strings.intercalate ", " (Lists.map (\c -> Strings.cat [
                    "(",
                    (ShowCore.type_ (Typing.typeConstraintLeft c)),
                    " ~ ",
                    (ShowCore.type_ (Typing.typeConstraintRight c)),
                    ")"]) [
                    Typing.TypeConstraint {
                      Typing.typeConstraintLeft = (Core.TypeVariable (Core.Name "a")),
                      Typing.typeConstraintRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Typing.typeConstraintComment = "join types; test"},
                    Typing.TypeConstraint {
                      Typing.typeConstraintLeft = (Core.TypeVariable (Core.Name "b")),
                      Typing.typeConstraintRight = (Core.TypeLiteral Core.LiteralTypeString),
                      Typing.typeConstraintComment = "join types; test"}])),
                  "]"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "join either types",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\cs -> Strings.cat [
                  "[",
                  (Strings.intercalate ", " (Lists.map (\c -> Strings.cat [
                    "(",
                    (ShowCore.type_ (Typing.typeConstraintLeft c)),
                    " ~ ",
                    (ShowCore.type_ (Typing.typeConstraintRight c)),
                    ")"]) cs)),
                  "]"]) (Unification.joinTypes Lexical.emptyContext (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "a")),
                  Core.eitherTypeRight = (Core.TypeVariable (Core.Name "b"))})) (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeString)})) "test")),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "[",
                  (Strings.intercalate ", " (Lists.map (\c -> Strings.cat [
                    "(",
                    (ShowCore.type_ (Typing.typeConstraintLeft c)),
                    " ~ ",
                    (ShowCore.type_ (Typing.typeConstraintRight c)),
                    ")"]) [
                    Typing.TypeConstraint {
                      Typing.typeConstraintLeft = (Core.TypeVariable (Core.Name "a")),
                      Typing.typeConstraintRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Typing.typeConstraintComment = "join types; test"},
                    Typing.TypeConstraint {
                      Typing.typeConstraintLeft = (Core.TypeVariable (Core.Name "b")),
                      Typing.typeConstraintRight = (Core.TypeLiteral Core.LiteralTypeString),
                      Typing.typeConstraintComment = "join types; test"}])),
                  "]"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "join map types",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\cs -> Strings.cat [
                  "[",
                  (Strings.intercalate ", " (Lists.map (\c -> Strings.cat [
                    "(",
                    (ShowCore.type_ (Typing.typeConstraintLeft c)),
                    " ~ ",
                    (ShowCore.type_ (Typing.typeConstraintRight c)),
                    ")"]) cs)),
                  "]"]) (Unification.joinTypes Lexical.emptyContext (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeVariable (Core.Name "k")),
                  Core.mapTypeValues = (Core.TypeVariable (Core.Name "v"))})) (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
                  Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})) "test")),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "[",
                  (Strings.intercalate ", " (Lists.map (\c -> Strings.cat [
                    "(",
                    (ShowCore.type_ (Typing.typeConstraintLeft c)),
                    " ~ ",
                    (ShowCore.type_ (Typing.typeConstraintRight c)),
                    ")"]) [
                    Typing.TypeConstraint {
                      Typing.typeConstraintLeft = (Core.TypeVariable (Core.Name "k")),
                      Typing.typeConstraintRight = (Core.TypeLiteral Core.LiteralTypeString),
                      Typing.typeConstraintComment = "join types; test"},
                    Typing.TypeConstraint {
                      Typing.typeConstraintLeft = (Core.TypeVariable (Core.Name "v")),
                      Typing.typeConstraintRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Typing.typeConstraintComment = "join types; test"}])),
                  "]"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "join set types",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\cs -> Strings.cat [
                  "[",
                  (Strings.intercalate ", " (Lists.map (\c -> Strings.cat [
                    "(",
                    (ShowCore.type_ (Typing.typeConstraintLeft c)),
                    " ~ ",
                    (ShowCore.type_ (Typing.typeConstraintRight c)),
                    ")"]) cs)),
                  "]"]) (Unification.joinTypes Lexical.emptyContext (Core.TypeSet (Core.TypeVariable (Core.Name "a"))) (Core.TypeSet (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))) "test")),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "[",
                  (Strings.intercalate ", " (Lists.map (\c -> Strings.cat [
                    "(",
                    (ShowCore.type_ (Typing.typeConstraintLeft c)),
                    " ~ ",
                    (ShowCore.type_ (Typing.typeConstraintRight c)),
                    ")"]) [
                    Typing.TypeConstraint {
                      Typing.typeConstraintLeft = (Core.TypeVariable (Core.Name "a")),
                      Typing.typeConstraintRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Typing.typeConstraintComment = "join types; test"}])),
                  "]"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "join unit types",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\cs -> Strings.cat [
                  "[",
                  (Strings.intercalate ", " (Lists.map (\c -> Strings.cat [
                    "(",
                    (ShowCore.type_ (Typing.typeConstraintLeft c)),
                    " ~ ",
                    (ShowCore.type_ (Typing.typeConstraintRight c)),
                    ")"]) cs)),
                  "]"]) (Unification.joinTypes Lexical.emptyContext Core.TypeUnit Core.TypeUnit "test")),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "[",
                  (Strings.intercalate ", " (Lists.map (\c -> Strings.cat [
                    "(",
                    (ShowCore.type_ (Typing.typeConstraintLeft c)),
                    " ~ ",
                    (ShowCore.type_ (Typing.typeConstraintRight c)),
                    ")"]) [])),
                  "]"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "fail to join int32 with string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\cs -> Strings.cat [
                  "[",
                  (Strings.intercalate ", " (Lists.map (\c -> Strings.cat [
                    "(",
                    (ShowCore.type_ (Typing.typeConstraintLeft c)),
                    " ~ ",
                    (ShowCore.type_ (Typing.typeConstraintRight c)),
                    ")"]) cs)),
                  "]"]) (Unification.joinTypes Lexical.emptyContext (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) (Core.TypeLiteral Core.LiteralTypeString) "test")),
                Testing.universalTestCaseExpected = "failure"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "fail to join list with function",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\cs -> Strings.cat [
                  "[",
                  (Strings.intercalate ", " (Lists.map (\c -> Strings.cat [
                    "(",
                    (ShowCore.type_ (Typing.typeConstraintLeft c)),
                    " ~ ",
                    (ShowCore.type_ (Typing.typeConstraintRight c)),
                    ")"]) cs)),
                  "]"]) (Unification.joinTypes Lexical.emptyContext (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))) (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})) "test")),
                Testing.universalTestCaseExpected = "failure"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "fail to join pair with either",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "failure") (\cs -> Strings.cat [
                  "[",
                  (Strings.intercalate ", " (Lists.map (\c -> Strings.cat [
                    "(",
                    (ShowCore.type_ (Typing.typeConstraintLeft c)),
                    " ~ ",
                    (ShowCore.type_ (Typing.typeConstraintRight c)),
                    ")"]) cs)),
                  "]"]) (Unification.joinTypes Lexical.emptyContext (Core.TypePair (Core.PairType {
                  Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)})) (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeString)})) "test")),
                Testing.universalTestCaseExpected = "failure"})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
