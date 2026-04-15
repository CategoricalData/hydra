-- Note: this is an automatically generated file. Do not edit.

-- | Inference tests for algebraic data types

module Hydra.Test.Inference.AlgebraicTypes where

import qualified Hydra.Core as Core
import qualified Hydra.Inference as Inference
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Test.TestGraph as TestGraph
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S

-- | Algebraic data type tests
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "Algebraic terms",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        testGroupForCollectionPrimitives,
        testGroupForEithers,
        testGroupForFolds,
        testGroupForLists,
        testGroupForMaps,
        testGroupForOptionals,
        testGroupForPairs,
        testGroupForSets],
      Testing.testGroupCases = []}

testGroupForCollectionPrimitives :: Testing.TestGroup
testGroupForCollectionPrimitives =
    Testing.TestGroup {
      Testing.testGroupName = "Collection primitives",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "maps.map applied to a function",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.map")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.math.negate"))})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeMap (Core.MapType {
                      Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                      Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                    Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
                      Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                      Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.map")),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermList [
                      Core.TermVariable (Core.Name "x")])}))})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0",
                    (Core.Name "t1")],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeMap (Core.MapType {
                      Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                      Core.mapTypeValues = (Core.TypeVariable (Core.Name "t1"))})),
                    Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
                      Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                      Core.mapTypeValues = (Core.TypeList (Core.TypeVariable (Core.Name "t1")))}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#3",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.map")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.sets.fromList"))})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0",
                    (Core.Name "t1")],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeMap (Core.MapType {
                      Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                      Core.mapTypeValues = (Core.TypeList (Core.TypeVariable (Core.Name "t1")))})),
                    Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
                      Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                      Core.mapTypeValues = (Core.TypeSet (Core.TypeVariable (Core.Name "t1")))}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])}),
                    (Core.Name "t1", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]}]},
        Testing.TestGroup {
          Testing.testGroupName = "sets.map applied to a function",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.map")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.math.negate"))})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeSet (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                    Core.functionTypeCodomain = (Core.TypeSet (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.map")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.lists.length"))})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeSet (Core.TypeList (Core.TypeVariable (Core.Name "t0")))),
                    Core.functionTypeCodomain = (Core.TypeSet (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]}]},
        Testing.TestGroup {
          Testing.testGroupName = "Composing collection primitives in let",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.map")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.sets.fromList"))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0",
                    (Core.Name "t1")],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeMap (Core.MapType {
                      Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                      Core.mapTypeValues = (Core.TypeList (Core.TypeVariable (Core.Name "t1")))})),
                    Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
                      Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                      Core.mapTypeValues = (Core.TypeSet (Core.TypeVariable (Core.Name "t1")))}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])}),
                    (Core.Name "t1", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.map")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.sets.fromList"))})),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "g"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                        Core.applicationArgument = (Core.TermMap (M.fromList [
                          (Core.TermLiteral (Core.LiteralString "a"), (Core.TermList [
                            Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                            (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]))]))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "g"))})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeMap (Core.MapType {
                    Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.mapTypeValues = (Core.TypeSet (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]}]},
        Testing.TestGroup {
          Testing.testGroupName = "Map operations in lambdas",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "m"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.map")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.lists.length"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0",
                    (Core.Name "t1")],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeMap (Core.MapType {
                      Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                      Core.mapTypeValues = (Core.TypeList (Core.TypeVariable (Core.Name "t1")))})),
                    Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
                      Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                      Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "f"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "m"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.map")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "f"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0",
                    (Core.Name "t1"),
                    (Core.Name "t2")],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))})),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeMap (Core.MapType {
                        Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t2")),
                        Core.mapTypeValues = (Core.TypeVariable (Core.Name "t0"))})),
                      Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
                        Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t2")),
                        Core.mapTypeValues = (Core.TypeVariable (Core.Name "t1"))}))}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t2", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]}]},
        Testing.TestGroup {
          Testing.testGroupName = "Fully applied collection conversions",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.fromList")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeSet (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.map")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.math.negate"))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.fromList")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))))])}))})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeMap (Core.MapType {
                    Core.mapTypeKeys = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                    Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#3",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.map")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.sets.fromList"))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.fromList")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermPair (Core.TermLiteral (Core.LiteralString "a"), (Core.TermList [
                        Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                        (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]))])}))})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeMap (Core.MapType {
                    Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.mapTypeValues = (Core.TypeSet (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]}]}],
      Testing.testGroupCases = []}

testGroupForEithers :: Testing.TestGroup
testGroupForEithers =
    Testing.TestGroup {
      Testing.testGroupName = "Either terms",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "Left values",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermList [
                  Core.TermEither (Left (Core.TermLiteral (Core.LiteralString "error"))),
                  (Core.TermEither (Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))]))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeList (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermEither (Left (Core.TermLiteral (Core.LiteralString "error")))))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.eitherTypeRight = (Core.TypeVariable (Core.Name "t0"))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Right values",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermList [
                  Core.TermEither (Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))),
                  (Core.TermEither (Left (Core.TermLiteral (Core.LiteralString "error"))))]))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeList (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermEither (Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "t0")),
                    Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Polymorphic either values",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermEither (Left (Core.TermList []))))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0",
                    (Core.Name "t1")],
                  Core.typeSchemeType = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
                    Core.eitherTypeRight = (Core.TypeVariable (Core.Name "t1"))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermEither (Right (Core.TermList []))))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0",
                    (Core.Name "t1")],
                  Core.typeSchemeType = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "t0")),
                    Core.eitherTypeRight = (Core.TypeList (Core.TypeVariable (Core.Name "t1")))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Nested either values",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermList [
                  Core.TermEither (Left (Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))))),
                  (Core.TermEither (Left (Core.TermEither (Right (Core.TermLiteral (Core.LiteralString "nested")))))),
                  (Core.TermEither (Right (Core.TermLiteral (Core.LiteralBoolean True))))]))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeList (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeEither (Core.EitherType {
                      Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeString)})),
                    Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeBoolean)}))),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermList [
                  Core.TermEither (Right (Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))),
                  (Core.TermEither (Right (Core.TermEither (Right (Core.TermLiteral (Core.LiteralBoolean True)))))),
                  (Core.TermEither (Left (Core.TermLiteral (Core.LiteralString "foo"))))]))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeList (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.eitherTypeRight = (Core.TypeEither (Core.EitherType {
                      Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeBoolean)}))}))),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Either in lambda",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "x"))))})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0",
                    (Core.Name "t1")],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                      Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "t0")),
                      Core.eitherTypeRight = (Core.TypeVariable (Core.Name "t1"))}))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "x"))))})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0",
                    (Core.Name "t1")],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                      Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "t1")),
                      Core.eitherTypeRight = (Core.TypeVariable (Core.Name "t0"))}))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Either in data structures",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermList [
                  Core.TermEither (Left (Core.TermLiteral (Core.LiteralString "error"))),
                  (Core.TermEither (Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))]))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeList (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermPair (Core.TermList [
                  Core.TermEither (Left (Core.TermLiteral (Core.LiteralString "error"))),
                  (Core.TermEither (Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))], (Core.TermList []))))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypePair (Core.PairType {
                    Core.pairTypeFirst = (Core.TypeList (Core.TypeEither (Core.EitherType {
                      Core.eitherTypeLeft = (Core.TypeLiteral Core.LiteralTypeString),
                      Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))),
                    Core.pairTypeSecond = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}

testGroupForFolds :: Testing.TestGroup
testGroupForFolds =
    Testing.TestGroup {
      Testing.testGroupName = "Eliminations",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "List eliminations (folds)",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.foldl")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.math.add"))})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                      Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.foldl")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.math.add"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                    Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#3",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.foldl")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.math.add"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]}]},
        Testing.TestGroup {
          Testing.testGroupName = "Optional eliminations",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.maybe")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.math.negate"))})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                    Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.maybe")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.math.negate"))})),
                  Core.applicationArgument = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 137)))))})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#3",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.maybe")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.math.negate"))})),
                  Core.applicationArgument = (Core.TermMaybe Nothing)})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#4",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.maybe")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.maybes.pure"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeMaybe (Core.TypeVariable (Core.Name "t0"))),
                    Core.functionTypeCodomain = (Core.TypeMaybe (Core.TypeVariable (Core.Name "t0")))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#5",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.maybe")),
                    Core.applicationArgument = (Core.TermList [])})),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermList [
                      Core.TermVariable (Core.Name "x")])}))})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeMaybe (Core.TypeVariable (Core.Name "t0"))),
                    Core.functionTypeCodomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]}]}],
      Testing.testGroupCases = []}

testGroupForLists :: Testing.TestGroup
testGroupForLists =
    Testing.TestGroup {
      Testing.testGroupName = "List terms",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "List of strings",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "foo"),
                  (Core.TermLiteral (Core.LiteralString "bar"))]))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "List of lists of strings",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermList [
                  Core.TermList [
                    Core.TermLiteral (Core.LiteralString "foo")],
                  (Core.TermList [])]))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeList (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Empty list",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermList []))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "List containing an empty list",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermList [
                  Core.TermList []]))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeList (Core.TypeList (Core.TypeVariable (Core.Name "t0")))),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Lambda producing a polymorphic list",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermList [
                    Core.TermVariable (Core.Name "x")])})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Lambda producing a list of integers",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermList [
                    Core.TermVariable (Core.Name "x"),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                    Core.functionTypeCodomain = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "List with repeated variables",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermList [
                    Core.TermVariable (Core.Name "x"),
                    (Core.TermLiteral (Core.LiteralString "foo")),
                    (Core.TermVariable (Core.Name "x"))])})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.functionTypeCodomain = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}

testGroupForMaps :: Testing.TestGroup
testGroupForMaps =
    Testing.TestGroup {
      Testing.testGroupName = "Map terms",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermMap (M.fromList [
              (Core.TermLiteral (Core.LiteralString "firstName"), (Core.TermLiteral (Core.LiteralString "Arthur"))),
              (Core.TermLiteral (Core.LiteralString "lastName"), (Core.TermLiteral (Core.LiteralString "Dent")))])))),
            Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
              Core.typeSchemeVariables = [],
              Core.typeSchemeType = (Core.TypeMap (Core.MapType {
                Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
                Core.mapTypeValues = (Core.TypeLiteral Core.LiteralTypeString)})),
              Core.typeSchemeConstraints = Nothing}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermMap M.empty))),
            Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
              Core.typeSchemeVariables = [
                Core.Name "t0",
                (Core.Name "t1")],
              Core.typeSchemeType = (Core.TypeMap (Core.MapType {
                Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                Core.mapTypeValues = (Core.TypeVariable (Core.Name "t1"))})),
              Core.typeSchemeConstraints = (Just (M.fromList [
                (Core.Name "t0", Core.TypeVariableMetadata {
                  Core.typeVariableMetadataClasses = (S.fromList [
                    Core.Name "ordering"])})]))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermMap (M.fromList [
                  (Core.TermVariable (Core.Name "x"), (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.1)))),
                  (Core.TermVariable (Core.Name "y"), (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.2))))]))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
              Core.typeSchemeVariables = [
                Core.Name "t0"],
              Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                  Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
                    Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                    Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}))}))})),
              Core.typeSchemeConstraints = (Just (M.fromList [
                (Core.Name "t0", Core.TypeVariableMetadata {
                  Core.typeVariableMetadataClasses = (S.fromList [
                    Core.Name "ordering"])})]))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]}]}

testGroupForOptionals :: Testing.TestGroup
testGroupForOptionals =
    Testing.TestGroup {
      Testing.testGroupName = "Optional terms",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))))),
            Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
              Core.typeSchemeVariables = [],
              Core.typeSchemeType = (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
              Core.typeSchemeConstraints = Nothing}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermMaybe Nothing))),
            Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
              Core.typeSchemeVariables = [
                Core.Name "t0"],
              Core.typeSchemeType = (Core.TypeMaybe (Core.TypeVariable (Core.Name "t0"))),
              Core.typeSchemeConstraints = Nothing}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]}]}

testGroupForPairs :: Testing.TestGroup
testGroupForPairs =
    Testing.TestGroup {
      Testing.testGroupName = "Pair terms",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "Monotyped pairs",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermPair (Core.TermLiteral (Core.LiteralString "foo"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypePair (Core.PairType {
                    Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.pairTypeSecond = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermPair (Core.TermLiteral (Core.LiteralString "foo"), (Core.TermList [
                  Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 42.0)),
                  (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 137.0)))]))))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypePair (Core.PairType {
                    Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.pairTypeSecond = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32)))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]}]},
        Testing.TestGroup {
          Testing.testGroupName = "Polytyped pairs",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermPair (Core.TermList [], (Core.TermLiteral (Core.LiteralString "foo")))))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypePair (Core.PairType {
                    Core.pairTypeFirst = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
                    Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermPair (Core.TermList [], (Core.TermList []))))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0",
                    (Core.Name "t1")],
                  Core.typeSchemeType = (Core.TypePair (Core.PairType {
                    Core.pairTypeFirst = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
                    Core.pairTypeSecond = (Core.TypeList (Core.TypeVariable (Core.Name "t1")))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]}]},
        Testing.TestGroup {
          Testing.testGroupName = "Nested pairs",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermPair (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermLiteral (Core.LiteralString "nested"))), (Core.TermLiteral (Core.LiteralBoolean True)))))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypePair (Core.PairType {
                    Core.pairTypeFirst = (Core.TypePair (Core.PairType {
                      Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)})),
                    Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeBoolean)})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermPair (Core.TermLiteral (Core.LiteralString "foo"), (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)), (Core.TermList [
                  Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 42.0))]))))))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypePair (Core.PairType {
                    Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.pairTypeSecond = (Core.TypePair (Core.PairType {
                      Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.pairTypeSecond = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32)))}))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]}]},
        Testing.TestGroup {
          Testing.testGroupName = "Pairs in lambda",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermLiteral (Core.LiteralString "constant"))))})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                      Core.pairTypeFirst = (Core.TypeVariable (Core.Name "t0")),
                      Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)}))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "p"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermPair (Core.TermVariable (Core.Name "p"), (Core.TermVariable (Core.Name "p"))))})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                      Core.pairTypeFirst = (Core.TypeVariable (Core.Name "t0")),
                      Core.pairTypeSecond = (Core.TypeVariable (Core.Name "t0"))}))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]}]},
        Testing.TestGroup {
          Testing.testGroupName = "Pairs in data structures",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermList [
                  Core.TermPair (Core.TermLiteral (Core.LiteralString "a"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))),
                  (Core.TermPair (Core.TermLiteral (Core.LiteralString "b"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))))]))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeList (Core.TypePair (Core.PairType {
                    Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.pairTypeSecond = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermList [
                  Core.TermPair (Core.TermList [], (Core.TermLiteral (Core.LiteralString "foo")))]))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeList (Core.TypePair (Core.PairType {
                    Core.pairTypeFirst = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
                    Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)}))),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]}]},
        Testing.TestGroup {
          Testing.testGroupName = "Additional cases",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)), (Core.TermLiteral (Core.LiteralString "foo")))))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypePair (Core.PairType {
                    Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                    Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermPair (Core.TermList [], (Core.TermLiteral (Core.LiteralString "foo")))))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypePair (Core.PairType {
                    Core.pairTypeFirst = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
                    Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#3",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermPair (Core.TermList [], (Core.TermList []))))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0",
                    (Core.Name "t1")],
                  Core.typeSchemeType = (Core.TypePair (Core.PairType {
                    Core.pairTypeFirst = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
                    Core.pairTypeSecond = (Core.TypeList (Core.TypeVariable (Core.Name "t1")))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]}]}],
      Testing.testGroupCases = []}

testGroupForSets :: Testing.TestGroup
testGroupForSets =
    Testing.TestGroup {
      Testing.testGroupName = "Set terms",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermSet (S.fromList [
              Core.TermLiteral (Core.LiteralBoolean True)])))),
            Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
              Core.typeSchemeVariables = [],
              Core.typeSchemeType = (Core.TypeSet (Core.TypeLiteral Core.LiteralTypeBoolean)),
              Core.typeSchemeConstraints = Nothing}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermSet (S.fromList [
              Core.TermSet S.empty])))),
            Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
              Core.typeSchemeVariables = [
                Core.Name "t0"],
              Core.typeSchemeType = (Core.TypeSet (Core.TypeSet (Core.TypeVariable (Core.Name "t0")))),
              Core.typeSchemeConstraints = (Just (M.fromList [
                (Core.Name "t0", Core.TypeVariableMetadata {
                  Core.typeVariableMetadataClasses = (S.fromList [
                    Core.Name "ordering"])})]))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]}]}
