-- Note: this is an automatically generated file. Do not edit.

-- | Inference tests for type class constraints (ordering and equality)

module Hydra.Test.Inference.Classes where

import qualified Hydra.Core as Core
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Type class constraint inference tests
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "Type classes",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        testGroupForMonomorphicConstraints,
        testGroupForPrimitiveReferences,
        testGroupForPartialApplication,
        testGroupForLetBindings,
        testGroupForComposition,
        testGroupForNestedContainers,
        testGroupForCollectionTerms],
      Testing.testGroupCases = []}

testGroupForCollectionTerms :: Testing.TestGroup
testGroupForCollectionTerms =
    Testing.TestGroup {
      Testing.testGroupName = "Collection term constraints",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "Set literals",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermSet (S.fromList [
                    Core.TermVariable (Core.Name "x")]))}))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeSet (Core.TypeVariable (Core.Name "t0")))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermSet (S.fromList [
                      Core.TermVariable (Core.Name "x"),
                      (Core.TermVariable (Core.Name "y"))]))})))}))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                      Core.functionTypeCodomain = (Core.TypeSet (Core.TypeVariable (Core.Name "t0")))}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#3",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermSet (S.fromList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeSet (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                  Core.typeSchemeConstraints = Nothing}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]}]},
        Testing.TestGroup {
          Testing.testGroupName = "Map literals",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "k"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "v"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermMap (M.fromList [
                      (Core.TermVariable (Core.Name "k"), (Core.TermVariable (Core.Name "v")))]))})))}))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0",
                    (Core.Name "t1")],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                      Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
                        Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                        Core.mapTypeValues = (Core.TypeVariable (Core.Name "t1"))}))}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermMap (M.fromList [
                  (Core.TermLiteral (Core.LiteralString "a"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))))])),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeMap (Core.MapType {
                    Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                  Core.typeSchemeConstraints = Nothing}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]}]},
        Testing.TestGroup {
          Testing.testGroupName = "Collection terms with primitives",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermSet (S.fromList [
                    Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.negate"))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}),
                    (Core.TermVariable (Core.Name "x"))]))}))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                    Core.functionTypeCodomain = (Core.TypeSet (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
                  Core.typeSchemeConstraints = Nothing}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "k"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermMap (M.fromList [
                    (Core.TermVariable (Core.Name "k"), (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.sort"))),
                      Core.applicationArgument = (Core.TermList [
                        Core.TermVariable (Core.Name "k")])})))]))}))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
                      Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                      Core.mapTypeValues = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]}]},
        Testing.TestGroup {
          Testing.testGroupName = "Constraint propagation through collection elements",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "xs"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermMap (M.fromList [
                    (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.length"))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}), (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.fromList"))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))})))]))}))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
                    Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
                      Core.mapTypeKeys = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.mapTypeValues = (Core.TypeSet (Core.TypeVariable (Core.Name "t0")))}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermList [
                  Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.sort"))]),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeList (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
                    Core.functionTypeCodomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))}))),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#3",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermPair (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.fromList")), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypePair (Core.PairType {
                    Core.pairTypeFirst = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
                      Core.functionTypeCodomain = (Core.TypeSet (Core.TypeVariable (Core.Name "t0")))})),
                    Core.pairTypeSecond = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#4",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "xs"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermSet (S.fromList [
                    Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.fromList"))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))})]))}))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
                    Core.functionTypeCodomain = (Core.TypeSet (Core.TypeSet (Core.TypeVariable (Core.Name "t0"))))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]}]}],
      Testing.testGroupCases = []}

testGroupForComposition :: Testing.TestGroup
testGroupForComposition =
    Testing.TestGroup {
      Testing.testGroupName = "Composition and constraint merging",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "Composing constrained primitives",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "xs"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.fromList"))),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.map"))),
                        Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermVariable (Core.Name "x"))))})))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}))}))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
                    Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
                      Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                      Core.mapTypeValues = (Core.TypeVariable (Core.Name "t0"))}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "f"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "xs"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.fromList"))),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.map"))),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "f"))})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}))})))}))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0",
                    (Core.Name "t1")],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))})),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
                      Core.functionTypeCodomain = (Core.TypeSet (Core.TypeVariable (Core.Name "t1")))}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t1", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Composing map and sort",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "m"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.map"))),
                      Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.sort")))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0",
                    (Core.Name "t1")],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeMap (Core.MapType {
                      Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                      Core.mapTypeValues = (Core.TypeList (Core.TypeVariable (Core.Name "t1")))})),
                    Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
                      Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                      Core.mapTypeValues = (Core.TypeList (Core.TypeVariable (Core.Name "t1")))}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])}),
                    (Core.Name "t1", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}

testGroupForLetBindings :: Testing.TestGroup
testGroupForLetBindings =
    Testing.TestGroup {
      Testing.testGroupName = "Let binding constraint propagation",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "Simple let-bound wrappers",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "lookup"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "k"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "m"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.lookup"))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "k"))})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))})))}))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "lookup"))})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0",
                    (Core.Name "t1")],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeMap (Core.MapType {
                        Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                        Core.mapTypeValues = (Core.TypeVariable (Core.Name "t1"))})),
                      Core.functionTypeCodomain = (Core.TypeMaybe (Core.TypeVariable (Core.Name "t1")))}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "member"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "s"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.member"))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))})))}))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "member"))})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeSet (Core.TypeVariable (Core.Name "t0"))),
                      Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeBoolean)}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#3",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "fromList"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.fromList"))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "fromList"))})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0",
                    (Core.Name "t1")],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeList (Core.TypePair (Core.PairType {
                      Core.pairTypeFirst = (Core.TypeVariable (Core.Name "t0")),
                      Core.pairTypeSecond = (Core.TypeVariable (Core.Name "t1"))}))),
                    Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
                      Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                      Core.mapTypeValues = (Core.TypeVariable (Core.Name "t1"))}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Let-bound with partial instantiation",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "m"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.map"))),
                            Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.negate")))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
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
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "xs"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.fromList"))),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
                    Core.functionTypeCodomain = (Core.TypeSet (Core.TypeVariable (Core.Name "t0")))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Multiple uses of a constrained let binding",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.fromList"))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermPair (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermPair (Core.TermLiteral (Core.LiteralString "a"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))))])}), (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermPair (Core.TermLiteral (Core.LiteralBoolean True), (Core.TermLiteral (Core.LiteralString "x")))])}))))})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypePair (Core.PairType {
                    Core.pairTypeFirst = (Core.TypeMap (Core.MapType {
                      Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
                      Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                    Core.pairTypeSecond = (Core.TypeMap (Core.MapType {
                      Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeBoolean),
                      Core.mapTypeValues = (Core.TypeLiteral Core.LiteralTypeString)}))})),
                  Core.typeSchemeConstraints = Nothing}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}

testGroupForMonomorphicConstraints :: Testing.TestGroup
testGroupForMonomorphicConstraints =
    Testing.TestGroup {
      Testing.testGroupName = "Monomorphic (constraints vanish)",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "Map operations with concrete types",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.fromList"))),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermPair (Core.TermLiteral (Core.LiteralString "a"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))))])})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeMap (Core.MapType {
                    Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                  Core.typeSchemeConstraints = Nothing}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.lookup"))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "k"))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.singleton"))),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "k"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                  Core.typeSchemeConstraints = Nothing}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#3",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.insert"))),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "k"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                  Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.empty")))})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeMap (Core.MapType {
                    Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                  Core.typeSchemeConstraints = Nothing}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Set operations with concrete types",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.fromList"))),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeSet (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                  Core.typeSchemeConstraints = Nothing}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.member"))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.singleton"))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeBoolean),
                  Core.typeSchemeConstraints = Nothing}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Equality operations with concrete types",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.equal"))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeBoolean),
                  Core.typeSchemeConstraints = Nothing}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "a"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "b"))})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeVariable (Core.Name "hydra.util.Comparison")),
                  Core.typeSchemeConstraints = Nothing}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "List operations with concrete types",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.sort"))),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                  Core.typeSchemeConstraints = Nothing}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}

testGroupForNestedContainers :: Testing.TestGroup
testGroupForNestedContainers =
    Testing.TestGroup {
      Testing.testGroupName = "Nested containers",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "Maps of sets",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "m"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.map"))),
                      Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.fromList")))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
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
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Sets of sets",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "xss"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.map"))),
                      Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.fromList")))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "xss"))}))}))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeSet (Core.TypeList (Core.TypeVariable (Core.Name "t0")))),
                    Core.functionTypeCodomain = (Core.TypeSet (Core.TypeSet (Core.TypeVariable (Core.Name "t0"))))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Map from sorted list",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "xs"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.fromList"))),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.map"))),
                        Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.singleton"))),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))))})))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}))}))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
                    Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
                      Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                      Core.mapTypeValues = (Core.TypeSet (Core.TypeVariable (Core.Name "t0")))}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}

testGroupForPartialApplication :: Testing.TestGroup
testGroupForPartialApplication =
    Testing.TestGroup {
      Testing.testGroupName = "Partial application preserving constraints",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "Map partial application",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "k"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.lookup"))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "k"))}))}))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0",
                    (Core.Name "t1")],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeMap (Core.MapType {
                        Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                        Core.mapTypeValues = (Core.TypeVariable (Core.Name "t1"))})),
                      Core.functionTypeCodomain = (Core.TypeMaybe (Core.TypeVariable (Core.Name "t1")))}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "k"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "v"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.singleton"))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "k"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0",
                    (Core.Name "t1")],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                      Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
                        Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                        Core.mapTypeValues = (Core.TypeVariable (Core.Name "t1"))}))}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Set partial application",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.member"))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeSet (Core.TypeVariable (Core.Name "t0"))),
                      Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeBoolean)}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Equality partial application",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.equal"))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))})))}))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                      Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeBoolean)}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "equality"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Partial application fixing the constrained variable",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "v"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.singleton"))),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "key"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
                      Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
                      Core.mapTypeValues = (Core.TypeVariable (Core.Name "t0"))}))})),
                  Core.typeSchemeConstraints = Nothing}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}

testGroupForPrimitiveReferences :: Testing.TestGroup
testGroupForPrimitiveReferences =
    Testing.TestGroup {
      Testing.testGroupName = "Primitive references with constraints",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "Map primitives (ordering on key type)",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.fromList"))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0",
                    (Core.Name "t1")],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeList (Core.TypePair (Core.PairType {
                      Core.pairTypeFirst = (Core.TypeVariable (Core.Name "t0")),
                      Core.pairTypeSecond = (Core.TypeVariable (Core.Name "t1"))}))),
                    Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
                      Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                      Core.mapTypeValues = (Core.TypeVariable (Core.Name "t1"))}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.lookup"))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0",
                    (Core.Name "t1")],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeMap (Core.MapType {
                        Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                        Core.mapTypeValues = (Core.TypeVariable (Core.Name "t1"))})),
                      Core.functionTypeCodomain = (Core.TypeMaybe (Core.TypeVariable (Core.Name "t1")))}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#3",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.insert"))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0",
                    (Core.Name "t1")],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeMap (Core.MapType {
                          Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                          Core.mapTypeValues = (Core.TypeVariable (Core.Name "t1"))})),
                        Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
                          Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                          Core.mapTypeValues = (Core.TypeVariable (Core.Name "t1"))}))}))}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#4",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.map"))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
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
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#5",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.empty"))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0",
                    (Core.Name "t1")],
                  Core.typeSchemeType = (Core.TypeMap (Core.MapType {
                    Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                    Core.mapTypeValues = (Core.TypeVariable (Core.Name "t1"))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Set primitives (ordering on element type)",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.fromList"))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
                    Core.functionTypeCodomain = (Core.TypeSet (Core.TypeVariable (Core.Name "t0")))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.member"))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeSet (Core.TypeVariable (Core.Name "t0"))),
                      Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeBoolean)}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#3",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.insert"))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeSet (Core.TypeVariable (Core.Name "t0"))),
                      Core.functionTypeCodomain = (Core.TypeSet (Core.TypeVariable (Core.Name "t0")))}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#4",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.map"))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0",
                    (Core.Name "t1")],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))})),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeSet (Core.TypeVariable (Core.Name "t0"))),
                      Core.functionTypeCodomain = (Core.TypeSet (Core.TypeVariable (Core.Name "t1")))}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])}),
                    (Core.Name "t1", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Equality primitives",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.equal"))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                      Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeBoolean)}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "equality"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.util.Comparison"))}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "List primitives with constraints",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.sort"))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
                    Core.functionTypeCodomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "ordering"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.nub"))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
                    Core.functionTypeCodomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "equality"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#3",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.elem"))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
                      Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeBoolean)}))})),
                  Core.typeSchemeConstraints = (Just (M.fromList [
                    (Core.Name "t0", Core.TypeVariableMetadata {
                      Core.typeVariableMetadataClasses = (S.fromList [
                        Core.Name "equality"])})]))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
