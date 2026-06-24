-- Note: this is an automatically generated file. Do not edit.
-- | Inference tests for annotated terms

module Hydra.Test.Inference.Annotations where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.File as ErrorFile
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Error.System as ErrorSystem
import qualified Hydra.Errors as Errors
import qualified Hydra.File as File
import qualified Hydra.Graph as Graph
import qualified Hydra.Inference as Inference
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.System as System
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Test.TestGraph as TestGraph
import qualified Hydra.Testing as Testing
import qualified Hydra.Time as Time
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
-- | Inference tests for annotated terms
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "Annotated terms",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        transparencyTests,
        failureTests],
      Testing.testGroupCases = []}
failureTests :: Testing.TestGroup
failureTests =
    Testing.TestGroup {
      Testing.testGroupName = "Errors in the body propagate",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "Type error in body propagates",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "FAIL") (\result -> Strings.cat2 "unexpected: " (ShowCore.typeScheme (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "x"))})),
                  Core.annotatedTermAnnotation = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})))),
                Testing.universalTestCaseExpected = (\_ -> "FAIL")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
transparencyTests :: Testing.TestGroup
transparencyTests =
    Testing.TestGroup {
      Testing.testGroupName = "Annotation is transparent to the inferred type",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "Arbitrary-term annotation",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.annotatedTermAnnotation = (Core.TermLiteral (Core.LiteralString "hello"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "foo")),
                  Core.annotatedTermAnnotation = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeLiteral Core.LiteralTypeString),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#3",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})),
                  Core.annotatedTermAnnotation = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Map-term annotation (conventional form)",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.annotatedTermAnnotation = (Core.TermMap M.empty)})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
                    (Core.TermLiteral (Core.LiteralString "description"), (Core.TermLiteral (Core.LiteralString "the answer")))]))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Nested annotations",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                    Core.annotatedTermAnnotation = (Core.TermLiteral (Core.LiteralString "inner"))})),
                  Core.annotatedTermAnnotation = (Core.TermLiteral (Core.LiteralString "outer"))})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                    Core.annotatedTermAnnotation = (Core.TermMap M.empty)})),
                  Core.annotatedTermAnnotation = (Core.TermMap M.empty)})))),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
