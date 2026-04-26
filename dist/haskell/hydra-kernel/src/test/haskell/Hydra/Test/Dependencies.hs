-- Note: this is an automatically generated file. Do not edit.
-- | Test cases for dependency analysis and let-term transformations

module Hydra.Test.Dependencies where
import qualified Hydra.Core as Core
import qualified Hydra.Dependencies as Dependencies
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
-- | Test cases for dependency analysis and let-term transformations
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "dependencies",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "simplifyTerm",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "const application with literal",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.simplifyTerm (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "foo"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralString "foo")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "identity application",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.simplifyTerm (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermList [
                      Core.TermVariable (Core.Name "x"),
                      (Core.TermVariable (Core.Name "x"))])})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermVariable (Core.Name "y"),
                  (Core.TermVariable (Core.Name "y"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unused parameter",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.simplifyTerm (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "foo"))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralString "foo")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested lambda applications",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.simplifyTerm (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "a"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermList [
                          Core.TermLiteral (Core.LiteralString "foo"),
                          (Core.TermVariable (Core.Name "a"))])})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "foo"),
                  (Core.TermVariable (Core.Name "y"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "flattenLetTerms",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "non-let term unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.flattenLetTerms (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "list term unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.flattenLetTerms (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "foo")]))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "foo")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "sequential lets in body are flattened",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.flattenLetTerms (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "y"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermList [
                      Core.TermVariable (Core.Name "x"),
                      (Core.TermVariable (Core.Name "y"))])}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "y"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermList [
                    Core.TermVariable (Core.Name "x"),
                    (Core.TermVariable (Core.Name "y"))])})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested binding in let value is flattened",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.flattenLetTerms (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "a"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "b"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "x"),
                            Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                            Core.bindingType = Nothing},
                          Core.Binding {
                            Core.bindingName = (Core.Name "y"),
                            Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermList [
                          Core.TermVariable (Core.Name "x"),
                          (Core.TermVariable (Core.Name "y"))])})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermList [
                    Core.TermVariable (Core.Name "a"),
                    (Core.TermVariable (Core.Name "b"))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "a"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "b_x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "b_y"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "b"),
                      Core.bindingTerm = (Core.TermList [
                        Core.TermVariable (Core.Name "b_x"),
                        (Core.TermVariable (Core.Name "b_y"))]),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermList [
                    Core.TermVariable (Core.Name "a"),
                    (Core.TermVariable (Core.Name "b"))])})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multiple levels of nesting are flattened",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.flattenLetTerms (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "a"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "b"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "x"),
                            Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                            Core.bindingType = Nothing},
                          Core.Binding {
                            Core.bindingName = (Core.Name "y"),
                            Core.bindingTerm = (Core.TermLet (Core.Let {
                              Core.letBindings = [
                                Core.Binding {
                                  Core.bindingName = (Core.Name "p"),
                                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 137))),
                                  Core.bindingType = Nothing},
                                Core.Binding {
                                  Core.bindingName = (Core.Name "q"),
                                  Core.bindingTerm = (Core.TermList [
                                    Core.TermVariable (Core.Name "x"),
                                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))]),
                                  Core.bindingType = Nothing}],
                              Core.letBody = (Core.TermList [
                                Core.TermVariable (Core.Name "a"),
                                (Core.TermVariable (Core.Name "q"))])})),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermList [
                          Core.TermVariable (Core.Name "x"),
                          (Core.TermVariable (Core.Name "y"))])})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermList [
                    Core.TermVariable (Core.Name "a"),
                    (Core.TermVariable (Core.Name "b"))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "a"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "b_x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "b_y_p"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 137))),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "b_y_q"),
                      Core.bindingTerm = (Core.TermList [
                        Core.TermVariable (Core.Name "b_x"),
                        (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))]),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "b_y"),
                      Core.bindingTerm = (Core.TermList [
                        Core.TermVariable (Core.Name "a"),
                        (Core.TermVariable (Core.Name "b_y_q"))]),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "b"),
                      Core.bindingTerm = (Core.TermList [
                        Core.TermVariable (Core.Name "b_x"),
                        (Core.TermVariable (Core.Name "b_y"))]),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermList [
                    Core.TermVariable (Core.Name "a"),
                    (Core.TermVariable (Core.Name "b"))])})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "liftLambdaAboveLet",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "simple let with lambda in body",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.liftLambdaAboveLet (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "x"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermVariable (Core.Name "x"))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "bare lambda unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.liftLambdaAboveLet (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "bare let unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.liftLambdaAboveLet (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "lambda with let in body unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.liftLambdaAboveLet (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "x"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermVariable (Core.Name "x"))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "x"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermVariable (Core.Name "x"))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "let with two nested lambdas",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.liftLambdaAboveLet (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "z"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "z"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "x"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "x"))}))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "lambda inside let body already above let",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.liftLambdaAboveLet (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "z"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "z"))}))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "z"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "z"))}))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "let without lambda in body unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.liftLambdaAboveLet (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "y"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "hello")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermVariable (Core.Name "y"))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "y"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "hello")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermVariable (Core.Name "y"))))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multiple let bindings with lambda",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.liftLambdaAboveLet (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "y"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "hello")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "z"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "z"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "x"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                        Core.bindingType = Nothing},
                      Core.Binding {
                        Core.bindingName = (Core.Name "y"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "hello")),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermVariable (Core.Name "x"))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested lets with lambda at innermost level",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.liftLambdaAboveLet (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "y"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "hello")),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "z"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "z"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "x"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "y"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "hello")),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "x"))}))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "lambda between two lets",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.liftLambdaAboveLet (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "z"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "hello")),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "x"))}))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "x"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "z"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "hello")),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "x"))}))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multiple lambdas between nested lets",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.liftLambdaAboveLet (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "a"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "b"),
                            Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermVariable (Core.Name "a"))}))}))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "a"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "b"),
                            Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermVariable (Core.Name "a"))}))}))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multiple lambdas already above let",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.liftLambdaAboveLet (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "z"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "z"))}))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "z"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "z"))}))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "annotation above let containing lambda",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.liftLambdaAboveLet (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "x"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))})),
                  Core.annotatedTermAnnotation = M.empty})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "x"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "x"))}))})),
                  Core.annotatedTermAnnotation = M.empty})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "annotation above lambda in let body",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.liftLambdaAboveLet (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})),
                    Core.annotatedTermAnnotation = M.empty}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "x"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "x"))})),
                    Core.annotatedTermAnnotation = M.empty}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "annotation between two lambdas",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.liftLambdaAboveLet (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                      Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "z"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})),
                      Core.annotatedTermAnnotation = M.empty}))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "z"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                      Core.annotatedTermBody = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "x"),
                            Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermVariable (Core.Name "x"))})),
                      Core.annotatedTermAnnotation = M.empty}))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "annotation on the body of lambda in let",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.liftLambdaAboveLet (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                      Core.annotatedTermBody = (Core.TermVariable (Core.Name "x")),
                      Core.annotatedTermAnnotation = M.empty}))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "x"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                      Core.annotatedTermBody = (Core.TermVariable (Core.Name "x")),
                      Core.annotatedTermAnnotation = M.empty}))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "annotation on lambda already above let",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.liftLambdaAboveLet (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "x"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "x"))}))})),
                  Core.annotatedTermAnnotation = M.empty})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "x"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "x"))}))})),
                  Core.annotatedTermAnnotation = M.empty})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "let-lambda inside a list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.liftLambdaAboveLet (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "x"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))})),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "x"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "x"))}))})),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "let-lambda in multiple list elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.liftLambdaAboveLet (Core.TermList [
                  Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "x"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))}),
                  (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "z"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "w"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "z"))}))}))]))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "x"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "x"))}))}),
                  (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "w"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "z"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "z"))}))}))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "let-lambda in a let binding value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.liftLambdaAboveLet (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "x"),
                            Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "y"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "y"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermLet (Core.Let {
                          Core.letBindings = [
                            Core.Binding {
                              Core.bindingName = (Core.Name "x"),
                              Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                              Core.bindingType = Nothing}],
                          Core.letBody = (Core.TermVariable (Core.Name "x"))}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "let-lambda inside a pair",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.liftLambdaAboveLet (Core.TermPair (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))}), (Core.TermLiteral (Core.LiteralString "test")))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermPair (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "x"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermVariable (Core.Name "x"))}))}), (Core.TermLiteral (Core.LiteralString "test")))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "let-lambda in both elements of a pair",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.liftLambdaAboveLet (Core.TermPair (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))}), (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "z"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "w"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "z"))}))})))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermPair (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "x"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermVariable (Core.Name "x"))}))}), (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "w"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "z"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermVariable (Core.Name "z"))}))})))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "let-lambda inside lambda body",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Dependencies.liftLambdaAboveLet (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "outer"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "x"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "inner"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "outer"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "inner"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "x"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "x"))}))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "topologicalSortBindings",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "isolated bindings",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.list (\group -> ShowCore.list (\pair -> Strings.cat [
                  "(",
                  (Core.unName (Pairs.first pair)),
                  ", ",
                  (ShowCore.term (Pairs.second pair)),
                  ")"]) group) (Dependencies.topologicalSortBindingMap (Maps.fromList [
                  (Core.Name "a", (Core.TermLiteral (Core.LiteralString "foo"))),
                  (Core.Name "b", (Core.TermLiteral (Core.LiteralString "bar")))]))),
                Testing.universalTestCaseExpected = (ShowCore.list (\group -> ShowCore.list (\pair -> Strings.cat [
                  "(",
                  (Core.unName (Pairs.first pair)),
                  ", ",
                  (ShowCore.term (Pairs.second pair)),
                  ")"]) group) [
                  [
                    (Core.Name "a", (Core.TermLiteral (Core.LiteralString "foo")))],
                  [
                    (Core.Name "b", (Core.TermLiteral (Core.LiteralString "bar")))]])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single recursive binding",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.list (\group -> ShowCore.list (\pair -> Strings.cat [
                  "(",
                  (Core.unName (Pairs.first pair)),
                  ", ",
                  (ShowCore.term (Pairs.second pair)),
                  ")"]) group) (Dependencies.topologicalSortBindingMap (Maps.fromList [
                  (Core.Name "a", (Core.TermList [
                    Core.TermVariable (Core.Name "a")]))]))),
                Testing.universalTestCaseExpected = (ShowCore.list (\group -> ShowCore.list (\pair -> Strings.cat [
                  "(",
                  (Core.unName (Pairs.first pair)),
                  ", ",
                  (ShowCore.term (Pairs.second pair)),
                  ")"]) group) [
                  [
                    (Core.Name "a", (Core.TermList [
                      Core.TermVariable (Core.Name "a")]))]])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "mutually recursive bindings",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.list (\group -> ShowCore.list (\pair -> Strings.cat [
                  "(",
                  (Core.unName (Pairs.first pair)),
                  ", ",
                  (ShowCore.term (Pairs.second pair)),
                  ")"]) group) (Dependencies.topologicalSortBindingMap (Maps.fromList [
                  (Core.Name "a", (Core.TermList [
                    Core.TermVariable (Core.Name "b")])),
                  (Core.Name "b", (Core.TermList [
                    Core.TermVariable (Core.Name "a")]))]))),
                Testing.universalTestCaseExpected = (ShowCore.list (\group -> ShowCore.list (\pair -> Strings.cat [
                  "(",
                  (Core.unName (Pairs.first pair)),
                  ", ",
                  (ShowCore.term (Pairs.second pair)),
                  ")"]) group) [
                  [
                    (Core.Name "a", (Core.TermList [
                      Core.TermVariable (Core.Name "b")])),
                    (Core.Name "b", (Core.TermList [
                      Core.TermVariable (Core.Name "a")]))]])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "mixed bindings",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.list (\group -> ShowCore.list (\pair -> Strings.cat [
                  "(",
                  (Core.unName (Pairs.first pair)),
                  ", ",
                  (ShowCore.term (Pairs.second pair)),
                  ")"]) group) (Dependencies.topologicalSortBindingMap (Maps.fromList [
                  (Core.Name "a", (Core.TermVariable (Core.Name "b"))),
                  (Core.Name "b", (Core.TermList [
                    Core.TermVariable (Core.Name "a"),
                    (Core.TermVariable (Core.Name "c"))])),
                  (Core.Name "c", (Core.TermLiteral (Core.LiteralString "foo"))),
                  (Core.Name "d", (Core.TermLiteral (Core.LiteralString "bar")))]))),
                Testing.universalTestCaseExpected = (ShowCore.list (\group -> ShowCore.list (\pair -> Strings.cat [
                  "(",
                  (Core.unName (Pairs.first pair)),
                  ", ",
                  (ShowCore.term (Pairs.second pair)),
                  ")"]) group) [
                  [
                    (Core.Name "c", (Core.TermLiteral (Core.LiteralString "foo")))],
                  [
                    (Core.Name "a", (Core.TermVariable (Core.Name "b"))),
                    (Core.Name "b", (Core.TermList [
                      Core.TermVariable (Core.Name "a"),
                      (Core.TermVariable (Core.Name "c"))]))],
                  [
                    (Core.Name "d", (Core.TermLiteral (Core.LiteralString "bar")))]])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
