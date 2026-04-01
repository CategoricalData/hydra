-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for term rewriting operations

module Hydra.Test.Rewriting where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Reduction as Reduction
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Show.Core as Core_
import qualified Hydra.Test.TestGraph as TestGraph
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Test cases for term rewriting operations
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "rewriting",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "freeVariables",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string literal has no free variables",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\n -> Core.unName n) (Sets.toList (Rewriting.freeVariablesInTerm (Core.TermLiteral (Core.LiteralString "foo")))))),
                  "}"]),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\n -> Core.unName n) (Sets.toList S.empty))),
                  "}"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single variable",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\n -> Core.unName n) (Sets.toList (Rewriting.freeVariablesInTerm (Core.TermVariable (Core.Name "x")))))),
                  "}"]),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\n -> Core.unName n) (Sets.toList (S.fromList [
                    Core.Name "x"])))),
                  "}"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "bound variable is not free",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\n -> Core.unName n) (Sets.toList (Rewriting.freeVariablesInTerm (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))))))),
                  "}"]),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\n -> Core.unName n) (Sets.toList S.empty))),
                  "}"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unbound variable in lambda body",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\n -> Core.unName n) (Sets.toList (Rewriting.freeVariablesInTerm (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))))))),
                  "}"]),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\n -> Core.unName n) (Sets.toList (S.fromList [
                    Core.Name "x"])))),
                  "}"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "mixed free and bound variables",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\n -> Core.unName n) (Sets.toList (Rewriting.freeVariablesInTerm (Core.TermList [
                    Core.TermVariable (Core.Name "x"),
                    (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "y"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))]))))),
                  "}"]),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\n -> Core.unName n) (Sets.toList (S.fromList [
                    Core.Name "x"])))),
                  "}"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multiple free variables",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\n -> Core.unName n) (Sets.toList (Rewriting.freeVariablesInTerm (Core.TermList [
                    Core.TermVariable (Core.Name "x"),
                    (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "y"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))]))))),
                  "}"]),
                Testing.universalTestCaseExpected = (Strings.cat [
                  "{",
                  (Strings.intercalate ", " (Lists.map (\n -> Core.unName n) (Sets.toList (S.fromList [
                    Core.Name "x",
                    (Core.Name "y")])))),
                  "}"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "simplifyTerm",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "const application with literal",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.simplifyTerm (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "foo"))}))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralString "foo")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "identity application",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.simplifyTerm (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermList [
                      Core.TermVariable (Core.Name "x"),
                      (Core.TermVariable (Core.Name "x"))])}))),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermVariable (Core.Name "y"),
                  (Core.TermVariable (Core.Name "y"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unused parameter",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.simplifyTerm (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "foo"))}))),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralString "foo")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested lambda applications",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.simplifyTerm (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "a"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermList [
                          Core.TermLiteral (Core.LiteralString "foo"),
                          (Core.TermVariable (Core.Name "a"))])}))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
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
                Testing.universalTestCaseActual = (Core_.term (Rewriting.flattenLetTerms (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "list term unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.flattenLetTerms (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "foo")]))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "foo")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "sequential lets in body are flattened",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.flattenLetTerms (Core.TermLet (Core.Let {
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
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
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
                Testing.universalTestCaseActual = (Core_.term (Rewriting.flattenLetTerms (Core.TermLet (Core.Let {
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
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
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
                Testing.universalTestCaseActual = (Core_.term (Rewriting.flattenLetTerms (Core.TermLet (Core.Let {
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
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
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
                Testing.universalTestCaseActual = (Core_.term (Rewriting.liftLambdaAboveLet (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "x"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermVariable (Core.Name "x"))}))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "bare lambda unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.liftLambdaAboveLet (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "bare let unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.liftLambdaAboveLet (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
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
                Testing.universalTestCaseActual = (Core_.term (Rewriting.liftLambdaAboveLet (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "x"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermVariable (Core.Name "x"))}))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "x"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermVariable (Core.Name "x"))}))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "let with two nested lambdas",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.liftLambdaAboveLet (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "z"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "z"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "x"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "x"))}))})))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "lambda inside let body already above let",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.liftLambdaAboveLet (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "z"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "z"))}))})))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "z"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "z"))}))})))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "let without lambda in body unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.liftLambdaAboveLet (Core.TermLet (Core.Let {
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
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
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
                Testing.universalTestCaseActual = (Core_.term (Rewriting.liftLambdaAboveLet (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "y"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "hello")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "z"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                    Core.letBody = (Core.TermVariable (Core.Name "x"))}))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested lets with lambda at innermost level",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.liftLambdaAboveLet (Core.TermLet (Core.Let {
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
                    Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "z"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                      Core.letBody = (Core.TermVariable (Core.Name "x"))}))}))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "lambda between two lets",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.liftLambdaAboveLet (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "z"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "hello")),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "x"))}))})))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                      Core.letBody = (Core.TermVariable (Core.Name "x"))}))}))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multiple lambdas between nested lets",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.liftLambdaAboveLet (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "a"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "b"),
                            Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermVariable (Core.Name "a"))}))})))})))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                        Core.letBody = (Core.TermVariable (Core.Name "a"))}))}))})))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multiple lambdas already above let",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.liftLambdaAboveLet (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "z"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "z"))}))})))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "z"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "z"))}))})))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "annotation above let containing lambda",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.liftLambdaAboveLet (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "x"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
                  Core.annotatedTermAnnotation = M.empty})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "x"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "x"))}))}))),
                  Core.annotatedTermAnnotation = M.empty})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "annotation above lambda in let body",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.liftLambdaAboveLet (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                    Core.annotatedTermAnnotation = M.empty}))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                    Core.annotatedTermAnnotation = M.empty}))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "annotation between two lambdas",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.liftLambdaAboveLet (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                      Core.annotatedTermBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "z"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                      Core.annotatedTermAnnotation = M.empty}))})))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                      Core.annotatedTermAnnotation = M.empty}))})))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "annotation on the body of lambda in let",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.liftLambdaAboveLet (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                      Core.annotatedTermBody = (Core.TermVariable (Core.Name "x")),
                      Core.annotatedTermAnnotation = M.empty}))})))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                      Core.annotatedTermAnnotation = M.empty}))}))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "annotation on lambda already above let",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.liftLambdaAboveLet (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "x"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "x"))}))}))),
                  Core.annotatedTermAnnotation = M.empty})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "x"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "x"))}))}))),
                  Core.annotatedTermAnnotation = M.empty})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "let-lambda inside a list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.liftLambdaAboveLet (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "x"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "x"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "x"))}))}))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "let-lambda in multiple list elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.liftLambdaAboveLet (Core.TermList [
                  Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "x"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}),
                  (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "z"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "w"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "z"))})))}))]))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "x"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "x"))}))})),
                  (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "w"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "z"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "z"))}))})))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "let-lambda in a let binding value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.liftLambdaAboveLet (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "x"),
                            Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "y"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "y"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermLet (Core.Let {
                          Core.letBindings = [
                            Core.Binding {
                              Core.bindingName = (Core.Name "x"),
                              Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                              Core.bindingType = Nothing}],
                          Core.letBody = (Core.TermVariable (Core.Name "x"))}))}))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "let-lambda inside a pair",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.liftLambdaAboveLet (Core.TermPair (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}), (Core.TermLiteral (Core.LiteralString "test")))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermPair (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "x"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermVariable (Core.Name "x"))}))})), (Core.TermLiteral (Core.LiteralString "test")))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "let-lambda in both elements of a pair",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.liftLambdaAboveLet (Core.TermPair (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}), (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "z"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "w"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "z"))})))})))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermPair (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "x"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermVariable (Core.Name "x"))}))})), (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "w"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "z"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermVariable (Core.Name "z"))}))}))))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "let-lambda inside lambda body",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.liftLambdaAboveLet (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "outer"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "x"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "inner"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "outer"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "inner"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "x"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "x"))}))})))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "deannotateTerm",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unannotated literal unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.deannotateTerm (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unannotated variable unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.deannotateTerm (Core.TermVariable (Core.Name "x")))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermVariable (Core.Name "x")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unannotated lambda unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.deannotateTerm (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single annotation stripped",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.deannotateTerm (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.annotatedTermAnnotation = M.empty})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested annotations stripped",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.deannotateTerm (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                    Core.annotatedTermAnnotation = M.empty})),
                  Core.annotatedTermAnnotation = M.empty})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "annotated lambda stripped",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.deannotateTerm (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                  Core.annotatedTermAnnotation = M.empty})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "annotated application stripped",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.deannotateTerm (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                  Core.annotatedTermAnnotation = M.empty})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "deannotateType",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unannotated primitive type unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.type_ (Rewriting.deannotateType (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unannotated string type unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.type_ (Rewriting.deannotateType (Core.TypeLiteral Core.LiteralTypeString))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeLiteral Core.LiteralTypeString))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unannotated function type unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.type_ (Rewriting.deannotateType (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single annotation stripped",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.type_ (Rewriting.deannotateType (Core.TypeAnnotated (Core.AnnotatedType {
                  Core.annotatedTypeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.annotatedTypeAnnotation = M.empty})))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested annotations stripped",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.type_ (Rewriting.deannotateType (Core.TypeAnnotated (Core.AnnotatedType {
                  Core.annotatedTypeBody = (Core.TypeAnnotated (Core.AnnotatedType {
                    Core.annotatedTypeBody = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.annotatedTypeAnnotation = M.empty})),
                  Core.annotatedTypeAnnotation = M.empty})))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeLiteral Core.LiteralTypeString))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "annotated list type stripped",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.type_ (Rewriting.deannotateType (Core.TypeAnnotated (Core.AnnotatedType {
                  Core.annotatedTypeBody = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                  Core.annotatedTypeAnnotation = M.empty})))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "annotated function type stripped",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.type_ (Rewriting.deannotateType (Core.TypeAnnotated (Core.AnnotatedType {
                  Core.annotatedTypeBody = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                    Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})),
                  Core.annotatedTypeAnnotation = M.empty})))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
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
                Testing.universalTestCaseActual = (Core_.list (\group -> Core_.list (\pair -> Strings.cat [
                  "(",
                  (Core.unName (Pairs.first pair)),
                  ", ",
                  (Core_.term (Pairs.second pair)),
                  ")"]) group) (Rewriting.topologicalSortBindingMap (Maps.fromList [
                  (Core.Name "a", (Core.TermLiteral (Core.LiteralString "foo"))),
                  (Core.Name "b", (Core.TermLiteral (Core.LiteralString "bar")))]))),
                Testing.universalTestCaseExpected = (Core_.list (\group -> Core_.list (\pair -> Strings.cat [
                  "(",
                  (Core.unName (Pairs.first pair)),
                  ", ",
                  (Core_.term (Pairs.second pair)),
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
                Testing.universalTestCaseActual = (Core_.list (\group -> Core_.list (\pair -> Strings.cat [
                  "(",
                  (Core.unName (Pairs.first pair)),
                  ", ",
                  (Core_.term (Pairs.second pair)),
                  ")"]) group) (Rewriting.topologicalSortBindingMap (Maps.fromList [
                  (Core.Name "a", (Core.TermList [
                    Core.TermVariable (Core.Name "a")]))]))),
                Testing.universalTestCaseExpected = (Core_.list (\group -> Core_.list (\pair -> Strings.cat [
                  "(",
                  (Core.unName (Pairs.first pair)),
                  ", ",
                  (Core_.term (Pairs.second pair)),
                  ")"]) group) [
                  [
                    (Core.Name "a", (Core.TermList [
                      Core.TermVariable (Core.Name "a")]))]])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "mutually recursive bindings",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.list (\group -> Core_.list (\pair -> Strings.cat [
                  "(",
                  (Core.unName (Pairs.first pair)),
                  ", ",
                  (Core_.term (Pairs.second pair)),
                  ")"]) group) (Rewriting.topologicalSortBindingMap (Maps.fromList [
                  (Core.Name "a", (Core.TermList [
                    Core.TermVariable (Core.Name "b")])),
                  (Core.Name "b", (Core.TermList [
                    Core.TermVariable (Core.Name "a")]))]))),
                Testing.universalTestCaseExpected = (Core_.list (\group -> Core_.list (\pair -> Strings.cat [
                  "(",
                  (Core.unName (Pairs.first pair)),
                  ", ",
                  (Core_.term (Pairs.second pair)),
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
                Testing.universalTestCaseActual = (Core_.list (\group -> Core_.list (\pair -> Strings.cat [
                  "(",
                  (Core.unName (Pairs.first pair)),
                  ", ",
                  (Core_.term (Pairs.second pair)),
                  ")"]) group) (Rewriting.topologicalSortBindingMap (Maps.fromList [
                  (Core.Name "a", (Core.TermVariable (Core.Name "b"))),
                  (Core.Name "b", (Core.TermList [
                    Core.TermVariable (Core.Name "a"),
                    (Core.TermVariable (Core.Name "c"))])),
                  (Core.Name "c", (Core.TermLiteral (Core.LiteralString "foo"))),
                  (Core.Name "d", (Core.TermLiteral (Core.LiteralString "bar")))]))),
                Testing.universalTestCaseExpected = (Core_.list (\group -> Core_.list (\pair -> Strings.cat [
                  "(",
                  (Core.unName (Pairs.first pair)),
                  ", ",
                  (Core_.term (Pairs.second pair)),
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
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "normalizeTypeVariables",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "literal without type variables unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.normalizeTypeVariablesInTerm (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "simple let without type annotations unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.normalizeTypeVariablesInTerm (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "foo"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "foo"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "let with monomorphic type scheme unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.normalizeTypeVariablesInTerm (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "foo"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.typeSchemeConstraints = Nothing}))}],
                  Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "foo"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.typeSchemeConstraints = Nothing}))}],
                  Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "let with monomorphic binding referencing string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.normalizeTypeVariablesInTerm (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "foo"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.typeSchemeConstraints = Nothing}))}],
                  Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "foo"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.typeSchemeConstraints = Nothing}))}],
                  Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "polymorphic binding with free type variable unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.normalizeTypeVariablesInTerm (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "foo"),
                      Core.bindingTerm = (Core.TermVariable (Core.Name "bar")),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeVariable (Core.Name "a")),
                        Core.typeSchemeConstraints = Nothing}))}],
                  Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "foo"),
                      Core.bindingTerm = (Core.TermVariable (Core.Name "bar")),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeVariable (Core.Name "a")),
                        Core.typeSchemeConstraints = Nothing}))}],
                  Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "monomorphic binding with typed lambda unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.normalizeTypeVariablesInTerm (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "foo"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.typeSchemeConstraints = Nothing}))}],
                  Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = (Just (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                      Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))),
                    Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "foo"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.typeSchemeConstraints = Nothing}))}],
                  Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = (Just (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                      Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))),
                    Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "polymorphic binding with typed lambda in body unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.normalizeTypeVariablesInTerm (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "foo"),
                      Core.bindingTerm = (Core.TermVariable (Core.Name "bar")),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeVariable (Core.Name "a")),
                        Core.typeSchemeConstraints = Nothing}))}],
                  Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = (Just (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                      Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))),
                    Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "foo"),
                      Core.bindingTerm = (Core.TermVariable (Core.Name "bar")),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeVariable (Core.Name "a")),
                        Core.typeSchemeConstraints = Nothing}))}],
                  Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = (Just (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                      Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))),
                    Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "polymorphic identity function normalized",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.normalizeTypeVariablesInTerm (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "id"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "a"],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                        Core.typeSchemeConstraints = Nothing}))}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "id"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "t0"],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})),
                        Core.typeSchemeConstraints = Nothing}))}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "polymorphic const function normalized",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.normalizeTypeVariablesInTerm (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "const"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "y"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}))),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "a",
                          (Core.Name "b")],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                          Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))})),
                        Core.typeSchemeConstraints = Nothing}))}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "const")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))}))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "const"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "y"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}))),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "t0",
                          (Core.Name "t1")],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                          Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))})),
                        Core.typeSchemeConstraints = Nothing}))}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "const")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "binding rewriting does not affect body with typed lambda",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.normalizeTypeVariablesInTerm (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "id"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "a"],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                        Core.typeSchemeConstraints = Nothing}))}],
                  Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = (Just (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                      Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))),
                    Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "id"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "t0"],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})),
                        Core.typeSchemeConstraints = Nothing}))}],
                  Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = (Just (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                      Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))),
                    Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested polymorphic lets normalized",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.normalizeTypeVariablesInTerm (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "id"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "a"],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                        Core.typeSchemeConstraints = Nothing}))}],
                  Core.letBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "id2"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "y"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))),
                        Core.bindingType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "b"],
                          Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "b"))})),
                          Core.typeSchemeConstraints = Nothing}))}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "id2")),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))}))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "id"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "t0"],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})),
                        Core.typeSchemeConstraints = Nothing}))}],
                  Core.letBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "id2"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "y"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))),
                        Core.bindingType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "t0"],
                          Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})),
                          Core.typeSchemeConstraints = Nothing}))}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "id2")),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested same substitution in bindings and environment",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.normalizeTypeVariablesInTerm (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "id"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "a"],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                        Core.typeSchemeConstraints = Nothing}))}],
                  Core.letBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "id2"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                        Core.bindingType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "a"],
                          Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                          Core.typeSchemeConstraints = Nothing}))}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "id"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "t0"],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})),
                        Core.typeSchemeConstraints = Nothing}))}],
                  Core.letBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "id2"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                        Core.bindingType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "t0"],
                          Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})),
                          Core.typeSchemeConstraints = Nothing}))}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "parent type variable shadows child variable",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.normalizeTypeVariablesInTerm (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "id"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "id2"),
                            Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "x"),
                              Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "a"))),
                              Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                            Core.bindingType = (Just (Core.TypeScheme {
                              Core.typeSchemeVariables = [
                                Core.Name "a"],
                              Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                              Core.typeSchemeConstraints = Nothing}))}],
                        Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "y"),
                          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "a"))),
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "id2")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))})))})),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "a"],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                        Core.typeSchemeConstraints = Nothing}))}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "id"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "id2"),
                            Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "x"),
                              Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t1"))),
                              Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                            Core.bindingType = (Just (Core.TypeScheme {
                              Core.typeSchemeVariables = [
                                Core.Name "t1"],
                              Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))})),
                              Core.typeSchemeConstraints = Nothing}))}],
                        Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "y"),
                          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "id2")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))})))})),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "t0"],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})),
                        Core.typeSchemeConstraints = Nothing}))}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "no shadowing distinct type variables",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.normalizeTypeVariablesInTerm (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "id"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "id2"),
                            Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "x"),
                              Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "b"))),
                              Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                            Core.bindingType = (Just (Core.TypeScheme {
                              Core.typeSchemeVariables = [
                                Core.Name "b"],
                              Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
                                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "b"))})),
                              Core.typeSchemeConstraints = Nothing}))}],
                        Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "y"),
                          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "a"))),
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "id2")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))})))})),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "a"],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                        Core.typeSchemeConstraints = Nothing}))}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "id"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "id2"),
                            Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "x"),
                              Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t1"))),
                              Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                            Core.bindingType = (Just (Core.TypeScheme {
                              Core.typeSchemeVariables = [
                                Core.Name "t1"],
                              Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))})),
                              Core.typeSchemeConstraints = Nothing}))}],
                        Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "y"),
                          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "id2")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))})))})),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "t0"],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})),
                        Core.typeSchemeConstraints = Nothing}))}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "locally free type variable in nested binding",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.normalizeTypeVariablesInTerm (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "fun1"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "a"))),
                        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "y"),
                          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "b"))),
                          Core.lambdaBody = (Core.TermLet (Core.Let {
                            Core.letBindings = [
                              Core.Binding {
                                Core.bindingName = (Core.Name "fun2"),
                                Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "z"),
                                  Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "c"))),
                                  Core.lambdaBody = (Core.TermPair (Core.TermVariable (Core.Name "z"), (Core.TermVariable (Core.Name "y"))))}))),
                                Core.bindingType = (Just (Core.TypeScheme {
                                  Core.typeSchemeVariables = [
                                    Core.Name "c"],
                                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "c")),
                                    Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                                      Core.pairTypeFirst = (Core.TypeVariable (Core.Name "c")),
                                      Core.pairTypeSecond = (Core.TypeVariable (Core.Name "b"))}))})),
                                  Core.typeSchemeConstraints = Nothing}))}],
                            Core.letBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "fun2")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})))}))),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "a",
                          (Core.Name "b")],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                          Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
                            Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                              Core.pairTypeFirst = (Core.TypeVariable (Core.Name "a")),
                              Core.pairTypeSecond = (Core.TypeVariable (Core.Name "b"))}))}))})),
                        Core.typeSchemeConstraints = Nothing}))}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "fun1")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "fun1"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
                        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "y"),
                          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t1"))),
                          Core.lambdaBody = (Core.TermLet (Core.Let {
                            Core.letBindings = [
                              Core.Binding {
                                Core.bindingName = (Core.Name "fun2"),
                                Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "z"),
                                  Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t2"))),
                                  Core.lambdaBody = (Core.TermPair (Core.TermVariable (Core.Name "z"), (Core.TermVariable (Core.Name "y"))))}))),
                                Core.bindingType = (Just (Core.TypeScheme {
                                  Core.typeSchemeVariables = [
                                    Core.Name "t2"],
                                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t2")),
                                    Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                                      Core.pairTypeFirst = (Core.TypeVariable (Core.Name "t2")),
                                      Core.pairTypeSecond = (Core.TypeVariable (Core.Name "t1"))}))})),
                                  Core.typeSchemeConstraints = Nothing}))}],
                            Core.letBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "fun2")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})))}))),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "t0",
                          (Core.Name "t1")],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                          Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                            Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                              Core.pairTypeFirst = (Core.TypeVariable (Core.Name "t0")),
                              Core.pairTypeSecond = (Core.TypeVariable (Core.Name "t1"))}))}))})),
                        Core.typeSchemeConstraints = Nothing}))}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "fun1")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "etaExpandTerm",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "integer literal unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "eta expansion failed") (\t -> Core_.term t) (Reduction.etaExpandTypedTerm TestGraph.testContext TestGraph.testGraph (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string list unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "eta expansion failed") (\t -> Core_.term t) (Reduction.etaExpandTypedTerm TestGraph.testContext TestGraph.testGraph (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "foo"),
                  (Core.TermLiteral (Core.LiteralString "bar"))]))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "foo"),
                  (Core.TermLiteral (Core.LiteralString "bar"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "fully applied binary function unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "eta expansion failed") (\t -> Core_.term t) (Reduction.etaExpandTypedTerm TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "bar"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "bar"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "lambda with fully applied primitive unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "eta expansion failed") (\t -> Core_.term t) (Reduction.etaExpandTypedTerm TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "lambda returning constant unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "eta expansion failed") (\t -> Core_.term t) (Reduction.etaExpandTypedTerm TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "bare unary primitive unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "eta expansion failed") (\t -> Core_.term t) (Reduction.etaExpandTypedTerm TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "bare binary primitive unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "eta expansion failed") (\t -> Core_.term t) (Reduction.etaExpandTypedTerm TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "partially applied binary primitive expands to one lambda",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "eta expansion failed") (\t -> Core_.term t) (Reduction.etaExpandTypedTerm TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "foo"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "v1"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "foo"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "projection expands to lambda",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "eta expansion failed") (\t -> Core_.term t) (Reduction.etaExpandTypedTerm TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                  Core.projectionTypeName = (Core.Name "Person"),
                  Core.projectionField = (Core.Name "firstName")})))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "v1"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                      Core.projectionTypeName = (Core.Name "Person"),
                      Core.projectionField = (Core.Name "firstName")})))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "partial application inside lambda expands",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "eta expansion failed") (\t -> Core_.term t) (Reduction.etaExpandTypedTerm TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "v1"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))})))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "let with constant body unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "eta expansion failed") (\t -> Core_.term t) (Reduction.etaExpandTypedTerm TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "foo"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 137))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "foo"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 137))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "let with bare primitive value unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "eta expansion failed") (\t -> Core_.term t) (Reduction.etaExpandTypedTerm TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "foo"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "foo"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "foo"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "foo"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "fully applied unary unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "eta expansion failed") (\t -> Core_.term t) (Reduction.etaExpandTypedTerm TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "FOO"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "FOO"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "partial application in list expands",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "eta expansion failed") (\t -> Core_.term t) (Reduction.etaExpandTypedTerm TestGraph.testContext TestGraph.testGraph (Core.TermList [
                  Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermList [
                      Core.TermLiteral (Core.LiteralString "foo")])})),
                  (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "bar"))}))]))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermList [
                      Core.TermLiteral (Core.LiteralString "foo")])})),
                  (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "v1"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "bar"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))})))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "foldOverTerm",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "collect labels from single node - pre-order",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Core.TermList (Lists.map (\lit -> Core.TermLiteral lit) (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Lists.concat [
                  acc,
                  case term of
                    Core.TermPair v0 -> case (Pairs.first v0) of
                      Core.TermLiteral v1 -> [
                        v1]
                      _ -> []
                    _ -> []]) [] (Core.TermPair (Core.TermLiteral (Core.LiteralString "a"), (Core.TermList []))))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "a")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "collect labels from tree - pre-order",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Core.TermList (Lists.map (\lit -> Core.TermLiteral lit) (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Lists.concat [
                  acc,
                  case term of
                    Core.TermPair v0 -> case (Pairs.first v0) of
                      Core.TermLiteral v1 -> [
                        v1]
                      _ -> []
                    _ -> []]) [] (Core.TermPair (Core.TermLiteral (Core.LiteralString "a"), (Core.TermList [
                  Core.TermPair (Core.TermLiteral (Core.LiteralString "b"), (Core.TermList [])),
                  (Core.TermPair (Core.TermLiteral (Core.LiteralString "c"), (Core.TermList [
                    Core.TermPair (Core.TermLiteral (Core.LiteralString "d"), (Core.TermList []))])))]))))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "a"),
                  (Core.TermLiteral (Core.LiteralString "b")),
                  (Core.TermLiteral (Core.LiteralString "c")),
                  (Core.TermLiteral (Core.LiteralString "d"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "collect labels from single node - post-order",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Core.TermList (Lists.map (\lit -> Core.TermLiteral lit) (Rewriting.foldOverTerm Coders.TraversalOrderPost (\acc -> \term -> Lists.concat [
                  acc,
                  case term of
                    Core.TermPair v0 -> case (Pairs.first v0) of
                      Core.TermLiteral v1 -> [
                        v1]
                      _ -> []
                    _ -> []]) [] (Core.TermPair (Core.TermLiteral (Core.LiteralString "a"), (Core.TermList []))))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "a")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "collect labels from tree - post-order",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Core.TermList (Lists.map (\lit -> Core.TermLiteral lit) (Rewriting.foldOverTerm Coders.TraversalOrderPost (\acc -> \term -> Lists.concat [
                  acc,
                  case term of
                    Core.TermPair v0 -> case (Pairs.first v0) of
                      Core.TermLiteral v1 -> [
                        v1]
                      _ -> []
                    _ -> []]) [] (Core.TermPair (Core.TermLiteral (Core.LiteralString "a"), (Core.TermList [
                  Core.TermPair (Core.TermLiteral (Core.LiteralString "b"), (Core.TermList [])),
                  (Core.TermPair (Core.TermLiteral (Core.LiteralString "c"), (Core.TermList [
                    Core.TermPair (Core.TermLiteral (Core.LiteralString "d"), (Core.TermList []))])))]))))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "b"),
                  (Core.TermLiteral (Core.LiteralString "d")),
                  (Core.TermLiteral (Core.LiteralString "c")),
                  (Core.TermLiteral (Core.LiteralString "a"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "sum int32 literals",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Math.add acc (case term of
                  Core.TermLiteral v0 -> case v0 of
                    Core.LiteralInteger v1 -> case v1 of
                      Core.IntegerValueInt32 v2 -> v2
                      _ -> 0
                    _ -> 0
                  _ -> 0)) 0 (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
                  (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))}))])))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 52))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "collect list lengths - pre-order",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Core.TermList (Lists.map (\n -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 n))) (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Lists.concat [
                  acc,
                  case term of
                    Core.TermList v0 -> [
                      Lists.length v0]
                    _ -> []]) [] (Core.TermList [
                  Core.TermList [
                    Core.TermLiteral (Core.LiteralString "foo"),
                    (Core.TermLiteral (Core.LiteralString "bar"))],
                  (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralString "quux")])}))]))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "collect list lengths - post-order",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Core.TermList (Lists.map (\n -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 n))) (Rewriting.foldOverTerm Coders.TraversalOrderPost (\acc -> \term -> Lists.concat [
                  acc,
                  case term of
                    Core.TermList v0 -> [
                      Lists.length v0]
                    _ -> []]) [] (Core.TermList [
                  Core.TermList [
                    Core.TermLiteral (Core.LiteralString "foo"),
                    (Core.TermLiteral (Core.LiteralString "bar"))],
                  (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralString "quux")])}))]))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "rewriteType",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "String type in left side of either is replaced",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.type_ (Rewriting.rewriteType (\recurse -> \typ -> Logic.ifElse (Equality.equal typ (Core.TypeLiteral Core.LiteralTypeString)) (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) (recurse typ)) (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeLiteral Core.LiteralTypeString),
                  Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "String type in right side of either is replaced",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.type_ (Rewriting.rewriteType (\recurse -> \typ -> Logic.ifElse (Equality.equal typ (Core.TypeLiteral Core.LiteralTypeString)) (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) (recurse typ)) (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeString)})))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "String types in both sides of either are replaced",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.type_ (Rewriting.rewriteType (\recurse -> \typ -> Logic.ifElse (Equality.equal typ (Core.TypeLiteral Core.LiteralTypeString)) (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) (recurse typ)) (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeLiteral Core.LiteralTypeString),
                  Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeString)})))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "String type in nested either (left of left) is replaced",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.type_ (Rewriting.rewriteType (\recurse -> \typ -> Logic.ifElse (Equality.equal typ (Core.TypeLiteral Core.LiteralTypeString)) (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) (recurse typ)) (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                  Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64))})))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                    Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                  Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "String type in nested either (right of right) is replaced",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.type_ (Rewriting.rewriteType (\recurse -> \typ -> Logic.ifElse (Equality.equal typ (Core.TypeLiteral Core.LiteralTypeString)) (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) (recurse typ)) (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64)),
                  Core.eitherTypeRight = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                    Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeString)}))})))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64)),
                  Core.eitherTypeRight = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                    Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "String types in complex nested either are all replaced",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.type_ (Rewriting.rewriteType (\recurse -> \typ -> Logic.ifElse (Equality.equal typ (Core.TypeLiteral Core.LiteralTypeString)) (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) (recurse typ)) (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeString)})),
                  Core.eitherTypeRight = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64))}))})))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                    Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                  Core.eitherTypeRight = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                    Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "String in list type is replaced",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.type_ (Rewriting.rewriteType (\recurse -> \typ -> Logic.ifElse (Equality.equal typ (Core.TypeLiteral Core.LiteralTypeString)) (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) (recurse typ)) (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "String in function domain is replaced",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.type_ (Rewriting.rewriteType (\recurse -> \typ -> Logic.ifElse (Equality.equal typ (Core.TypeLiteral Core.LiteralTypeString)) (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) (recurse typ)) (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                  Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64))})))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "String in function codomain is replaced",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.type_ (Rewriting.rewriteType (\recurse -> \typ -> Logic.ifElse (Equality.equal typ (Core.TypeLiteral Core.LiteralTypeString)) (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) (recurse typ)) (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64)),
                  Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64)),
                  Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "String in optional type is replaced",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.type_ (Rewriting.rewriteType (\recurse -> \typ -> Logic.ifElse (Equality.equal typ (Core.TypeLiteral Core.LiteralTypeString)) (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)) (recurse typ)) (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString)))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "rewriteTerm",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string literal foo replaced with bar",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermLiteral (Core.LiteralString "foo")))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralString "bar")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in variable not changed",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermVariable (Core.Name "x")))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermVariable (Core.Name "x")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "foo"),
                  (Core.TermLiteral (Core.LiteralString "baz"))]))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "bar"),
                  (Core.TermLiteral (Core.LiteralString "baz"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multiple strings in list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "foo"),
                  (Core.TermLiteral (Core.LiteralString "foo")),
                  (Core.TermLiteral (Core.LiteralString "baz"))]))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "bar"),
                  (Core.TermLiteral (Core.LiteralString "bar")),
                  (Core.TermLiteral (Core.LiteralString "baz"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in optional (just)",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "foo")))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "bar")))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in function application",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "print")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "print")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "bar"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in lambda body",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "foo"))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "bar"))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in nested applications",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))}))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "bar"))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in record field",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "Person"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "name"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))}]})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "Person"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "name"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))}]})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "strings in multiple record fields",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "Data"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "a"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "b"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "c"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))}]})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "Data"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "a"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "b"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "c"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))}]})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in pair",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermPair (Core.TermLiteral (Core.LiteralString "foo"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermPair (Core.TermLiteral (Core.LiteralString "bar"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in let binding value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "bar")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in let body",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLiteral (Core.LiteralString "foo"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLiteral (Core.LiteralString "bar"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in first case branch",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = (Core.Name "Result"),
                  Core.caseStatementDefault = Nothing,
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "success"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "error"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))}]})))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = (Core.Name "Result"),
                  Core.caseStatementDefault = Nothing,
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "success"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "error"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))}]})))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in second case branch",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = (Core.Name "Result"),
                  Core.caseStatementDefault = Nothing,
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "success"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "error"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))}]})))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = (Core.Name "Result"),
                  Core.caseStatementDefault = Nothing,
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "success"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "error"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))}]})))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in default branch",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = (Core.Name "Result"),
                  Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "foo"))),
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "success"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "error"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))}]})))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = (Core.Name "Result"),
                  Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "bar"))),
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "success"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "error"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))}]})))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string deeply nested in record in list in application",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "process")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermRecord (Core.Record {
                      Core.recordTypeName = (Core.Name "Item"),
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "value"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))}]})])})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "process")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermRecord (Core.Record {
                      Core.recordTypeName = (Core.Name "Item"),
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "value"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))}]})])})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in union inject value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermUnion (Core.Injection {
                  Core.injectionTypeName = (Core.Name "Result"),
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "success"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))}})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermUnion (Core.Injection {
                  Core.injectionTypeName = (Core.Name "Result"),
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "success"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))}})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in wrapped term",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "Email"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "foo"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "Email"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "bar"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in annotated term body",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "foo")),
                  Core.annotatedTermAnnotation = M.empty})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "bar")),
                  Core.annotatedTermAnnotation = M.empty})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in first of multiple let bindings",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "y"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "baz")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "bar")),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "y"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "baz")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in second of multiple let bindings",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "baz")),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "y"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "y"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "baz")),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "y"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "bar")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "y"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in all let bindings and body",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "y"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLiteral (Core.LiteralString "foo"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "bar")),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "y"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "bar")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLiteral (Core.LiteralString "bar"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in set",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermSet (S.fromList [
                  Core.TermLiteral (Core.LiteralString "baz"),
                  (Core.TermLiteral (Core.LiteralString "foo"))])))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermSet (S.fromList [
                  Core.TermLiteral (Core.LiteralString "bar"),
                  (Core.TermLiteral (Core.LiteralString "baz"))])))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in type lambda body",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermTypeLambda (Core.TypeLambda {
                  Core.typeLambdaParameter = (Core.Name "a"),
                  Core.typeLambdaBody = (Core.TermLiteral (Core.LiteralString "foo"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermTypeLambda (Core.TypeLambda {
                  Core.typeLambdaParameter = (Core.Name "a"),
                  Core.typeLambdaBody = (Core.TermLiteral (Core.LiteralString "bar"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in type application body",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermLiteral (Core.LiteralString "foo")),
                  Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermLiteral (Core.LiteralString "bar")),
                  Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in nested type lambdas",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermTypeLambda (Core.TypeLambda {
                  Core.typeLambdaParameter = (Core.Name "a"),
                  Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "b"),
                    Core.typeLambdaBody = (Core.TermLiteral (Core.LiteralString "foo"))}))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermTypeLambda (Core.TypeLambda {
                  Core.typeLambdaParameter = (Core.Name "a"),
                  Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "b"),
                    Core.typeLambdaBody = (Core.TermLiteral (Core.LiteralString "bar"))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in case branch within let binding",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "handler"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Result"),
                        Core.caseStatementDefault = Nothing,
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "ok"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))},
                          Core.Field {
                            Core.fieldName = (Core.Name "err"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))}]})))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "handler"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "handler"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Result"),
                        Core.caseStatementDefault = Nothing,
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "ok"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))},
                          Core.Field {
                            Core.fieldName = (Core.Name "err"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))}]})))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "handler"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string in annotated wrapped record field",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.rewriteTerm (\recurse -> \term -> Logic.ifElse (Equality.equal term (Core.TermLiteral (Core.LiteralString "foo"))) (Core.TermLiteral (Core.LiteralString "bar")) (recurse term)) (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermWrap (Core.WrappedTerm {
                    Core.wrappedTermTypeName = (Core.Name "User"),
                    Core.wrappedTermBody = (Core.TermRecord (Core.Record {
                      Core.recordTypeName = (Core.Name "UserData"),
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "name"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))}]}))})),
                  Core.annotatedTermAnnotation = M.empty})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermWrap (Core.WrappedTerm {
                    Core.wrappedTermTypeName = (Core.Name "User"),
                    Core.wrappedTermBody = (Core.TermRecord (Core.Record {
                      Core.recordTypeName = (Core.Name "UserData"),
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "name"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))}]}))})),
                  Core.annotatedTermAnnotation = M.empty})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "rewriteAndFoldTermWithPath",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "path tracking through application - sum literals",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Math.add acc (case term of
                  Core.TermLiteral v0 -> case v0 of
                    Core.LiteralInteger v1 -> case v1 of
                      Core.IntegerValueInt32 v2 -> v2
                      _ -> 0
                    _ -> 0
                  _ -> 0)) 0 (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "path tracking through nested applications",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Math.add acc (case term of
                  Core.TermLiteral v0 -> case v0 of
                    Core.LiteralInteger v1 -> case v1 of
                      Core.IntegerValueInt32 v2 -> v2
                      _ -> 0
                    _ -> 0
                  _ -> 0)) 0 (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "y"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermList [
                          Core.TermVariable (Core.Name "x"),
                          (Core.TermVariable (Core.Name "y"))])})))}))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "path tracking through let bindings",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Math.add acc (case term of
                  Core.TermLiteral v0 -> case v0 of
                    Core.LiteralInteger v1 -> case v1 of
                      Core.IntegerValueInt32 v2 -> v2
                      _ -> 0
                    _ -> 0
                  _ -> 0)) 0 (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermList [
                    Core.TermVariable (Core.Name "x"),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 32)))])}))))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "path tracking through record fields",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Math.add acc (case term of
                  Core.TermLiteral v0 -> case v0 of
                    Core.LiteralInteger v1 -> case v1 of
                      Core.IntegerValueInt32 v2 -> v2
                      _ -> 0
                    _ -> 0
                  _ -> 0)) 0 (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "Point"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "x"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))},
                    Core.Field {
                      Core.fieldName = (Core.Name "y"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))}]}))))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "path tracking through case branches",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Math.add acc (case term of
                  Core.TermLiteral v0 -> case v0 of
                    Core.LiteralInteger v1 -> case v1 of
                      Core.IntegerValueInt32 v2 -> v2
                      _ -> 0
                    _ -> 0
                  _ -> 0)) 0 (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = (Core.Name "Result"),
                  Core.caseStatementDefault = Nothing,
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "ok"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))},
                    Core.Field {
                      Core.fieldName = (Core.Name "err"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}]}))))))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "path tracking through pair",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Math.add acc (case term of
                  Core.TermLiteral v0 -> case v0 of
                    Core.LiteralInteger v1 -> case v1 of
                      Core.IntegerValueInt32 v2 -> v2
                      _ -> 0
                    _ -> 0
                  _ -> 0)) 0 (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 7)))))))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 12))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "path tracking through optional",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Math.add acc (case term of
                  Core.TermLiteral v0 -> case v0 of
                    Core.LiteralInteger v1 -> case v1 of
                      Core.IntegerValueInt32 v2 -> v2
                      _ -> 0
                    _ -> 0
                  _ -> 0)) 0 (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "path tracking through wrapped term",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Math.add acc (case term of
                  Core.TermLiteral v0 -> case v0 of
                    Core.LiteralInteger v1 -> case v1 of
                      Core.IntegerValueInt32 v2 -> v2
                      _ -> 0
                    _ -> 0
                  _ -> 0)) 0 (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "Age"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25)))}))))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "path tracking through type lambda",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Math.add acc (case term of
                  Core.TermLiteral v0 -> case v0 of
                    Core.LiteralInteger v1 -> case v1 of
                      Core.IntegerValueInt32 v2 -> v2
                      _ -> 0
                    _ -> 0
                  _ -> 0)) 0 (Core.TermTypeLambda (Core.TypeLambda {
                  Core.typeLambdaParameter = (Core.Name "a"),
                  Core.typeLambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 100)))}))))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 100))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "path tracking through type application",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Math.add acc (case term of
                  Core.TermLiteral v0 -> case v0 of
                    Core.LiteralInteger v1 -> case v1 of
                      Core.IntegerValueInt32 v2 -> v2
                      _ -> 0
                    _ -> 0
                  _ -> 0)) 0 (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 50))),
                  Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}))))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 50))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "path tracking through set elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Math.add acc (case term of
                  Core.TermLiteral v0 -> case v0 of
                    Core.LiteralInteger v1 -> case v1 of
                      Core.IntegerValueInt32 v2 -> v2
                      _ -> 0
                    _ -> 0
                  _ -> 0)) 0 (Core.TermSet (S.fromList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "deep nesting - application in lambda in let",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Math.add acc (case term of
                  Core.TermLiteral v0 -> case v0 of
                    Core.LiteralInteger v1 -> case v1 of
                      Core.IntegerValueInt32 v2 -> v2
                      _ -> 0
                    _ -> 0
                  _ -> 0)) 0 (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "x")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))}))}))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))}))))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 15))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "collect list lengths in nested structure",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Core.TermList (Lists.map (\n -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 n))) (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Lists.concat [
                  acc,
                  case term of
                    Core.TermList v0 -> [
                      Lists.length v0]
                    _ -> []]) [] (Core.TermList [
                  Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))],
                  (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))])]))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "collect list lengths in let body",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Core.TermList (Lists.map (\n -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 n))) (Rewriting.foldOverTerm Coders.TraversalOrderPre (\acc -> \term -> Lists.concat [
                  acc,
                  case term of
                    Core.TermList v0 -> [
                      Lists.length v0]
                    _ -> []]) [] (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "xs"),
                      Core.bindingTerm = (Core.TermList [
                        Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))]),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "unshadowVariables",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "literal unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermVariable (Core.Name "x")))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermVariable (Core.Name "x")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single lambda unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "distinct lambda parameters unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermList [
                      Core.TermVariable (Core.Name "x"),
                      (Core.TermVariable (Core.Name "y"))])})))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermList [
                      Core.TermVariable (Core.Name "x"),
                      (Core.TermVariable (Core.Name "y"))])})))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "let with no shadowing unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "let and lambda with distinct names unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermList [
                      Core.TermVariable (Core.Name "x"),
                      (Core.TermVariable (Core.Name "y"))])})))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermList [
                      Core.TermVariable (Core.Name "x"),
                      (Core.TermVariable (Core.Name "y"))])})))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "inner lambda shadows outer lambda",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x2"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x2"))})))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "inner lambda shadows outer - body references both",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermList [
                    Core.TermVariable (Core.Name "x"),
                    (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))])}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermList [
                    Core.TermVariable (Core.Name "x"),
                    (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x2"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x2"))})))])}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "triple nested lambda same name",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x2"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x3"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x3"))})))})))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "two parameters shadow sequentially",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "y"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermList [
                          Core.TermVariable (Core.Name "x"),
                          (Core.TermVariable (Core.Name "y"))])})))})))})))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x2"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "y2"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermList [
                          Core.TermVariable (Core.Name "x2"),
                          (Core.TermVariable (Core.Name "y2"))])})))})))})))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "lambda shadows let-bound variable",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x2"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x2"))})))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "lambda shadows one of multiple let bindings",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "y"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermList [
                      Core.TermVariable (Core.Name "x"),
                      (Core.TermVariable (Core.Name "y"))])})))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "y"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x2"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermList [
                      Core.TermVariable (Core.Name "x2"),
                      (Core.TermVariable (Core.Name "y"))])})))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "inner let body with lambda shadowing outer let",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermLet (Core.Let {
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
                    Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
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
                    Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x2"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x2"))})))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "shadowed lambda in function position of application",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "f"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "f"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "f"))}))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "f"))}))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "f"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "f2"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "f2"))}))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "f"))}))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "shadowed lambdas in list elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermList [
                    Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})),
                    (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))])}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermList [
                    Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x2"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x2"))})),
                    (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x2"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x2"))})))])}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "shadowed lambda in record field",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "Pair"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "fst"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "snd"),
                        Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}]}))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "Pair"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "fst"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x2"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x2"))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "snd"),
                        Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}]}))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "shadowed lambda in case branch",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "Maybe"),
                    Core.caseStatementDefault = Nothing,
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "nothing"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))},
                      Core.Field {
                        Core.fieldName = (Core.Name "just"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}]}))))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "Maybe"),
                    Core.caseStatementDefault = Nothing,
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "nothing"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))},
                      Core.Field {
                        Core.fieldName = (Core.Name "just"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x2"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x2"))})))}]}))))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "shadowed lambda in pair",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermPair (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})), (Core.TermVariable (Core.Name "x"))))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermPair (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x2"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x2"))})), (Core.TermVariable (Core.Name "x"))))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "shadowed lambda inside optional",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermMaybe (Just (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermMaybe (Just (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x2"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x2"))})))))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "shadowed lambda inside set element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermSet (S.fromList [
                    Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))]))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermSet (S.fromList [
                    Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x2"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x2"))}))]))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "shadowed lambda in union injection",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "Result"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "ok"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}}))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "Result"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "ok"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x2"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "x2"))})))}}))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "shadowed lambda inside wrapped term",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermWrap (Core.WrappedTerm {
                    Core.wrappedTermTypeName = (Core.Name "Age"),
                    Core.wrappedTermBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermWrap (Core.WrappedTerm {
                    Core.wrappedTermTypeName = (Core.Name "Age"),
                    Core.wrappedTermBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x2"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x2"))})))}))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "shadowed lambda inside type lambda",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "a"),
                    Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "a"),
                    Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x2"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x2"))})))}))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "shadowed lambda inside type application",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                    Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x2"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x2"))}))),
                    Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "shadowed lambda inside annotated term",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                    Core.annotatedTermAnnotation = M.empty}))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x2"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x2"))}))),
                    Core.annotatedTermAnnotation = M.empty}))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "shadowing at multiple depths",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "y"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermList [
                          Core.TermVariable (Core.Name "x"),
                          (Core.TermVariable (Core.Name "y"))])})))})))})))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x2"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "y2"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermList [
                          Core.TermVariable (Core.Name "x2"),
                          (Core.TermVariable (Core.Name "y2"))])})))})))})))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "let then lambda then lambda all same name",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x2"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x3"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x3"))})))})))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "lambda with shadowing in let binding value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "y"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "y")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "y"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x2"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x2"))}))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "y")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "application without shadowing unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "list of literals unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested record unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Rewriting.unshadowVariables (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "Point"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "x"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))},
                    Core.Field {
                      Core.fieldName = (Core.Name "y"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))}]})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "Point"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "x"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))},
                    Core.Field {
                      Core.fieldName = (Core.Name "y"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))}]})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
