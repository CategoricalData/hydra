-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for automatic differentiation

module Hydra.Test.Differentiation where

import qualified Hydra.Core as Core
import qualified Hydra.Differentiation as Differentiation
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Reduction as Reduction
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Test.TestGraph as TestGraph
import qualified Hydra.Testing as Testing
import qualified Hydra.Variables as Variables
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | Test cases for automatic differentiation
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "differentiation",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "basic rules",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable wrt itself",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Differentiation.differentiateTerm (Core.Name "x") (Core.TermVariable (Core.Name "x")))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable wrt different variable",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Differentiation.differentiateTerm (Core.Name "x") (Core.TermVariable (Core.Name "y")))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "float64 literal",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Differentiation.differentiateTerm (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 42.0))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "zero literal",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Differentiation.differentiateTerm (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "identity function",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Differentiation.differentiateFunction (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0)))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "constant function",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Differentiation.differentiateFunction (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 5.0)))}))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0)))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "lambda binding different variable",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Differentiation.differentiateTerm (Core.Name "x") (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0)))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "lambda shadowing differentiation variable",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Differentiation.differentiateTerm (Core.Name "x") (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0)))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "unary primitives",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "sin of variable",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.sin")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.cos")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0)))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "exp of variable",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.exp")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.exp")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0)))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "sin of different variable",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.sin")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.cos")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0)))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "binary primitives",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "add variable to itself",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.addFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0)))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0)))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multiply variable by itself",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mul")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.addFloat64")),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0)))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0)))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "subtract constant from variable",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.sub")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 5.0)))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.subFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0)))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0)))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "chain rule",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested sin(cos(x))",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.sin")),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.cos")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.cos")),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.cos")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "_x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.negateFloat64")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.sin")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "_x"))}))}))}))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0)))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "structural",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "list of terms",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Differentiation.differentiateTerm (Core.Name "x") (Core.TermList [
                  Core.TermVariable (Core.Name "x"),
                  (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 5.0)))]))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0)),
                  (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "pair of terms",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Differentiation.differentiateTerm (Core.Name "x") (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 5.0))))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermPair (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0)), (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0))))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unit term",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Differentiation.differentiateTerm (Core.Name "x") Core.TermUnit)),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "evaluate derivatives",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "d/dx(x) at 3.0",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.roundFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 12)))})),
                  Core.applicationArgument = (Variables.replaceFreeTermVariable (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.0))) (Differentiation.differentiateTerm (Core.Name "x") (Core.TermVariable (Core.Name "x"))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "d/dx(42.0) at 3.0",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.roundFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 12)))})),
                  Core.applicationArgument = (Variables.replaceFreeTermVariable (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.0))) (Differentiation.differentiateTerm (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 42.0)))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "d/dx(x+x) at 3.0",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.roundFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 12)))})),
                  Core.applicationArgument = (Variables.replaceFreeTermVariable (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.0))) (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 2.0))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "d/dx(x*x) at 3.0",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.roundFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 12)))})),
                  Core.applicationArgument = (Variables.replaceFreeTermVariable (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.0))) (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mul")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 6.0))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "d/dx(x*x) at 5.0",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.roundFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 12)))})),
                  Core.applicationArgument = (Variables.replaceFreeTermVariable (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 5.0))) (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mul")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 10.0))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "d/dx(x-5) at 7.0",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.roundFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 12)))})),
                  Core.applicationArgument = (Variables.replaceFreeTermVariable (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 7.0))) (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.sub")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 5.0)))}))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "d/dx(sin(x)) at 0.0",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.roundFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 12)))})),
                  Core.applicationArgument = (Variables.replaceFreeTermVariable (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0))) (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.sin")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "d/dx(sin(x)) at pi/2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.roundFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 12)))})),
                  Core.applicationArgument = (Variables.replaceFreeTermVariable (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.5707963267948966))) (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.sin")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 6.12323399574e-17))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "d/dx(cos(x)) at 1.0",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.roundFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 12)))})),
                  Core.applicationArgument = (Variables.replaceFreeTermVariable (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0))) (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.cos")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-0.841470984808)))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "d/dx(cos(x)) at pi",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.roundFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 12)))})),
                  Core.applicationArgument = (Variables.replaceFreeTermVariable (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.141592653589793))) (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.cos")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-1.22464679915e-16)))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "d/dx(exp(x)) at 0.0",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.roundFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 12)))})),
                  Core.applicationArgument = (Variables.replaceFreeTermVariable (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0))) (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.exp")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "d/dx(exp(x)) at 1.0",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.roundFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 12)))})),
                  Core.applicationArgument = (Variables.replaceFreeTermVariable (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0))) (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.exp")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 2.71828182846))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "d/dx(log(x)) at 1.0",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.roundFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 12)))})),
                  Core.applicationArgument = (Variables.replaceFreeTermVariable (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0))) (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.log")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "d/dx(log(x)) at e",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.roundFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 12)))})),
                  Core.applicationArgument = (Variables.replaceFreeTermVariable (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 2.718281828459045))) (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.log")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.367879441171))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "d/dx(sqrt(x)) at 4.0",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.roundFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 12)))})),
                  Core.applicationArgument = (Variables.replaceFreeTermVariable (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 4.0))) (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.sqrt")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.25))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "d/dx(x^3) at 2.0",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.roundFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 12)))})),
                  Core.applicationArgument = (Variables.replaceFreeTermVariable (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 2.0))) (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.pow")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.0)))}))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 12.0))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "d/dx(sin(cos(x))) at 0.5",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.roundFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 12)))})),
                  Core.applicationArgument = (Variables.replaceFreeTermVariable (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.5))) (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.sin")),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.cos")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-0.30635890919)))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "d/dx(sin(cos(x))) at 1.0",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.roundFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 12)))})),
                  Core.applicationArgument = (Variables.replaceFreeTermVariable (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0))) (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.sin")),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.cos")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-0.721606149063)))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "d/dx(exp(sin(x))) at 0.0",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.roundFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 12)))})),
                  Core.applicationArgument = (Variables.replaceFreeTermVariable (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0))) (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.exp")),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.sin")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "d/dx(x*sin(x)) at pi",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.roundFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 12)))})),
                  Core.applicationArgument = (Variables.replaceFreeTermVariable (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.141592653589793))) (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mul")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.sin")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-3.14159265359)))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "gradient check",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "x^2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.roundFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
                  Core.applicationArgument = (Variables.replaceFreeTermVariable (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 2.0))) (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mul")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 4.0))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "sin(x)",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.roundFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
                  Core.applicationArgument = (Variables.replaceFreeTermVariable (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0))) (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.sin")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.5403))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "exp(x)",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.roundFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
                  Core.applicationArgument = (Variables.replaceFreeTermVariable (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.5))) (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.exp")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.6487))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "x^3",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.roundFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
                  Core.applicationArgument = (Variables.replaceFreeTermVariable (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.5))) (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.pow")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.0)))}))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 6.75))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "log(x)",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.roundFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
                  Core.applicationArgument = (Variables.replaceFreeTermVariable (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 2.0))) (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.log")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.5))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "cos(x)",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.roundFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
                  Core.applicationArgument = (Variables.replaceFreeTermVariable (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0))) (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.cos")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-0.84147)))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "sin(cos(x))",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.roundFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
                  Core.applicationArgument = (Variables.replaceFreeTermVariable (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0))) (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.sin")),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.cos")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-0.72161)))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "x*sin(x)",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.roundFloat64")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
                  Core.applicationArgument = (Variables.replaceFreeTermVariable (Core.Name "x") (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 2.0))) (Differentiation.differentiateTerm (Core.Name "x") (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mul")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.sin")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 7.7004e-2))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "gradient",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "add two variables",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Differentiation.gradient (Core.Name "Gradient") [
                  Core.Name "x",
                  (Core.Name "y")] (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.addFloat64")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "Gradient"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "x"),
                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.addFloat64")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0)))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0)))}))},
                    Core.Field {
                      Core.fieldName = (Core.Name "y"),
                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.addFloat64")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0)))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0)))}))}]})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "product of two variables",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Differentiation.gradient (Core.Name "Gradient") [
                  Core.Name "x",
                  (Core.Name "y")] (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "Gradient"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "x"),
                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.addFloat64")),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0)))}))})),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0)))}))}))},
                    Core.Field {
                      Core.fieldName = (Core.Name "y"),
                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.addFloat64")),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0)))}))})),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0)))}))}))}]})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single variable partial",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Differentiation.gradient (Core.Name "Gradient") [
                  Core.Name "x",
                  (Core.Name "y")] (Core.TermVariable (Core.Name "x")))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "Gradient"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "x"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0)))},
                    Core.Field {
                      Core.fieldName = (Core.Name "y"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0)))}]})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "constant",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Differentiation.gradient (Core.Name "Gradient") [
                  Core.Name "x",
                  (Core.Name "y")] (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 7.0))))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "Gradient"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "x"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0)))},
                    Core.Field {
                      Core.fieldName = (Core.Name "y"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0)))}]})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
