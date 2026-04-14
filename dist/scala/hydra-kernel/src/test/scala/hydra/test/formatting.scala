package hydra.test.formatting

import hydra.testing.*

import hydra.util.*

lazy val allTests: hydra.testing.TestGroup = hydra.testing.TestGroup("formatting",
   None, Seq(hydra.test.formatting.caseConversionTests), Seq())

lazy val caseConversionTests: hydra.testing.TestGroup = hydra.testing.TestGroup("case conversion",
   None, Seq(), Seq(hydra.testing.TestCaseWithMetadata("#1 (lower_snake_case -> UPPER_SNAKE_CASE)",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.formatting.convertCase(hydra.util.CaseConvention.lowerSnake)(hydra.util.CaseConvention.upperSnake)("a_hello_world_42_a42_42a_b"),
   "A_HELLO_WORLD_42_A42_42A_B")), None, Seq()), hydra.testing.TestCaseWithMetadata("#2 (lower_snake_case -> camelCase)",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.formatting.convertCase(hydra.util.CaseConvention.lowerSnake)(hydra.util.CaseConvention.camel)("a_hello_world_42_a42_42a_b"),
   "aHelloWorld42A4242aB")), None, Seq()), hydra.testing.TestCaseWithMetadata("#3 (lower_snake_case -> PascalCase)",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.formatting.convertCase(hydra.util.CaseConvention.lowerSnake)(hydra.util.CaseConvention.pascal)("a_hello_world_42_a42_42a_b"),
   "AHelloWorld42A4242aB")), None, Seq()), hydra.testing.TestCaseWithMetadata("#4 (lower_snake_case -> lower_snake_case)",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.formatting.convertCase(hydra.util.CaseConvention.lowerSnake)(hydra.util.CaseConvention.lowerSnake)("a_hello_world_42_a42_42a_b"),
   "a_hello_world_42_a42_42a_b")), None, Seq()), hydra.testing.TestCaseWithMetadata("#5 (UPPER_SNAKE_CASE -> lower_snake_case)",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.formatting.convertCase(hydra.util.CaseConvention.upperSnake)(hydra.util.CaseConvention.lowerSnake)("A_HELLO_WORLD_42_A42_42A_B"),
   "a_hello_world_42_a42_42a_b")), None, Seq()), hydra.testing.TestCaseWithMetadata("#6 (UPPER_SNAKE_CASE -> camelCase)",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.formatting.convertCase(hydra.util.CaseConvention.upperSnake)(hydra.util.CaseConvention.camel)("A_HELLO_WORLD_42_A42_42A_B"),
   "aHelloWorld42A4242aB")), None, Seq()), hydra.testing.TestCaseWithMetadata("#7 (UPPER_SNAKE_CASE -> PascalCase)",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.formatting.convertCase(hydra.util.CaseConvention.upperSnake)(hydra.util.CaseConvention.pascal)("A_HELLO_WORLD_42_A42_42A_B"),
   "AHelloWorld42A4242aB")), None, Seq()), hydra.testing.TestCaseWithMetadata("#8 (UPPER_SNAKE_CASE -> UPPER_SNAKE_CASE)",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.formatting.convertCase(hydra.util.CaseConvention.upperSnake)(hydra.util.CaseConvention.upperSnake)("A_HELLO_WORLD_42_A42_42A_B"),
   "A_HELLO_WORLD_42_A42_42A_B")), None, Seq()), hydra.testing.TestCaseWithMetadata("#9 (camelCase -> lower_snake_case)",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.formatting.convertCase(hydra.util.CaseConvention.camel)(hydra.util.CaseConvention.lowerSnake)("aHelloWorld42A4242aB"),
   "a_hello_world42_a4242a_b")), None, Seq()), hydra.testing.TestCaseWithMetadata("#10 (camelCase -> UPPER_SNAKE_CASE)",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.formatting.convertCase(hydra.util.CaseConvention.camel)(hydra.util.CaseConvention.upperSnake)("aHelloWorld42A4242aB"),
   "A_HELLO_WORLD42_A4242A_B")), None, Seq()), hydra.testing.TestCaseWithMetadata("#11 (camelCase -> PascalCase)",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.formatting.convertCase(hydra.util.CaseConvention.camel)(hydra.util.CaseConvention.pascal)("aHelloWorld42A4242aB"),
   "AHelloWorld42A4242aB")), None, Seq()), hydra.testing.TestCaseWithMetadata("#12 (camelCase -> camelCase)",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.formatting.convertCase(hydra.util.CaseConvention.camel)(hydra.util.CaseConvention.camel)("aHelloWorld42A4242aB"),
   "aHelloWorld42A4242aB")), None, Seq()), hydra.testing.TestCaseWithMetadata("#13 (PascalCase -> lower_snake_case)",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.formatting.convertCase(hydra.util.CaseConvention.pascal)(hydra.util.CaseConvention.lowerSnake)("AHelloWorld42A4242aB"),
   "a_hello_world42_a4242a_b")), None, Seq()), hydra.testing.TestCaseWithMetadata("#14 (PascalCase -> UPPER_SNAKE_CASE)",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.formatting.convertCase(hydra.util.CaseConvention.pascal)(hydra.util.CaseConvention.upperSnake)("AHelloWorld42A4242aB"),
   "A_HELLO_WORLD42_A4242A_B")), None, Seq()), hydra.testing.TestCaseWithMetadata("#15 (PascalCase -> camelCase)",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.formatting.convertCase(hydra.util.CaseConvention.pascal)(hydra.util.CaseConvention.camel)("AHelloWorld42A4242aB"),
   "aHelloWorld42A4242aB")), None, Seq()), hydra.testing.TestCaseWithMetadata("#16 (PascalCase -> PascalCase)",
   hydra.testing.TestCase.universal(hydra.testing.UniversalTestCase(hydra.formatting.convertCase(hydra.util.CaseConvention.pascal)(hydra.util.CaseConvention.pascal)("AHelloWorld42A4242aB"),
   "AHelloWorld42A4242aB")), None, Seq())))
