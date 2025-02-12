"""Test cases for primitive functions."""

from __future__ import annotations
import hydra.core
import hydra.testing

allTests = hydra.testing.TestGroup("All tests", None, [
  hydra.testing.TestGroup("hydra/lib/lists primitives", None, [
    hydra.testing.TestGroup("apply", None, [], [
      hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/lists.apply"))), hydra.core.TermList([
        hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/strings.toUpper"))),
        hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/strings.toLower")))]))), hydra.core.TermList([
        hydra.core.TermLiteral(hydra.core.LiteralString("One")),
        hydra.core.TermLiteral(hydra.core.LiteralString("Two")),
        hydra.core.TermLiteral(hydra.core.LiteralString("Three"))]))), hydra.core.TermList([
        hydra.core.TermLiteral(hydra.core.LiteralString("ONE")),
        hydra.core.TermLiteral(hydra.core.LiteralString("TWO")),
        hydra.core.TermLiteral(hydra.core.LiteralString("THREE")),
        hydra.core.TermLiteral(hydra.core.LiteralString("one")),
        hydra.core.TermLiteral(hydra.core.LiteralString("two")),
        hydra.core.TermLiteral(hydra.core.LiteralString("three"))]))), None, [])]),
    hydra.testing.TestGroup("bind", None, [], [
      hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/lists.bind"))), hydra.core.TermList([
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(1))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(2))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(3))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(4)))]))), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/lists.pure"))), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/math.neg"))), hydra.core.TermVariable(hydra.core.Name("x"))))))))))), hydra.core.TermList([
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(-1))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(-2))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(-3))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(-4)))]))), None, [])]),
    hydra.testing.TestGroup("concat", None, [], [
      hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/lists.concat"))), hydra.core.TermList([
        hydra.core.TermList([
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(1))),
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(2))),
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(3)))]),
        hydra.core.TermList([
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(4))),
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(5)))]),
        hydra.core.TermList([
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(6))),
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(7))),
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(8)))])]))), hydra.core.TermList([
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(1))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(2))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(3))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(4))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(5))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(6))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(7))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(8)))]))), None, [])]),
    hydra.testing.TestGroup("head", None, [], [
      hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/lists.head"))), hydra.core.TermList([
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(1))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(2))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(3)))]))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(1))))), None, [])]),
    hydra.testing.TestGroup("intercalate", None, [], [
      hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/lists.intercalate"))), hydra.core.TermList([
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0)))]))), hydra.core.TermList([
        hydra.core.TermList([
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(1))),
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(2))),
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(3)))]),
        hydra.core.TermList([
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(4))),
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(5)))]),
        hydra.core.TermList([
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(6))),
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(7))),
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(8)))])]))), hydra.core.TermList([
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(1))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(2))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(3))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(4))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(5))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(6))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(7))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(8)))]))), None, [])]),
    hydra.testing.TestGroup("intersperse", None, [], [
      hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/lists.intersperse"))), hydra.core.TermLiteral(hydra.core.LiteralString("and")))), hydra.core.TermList([
        hydra.core.TermLiteral(hydra.core.LiteralString("one")),
        hydra.core.TermLiteral(hydra.core.LiteralString("two")),
        hydra.core.TermLiteral(hydra.core.LiteralString("three"))]))), hydra.core.TermList([
        hydra.core.TermLiteral(hydra.core.LiteralString("one")),
        hydra.core.TermLiteral(hydra.core.LiteralString("and")),
        hydra.core.TermLiteral(hydra.core.LiteralString("two")),
        hydra.core.TermLiteral(hydra.core.LiteralString("and")),
        hydra.core.TermLiteral(hydra.core.LiteralString("three"))]))), None, [])]),
    hydra.testing.TestGroup("last", None, [], [
      hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/lists.last"))), hydra.core.TermList([
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(1))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(2))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(3)))]))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(3))))), None, [])]),
    hydra.testing.TestGroup("length", None, [], [
      hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/lists.length"))), hydra.core.TermList([
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(1))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(2))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(3)))]))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(3))))), None, [])]),
    hydra.testing.TestGroup("map", None, [], [
      hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/lists.map"))), hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/strings.toUpper"))))), hydra.core.TermList([
        hydra.core.TermLiteral(hydra.core.LiteralString("one")),
        hydra.core.TermLiteral(hydra.core.LiteralString("two"))]))), hydra.core.TermList([
        hydra.core.TermLiteral(hydra.core.LiteralString("ONE")),
        hydra.core.TermLiteral(hydra.core.LiteralString("TWO"))]))), None, [])]),
    hydra.testing.TestGroup("pure", None, [], [
      hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/lists.pure"))), hydra.core.TermLiteral(hydra.core.LiteralString("one")))), hydra.core.TermList([hydra.core.TermLiteral(hydra.core.LiteralString("one"))]))), None, [])])], []),
  hydra.testing.TestGroup("hydra/lib/strings primitives", None, [
    hydra.testing.TestGroup("cat", None, [], [
      hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/strings.cat"))), hydra.core.TermList([
        hydra.core.TermLiteral(hydra.core.LiteralString("one")),
        hydra.core.TermLiteral(hydra.core.LiteralString("two")),
        hydra.core.TermLiteral(hydra.core.LiteralString("three"))]))), hydra.core.TermLiteral(hydra.core.LiteralString("onetwothree")))), None, []),
      hydra.testing.TestCaseWithMetadata("2", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/strings.cat"))), hydra.core.TermList([
        hydra.core.TermLiteral(hydra.core.LiteralString("")),
        hydra.core.TermLiteral(hydra.core.LiteralString("one")),
        hydra.core.TermLiteral(hydra.core.LiteralString("")),
        hydra.core.TermLiteral(hydra.core.LiteralString(""))]))), hydra.core.TermLiteral(hydra.core.LiteralString("one")))), None, []),
      hydra.testing.TestCaseWithMetadata("3", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/strings.cat"))), hydra.core.TermList([]))), hydra.core.TermLiteral(hydra.core.LiteralString("")))), None, [])]),
    hydra.testing.TestGroup("length", None, [], [
      hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/strings.length"))), hydra.core.TermLiteral(hydra.core.LiteralString("")))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))))), None, []),
      hydra.testing.TestCaseWithMetadata("2", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/strings.length"))), hydra.core.TermLiteral(hydra.core.LiteralString("a")))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(1))))), None, []),
      hydra.testing.TestCaseWithMetadata("3", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/strings.length"))), hydra.core.TermLiteral(hydra.core.LiteralString("one")))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(3))))), None, [])]),
    hydra.testing.TestGroup("splitOn", None, [], [
      hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/strings.splitOn"))), hydra.core.TermLiteral(hydra.core.LiteralString("ss")))), hydra.core.TermLiteral(hydra.core.LiteralString("Mississippi")))), hydra.core.TermList([
        hydra.core.TermLiteral(hydra.core.LiteralString("Mi")),
        hydra.core.TermLiteral(hydra.core.LiteralString("i")),
        hydra.core.TermLiteral(hydra.core.LiteralString("ippi"))]))), None, []),
      hydra.testing.TestCaseWithMetadata("2", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/strings.splitOn"))), hydra.core.TermLiteral(hydra.core.LiteralString("Mississippi")))), hydra.core.TermLiteral(hydra.core.LiteralString("Mississippi")))), hydra.core.TermList([
        hydra.core.TermLiteral(hydra.core.LiteralString("")),
        hydra.core.TermLiteral(hydra.core.LiteralString(""))]))), None, []),
      hydra.testing.TestCaseWithMetadata("3", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/strings.splitOn"))), hydra.core.TermLiteral(hydra.core.LiteralString(" ")))), hydra.core.TermLiteral(hydra.core.LiteralString("one two three")))), hydra.core.TermList([
        hydra.core.TermLiteral(hydra.core.LiteralString("one")),
        hydra.core.TermLiteral(hydra.core.LiteralString("two")),
        hydra.core.TermLiteral(hydra.core.LiteralString("three"))]))), None, []),
      hydra.testing.TestCaseWithMetadata("4", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/strings.splitOn"))), hydra.core.TermLiteral(hydra.core.LiteralString(" ")))), hydra.core.TermLiteral(hydra.core.LiteralString(" one two three ")))), hydra.core.TermList([
        hydra.core.TermLiteral(hydra.core.LiteralString("")),
        hydra.core.TermLiteral(hydra.core.LiteralString("one")),
        hydra.core.TermLiteral(hydra.core.LiteralString("two")),
        hydra.core.TermLiteral(hydra.core.LiteralString("three")),
        hydra.core.TermLiteral(hydra.core.LiteralString(""))]))), None, []),
      hydra.testing.TestCaseWithMetadata("5", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/strings.splitOn"))), hydra.core.TermLiteral(hydra.core.LiteralString(" ")))), hydra.core.TermLiteral(hydra.core.LiteralString("  one two three")))), hydra.core.TermList([
        hydra.core.TermLiteral(hydra.core.LiteralString("")),
        hydra.core.TermLiteral(hydra.core.LiteralString("")),
        hydra.core.TermLiteral(hydra.core.LiteralString("one")),
        hydra.core.TermLiteral(hydra.core.LiteralString("two")),
        hydra.core.TermLiteral(hydra.core.LiteralString("three"))]))), None, []),
      hydra.testing.TestCaseWithMetadata("6", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/strings.splitOn"))), hydra.core.TermLiteral(hydra.core.LiteralString("  ")))), hydra.core.TermLiteral(hydra.core.LiteralString("  one two three")))), hydra.core.TermList([
        hydra.core.TermLiteral(hydra.core.LiteralString("")),
        hydra.core.TermLiteral(hydra.core.LiteralString("one two three"))]))), None, []),
      hydra.testing.TestCaseWithMetadata("6", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/strings.splitOn"))), hydra.core.TermLiteral(hydra.core.LiteralString("aa")))), hydra.core.TermLiteral(hydra.core.LiteralString("aaa")))), hydra.core.TermList([
        hydra.core.TermLiteral(hydra.core.LiteralString("")),
        hydra.core.TermLiteral(hydra.core.LiteralString("a"))]))), None, []),
      hydra.testing.TestCaseWithMetadata("7", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/strings.splitOn"))), hydra.core.TermLiteral(hydra.core.LiteralString("a")))), hydra.core.TermLiteral(hydra.core.LiteralString("")))), hydra.core.TermList([hydra.core.TermLiteral(hydra.core.LiteralString(""))]))), None, []),
      hydra.testing.TestCaseWithMetadata("8", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/strings.splitOn"))), hydra.core.TermLiteral(hydra.core.LiteralString("")))), hydra.core.TermLiteral(hydra.core.LiteralString("abc")))), hydra.core.TermList([
        hydra.core.TermLiteral(hydra.core.LiteralString("")),
        hydra.core.TermLiteral(hydra.core.LiteralString("a")),
        hydra.core.TermLiteral(hydra.core.LiteralString("b")),
        hydra.core.TermLiteral(hydra.core.LiteralString("c"))]))), None, []),
      hydra.testing.TestCaseWithMetadata("9", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/strings.splitOn"))), hydra.core.TermLiteral(hydra.core.LiteralString("")))), hydra.core.TermLiteral(hydra.core.LiteralString("")))), hydra.core.TermList([hydra.core.TermLiteral(hydra.core.LiteralString(""))]))), None, [])]),
    hydra.testing.TestGroup("toLower", None, [], [
      hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/strings.toLower"))), hydra.core.TermLiteral(hydra.core.LiteralString("One TWO threE")))), hydra.core.TermLiteral(hydra.core.LiteralString("one two three")))), None, []),
      hydra.testing.TestCaseWithMetadata("2", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/strings.toLower"))), hydra.core.TermLiteral(hydra.core.LiteralString("Abc123")))), hydra.core.TermLiteral(hydra.core.LiteralString("abc123")))), None, [])]),
    hydra.testing.TestGroup("toUpper", None, [], [
      hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/strings.toUpper"))), hydra.core.TermLiteral(hydra.core.LiteralString("One TWO threE")))), hydra.core.TermLiteral(hydra.core.LiteralString("ONE TWO THREE")))), None, []),
      hydra.testing.TestCaseWithMetadata("2", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyleEager(hydra.core.Unit()), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/strings.toUpper"))), hydra.core.TermLiteral(hydra.core.LiteralString("Abc123")))), hydra.core.TermLiteral(hydra.core.LiteralString("ABC123")))), None, [])])], []),
  hydra.testing.TestGroup("inference tests", None, [
    hydra.testing.TestGroup("Application terms", "Check a few hand-picked application terms", [], [
      hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermVariable(hydra.core.Name("x"))))), hydra.core.TermLiteral(hydra.core.LiteralString("foo")))), hydra.core.TypeScheme([], hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))))), None, []),
      hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/math.sub"))), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/math.add"))), hydra.core.TermVariable(hydra.core.Name("x")))), hydra.core.TermVariable(hydra.core.Name("x")))))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(1)))))))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerTypeInt32(hydra.core.Unit()))), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerTypeInt32(hydra.core.Unit())))))))), None, [])]),
    hydra.testing.TestGroup("Function terms", "Check a few hand-picked function terms", [
      hydra.testing.TestGroup("Check lambdas", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermVariable(hydra.core.Name("x"))))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeVariable(hydra.core.Name("t0"))))))), None, []),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt16(137)))))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerTypeInt16(hydra.core.Unit())))))))), None, [])]),
      hydra.testing.TestGroup("Check list eliminations", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionElimination(hydra.core.EliminationList(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/math.add")))))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerTypeInt32(hydra.core.Unit()))), hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeList(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerTypeInt32(hydra.core.Unit())))), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerTypeInt32(hydra.core.Unit())))))))))), None, []),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionElimination(hydra.core.EliminationList(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/math.add")))))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeList(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerTypeInt32(hydra.core.Unit())))), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerTypeInt32(hydra.core.Unit())))))))), None, []),
        hydra.testing.TestCaseWithMetadata("#3", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionElimination(hydra.core.EliminationList(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra/lib/math.add")))))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))))), hydra.core.TermList([
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(1))),
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(2))),
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(3))),
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(4))),
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(5)))]))), hydra.core.TypeScheme([], hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerTypeInt32(hydra.core.Unit())))))), None, [])])], [])], [])], [])