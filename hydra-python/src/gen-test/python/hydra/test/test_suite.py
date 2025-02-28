"""Test cases for primitive functions."""

from __future__ import annotations
import hydra.core
import hydra.lib.maps
import hydra.mantle
import hydra.test.test_graph
import hydra.testing

allTests = hydra.testing.TestGroup("All tests", None, ["formattingTests", "inferenceTests", "primitiveTests"], [])

formattingTests = hydra.testing.TestGroup("formatting tests", None, [], [
  hydra.testing.TestCaseWithMetadata("#1 (lower_snake_case -> UPPER_SNAKE_CASE)", hydra.testing.TestCaseCaseConversion(hydra.testing.CaseConversionTestCase(hydra.mantle.CaseConvention.LOWER_SNAKE, hydra.mantle.CaseConvention.UPPER_SNAKE, "a_hello_world_42_a42_42a_b", "A_HELLO_WORLD_42_A42_42A_B")), None, []),
  hydra.testing.TestCaseWithMetadata("#2 (lower_snake_case -> camelCase)", hydra.testing.TestCaseCaseConversion(hydra.testing.CaseConversionTestCase(hydra.mantle.CaseConvention.LOWER_SNAKE, hydra.mantle.CaseConvention.CAMEL, "a_hello_world_42_a42_42a_b", "aHelloWorld42A4242aB")), None, []),
  hydra.testing.TestCaseWithMetadata("#3 (lower_snake_case -> PascalCase)", hydra.testing.TestCaseCaseConversion(hydra.testing.CaseConversionTestCase(hydra.mantle.CaseConvention.LOWER_SNAKE, hydra.mantle.CaseConvention.PASCAL, "a_hello_world_42_a42_42a_b", "AHelloWorld42A4242aB")), None, []),
  hydra.testing.TestCaseWithMetadata("#4 (lower_snake_case -> lower_snake_case)", hydra.testing.TestCaseCaseConversion(hydra.testing.CaseConversionTestCase(hydra.mantle.CaseConvention.LOWER_SNAKE, hydra.mantle.CaseConvention.LOWER_SNAKE, "a_hello_world_42_a42_42a_b", "a_hello_world_42_a42_42a_b")), None, []),
  hydra.testing.TestCaseWithMetadata("#5 (UPPER_SNAKE_CASE -> lower_snake_case)", hydra.testing.TestCaseCaseConversion(hydra.testing.CaseConversionTestCase(hydra.mantle.CaseConvention.UPPER_SNAKE, hydra.mantle.CaseConvention.LOWER_SNAKE, "A_HELLO_WORLD_42_A42_42A_B", "a_hello_world_42_a42_42a_b")), None, []),
  hydra.testing.TestCaseWithMetadata("#6 (UPPER_SNAKE_CASE -> camelCase)", hydra.testing.TestCaseCaseConversion(hydra.testing.CaseConversionTestCase(hydra.mantle.CaseConvention.UPPER_SNAKE, hydra.mantle.CaseConvention.CAMEL, "A_HELLO_WORLD_42_A42_42A_B", "aHelloWorld42A4242aB")), None, []),
  hydra.testing.TestCaseWithMetadata("#7 (UPPER_SNAKE_CASE -> PascalCase)", hydra.testing.TestCaseCaseConversion(hydra.testing.CaseConversionTestCase(hydra.mantle.CaseConvention.UPPER_SNAKE, hydra.mantle.CaseConvention.PASCAL, "A_HELLO_WORLD_42_A42_42A_B", "AHelloWorld42A4242aB")), None, []),
  hydra.testing.TestCaseWithMetadata("#8 (UPPER_SNAKE_CASE -> UPPER_SNAKE_CASE)", hydra.testing.TestCaseCaseConversion(hydra.testing.CaseConversionTestCase(hydra.mantle.CaseConvention.UPPER_SNAKE, hydra.mantle.CaseConvention.UPPER_SNAKE, "A_HELLO_WORLD_42_A42_42A_B", "A_HELLO_WORLD_42_A42_42A_B")), None, []),
  hydra.testing.TestCaseWithMetadata("#9 (camelCase -> lower_snake_case)", hydra.testing.TestCaseCaseConversion(hydra.testing.CaseConversionTestCase(hydra.mantle.CaseConvention.CAMEL, hydra.mantle.CaseConvention.LOWER_SNAKE, "aHelloWorld42A4242aB", "a_hello_world42_a4242a_b")), None, []),
  hydra.testing.TestCaseWithMetadata("#10 (camelCase -> UPPER_SNAKE_CASE)", hydra.testing.TestCaseCaseConversion(hydra.testing.CaseConversionTestCase(hydra.mantle.CaseConvention.CAMEL, hydra.mantle.CaseConvention.UPPER_SNAKE, "aHelloWorld42A4242aB", "A_HELLO_WORLD42_A4242A_B")), None, []),
  hydra.testing.TestCaseWithMetadata("#11 (camelCase -> PascalCase)", hydra.testing.TestCaseCaseConversion(hydra.testing.CaseConversionTestCase(hydra.mantle.CaseConvention.CAMEL, hydra.mantle.CaseConvention.PASCAL, "aHelloWorld42A4242aB", "AHelloWorld42A4242aB")), None, []),
  hydra.testing.TestCaseWithMetadata("#12 (camelCase -> camelCase)", hydra.testing.TestCaseCaseConversion(hydra.testing.CaseConversionTestCase(hydra.mantle.CaseConvention.CAMEL, hydra.mantle.CaseConvention.CAMEL, "aHelloWorld42A4242aB", "aHelloWorld42A4242aB")), None, []),
  hydra.testing.TestCaseWithMetadata("#13 (PascalCase -> lower_snake_case)", hydra.testing.TestCaseCaseConversion(hydra.testing.CaseConversionTestCase(hydra.mantle.CaseConvention.PASCAL, hydra.mantle.CaseConvention.LOWER_SNAKE, "AHelloWorld42A4242aB", "a_hello_world42_a4242a_b")), None, []),
  hydra.testing.TestCaseWithMetadata("#14 (PascalCase -> UPPER_SNAKE_CASE)", hydra.testing.TestCaseCaseConversion(hydra.testing.CaseConversionTestCase(hydra.mantle.CaseConvention.PASCAL, hydra.mantle.CaseConvention.UPPER_SNAKE, "AHelloWorld42A4242aB", "A_HELLO_WORLD42_A4242A_B")), None, []),
  hydra.testing.TestCaseWithMetadata("#15 (PascalCase -> camelCase)", hydra.testing.TestCaseCaseConversion(hydra.testing.CaseConversionTestCase(hydra.mantle.CaseConvention.PASCAL, hydra.mantle.CaseConvention.CAMEL, "AHelloWorld42A4242aB", "aHelloWorld42A4242aB")), None, []),
  hydra.testing.TestCaseWithMetadata("#16 (PascalCase -> PascalCase)", hydra.testing.TestCaseCaseConversion(hydra.testing.CaseConversionTestCase(hydra.mantle.CaseConvention.PASCAL, hydra.mantle.CaseConvention.PASCAL, "AHelloWorld42A4242aB", "AHelloWorld42A4242aB")), None, [])])

inferenceTests = hydra.testing.TestGroup("Inference tests", None, [
  hydra.testing.TestGroup("Algebraic terms", None, [
    hydra.testing.TestGroup("List eliminations (folds)", None, [], [
      # hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionElimination(hydra.core.EliminationList(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.math.add")))))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeList(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))))))), None, [
      #   hydra.testing.Tag("disabledForAlgorithmWInference"),
      #   hydra.testing.Tag("disabledForAltInference")]),
      hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionElimination(hydra.core.EliminationList(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.math.add")))))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeList(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))))), None, [
        hydra.testing.Tag("disabledForAlgorithmWInference"),
        hydra.testing.Tag("disabledForAltInference")]),
      hydra.testing.TestCaseWithMetadata("#3", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionElimination(hydra.core.EliminationList(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.math.add")))))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))))), hydra.core.TermList([
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(1))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(2))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(3))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(4))),
        hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(5)))]))), hydra.core.TypeScheme([], hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))), None, [
        hydra.testing.Tag("disabledForAlgorithmWInference"),
        hydra.testing.Tag("disabledForAltInference")])]),
    hydra.testing.TestGroup("List terms", None, [
      hydra.testing.TestGroup("List of strings", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermList([
          hydra.core.TermLiteral(hydra.core.LiteralString("foo")),
          hydra.core.TermLiteral(hydra.core.LiteralString("bar"))]), hydra.core.TypeScheme([], hydra.core.TypeList(hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit())))))), None, [])]),
      hydra.testing.TestGroup("List of lists of strings", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermList([
          hydra.core.TermList([hydra.core.TermLiteral(hydra.core.LiteralString("foo"))]),
          hydra.core.TermList([])]), hydra.core.TypeScheme([], hydra.core.TypeList(hydra.core.TypeList(hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))))))), None, [])]),
      hydra.testing.TestGroup("Empty list", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermList([]), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeList(hydra.core.TypeVariable(hydra.core.Name("t0")))))), None, [])]),
      hydra.testing.TestGroup("List containing an empty list", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermList([hydra.core.TermList([])]), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeList(hydra.core.TypeList(hydra.core.TypeVariable(hydra.core.Name("t0"))))))), None, [])]),
      hydra.testing.TestGroup("Lambda producing a polymorphic list", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermList([hydra.core.TermVariable(hydra.core.Name("x"))])))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeList(hydra.core.TypeVariable(hydra.core.Name("t0")))))))), None, [])]),
      hydra.testing.TestGroup("Lambda producing a list of integers", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermList([
          hydra.core.TermVariable(hydra.core.Name("x")),
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42)))])))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeList(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)))))))), None, [])]),
      hydra.testing.TestGroup("List with repeated variables", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermList([
          hydra.core.TermVariable(hydra.core.Name("x")),
          hydra.core.TermLiteral(hydra.core.LiteralString("foo")),
          hydra.core.TermVariable(hydra.core.Name("x"))])))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit())), hydra.core.TypeList(hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit())))))))), None, [])])], []),
    hydra.testing.TestGroup("Map terms", None, [], [
      hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermMap({
        hydra.core.TermLiteral(hydra.core.LiteralString("firstName")): hydra.core.TermLiteral(hydra.core.LiteralString("Arthur")),
        hydra.core.TermLiteral(hydra.core.LiteralString("lastName")): hydra.core.TermLiteral(hydra.core.LiteralString("Dent"))}), hydra.core.TypeScheme([], hydra.core.TypeMap(hydra.core.MapType(hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit())), hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))))))), None, [hydra.testing.Tag("disabledForAlgorithmWInference")]),
      hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermMap({}), hydra.core.TypeScheme([hydra.core.Name("t0"), hydra.core.Name("t1")], hydra.core.TypeMap(hydra.core.MapType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeVariable(hydra.core.Name("t1"))))))), None, [hydra.testing.Tag("disabledForAlgorithmWInference")])]),
    hydra.testing.TestGroup("Optional terms", None, [], [
      hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermOptional(hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42)))), hydra.core.TypeScheme([], hydra.core.TypeOptional(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)))))), None, [
        hydra.testing.Tag("disabledForAlgorithmWInference"),
        hydra.testing.Tag("disabledForAltInference")]),
      hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermOptional(None), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeOptional(hydra.core.TypeVariable(hydra.core.Name("t0")))))), None, [
        hydra.testing.Tag("disabledForAlgorithmWInference"),
        hydra.testing.Tag("disabledForAltInference")])]),
    hydra.testing.TestGroup("Product terms", None, [
      hydra.testing.TestGroup("Empty products", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermProduct([]), hydra.core.TypeScheme([], hydra.core.TypeProduct([])))), None, [])]),
      hydra.testing.TestGroup("Non-empty, monotyped products", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermProduct([
          hydra.core.TermLiteral(hydra.core.LiteralString("foo")),
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42)))]), hydra.core.TypeScheme([], hydra.core.TypeProduct([
          hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit())),
          hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))])))), None, []),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermProduct([
          hydra.core.TermLiteral(hydra.core.LiteralString("foo")),
          hydra.core.TermList([
            hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat32(42.0))),
            hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat32(137.0)))])]), hydra.core.TypeScheme([], hydra.core.TypeProduct([
          hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit())),
          hydra.core.TypeList(hydra.core.TypeLiteral(hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT32)))])))), None, []),
        hydra.testing.TestCaseWithMetadata("#3", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermProduct([
          hydra.core.TermLiteral(hydra.core.LiteralString("foo")),
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42))),
          hydra.core.TermList([
            hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat32(42.0))),
            hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat32(137.0)))])]), hydra.core.TypeScheme([], hydra.core.TypeProduct([
          hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit())),
          hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)),
          hydra.core.TypeList(hydra.core.TypeLiteral(hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT32)))])))), None, [])]),
      hydra.testing.TestGroup("Polytyped products", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermProduct([
          hydra.core.TermList([]),
          hydra.core.TermLiteral(hydra.core.LiteralString("foo"))]), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeProduct([
          hydra.core.TypeList(hydra.core.TypeVariable(hydra.core.Name("t0"))),
          hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))])))), None, []),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermProduct([
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42))),
          hydra.core.TermLiteral(hydra.core.LiteralString("foo")),
          hydra.core.TermList([])]), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeProduct([
          hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)),
          hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit())),
          hydra.core.TypeList(hydra.core.TypeVariable(hydra.core.Name("t0")))])))), None, [])]),
      hydra.testing.TestGroup("Pairs", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermProduct([
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42))),
          hydra.core.TermLiteral(hydra.core.LiteralString("foo"))]), hydra.core.TypeScheme([], hydra.core.TypeProduct([
          hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)),
          hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))])))), None, []),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermProduct([
          hydra.core.TermList([]),
          hydra.core.TermLiteral(hydra.core.LiteralString("foo"))]), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeProduct([
          hydra.core.TypeList(hydra.core.TypeVariable(hydra.core.Name("t0"))),
          hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))])))), None, []),
        hydra.testing.TestCaseWithMetadata("#3", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermProduct([hydra.core.TermList([]), hydra.core.TermList([])]), hydra.core.TypeScheme([hydra.core.Name("t0"), hydra.core.Name("t1")], hydra.core.TypeProduct([
          hydra.core.TypeList(hydra.core.TypeVariable(hydra.core.Name("t0"))),
          hydra.core.TypeList(hydra.core.TypeVariable(hydra.core.Name("t1")))])))), None, [])])], []),
    hydra.testing.TestGroup("Set terms", None, [], [
      hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermSet({hydra.core.TermLiteral(hydra.core.LiteralBoolean(True))}), hydra.core.TypeScheme([], hydra.core.TypeSet(hydra.core.TypeLiteral(hydra.core.LiteralTypeBoolean(hydra.core.Unit())))))), None, [
        hydra.testing.Tag("disabledForAlgorithmWInference"),
        hydra.testing.Tag("disabledForAltInference")]),
      hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermSet({hydra.core.TermSet({})}), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeSet(hydra.core.TypeSet(hydra.core.TypeVariable(hydra.core.Name("t0"))))))), None, [
        hydra.testing.Tag("disabledForAlgorithmWInference"),
        hydra.testing.Tag("disabledForAltInference")])]),
    hydra.testing.TestGroup("Sum terms", None, [
      hydra.testing.TestGroup("Singleton sum terms", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermSum(hydra.core.Sum(0, 1, hydra.core.TermLiteral(hydra.core.LiteralString("foo")))), hydra.core.TypeScheme([], hydra.core.TypeSum([
          hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))])))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")]),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermSum(hydra.core.Sum(0, 1, hydra.core.TermList([]))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeSum([hydra.core.TypeList(hydra.core.TypeVariable(hydra.core.Name("t0")))])))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")])]),
      hydra.testing.TestGroup("Non-singleton sum terms", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermSum(hydra.core.Sum(0, 2, hydra.core.TermLiteral(hydra.core.LiteralString("foo")))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeSum([
          hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit())),
          hydra.core.TypeVariable(hydra.core.Name("t0"))])))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")]),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermSum(hydra.core.Sum(1, 2, hydra.core.TermLiteral(hydra.core.LiteralString("foo")))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeSum([
          hydra.core.TypeVariable(hydra.core.Name("t0")),
          hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))])))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")])])], [])], []),
  hydra.testing.TestGroup("Algorithm W test cases", None, [
    hydra.testing.TestGroup("STLC to System F", None, [], [
      hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermVariable(hydra.core.Name("x"))))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeVariable(hydra.core.Name("t0"))))))), None, []),
      hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([
        hydra.core.LetBinding(hydra.core.Name("foo"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermVariable(hydra.core.Name("x"))))), None)], hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(32))))), hydra.core.TypeScheme([], hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))), None, []),
      hydra.testing.TestCaseWithMetadata("#3", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([
        hydra.core.LetBinding(hydra.core.Name("f"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermVariable(hydra.core.Name("x"))))), None)], hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("f")), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))))))), hydra.core.TypeScheme([], hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))), None, []),
      hydra.testing.TestCaseWithMetadata("#4", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([
        hydra.core.LetBinding(hydra.core.Name("f"), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermVariable(hydra.core.Name("x"))))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))))), None)], hydra.core.TermVariable(hydra.core.Name("f")))), hydra.core.TypeScheme([], hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))), None, []),
      hydra.testing.TestCaseWithMetadata("#5", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([
        hydra.core.LetBinding(hydra.core.Name("sng"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermList([hydra.core.TermVariable(hydra.core.Name("x"))])))), None)], hydra.core.TermVariable(hydra.core.Name("sng")))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeList(hydra.core.TypeVariable(hydra.core.Name("t0")))))))), None, []),
      hydra.testing.TestCaseWithMetadata("#6", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([
        hydra.core.LetBinding(hydra.core.Name("sng"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermList([hydra.core.TermVariable(hydra.core.Name("x"))])))), None)], hydra.core.TermProduct([
        hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("sng")), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))))),
        hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("sng")), hydra.core.TermLiteral(hydra.core.LiteralString("alice"))))]))), hydra.core.TypeScheme([], hydra.core.TypeProduct([
        hydra.core.TypeList(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))),
        hydra.core.TypeList(hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit())))])))), None, [hydra.testing.Tag("disabledForDefaultInference")]),
      hydra.testing.TestCaseWithMetadata("#7", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([
        hydra.core.LetBinding(hydra.core.Name("+"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("y"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.math.neg"))), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("+")), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.math.neg"))), hydra.core.TermVariable(hydra.core.Name("x")))))), hydra.core.TermVariable(hydra.core.Name("y")))))))))))), None)], hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("+")), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.math.neg"))), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.math.neg"))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))))))))), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.math.neg"))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))))))))), hydra.core.TypeScheme([], hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))), None, [
        hydra.testing.Tag("disabledForDefaultInference"),
        hydra.testing.Tag("disabledForAltInference")]),
      hydra.testing.TestCaseWithMetadata("#9", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([
        hydra.core.LetBinding(hydra.core.Name("f"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("y"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("f")), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))))), hydra.core.TermVariable(hydra.core.Name("x")))))))))), None)], hydra.core.TermVariable(hydra.core.Name("f")))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeVariable(hydra.core.Name("t0"))))))))), None, [
        hydra.testing.Tag("disabledForDefaultInference"),
        hydra.testing.Tag("disabledForAltInference")]),
      hydra.testing.TestCaseWithMetadata("#10", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([
        hydra.core.LetBinding(hydra.core.Name("f"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("y"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("f")), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))))), hydra.core.TermVariable(hydra.core.Name("x")))))))))), None),
        hydra.core.LetBinding(hydra.core.Name("g"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("xx"), None, hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("yy"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("g")), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))))), hydra.core.TermVariable(hydra.core.Name("xx")))))))))), None)], hydra.core.TermProduct([
        hydra.core.TermVariable(hydra.core.Name("f")),
        hydra.core.TermVariable(hydra.core.Name("g"))]))), hydra.core.TypeScheme([hydra.core.Name("t0"), hydra.core.Name("t1")], hydra.core.TypeProduct([
        hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeVariable(hydra.core.Name("t0")))))),
        hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeVariable(hydra.core.Name("t1"))))))])))), None, [
        hydra.testing.Tag("disabledForDefaultInference"),
        hydra.testing.Tag("disabledForAltInference")]),
      hydra.testing.TestCaseWithMetadata("#11", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([
        hydra.core.LetBinding(hydra.core.Name("f"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("y"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("g")), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))))), hydra.core.TermVariable(hydra.core.Name("x")))))))))), None),
        hydra.core.LetBinding(hydra.core.Name("g"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("u"), None, hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("v"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("f")), hydra.core.TermVariable(hydra.core.Name("v")))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))))))))))), None)], hydra.core.TermProduct([
        hydra.core.TermVariable(hydra.core.Name("f")),
        hydra.core.TermVariable(hydra.core.Name("g"))]))), hydra.core.TypeScheme([
        hydra.core.Name("t0"),
        hydra.core.Name("t1"),
        hydra.core.Name("t2"),
        hydra.core.Name("t3")], hydra.core.TypeProduct([
        hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeVariable(hydra.core.Name("t1")))))),
        hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t2")), hydra.core.TypeVariable(hydra.core.Name("t3"))))))])))), None, [
        hydra.testing.Tag("disabledForDefaultInference"),
        hydra.testing.Tag("disabledForAltInference")]),
      hydra.testing.TestCaseWithMetadata("#12", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([
        hydra.core.LetBinding(hydra.core.Name("f"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("y"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("g")), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))))))))))), None),
        hydra.core.LetBinding(hydra.core.Name("g"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("u"), None, hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("v"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("f")), hydra.core.TermVariable(hydra.core.Name("v")))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))))))))))), None)], hydra.core.TermProduct([
        hydra.core.TermVariable(hydra.core.Name("f")),
        hydra.core.TermVariable(hydra.core.Name("g"))]))), hydra.core.TypeScheme([hydra.core.Name("t0"), hydra.core.Name("t1")], hydra.core.TypeProduct([
        hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeVariable(hydra.core.Name("t0")))))),
        hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeVariable(hydra.core.Name("t1"))))))])))), None, [
        hydra.testing.Tag("disabledForDefaultInference"),
        hydra.testing.Tag("disabledForAltInference")]),
      hydra.testing.TestCaseWithMetadata("#13", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([
        hydra.core.LetBinding(hydra.core.Name("f"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("y"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("g")), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))))), hydra.core.TermVariable(hydra.core.Name("x")))))))))), None),
        hydra.core.LetBinding(hydra.core.Name("g"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("u"), None, hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("v"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("f")), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))))))))))), None)], hydra.core.TermProduct([
        hydra.core.TermVariable(hydra.core.Name("f")),
        hydra.core.TermVariable(hydra.core.Name("g"))]))), hydra.core.TypeScheme([hydra.core.Name("t0"), hydra.core.Name("t1")], hydra.core.TypeProduct([
        hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeVariable(hydra.core.Name("t0")))))),
        hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeVariable(hydra.core.Name("t1"))))))])))), None, [
        hydra.testing.Tag("disabledForDefaultInference"),
        hydra.testing.Tag("disabledForAltInference")])])], []),
  hydra.testing.TestGroup("Fundamentals", None, [
    hydra.testing.TestGroup("Lambdas", None, [], [
      hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermVariable(hydra.core.Name("x"))))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeVariable(hydra.core.Name("t0"))))))), None, []),
      hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt16(137)))))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT16))))))), None, [])]),
    hydra.testing.TestGroup("Let", None, [
      hydra.testing.TestGroup("Simple", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([
          hydra.core.LetBinding(hydra.core.Name("x"), hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat32(42.0))), None)], hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("y"), None, hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("z"), None, hydra.core.TermVariable(hydra.core.Name("x")))))))))), hydra.core.TypeScheme([hydra.core.Name("t0"), hydra.core.Name("t1")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t1")), hydra.core.TypeLiteral(hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT32))))))))), None, [])]),
      hydra.testing.TestGroup("Empty let", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([], hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42))))), hydra.core.TypeScheme([], hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))), None, [hydra.testing.Tag("disabledForAltInference")])]),
      hydra.testing.TestGroup("Trivial let", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([
          hydra.core.LetBinding(hydra.core.Name("foo"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42))), None)], hydra.core.TermVariable(hydra.core.Name("foo")))), hydra.core.TypeScheme([], hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))), None, [])]),
      hydra.testing.TestGroup("Multiple references to a let-bound term", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([
          hydra.core.LetBinding(hydra.core.Name("foo"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42))), None),
          hydra.core.LetBinding(hydra.core.Name("bar"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(137))), None)], hydra.core.TermList([
          hydra.core.TermVariable(hydra.core.Name("foo")),
          hydra.core.TermVariable(hydra.core.Name("bar")),
          hydra.core.TermVariable(hydra.core.Name("foo"))]))), hydra.core.TypeScheme([], hydra.core.TypeList(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)))))), None, [hydra.testing.Tag("disabledForAltInference")])]),
      hydra.testing.TestGroup("Let-polymorphism", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([
          hydra.core.LetBinding(hydra.core.Name("id"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermVariable(hydra.core.Name("x"))))), None)], hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("id")), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("id")), hydra.core.TermVariable(hydra.core.Name("x"))))))))))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeVariable(hydra.core.Name("t0"))))))), None, []),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([
          hydra.core.LetBinding(hydra.core.Name("id"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermVariable(hydra.core.Name("x"))))), None)], hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("id")), hydra.core.TermList([
          hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("id")), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42)))))]))))), hydra.core.TypeScheme([], hydra.core.TypeList(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)))))), None, [hydra.testing.Tag("disabled")]),
        hydra.testing.TestCaseWithMetadata("#3", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([
          hydra.core.LetBinding(hydra.core.Name("id"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermVariable(hydra.core.Name("x"))))), None)], hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("id")), hydra.core.TermList([
          hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("id")), hydra.core.TermVariable(hydra.core.Name("x"))))])))))))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeList(hydra.core.TypeVariable(hydra.core.Name("t0")))))))), None, [hydra.testing.Tag("disabled")]),
        hydra.testing.TestCaseWithMetadata("#4", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([
          hydra.core.LetBinding(hydra.core.Name("id"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermVariable(hydra.core.Name("x"))))), None)], hydra.core.TermProduct([
          hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("id")), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42))))),
          hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("id")), hydra.core.TermLiteral(hydra.core.LiteralString("foo"))))]))), hydra.core.TypeScheme([], hydra.core.TypeProduct([
          hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)),
          hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))])))), None, [hydra.testing.Tag("disabled")]),
        hydra.testing.TestCaseWithMetadata("#5", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([
          hydra.core.LetBinding(hydra.core.Name("list"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermList([hydra.core.TermVariable(hydra.core.Name("x"))])))), None)], hydra.core.TermProduct([
          hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("list")), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42))))),
          hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("list")), hydra.core.TermLiteral(hydra.core.LiteralString("foo"))))]))), hydra.core.TypeScheme([], hydra.core.TypeProduct([
          hydra.core.TypeList(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))),
          hydra.core.TypeList(hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit())))])))), None, [hydra.testing.Tag("disabled")]),
        hydra.testing.TestCaseWithMetadata("#6", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([
          hydra.core.LetBinding(hydra.core.Name("singleton"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermList([hydra.core.TermVariable(hydra.core.Name("x"))])))), None),
          hydra.core.LetBinding(hydra.core.Name("f"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("y"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.cons"))), hydra.core.TermProduct([
            hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("singleton")), hydra.core.TermVariable(hydra.core.Name("x")))),
            hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("singleton")), hydra.core.TermVariable(hydra.core.Name("y"))))]))), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("g")), hydra.core.TermVariable(hydra.core.Name("x")))), hydra.core.TermVariable(hydra.core.Name("y")))))))))))), None),
          hydra.core.LetBinding(hydra.core.Name("g"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("y"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("f")), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42))))), hydra.core.TermVariable(hydra.core.Name("y")))))))))), None)], hydra.core.TermVariable(hydra.core.Name("f")))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeList(hydra.core.TypeProduct([
          hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)),
          hydra.core.TypeVariable(hydra.core.Name("t0"))]))))), None, [hydra.testing.Tag("disabled")])]),
      hydra.testing.TestGroup("Recursive and mutually recursive let (@wisnesky's test cases)", None, [], [])], []),
    hydra.testing.TestGroup("Literals", None, [], [
      hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42))), hydra.core.TypeScheme([], hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))), None, []),
      hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLiteral(hydra.core.LiteralString("foo")), hydra.core.TypeScheme([], hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))))), None, []),
      hydra.testing.TestCaseWithMetadata("#3", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLiteral(hydra.core.LiteralBoolean(False)), hydra.core.TypeScheme([], hydra.core.TypeLiteral(hydra.core.LiteralTypeBoolean(hydra.core.Unit()))))), None, []),
      hydra.testing.TestCaseWithMetadata("#4", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat64(42.0))), hydra.core.TypeScheme([], hydra.core.TypeLiteral(hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT64))))), None, [])]),
    hydra.testing.TestGroup("Pathological terms", None, [
      hydra.testing.TestGroup("Infinite lists", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([
          hydra.core.LetBinding(hydra.core.Name("self"), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.cons"))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42))))), hydra.core.TermVariable(hydra.core.Name("self")))), None)], hydra.core.TermVariable(hydra.core.Name("self")))), hydra.core.TypeScheme([], hydra.core.TypeList(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)))))), None, [hydra.testing.Tag("disabled")]),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermLet(hydra.core.Let([
          hydra.core.LetBinding(hydra.core.Name("self"), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.cons"))), hydra.core.TermVariable(hydra.core.Name("x")))), hydra.core.TermVariable(hydra.core.Name("self")))), None)], hydra.core.TermVariable(hydra.core.Name("self"))))))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeList(hydra.core.TypeVariable(hydra.core.Name("t0")))))))), None, [hydra.testing.Tag("disabled")]),
        hydra.testing.TestCaseWithMetadata("#3", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([
          hydra.core.LetBinding(hydra.core.Name("self"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("e"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.cons"))), hydra.core.TermVariable(hydra.core.Name("e")))), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("self")), hydra.core.TermVariable(hydra.core.Name("e"))))))))), None)], hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("self")), hydra.core.TermVariable(hydra.core.Name("x"))))))))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeList(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)))))))), None, [hydra.testing.Tag("disabled")]),
        hydra.testing.TestCaseWithMetadata("#4", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([
          hydra.core.LetBinding(hydra.core.Name("build"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.cons"))), hydra.core.TermVariable(hydra.core.Name("x")))), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("build")), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.math.add"))), hydra.core.TermVariable(hydra.core.Name("x")))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(1)))))))))))), None)], hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("build")), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))))))), hydra.core.TypeScheme([], hydra.core.TypeList(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)))))), None, [hydra.testing.Tag("disabled")])])], []),
    hydra.testing.TestGroup("Polymorphism", None, [
      hydra.testing.TestGroup("Simple lists and optionals", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermList([]), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeList(hydra.core.TypeVariable(hydra.core.Name("t0")))))), None, []),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermOptional(None), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeOptional(hydra.core.TypeVariable(hydra.core.Name("t0")))))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")]),
        hydra.testing.TestCaseWithMetadata("#3", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermOptional(hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42)))), hydra.core.TypeScheme([], hydra.core.TypeOptional(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)))))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")])]),
      hydra.testing.TestGroup("Lambdas, lists, and products", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermVariable(hydra.core.Name("x"))))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeVariable(hydra.core.Name("t0"))))))), None, []),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermProduct([
          hydra.core.TermVariable(hydra.core.Name("x")),
          hydra.core.TermVariable(hydra.core.Name("x"))])))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeProduct([
          hydra.core.TypeVariable(hydra.core.Name("t0")),
          hydra.core.TypeVariable(hydra.core.Name("t0"))])))))), None, []),
        hydra.testing.TestCaseWithMetadata("#3", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermList([hydra.core.TermVariable(hydra.core.Name("x"))])))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeList(hydra.core.TypeVariable(hydra.core.Name("t0")))))))), None, []),
        hydra.testing.TestCaseWithMetadata("#4", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermList([
          hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermVariable(hydra.core.Name("x"))))),
          hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("y"), None, hydra.core.TermVariable(hydra.core.Name("y")))))]), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeList(hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeVariable(hydra.core.Name("t0")))))))), None, []),
        hydra.testing.TestCaseWithMetadata("#5", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermList([
          hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("y"), None, hydra.core.TermProduct([
            hydra.core.TermVariable(hydra.core.Name("y")),
            hydra.core.TermVariable(hydra.core.Name("x"))])))))))]), hydra.core.TypeScheme([hydra.core.Name("t0"), hydra.core.Name("t1")], hydra.core.TypeList(hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t1")), hydra.core.TypeProduct([
          hydra.core.TypeVariable(hydra.core.Name("t1")),
          hydra.core.TypeVariable(hydra.core.Name("t0"))]))))))))), None, [])]),
      hydra.testing.TestGroup("Lambdas and application", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermVariable(hydra.core.Name("x"))))), hydra.core.TermLiteral(hydra.core.LiteralString("foo")))), hydra.core.TypeScheme([], hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))))), None, [])]),
      hydra.testing.TestGroup("Primitives and application", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.concat"))), hydra.core.TermList([
          hydra.core.TermList([
            hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42)))]),
          hydra.core.TermList([])]))), hydra.core.TypeScheme([], hydra.core.TypeList(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)))))), None, [hydra.testing.Tag("disabledForAltInference")])]),
      hydra.testing.TestGroup("Lambdas and primitives", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.math.add"))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))))))), None, [hydra.testing.Tag("disabledForAltInference")]),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.math.add"))), hydra.core.TermVariable(hydra.core.Name("x"))))))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))))))), None, [hydra.testing.Tag("disabledForAltInference")]),
        hydra.testing.TestCaseWithMetadata("#3", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.math.add"))), hydra.core.TermVariable(hydra.core.Name("x")))), hydra.core.TermVariable(hydra.core.Name("x"))))))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))))), None, [hydra.testing.Tag("disabledForAltInference")])]),
      hydra.testing.TestGroup("Mixed expressions with lambdas, constants, and primitive functions", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.math.sub"))), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.math.add"))), hydra.core.TermVariable(hydra.core.Name("x")))), hydra.core.TermVariable(hydra.core.Name("x")))))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(1)))))))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))))), None, [hydra.testing.Tag("disabledForAltInference")])])], []),
    hydra.testing.TestGroup("Primitives", None, [
      hydra.testing.TestGroup("Monomorphic primitive functions", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.strings.length"))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit())), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))))), None, [hydra.testing.Tag("disabledForAltInference")]),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.math.sub"))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))))))), None, [hydra.testing.Tag("disabledForAltInference")])]),
      hydra.testing.TestGroup("Polymorphic primitive functions", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("el"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.length"))), hydra.core.TermList([hydra.core.TermVariable(hydra.core.Name("el"))])))))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))))), None, [hydra.testing.Tag("disabledForAltInference")]),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("el"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.length"))), hydra.core.TermList([
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42))),
          hydra.core.TermVariable(hydra.core.Name("el"))])))))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))))), None, [hydra.testing.Tag("disabledForAltInference")]),
        hydra.testing.TestCaseWithMetadata("#3", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.concat"))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeList(hydra.core.TypeList(hydra.core.TypeVariable(hydra.core.Name("t0")))), hydra.core.TypeList(hydra.core.TypeVariable(hydra.core.Name("t0")))))))), None, [hydra.testing.Tag("disabledForAltInference")]),
        hydra.testing.TestCaseWithMetadata("#4", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("lists"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.concat"))), hydra.core.TermVariable(hydra.core.Name("lists"))))))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeList(hydra.core.TypeList(hydra.core.TypeVariable(hydra.core.Name("t0")))), hydra.core.TypeList(hydra.core.TypeVariable(hydra.core.Name("t0")))))))), None, [hydra.testing.Tag("disabledForAltInference")]),
        hydra.testing.TestCaseWithMetadata("#5", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("lists"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.length"))), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.concat"))), hydra.core.TermVariable(hydra.core.Name("lists"))))))))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeList(hydra.core.TypeList(hydra.core.TypeVariable(hydra.core.Name("t0")))), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))))), None, [hydra.testing.Tag("disabledForAltInference")]),
        hydra.testing.TestCaseWithMetadata("#6", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("list"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.length"))), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.concat"))), hydra.core.TermList([
          hydra.core.TermVariable(hydra.core.Name("list")),
          hydra.core.TermList([])])))))))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeList(hydra.core.TypeVariable(hydra.core.Name("t0"))), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))))), None, [hydra.testing.Tag("disabledForAltInference")]),
        hydra.testing.TestCaseWithMetadata("#7", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("list"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.math.add"))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(1))))), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.length"))), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.concat"))), hydra.core.TermList([
          hydra.core.TermVariable(hydra.core.Name("list")),
          hydra.core.TermList([])])))))))))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeList(hydra.core.TypeVariable(hydra.core.Name("t0"))), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))))), None, [hydra.testing.Tag("disabledForAltInference")]),
        hydra.testing.TestCaseWithMetadata("#8", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("lists"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.length"))), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.concat"))), hydra.core.TermVariable(hydra.core.Name("lists"))))))))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeList(hydra.core.TypeList(hydra.core.TypeVariable(hydra.core.Name("t0")))), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))))), None, [hydra.testing.Tag("disabledForAltInference")])])], [])], []),
  hydra.testing.TestGroup("Nominal terms", None, [
    hydra.testing.TestGroup("Case statements", None, [], [
      hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionElimination(hydra.core.EliminationUnion(hydra.core.CaseStatement("hydra.test.testGraph.testTypeSimpleNumberName", None, [
        hydra.core.Field(hydra.core.Name("int"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermVariable(hydra.core.Name("x")))))),
        hydra.core.Field(hydra.core.Name("float"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42)))))))])))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))))), None, [hydra.testing.Tag("disabled")]),
      hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionElimination(hydra.core.EliminationUnion(hydra.core.CaseStatement("hydra.test.testGraph.testTypeUnionMonomorphicName", None, [
        hydra.core.Field(hydra.core.Name("bool"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("_"), None, hydra.core.TermLiteral(hydra.core.LiteralBoolean(True)))))),
        hydra.core.Field(hydra.core.Name("string"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("_"), None, hydra.core.TermLiteral(hydra.core.LiteralBoolean(False)))))),
        hydra.core.Field(hydra.core.Name("unit"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("_"), None, hydra.core.TermLiteral(hydra.core.LiteralBoolean(False))))))])))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable("hydra.test.testGraph.testTypeUnionMonomorphicName"), hydra.core.TypeLiteral(hydra.core.LiteralTypeBoolean(hydra.core.Unit()))))))), None, [hydra.testing.Tag("disabled")])]),
    hydra.testing.TestGroup("Projections", None, [
      hydra.testing.TestGroup("Record eliminations", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionElimination(hydra.core.EliminationRecord(hydra.core.Projection("hydra.test.testGraph.testTypePersonName", hydra.core.Name("firstName"))))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable("hydra.test.testGraph.testTypePersonName"), hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))))))), None, [hydra.testing.Tag("disabled")])])], []),
    hydra.testing.TestGroup("Records", None, [
      hydra.testing.TestGroup("Simple records", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermRecord(hydra.core.Record("hydra.test.testGraph.testTypeLatLonName", [
          hydra.core.Field(hydra.core.Name("lat"), hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat32(37.774898529052734)))),
          hydra.core.Field(hydra.core.Name("lon"), hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat32(-122.41940307617188))))])), hydra.core.TypeScheme([], hydra.core.TypeVariable("hydra.test.testGraph.testTypeLatLonName")))), None, [hydra.testing.Tag("disabled")]),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermRecord(hydra.core.Record("hydra.test.testGraph.testTypeLatLonPolyName", [
          hydra.core.Field(hydra.core.Name("lat"), hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat32(37.774898529052734)))),
          hydra.core.Field(hydra.core.Name("lon"), hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat32(-122.41940307617188))))])), hydra.core.TypeScheme([], hydra.core.TypeApplication(hydra.core.ApplicationType(hydra.core.TypeVariable("hydra.test.testGraph.testTypeLatLonPolyName"), hydra.core.TypeLiteral(hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT32))))))), None, [hydra.testing.Tag("disabled")]),
        hydra.testing.TestCaseWithMetadata("#3", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("lon"), None, hydra.core.TermRecord(hydra.core.Record("hydra.test.testGraph.testTypeLatLonPolyName", [
          hydra.core.Field(hydra.core.Name("lat"), hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat32(37.774898529052734)))),
          hydra.core.Field(hydra.core.Name("lon"), hydra.core.TermVariable(hydra.core.Name("lon")))]))))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT32)), hydra.core.TypeApplication(hydra.core.ApplicationType(hydra.core.TypeVariable("hydra.test.testGraph.testTypeLatLonPolyName"), hydra.core.TypeLiteral(hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT32))))))))), None, [hydra.testing.Tag("disabled")]),
        hydra.testing.TestCaseWithMetadata("#4", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("latlon"), None, hydra.core.TermRecord(hydra.core.Record("hydra.test.testGraph.testTypeLatLonPolyName", [
          hydra.core.Field(hydra.core.Name("lat"), hydra.core.TermVariable(hydra.core.Name("latlon"))),
          hydra.core.Field(hydra.core.Name("lon"), hydra.core.TermVariable(hydra.core.Name("latlon")))]))))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeApplication(hydra.core.ApplicationType(hydra.core.TypeVariable("hydra.test.testGraph.testTypeLatLonPolyName"), hydra.core.TypeVariable(hydra.core.Name("t0"))))))))), None, [hydra.testing.Tag("disabled")]),
        hydra.testing.TestCaseWithMetadata("#5", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase("hydra.test.testGraph.testDataArthur", hydra.core.TypeScheme([], hydra.core.TypeVariable("hydra.test.testGraph.testTypePersonName")))), None, [hydra.testing.Tag("disabled")])]),
      hydra.testing.TestGroup("Record instances of simply recursive record types", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermRecord(hydra.core.Record("hydra.test.testGraph.testTypeIntListName", [
          hydra.core.Field(hydra.core.Name("head"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42)))),
          hydra.core.Field(hydra.core.Name("tail"), hydra.core.TermOptional(hydra.core.TermRecord(hydra.core.Record("hydra.test.testGraph.testTypeIntListName", [
            hydra.core.Field(hydra.core.Name("head"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(43)))),
            hydra.core.Field(hydra.core.Name("tail"), hydra.core.TermOptional(None))]))))])), hydra.core.TypeScheme([], hydra.core.TypeVariable("hydra.test.testGraph.testTypeIntListName")))), None, [hydra.testing.Tag("disabled")]),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermRecord(hydra.core.Record("hydra.test.testGraph.testTypeIntListName", [
          hydra.core.Field(hydra.core.Name("head"), hydra.core.TermVariable(hydra.core.Name("x"))),
          hydra.core.Field(hydra.core.Name("tail"), hydra.core.TermOptional(hydra.core.TermRecord(hydra.core.Record("hydra.test.testGraph.testTypeIntListName", [
            hydra.core.Field(hydra.core.Name("head"), hydra.core.TermVariable(hydra.core.Name("x"))),
            hydra.core.Field(hydra.core.Name("tail"), hydra.core.TermOptional(None))]))))]))))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42))))), hydra.core.TypeScheme([], hydra.core.TypeVariable("hydra.test.testGraph.testTypeIntListName")))), None, [hydra.testing.Tag("disabled")]),
        hydra.testing.TestCaseWithMetadata("#3", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermRecord(hydra.core.Record("hydra.test.testGraph.testTypeListName", [
          hydra.core.Field(hydra.core.Name("head"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42)))),
          hydra.core.Field(hydra.core.Name("tail"), hydra.core.TermOptional(hydra.core.TermRecord(hydra.core.Record("hydra.test.testGraph.testTypeListName", [
            hydra.core.Field(hydra.core.Name("head"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(43)))),
            hydra.core.Field(hydra.core.Name("tail"), hydra.core.TermOptional(None))]))))])), hydra.core.TypeScheme([], hydra.core.TypeApplication(hydra.core.ApplicationType(hydra.core.TypeVariable("hydra.test.testGraph.testTypeListName"), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))))), None, [hydra.testing.Tag("disabled")]),
        hydra.testing.TestCaseWithMetadata("#4", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermRecord(hydra.core.Record("hydra.test.testGraph.testTypeListName", [
          hydra.core.Field(hydra.core.Name("head"), hydra.core.TermVariable(hydra.core.Name("x"))),
          hydra.core.Field(hydra.core.Name("tail"), hydra.core.TermOptional(hydra.core.TermRecord(hydra.core.Record("hydra.test.testGraph.testTypeListName", [
            hydra.core.Field(hydra.core.Name("head"), hydra.core.TermVariable(hydra.core.Name("x"))),
            hydra.core.Field(hydra.core.Name("tail"), hydra.core.TermOptional(None))]))))]))))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42))))), hydra.core.TypeScheme([], hydra.core.TypeApplication(hydra.core.ApplicationType(hydra.core.TypeVariable("hydra.test.testGraph.testTypeListName"), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))))), None, [hydra.testing.Tag("disabled")]),
        hydra.testing.TestCaseWithMetadata("#5", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermRecord(hydra.core.Record("hydra.test.testGraph.testTypeListName", [
          hydra.core.Field(hydra.core.Name("head"), hydra.core.TermVariable(hydra.core.Name("x"))),
          hydra.core.Field(hydra.core.Name("tail"), hydra.core.TermOptional(hydra.core.TermRecord(hydra.core.Record("hydra.test.testGraph.testTypeListName", [
            hydra.core.Field(hydra.core.Name("head"), hydra.core.TermVariable(hydra.core.Name("x"))),
            hydra.core.Field(hydra.core.Name("tail"), hydra.core.TermOptional(None))]))))]))))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeApplication(hydra.core.ApplicationType(hydra.core.TypeVariable("hydra.test.testGraph.testTypeListName"), hydra.core.TypeVariable(hydra.core.Name("t0"))))))))), None, [hydra.testing.Tag("disabled")])]),
      hydra.testing.TestGroup("Record instances of mutually recursive record types", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermRecord(hydra.core.Record("hydra.test.testGraph.testTypeBuddyListAName", [
          hydra.core.Field(hydra.core.Name("head"), hydra.core.TermVariable(hydra.core.Name("x"))),
          hydra.core.Field(hydra.core.Name("tail"), hydra.core.TermOptional(hydra.core.TermRecord(hydra.core.Record("hydra.test.testGraph.testTypeBuddyListBName", [
            hydra.core.Field(hydra.core.Name("head"), hydra.core.TermVariable(hydra.core.Name("x"))),
            hydra.core.Field(hydra.core.Name("tail"), hydra.core.TermOptional(None))]))))]))))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42))))), hydra.core.TypeScheme([], hydra.core.TypeApplication(hydra.core.ApplicationType(hydra.core.TypeVariable("hydra.test.testGraph.testTypeBuddyListAName"), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))))), None, [hydra.testing.Tag("disabled")]),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermRecord(hydra.core.Record("hydra.test.testGraph.testTypeBuddyListAName", [
          hydra.core.Field(hydra.core.Name("head"), hydra.core.TermVariable(hydra.core.Name("x"))),
          hydra.core.Field(hydra.core.Name("tail"), hydra.core.TermOptional(hydra.core.TermRecord(hydra.core.Record("hydra.test.testGraph.testTypeBuddyListBName", [
            hydra.core.Field(hydra.core.Name("head"), hydra.core.TermVariable(hydra.core.Name("x"))),
            hydra.core.Field(hydra.core.Name("tail"), hydra.core.TermOptional(None))]))))]))))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeApplication(hydra.core.ApplicationType(hydra.core.TypeVariable("hydra.test.testGraph.testTypeBuddyListAName"), hydra.core.TypeVariable(hydra.core.Name("t0"))))))))), None, [hydra.testing.Tag("disabled")])])], []),
    hydra.testing.TestGroup("Variant terms", None, [
      hydra.testing.TestGroup("Variants", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermUnion(hydra.core.Injection("hydra.test.testGraph.testTypeTimestampName", hydra.core.Field(hydra.core.Name("unixTimeMillis"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueUint64(1638200308368)))))), hydra.core.TypeScheme([], hydra.core.TypeVariable("hydra.test.testGraph.testTypeTimestampName")))), None, [hydra.testing.Tag("disabled")]),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermUnion(hydra.core.Injection("hydra.test.testGraph.testTypeUnionMonomorphicName", hydra.core.Field(hydra.core.Name("string"), hydra.core.TermLiteral(hydra.core.LiteralString("bar"))))), hydra.core.TypeScheme([], hydra.core.TypeVariable("hydra.test.testGraph.testTypeUnionMonomorphicName")))), None, [hydra.testing.Tag("disabled")])]),
      hydra.testing.TestGroup("Polymorphic and recursive variants", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermUnion(hydra.core.Injection("hydra.test.testGraph.testTypeUnionPolymorphicRecursiveName", hydra.core.Field(hydra.core.Name("bool"), hydra.core.TermLiteral(hydra.core.LiteralBoolean(True))))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeApplication(hydra.core.ApplicationType(hydra.core.TypeVariable("hydra.test.testGraph.testTypeUnionPolymorphicRecursiveName"), hydra.core.TypeVariable(hydra.core.Name("t0"))))))), None, [hydra.testing.Tag("disabled")]),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermUnion(hydra.core.Injection("hydra.test.testGraph.testTypeUnionPolymorphicRecursiveName", hydra.core.Field(hydra.core.Name("value"), hydra.core.TermLiteral(hydra.core.LiteralString("foo"))))), hydra.core.TypeScheme([], hydra.core.TypeApplication(hydra.core.ApplicationType(hydra.core.TypeVariable("hydra.test.testGraph.testTypeUnionPolymorphicRecursiveName"), hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))))))), None, [hydra.testing.Tag("disabled")]),
        hydra.testing.TestCaseWithMetadata("#3", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([
          hydra.core.LetBinding(hydra.core.Name("other"), hydra.core.TermUnion(hydra.core.Injection("hydra.test.testGraph.testTypeUnionPolymorphicRecursiveName", hydra.core.Field(hydra.core.Name("value"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42)))))), None)], hydra.core.TermUnion(hydra.core.Injection("hydra.test.testGraph.testTypeUnionPolymorphicRecursiveName", hydra.core.Field(hydra.core.Name("other"), hydra.core.TermVariable(hydra.core.Name("other"))))))), hydra.core.TypeScheme([], hydra.core.TypeApplication(hydra.core.ApplicationType(hydra.core.TypeVariable("hydra.test.testGraph.testTypeUnionPolymorphicRecursiveName"), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))))), None, [hydra.testing.Tag("disabled")])])], []),
    hydra.testing.TestGroup("Wrapper introductions and eliminations", None, [
      hydra.testing.TestGroup("Wrapper eliminations", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermWrap(hydra.core.WrappedTerm("hydra.test.testGraph.testTypeStringAliasName", hydra.core.TermLiteral(hydra.core.LiteralString("foo")))), hydra.core.TypeScheme([], hydra.core.TypeVariable("hydra.test.testGraph.testTypeStringAliasName")))), None, [hydra.testing.Tag("disabled")]),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("v"), None, hydra.core.TermWrap(hydra.core.WrappedTerm("hydra.test.testGraph.testTypeStringAliasName", hydra.core.TermVariable(hydra.core.Name("v"))))))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit())), hydra.core.TypeVariable("hydra.test.testGraph.testTypeStringAliasName")))))), None, [hydra.testing.Tag("disabled")])]),
      hydra.testing.TestGroup("Wrapper introductions", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionElimination(hydra.core.EliminationWrap("hydra.test.testGraph.testTypeStringAliasName"))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable("hydra.test.testGraph.testTypeStringAliasName"), hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))))))), None, [hydra.testing.Tag("disabled")]),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionElimination(hydra.core.EliminationWrap("hydra.test.testGraph.testTypeStringAliasName"))), hydra.core.TermWrap(hydra.core.WrappedTerm("hydra.test.testGraph.testTypeStringAliasName", hydra.core.TermLiteral(hydra.core.LiteralString("foo")))))), hydra.core.TypeScheme([], hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")])])], [])], []),
  hydra.testing.TestGroup("Simple terms", None, [
    hydra.testing.TestGroup("Application terms", None, [], [
      hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermVariable(hydra.core.Name("x"))))), hydra.core.TermLiteral(hydra.core.LiteralString("foo")))), hydra.core.TypeScheme([], hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))))), None, []),
      hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.math.sub"))), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.math.add"))), hydra.core.TermVariable(hydra.core.Name("x")))), hydra.core.TermVariable(hydra.core.Name("x")))))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(1)))))))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))))), None, [hydra.testing.Tag("disabledForAltInference")])]),
    hydra.testing.TestGroup("Function terms", None, [
      hydra.testing.TestGroup("Lambdas", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermVariable(hydra.core.Name("x"))))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeVariable(hydra.core.Name("t0"))))))), None, []),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt16(137)))))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT16))))))), None, [])]),
      hydra.testing.TestGroup("List eliminations", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionElimination(hydra.core.EliminationList(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.math.add")))))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeList(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))))))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")]),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionElimination(hydra.core.EliminationList(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.math.add")))))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeList(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")]),
        hydra.testing.TestCaseWithMetadata("#3", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionElimination(hydra.core.EliminationList(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.math.add")))))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))))), hydra.core.TermList([
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(1))),
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(2))),
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(3))),
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(4))),
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(5)))]))), hydra.core.TypeScheme([], hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")])]),
      hydra.testing.TestGroup("Projections", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionElimination(hydra.core.EliminationRecord(hydra.core.Projection("hydra.test.testGraph.testTypePersonName", hydra.core.Name("firstName"))))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType("hydra.test.testGraph.testTypePerson", hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))))))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")])]),
      hydra.testing.TestGroup("Case statements", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionElimination(hydra.core.EliminationUnion(hydra.core.CaseStatement("hydra.test.testGraph.testTypeFoobarValueName", None, [
          hydra.core.Field(hydra.core.Name("bool"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("_"), None, hydra.core.TermLiteral(hydra.core.LiteralBoolean(True)))))),
          hydra.core.Field(hydra.core.Name("string"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("_"), None, hydra.core.TermLiteral(hydra.core.LiteralBoolean(False)))))),
          hydra.core.Field(hydra.core.Name("unit"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("_"), None, hydra.core.TermLiteral(hydra.core.LiteralBoolean(False))))))])))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType("hydra.test.testGraph.testTypeFoobarValue", hydra.core.TypeLiteral(hydra.core.LiteralTypeBoolean(hydra.core.Unit()))))))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")])])], []),
    hydra.testing.TestGroup("Individual terms", None, [
      hydra.testing.TestGroup("Literal values", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42))), hydra.core.TypeScheme([], hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))), None, []),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLiteral(hydra.core.LiteralString("foo")), hydra.core.TypeScheme([], hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))))), None, []),
        hydra.testing.TestCaseWithMetadata("#3", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLiteral(hydra.core.LiteralBoolean(False)), hydra.core.TypeScheme([], hydra.core.TypeLiteral(hydra.core.LiteralTypeBoolean(hydra.core.Unit()))))), None, []),
        hydra.testing.TestCaseWithMetadata("#4", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat64(42.0))), hydra.core.TypeScheme([], hydra.core.TypeLiteral(hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT64))))), None, [])]),
      hydra.testing.TestGroup("Let terms", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([
          hydra.core.LetBinding(hydra.core.Name("x"), hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat32(42.0))), None)], hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("y"), None, hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("z"), None, hydra.core.TermVariable(hydra.core.Name("x")))))))))), hydra.core.TypeScheme([hydra.core.Name("t0"), hydra.core.Name("t1")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t1")), hydra.core.TypeLiteral(hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT32))))))))), None, []),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([
          hydra.core.LetBinding(hydra.core.Name("square"), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("z"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.math.mul"))), hydra.core.TermVariable(hydra.core.Name("z")))), hydra.core.TermVariable(hydra.core.Name("z"))))))), None)], hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("f"), None, hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("y"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.logic.ifElse"))), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("f")), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("square")), hydra.core.TermVariable(hydra.core.Name("x")))))), hydra.core.TermVariable(hydra.core.Name("y")))))), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("f")), hydra.core.TermVariable(hydra.core.Name("x")))), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("f")), hydra.core.TermVariable(hydra.core.Name("x")))), hydra.core.TermVariable(hydra.core.Name("y")))))))), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermVariable(hydra.core.Name("f")), hydra.core.TermVariable(hydra.core.Name("x")))), hydra.core.TermVariable(hydra.core.Name("y"))))))))))))))))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeBoolean(hydra.core.Unit())), hydra.core.TypeLiteral(hydra.core.LiteralTypeBoolean(hydra.core.Unit())))))), hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeBoolean(hydra.core.Unit())), hydra.core.TypeLiteral(hydra.core.LiteralTypeBoolean(hydra.core.Unit()))))))))))), None, [hydra.testing.Tag("disabledForAltInference")])]),
      hydra.testing.TestGroup("Optionals", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermOptional(hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42)))), hydra.core.TypeScheme([], hydra.core.TypeOptional(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)))))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")]),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermOptional(None), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeOptional(hydra.core.TypeVariable(hydra.core.Name("t0")))))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")])]),
      hydra.testing.TestGroup("Products", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermProduct([]), hydra.core.TypeScheme([], hydra.core.TypeProduct([])))), None, []),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermProduct([
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42))),
          hydra.core.TermLiteral(hydra.core.LiteralString("foo"))]), hydra.core.TypeScheme([], hydra.core.TypeProduct([
          hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)),
          hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))])))), None, [])]),
      hydra.testing.TestGroup("Records", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermRecord(hydra.core.Record("hydra.test.testGraph.testTypeLatLonName", [
          hydra.core.Field(hydra.core.Name("lat"), hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat32(37.774898529052734)))),
          hydra.core.Field(hydra.core.Name("lon"), hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat32(-122.41940307617188))))])), hydra.core.TypeScheme([], hydra.core.TypeRecord(hydra.core.RowType("hydra.test.testGraph.testTypeLatLonName", [
          hydra.core.FieldType(hydra.core.Name("lat"), hydra.core.TypeLiteral(hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT32))),
          hydra.core.FieldType(hydra.core.Name("lon"), hydra.core.TypeLiteral(hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT32)))]))))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")]),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermRecord(hydra.core.Record("hydra.test.testGraph.testTypeLatLonPolyName", [
          hydra.core.Field(hydra.core.Name("lat"), hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat32(37.774898529052734)))),
          hydra.core.Field(hydra.core.Name("lon"), hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat32(-122.41940307617188))))])), hydra.core.TypeScheme([], hydra.core.TypeRecord(hydra.core.RowType("hydra.test.testGraph.testTypeLatLonPolyName", [
          hydra.core.FieldType(hydra.core.Name("lat"), hydra.core.TypeLiteral(hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT32))),
          hydra.core.FieldType(hydra.core.Name("lon"), hydra.core.TypeLiteral(hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT32)))]))))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")]),
        hydra.testing.TestCaseWithMetadata("#3", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("lon"), None, hydra.core.TermRecord(hydra.core.Record("hydra.test.testGraph.testTypeLatLonPolyName", [
          hydra.core.Field(hydra.core.Name("lat"), hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat32(37.774898529052734)))),
          hydra.core.Field(hydra.core.Name("lon"), hydra.core.TermVariable(hydra.core.Name("lon")))]))))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT32)), hydra.core.TypeRecord(hydra.core.RowType("hydra.test.testGraph.testTypeLatLonPolyName", [
          hydra.core.FieldType(hydra.core.Name("lat"), hydra.core.TypeLiteral(hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT32))),
          hydra.core.FieldType(hydra.core.Name("lon"), hydra.core.TypeLiteral(hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT32)))]))))))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")]),
        hydra.testing.TestCaseWithMetadata("#4", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("latlon"), None, hydra.core.TermRecord(hydra.core.Record("hydra.test.testGraph.testTypeLatLonPolyName", [
          hydra.core.Field(hydra.core.Name("lat"), hydra.core.TermVariable(hydra.core.Name("latlon"))),
          hydra.core.Field(hydra.core.Name("lon"), hydra.core.TermVariable(hydra.core.Name("latlon")))]))))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeRecord(hydra.core.RowType("hydra.test.testGraph.testTypeLatLonPolyName", [
          hydra.core.FieldType(hydra.core.Name("lat"), hydra.core.TypeVariable(hydra.core.Name("t0"))),
          hydra.core.FieldType(hydra.core.Name("lon"), hydra.core.TypeVariable(hydra.core.Name("t0")))]))))))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")])]),
      hydra.testing.TestGroup("Unions", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermUnion(hydra.core.Injection("hydra.test.testGraph.testTypeTimestampName", hydra.core.Field(hydra.core.Name("unixTimeMillis"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueUint64(1638200308368)))))), hydra.core.TypeScheme([], "hydra.test.testGraph.testTypeTimestamp"))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")])]),
      hydra.testing.TestGroup("Sets", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermSet({hydra.core.TermLiteral(hydra.core.LiteralBoolean(True))}), hydra.core.TypeScheme([], hydra.core.TypeSet(hydra.core.TypeLiteral(hydra.core.LiteralTypeBoolean(hydra.core.Unit())))))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")]),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermSet({hydra.core.TermSet({})}), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeSet(hydra.core.TypeSet(hydra.core.TypeVariable(hydra.core.Name("t0"))))))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")])]),
      hydra.testing.TestGroup("Maps", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermMap({
          hydra.core.TermLiteral(hydra.core.LiteralString("firstName")): hydra.core.TermLiteral(hydra.core.LiteralString("Arthur")),
          hydra.core.TermLiteral(hydra.core.LiteralString("lastName")): hydra.core.TermLiteral(hydra.core.LiteralString("Dent"))}), hydra.core.TypeScheme([], hydra.core.TypeMap(hydra.core.MapType(hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit())), hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))))))), None, [hydra.testing.Tag("disabledForAlgorithmWInference")]),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermMap(hydra.lib.maps.empty), hydra.core.TypeScheme([hydra.core.Name("t0"), hydra.core.Name("t1")], hydra.core.TypeMap(hydra.core.MapType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeVariable(hydra.core.Name("t1"))))))), None, [hydra.testing.Tag("disabledForAlgorithmWInference")]),
        hydra.testing.TestCaseWithMetadata("#3", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("y"), None, hydra.core.TermMap({
          hydra.core.TermVariable(hydra.core.Name("x")): hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat64(0.1))),
          hydra.core.TermVariable(hydra.core.Name("y")): hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat64(0.2)))}))))))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeMap(hydra.core.MapType(hydra.core.TypeVariable(hydra.core.Name("t0")), hydra.core.TypeLiteral(hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT64))))))))))), None, [hydra.testing.Tag("disabledForAlgorithmWInference")])])], []),
    hydra.testing.TestGroup("Let terms", None, [
      hydra.testing.TestGroup("Empty let", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([], hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42))))), hydra.core.TypeScheme([], hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))), None, [hydra.testing.Tag("disabledForAltInference")])]),
      hydra.testing.TestGroup("Trivial let", None, [], [
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermLet(hydra.core.Let([
          hydra.core.LetBinding(hydra.core.Name("foo"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42))), None)], hydra.core.TermVariable(hydra.core.Name("foo")))), hydra.core.TypeScheme([], hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))), None, [])])], []),
    hydra.testing.TestGroup("List terms", None, [
      hydra.testing.TestGroup("List of strings", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermList([
          hydra.core.TermLiteral(hydra.core.LiteralString("foo")),
          hydra.core.TermLiteral(hydra.core.LiteralString("bar"))]), hydra.core.TypeScheme([], hydra.core.TypeList(hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit())))))), None, [])]),
      hydra.testing.TestGroup("List of lists of strings", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermList([
          hydra.core.TermList([hydra.core.TermLiteral(hydra.core.LiteralString("foo"))]),
          hydra.core.TermList([])]), hydra.core.TypeScheme([], hydra.core.TypeList(hydra.core.TypeList(hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))))))), None, [])]),
      hydra.testing.TestGroup("Empty list", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermList([]), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeList(hydra.core.TypeVariable(hydra.core.Name("t0")))))), None, [])]),
      hydra.testing.TestGroup("List containing an empty list", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermList([hydra.core.TermList([])]), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeList(hydra.core.TypeList(hydra.core.TypeVariable(hydra.core.Name("t0"))))))), None, [])]),
      hydra.testing.TestGroup("Lambda producing a list of integers", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermList([
          hydra.core.TermVariable(hydra.core.Name("x")),
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42)))])))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeList(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)))))))), None, [])]),
      hydra.testing.TestGroup("List with bound variables", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermList([
          hydra.core.TermVariable(hydra.core.Name("x")),
          hydra.core.TermLiteral(hydra.core.LiteralString("foo")),
          hydra.core.TermVariable(hydra.core.Name("x"))])))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit())), hydra.core.TypeList(hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit())))))))), None, [])])], []),
    hydra.testing.TestGroup("Primitive terms", None, [
      hydra.testing.TestGroup("Monomorphic primitive functions", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.strings.length"))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit())), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))))), None, [hydra.testing.Tag("disabledForAltInference")]),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.math.sub"))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))))))), None, [hydra.testing.Tag("disabledForAltInference")])]),
      hydra.testing.TestGroup("Polymorphic primitive functions", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("els"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.length"))), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.concat"))), hydra.core.TermVariable(hydra.core.Name("els"))))))))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeList(hydra.core.TypeList(hydra.core.TypeVariable(hydra.core.Name("t0")))), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))))), None, [hydra.testing.Tag("disabledForAltInference")])])], []),
    hydra.testing.TestGroup("Product terms", None, [
      hydra.testing.TestGroup("Empty product", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermProduct([]), hydra.core.TypeScheme([], hydra.core.TypeProduct([])))), None, [])]),
      hydra.testing.TestGroup("Non-empty, monotyped products", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermProduct([
          hydra.core.TermLiteral(hydra.core.LiteralString("foo")),
          hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42)))]), hydra.core.TypeScheme([], hydra.core.TypeProduct([
          hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit())),
          hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))])))), None, []),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermProduct([
          hydra.core.TermLiteral(hydra.core.LiteralString("foo")),
          hydra.core.TermList([
            hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat32(42.0))),
            hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat32(137.0)))])]), hydra.core.TypeScheme([], hydra.core.TypeProduct([
          hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit())),
          hydra.core.TypeList(hydra.core.TypeLiteral(hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT32)))])))), None, [])]),
      hydra.testing.TestGroup("Polytyped products", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermProduct([
          hydra.core.TermList([]),
          hydra.core.TermLiteral(hydra.core.LiteralString("foo"))]), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeProduct([
          hydra.core.TypeList(hydra.core.TypeVariable(hydra.core.Name("t0"))),
          hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))])))), None, [])])], []),
    hydra.testing.TestGroup("Sum terms", None, [
      hydra.testing.TestGroup("Singleton sum terms", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermSum(hydra.core.Sum(0, 1, hydra.core.TermLiteral(hydra.core.LiteralString("foo")))), hydra.core.TypeScheme([], hydra.core.TypeSum([
          hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))])))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")]),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermSum(hydra.core.Sum(0, 1, hydra.core.TermList([]))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeSum([hydra.core.TypeList(hydra.core.TypeVariable(hydra.core.Name("t0")))])))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")])]),
      hydra.testing.TestGroup("Non-singleton sum terms", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermSum(hydra.core.Sum(0, 2, hydra.core.TermLiteral(hydra.core.LiteralString("foo")))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeSum([
          hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit())),
          hydra.core.TypeVariable(hydra.core.Name("t0"))])))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")]),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermSum(hydra.core.Sum(1, 2, hydra.core.TermLiteral(hydra.core.LiteralString("foo")))), hydra.core.TypeScheme([hydra.core.Name("t0")], hydra.core.TypeSum([
          hydra.core.TypeVariable(hydra.core.Name("t0")),
          hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))])))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")])])], []),
    hydra.testing.TestGroup("Wrap terms", None, [
      hydra.testing.TestGroup("Wrap introductions", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermWrap(hydra.core.WrappedTerm("hydra.test.testGraph.testTypeStringAliasName", hydra.core.TermLiteral(hydra.core.LiteralString("foo")))), hydra.core.TypeScheme([], "hydra.test.testGraph.testTypeStringAlias"))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")]),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("v"), None, hydra.core.TermWrap(hydra.core.WrappedTerm("hydra.test.testGraph.testTypeStringAliasName", hydra.core.TermVariable(hydra.core.Name("v"))))))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit())), "hydra.test.testGraph.testTypeStringAlias"))))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")])]),
      hydra.testing.TestGroup("Wrap eliminations", None, [], [
        hydra.testing.TestCaseWithMetadata("#1", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionElimination(hydra.core.EliminationWrap("hydra.test.testGraph.testTypeStringAliasName"))), hydra.core.TermWrap(hydra.core.WrappedTerm("hydra.test.testGraph.testTypeStringAliasName", hydra.core.TermLiteral(hydra.core.LiteralString("foo")))))), hydra.core.TypeScheme([], hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")]),
        hydra.testing.TestCaseWithMetadata("#2", hydra.testing.TestCaseInference(hydra.testing.InferenceTestCase(hydra.core.TermFunction(hydra.core.FunctionElimination(hydra.core.EliminationWrap("hydra.test.testGraph.testTypeStringAliasName"))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType("hydra.test.testGraph.testTypeStringAlias", hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))))))), None, [
          hydra.testing.Tag("disabledForAlgorithmWInference"),
          hydra.testing.Tag("disabledForAltInference")])])], [])], [])], [])

listPrimitiveTests = hydra.testing.TestGroup("hydra.lib.lists primitives", None, [
  hydra.testing.TestGroup("apply", None, [], [
    hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.apply"))), hydra.core.TermList([
      hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.strings.toUpper"))),
      hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.strings.toLower")))]))), hydra.core.TermList([
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
    hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.bind"))), hydra.core.TermList([
      hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(1))),
      hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(2))),
      hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(3))),
      hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(4)))]))), hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), None, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.pure"))), hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.math.neg"))), hydra.core.TermVariable(hydra.core.Name("x"))))))))))), hydra.core.TermList([
      hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(-1))),
      hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(-2))),
      hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(-3))),
      hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(-4)))]))), None, [])]),
  hydra.testing.TestGroup("concat", None, [], [
    hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.concat"))), hydra.core.TermList([
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
    hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.head"))), hydra.core.TermList([
      hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(1))),
      hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(2))),
      hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(3)))]))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(1))))), None, [])]),
  hydra.testing.TestGroup("intercalate", None, [], [
    hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.intercalate"))), hydra.core.TermList([
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
    hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.intersperse"))), hydra.core.TermLiteral(hydra.core.LiteralString("and")))), hydra.core.TermList([
      hydra.core.TermLiteral(hydra.core.LiteralString("one")),
      hydra.core.TermLiteral(hydra.core.LiteralString("two")),
      hydra.core.TermLiteral(hydra.core.LiteralString("three"))]))), hydra.core.TermList([
      hydra.core.TermLiteral(hydra.core.LiteralString("one")),
      hydra.core.TermLiteral(hydra.core.LiteralString("and")),
      hydra.core.TermLiteral(hydra.core.LiteralString("two")),
      hydra.core.TermLiteral(hydra.core.LiteralString("and")),
      hydra.core.TermLiteral(hydra.core.LiteralString("three"))]))), None, [])]),
  hydra.testing.TestGroup("last", None, [], [
    hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.last"))), hydra.core.TermList([
      hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(1))),
      hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(2))),
      hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(3)))]))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(3))))), None, [])]),
  hydra.testing.TestGroup("length", None, [], [
    hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.length"))), hydra.core.TermList([
      hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(1))),
      hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(2))),
      hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(3)))]))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(3))))), None, [])]),
  hydra.testing.TestGroup("map", None, [], [
    hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.map"))), hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.strings.toUpper"))))), hydra.core.TermList([
      hydra.core.TermLiteral(hydra.core.LiteralString("one")),
      hydra.core.TermLiteral(hydra.core.LiteralString("two"))]))), hydra.core.TermList([
      hydra.core.TermLiteral(hydra.core.LiteralString("ONE")),
      hydra.core.TermLiteral(hydra.core.LiteralString("TWO"))]))), None, [])]),
  hydra.testing.TestGroup("pure", None, [], [
    hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.pure"))), hydra.core.TermLiteral(hydra.core.LiteralString("one")))), hydra.core.TermList([hydra.core.TermLiteral(hydra.core.LiteralString("one"))]))), None, [])])], [])

primitiveTests = hydra.testing.TestGroup("Primitive functions", "Test cases for primitive functions", ["listPrimitiveTests", "stringPrimitiveTests"], [])

stringPrimitiveTests = hydra.testing.TestGroup("hydra.lib.strings primitives", None, [
  hydra.testing.TestGroup("cat", None, [], [
    hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.strings.cat"))), hydra.core.TermList([
      hydra.core.TermLiteral(hydra.core.LiteralString("one")),
      hydra.core.TermLiteral(hydra.core.LiteralString("two")),
      hydra.core.TermLiteral(hydra.core.LiteralString("three"))]))), hydra.core.TermLiteral(hydra.core.LiteralString("onetwothree")))), None, []),
    hydra.testing.TestCaseWithMetadata("2", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.strings.cat"))), hydra.core.TermList([
      hydra.core.TermLiteral(hydra.core.LiteralString("")),
      hydra.core.TermLiteral(hydra.core.LiteralString("one")),
      hydra.core.TermLiteral(hydra.core.LiteralString("")),
      hydra.core.TermLiteral(hydra.core.LiteralString(""))]))), hydra.core.TermLiteral(hydra.core.LiteralString("one")))), None, []),
    hydra.testing.TestCaseWithMetadata("3", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.strings.cat"))), hydra.core.TermList([]))), hydra.core.TermLiteral(hydra.core.LiteralString("")))), None, [])]),
  hydra.testing.TestGroup("length", None, [], [
    hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.strings.length"))), hydra.core.TermLiteral(hydra.core.LiteralString("")))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(0))))), None, []),
    hydra.testing.TestCaseWithMetadata("2", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.strings.length"))), hydra.core.TermLiteral(hydra.core.LiteralString("a")))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(1))))), None, []),
    hydra.testing.TestCaseWithMetadata("3", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.strings.length"))), hydra.core.TermLiteral(hydra.core.LiteralString("one")))), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(3))))), None, [])]),
  hydra.testing.TestGroup("splitOn", None, [], [
    hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.strings.splitOn"))), hydra.core.TermLiteral(hydra.core.LiteralString("ss")))), hydra.core.TermLiteral(hydra.core.LiteralString("Mississippi")))), hydra.core.TermList([
      hydra.core.TermLiteral(hydra.core.LiteralString("Mi")),
      hydra.core.TermLiteral(hydra.core.LiteralString("i")),
      hydra.core.TermLiteral(hydra.core.LiteralString("ippi"))]))), None, []),
    hydra.testing.TestCaseWithMetadata("2", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.strings.splitOn"))), hydra.core.TermLiteral(hydra.core.LiteralString("Mississippi")))), hydra.core.TermLiteral(hydra.core.LiteralString("Mississippi")))), hydra.core.TermList([
      hydra.core.TermLiteral(hydra.core.LiteralString("")),
      hydra.core.TermLiteral(hydra.core.LiteralString(""))]))), None, []),
    hydra.testing.TestCaseWithMetadata("3", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.strings.splitOn"))), hydra.core.TermLiteral(hydra.core.LiteralString(" ")))), hydra.core.TermLiteral(hydra.core.LiteralString("one two three")))), hydra.core.TermList([
      hydra.core.TermLiteral(hydra.core.LiteralString("one")),
      hydra.core.TermLiteral(hydra.core.LiteralString("two")),
      hydra.core.TermLiteral(hydra.core.LiteralString("three"))]))), None, []),
    hydra.testing.TestCaseWithMetadata("4", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.strings.splitOn"))), hydra.core.TermLiteral(hydra.core.LiteralString(" ")))), hydra.core.TermLiteral(hydra.core.LiteralString(" one two three ")))), hydra.core.TermList([
      hydra.core.TermLiteral(hydra.core.LiteralString("")),
      hydra.core.TermLiteral(hydra.core.LiteralString("one")),
      hydra.core.TermLiteral(hydra.core.LiteralString("two")),
      hydra.core.TermLiteral(hydra.core.LiteralString("three")),
      hydra.core.TermLiteral(hydra.core.LiteralString(""))]))), None, []),
    hydra.testing.TestCaseWithMetadata("5", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.strings.splitOn"))), hydra.core.TermLiteral(hydra.core.LiteralString(" ")))), hydra.core.TermLiteral(hydra.core.LiteralString("  one two three")))), hydra.core.TermList([
      hydra.core.TermLiteral(hydra.core.LiteralString("")),
      hydra.core.TermLiteral(hydra.core.LiteralString("")),
      hydra.core.TermLiteral(hydra.core.LiteralString("one")),
      hydra.core.TermLiteral(hydra.core.LiteralString("two")),
      hydra.core.TermLiteral(hydra.core.LiteralString("three"))]))), None, []),
    hydra.testing.TestCaseWithMetadata("6", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.strings.splitOn"))), hydra.core.TermLiteral(hydra.core.LiteralString("  ")))), hydra.core.TermLiteral(hydra.core.LiteralString("  one two three")))), hydra.core.TermList([
      hydra.core.TermLiteral(hydra.core.LiteralString("")),
      hydra.core.TermLiteral(hydra.core.LiteralString("one two three"))]))), None, []),
    hydra.testing.TestCaseWithMetadata("6", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.strings.splitOn"))), hydra.core.TermLiteral(hydra.core.LiteralString("aa")))), hydra.core.TermLiteral(hydra.core.LiteralString("aaa")))), hydra.core.TermList([
      hydra.core.TermLiteral(hydra.core.LiteralString("")),
      hydra.core.TermLiteral(hydra.core.LiteralString("a"))]))), None, []),
    hydra.testing.TestCaseWithMetadata("7", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.strings.splitOn"))), hydra.core.TermLiteral(hydra.core.LiteralString("a")))), hydra.core.TermLiteral(hydra.core.LiteralString("")))), hydra.core.TermList([hydra.core.TermLiteral(hydra.core.LiteralString(""))]))), None, []),
    hydra.testing.TestCaseWithMetadata("8", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.strings.splitOn"))), hydra.core.TermLiteral(hydra.core.LiteralString("")))), hydra.core.TermLiteral(hydra.core.LiteralString("abc")))), hydra.core.TermList([
      hydra.core.TermLiteral(hydra.core.LiteralString("")),
      hydra.core.TermLiteral(hydra.core.LiteralString("a")),
      hydra.core.TermLiteral(hydra.core.LiteralString("b")),
      hydra.core.TermLiteral(hydra.core.LiteralString("c"))]))), None, []),
    hydra.testing.TestCaseWithMetadata("9", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.strings.splitOn"))), hydra.core.TermLiteral(hydra.core.LiteralString("")))), hydra.core.TermLiteral(hydra.core.LiteralString("")))), hydra.core.TermList([hydra.core.TermLiteral(hydra.core.LiteralString(""))]))), None, [])]),
  hydra.testing.TestGroup("toLower", None, [], [
    hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.strings.toLower"))), hydra.core.TermLiteral(hydra.core.LiteralString("One TWO threE")))), hydra.core.TermLiteral(hydra.core.LiteralString("one two three")))), None, []),
    hydra.testing.TestCaseWithMetadata("2", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.strings.toLower"))), hydra.core.TermLiteral(hydra.core.LiteralString("Abc123")))), hydra.core.TermLiteral(hydra.core.LiteralString("abc123")))), None, [])]),
  hydra.testing.TestGroup("toUpper", None, [], [
    hydra.testing.TestCaseWithMetadata("1", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.strings.toUpper"))), hydra.core.TermLiteral(hydra.core.LiteralString("One TWO threE")))), hydra.core.TermLiteral(hydra.core.LiteralString("ONE TWO THREE")))), None, []),
    hydra.testing.TestCaseWithMetadata("2", hydra.testing.TestCaseEvaluation(hydra.testing.EvaluationTestCase(hydra.testing.EvaluationStyle.EAGER, hydra.core.TermApplication(hydra.core.Application(hydra.core.TermFunction(hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.strings.toUpper"))), hydra.core.TermLiteral(hydra.core.LiteralString("Abc123")))), hydra.core.TermLiteral(hydra.core.LiteralString("ABC123")))), None, [])])], [])
