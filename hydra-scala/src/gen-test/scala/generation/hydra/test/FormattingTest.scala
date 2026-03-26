// Note: this is an automatically generated file. Do not edit.
// formatting

package generation.hydra.test

import org.scalatest.funsuite.AnyFunSuite

class FormattingTest extends AnyFunSuite {

  // case conversion

  test("case conversion - #1 (lower_snake_case -> UPPER_SNAKE_CASE)") {

    assert((

      hydra.formatting.convertCase(hydra.util.CaseConvention.lowerSnake)(hydra.util.CaseConvention.upperSnake)("a_hello_world_42_a42_42a_b")) == (

      "A_HELLO_WORLD_42_A42_42A_B"))

  }

  test("case conversion - #2 (lower_snake_case -> camelCase)") {

    assert((

      hydra.formatting.convertCase(hydra.util.CaseConvention.lowerSnake)(hydra.util.CaseConvention.camel)("a_hello_world_42_a42_42a_b")) == (

      "aHelloWorld42A4242aB"))

  }

  test("case conversion - #3 (lower_snake_case -> PascalCase)") {

    assert((

      hydra.formatting.convertCase(hydra.util.CaseConvention.lowerSnake)(hydra.util.CaseConvention.pascal)("a_hello_world_42_a42_42a_b")) == (

      "AHelloWorld42A4242aB"))

  }

  test("case conversion - #4 (lower_snake_case -> lower_snake_case)") {

    assert((

      hydra.formatting.convertCase(hydra.util.CaseConvention.lowerSnake)(hydra.util.CaseConvention.lowerSnake)("a_hello_world_42_a42_42a_b")) == (

      "a_hello_world_42_a42_42a_b"))

  }

  test("case conversion - #5 (UPPER_SNAKE_CASE -> lower_snake_case)") {

    assert((

      hydra.formatting.convertCase(hydra.util.CaseConvention.upperSnake)(hydra.util.CaseConvention.lowerSnake)("A_HELLO_WORLD_42_A42_42A_B")) == (

      "a_hello_world_42_a42_42a_b"))

  }

  test("case conversion - #6 (UPPER_SNAKE_CASE -> camelCase)") {

    assert((

      hydra.formatting.convertCase(hydra.util.CaseConvention.upperSnake)(hydra.util.CaseConvention.camel)("A_HELLO_WORLD_42_A42_42A_B")) == (

      "aHelloWorld42A4242aB"))

  }

  test("case conversion - #7 (UPPER_SNAKE_CASE -> PascalCase)") {

    assert((

      hydra.formatting.convertCase(hydra.util.CaseConvention.upperSnake)(hydra.util.CaseConvention.pascal)("A_HELLO_WORLD_42_A42_42A_B")) == (

      "AHelloWorld42A4242aB"))

  }

  test("case conversion - #8 (UPPER_SNAKE_CASE -> UPPER_SNAKE_CASE)") {

    assert((

      hydra.formatting.convertCase(hydra.util.CaseConvention.upperSnake)(hydra.util.CaseConvention.upperSnake)("A_HELLO_WORLD_42_A42_42A_B")) == (

      "A_HELLO_WORLD_42_A42_42A_B"))

  }

  test("case conversion - #9 (camelCase -> lower_snake_case)") {

    assert((

      hydra.formatting.convertCase(hydra.util.CaseConvention.camel)(hydra.util.CaseConvention.lowerSnake)("aHelloWorld42A4242aB")) == (

      "a_hello_world42_a4242a_b"))

  }

  test("case conversion - #10 (camelCase -> UPPER_SNAKE_CASE)") {

    assert((

      hydra.formatting.convertCase(hydra.util.CaseConvention.camel)(hydra.util.CaseConvention.upperSnake)("aHelloWorld42A4242aB")) == (

      "A_HELLO_WORLD42_A4242A_B"))

  }

  test("case conversion - #11 (camelCase -> PascalCase)") {

    assert((

      hydra.formatting.convertCase(hydra.util.CaseConvention.camel)(hydra.util.CaseConvention.pascal)("aHelloWorld42A4242aB")) == (

      "AHelloWorld42A4242aB"))

  }

  test("case conversion - #12 (camelCase -> camelCase)") {

    assert((

      hydra.formatting.convertCase(hydra.util.CaseConvention.camel)(hydra.util.CaseConvention.camel)("aHelloWorld42A4242aB")) == (

      "aHelloWorld42A4242aB"))

  }

  test("case conversion - #13 (PascalCase -> lower_snake_case)") {

    assert((

      hydra.formatting.convertCase(hydra.util.CaseConvention.pascal)(hydra.util.CaseConvention.lowerSnake)("AHelloWorld42A4242aB")) == (

      "a_hello_world42_a4242a_b"))

  }

  test("case conversion - #14 (PascalCase -> UPPER_SNAKE_CASE)") {

    assert((

      hydra.formatting.convertCase(hydra.util.CaseConvention.pascal)(hydra.util.CaseConvention.upperSnake)("AHelloWorld42A4242aB")) == (

      "A_HELLO_WORLD42_A4242A_B"))

  }

  test("case conversion - #15 (PascalCase -> camelCase)") {

    assert((

      hydra.formatting.convertCase(hydra.util.CaseConvention.pascal)(hydra.util.CaseConvention.camel)("AHelloWorld42A4242aB")) == (

      "aHelloWorld42A4242aB"))

  }

  test("case conversion - #16 (PascalCase -> PascalCase)") {

    assert((

      hydra.formatting.convertCase(hydra.util.CaseConvention.pascal)(hydra.util.CaseConvention.pascal)("AHelloWorld42A4242aB")) == (

      "AHelloWorld42A4242aB"))

  }
}
