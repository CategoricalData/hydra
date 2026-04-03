// Note: this is an automatically generated file. Do not edit.
// formatting

package generation.hydra.test

import org.scalatest.funsuite.AnyFunSuite

class FormattingTest extends AnyFunSuite {

  // case conversion

  test("case conversion - #1 (lower_snake_case -> UPPER_SNAKE_CASE)") {

    assert((

      A_HELLO_WORLD_42_A42_42A_B) == (

      A_HELLO_WORLD_42_A42_42A_B))

  }

  test("case conversion - #2 (lower_snake_case -> camelCase)") {

    assert((

      aHelloWorld42A4242aB) == (

      aHelloWorld42A4242aB))

  }

  test("case conversion - #3 (lower_snake_case -> PascalCase)") {

    assert((

      AHelloWorld42A4242aB) == (

      AHelloWorld42A4242aB))

  }

  test("case conversion - #4 (lower_snake_case -> lower_snake_case)") {

    assert((

      a_hello_world_42_a42_42a_b) == (

      a_hello_world_42_a42_42a_b))

  }

  test("case conversion - #5 (UPPER_SNAKE_CASE -> lower_snake_case)") {

    assert((

      a_hello_world_42_a42_42a_b) == (

      a_hello_world_42_a42_42a_b))

  }

  test("case conversion - #6 (UPPER_SNAKE_CASE -> camelCase)") {

    assert((

      aHelloWorld42A4242aB) == (

      aHelloWorld42A4242aB))

  }

  test("case conversion - #7 (UPPER_SNAKE_CASE -> PascalCase)") {

    assert((

      AHelloWorld42A4242aB) == (

      AHelloWorld42A4242aB))

  }

  test("case conversion - #8 (UPPER_SNAKE_CASE -> UPPER_SNAKE_CASE)") {

    assert((

      A_HELLO_WORLD_42_A42_42A_B) == (

      A_HELLO_WORLD_42_A42_42A_B))

  }

  test("case conversion - #9 (camelCase -> lower_snake_case)") {

    assert((

      a_hello_world42_a4242a_b) == (

      a_hello_world42_a4242a_b))

  }

  test("case conversion - #10 (camelCase -> UPPER_SNAKE_CASE)") {

    assert((

      A_HELLO_WORLD42_A4242A_B) == (

      A_HELLO_WORLD42_A4242A_B))

  }

  test("case conversion - #11 (camelCase -> PascalCase)") {

    assert((

      AHelloWorld42A4242aB) == (

      AHelloWorld42A4242aB))

  }

  test("case conversion - #12 (camelCase -> camelCase)") {

    assert((

      aHelloWorld42A4242aB) == (

      aHelloWorld42A4242aB))

  }

  test("case conversion - #13 (PascalCase -> lower_snake_case)") {

    assert((

      a_hello_world42_a4242a_b) == (

      a_hello_world42_a4242a_b))

  }

  test("case conversion - #14 (PascalCase -> UPPER_SNAKE_CASE)") {

    assert((

      A_HELLO_WORLD42_A4242A_B) == (

      A_HELLO_WORLD42_A4242A_B))

  }

  test("case conversion - #15 (PascalCase -> camelCase)") {

    assert((

      aHelloWorld42A4242aB) == (

      aHelloWorld42A4242aB))

  }

  test("case conversion - #16 (PascalCase -> PascalCase)") {

    assert((

      AHelloWorld42A4242aB) == (

      AHelloWorld42A4242aB))

  }
}
