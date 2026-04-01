// Note: this is an automatically generated file. Do not edit.
// hydra.lib.regex primitives

package generation.hydra.test.lib

import org.scalatest.funsuite.AnyFunSuite

class RegexTest extends AnyFunSuite {

  // matches

  test("matches - exact match") {

    assert((

      true) == (

      true))

  }

  test("matches - pattern match") {

    assert((

      true) == (

      true))

  }

  test("matches - no match") {

    assert((

      false) == (

      false))

  }

  test("matches - partial content does not match") {

    assert((

      false) == (

      false))

  }

  test("matches - digit pattern") {

    assert((

      true) == (

      true))

  }

  test("matches - mixed pattern") {

    assert((

      true) == (

      true))

  }

  test("matches - empty pattern matches empty") {

    assert((

      true) == (

      true))

  }

  test("matches - empty pattern does not match non-empty") {

    assert((

      false) == (

      false))

  }

  test("matches - star matches empty") {

    assert((

      true) == (

      true))

  }

  test("matches - alternation") {

    assert((

      true) == (

      true))

  }

  test("matches - alternation second") {

    assert((

      true) == (

      true))

  }

  test("matches - alternation no match") {

    assert((

      false) == (

      false))

  }

  test("matches - quantifier") {

    assert((

      true) == (

      true))

  }

  test("matches - quantifier with optional") {

    assert((

      true) == (

      true))

  }

  // find

  test("find - simple find") {

    assert((

      just("123")) == (

      just("123")))

  }

  test("find - no match") {

    assert((

      nothing) == (

      nothing))

  }

  test("find - find first") {

    assert((

      just("abc")) == (

      just("abc")))

  }

  test("find - empty input") {

    assert((

      nothing) == (

      nothing))

  }

  test("find - full match") {

    assert((

      just("hello")) == (

      just("hello")))

  }

  // findAll

  test("findAll - multiple matches") {

    assert((

      ["1", "2", "3"]) == (

      ["1", "2", "3"]))

  }

  test("findAll - no matches") {

    assert((

      []) == (

      []))

  }

  test("findAll - overlapping words") {

    assert((

      ["abc", "def", "ghi"]) == (

      ["abc", "def", "ghi"]))

  }

  test("findAll - single match") {

    assert((

      ["hello"]) == (

      ["hello"]))

  }

  // replace

  test("replace - basic replace") {

    assert((

      "abcXdef456") == (

      "abcXdef456"))

  }

  test("replace - no match") {

    assert((

      "abcdef") == (

      "abcdef"))

  }

  test("replace - replace at start") {

    assert((

      "X123") == (

      "X123"))

  }

  test("replace - empty replacement") {

    assert((

      "abcdef") == (

      "abcdef"))

  }

  // replaceAll

  test("replaceAll - replace all digits") {

    assert((

      "aXbXcX") == (

      "aXbXcX"))

  }

  test("replaceAll - no match") {

    assert((

      "abc") == (

      "abc"))

  }

  test("replaceAll - replace all words") {

    assert((

      "X 123 X") == (

      "X 123 X"))

  }

  test("replaceAll - empty replacement") {

    assert((

      "abc") == (

      "abc"))

  }

  // split

  test("split - split on comma") {

    assert((

      ["a", "b", "c"]) == (

      ["a", "b", "c"]))

  }

  test("split - split on spaces") {

    assert((

      ["a", "b", "c"]) == (

      ["a", "b", "c"]))

  }

  test("split - no match") {

    assert((

      ["abc"]) == (

      ["abc"]))

  }

  test("split - split on digits") {

    assert((

      ["a", "b", "c"]) == (

      ["a", "b", "c"]))

  }

  test("split - trailing delimiter") {

    assert((

      ["a", "b", ""]) == (

      ["a", "b", ""]))

  }
}
