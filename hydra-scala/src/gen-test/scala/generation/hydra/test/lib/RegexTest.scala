// Note: this is an automatically generated file. Do not edit.
// hydra.lib.regex primitives

package generation.hydra.test.lib

import org.scalatest.funsuite.AnyFunSuite

class RegexTest extends AnyFunSuite {

  // matches

  test("matches - exact match") {

    assert((

      hydra.lib.regex.matches("hello")("hello")) == (

      true))

  }

  test("matches - pattern match") {

    assert((

      hydra.lib.regex.matches("[a-z]+")("hello")) == (

      true))

  }

  test("matches - no match") {

    assert((

      hydra.lib.regex.matches("[0-9]+")("hello")) == (

      false))

  }

  test("matches - partial content does not match") {

    assert((

      hydra.lib.regex.matches("[a-z]+")("hello123")) == (

      false))

  }

  test("matches - digit pattern") {

    assert((

      hydra.lib.regex.matches("[0-9]+")("12345")) == (

      true))

  }

  test("matches - mixed pattern") {

    assert((

      hydra.lib.regex.matches("[a-z]+[0-9]+")("hello123")) == (

      true))

  }

  test("matches - empty pattern matches empty") {

    assert((

      hydra.lib.regex.matches("")("")) == (

      true))

  }

  test("matches - empty pattern does not match non-empty") {

    assert((

      hydra.lib.regex.matches("")("hello")) == (

      false))

  }

  test("matches - star matches empty") {

    assert((

      hydra.lib.regex.matches("a*")("")) == (

      true))

  }

  test("matches - alternation") {

    assert((

      hydra.lib.regex.matches("cat|dog")("cat")) == (

      true))

  }

  test("matches - alternation second") {

    assert((

      hydra.lib.regex.matches("cat|dog")("dog")) == (

      true))

  }

  test("matches - alternation no match") {

    assert((

      hydra.lib.regex.matches("cat|dog")("bird")) == (

      false))

  }

  test("matches - quantifier") {

    assert((

      hydra.lib.regex.matches("ab?c")("ac")) == (

      true))

  }

  test("matches - quantifier with optional") {

    assert((

      hydra.lib.regex.matches("ab?c")("abc")) == (

      true))

  }

  // find

  test("find - simple find") {

    assert((

      hydra.lib.regex.find("[0-9]+")("abc123def")) == (

      Some("123")))

  }

  test("find - no match") {

    assert((

      hydra.lib.regex.find("[0-9]+")("abcdef")) == (

      None))

  }

  test("find - find first") {

    assert((

      hydra.lib.regex.find("[a-z]+")("123abc456def")) == (

      Some("abc")))

  }

  test("find - empty input") {

    assert((

      hydra.lib.regex.find("[0-9]+")("")) == (

      None))

  }

  test("find - full match") {

    assert((

      hydra.lib.regex.find(".*")("hello")) == (

      Some("hello")))

  }

  // findAll

  test("findAll - multiple matches") {

    assert((

      hydra.lib.regex.findAll("[0-9]+")("a1b2c3")) == (

      Seq("1", "2", "3")))

  }

  test("findAll - no matches") {

    assert((

      hydra.lib.regex.findAll("[0-9]+")("abc")) == (

      Seq()))

  }

  test("findAll - overlapping words") {

    assert((

      hydra.lib.regex.findAll("[a-z]+")("abc def ghi")) == (

      Seq("abc", "def", "ghi")))

  }

  test("findAll - single match") {

    assert((

      hydra.lib.regex.findAll("hello")("say hello world")) == (

      Seq("hello")))

  }

  // replace

  test("replace - basic replace") {

    assert((

      hydra.lib.regex.replace("[0-9]+")("X")("abc123def456")) == (

      "abcXdef456"))

  }

  test("replace - no match") {

    assert((

      hydra.lib.regex.replace("[0-9]+")("X")("abcdef")) == (

      "abcdef"))

  }

  test("replace - replace at start") {

    assert((

      hydra.lib.regex.replace("^[a-z]+")("X")("abc123")) == (

      "X123"))

  }

  test("replace - empty replacement") {

    assert((

      hydra.lib.regex.replace("[0-9]+")("")("abc123def")) == (

      "abcdef"))

  }

  // replaceAll

  test("replaceAll - replace all digits") {

    assert((

      hydra.lib.regex.replaceAll("[0-9]+")("X")("a1b2c3")) == (

      "aXbXcX"))

  }

  test("replaceAll - no match") {

    assert((

      hydra.lib.regex.replaceAll("[0-9]+")("X")("abc")) == (

      "abc"))

  }

  test("replaceAll - replace all words") {

    assert((

      hydra.lib.regex.replaceAll("[a-z]+")("X")("abc 123 def")) == (

      "X 123 X"))

  }

  test("replaceAll - empty replacement") {

    assert((

      hydra.lib.regex.replaceAll("[0-9]+")("")("a1b2c3")) == (

      "abc"))

  }

  // split

  test("split - split on comma") {

    assert((

      hydra.lib.regex.split(",")("a,b,c")) == (

      Seq("a", "b", "c")))

  }

  test("split - split on spaces") {

    assert((

      hydra.lib.regex.split(" +")("a b  c")) == (

      Seq("a", "b", "c")))

  }

  test("split - no match") {

    assert((

      hydra.lib.regex.split(",")("abc")) == (

      Seq("abc")))

  }

  test("split - split on digits") {

    assert((

      hydra.lib.regex.split("[0-9]+")("a1b2c")) == (

      Seq("a", "b", "c")))

  }

  test("split - trailing delimiter") {

    assert((

      hydra.lib.regex.split(",")("a,b,")) == (

      Seq("a", "b", "")))

  }
}
