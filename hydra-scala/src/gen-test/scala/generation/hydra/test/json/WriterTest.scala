// Note: this is an automatically generated file. Do not edit.
// JSON serialization

package generation.hydra.test.json

import org.scalatest.funsuite.AnyFunSuite

class WriterTest extends AnyFunSuite {

  // primitives

  test("primitives - null") {

    assert((

      null) == (

      null))

  }

  test("primitives - true") {

    assert((

      true) == (

      true))

  }

  test("primitives - false") {

    assert((

      false) == (

      false))

  }

  test("primitives - zero") {

    assert((

      0) == (

      0))

  }

  test("primitives - positive integer") {

    assert((

      42) == (

      42))

  }

  test("primitives - negative integer") {

    assert((

      -17) == (

      -17))

  }

  test("primitives - large integer") {

    assert((

      1000000) == (

      1000000))

  }

  test("primitives - decimal") {

    assert((

      3.14) == (

      3.14))

  }

  test("primitives - negative decimal") {

    assert((

      -2.5) == (

      -2.5))

  }

  test("primitives - small decimal") {

    assert((

      1.0e-3) == (

      1.0e-3))

  }

  // strings

  test("strings - empty string") {

    assert((

      "") == (

      ""))

  }

  test("strings - simple string") {

    assert((

      "hello") == (

      "hello"))

  }

  test("strings - string with spaces") {

    assert((

      "hello world") == (

      "hello world"))

  }

  test("strings - string with double quote") {

    assert((

      "say \"hi\"") == (

      "say \"hi\""))

  }

  test("strings - string with backslash") {

    assert((

      "path\\to\\file") == (

      "path\\to\\file"))

  }

  test("strings - string with newline") {

    assert((

      "line1\nline2") == (

      "line1\nline2"))

  }

  test("strings - string with carriage return") {

    assert((

      "line1\rline2") == (

      "line1\rline2"))

  }

  test("strings - string with tab") {

    assert((

      "col1\tcol2") == (

      "col1\tcol2"))

  }

  test("strings - string with mixed escapes") {

    assert((

      "a\"b\\c\nd") == (

      "a\"b\\c\nd"))

  }

  // arrays

  test("arrays - empty array") {

    assert((

      []) == (

      []))

  }

  test("arrays - single element") {

    assert((

      [1]) == (

      [1]))

  }

  test("arrays - multiple numbers") {

    assert((

      [1, 2, 3]) == (

      [1, 2, 3]))

  }

  test("arrays - multiple strings") {

    assert((

      ["a", "b"]) == (

      ["a", "b"]))

  }

  test("arrays - mixed types") {

    assert((

      [1, "two", true, null]) == (

      [1, "two", true, null]))

  }

  // objects

  test("objects - empty object") {

    assert((

      {}) == (

      {}))

  }

  test("objects - single key-value") {

    assert((

      {"name": "Alice"}) == (

      {"name": "Alice"}))

  }

  test("objects - multiple keys") {

    assert((

      {"a": 1, "b": 2}) == (

      {"a": 1, "b": 2}))

  }

  test("objects - mixed value types") {

    assert((

      {"active": true, "count": 42, "name": "test"}) == (

      {"active": true, "count": 42, "name": "test"}))

  }

  // nested structures

  test("nested structures - nested arrays") {

    assert((

      [[1, 2], [3, 4]]) == (

      [[1, 2], [3, 4]]))

  }

  test("nested structures - object with array") {

    assert((

      {"items": [1, 2]}) == (

      {"items": [1, 2]}))

  }

  test("nested structures - array of objects") {

    assert((

      [{"id": 1}, {"id": 2}]) == (

      [{"id": 1}, {"id": 2}]))

  }

  test("nested structures - nested object") {

    assert((

      {"user": {"name": "Bob"}}) == (

      {"user": {"name": "Bob"}}))

  }
}
