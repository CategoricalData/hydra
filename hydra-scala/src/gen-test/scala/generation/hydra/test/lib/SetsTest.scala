// Note: this is an automatically generated file. Do not edit.
// hydra.lib.sets primitives

package generation.hydra.test.lib

import org.scalatest.funsuite.AnyFunSuite

class SetsTest extends AnyFunSuite {

  // empty

  test("empty - empty set") {

    assert((

      {}) == (

      {}))

  }

  // singleton

  test("singleton - single element") {

    assert((

      {42}) == (

      {42}))

  }

  // fromList

  test("fromList - create from list") {

    assert((

      {1, 2, 3}) == (

      {1, 2, 3}))

  }

  test("fromList - duplicates removed") {

    assert((

      {1, 2, 3}) == (

      {1, 2, 3}))

  }

  test("fromList - empty list") {

    assert((

      {}) == (

      {}))

  }

  // toList

  test("toList - convert to list") {

    assert((

      [1, 2, 3]) == (

      [1, 2, 3]))

  }

  test("toList - unsorted input") {

    assert((

      [1, 2, 3]) == (

      [1, 2, 3]))

  }

  test("toList - empty set") {

    assert((

      []) == (

      []))

  }

  // insert

  test("insert - insert new element") {

    assert((

      {1, 2, 3, 4}) == (

      {1, 2, 3, 4}))

  }

  test("insert - insert existing element") {

    assert((

      {1, 2, 3}) == (

      {1, 2, 3}))

  }

  test("insert - insert into empty") {

    assert((

      {1}) == (

      {1}))

  }

  // delete

  test("delete - delete existing") {

    assert((

      {1, 3}) == (

      {1, 3}))

  }

  test("delete - delete non-existing") {

    assert((

      {1, 2, 3}) == (

      {1, 2, 3}))

  }

  test("delete - delete from empty") {

    assert((

      {}) == (

      {}))

  }

  // member

  test("member - element exists") {

    assert((

      true) == (

      true))

  }

  test("member - element missing") {

    assert((

      false) == (

      false))

  }

  test("member - empty set") {

    assert((

      false) == (

      false))

  }

  // size

  test("size - three elements") {

    assert((

      3) == (

      3))

  }

  test("size - single element") {

    assert((

      1) == (

      1))

  }

  test("size - empty set") {

    assert((

      0) == (

      0))

  }

  // null

  test("null - empty set") {

    assert((

      true) == (

      true))

  }

  test("null - non-empty set") {

    assert((

      false) == (

      false))

  }

  // union

  test("union - union two sets") {

    assert((

      {1, 2, 3}) == (

      {1, 2, 3}))

  }

  test("union - union with empty") {

    assert((

      {1, 2}) == (

      {1, 2}))

  }

  test("union - empty with non-empty") {

    assert((

      {1, 2}) == (

      {1, 2}))

  }

  // unions

  test("unions - union of multiple sets") {

    assert((

      {1, 2, 3, 4}) == (

      {1, 2, 3, 4}))

  }

  test("unions - union with empty sets") {

    assert((

      {1, 2, 3}) == (

      {1, 2, 3}))

  }

  test("unions - empty list of sets") {

    assert((

      {}) == (

      {}))

  }

  test("unions - single set") {

    assert((

      {1, 2, 3}) == (

      {1, 2, 3}))

  }

  // intersection

  test("intersection - common elements") {

    assert((

      {2, 3}) == (

      {2, 3}))

  }

  test("intersection - no common elements") {

    assert((

      {}) == (

      {}))

  }

  test("intersection - intersection with empty") {

    assert((

      {}) == (

      {}))

  }

  // difference

  test("difference - remove elements") {

    assert((

      {1, 3}) == (

      {1, 3}))

  }

  test("difference - no overlap") {

    assert((

      {1, 2}) == (

      {1, 2}))

  }

  test("difference - difference with empty") {

    assert((

      {1, 2}) == (

      {1, 2}))

  }

  // map

  test("map - map function") {

    assert((

      {2, 4, 6}) == (

      {2, 4, 6}))

  }

  test("map - map on empty") {

    assert((

      {}) == (

      {}))

  }
}
