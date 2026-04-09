// Note: this is an automatically generated file. Do not edit.
// hydra.lib.maps primitives

package generation.hydra.test.lib

import org.scalatest.funsuite.AnyFunSuite

class MapsTest extends AnyFunSuite {

  // alter

  test("alter - insert new key") {

    assert((

      {1: "a", 2: "b", 3: "new"}) == (

      {1: "a", 2: "b", 3: "new"}))

  }

  test("alter - update existing key") {

    assert((

      {1: "a", 2: "updated"}) == (

      {1: "a", 2: "updated"}))

  }

  test("alter - delete key") {

    assert((

      {1: "a"}) == (

      {1: "a"}))

  }

  // bimap

  test("bimap - transform both") {

    assert((

      {2: "A", 4: "B"}) == (

      {2: "A", 4: "B"}))

  }

  test("bimap - empty map") {

    assert((

      {}) == (

      {}))

  }

  // elems

  test("elems - get all elements") {

    assert((

      ["a", "b"]) == (

      ["a", "b"]))

  }

  test("elems - unsorted keys") {

    assert((

      ["a", "b", "c"]) == (

      ["a", "b", "c"]))

  }

  test("elems - empty map") {

    assert((

      []) == (

      []))

  }

  // empty

  test("empty - empty map") {

    assert((

      {}) == (

      {}))

  }

  // filter

  test("filter - filter values starting with a") {

    assert((

      {1: "a", 3: "ab"}) == (

      {1: "a", 3: "ab"}))

  }

  test("filter - filter all") {

    assert((

      {}) == (

      {}))

  }

  test("filter - empty map") {

    assert((

      {}) == (

      {}))

  }

  // filterWithKey

  test("filterWithKey - filter by key > 1") {

    assert((

      {2: "b", 3: "c"}) == (

      {2: "b", 3: "c"}))

  }

  test("filterWithKey - filter all") {

    assert((

      {}) == (

      {}))

  }

  test("filterWithKey - empty map") {

    assert((

      {}) == (

      {}))

  }

  // findWithDefault

  test("findWithDefault - find existing") {

    assert((

      b) == (

      b))

  }

  test("findWithDefault - use default") {

    assert((

      default) == (

      default))

  }

  // fromList

  test("fromList - create from pairs") {

    assert((

      {1: "a", 2: "b"}) == (

      {1: "a", 2: "b"}))

  }

  test("fromList - duplicate keys") {

    assert((

      {1: "b"}) == (

      {1: "b"}))

  }

  test("fromList - empty list") {

    assert((

      {}) == (

      {}))

  }

  // insert

  test("insert - insert new key") {

    assert((

      {1: "a", 2: "b", 3: "c"}) == (

      {1: "a", 2: "b", 3: "c"}))

  }

  test("insert - update existing") {

    assert((

      {1: "a", 2: "updated"}) == (

      {1: "a", 2: "updated"}))

  }

  test("insert - insert into empty") {

    assert((

      {1: "x"}) == (

      {1: "x"}))

  }

  // keys

  test("keys - get all keys") {

    assert((

      [1, 2, 3]) == (

      [1, 2, 3]))

  }

  test("keys - unsorted keys") {

    assert((

      [1, 2, 3]) == (

      [1, 2, 3]))

  }

  test("keys - empty map") {

    assert((

      []) == (

      []))

  }

  // lookup

  test("lookup - find existing key") {

    assert((

      just("b")) == (

      just("b")))

  }

  test("lookup - key not found") {

    assert((

      nothing) == (

      nothing))

  }

  test("lookup - lookup in empty") {

    assert((

      nothing) == (

      nothing))

  }

  // map

  test("map - map over values") {

    assert((

      {1: "A", 2: "B"}) == (

      {1: "A", 2: "B"}))

  }

  test("map - map empty") {

    assert((

      {}) == (

      {}))

  }

  // mapKeys

  test("mapKeys - double keys") {

    assert((

      {2: "a", 4: "b"}) == (

      {2: "a", 4: "b"}))

  }

  test("mapKeys - empty map") {

    assert((

      {}) == (

      {}))

  }

  // member

  test("member - key exists") {

    assert((

      true) == (

      true))

  }

  test("member - key missing") {

    assert((

      false) == (

      false))

  }

  test("member - empty map") {

    assert((

      false) == (

      false))

  }

  // null

  test("null - empty map") {

    assert((

      true) == (

      true))

  }

  test("null - non-empty map") {

    assert((

      false) == (

      false))

  }

  // remove

  test("remove - remove existing") {

    assert((

      {1: "a", 3: "c"}) == (

      {1: "a", 3: "c"}))

  }

  test("remove - remove non-existing") {

    assert((

      {1: "a", 2: "b"}) == (

      {1: "a", 2: "b"}))

  }

  test("remove - remove from empty") {

    assert((

      {}) == (

      {}))

  }

  // singleton

  test("singleton - single entry") {

    assert((

      {42: "hello"}) == (

      {42: "hello"}))

  }

  // size

  test("size - three entries") {

    assert((

      3) == (

      3))

  }

  test("size - single entry") {

    assert((

      1) == (

      1))

  }

  test("size - empty map") {

    assert((

      0) == (

      0))

  }

  // toList

  test("toList - convert to pairs") {

    assert((

      [(1, "a"), (2, "b")]) == (

      [(1, "a"), (2, "b")]))

  }

  test("toList - unsorted keys") {

    assert((

      [(1, "a"), (2, "b"), (3, "c")]) == (

      [(1, "a"), (2, "b"), (3, "c")]))

  }

  test("toList - empty map") {

    assert((

      []) == (

      []))

  }

  // union

  test("union - union two maps") {

    assert((

      {1: "a", 2: "b", 3: "c"}) == (

      {1: "a", 2: "b", 3: "c"}))

  }

  test("union - union with empty") {

    assert((

      {1: "a"}) == (

      {1: "a"}))

  }

  test("union - empty with map") {

    assert((

      {1: "a"}) == (

      {1: "a"}))

  }
}
