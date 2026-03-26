// Note: this is an automatically generated file. Do not edit.
// hydra.lib.maps primitives

package generation.hydra.test.lib

import org.scalatest.funsuite.AnyFunSuite

class MapsTest extends AnyFunSuite {

  // alter

  test("alter - insert new key") {

    assert((

      hydra.lib.maps.alter[scala.Predef.String, Int]((opt: Option[scala.Predef.String]) => Some("new"))(3)(Map(1 -> "a", 2 -> "b"))) == (

      Map(1 -> "a", 2 -> "b", 3 -> "new")))

  }

  test("alter - update existing key") {

    assert((

      hydra.lib.maps.alter[scala.Predef.String, Int]((opt: Option[scala.Predef.String]) => Some("updated"))(2)(Map(1 -> "a", 2 -> "b"))) == (

      Map(1 -> "a", 2 -> "updated")))

  }

  test("alter - delete key") {

    assert((

      hydra.lib.maps.alter[scala.Predef.String, Int]((opt: Option[scala.Predef.String]) => None)(2)(Map(1 -> "a", 2 -> "b"))) == (

      Map(1 -> "a")))

  }

  // bimap

  test("bimap - transform both") {

    assert((

      hydra.lib.maps.bimap[Int, Int, scala.Predef.String, scala.Predef.String]((k: Int) => hydra.lib.math.mul(k)(2))((v: scala.Predef.String) => hydra.lib.strings.toUpper(v))(Map(1 -> "a", 2 -> "b"))) == (

      Map(2 -> "A", 4 -> "B")))

  }

  test("bimap - empty map") {

    assert((

      hydra.lib.maps.bimap[Int, Int, scala.Predef.String, scala.Predef.String]((k: Int) => hydra.lib.math.mul(k)(2))((v: scala.Predef.String) => hydra.lib.strings.toUpper(v))(Map())) == (

      Map()))

  }

  // elems

  test("elems - get all elements") {

    assert((

      hydra.lib.maps.elems[Int, scala.Predef.String](Map(1 -> "a", 2 -> "b"))) == (

      Seq("a", "b")))

  }

  test("elems - unsorted keys") {

    assert((

      hydra.lib.maps.elems[Int, scala.Predef.String](Map(1 -> "a", 2 -> "b", 3 -> "c"))) == (

      Seq("a", "b", "c")))

  }

  test("elems - empty map") {

    assert((

      hydra.lib.maps.elems(Map())) == (

      Seq()))

  }

  // empty

  test("empty - empty map") {

    assert((

      hydra.lib.maps.empty) == (

      Map()))

  }

  // filter

  test("filter - filter values starting with a") {

    assert((

      hydra.lib.maps.filter[scala.Predef.String, Int]((v: scala.Predef.String) =>
  hydra.lib.equality.equal[Int](hydra.lib.strings.charAt(0)(v))(97))(Map(1 -> "a", 2 -> "b", 3 -> "ab"))) == (

      Map(1 -> "a", 3 -> "ab")))

  }

  test("filter - filter all") {

    assert((

      hydra.lib.maps.filter[scala.Predef.String, Int]((v: scala.Predef.String) =>
  hydra.lib.equality.equal[Int](hydra.lib.strings.charAt(0)(v))(97))(Map(1 -> "b", 2 -> "c"))) == (

      Map()))

  }

  test("filter - empty map") {

    assert((

      hydra.lib.maps.filter((v: scala.Predef.String) =>
  hydra.lib.equality.equal[Int](hydra.lib.strings.charAt(0)(v))(97))(Map())) == (

      Map()))

  }

  // filterWithKey

  test("filterWithKey - filter by key > 1") {

    assert((

      hydra.lib.maps.filterWithKey[Int, scala.Predef.String]((k: Int) => (v: scala.Predef.String) => hydra.lib.equality.gt[Int](k)(1))(Map(1 -> "a", 2 -> "b", 3 -> "c"))) == (

      Map(2 -> "b", 3 -> "c")))

  }

  test("filterWithKey - filter all") {

    assert((

      hydra.lib.maps.filterWithKey[Int, scala.Predef.String]((k: Int) => (v: scala.Predef.String) => hydra.lib.equality.gt[Int](k)(1))(Map(1 -> "a"))) == (

      Map()))

  }

  test("filterWithKey - empty map") {

    assert((

      hydra.lib.maps.filterWithKey((k: Int) => (v) => hydra.lib.equality.gt[Int](k)(1))(Map())) == (

      Map()))

  }

  // findWithDefault

  test("findWithDefault - find existing") {

    assert((

      hydra.lib.maps.findWithDefault[scala.Predef.String, Int]("default")(2)(Map(1 -> "a", 2 -> "b"))) == (

      "b"))

  }

  test("findWithDefault - use default") {

    assert((

      hydra.lib.maps.findWithDefault[scala.Predef.String, Int]("default")(3)(Map(1 -> "a", 2 -> "b"))) == (

      "default"))

  }

  // fromList

  test("fromList - create from pairs") {

    assert((

      hydra.lib.maps.fromList[Int, scala.Predef.String](Seq(Tuple2(1, "a"), Tuple2(2, "b")))) == (

      Map(1 -> "a", 2 -> "b")))

  }

  test("fromList - duplicate keys") {

    assert((

      hydra.lib.maps.fromList[Int, scala.Predef.String](Seq(Tuple2(1, "a"), Tuple2(1, "b")))) == (

      Map(1 -> "b")))

  }

  test("fromList - empty list") {

    assert((

      hydra.lib.maps.fromList(Seq())) == (

      Map()))

  }

  // insert

  test("insert - insert new key") {

    assert((

      hydra.lib.maps.insert[Int, scala.Predef.String](3)("c")(Map(1 -> "a", 2 -> "b"))) == (

      Map(1 -> "a", 2 -> "b", 3 -> "c")))

  }

  test("insert - update existing") {

    assert((

      hydra.lib.maps.insert[Int, scala.Predef.String](2)("updated")(Map(1 -> "a", 2 -> "b"))) == (

      Map(1 -> "a", 2 -> "updated")))

  }

  test("insert - insert into empty") {

    assert((

      hydra.lib.maps.insert[Int, scala.Predef.String](1)("x")(Map())) == (

      Map(1 -> "x")))

  }

  // keys

  test("keys - get all keys") {

    assert((

      hydra.lib.maps.keys[Int, scala.Predef.String](Map(1 -> "a", 2 -> "b", 3 -> "c"))) == (

      Seq(1, 2, 3)))

  }

  test("keys - unsorted keys") {

    assert((

      hydra.lib.maps.keys[Int, scala.Predef.String](Map(1 -> "a", 2 -> "b", 3 -> "c"))) == (

      Seq(1, 2, 3)))

  }

  test("keys - empty map") {

    assert((

      hydra.lib.maps.keys(Map())) == (

      Seq()))

  }

  // lookup

  test("lookup - find existing key") {

    assert((

      hydra.lib.maps.lookup[Int, scala.Predef.String](2)(Map(1 -> "a", 2 -> "b"))) == (

      Some("b")))

  }

  test("lookup - key not found") {

    assert((

      hydra.lib.maps.lookup[Int, scala.Predef.String](3)(Map(1 -> "a", 2 -> "b"))) == (

      None))

  }

  test("lookup - lookup in empty") {

    assert((

      hydra.lib.maps.lookup(1)(Map())) == (

      None))

  }

  // map

  test("map - map over values") {

    assert((

      hydra.lib.maps.map[scala.Predef.String, scala.Predef.String, Int]((s: scala.Predef.String) => hydra.lib.strings.toUpper(s))(Map(1 -> "a", 2 -> "b"))) == (

      Map(1 -> "A", 2 -> "B")))

  }

  test("map - map empty") {

    assert((

      hydra.lib.maps.map((s: scala.Predef.String) => hydra.lib.strings.toUpper(s))(Map())) == (

      Map()))

  }

  // mapKeys

  test("mapKeys - double keys") {

    assert((

      hydra.lib.maps.mapKeys[Int, Int, scala.Predef.String]((k: Int) => hydra.lib.math.mul(k)(2))(Map(1 -> "a", 2 -> "b"))) == (

      Map(2 -> "a", 4 -> "b")))

  }

  test("mapKeys - empty map") {

    assert((

      hydra.lib.maps.mapKeys((k: Int) => hydra.lib.math.mul(k)(2))(Map())) == (

      Map()))

  }

  // member

  test("member - key exists") {

    assert((

      hydra.lib.maps.member[Int, scala.Predef.String](2)(Map(1 -> "a", 2 -> "b"))) == (

      true))

  }

  test("member - key missing") {

    assert((

      hydra.lib.maps.member[Int, scala.Predef.String](3)(Map(1 -> "a", 2 -> "b"))) == (

      false))

  }

  test("member - empty map") {

    assert((

      hydra.lib.maps.member(1)(Map())) == (

      false))

  }

  // null

  test("null - empty map") {

    assert((

      hydra.lib.maps.`null`(Map())) == (

      true))

  }

  test("null - non-empty map") {

    assert((

      hydra.lib.maps.`null`[Int, scala.Predef.String](Map(1 -> "a"))) == (

      false))

  }

  // remove

  test("remove - remove existing") {

    assert((

      hydra.lib.maps.delete[Int, scala.Predef.String](2)(Map(1 -> "a", 2 -> "b", 3 -> "c"))) == (

      Map(1 -> "a", 3 -> "c")))

  }

  test("remove - remove non-existing") {

    assert((

      hydra.lib.maps.delete[Int, scala.Predef.String](4)(Map(1 -> "a", 2 -> "b"))) == (

      Map(1 -> "a", 2 -> "b")))

  }

  test("remove - remove from empty") {

    assert((

      hydra.lib.maps.delete(1)(Map())) == (

      Map()))

  }

  // singleton

  test("singleton - single entry") {

    assert((

      hydra.lib.maps.singleton[Int, scala.Predef.String](42)("hello")) == (

      Map(42 -> "hello")))

  }

  // size

  test("size - three entries") {

    assert((

      hydra.lib.maps.size[Int, scala.Predef.String](Map(1 -> "a", 2 -> "b", 3 -> "c"))) == (

      3))

  }

  test("size - single entry") {

    assert((

      hydra.lib.maps.size[Int, scala.Predef.String](Map(42 -> "test"))) == (

      1))

  }

  test("size - empty map") {

    assert((

      hydra.lib.maps.size(Map())) == (

      0))

  }

  // toList

  test("toList - convert to pairs") {

    assert((

      hydra.lib.maps.toList[Int, scala.Predef.String](Map(1 -> "a", 2 -> "b"))) == (

      Seq(Tuple2(1, "a"), Tuple2(2, "b"))))

  }

  test("toList - unsorted keys") {

    assert((

      hydra.lib.maps.toList[Int, scala.Predef.String](Map(1 -> "a", 2 -> "b", 3 -> "c"))) == (

      Seq(Tuple2(1, "a"), Tuple2(2, "b"), Tuple2(3, "c"))))

  }

  test("toList - empty map") {

    assert((

      hydra.lib.maps.toList(Map())) == (

      Seq()))

  }

  // union

  test("union - union two maps") {

    assert((

      hydra.lib.maps.union[Int, scala.Predef.String](Map(1 -> "a", 2 -> "b"))(Map(2 -> "x", 3 -> "c"))) == (

      Map(1 -> "a", 2 -> "b", 3 -> "c")))

  }

  test("union - union with empty") {

    assert((

      hydra.lib.maps.union[Int, scala.Predef.String](Map(1 -> "a"))(Map())) == (

      Map(1 -> "a")))

  }

  test("union - empty with map") {

    assert((

      hydra.lib.maps.union[Int, scala.Predef.String](Map())(Map(1 -> "a"))) == (

      Map(1 -> "a")))

  }
}
