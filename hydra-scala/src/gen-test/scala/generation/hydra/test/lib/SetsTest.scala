// Note: this is an automatically generated file. Do not edit.
// hydra.lib.sets primitives

package generation.hydra.test.lib

import org.scalatest.funsuite.AnyFunSuite

class SetsTest extends AnyFunSuite {

  // empty

  test("empty - empty set") {

    assert((

      hydra.lib.sets.empty) == (

      scala.collection.immutable.Set()))

  }

  // singleton

  test("singleton - single element") {

    assert((

      hydra.lib.sets.singleton[Int](42)) == (

      scala.collection.immutable.Set(42)))

  }

  // fromList

  test("fromList - create from list") {

    assert((

      hydra.lib.sets.fromList[Int](Seq(1, 2, 3))) == (

      scala.collection.immutable.Set(1, 2, 3)))

  }

  test("fromList - duplicates removed") {

    assert((

      hydra.lib.sets.fromList[Int](Seq(1, 2, 1, 3))) == (

      scala.collection.immutable.Set(1, 2, 3)))

  }

  test("fromList - empty list") {

    assert((

      hydra.lib.sets.fromList(Seq())) == (

      scala.collection.immutable.Set()))

  }

  // toList

  test("toList - convert to list") {

    assert((

      hydra.lib.sets.toList[Int](scala.collection.immutable.Set(1, 2, 3))) == (

      Seq(1, 2, 3)))

  }

  test("toList - unsorted input") {

    assert((

      hydra.lib.sets.toList[Int](scala.collection.immutable.Set(1, 2, 3))) == (

      Seq(1, 2, 3)))

  }

  test("toList - empty set") {

    assert((

      hydra.lib.sets.toList(scala.collection.immutable.Set())) == (

      Seq()))

  }

  // insert

  test("insert - insert new element") {

    assert((

      hydra.lib.sets.insert[Int](4)(scala.collection.immutable.Set(1, 2, 3))) == (

      scala.collection.immutable.Set(1, 2, 3, 4)))

  }

  test("insert - insert existing element") {

    assert((

      hydra.lib.sets.insert[Int](2)(scala.collection.immutable.Set(1, 2, 3))) == (

      scala.collection.immutable.Set(1, 2, 3)))

  }

  test("insert - insert into empty") {

    assert((

      hydra.lib.sets.insert[Int](1)(scala.collection.immutable.Set())) == (

      scala.collection.immutable.Set(1)))

  }

  // delete

  test("delete - delete existing") {

    assert((

      hydra.lib.sets.delete[Int](2)(scala.collection.immutable.Set(1, 2, 3))) == (

      scala.collection.immutable.Set(1, 3)))

  }

  test("delete - delete non-existing") {

    assert((

      hydra.lib.sets.delete[Int](4)(scala.collection.immutable.Set(1, 2, 3))) == (

      scala.collection.immutable.Set(1, 2, 3)))

  }

  test("delete - delete from empty") {

    assert((

      hydra.lib.sets.delete[Int](1)(scala.collection.immutable.Set())) == (

      scala.collection.immutable.Set()))

  }

  // member

  test("member - element exists") {

    assert((

      hydra.lib.sets.member[Int](2)(scala.collection.immutable.Set(1, 2, 3))) == (

      true))

  }

  test("member - element missing") {

    assert((

      hydra.lib.sets.member[Int](4)(scala.collection.immutable.Set(1, 2, 3))) == (

      false))

  }

  test("member - empty set") {

    assert((

      hydra.lib.sets.member[Int](1)(scala.collection.immutable.Set())) == (

      false))

  }

  // size

  test("size - three elements") {

    assert((

      hydra.lib.sets.size[Int](scala.collection.immutable.Set(1, 2, 3))) == (

      3))

  }

  test("size - single element") {

    assert((

      hydra.lib.sets.size[Int](scala.collection.immutable.Set(42))) == (

      1))

  }

  test("size - empty set") {

    assert((

      hydra.lib.sets.size(scala.collection.immutable.Set())) == (

      0))

  }

  // null

  test("null - empty set") {

    assert((

      hydra.lib.sets.`null`(scala.collection.immutable.Set())) == (

      true))

  }

  test("null - non-empty set") {

    assert((

      hydra.lib.sets.`null`[Int](scala.collection.immutable.Set(1, 2))) == (

      false))

  }

  // union

  test("union - union two sets") {

    assert((

      hydra.lib.sets.union[Int](scala.collection.immutable.Set(1, 2))(scala.collection.immutable.Set(2, 3))) == (

      scala.collection.immutable.Set(1, 2, 3)))

  }

  test("union - union with empty") {

    assert((

      hydra.lib.sets.union[Int](scala.collection.immutable.Set(1, 2))(scala.collection.immutable.Set())) == (

      scala.collection.immutable.Set(1, 2)))

  }

  test("union - empty with non-empty") {

    assert((

      hydra.lib.sets.union[Int](scala.collection.immutable.Set())(scala.collection.immutable.Set(1, 2))) == (

      scala.collection.immutable.Set(1, 2)))

  }

  // unions

  test("unions - union of multiple sets") {

    assert((

      hydra.lib.sets.unions[Int](Seq(scala.collection.immutable.Set(1, 2), scala.collection.immutable.Set(2, 3), scala.collection.immutable.Set(3, 4)))) == (

      scala.collection.immutable.Set(1, 2, 3, 4)))

  }

  test("unions - union with empty sets") {

    assert((

      hydra.lib.sets.unions[Int](Seq(scala.collection.immutable.Set(1, 2), scala.collection.immutable.Set(), scala.collection.immutable.Set(3)))) == (

      scala.collection.immutable.Set(1, 2, 3)))

  }

  test("unions - empty list of sets") {

    assert((

      hydra.lib.sets.unions(Seq())) == (

      scala.collection.immutable.Set()))

  }

  test("unions - single set") {

    assert((

      hydra.lib.sets.unions[Int](Seq(scala.collection.immutable.Set(1, 2, 3)))) == (

      scala.collection.immutable.Set(1, 2, 3)))

  }

  // intersection

  test("intersection - common elements") {

    assert((

      hydra.lib.sets.intersection[Int](scala.collection.immutable.Set(1, 2, 3))(scala.collection.immutable.Set(2, 3, 4))) == (

      scala.collection.immutable.Set(2, 3)))

  }

  test("intersection - no common elements") {

    assert((

      hydra.lib.sets.intersection[Int](scala.collection.immutable.Set(1, 2))(scala.collection.immutable.Set(3, 4))) == (

      scala.collection.immutable.Set()))

  }

  test("intersection - intersection with empty") {

    assert((

      hydra.lib.sets.intersection[Int](scala.collection.immutable.Set(1, 2))(scala.collection.immutable.Set())) == (

      scala.collection.immutable.Set()))

  }

  // difference

  test("difference - remove elements") {

    assert((

      hydra.lib.sets.difference[Int](scala.collection.immutable.Set(1, 2, 3))(scala.collection.immutable.Set(2, 4))) == (

      scala.collection.immutable.Set(1, 3)))

  }

  test("difference - no overlap") {

    assert((

      hydra.lib.sets.difference[Int](scala.collection.immutable.Set(1, 2))(scala.collection.immutable.Set(3, 4))) == (

      scala.collection.immutable.Set(1, 2)))

  }

  test("difference - difference with empty") {

    assert((

      hydra.lib.sets.difference[Int](scala.collection.immutable.Set(1, 2))(scala.collection.immutable.Set())) == (

      scala.collection.immutable.Set(1, 2)))

  }

  // map

  test("map - map function") {

    assert((

      hydra.lib.sets.map[Int, Int]((x: Int) => hydra.lib.math.mul(x)(2))(scala.collection.immutable.Set(1, 2, 3))) == (

      scala.collection.immutable.Set(2, 4, 6)))

  }

  test("map - map on empty") {

    assert((

      hydra.lib.sets.map[Int, Int]((x: Int) => hydra.lib.math.mul(x)(2))(scala.collection.immutable.Set())) == (

      scala.collection.immutable.Set()))

  }
}
