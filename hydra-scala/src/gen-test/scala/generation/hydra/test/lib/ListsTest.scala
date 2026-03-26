// Note: this is an automatically generated file. Do not edit.
// hydra.lib.lists primitives

package generation.hydra.test.lib

import org.scalatest.funsuite.AnyFunSuite

class ListsTest extends AnyFunSuite {

  // apply

  // string transformations

  test("apply - string transformations - string transformations") {

    assert((

      hydra.lib.lists.apply[scala.Predef.String, scala.Predef.String](Seq(hydra.lib.strings.toUpper, hydra.lib.strings.toLower))(Seq("One", "Two", "Three"))) == (

      Seq("ONE", "TWO", "THREE", "one", "two", "three")))

  }

  // edge cases

  test("apply - edge cases - empty function list") {

    assert((

      hydra.lib.lists.apply(Seq())(Seq("a", "b"))) == (

      Seq()))

  }

  test("apply - edge cases - empty input list") {

    assert((

      hydra.lib.lists.apply[scala.Predef.String, scala.Predef.String](Seq(hydra.lib.strings.toUpper))(Seq())) == (

      Seq()))

  }

  test("apply - edge cases - single function") {

    assert((

      hydra.lib.lists.apply[scala.Predef.String, scala.Predef.String](Seq(hydra.lib.strings.toUpper))(Seq("hello"))) == (

      Seq("HELLO")))

  }

  test("apply - edge cases - single input") {

    assert((

      hydra.lib.lists.apply[scala.Predef.String, scala.Predef.String](Seq(hydra.lib.strings.toUpper, hydra.lib.strings.toLower))(Seq("Test"))) == (

      Seq("TEST", "test")))

  }

  // at

  test("at - first element") {

    assert((

      hydra.lib.lists.at[Int](0)(Seq(1, 2, 3))) == (

      1))

  }

  test("at - middle element") {

    assert((

      hydra.lib.lists.at[Int](1)(Seq(1, 2, 3))) == (

      2))

  }

  test("at - last element") {

    assert((

      hydra.lib.lists.at[Int](2)(Seq(1, 2, 3))) == (

      3))

  }

  test("at - single element list") {

    assert((

      hydra.lib.lists.at[Int](0)(Seq(42))) == (

      42))

  }

  test("at - string list access") {

    assert((

      hydra.lib.lists.at[scala.Predef.String](1)(Seq("hello", "world"))) == (

      "world"))

  }

  // bind

  test("bind - negation function") {

    assert((

      hydra.lib.lists.bind[Int, Int](Seq(1, 2, 3, 4))((x: Int) => hydra.lib.lists.pure[Int](hydra.lib.math.negate(x)))) == (

      Seq(-1, -2, -3, -4)))

  }

  test("bind - empty list") {

    assert((

      hydra.lib.lists.bind[Int, Int](Seq())((x: Int) => hydra.lib.lists.pure[Int](hydra.lib.math.negate(x)))) == (

      Seq()))

  }

  test("bind - single element") {

    assert((

      hydra.lib.lists.bind[Int, Int](Seq(5))((x: Int) => hydra.lib.lists.pure[Int](hydra.lib.math.negate(x)))) == (

      Seq(-5)))

  }

  test("bind - duplicate elements") {

    assert((

      hydra.lib.lists.bind[Int, Int](Seq(1, 1, 2))((x: Int) => hydra.lib.lists.pure[Int](hydra.lib.math.negate(x)))) == (

      Seq(-1, -1, -2)))

  }

  // concat

  test("concat - multiple non-empty lists") {

    assert((

      hydra.lib.lists.concat[Int](Seq(Seq(1, 2, 3), Seq(4, 5), Seq(6, 7, 8)))) == (

      Seq(1, 2, 3, 4, 5, 6, 7, 8)))

  }

  test("concat - empty lists included") {

    assert((

      hydra.lib.lists.concat[Int](Seq(Seq(), Seq(1, 2), Seq(), Seq(3)))) == (

      Seq(1, 2, 3)))

  }

  test("concat - single list") {

    assert((

      hydra.lib.lists.concat[Int](Seq(Seq(1, 2, 3)))) == (

      Seq(1, 2, 3)))

  }

  test("concat - all empty lists") {

    assert((

      hydra.lib.lists.concat(Seq(Seq(), Seq(), Seq()))) == (

      Seq()))

  }

  test("concat - empty list of lists") {

    assert((

      hydra.lib.lists.concat(Seq())) == (

      Seq()))

  }

  // concat2

  test("concat2 - two non-empty lists") {

    assert((

      hydra.lib.lists.concat2[Int](Seq(1, 2))(Seq(3, 4))) == (

      Seq(1, 2, 3, 4)))

  }

  test("concat2 - first list empty") {

    assert((

      hydra.lib.lists.concat2[Int](Seq())(Seq(1, 2))) == (

      Seq(1, 2)))

  }

  test("concat2 - second list empty") {

    assert((

      hydra.lib.lists.concat2[Int](Seq(1, 2))(Seq())) == (

      Seq(1, 2)))

  }

  test("concat2 - both lists empty") {

    assert((

      hydra.lib.lists.concat2(Seq())(Seq())) == (

      Seq()))

  }

  test("concat2 - single elements") {

    assert((

      hydra.lib.lists.concat2[Int](Seq(1))(Seq(2))) == (

      Seq(1, 2)))

  }

  test("concat2 - string lists") {

    assert((

      hydra.lib.lists.concat2[scala.Predef.String](Seq("a", "b"))(Seq("c", "d"))) == (

      Seq("a", "b", "c", "d")))

  }

  // cons

  test("cons - cons to non-empty list") {

    assert((

      hydra.lib.lists.cons[Int](1)(Seq(2, 3))) == (

      Seq(1, 2, 3)))

  }

  test("cons - cons to empty list") {

    assert((

      hydra.lib.lists.cons[Int](1)(Seq())) == (

      Seq(1)))

  }

  test("cons - cons negative number") {

    assert((

      hydra.lib.lists.cons[Int](-1)(Seq(2, 3))) == (

      Seq(-1, 2, 3)))

  }

  test("cons - cons string") {

    assert((

      hydra.lib.lists.cons[scala.Predef.String]("hello")(Seq("world"))) == (

      Seq("hello", "world")))

  }

  // drop

  test("drop - drop from beginning") {

    assert((

      hydra.lib.lists.drop[Int](2)(Seq(1, 2, 3, 4, 5))) == (

      Seq(3, 4, 5)))

  }

  test("drop - drop zero elements") {

    assert((

      hydra.lib.lists.drop[Int](0)(Seq(1, 2, 3))) == (

      Seq(1, 2, 3)))

  }

  test("drop - drop all elements") {

    assert((

      hydra.lib.lists.drop[Int](3)(Seq(1, 2, 3))) == (

      Seq()))

  }

  test("drop - drop more than length") {

    assert((

      hydra.lib.lists.drop[Int](5)(Seq(1, 2))) == (

      Seq()))

  }

  test("drop - drop from empty list") {

    assert((

      hydra.lib.lists.drop(3)(Seq())) == (

      Seq()))

  }

  test("drop - drop negative amount") {

    assert((

      hydra.lib.lists.drop[Int](-1)(Seq(1, 2, 3))) == (

      Seq(1, 2, 3)))

  }

  // dropWhile

  test("dropWhile - drop while less than 3") {

    assert((

      hydra.lib.lists.dropWhile[Int]((x: Int) => hydra.lib.equality.lt[Int](x)(3))(Seq(1, 2, 3, 2, 1))) == (

      Seq(3, 2, 1)))

  }

  test("dropWhile - drop all elements") {

    assert((

      hydra.lib.lists.dropWhile[Int]((x: Int) => hydra.lib.equality.lt[Int](x)(10))(Seq(1, 2, 3))) == (

      Seq()))

  }

  test("dropWhile - drop no elements") {

    assert((

      hydra.lib.lists.dropWhile[Int]((x: Int) => hydra.lib.equality.lt[Int](x)(0))(Seq(1, 2, 3))) == (

      Seq(1, 2, 3)))

  }

  test("dropWhile - empty list") {

    assert((

      hydra.lib.lists.dropWhile[Int]((x: Int) => hydra.lib.equality.lt[Int](x)(5))(Seq())) == (

      Seq()))

  }

  // elem

  test("elem - element present") {

    assert((

      hydra.lib.lists.elem[Int](2)(Seq(1, 2, 3))) == (

      true))

  }

  test("elem - element not present") {

    assert((

      hydra.lib.lists.elem[Int](4)(Seq(1, 2, 3))) == (

      false))

  }

  test("elem - empty list") {

    assert((

      hydra.lib.lists.elem[Int](1)(Seq())) == (

      false))

  }

  test("elem - single element present") {

    assert((

      hydra.lib.lists.elem[Int](1)(Seq(1))) == (

      true))

  }

  test("elem - single element not present") {

    assert((

      hydra.lib.lists.elem[Int](2)(Seq(1))) == (

      false))

  }

  test("elem - duplicate elements") {

    assert((

      hydra.lib.lists.elem[Int](2)(Seq(1, 2, 2, 3))) == (

      true))

  }

  test("elem - string element present") {

    assert((

      hydra.lib.lists.elem[scala.Predef.String]("hello")(Seq("world", "hello", "test"))) == (

      true))

  }

  test("elem - string element not present") {

    assert((

      hydra.lib.lists.elem[scala.Predef.String]("missing")(Seq("world", "hello"))) == (

      false))

  }

  // filter

  test("filter - filter positive numbers") {

    assert((

      hydra.lib.lists.filter[Int]((x: Int) => hydra.lib.equality.gt[Int](x)(0))(Seq(-1, 2, -3, 4, 5))) == (

      Seq(2, 4, 5)))

  }

  test("filter - filter all elements") {

    assert((

      hydra.lib.lists.filter[Int]((x: Int) => hydra.lib.equality.lt[Int](x)(10))(Seq(1, 2, 3))) == (

      Seq(1, 2, 3)))

  }

  test("filter - filter no elements") {

    assert((

      hydra.lib.lists.filter[Int]((x: Int) => hydra.lib.equality.gt[Int](x)(10))(Seq(1, 2, 3))) == (

      Seq()))

  }

  test("filter - empty list") {

    assert((

      hydra.lib.lists.filter[Int]((x: Int) => hydra.lib.equality.gt[Int](x)(0))(Seq())) == (

      Seq()))

  }

  // find

  test("find - find existing element") {

    assert((

      hydra.lib.lists.find[Int]((x: Int) => hydra.lib.equality.gt[Int](x)(3))(Seq(1, 2, 4, 5))) == (

      Some(4)))

  }

  test("find - find first matching") {

    assert((

      hydra.lib.lists.find[Int]((x: Int) => hydra.lib.equality.gt[Int](x)(0))(Seq(1, 2, 3))) == (

      Some(1)))

  }

  test("find - find no match") {

    assert((

      hydra.lib.lists.find[Int]((x: Int) => hydra.lib.equality.gt[Int](x)(10))(Seq(1, 2, 3))) == (

      None))

  }

  test("find - find in empty list") {

    assert((

      hydra.lib.lists.find[Int]((x: Int) => hydra.lib.equality.gt[Int](x)(0))(Seq())) == (

      None))

  }

  test("find - find single element") {

    assert((

      hydra.lib.lists.find[Int]((x: Int) => hydra.lib.equality.equal[Int](x)(42))(Seq(42))) == (

      Some(42)))

  }

  // foldl

  test("foldl - sum with addition") {

    assert((

      hydra.lib.lists.foldl[Int, Int](hydra.lib.math.add)(0)(Seq(1, 2, 3, 4))) == (

      10))

  }

  test("foldl - product with multiplication") {

    assert((

      hydra.lib.lists.foldl[Int, Int](hydra.lib.math.mul)(1)(Seq(2, 3, 4))) == (

      24))

  }

  test("foldl - empty list") {

    assert((

      hydra.lib.lists.foldl[Int, Int](hydra.lib.math.add)(5)(Seq())) == (

      5))

  }

  test("foldl - single element") {

    assert((

      hydra.lib.lists.foldl[Int, Int](hydra.lib.math.add)(10)(Seq(5))) == (

      15))

  }

  test("foldl - subtraction fold") {

    assert((

      hydra.lib.lists.foldl[Int, Int](hydra.lib.math.sub)(10)(Seq(1, 2, 3))) == (

      4))

  }

  // foldr

  test("foldr - subtraction fold right") {

    assert((

      hydra.lib.lists.foldr[Int, Int](hydra.lib.math.sub)(0)(Seq(1, 2, 3))) == (

      2))

  }

  test("foldr - empty list") {

    assert((

      hydra.lib.lists.foldr[Int, Int](hydra.lib.math.add)(5)(Seq())) == (

      5))

  }

  test("foldr - single element") {

    assert((

      hydra.lib.lists.foldr[Int, Int](hydra.lib.math.add)(10)(Seq(5))) == (

      15))

  }

  test("foldr - sum with addition") {

    assert((

      hydra.lib.lists.foldr[Int, Int](hydra.lib.math.add)(0)(Seq(1, 2, 3, 4))) == (

      10))

  }

  test("foldr - subtraction vs foldl") {

    assert((

      hydra.lib.lists.foldr[Int, Int](hydra.lib.math.sub)(10)(Seq(1, 2, 3))) == (

      -8))

  }

  // group

  test("group - consecutive duplicates") {

    assert((

      hydra.lib.lists.group[Int](Seq(1, 1, 2, 2, 2, 3, 1))) == (

      Seq(Seq(1, 1), Seq(2, 2, 2), Seq(3), Seq(1))))

  }

  test("group - no duplicates") {

    assert((

      hydra.lib.lists.group[Int](Seq(1, 2, 3))) == (

      Seq(Seq(1), Seq(2), Seq(3))))

  }

  test("group - all same") {

    assert((

      hydra.lib.lists.group[Int](Seq(1, 1, 1))) == (

      Seq(Seq(1, 1, 1))))

  }

  test("group - empty list") {

    assert((

      hydra.lib.lists.group(Seq())) == (

      Seq()))

  }

  test("group - single element") {

    assert((

      hydra.lib.lists.group[Int](Seq(1))) == (

      Seq(Seq(1))))

  }

  // head

  test("head - three element list") {

    assert((

      hydra.lib.lists.head[Int](Seq(1, 2, 3))) == (

      1))

  }

  test("head - single element list") {

    assert((

      hydra.lib.lists.head[Int](Seq(42))) == (

      42))

  }

  test("head - negative numbers") {

    assert((

      hydra.lib.lists.head[Int](Seq(-1, -2, -3))) == (

      -1))

  }

  test("head - string list") {

    assert((

      hydra.lib.lists.head[scala.Predef.String](Seq("hello", "world"))) == (

      "hello"))

  }

  // init

  test("init - multiple elements") {

    assert((

      hydra.lib.lists.init[Int](Seq(1, 2, 3, 4))) == (

      Seq(1, 2, 3)))

  }

  test("init - two elements") {

    assert((

      hydra.lib.lists.init[Int](Seq(1, 2))) == (

      Seq(1)))

  }

  test("init - single element") {

    assert((

      hydra.lib.lists.init[Int](Seq(1))) == (

      Seq()))

  }

  test("init - string list") {

    assert((

      hydra.lib.lists.init[scala.Predef.String](Seq("a", "b", "c"))) == (

      Seq("a", "b")))

  }

  // intercalate

  test("intercalate - double zero separator") {

    assert((

      hydra.lib.lists.intercalate[Int](Seq(0, 0))(Seq(Seq(1, 2, 3), Seq(4, 5), Seq(6, 7, 8)))) == (

      Seq(1, 2, 3, 0, 0, 4, 5, 0, 0, 6, 7, 8)))

  }

  test("intercalate - empty separator") {

    assert((

      hydra.lib.lists.intercalate[Int](Seq())(Seq(Seq(1, 2), Seq(3, 4)))) == (

      Seq(1, 2, 3, 4)))

  }

  test("intercalate - single element separator") {

    assert((

      hydra.lib.lists.intercalate[Int](Seq(99))(Seq(Seq(1), Seq(2), Seq(3)))) == (

      Seq(1, 99, 2, 99, 3)))

  }

  test("intercalate - empty list of lists") {

    assert((

      hydra.lib.lists.intercalate[Int](Seq(0))(Seq())) == (

      Seq()))

  }

  test("intercalate - single list") {

    assert((

      hydra.lib.lists.intercalate[Int](Seq(0))(Seq(Seq(1, 2, 3)))) == (

      Seq(1, 2, 3)))

  }

  test("intercalate - lists with empty lists") {

    assert((

      hydra.lib.lists.intercalate[Int](Seq(0))(Seq(Seq(), Seq(1), Seq()))) == (

      Seq(0, 1, 0)))

  }

  // intersperse

  test("intersperse - string interspersion") {

    assert((

      hydra.lib.lists.intersperse[scala.Predef.String]("and")(Seq("one", "two", "three"))) == (

      Seq("one", "and", "two", "and", "three")))

  }

  test("intersperse - single element") {

    assert((

      hydra.lib.lists.intersperse[scala.Predef.String]("x")(Seq("only"))) == (

      Seq("only")))

  }

  test("intersperse - empty list") {

    assert((

      hydra.lib.lists.intersperse[scala.Predef.String]("x")(Seq())) == (

      Seq()))

  }

  test("intersperse - two elements") {

    assert((

      hydra.lib.lists.intersperse[scala.Predef.String]("+")(Seq("a", "b"))) == (

      Seq("a", "+", "b")))

  }

  test("intersperse - number interspersion") {

    assert((

      hydra.lib.lists.intersperse[Int](0)(Seq(1, 2, 3))) == (

      Seq(1, 0, 2, 0, 3)))

  }

  // last

  test("last - three element list") {

    assert((

      hydra.lib.lists.last[Int](Seq(1, 2, 3))) == (

      3))

  }

  test("last - single element list") {

    assert((

      hydra.lib.lists.last[Int](Seq(42))) == (

      42))

  }

  test("last - negative numbers") {

    assert((

      hydra.lib.lists.last[Int](Seq(-1, -2, -3))) == (

      -3))

  }

  test("last - string list") {

    assert((

      hydra.lib.lists.last[scala.Predef.String](Seq("hello", "world"))) == (

      "world"))

  }

  // length

  test("length - three elements") {

    assert((

      hydra.lib.lists.length[Int](Seq(1, 2, 3))) == (

      3))

  }

  test("length - empty list") {

    assert((

      hydra.lib.lists.length(Seq())) == (

      0))

  }

  test("length - single element") {

    assert((

      hydra.lib.lists.length[Int](Seq(42))) == (

      1))

  }

  test("length - many elements") {

    assert((

      hydra.lib.lists.length[Int](Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))) == (

      10))

  }

  test("length - string list") {

    assert((

      hydra.lib.lists.length[scala.Predef.String](Seq("a", "b", "c"))) == (

      3))

  }

  // map

  test("map - string to uppercase") {

    assert((

      hydra.lib.lists.map[scala.Predef.String, scala.Predef.String](hydra.lib.strings.toUpper)(Seq("one", "two"))) == (

      Seq("ONE", "TWO")))

  }

  test("map - empty list") {

    assert((

      hydra.lib.lists.map[scala.Predef.String, scala.Predef.String](hydra.lib.strings.toUpper)(Seq())) == (

      Seq()))

  }

  test("map - single element") {

    assert((

      hydra.lib.lists.map[scala.Predef.String, scala.Predef.String](hydra.lib.strings.toUpper)(Seq("hello"))) == (

      Seq("HELLO")))

  }

  test("map - number negation") {

    assert((

      hydra.lib.lists.map[Int, Int](hydra.lib.math.negate)(Seq(1, 2, 3))) == (

      Seq(-1, -2, -3)))

  }

  test("map - identity function") {

    assert((

      hydra.lib.lists.map[Int, Int](hydra.lib.equality.identity[Int])(Seq(1, 2, 3))) == (

      Seq(1, 2, 3)))

  }

  // nub

  test("nub - remove duplicates") {

    assert((

      hydra.lib.lists.nub[Int](Seq(1, 2, 1, 3, 2, 4))) == (

      Seq(1, 2, 3, 4)))

  }

  test("nub - no duplicates") {

    assert((

      hydra.lib.lists.nub[Int](Seq(1, 2, 3))) == (

      Seq(1, 2, 3)))

  }

  test("nub - all duplicates") {

    assert((

      hydra.lib.lists.nub[Int](Seq(1, 1, 1))) == (

      Seq(1)))

  }

  test("nub - empty list") {

    assert((

      hydra.lib.lists.nub(Seq())) == (

      Seq()))

  }

  test("nub - single element") {

    assert((

      hydra.lib.lists.nub[Int](Seq(1))) == (

      Seq(1)))

  }

  test("nub - string duplicates") {

    assert((

      hydra.lib.lists.nub[scala.Predef.String](Seq("a", "b", "a", "c"))) == (

      Seq("a", "b", "c")))

  }

  // null

  test("null - empty int list") {

    assert((

      hydra.lib.lists.`null`(Seq())) == (

      true))

  }

  test("null - single element") {

    assert((

      hydra.lib.lists.`null`[Int](Seq(1))) == (

      false))

  }

  test("null - multiple elements") {

    assert((

      hydra.lib.lists.`null`[Int](Seq(1, 2, 3))) == (

      false))

  }

  test("null - empty string list") {

    assert((

      hydra.lib.lists.`null`(Seq())) == (

      true))

  }

  test("null - non-empty string list") {

    assert((

      hydra.lib.lists.`null`[scala.Predef.String](Seq("a"))) == (

      false))

  }

  // partition

  test("partition - partition greater than 3") {

    assert((

      hydra.lib.lists.partition[Int]((x: Int) => hydra.lib.equality.gt[Int](x)(3))(Seq(1, 2, 3, 4, 5, 6))) == (

      Tuple2(Seq(4, 5, 6), Seq(1, 2, 3))))

  }

  test("partition - partition all elements") {

    assert((

      hydra.lib.lists.partition[Int]((x: Int) => hydra.lib.equality.lt[Int](x)(10))(Seq(1, 2, 3))) == (

      Tuple2(Seq(1, 2, 3), Seq())))

  }

  test("partition - partition no elements") {

    assert((

      hydra.lib.lists.partition[Int]((x: Int) => hydra.lib.equality.gt[Int](x)(10))(Seq(1, 2, 3))) == (

      Tuple2(Seq(), Seq(1, 2, 3))))

  }

  test("partition - partition even numbers") {

    assert((

      hydra.lib.lists.partition[Int]((x: Int) => hydra.lib.math.even(x))(Seq(1, 2, 3, 4, 5, 6))) == (

      Tuple2(Seq(2, 4, 6), Seq(1, 3, 5))))

  }

  test("partition - empty list") {

    assert((

      hydra.lib.lists.partition[Int]((x: Int) => hydra.lib.equality.lt[Int](x)(5))(Seq())) == (

      Tuple2(Seq(), Seq())))

  }

  // pure

  test("pure - string element") {

    assert((

      hydra.lib.lists.pure[scala.Predef.String]("one")) == (

      Seq("one")))

  }

  test("pure - empty string") {

    assert((

      hydra.lib.lists.pure[scala.Predef.String]("")) == (

      Seq("")))

  }

  test("pure - number element") {

    assert((

      hydra.lib.lists.pure[Int](42)) == (

      Seq(42)))

  }

  test("pure - negative number") {

    assert((

      hydra.lib.lists.pure[Int](-5)) == (

      Seq(-5)))

  }

  // replicate

  test("replicate - replicate three times") {

    assert((

      hydra.lib.lists.replicate[Int](3)(42)) == (

      Seq(42, 42, 42)))

  }

  test("replicate - replicate zero times") {

    assert((

      hydra.lib.lists.replicate[Int](0)(1)) == (

      Seq()))

  }

  test("replicate - replicate once") {

    assert((

      hydra.lib.lists.replicate[Int](1)(99)) == (

      Seq(99)))

  }

  test("replicate - replicate string") {

    assert((

      hydra.lib.lists.replicate[scala.Predef.String](2)("hello")) == (

      Seq("hello", "hello")))

  }

  // reverse

  test("reverse - multiple elements") {

    assert((

      hydra.lib.lists.reverse[Int](Seq(1, 2, 3, 4))) == (

      Seq(4, 3, 2, 1)))

  }

  test("reverse - single element") {

    assert((

      hydra.lib.lists.reverse[Int](Seq(1))) == (

      Seq(1)))

  }

  test("reverse - empty list") {

    assert((

      hydra.lib.lists.reverse(Seq())) == (

      Seq()))

  }

  test("reverse - two elements") {

    assert((

      hydra.lib.lists.reverse[Int](Seq(1, 2))) == (

      Seq(2, 1)))

  }

  test("reverse - string list") {

    assert((

      hydra.lib.lists.reverse[scala.Predef.String](Seq("a", "b", "c"))) == (

      Seq("c", "b", "a")))

  }

  // safeHead

  test("safeHead - non-empty int list") {

    assert((

      hydra.lib.lists.safeHead[Int](Seq(1, 2, 3))) == (

      Some(1)))

  }

  test("safeHead - empty int list") {

    assert((

      hydra.lib.lists.safeHead(Seq())) == (

      None))

  }

  test("safeHead - single element") {

    assert((

      hydra.lib.lists.safeHead[Int](Seq(42))) == (

      Some(42)))

  }

  test("safeHead - non-empty string list") {

    assert((

      hydra.lib.lists.safeHead[scala.Predef.String](Seq("hello", "world"))) == (

      Some("hello")))

  }

  test("safeHead - empty string list") {

    assert((

      hydra.lib.lists.safeHead(Seq())) == (

      None))

  }

  // singleton

  test("singleton - number element") {

    assert((

      hydra.lib.lists.singleton[Int](42)) == (

      Seq(42)))

  }

  test("singleton - negative number") {

    assert((

      hydra.lib.lists.singleton[Int](-1)) == (

      Seq(-1)))

  }

  test("singleton - zero") {

    assert((

      hydra.lib.lists.singleton[Int](0)) == (

      Seq(0)))

  }

  test("singleton - string element") {

    assert((

      hydra.lib.lists.singleton[scala.Predef.String]("hello")) == (

      Seq("hello")))

  }

  // sort

  test("sort - unsorted numbers") {

    assert((

      hydra.lib.lists.sort[Int](Seq(3, 1, 4, 1, 5))) == (

      Seq(1, 1, 3, 4, 5)))

  }

  test("sort - already sorted") {

    assert((

      hydra.lib.lists.sort[Int](Seq(1, 2, 3))) == (

      Seq(1, 2, 3)))

  }

  test("sort - reverse sorted") {

    assert((

      hydra.lib.lists.sort[Int](Seq(3, 2, 1))) == (

      Seq(1, 2, 3)))

  }

  test("sort - single element") {

    assert((

      hydra.lib.lists.sort[Int](Seq(1))) == (

      Seq(1)))

  }

  test("sort - empty list") {

    assert((

      hydra.lib.lists.sort(Seq())) == (

      Seq()))

  }

  test("sort - duplicates") {

    assert((

      hydra.lib.lists.sort[Int](Seq(2, 1, 2, 3, 1))) == (

      Seq(1, 1, 2, 2, 3)))

  }

  test("sort - string sort") {

    assert((

      hydra.lib.lists.sort[scala.Predef.String](Seq("zebra", "apple", "banana"))) == (

      Seq("apple", "banana", "zebra")))

  }

  // sortOn

  test("sortOn - sort by string length") {

    assert((

      hydra.lib.lists.sortOn[scala.Predef.String, Int](hydra.lib.strings.length)(Seq("hello", "hi", "world"))) == (

      Seq("hi", "hello", "world")))

  }

  test("sortOn - empty string list") {

    assert((

      hydra.lib.lists.sortOn[scala.Predef.String, Int](hydra.lib.strings.length)(Seq())) == (

      Seq()))

  }

  test("sortOn - single string element") {

    assert((

      hydra.lib.lists.sortOn[scala.Predef.String, Int](hydra.lib.strings.length)(Seq("test"))) == (

      Seq("test")))

  }

  test("sortOn - sort by negation") {

    assert((

      hydra.lib.lists.sortOn[Int, Int](hydra.lib.math.negate)(Seq(1, 3, 2))) == (

      Seq(3, 2, 1)))

  }

  test("sortOn - sort by absolute value") {

    assert((

      hydra.lib.lists.sortOn[Int, Int](hydra.lib.math.abs)(Seq(-1, -3, 2))) == (

      Seq(-1, 2, -3)))

  }

  // span

  test("span - span less than 3") {

    assert((

      hydra.lib.lists.span[Int]((x: Int) => hydra.lib.equality.lt[Int](x)(3))(Seq(1, 2, 3, 1, 2))) == (

      Tuple2(Seq(1, 2), Seq(3, 1, 2))))

  }

  test("span - span all elements") {

    assert((

      hydra.lib.lists.span[Int]((x: Int) => hydra.lib.equality.lt[Int](x)(10))(Seq(1, 2, 3))) == (

      Tuple2(Seq(1, 2, 3), Seq())))

  }

  test("span - span no elements") {

    assert((

      hydra.lib.lists.span[Int]((x: Int) => hydra.lib.equality.gt[Int](x)(10))(Seq(1, 2, 3))) == (

      Tuple2(Seq(), Seq(1, 2, 3))))

  }

  test("span - empty list") {

    assert((

      hydra.lib.lists.span[Int]((x: Int) => hydra.lib.equality.lt[Int](x)(5))(Seq())) == (

      Tuple2(Seq(), Seq())))

  }

  // tail

  test("tail - multiple elements") {

    assert((

      hydra.lib.lists.tail[Int](Seq(1, 2, 3, 4))) == (

      Seq(2, 3, 4)))

  }

  test("tail - two elements") {

    assert((

      hydra.lib.lists.tail[Int](Seq(1, 2))) == (

      Seq(2)))

  }

  test("tail - single element") {

    assert((

      hydra.lib.lists.tail[Int](Seq(1))) == (

      Seq()))

  }

  test("tail - string list") {

    assert((

      hydra.lib.lists.tail[scala.Predef.String](Seq("a", "b", "c"))) == (

      Seq("b", "c")))

  }

  // take

  test("take - take from beginning") {

    assert((

      hydra.lib.lists.take[Int](2)(Seq(1, 2, 3, 4, 5))) == (

      Seq(1, 2)))

  }

  test("take - take zero elements") {

    assert((

      hydra.lib.lists.take[Int](0)(Seq(1, 2, 3))) == (

      Seq()))

  }

  test("take - take all elements") {

    assert((

      hydra.lib.lists.take[Int](3)(Seq(1, 2, 3))) == (

      Seq(1, 2, 3)))

  }

  test("take - take more than length") {

    assert((

      hydra.lib.lists.take[Int](5)(Seq(1, 2))) == (

      Seq(1, 2)))

  }

  test("take - take from empty list") {

    assert((

      hydra.lib.lists.take(3)(Seq())) == (

      Seq()))

  }

  test("take - take negative amount") {

    assert((

      hydra.lib.lists.take[Int](-1)(Seq(1, 2, 3))) == (

      Seq()))

  }

  // transpose

  test("transpose - square matrix") {

    assert((

      hydra.lib.lists.transpose[Int](Seq(Seq(1, 2, 3), Seq(4, 5, 6)))) == (

      Seq(Seq(1, 4), Seq(2, 5), Seq(3, 6))))

  }

  test("transpose - empty lists") {

    assert((

      hydra.lib.lists.transpose(Seq())) == (

      Seq()))

  }

  test("transpose - single row") {

    assert((

      hydra.lib.lists.transpose[Int](Seq(Seq(1, 2, 3)))) == (

      Seq(Seq(1), Seq(2), Seq(3))))

  }

  test("transpose - single column") {

    assert((

      hydra.lib.lists.transpose[Int](Seq(Seq(1), Seq(2), Seq(3)))) == (

      Seq(Seq(1, 2, 3))))

  }

  test("transpose - ragged matrix") {

    assert((

      hydra.lib.lists.transpose[Int](Seq(Seq(1, 2), Seq(3), Seq(4, 5, 6)))) == (

      Seq(Seq(1, 3, 4), Seq(2, 5), Seq(6))))

  }

  // zip

  test("zip - equal length lists") {

    assert((

      hydra.lib.lists.zip[Int, scala.Predef.String](Seq(1, 2, 3))(Seq("a", "b", "c"))) == (

      Seq(Tuple2(1, "a"), Tuple2(2, "b"), Tuple2(3, "c"))))

  }

  test("zip - first list shorter") {

    assert((

      hydra.lib.lists.zip[Int, scala.Predef.String](Seq(1, 2))(Seq("a", "b", "c"))) == (

      Seq(Tuple2(1, "a"), Tuple2(2, "b"))))

  }

  test("zip - second list shorter") {

    assert((

      hydra.lib.lists.zip[Int, scala.Predef.String](Seq(1, 2, 3))(Seq("a", "b"))) == (

      Seq(Tuple2(1, "a"), Tuple2(2, "b"))))

  }

  test("zip - empty first list") {

    assert((

      hydra.lib.lists.zip(Seq())(Seq("a", "b"))) == (

      Seq()))

  }

  test("zip - empty second list") {

    assert((

      hydra.lib.lists.zip(Seq(1, 2))(Seq())) == (

      Seq()))

  }

  test("zip - both empty lists") {

    assert((

      hydra.lib.lists.zip(Seq())(Seq())) == (

      Seq()))

  }

  // zipWith

  test("zipWith - addition") {

    assert((

      hydra.lib.lists.zipWith[Int, Int, Int](hydra.lib.math.add)(Seq(1, 2, 3))(Seq(4, 5, 6))) == (

      Seq(5, 7, 9)))

  }

  test("zipWith - first list shorter") {

    assert((

      hydra.lib.lists.zipWith[Int, Int, Int](hydra.lib.math.add)(Seq(1, 2))(Seq(4, 5, 6))) == (

      Seq(5, 7)))

  }

  test("zipWith - second list shorter") {

    assert((

      hydra.lib.lists.zipWith[Int, Int, Int](hydra.lib.math.add)(Seq(1, 2, 3))(Seq(4, 5))) == (

      Seq(5, 7)))

  }

  test("zipWith - empty first list") {

    assert((

      hydra.lib.lists.zipWith[Int, Int, Int](hydra.lib.math.add)(Seq())(Seq(1, 2, 3))) == (

      Seq()))

  }

  test("zipWith - empty second list") {

    assert((

      hydra.lib.lists.zipWith[Int, Int, Int](hydra.lib.math.add)(Seq(1, 2, 3))(Seq())) == (

      Seq()))

  }

  test("zipWith - string concatenation") {

    assert((

      hydra.lib.lists.zipWith[scala.Predef.String, scala.Predef.String, scala.Predef.String](hydra.lib.strings.cat2)(Seq("a", "b"))(Seq("1", "2"))) == (

      Seq("a1", "b2")))

  }
}
