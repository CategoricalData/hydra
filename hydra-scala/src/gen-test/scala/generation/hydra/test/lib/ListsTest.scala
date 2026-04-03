// Note: this is an automatically generated file. Do not edit.
// hydra.lib.lists primitives

package generation.hydra.test.lib

import org.scalatest.funsuite.AnyFunSuite

class ListsTest extends AnyFunSuite {

  // apply

  // string transformations

  test("apply - string transformations - string transformations") {

    assert((

      ["ONE", "TWO", "THREE", "one", "two", "three"]) == (

      ["ONE", "TWO", "THREE", "one", "two", "three"]))

  }

  // edge cases

  test("apply - edge cases - empty function list") {

    assert((

      []) == (

      []))

  }

  test("apply - edge cases - empty input list") {

    assert((

      []) == (

      []))

  }

  test("apply - edge cases - single function") {

    assert((

      ["HELLO"]) == (

      ["HELLO"]))

  }

  test("apply - edge cases - single input") {

    assert((

      ["TEST", "test"]) == (

      ["TEST", "test"]))

  }

  // at

  test("at - first element") {

    assert((

      1:int32) == (

      1:int32))

  }

  test("at - middle element") {

    assert((

      2:int32) == (

      2:int32))

  }

  test("at - last element") {

    assert((

      3:int32) == (

      3:int32))

  }

  test("at - single element list") {

    assert((

      42:int32) == (

      42:int32))

  }

  test("at - string list access") {

    assert((

      "world") == (

      "world"))

  }

  // bind

  test("bind - negation function") {

    assert((

      [-1:int32, -2:int32, -3:int32, -4:int32]) == (

      [-1:int32, -2:int32, -3:int32, -4:int32]))

  }

  test("bind - empty list") {

    assert((

      []) == (

      []))

  }

  test("bind - single element") {

    assert((

      [-5:int32]) == (

      [-5:int32]))

  }

  test("bind - duplicate elements") {

    assert((

      [-1:int32, -1:int32, -2:int32]) == (

      [-1:int32, -1:int32, -2:int32]))

  }

  // concat

  test("concat - multiple non-empty lists") {

    assert((

      [1:int32, 2:int32, 3:int32, 4:int32, 5:int32, 6:int32, 7:int32, 8:int32]) == (

      [1:int32, 2:int32, 3:int32, 4:int32, 5:int32, 6:int32, 7:int32, 8:int32]))

  }

  test("concat - empty lists included") {

    assert((

      [1:int32, 2:int32, 3:int32]) == (

      [1:int32, 2:int32, 3:int32]))

  }

  test("concat - single list") {

    assert((

      [1:int32, 2:int32, 3:int32]) == (

      [1:int32, 2:int32, 3:int32]))

  }

  test("concat - all empty lists") {

    assert((

      []) == (

      []))

  }

  test("concat - empty list of lists") {

    assert((

      []) == (

      []))

  }

  // concat2

  test("concat2 - two non-empty lists") {

    assert((

      [1:int32, 2:int32, 3:int32, 4:int32]) == (

      [1:int32, 2:int32, 3:int32, 4:int32]))

  }

  test("concat2 - first list empty") {

    assert((

      [1:int32, 2:int32]) == (

      [1:int32, 2:int32]))

  }

  test("concat2 - second list empty") {

    assert((

      [1:int32, 2:int32]) == (

      [1:int32, 2:int32]))

  }

  test("concat2 - both lists empty") {

    assert((

      []) == (

      []))

  }

  test("concat2 - single elements") {

    assert((

      [1:int32, 2:int32]) == (

      [1:int32, 2:int32]))

  }

  test("concat2 - string lists") {

    assert((

      ["a", "b", "c", "d"]) == (

      ["a", "b", "c", "d"]))

  }

  // cons

  test("cons - cons to non-empty list") {

    assert((

      [1:int32, 2:int32, 3:int32]) == (

      [1:int32, 2:int32, 3:int32]))

  }

  test("cons - cons to empty list") {

    assert((

      [1:int32]) == (

      [1:int32]))

  }

  test("cons - cons negative number") {

    assert((

      [-1:int32, 2:int32, 3:int32]) == (

      [-1:int32, 2:int32, 3:int32]))

  }

  test("cons - cons string") {

    assert((

      ["hello", "world"]) == (

      ["hello", "world"]))

  }

  // drop

  test("drop - drop from beginning") {

    assert((

      [3:int32, 4:int32, 5:int32]) == (

      [3:int32, 4:int32, 5:int32]))

  }

  test("drop - drop zero elements") {

    assert((

      [1:int32, 2:int32, 3:int32]) == (

      [1:int32, 2:int32, 3:int32]))

  }

  test("drop - drop all elements") {

    assert((

      []) == (

      []))

  }

  test("drop - drop more than length") {

    assert((

      []) == (

      []))

  }

  test("drop - drop from empty list") {

    assert((

      []) == (

      []))

  }

  test("drop - drop negative amount") {

    assert((

      [1:int32, 2:int32, 3:int32]) == (

      [1:int32, 2:int32, 3:int32]))

  }

  // dropWhile

  test("dropWhile - drop while less than 3") {

    assert((

      [3:int32, 2:int32, 1:int32]) == (

      [3:int32, 2:int32, 1:int32]))

  }

  test("dropWhile - drop all elements") {

    assert((

      []) == (

      []))

  }

  test("dropWhile - drop no elements") {

    assert((

      [1:int32, 2:int32, 3:int32]) == (

      [1:int32, 2:int32, 3:int32]))

  }

  test("dropWhile - empty list") {

    assert((

      []) == (

      []))

  }

  // elem

  test("elem - element present") {

    assert((

      true) == (

      true))

  }

  test("elem - element not present") {

    assert((

      false) == (

      false))

  }

  test("elem - empty list") {

    assert((

      false) == (

      false))

  }

  test("elem - single element present") {

    assert((

      true) == (

      true))

  }

  test("elem - single element not present") {

    assert((

      false) == (

      false))

  }

  test("elem - duplicate elements") {

    assert((

      true) == (

      true))

  }

  test("elem - string element present") {

    assert((

      true) == (

      true))

  }

  test("elem - string element not present") {

    assert((

      false) == (

      false))

  }

  // filter

  test("filter - filter positive numbers") {

    assert((

      [2:int32, 4:int32, 5:int32]) == (

      [2:int32, 4:int32, 5:int32]))

  }

  test("filter - filter all elements") {

    assert((

      [1:int32, 2:int32, 3:int32]) == (

      [1:int32, 2:int32, 3:int32]))

  }

  test("filter - filter no elements") {

    assert((

      []) == (

      []))

  }

  test("filter - empty list") {

    assert((

      []) == (

      []))

  }

  // find

  test("find - find existing element") {

    assert((

      just(4:int32)) == (

      just(4:int32)))

  }

  test("find - find first matching") {

    assert((

      just(1:int32)) == (

      just(1:int32)))

  }

  test("find - find no match") {

    assert((

      nothing) == (

      nothing))

  }

  test("find - find in empty list") {

    assert((

      nothing) == (

      nothing))

  }

  test("find - find single element") {

    assert((

      just(42:int32)) == (

      just(42:int32)))

  }

  // foldl

  test("foldl - sum with addition") {

    assert((

      10:int32) == (

      10:int32))

  }

  test("foldl - product with multiplication") {

    assert((

      24:int32) == (

      24:int32))

  }

  test("foldl - empty list") {

    assert((

      5:int32) == (

      5:int32))

  }

  test("foldl - single element") {

    assert((

      15:int32) == (

      15:int32))

  }

  test("foldl - subtraction fold") {

    assert((

      4:int32) == (

      4:int32))

  }

  // foldr

  test("foldr - subtraction fold right") {

    assert((

      2:int32) == (

      2:int32))

  }

  test("foldr - empty list") {

    assert((

      5:int32) == (

      5:int32))

  }

  test("foldr - single element") {

    assert((

      15:int32) == (

      15:int32))

  }

  test("foldr - sum with addition") {

    assert((

      10:int32) == (

      10:int32))

  }

  test("foldr - subtraction vs foldl") {

    assert((

      -8:int32) == (

      -8:int32))

  }

  // group

  test("group - consecutive duplicates") {

    assert((

      [[1:int32, 1:int32], [2:int32, 2:int32, 2:int32], [3:int32], [1:int32]]) == (

      [[1:int32, 1:int32], [2:int32, 2:int32, 2:int32], [3:int32], [1:int32]]))

  }

  test("group - no duplicates") {

    assert((

      [[1:int32], [2:int32], [3:int32]]) == (

      [[1:int32], [2:int32], [3:int32]]))

  }

  test("group - all same") {

    assert((

      [[1:int32, 1:int32, 1:int32]]) == (

      [[1:int32, 1:int32, 1:int32]]))

  }

  test("group - empty list") {

    assert((

      []) == (

      []))

  }

  test("group - single element") {

    assert((

      [[1:int32]]) == (

      [[1:int32]]))

  }

  // head

  test("head - three element list") {

    assert((

      1:int32) == (

      1:int32))

  }

  test("head - single element list") {

    assert((

      42:int32) == (

      42:int32))

  }

  test("head - negative numbers") {

    assert((

      -1:int32) == (

      -1:int32))

  }

  test("head - string list") {

    assert((

      "hello") == (

      "hello"))

  }

  // init

  test("init - multiple elements") {

    assert((

      [1:int32, 2:int32, 3:int32]) == (

      [1:int32, 2:int32, 3:int32]))

  }

  test("init - two elements") {

    assert((

      [1:int32]) == (

      [1:int32]))

  }

  test("init - single element") {

    assert((

      []) == (

      []))

  }

  test("init - string list") {

    assert((

      ["a", "b"]) == (

      ["a", "b"]))

  }

  // intercalate

  test("intercalate - double zero separator") {

    assert((

      [1:int32, 2:int32, 3:int32, 0:int32, 0:int32, 4:int32, 5:int32, 0:int32, 0:int32, 6:int32, 7:int32, 8:int32]) == (

      [1:int32, 2:int32, 3:int32, 0:int32, 0:int32, 4:int32, 5:int32, 0:int32, 0:int32, 6:int32, 7:int32, 8:int32]))

  }

  test("intercalate - empty separator") {

    assert((

      [1:int32, 2:int32, 3:int32, 4:int32]) == (

      [1:int32, 2:int32, 3:int32, 4:int32]))

  }

  test("intercalate - single element separator") {

    assert((

      [1:int32, 99:int32, 2:int32, 99:int32, 3:int32]) == (

      [1:int32, 99:int32, 2:int32, 99:int32, 3:int32]))

  }

  test("intercalate - empty list of lists") {

    assert((

      []) == (

      []))

  }

  test("intercalate - single list") {

    assert((

      [1:int32, 2:int32, 3:int32]) == (

      [1:int32, 2:int32, 3:int32]))

  }

  test("intercalate - lists with empty lists") {

    assert((

      [0:int32, 1:int32, 0:int32]) == (

      [0:int32, 1:int32, 0:int32]))

  }

  // intersperse

  test("intersperse - string interspersion") {

    assert((

      ["one", "and", "two", "and", "three"]) == (

      ["one", "and", "two", "and", "three"]))

  }

  test("intersperse - single element") {

    assert((

      ["only"]) == (

      ["only"]))

  }

  test("intersperse - empty list") {

    assert((

      []) == (

      []))

  }

  test("intersperse - two elements") {

    assert((

      ["a", "+", "b"]) == (

      ["a", "+", "b"]))

  }

  test("intersperse - number interspersion") {

    assert((

      [1:int32, 0:int32, 2:int32, 0:int32, 3:int32]) == (

      [1:int32, 0:int32, 2:int32, 0:int32, 3:int32]))

  }

  // last

  test("last - three element list") {

    assert((

      3:int32) == (

      3:int32))

  }

  test("last - single element list") {

    assert((

      42:int32) == (

      42:int32))

  }

  test("last - negative numbers") {

    assert((

      -3:int32) == (

      -3:int32))

  }

  test("last - string list") {

    assert((

      "world") == (

      "world"))

  }

  // length

  test("length - three elements") {

    assert((

      3:int32) == (

      3:int32))

  }

  test("length - empty list") {

    assert((

      0:int32) == (

      0:int32))

  }

  test("length - single element") {

    assert((

      1:int32) == (

      1:int32))

  }

  test("length - many elements") {

    assert((

      10:int32) == (

      10:int32))

  }

  test("length - string list") {

    assert((

      3:int32) == (

      3:int32))

  }

  // map

  test("map - string to uppercase") {

    assert((

      ["ONE", "TWO"]) == (

      ["ONE", "TWO"]))

  }

  test("map - empty list") {

    assert((

      []) == (

      []))

  }

  test("map - single element") {

    assert((

      ["HELLO"]) == (

      ["HELLO"]))

  }

  test("map - number negation") {

    assert((

      [-1:int32, -2:int32, -3:int32]) == (

      [-1:int32, -2:int32, -3:int32]))

  }

  test("map - identity function") {

    assert((

      [1:int32, 2:int32, 3:int32]) == (

      [1:int32, 2:int32, 3:int32]))

  }

  // maybeAt

  test("maybeAt - valid index") {

    assert((

      just(20:int32)) == (

      just(20:int32)))

  }

  test("maybeAt - first element") {

    assert((

      just(10:int32)) == (

      just(10:int32)))

  }

  test("maybeAt - last element") {

    assert((

      just(30:int32)) == (

      just(30:int32)))

  }

  test("maybeAt - out of bounds") {

    assert((

      nothing) == (

      nothing))

  }

  test("maybeAt - negative index") {

    assert((

      nothing) == (

      nothing))

  }

  test("maybeAt - empty list") {

    assert((

      nothing) == (

      nothing))

  }

  // maybeHead

  test("maybeHead - non-empty int list") {

    assert((

      just(1:int32)) == (

      just(1:int32)))

  }

  test("maybeHead - empty int list") {

    assert((

      nothing) == (

      nothing))

  }

  test("maybeHead - single element") {

    assert((

      just(42:int32)) == (

      just(42:int32)))

  }

  test("maybeHead - non-empty string list") {

    assert((

      just("hello")) == (

      just("hello")))

  }

  test("maybeHead - empty string list") {

    assert((

      nothing) == (

      nothing))

  }

  // maybeInit

  test("maybeInit - three elements") {

    assert((

      just([1:int32, 2:int32])) == (

      just([1:int32, 2:int32])))

  }

  test("maybeInit - single element") {

    assert((

      just([])) == (

      just([])))

  }

  test("maybeInit - empty list") {

    assert((

      nothing) == (

      nothing))

  }

  // maybeLast

  test("maybeLast - three elements") {

    assert((

      just(3:int32)) == (

      just(3:int32)))

  }

  test("maybeLast - single element") {

    assert((

      just(42:int32)) == (

      just(42:int32)))

  }

  test("maybeLast - empty list") {

    assert((

      nothing) == (

      nothing))

  }

  // maybeTail

  test("maybeTail - three elements") {

    assert((

      just([2:int32, 3:int32])) == (

      just([2:int32, 3:int32])))

  }

  test("maybeTail - single element") {

    assert((

      just([])) == (

      just([])))

  }

  test("maybeTail - empty list") {

    assert((

      nothing) == (

      nothing))

  }

  // nub

  test("nub - remove duplicates") {

    assert((

      [1:int32, 2:int32, 3:int32, 4:int32]) == (

      [1:int32, 2:int32, 3:int32, 4:int32]))

  }

  test("nub - no duplicates") {

    assert((

      [1:int32, 2:int32, 3:int32]) == (

      [1:int32, 2:int32, 3:int32]))

  }

  test("nub - all duplicates") {

    assert((

      [1:int32]) == (

      [1:int32]))

  }

  test("nub - empty list") {

    assert((

      []) == (

      []))

  }

  test("nub - single element") {

    assert((

      [1:int32]) == (

      [1:int32]))

  }

  test("nub - string duplicates") {

    assert((

      ["a", "b", "c"]) == (

      ["a", "b", "c"]))

  }

  // null

  test("null - empty int list") {

    assert((

      true) == (

      true))

  }

  test("null - single element") {

    assert((

      false) == (

      false))

  }

  test("null - multiple elements") {

    assert((

      false) == (

      false))

  }

  test("null - empty string list") {

    assert((

      true) == (

      true))

  }

  test("null - non-empty string list") {

    assert((

      false) == (

      false))

  }

  // partition

  test("partition - partition greater than 3") {

    assert((

      ([4:int32, 5:int32, 6:int32], [1:int32, 2:int32, 3:int32])) == (

      ([4:int32, 5:int32, 6:int32], [1:int32, 2:int32, 3:int32])))

  }

  test("partition - partition all elements") {

    assert((

      ([1:int32, 2:int32, 3:int32], [])) == (

      ([1:int32, 2:int32, 3:int32], [])))

  }

  test("partition - partition no elements") {

    assert((

      ([], [1:int32, 2:int32, 3:int32])) == (

      ([], [1:int32, 2:int32, 3:int32])))

  }

  test("partition - partition even numbers") {

    assert((

      ([2:int32, 4:int32, 6:int32], [1:int32, 3:int32, 5:int32])) == (

      ([2:int32, 4:int32, 6:int32], [1:int32, 3:int32, 5:int32])))

  }

  test("partition - empty list") {

    assert((

      ([], [])) == (

      ([], [])))

  }

  // pure

  test("pure - string element") {

    assert((

      ["one"]) == (

      ["one"]))

  }

  test("pure - empty string") {

    assert((

      [""]) == (

      [""]))

  }

  test("pure - number element") {

    assert((

      [42:int32]) == (

      [42:int32]))

  }

  test("pure - negative number") {

    assert((

      [-5:int32]) == (

      [-5:int32]))

  }

  // replicate

  test("replicate - replicate three times") {

    assert((

      [42:int32, 42:int32, 42:int32]) == (

      [42:int32, 42:int32, 42:int32]))

  }

  test("replicate - replicate zero times") {

    assert((

      []) == (

      []))

  }

  test("replicate - replicate once") {

    assert((

      [99:int32]) == (

      [99:int32]))

  }

  test("replicate - replicate string") {

    assert((

      ["hello", "hello"]) == (

      ["hello", "hello"]))

  }

  // reverse

  test("reverse - multiple elements") {

    assert((

      [4:int32, 3:int32, 2:int32, 1:int32]) == (

      [4:int32, 3:int32, 2:int32, 1:int32]))

  }

  test("reverse - single element") {

    assert((

      [1:int32]) == (

      [1:int32]))

  }

  test("reverse - empty list") {

    assert((

      []) == (

      []))

  }

  test("reverse - two elements") {

    assert((

      [2:int32, 1:int32]) == (

      [2:int32, 1:int32]))

  }

  test("reverse - string list") {

    assert((

      ["c", "b", "a"]) == (

      ["c", "b", "a"]))

  }

  // safeHead

  test("safeHead - non-empty int list") {

    assert((

      just(1:int32)) == (

      just(1:int32)))

  }

  test("safeHead - empty int list") {

    assert((

      nothing) == (

      nothing))

  }

  test("safeHead - single element") {

    assert((

      just(42:int32)) == (

      just(42:int32)))

  }

  test("safeHead - non-empty string list") {

    assert((

      just("hello")) == (

      just("hello")))

  }

  test("safeHead - empty string list") {

    assert((

      nothing) == (

      nothing))

  }

  // singleton

  test("singleton - number element") {

    assert((

      [42:int32]) == (

      [42:int32]))

  }

  test("singleton - negative number") {

    assert((

      [-1:int32]) == (

      [-1:int32]))

  }

  test("singleton - zero") {

    assert((

      [0:int32]) == (

      [0:int32]))

  }

  test("singleton - string element") {

    assert((

      ["hello"]) == (

      ["hello"]))

  }

  // sort

  test("sort - unsorted numbers") {

    assert((

      [1:int32, 1:int32, 3:int32, 4:int32, 5:int32]) == (

      [1:int32, 1:int32, 3:int32, 4:int32, 5:int32]))

  }

  test("sort - already sorted") {

    assert((

      [1:int32, 2:int32, 3:int32]) == (

      [1:int32, 2:int32, 3:int32]))

  }

  test("sort - reverse sorted") {

    assert((

      [1:int32, 2:int32, 3:int32]) == (

      [1:int32, 2:int32, 3:int32]))

  }

  test("sort - single element") {

    assert((

      [1:int32]) == (

      [1:int32]))

  }

  test("sort - empty list") {

    assert((

      []) == (

      []))

  }

  test("sort - duplicates") {

    assert((

      [1:int32, 1:int32, 2:int32, 2:int32, 3:int32]) == (

      [1:int32, 1:int32, 2:int32, 2:int32, 3:int32]))

  }

  test("sort - string sort") {

    assert((

      ["apple", "banana", "zebra"]) == (

      ["apple", "banana", "zebra"]))

  }

  // sortOn

  test("sortOn - sort by string length") {

    assert((

      ["hi", "hello", "world"]) == (

      ["hi", "hello", "world"]))

  }

  test("sortOn - empty string list") {

    assert((

      []) == (

      []))

  }

  test("sortOn - single string element") {

    assert((

      ["test"]) == (

      ["test"]))

  }

  test("sortOn - sort by negation") {

    assert((

      [3:int32, 2:int32, 1:int32]) == (

      [3:int32, 2:int32, 1:int32]))

  }

  test("sortOn - sort by absolute value") {

    assert((

      [-1:int32, 2:int32, -3:int32]) == (

      [-1:int32, 2:int32, -3:int32]))

  }

  // span

  test("span - span less than 3") {

    assert((

      ([1:int32, 2:int32], [3:int32, 1:int32, 2:int32])) == (

      ([1:int32, 2:int32], [3:int32, 1:int32, 2:int32])))

  }

  test("span - span all elements") {

    assert((

      ([1:int32, 2:int32, 3:int32], [])) == (

      ([1:int32, 2:int32, 3:int32], [])))

  }

  test("span - span no elements") {

    assert((

      ([], [1:int32, 2:int32, 3:int32])) == (

      ([], [1:int32, 2:int32, 3:int32])))

  }

  test("span - empty list") {

    assert((

      ([], [])) == (

      ([], [])))

  }

  // tail

  test("tail - multiple elements") {

    assert((

      [2:int32, 3:int32, 4:int32]) == (

      [2:int32, 3:int32, 4:int32]))

  }

  test("tail - two elements") {

    assert((

      [2:int32]) == (

      [2:int32]))

  }

  test("tail - single element") {

    assert((

      []) == (

      []))

  }

  test("tail - string list") {

    assert((

      ["b", "c"]) == (

      ["b", "c"]))

  }

  // take

  test("take - take from beginning") {

    assert((

      [1:int32, 2:int32]) == (

      [1:int32, 2:int32]))

  }

  test("take - take zero elements") {

    assert((

      []) == (

      []))

  }

  test("take - take all elements") {

    assert((

      [1:int32, 2:int32, 3:int32]) == (

      [1:int32, 2:int32, 3:int32]))

  }

  test("take - take more than length") {

    assert((

      [1:int32, 2:int32]) == (

      [1:int32, 2:int32]))

  }

  test("take - take from empty list") {

    assert((

      []) == (

      []))

  }

  test("take - take negative amount") {

    assert((

      []) == (

      []))

  }

  // transpose

  test("transpose - square matrix") {

    assert((

      [[1:int32, 4:int32], [2:int32, 5:int32], [3:int32, 6:int32]]) == (

      [[1:int32, 4:int32], [2:int32, 5:int32], [3:int32, 6:int32]]))

  }

  test("transpose - empty lists") {

    assert((

      []) == (

      []))

  }

  test("transpose - single row") {

    assert((

      [[1:int32], [2:int32], [3:int32]]) == (

      [[1:int32], [2:int32], [3:int32]]))

  }

  test("transpose - single column") {

    assert((

      [[1:int32, 2:int32, 3:int32]]) == (

      [[1:int32, 2:int32, 3:int32]]))

  }

  test("transpose - ragged matrix") {

    assert((

      [[1:int32, 3:int32, 4:int32], [2:int32, 5:int32], [6:int32]]) == (

      [[1:int32, 3:int32, 4:int32], [2:int32, 5:int32], [6:int32]]))

  }

  // zip

  test("zip - equal length lists") {

    assert((

      [(1:int32, "a"), (2:int32, "b"), (3:int32, "c")]) == (

      [(1:int32, "a"), (2:int32, "b"), (3:int32, "c")]))

  }

  test("zip - first list shorter") {

    assert((

      [(1:int32, "a"), (2:int32, "b")]) == (

      [(1:int32, "a"), (2:int32, "b")]))

  }

  test("zip - second list shorter") {

    assert((

      [(1:int32, "a"), (2:int32, "b")]) == (

      [(1:int32, "a"), (2:int32, "b")]))

  }

  test("zip - empty first list") {

    assert((

      []) == (

      []))

  }

  test("zip - empty second list") {

    assert((

      []) == (

      []))

  }

  test("zip - both empty lists") {

    assert((

      []) == (

      []))

  }

  // zipWith

  test("zipWith - addition") {

    assert((

      [5:int32, 7:int32, 9:int32]) == (

      [5:int32, 7:int32, 9:int32]))

  }

  test("zipWith - first list shorter") {

    assert((

      [5:int32, 7:int32]) == (

      [5:int32, 7:int32]))

  }

  test("zipWith - second list shorter") {

    assert((

      [5:int32, 7:int32]) == (

      [5:int32, 7:int32]))

  }

  test("zipWith - empty first list") {

    assert((

      []) == (

      []))

  }

  test("zipWith - empty second list") {

    assert((

      []) == (

      []))

  }

  test("zipWith - string concatenation") {

    assert((

      ["a1", "b2"]) == (

      ["a1", "b2"]))

  }
}
