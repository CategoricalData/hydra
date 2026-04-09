// Note: this is an automatically generated file. Do not edit.
// sorting

package generation.hydra.test

import org.scalatest.funsuite.AnyFunSuite

class SortingTest extends AnyFunSuite {

  // topological sort

  test("topological sort - empty set") {

    assert((

      right([])) == (

      right([])))

  }

  test("topological sort - singleton set") {

    assert((

      right([1])) == (

      right([1])))

  }

  test("topological sort - discrete set with multiple elements") {

    assert((

      right([1, 2, 3])) == (

      right([1, 2, 3])))

  }

  test("topological sort - linked list") {

    assert((

      right([1, 3, 2])) == (

      right([1, 3, 2])))

  }

  test("topological sort - binary tree") {

    assert((

      right([5, 1, 2, 6, 4, 3])) == (

      right([5, 1, 2, 6, 4, 3])))

  }

  test("topological sort - two trees") {

    assert((

      right([1, 7, 2, 4, 3, 6, 5])) == (

      right([1, 7, 2, 4, 3, 6, 5])))

  }

  test("topological sort - diamond DAG") {

    assert((

      right([5, 2, 3, 4, 1])) == (

      right([5, 2, 3, 4, 1])))

  }

  test("topological sort - two-node cycle") {

    assert((

      left([[1, 2]])) == (

      left([[1, 2]])))

  }

  test("topological sort - cycle with incoming and outgoing edges") {

    assert((

      left([[2, 3]])) == (

      left([[2, 3]])))

  }

  // topological sort SCC

  test("topological sort SCC - empty set") {

    assert((

      []) == (

      []))

  }

  test("topological sort SCC - singleton set") {

    assert((

      [[1]]) == (

      [[1]]))

  }

  test("topological sort SCC - discrete set with multiple elements") {

    assert((

      [[1], [2], [3]]) == (

      [[1], [2], [3]]))

  }

  test("topological sort SCC - single two-element component #1") {

    assert((

      [[2], [1]]) == (

      [[2], [1]]))

  }

  test("topological sort SCC - single two-element component #2") {

    assert((

      [[1], [2]]) == (

      [[1], [2]]))

  }

  test("topological sort SCC - multiple-element component") {

    assert((

      [[3], [1], [2]]) == (

      [[3], [1], [2]]))

  }

  test("topological sort SCC - cycle of two nodes #1") {

    assert((

      [[1, 2]]) == (

      [[1, 2]]))

  }

  test("topological sort SCC - cycle of two nodes #2") {

    assert((

      [[1, 2]]) == (

      [[1, 2]]))

  }

  test("topological sort SCC - cycle of three nodes #1") {

    assert((

      [[1, 2, 3]]) == (

      [[1, 2, 3]]))

  }

  test("topological sort SCC - cycle of three nodes #2") {

    assert((

      [[1, 2, 3]]) == (

      [[1, 2, 3]]))

  }

  test("topological sort SCC - multiple disconnected cycles") {

    assert((

      [[1, 2, 3], [10, 20], [100], [200], [300]]) == (

      [[1, 2, 3], [10, 20], [100], [200], [300]]))

  }

  test("topological sort SCC - complex cycles") {

    assert((

      [[1, 2, 3]]) == (

      [[1, 2, 3]]))

  }

  test("topological sort SCC - chain of three SCCs") {

    assert((

      [[100], [10, 20], [1, 2, 3]]) == (

      [[100], [10, 20], [1, 2, 3]]))

  }

  test("topological sort SCC - SCCs with dependencies to/from non-SCC nodes") {

    assert((

      [[30], [20], [10], [1, 2, 3], [200], [100], [300], [1000], [2000]]) == (

      [[30], [20], [10], [1, 2, 3], [200], [100], [300], [1000], [2000]]))

  }
}
