// Note: this is an automatically generated file. Do not edit.
// sorting

package generation.hydra.test;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import java.util.*;
import hydra.util.*;

public class SortingTest {

    // topological sort

    @Test

    public <T0, T1> void testTopologicalSortEmptySet() {

        assertEquals(

            (hydra.util.Either<T1, java.util.List<T0>>) ((hydra.util.Either<T1, java.util.List<T0>>) (hydra.util.Either.<T1, java.util.List<T0>>right((java.util.List<T0>) (java.util.List.<T0>of())))),

            hydra.sorting.Sorting.topologicalSort((java.util.List<hydra.util.Pair<T0, java.util.List<T0>>>) (java.util.List.<hydra.util.Pair<T0, java.util.List<T0>>>of())));

    }

    @Test

    public <T1> void testTopologicalSortSingletonSet() {

        assertEquals(

            (hydra.util.Either<T1, java.util.List<Integer>>) ((hydra.util.Either<T1, java.util.List<Integer>>) (hydra.util.Either.<T1, java.util.List<Integer>>right(java.util.List.of(1)))),

            hydra.sorting.Sorting.topologicalSort(java.util.List.of((hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, (java.util.List<Integer>) (java.util.List.<Integer>of())))))));

    }

    @Test

    public <T1> void testTopologicalSortDiscreteSetWithMultipleElements() {

        assertEquals(

            (hydra.util.Either<T1, java.util.List<Integer>>) ((hydra.util.Either<T1, java.util.List<Integer>>) (hydra.util.Either.<T1, java.util.List<Integer>>right(java.util.List.of(
  1,
  2,
  3)))),

            hydra.sorting.Sorting.topologicalSort(java.util.List.of(
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(3, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, (java.util.List<Integer>) (java.util.List.<Integer>of())))))));

    }

    @Test

    public <T1> void testTopologicalSortLinkedList() {

        assertEquals(

            (hydra.util.Either<T1, java.util.List<Integer>>) ((hydra.util.Either<T1, java.util.List<Integer>>) (hydra.util.Either.<T1, java.util.List<Integer>>right(java.util.List.of(
  1,
  3,
  2)))),

            hydra.sorting.Sorting.topologicalSort(java.util.List.of(
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(3, java.util.List.of(1)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.List.of(3)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, (java.util.List<Integer>) (java.util.List.<Integer>of())))))));

    }

    @Test

    public <T1> void testTopologicalSortBinaryTree() {

        assertEquals(

            (hydra.util.Either<T1, java.util.List<Integer>>) ((hydra.util.Either<T1, java.util.List<Integer>>) (hydra.util.Either.<T1, java.util.List<Integer>>right(java.util.List.of(
  5,
  1,
  2,
  6,
  4,
  3)))),

            hydra.sorting.Sorting.topologicalSort(java.util.List.of(
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(3, java.util.List.of(
    1,
    4)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(4, java.util.List.of(
    6,
    2)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, java.util.List.of(5)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(6, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(5, (java.util.List<Integer>) (java.util.List.<Integer>of())))))));

    }

    @Test

    public <T1> void testTopologicalSortTwoTrees() {

        assertEquals(

            (hydra.util.Either<T1, java.util.List<Integer>>) ((hydra.util.Either<T1, java.util.List<Integer>>) (hydra.util.Either.<T1, java.util.List<Integer>>right(java.util.List.of(
  1,
  7,
  2,
  4,
  3,
  6,
  5)))),

            hydra.sorting.Sorting.topologicalSort(java.util.List.of(
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(3, java.util.List.of(
    1,
    4)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(5, java.util.List.of(
    6,
    2)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.List.of(7)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(4, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(6, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(7, (java.util.List<Integer>) (java.util.List.<Integer>of())))))));

    }

    @Test

    public <T1> void testTopologicalSortDiamondDag() {

        assertEquals(

            (hydra.util.Either<T1, java.util.List<Integer>>) ((hydra.util.Either<T1, java.util.List<Integer>>) (hydra.util.Either.<T1, java.util.List<Integer>>right(java.util.List.of(
  5,
  2,
  3,
  4,
  1)))),

            hydra.sorting.Sorting.topologicalSort(java.util.List.of(
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, java.util.List.of(
    3,
    4)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(3, java.util.List.of(2)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(4, java.util.List.of(2)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.List.of(5)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(5, (java.util.List<Integer>) (java.util.List.<Integer>of())))))));

    }

    @Test

    public <T2> void testTopologicalSortTwoNegnodeCycle() {

        assertEquals(

            (hydra.util.Either<java.util.List<java.util.List<Integer>>, T2>) ((hydra.util.Either<java.util.List<java.util.List<Integer>>, T2>) (hydra.util.Either.<java.util.List<java.util.List<Integer>>, T2>left(java.util.List.of(java.util.List.of(
  1,
  2))))),

            hydra.sorting.Sorting.topologicalSort(java.util.List.of(
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, java.util.List.of(2)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.List.of(1)))))));

    }

    @Test

    public <T2> void testTopologicalSortCycleWithIncomingAndOutgoingEdges() {

        assertEquals(

            (hydra.util.Either<java.util.List<java.util.List<Integer>>, T2>) ((hydra.util.Either<java.util.List<java.util.List<Integer>>, T2>) (hydra.util.Either.<java.util.List<java.util.List<Integer>>, T2>left(java.util.List.of(java.util.List.of(
  2,
  3))))),

            hydra.sorting.Sorting.topologicalSort(java.util.List.of(
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, java.util.List.of(3)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(3, java.util.List.of(2)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.List.of(
    3,
    4)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(4, java.util.List.of(5)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(5, (java.util.List<Integer>) (java.util.List.<Integer>of())))))));

    }

    // topological sort SCC

    @Test

    public <T0> void testTopologicalSortSccEmptySet() {

        assertEquals(

            (java.util.List<T0>) (java.util.List.<T0>of()),

            hydra.sorting.Sorting.topologicalSortComponents((java.util.List<hydra.util.Pair<T0, java.util.List<T0>>>) (java.util.List.<hydra.util.Pair<T0, java.util.List<T0>>>of())));

    }

    @Test

    public void testTopologicalSortSccSingletonSet() {

        assertEquals(

            java.util.List.of(java.util.List.of(1)),

            hydra.sorting.Sorting.topologicalSortComponents(java.util.List.of((hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, (java.util.List<Integer>) (java.util.List.<Integer>of())))))));

    }

    @Test

    public void testTopologicalSortSccDiscreteSetWithMultipleElements() {

        assertEquals(

            java.util.List.of(
  java.util.List.of(1),
  java.util.List.of(2),
  java.util.List.of(3)),

            hydra.sorting.Sorting.topologicalSortComponents(java.util.List.of(
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(3, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, (java.util.List<Integer>) (java.util.List.<Integer>of())))))));

    }

    @Test

    public void testTopologicalSortSccSingleTwoNegelementComponentNum1() {

        assertEquals(

            java.util.List.of(
  java.util.List.of(2),
  java.util.List.of(1)),

            hydra.sorting.Sorting.topologicalSortComponents(java.util.List.of(
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, java.util.List.of(2)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, (java.util.List<Integer>) (java.util.List.<Integer>of())))))));

    }

    @Test

    public void testTopologicalSortSccSingleTwoNegelementComponentNum2() {

        assertEquals(

            java.util.List.of(
  java.util.List.of(1),
  java.util.List.of(2)),

            hydra.sorting.Sorting.topologicalSortComponents(java.util.List.of(
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.List.of(1)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, (java.util.List<Integer>) (java.util.List.<Integer>of())))))));

    }

    @Test

    public void testTopologicalSortSccMultipleNegelementComponent() {

        assertEquals(

            java.util.List.of(
  java.util.List.of(3),
  java.util.List.of(1),
  java.util.List.of(2)),

            hydra.sorting.Sorting.topologicalSortComponents(java.util.List.of(
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.List.of(
    1,
    3)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, java.util.List.of(3)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(3, (java.util.List<Integer>) (java.util.List.<Integer>of())))))));

    }

    @Test

    public void testTopologicalSortSccCycleOfTwoNodesNum1() {

        assertEquals(

            java.util.List.of(java.util.List.of(
  1,
  2)),

            hydra.sorting.Sorting.topologicalSortComponents(java.util.List.of(
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, java.util.List.of(2)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.List.of(1)))))));

    }

    @Test

    public void testTopologicalSortSccCycleOfTwoNodesNum2() {

        assertEquals(

            java.util.List.of(java.util.List.of(
  1,
  2)),

            hydra.sorting.Sorting.topologicalSortComponents(java.util.List.of(
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.List.of(1)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, java.util.List.of(2)))))));

    }

    @Test

    public void testTopologicalSortSccCycleOfThreeNodesNum1() {

        assertEquals(

            java.util.List.of(java.util.List.of(
  1,
  2,
  3)),

            hydra.sorting.Sorting.topologicalSortComponents(java.util.List.of(
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, java.util.List.of(2)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.List.of(3)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(3, java.util.List.of(1)))))));

    }

    @Test

    public void testTopologicalSortSccCycleOfThreeNodesNum2() {

        assertEquals(

            java.util.List.of(java.util.List.of(
  1,
  2,
  3)),

            hydra.sorting.Sorting.topologicalSortComponents(java.util.List.of(
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.List.of(1)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(3, java.util.List.of(2)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, java.util.List.of(3)))))));

    }

    @Test

    public void testTopologicalSortSccMultipleDisconnectedCycles() {

        assertEquals(

            java.util.List.of(
  java.util.List.of(
    1,
    2,
    3),
  java.util.List.of(
    10,
    20),
  java.util.List.of(100),
  java.util.List.of(200),
  java.util.List.of(300)),

            hydra.sorting.Sorting.topologicalSortComponents(java.util.List.of(
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(200, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(100, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(300, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(10, java.util.List.of(20)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(20, java.util.List.of(10)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, java.util.List.of(2)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.List.of(3)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(3, java.util.List.of(1)))))));

    }

    @Test

    public void testTopologicalSortSccComplexCycles() {

        assertEquals(

            java.util.List.of(java.util.List.of(
  1,
  2,
  3)),

            hydra.sorting.Sorting.topologicalSortComponents(java.util.List.of(
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, java.util.List.of(
    2,
    3)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.List.of(3)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(3, java.util.List.of(1)))))));

    }

    @Test

    public void testTopologicalSortSccChainOfThreeSccs() {

        assertEquals(

            java.util.List.of(
  java.util.List.of(100),
  java.util.List.of(
    10,
    20),
  java.util.List.of(
    1,
    2,
    3)),

            hydra.sorting.Sorting.topologicalSortComponents(java.util.List.of(
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, java.util.List.of(
    2,
    10)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.List.of(3)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(3, java.util.List.of(1)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(10, java.util.List.of(20)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(20, java.util.List.of(
    100,
    10)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(100, (java.util.List<Integer>) (java.util.List.<Integer>of())))))));

    }

    @Test

    public void testTopologicalSortSccSccsWithDependenciesToDivfromNonNegsccNodes() {

        assertEquals(

            java.util.List.of(
  java.util.List.of(30),
  java.util.List.of(20),
  java.util.List.of(10),
  java.util.List.of(
    1,
    2,
    3),
  java.util.List.of(200),
  java.util.List.of(100),
  java.util.List.of(300),
  java.util.List.of(1000),
  java.util.List.of(2000)),

            hydra.sorting.Sorting.topologicalSortComponents(java.util.List.of(
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1, java.util.List.of(
    2,
    3,
    10)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2, java.util.List.of(3)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(3, java.util.List.of(1)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(10, java.util.List.of(
    20,
    30)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(20, java.util.List.of(30)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(30, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(100, java.util.List.of(
    200,
    2)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(200, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(300, java.util.List.of(100)))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(1000, (java.util.List<Integer>) (java.util.List.<Integer>of())))),
  (hydra.util.Pair<Integer, java.util.List<Integer>>) ((hydra.util.Pair<Integer, java.util.List<Integer>>) (new hydra.util.Pair<Integer, java.util.List<Integer>>(2000, (java.util.List<Integer>) (java.util.List.<Integer>of())))))));

    }
}
