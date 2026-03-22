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

            hydra.util.Either.<T1, hydra.util.ConsList<T0>>right((hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty())),

            hydra.Sorting.topologicalSort((hydra.util.ConsList<hydra.util.Pair<T0, hydra.util.ConsList<T0>>>) (hydra.util.ConsList.<hydra.util.Pair<T0, hydra.util.ConsList<T0>>>empty())));

    }

    @Test

    public <T1> void testTopologicalSortSingletonSet() {

        assertEquals(

            hydra.util.Either.<T1, hydra.util.ConsList<Integer>>right(hydra.util.ConsList.of(1)),

            hydra.Sorting.topologicalSort(hydra.util.ConsList.of((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))))));

    }

    @Test

    public <T1> void testTopologicalSortDiscreteSetWithMultipleElements() {

        assertEquals(

            hydra.util.Either.<T1, hydra.util.ConsList<Integer>>right(hydra.util.ConsList.of(
  1,
  2,
  3)),

            hydra.Sorting.topologicalSort(hydra.util.ConsList.of(
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(3, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))))));

    }

    @Test

    public <T1> void testTopologicalSortLinkedList() {

        assertEquals(

            hydra.util.Either.<T1, hydra.util.ConsList<Integer>>right(hydra.util.ConsList.of(
  1,
  3,
  2)),

            hydra.Sorting.topologicalSort(hydra.util.ConsList.of(
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(3, hydra.util.ConsList.of(1)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(3)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))))));

    }

    @Test

    public <T1> void testTopologicalSortBinaryTree() {

        assertEquals(

            hydra.util.Either.<T1, hydra.util.ConsList<Integer>>right(hydra.util.ConsList.of(
  5,
  1,
  2,
  6,
  4,
  3)),

            hydra.Sorting.topologicalSort(hydra.util.ConsList.of(
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(3, hydra.util.ConsList.of(
    1,
    4)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(4, hydra.util.ConsList.of(
    6,
    2)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, hydra.util.ConsList.of(5)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(6, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(5, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))))));

    }

    @Test

    public <T1> void testTopologicalSortTwoTrees() {

        assertEquals(

            hydra.util.Either.<T1, hydra.util.ConsList<Integer>>right(hydra.util.ConsList.of(
  1,
  7,
  2,
  4,
  3,
  6,
  5)),

            hydra.Sorting.topologicalSort(hydra.util.ConsList.of(
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(3, hydra.util.ConsList.of(
    1,
    4)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(5, hydra.util.ConsList.of(
    6,
    2)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(7)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(4, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(6, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(7, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))))));

    }

    @Test

    public <T1> void testTopologicalSortDiamondDag() {

        assertEquals(

            hydra.util.Either.<T1, hydra.util.ConsList<Integer>>right(hydra.util.ConsList.of(
  5,
  2,
  3,
  4,
  1)),

            hydra.Sorting.topologicalSort(hydra.util.ConsList.of(
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, hydra.util.ConsList.of(
    3,
    4)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(3, hydra.util.ConsList.of(2)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(4, hydra.util.ConsList.of(2)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(5)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(5, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))))));

    }

    @Test

    public <T2> void testTopologicalSortTwoNegnodeCycle() {

        assertEquals(

            hydra.util.Either.<hydra.util.ConsList<hydra.util.ConsList<Integer>>, T2>left(hydra.util.ConsList.of(hydra.util.ConsList.of(
  1,
  2))),

            hydra.Sorting.topologicalSort(hydra.util.ConsList.of(
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, hydra.util.ConsList.of(2)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(1)))))));

    }

    @Test

    public <T2> void testTopologicalSortCycleWithIncomingAndOutgoingEdges() {

        assertEquals(

            hydra.util.Either.<hydra.util.ConsList<hydra.util.ConsList<Integer>>, T2>left(hydra.util.ConsList.of(hydra.util.ConsList.of(
  2,
  3))),

            hydra.Sorting.topologicalSort(hydra.util.ConsList.of(
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, hydra.util.ConsList.of(3)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(3, hydra.util.ConsList.of(2)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(
    3,
    4)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(4, hydra.util.ConsList.of(5)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(5, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))))));

    }

    // topological sort SCC

    @Test

    public <T0> void testTopologicalSortSccEmptySet() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>empty()),

            hydra.Sorting.topologicalSortComponents((hydra.util.ConsList<hydra.util.Pair<T0, hydra.util.ConsList<T0>>>) (hydra.util.ConsList.<hydra.util.Pair<T0, hydra.util.ConsList<T0>>>empty())));

    }

    @Test

    public void testTopologicalSortSccSingletonSet() {

        assertEquals(

            hydra.util.ConsList.of(hydra.util.ConsList.of(1)),

            hydra.Sorting.topologicalSortComponents(hydra.util.ConsList.of((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))))));

    }

    @Test

    public void testTopologicalSortSccDiscreteSetWithMultipleElements() {

        assertEquals(

            hydra.util.ConsList.of(
  hydra.util.ConsList.of(1),
  hydra.util.ConsList.of(2),
  hydra.util.ConsList.of(3)),

            hydra.Sorting.topologicalSortComponents(hydra.util.ConsList.of(
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(3, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))))));

    }

    @Test

    public void testTopologicalSortSccSingleTwoNegelementComponentNum1() {

        assertEquals(

            hydra.util.ConsList.of(
  hydra.util.ConsList.of(2),
  hydra.util.ConsList.of(1)),

            hydra.Sorting.topologicalSortComponents(hydra.util.ConsList.of(
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, hydra.util.ConsList.of(2)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))))));

    }

    @Test

    public void testTopologicalSortSccSingleTwoNegelementComponentNum2() {

        assertEquals(

            hydra.util.ConsList.of(
  hydra.util.ConsList.of(1),
  hydra.util.ConsList.of(2)),

            hydra.Sorting.topologicalSortComponents(hydra.util.ConsList.of(
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(1)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))))));

    }

    @Test

    public void testTopologicalSortSccMultipleNegelementComponent() {

        assertEquals(

            hydra.util.ConsList.of(
  hydra.util.ConsList.of(3),
  hydra.util.ConsList.of(1),
  hydra.util.ConsList.of(2)),

            hydra.Sorting.topologicalSortComponents(hydra.util.ConsList.of(
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(
    1,
    3)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, hydra.util.ConsList.of(3)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(3, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))))));

    }

    @Test

    public void testTopologicalSortSccCycleOfTwoNodesNum1() {

        assertEquals(

            hydra.util.ConsList.of(hydra.util.ConsList.of(
  1,
  2)),

            hydra.Sorting.topologicalSortComponents(hydra.util.ConsList.of(
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, hydra.util.ConsList.of(2)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(1)))))));

    }

    @Test

    public void testTopologicalSortSccCycleOfTwoNodesNum2() {

        assertEquals(

            hydra.util.ConsList.of(hydra.util.ConsList.of(
  1,
  2)),

            hydra.Sorting.topologicalSortComponents(hydra.util.ConsList.of(
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(1)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, hydra.util.ConsList.of(2)))))));

    }

    @Test

    public void testTopologicalSortSccCycleOfThreeNodesNum1() {

        assertEquals(

            hydra.util.ConsList.of(hydra.util.ConsList.of(
  1,
  2,
  3)),

            hydra.Sorting.topologicalSortComponents(hydra.util.ConsList.of(
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, hydra.util.ConsList.of(2)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(3)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(3, hydra.util.ConsList.of(1)))))));

    }

    @Test

    public void testTopologicalSortSccCycleOfThreeNodesNum2() {

        assertEquals(

            hydra.util.ConsList.of(hydra.util.ConsList.of(
  1,
  2,
  3)),

            hydra.Sorting.topologicalSortComponents(hydra.util.ConsList.of(
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(1)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(3, hydra.util.ConsList.of(2)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, hydra.util.ConsList.of(3)))))));

    }

    @Test

    public void testTopologicalSortSccMultipleDisconnectedCycles() {

        assertEquals(

            hydra.util.ConsList.of(
  hydra.util.ConsList.of(
    1,
    2,
    3),
  hydra.util.ConsList.of(
    10,
    20),
  hydra.util.ConsList.of(100),
  hydra.util.ConsList.of(200),
  hydra.util.ConsList.of(300)),

            hydra.Sorting.topologicalSortComponents(hydra.util.ConsList.of(
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(200, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(100, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(300, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(10, hydra.util.ConsList.of(20)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(20, hydra.util.ConsList.of(10)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, hydra.util.ConsList.of(2)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(3)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(3, hydra.util.ConsList.of(1)))))));

    }

    @Test

    public void testTopologicalSortSccComplexCycles() {

        assertEquals(

            hydra.util.ConsList.of(hydra.util.ConsList.of(
  1,
  2,
  3)),

            hydra.Sorting.topologicalSortComponents(hydra.util.ConsList.of(
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, hydra.util.ConsList.of(
    2,
    3)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(3)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(3, hydra.util.ConsList.of(1)))))));

    }

    @Test

    public void testTopologicalSortSccChainOfThreeSccs() {

        assertEquals(

            hydra.util.ConsList.of(
  hydra.util.ConsList.of(100),
  hydra.util.ConsList.of(
    10,
    20),
  hydra.util.ConsList.of(
    1,
    2,
    3)),

            hydra.Sorting.topologicalSortComponents(hydra.util.ConsList.of(
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, hydra.util.ConsList.of(
    2,
    10)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(3)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(3, hydra.util.ConsList.of(1)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(10, hydra.util.ConsList.of(20)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(20, hydra.util.ConsList.of(
    100,
    10)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(100, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))))));

    }

    @Test

    public void testTopologicalSortSccSccsWithDependenciesToDivfromNonNegsccNodes() {

        assertEquals(

            hydra.util.ConsList.of(
  hydra.util.ConsList.of(30),
  hydra.util.ConsList.of(20),
  hydra.util.ConsList.of(10),
  hydra.util.ConsList.of(
    1,
    2,
    3),
  hydra.util.ConsList.of(200),
  hydra.util.ConsList.of(100),
  hydra.util.ConsList.of(300),
  hydra.util.ConsList.of(1000),
  hydra.util.ConsList.of(2000)),

            hydra.Sorting.topologicalSortComponents(hydra.util.ConsList.of(
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1, hydra.util.ConsList.of(
    2,
    3,
    10)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2, hydra.util.ConsList.of(3)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(3, hydra.util.ConsList.of(1)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(10, hydra.util.ConsList.of(
    20,
    30)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(20, hydra.util.ConsList.of(30)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(30, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(100, hydra.util.ConsList.of(
    200,
    2)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(200, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(300, hydra.util.ConsList.of(100)))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(1000, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))),
  (hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) ((hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>) (new hydra.util.Pair<Integer, hydra.util.ConsList<Integer>>(2000, (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))))));

    }
}
