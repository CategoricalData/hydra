# Note: this is an automatically generated file. Do not edit.
# sorting

from __future__ import annotations
from typing import cast
from decimal import Decimal
from hydra.dsl.python import FrozenDict, frozenlist, Either, Left, Right, Maybe, Just, Nothing
import hydra.accessors
import hydra.annotations
import hydra.ast
import hydra.classes
import hydra.coders
import hydra.compute
import hydra.constants
import hydra.core
import hydra.decode.core
import hydra.encode.core
import hydra.ext.haskell.operators
import hydra.extract.core
import hydra.extract.helpers
import hydra.formatting
import hydra.graph
import hydra.json.model
import hydra.lexical
import hydra.lib.chars
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.module
import hydra.monads
import hydra.names
import hydra.parsing
import hydra.rewriting
import hydra.serialization
import hydra.show.core
import hydra.sorting
import hydra.tarjan
import hydra.testing
import hydra.topology
import hydra.typing
import hydra.util

# topological sort

def test_topological_sort__empty_set():

    assert (hydra.sorting.topological_sort(())) == (Right(()))

def test_topological_sort__singleton_set():

    assert (hydra.sorting.topological_sort(((1, ()),))) == (Right((1,)))

def test_topological_sort__discrete_set_with_multiple_elements():

    assert (hydra.sorting.topological_sort(((3, ()), (1, ()), (2, ())))) == (Right((1, 2, 3)))

def test_topological_sort__linked_list():

    assert (hydra.sorting.topological_sort(((3, (1,)), (2, (3,)), (1, ())))) == (Right((1, 3, 2)))

def test_topological_sort__binary_tree():

    assert (hydra.sorting.topological_sort(((3, (1, 4)), (4, (6, 2)), (1, (5,)), (2, ()), (6, ()), (5, ())))) == (Right((5, 1, 2, 6, 4, 3)))

def test_topological_sort__two_trees():

    assert (hydra.sorting.topological_sort(((3, (1, 4)), (5, (6, 2)), (2, (7,)), (1, ()), (4, ()), (6, ()), (7, ())))) == (Right((1, 7, 2, 4, 3, 6, 5)))

def test_topological_sort__diamond_dag():

    assert (hydra.sorting.topological_sort(((1, (3, 4)), (3, (2,)), (4, (2,)), (2, (5,)), (5, ())))) == (Right((5, 2, 3, 4, 1)))

def test_topological_sort__two_node_cycle():

    assert (hydra.sorting.topological_sort(((1, (2,)), (2, (1,))))) == (Left(((1, 2),)))

def test_topological_sort__cycle_with_incoming_and_outgoing_edges():

    assert (hydra.sorting.topological_sort(((1, (3,)), (3, (2,)), (2, (3, 4)), (4, (5,)), (5, ())))) == (Left(((2, 3),)))

# topological sort SCC

def test_topological_sort_scc__empty_set():

    assert (hydra.sorting.topological_sort_components(())) == (())

def test_topological_sort_scc__singleton_set():

    assert (hydra.sorting.topological_sort_components(((1, ()),))) == (((1,),))

def test_topological_sort_scc__discrete_set_with_multiple_elements():

    assert (hydra.sorting.topological_sort_components(((3, ()), (1, ()), (2, ())))) == (((1,), (2,), (3,)))

def test_topological_sort_scc__single_two_element_component__1():

    assert (hydra.sorting.topological_sort_components(((1, (2,)), (2, ())))) == (((2,), (1,)))

def test_topological_sort_scc__single_two_element_component__2():

    assert (hydra.sorting.topological_sort_components(((2, (1,)), (1, ())))) == (((1,), (2,)))

def test_topological_sort_scc__multiple_element_component():

    assert (hydra.sorting.topological_sort_components(((2, (1, 3)), (1, (3,)), (3, ())))) == (((3,), (1,), (2,)))

def test_topological_sort_scc__cycle_of_two_nodes__1():

    assert (hydra.sorting.topological_sort_components(((1, (2,)), (2, (1,))))) == (((1, 2),))

def test_topological_sort_scc__cycle_of_two_nodes__2():

    assert (hydra.sorting.topological_sort_components(((2, (1,)), (1, (2,))))) == (((1, 2),))

def test_topological_sort_scc__cycle_of_three_nodes__1():

    assert (hydra.sorting.topological_sort_components(((1, (2,)), (2, (3,)), (3, (1,))))) == (((1, 2, 3),))

def test_topological_sort_scc__cycle_of_three_nodes__2():

    assert (hydra.sorting.topological_sort_components(((2, (1,)), (3, (2,)), (1, (3,))))) == (((1, 2, 3),))

def test_topological_sort_scc__multiple_disconnected_cycles():

    assert (hydra.sorting.topological_sort_components(((200, ()), (100, ()), (300, ()), (10, (20,)), (20, (10,)), (1, (2,)), (2, (3,)), (3, (1,))))) == (((1, 2, 3), (10, 20), (100,), (200,), (300,)))

def test_topological_sort_scc__complex_cycles():

    assert (hydra.sorting.topological_sort_components(((1, (2, 3)), (2, (3,)), (3, (1,))))) == (((1, 2, 3),))

def test_topological_sort_scc__chain_of_three_sccs():

    assert (hydra.sorting.topological_sort_components(((1, (2, 10)), (2, (3,)), (3, (1,)), (10, (20,)), (20, (100, 10)), (100, ())))) == (((100,), (10, 20), (1, 2, 3)))

def test_topological_sort_scc__sccs_with_dependencies_to_from_non_scc_nodes():

    assert (hydra.sorting.topological_sort_components(((1, (2, 3, 10)), (2, (3,)), (3, (1,)), (10, (20, 30)), (20, (30,)), (30, ()), (100, (200, 2)), (200, ()), (300, (100,)), (1000, ()), (2000, ())))) == (((30,), (20,), (10,), (1, 2, 3), (200,), (100,), (300,), (1000,), (2000,)))
