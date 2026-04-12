# Note: this is an automatically generated file. Do not edit.

r"""A module defining the graph used in the test suite."""

from __future__ import annotations
from functools import lru_cache
from hydra.dsl.python import FrozenDict
from typing import cast
import hydra.core
import hydra.lexical
import hydra.lib.maps
import hydra.packaging
import hydra.test.test_terms
import hydra.test.test_types



test_namespace = hydra.packaging.Namespace("testGraph")

test_schema_namespace = hydra.packaging.Namespace("testSchemaGraph")

@lru_cache(1)
def test_terms() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
    return hydra.lib.maps.from_list(((hydra.core.Name("testDataArthur"), hydra.test.test_terms.test_data_arthur),))

@lru_cache(1)
def test_types() -> FrozenDict[hydra.core.Name, hydra.core.Type]:
    return hydra.lib.maps.from_list(((hydra.test.test_types.test_type_buddy_list_a_name, hydra.test.test_types.test_type_buddy_list_a), (hydra.test.test_types.test_type_buddy_list_b_name, hydra.test.test_types.test_type_buddy_list_b), (hydra.test.test_types.test_type_comparison_name, hydra.test.test_types.test_type_comparison), (hydra.test.test_types.test_type_either_name, hydra.test.test_types.test_type_either), (hydra.test.test_types.test_type_hydra_literal_type_name, hydra.test.test_types.test_type_hydra_literal_type), (hydra.test.test_types.test_type_hydra_type_name, hydra.test.test_types.test_type_hydra_type), (hydra.test.test_types.test_type_int_list_name, hydra.test.test_types.test_type_int_list), (hydra.test.test_types.test_type_lat_lon_name, hydra.test.test_types.test_type_lat_lon), (hydra.test.test_types.test_type_lat_lon_poly_name, hydra.test.test_types.test_type_lat_lon_poly), (hydra.test.test_types.test_type_list_name, hydra.test.test_types.test_type_list), (hydra.test.test_types.test_type_number_name, hydra.test.test_types.test_type_number), (hydra.test.test_types.test_type_person_name, hydra.test.test_types.test_type_person), (hydra.test.test_types.test_type_person_or_something_name, hydra.test.test_types.test_type_person_or_something), (hydra.test.test_types.test_type_polymorphic_wrapper_name, hydra.test.test_types.test_type_polymorphic_wrapper), (hydra.test.test_types.test_type_simple_number_name, hydra.test.test_types.test_type_simple_number), (hydra.test.test_types.test_type_string_alias_name, hydra.test.test_types.test_type_string_alias), (hydra.test.test_types.test_type_symmetric_triple_name, hydra.test.test_types.test_type_symmetric_triple), (hydra.test.test_types.test_type_timestamp_name, hydra.test.test_types.test_type_timestamp), (hydra.test.test_types.test_type_triple_name, hydra.test.test_types.test_type_triple), (hydra.test.test_types.test_type_union_monomorphic_name, hydra.test.test_types.test_type_union_monomorphic), (hydra.test.test_types.test_type_union_polymorphic_recursive_name, hydra.test.test_types.test_type_union_polymorphic_recursive), (hydra.test.test_types.test_type_unit_name, hydra.test.test_types.test_type_unit())))

_test_graph_cache = None
_test_context_cache = None

def __getattr__(name):
    global _test_graph_cache, _test_context_cache
    if name == "test_graph":
        if _test_graph_cache is None:
            import hydra.test.test_env as _test_env
            _test_graph_cache = _test_env.test_graph()
        return _test_graph_cache
    elif name == "test_context":
        if _test_context_cache is None:
            import hydra.test.test_env as _test_env
            _test_context_cache = _test_env.test_context()
        return _test_context_cache
    raise AttributeError(f"module 'hydra.test.test_graph' has no attribute {name!r}")
