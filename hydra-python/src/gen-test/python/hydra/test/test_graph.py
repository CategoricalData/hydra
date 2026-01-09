# Note: this is an automatically generated file. Do not edit.

r"""A module defining the graph used in the test suite."""

from __future__ import annotations
from hydra.dsl.python import FrozenDict
import hydra.core
import hydra.lib.maps
import hydra.module
import hydra.test.test_terms
import hydra.test.test_types

test_namespace = hydra.module.Namespace("testGraph")

test_schema_namespace = hydra.module.Namespace("testSchemaGraph")

def test_terms() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
    return hydra.lib.maps.from_list(((hydra.core.Name("testDataArthur"), hydra.test.test_terms.test_data_arthur),))

def test_types() -> FrozenDict[hydra.core.Name, hydra.core.Type]:
    return hydra.lib.maps.from_list(((hydra.test.test_types.test_type_buddy_list_a_name, hydra.test.test_types.test_type_buddy_list_a), (hydra.test.test_types.test_type_buddy_list_b_name, hydra.test.test_types.test_type_buddy_list_b), (hydra.test.test_types.test_type_comparison_name, hydra.test.test_types.test_type_comparison), (hydra.test.test_types.test_type_either_name, hydra.test.test_types.test_type_either), (hydra.test.test_types.test_type_flow_name, hydra.test.test_types.test_type_flow), (hydra.test.test_types.test_type_flow_state_name, hydra.test.test_types.test_type_flow_state), (hydra.test.test_types.test_type_hydra_literal_type_name, hydra.test.test_types.test_type_hydra_literal_type), (hydra.test.test_types.test_type_hydra_type_name, hydra.test.test_types.test_type_hydra_type), (hydra.test.test_types.test_type_int_list_name, hydra.test.test_types.test_type_int_list), (hydra.test.test_types.test_type_lat_lon_name, hydra.test.test_types.test_type_lat_lon), (hydra.test.test_types.test_type_lat_lon_poly_name, hydra.test.test_types.test_type_lat_lon_poly), (hydra.test.test_types.test_type_list_name, hydra.test.test_types.test_type_list), (hydra.test.test_types.test_type_number_name, hydra.test.test_types.test_type_number), (hydra.test.test_types.test_type_person_name, hydra.test.test_types.test_type_person), (hydra.test.test_types.test_type_person_or_something_name, hydra.test.test_types.test_type_person_or_something), (hydra.test.test_types.test_type_polymorphic_wrapper_name, hydra.test.test_types.test_type_polymorphic_wrapper), (hydra.test.test_types.test_type_simple_number_name, hydra.test.test_types.test_type_simple_number), (hydra.test.test_types.test_type_string_alias_name, hydra.test.test_types.test_type_string_alias), (hydra.test.test_types.test_type_symmetric_triple_name, hydra.test.test_types.test_type_symmetric_triple), (hydra.test.test_types.test_type_timestamp_name, hydra.test.test_types.test_type_timestamp), (hydra.test.test_types.test_type_trace_name, hydra.test.test_types.test_type_trace), (hydra.test.test_types.test_type_triple_name, hydra.test.test_types.test_type_triple), (hydra.test.test_types.test_type_union_monomorphic_name, hydra.test.test_types.test_type_union_monomorphic), (hydra.test.test_types.test_type_union_polymorphic_recursive_name, hydra.test.test_types.test_type_union_polymorphic_recursive), (hydra.test.test_types.test_type_unit_name, hydra.test.test_types.test_type_unit())))
