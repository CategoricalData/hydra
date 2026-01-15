# Note: this is an automatically generated file. Do not edit.

r"""Type definitions for the test suite."""

from __future__ import annotations
from functools import lru_cache
from typing import cast
import hydra.core

compare_strings_type = cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()))), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()))))))

concat_type = cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()))), cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()))), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))))))))

either_string_or_int8_type_name = hydra.core.Name("EitherStringOrInt8")

either_string_or_int8_type = cast(hydra.core.Type, hydra.core.TypeUnion(hydra.core.RowType(either_string_or_int8_type_name, (hydra.core.FieldType(hydra.core.Name("left"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("right"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT8)))))))))

test_type_person_name = hydra.core.Name("Person")

example_projection_type = cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(cast(hydra.core.Type, hydra.core.TypeVariable(test_type_person_name)), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()))))))

list_of_int16s_type = cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT16))))))

list_of_int8s_type = cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT8))))))

list_of_lists_of_strings_type = cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))))))

list_of_set_of_strings_type = cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeSet(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))))))

list_of_strings_type = cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))))

map_of_strings_to_ints_type = cast(hydra.core.Type, hydra.core.TypeMap(hydra.core.MapType(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()))), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)))))))

optional_int16_type = cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT16))))))

optional_int8_type = cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT8))))))

optional_string_type = cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))))

set_of_strings_type = cast(hydra.core.Type, hydra.core.TypeSet(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))))

string_or_int_name = hydra.core.Name("StringOrInt")

string_or_int_type = cast(hydra.core.Type, hydra.core.TypeUnion(hydra.core.RowType(string_or_int_name, (hydra.core.FieldType(hydra.core.Name("left"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("right"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)))))))))

test_type_buddy_list_a_name = hydra.core.Name("BuddyListA")

test_type_buddy_list_b_name = hydra.core.Name("BuddyListB")

test_type_buddy_list_a = cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(hydra.core.Name("a"), cast(hydra.core.Type, hydra.core.TypeRecord(hydra.core.RowType(test_type_buddy_list_a_name, (hydra.core.FieldType(hydra.core.Name("head"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("a")))), hydra.core.FieldType(hydra.core.Name("tail"), cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(cast(hydra.core.Type, hydra.core.TypeVariable(test_type_buddy_list_b_name)), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("a"))))))))))))))))

test_type_buddy_list_b = cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(hydra.core.Name("a"), cast(hydra.core.Type, hydra.core.TypeRecord(hydra.core.RowType(test_type_buddy_list_b_name, (hydra.core.FieldType(hydra.core.Name("head"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("a")))), hydra.core.FieldType(hydra.core.Name("tail"), cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(cast(hydra.core.Type, hydra.core.TypeVariable(test_type_buddy_list_a_name)), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("a"))))))))))))))))

test_type_comparison_name = hydra.core.Name("Comparison")

test_type_comparison = cast(hydra.core.Type, hydra.core.TypeUnion(hydra.core.RowType(test_type_comparison_name, (hydra.core.FieldType(hydra.core.Name("lessThan"), cast(hydra.core.Type, hydra.core.TypeUnit())), hydra.core.FieldType(hydra.core.Name("equalTo"), cast(hydra.core.Type, hydra.core.TypeUnit())), hydra.core.FieldType(hydra.core.Name("greaterThan"), cast(hydra.core.Type, hydra.core.TypeUnit()))))))

test_type_either_name = hydra.core.Name("Either")

test_type_either = cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(hydra.core.Name("a"), cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(hydra.core.Name("b"), cast(hydra.core.Type, hydra.core.TypeUnion(hydra.core.RowType(test_type_either_name, (hydra.core.FieldType(hydra.core.Name("left"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("a")))), hydra.core.FieldType(hydra.core.Name("right"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("b"))))))))))))))

test_type_flow_name = hydra.core.Name("hydra.compute.Flow")

test_type_flow_state_name = hydra.core.Name("hydra.compute.FlowState")

test_type_flow = cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(hydra.core.Name("s"), cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(hydra.core.Name("a"), cast(hydra.core.Type, hydra.core.TypeRecord(hydra.core.RowType(test_type_flow_name, (hydra.core.FieldType(hydra.core.Name("value"), cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("s"))), cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(cast(hydra.core.Type, hydra.core.TypeVariable(test_type_flow_state_name)), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("s")))))), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("a")))))))))),))))))))))

test_type_trace_name = hydra.core.Name("hydra.compute.Trace")

test_type_flow_state = cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(hydra.core.Name("s"), cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(hydra.core.Name("a"), cast(hydra.core.Type, hydra.core.TypeRecord(hydra.core.RowType(test_type_flow_state_name, (hydra.core.FieldType(hydra.core.Name("value"), cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("a")))))), hydra.core.FieldType(hydra.core.Name("state"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("s")))), hydra.core.FieldType(hydra.core.Name("trace"), cast(hydra.core.Type, hydra.core.TypeVariable(test_type_trace_name)))))))))))))

test_type_hydra_literal_type_name = hydra.core.Name("HydraLiteralType")

test_type_hydra_literal_type = cast(hydra.core.Type, hydra.core.TypeUnion(hydra.core.RowType(test_type_hydra_literal_type_name, (hydra.core.FieldType(hydra.core.Name("boolean"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeBoolean())))), hydra.core.FieldType(hydra.core.Name("string"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()))))))))

test_type_hydra_type_name = hydra.core.Name("HydraType")

test_type_hydra_type = cast(hydra.core.Type, hydra.core.TypeUnion(hydra.core.RowType(test_type_hydra_type_name, (hydra.core.FieldType(hydra.core.Name("literal"), cast(hydra.core.Type, hydra.core.TypeVariable(test_type_hydra_literal_type_name))), hydra.core.FieldType(hydra.core.Name("list"), cast(hydra.core.Type, hydra.core.TypeVariable(test_type_hydra_type_name)))))))

test_type_int_list_name = hydra.core.Name("IntList")

test_type_int_list = cast(hydra.core.Type, hydra.core.TypeRecord(hydra.core.RowType(test_type_int_list_name, (hydra.core.FieldType(hydra.core.Name("head"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))), hydra.core.FieldType(hydra.core.Name("tail"), cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeVariable(test_type_int_list_name)))))))))

test_type_lat_lon_name = hydra.core.Name("LatLon")

test_type_lat_lon = cast(hydra.core.Type, hydra.core.TypeRecord(hydra.core.RowType(test_type_lat_lon_name, (hydra.core.FieldType(hydra.core.Name("lat"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT32))))), hydra.core.FieldType(hydra.core.Name("lon"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT32)))))))))

test_type_lat_lon_poly_name = hydra.core.Name("LatLonPoly")

test_type_lat_lon_poly = cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(hydra.core.Name("a"), cast(hydra.core.Type, hydra.core.TypeRecord(hydra.core.RowType(test_type_lat_lon_poly_name, (hydra.core.FieldType(hydra.core.Name("lat"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("a")))), hydra.core.FieldType(hydra.core.Name("lon"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("a")))))))))))

test_type_list_name = hydra.core.Name("List")

test_type_list = cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(hydra.core.Name("a"), cast(hydra.core.Type, hydra.core.TypeRecord(hydra.core.RowType(test_type_list_name, (hydra.core.FieldType(hydra.core.Name("head"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("a")))), hydra.core.FieldType(hydra.core.Name("tail"), cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(cast(hydra.core.Type, hydra.core.TypeVariable(test_type_list_name)), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("a"))))))))))))))))

test_type_name = hydra.core.Name("Test")

test_type_number_name = hydra.core.Name("Number")

test_type_number = cast(hydra.core.Type, hydra.core.TypeUnion(hydra.core.RowType(test_type_number_name, (hydra.core.FieldType(hydra.core.Name("int"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))), hydra.core.FieldType(hydra.core.Name("float"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT32)))))))))

test_type_person = cast(hydra.core.Type, hydra.core.TypeRecord(hydra.core.RowType(test_type_person_name, (hydra.core.FieldType(hydra.core.Name("firstName"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("lastName"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("age"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)))))))))

test_type_person_or_something_name = hydra.core.Name("PersonOrSomething")

test_type_person_or_something = cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(hydra.core.Name("a"), cast(hydra.core.Type, hydra.core.TypeUnion(hydra.core.RowType(test_type_person_or_something_name, (hydra.core.FieldType(hydra.core.Name("person"), cast(hydra.core.Type, hydra.core.TypeVariable(test_type_person_name))), hydra.core.FieldType(hydra.core.Name("other"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("a")))))))))))

test_type_polymorphic_wrapper_name = hydra.core.Name("PolymorphicWrapper")

test_type_polymorphic_wrapper = cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(hydra.core.Name("a"), cast(hydra.core.Type, hydra.core.TypeWrap(hydra.core.WrappedType(test_type_polymorphic_wrapper_name, cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("a")))))))))))

test_type_simple_number_name = hydra.core.Name("SimpleNumber")

test_type_simple_number = cast(hydra.core.Type, hydra.core.TypeUnion(hydra.core.RowType(test_type_simple_number_name, (hydra.core.FieldType(hydra.core.Name("int"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))))), hydra.core.FieldType(hydra.core.Name("float"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT32)))))))))

test_type_string_alias_name = hydra.core.Name("StringAlias")

test_type_string_alias = cast(hydra.core.Type, hydra.core.TypeWrap(hydra.core.WrappedType(test_type_string_alias_name, cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()))))))

test_type_symmetric_triple_name = hydra.core.Name("SymmetricTriple")

test_type_triple_name = hydra.core.Name("Triple")

test_type_symmetric_triple = cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(hydra.core.Name("v"), cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(hydra.core.Name("e"), cast(hydra.core.Type, hydra.core.TypeWrap(hydra.core.WrappedType(test_type_symmetric_triple_name, cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(cast(hydra.core.Type, hydra.core.TypeVariable(test_type_triple_name)), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("v")))))), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("e")))))), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("v")))))))))))))))

test_type_timestamp_name = hydra.core.Name("Timestamp")

test_type_timestamp = cast(hydra.core.Type, hydra.core.TypeUnion(hydra.core.RowType(test_type_timestamp_name, (hydra.core.FieldType(hydra.core.Name("unixTimeMillis"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(hydra.core.IntegerType.UINT64))))), hydra.core.FieldType(hydra.core.Name("date"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()))))))))

test_type_trace = cast(hydra.core.Type, hydra.core.TypeRecord(hydra.core.RowType(test_type_trace_name, (hydra.core.FieldType(hydra.core.Name("stack"), cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))))), hydra.core.FieldType(hydra.core.Name("messages"), cast(hydra.core.Type, hydra.core.TypeList(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))))), hydra.core.FieldType(hydra.core.Name("other"), cast(hydra.core.Type, hydra.core.TypeMap(hydra.core.MapType(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()))), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))))))))))

test_type_triple = cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(hydra.core.Name("a"), cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(hydra.core.Name("b"), cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(hydra.core.Name("c"), cast(hydra.core.Type, hydra.core.TypeRecord(hydra.core.RowType(test_type_triple_name, (hydra.core.FieldType(hydra.core.Name("first"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("a")))), hydra.core.FieldType(hydra.core.Name("second"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("b")))), hydra.core.FieldType(hydra.core.Name("third"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("c")))))))))))))))))

test_type_union_monomorphic_name = hydra.core.Name("UnionMonomorphic")

test_type_union_monomorphic = cast(hydra.core.Type, hydra.core.TypeUnion(hydra.core.RowType(test_type_union_monomorphic_name, (hydra.core.FieldType(hydra.core.Name("bool"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeBoolean())))), hydra.core.FieldType(hydra.core.Name("string"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), hydra.core.FieldType(hydra.core.Name("unit"), cast(hydra.core.Type, hydra.core.TypeUnit()))))))

test_type_union_polymorphic_recursive_name = hydra.core.Name("UnionPolymorphicRecursive")

test_type_union_polymorphic_recursive = cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(hydra.core.Name("a"), cast(hydra.core.Type, hydra.core.TypeUnion(hydra.core.RowType(test_type_union_polymorphic_recursive_name, (hydra.core.FieldType(hydra.core.Name("bool"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeBoolean())))), hydra.core.FieldType(hydra.core.Name("value"), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("a")))), hydra.core.FieldType(hydra.core.Name("other"), cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(cast(hydra.core.Type, hydra.core.TypeVariable(test_type_union_polymorphic_recursive_name)), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("a"))))))))))))))

test_type_unit_name = hydra.core.Name("Unit")

@lru_cache(1)
def test_type_unit() -> hydra.core.Type:
    return cast(hydra.core.Type, hydra.core.TypeRecord(hydra.core.RowType(test_type_unit_name, ())))
