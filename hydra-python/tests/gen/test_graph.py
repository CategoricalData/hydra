

"""A module defining the graph used in the test suite."""

from __future__ import annotations

import hydra.gen.core
import hydra.gen.graph
import hydra.gen.module

test_type_lat_lon_name = hydra.gen.core.Name("LatLon")

test_type_lat_lon_poly_name = hydra.gen.core.Name("LatLonPoly")

def latlon_record(lat: float, lon: float) -> hydra.gen.core.Term:
    return hydra.gen.core.TermRecord(hydra.gen.core.Record("test_type_lat_lon_name", tuple([
      hydra.gen.core.Field(hydra.gen.core.Name("lat"), hydra.gen.core.TermLiteral(hydra.gen.core.LiteralFloat(hydra.gen.core.FloatValueFloat32(lat)))),
      hydra.gen.core.Field(hydra.gen.core.Name("lon"), hydra.gen.core.TermLiteral(hydra.gen.core.LiteralFloat(hydra.gen.core.FloatValueFloat32(lon))))])))

test_type_lat_lon = hydra.gen.core.TypeRecord(hydra.gen.core.RowType("test_type_lat_lon_name", tuple([
  hydra.gen.core.FieldType(hydra.gen.core.Name("lat"), hydra.gen.core.TypeLiteral(hydra.gen.core.LiteralTypeFloat(hydra.gen.core.FloatType.FLOAT32))),
  hydra.gen.core.FieldType(hydra.gen.core.Name("lon"), hydra.gen.core.TypeLiteral(hydra.gen.core.LiteralTypeFloat(hydra.gen.core.FloatType.FLOAT32)))])))

test_type_lat_lon_poly = hydra.gen.core.TypeForall(hydra.gen.core.ForallType(hydra.gen.core.Name("a"), hydra.gen.core.TypeRecord(hydra.gen.core.RowType("test_type_lat_lon_poly_name", tuple([
  hydra.gen.core.FieldType(hydra.gen.core.Name("lat"), hydra.gen.core.TypeVariable(hydra.gen.core.Name("a"))),
  hydra.gen.core.FieldType(hydra.gen.core.Name("lon"), hydra.gen.core.TypeVariable(hydra.gen.core.Name("a")))])))))

test_type_string_alias = hydra.gen.core.TypeWrap(hydra.gen.core.WrappedType("test_type_string_alias_name", hydra.gen.core.TypeLiteral(hydra.gen.core.LiteralTypeString(hydra.gen.core.Unit()))))

test_type_string_alias_name = hydra.gen.core.Name("StringTypeAlias")

test_type_polymorphic_wrapper = hydra.gen.core.TypeForall(hydra.gen.core.ForallType(hydra.gen.core.Name("a"), hydra.gen.core.TypeWrap(hydra.gen.core.WrappedType("test_type_polymorphic_wrapper_name", hydra.gen.core.TypeList(hydra.gen.core.TypeVariable(hydra.gen.core.Name("a")))))))

test_type_polymorphic_wrapper_name = hydra.gen.core.Name("PolymorphicWrapper")

test_element_arthur = hydra.gen.graph.Element(hydra.gen.core.Name("firstName"), "test_data_arthur", hydra.gen.core.TypeScheme(tuple([]), hydra.gen.core.TypeVariable("test_type_person_name")))

test_element_first_name = hydra.gen.graph.Element(hydra.gen.core.Name("firstName"), hydra.gen.core.TermFunction(hydra.gen.core.FunctionElimination(hydra.gen.core.EliminationRecord(hydra.gen.core.Projection("test_type_person_name", hydra.gen.core.Name("firstName"))))), hydra.gen.core.TypeScheme(tuple([]), hydra.gen.core.TypeFunction(hydra.gen.core.FunctionType(hydra.gen.core.TypeVariable("test_type_person_name"), hydra.gen.core.TypeLiteral(hydra.gen.core.LiteralTypeString(hydra.gen.core.Unit()))))))

test_namespace = hydra.gen.module.Namespace("testGraph")

test_schema_namespace = hydra.gen.module.Namespace("testSchemaGraph")

test_data_arthur = hydra.gen.core.TermRecord(hydra.gen.core.Record("test_type_person_name", tuple([
  hydra.gen.core.Field(hydra.gen.core.Name("firstName"), hydra.gen.core.TermLiteral(hydra.gen.core.LiteralString("Arthur"))),
  hydra.gen.core.Field(hydra.gen.core.Name("lastName"), hydra.gen.core.TermLiteral(hydra.gen.core.LiteralString("Dent"))),
  hydra.gen.core.Field(hydra.gen.core.Name("age"), hydra.gen.core.TermLiteral(hydra.gen.core.LiteralInteger(hydra.gen.core.IntegerValueInt32(42))))])))

test_type_buddy_list_a = hydra.gen.core.TypeForall(hydra.gen.core.ForallType(hydra.gen.core.Name("a"), hydra.gen.core.TypeRecord(hydra.gen.core.RowType("test_type_buddy_list_a_name", tuple([
  hydra.gen.core.FieldType(hydra.gen.core.Name("head"), hydra.gen.core.TypeVariable(hydra.gen.core.Name("a"))),
  hydra.gen.core.FieldType(hydra.gen.core.Name("tail"), hydra.gen.core.TypeOptional(hydra.gen.core.TypeApplication(hydra.gen.core.ApplicationType(hydra.gen.core.TypeVariable("test_type_buddy_list_b_name"), hydra.gen.core.TypeVariable(hydra.gen.core.Name("a"))))))])))))

test_type_buddy_list_a_name = hydra.gen.core.Name("BuddyListA")

test_type_buddy_list_b = hydra.gen.core.TypeForall(hydra.gen.core.ForallType(hydra.gen.core.Name("a"), hydra.gen.core.TypeRecord(hydra.gen.core.RowType("test_type_buddy_list_b_name", tuple([
  hydra.gen.core.FieldType(hydra.gen.core.Name("head"), hydra.gen.core.TypeVariable(hydra.gen.core.Name("a"))),
  hydra.gen.core.FieldType(hydra.gen.core.Name("tail"), hydra.gen.core.TypeOptional(hydra.gen.core.TypeApplication(hydra.gen.core.ApplicationType(hydra.gen.core.TypeVariable("test_type_buddy_list_a_name"), hydra.gen.core.TypeVariable(hydra.gen.core.Name("a"))))))])))))

test_type_buddy_list_b_name = hydra.gen.core.Name("BuddyListB")

test_type_comparison = hydra.gen.core.TypeUnion(hydra.gen.core.RowType("test_type_comparison_name", tuple([
  hydra.gen.core.FieldType(hydra.gen.core.Name("lessThan"), hydra.gen.core.TypeRecord(hydra.gen.core.RowType(hydra.gen.core.Name("hydra.gen.core.Unit"), tuple([])))),
  hydra.gen.core.FieldType(hydra.gen.core.Name("equalTo"), hydra.gen.core.TypeRecord(hydra.gen.core.RowType(hydra.gen.core.Name("hydra.gen.core.Unit"), tuple([])))),
  hydra.gen.core.FieldType(hydra.gen.core.Name("greaterThan"), hydra.gen.core.TypeRecord(hydra.gen.core.RowType(hydra.gen.core.Name("hydra.gen.core.Unit"), tuple([]))))])))

test_type_comparison_name = hydra.gen.core.Name("Comparison")

test_type_int_list = hydra.gen.core.TypeRecord(hydra.gen.core.RowType("test_type_int_list_name", tuple([
  hydra.gen.core.FieldType(hydra.gen.core.Name("head"), hydra.gen.core.TypeLiteral(hydra.gen.core.LiteralTypeInteger(hydra.gen.core.IntegerType.INT32))),
  hydra.gen.core.FieldType(hydra.gen.core.Name("tail"), hydra.gen.core.TypeOptional(hydra.gen.core.TypeVariable("test_type_int_list_name")))])))

test_type_int_list_name = hydra.gen.core.Name("IntList")

test_type_hydra_literal_type = hydra.gen.core.TypeUnion(hydra.gen.core.RowType("test_type_hydra_literal_type_name", tuple([
  hydra.gen.core.FieldType(hydra.gen.core.Name("boolean"), hydra.gen.core.TypeLiteral(hydra.gen.core.LiteralTypeBoolean(hydra.gen.core.Unit()))),
  hydra.gen.core.FieldType(hydra.gen.core.Name("string"), hydra.gen.core.TypeLiteral(hydra.gen.core.LiteralTypeString(hydra.gen.core.Unit())))])))

test_type_hydra_literal_type_name = hydra.gen.core.Name("HydraLiteralType")

test_type_hydra_type = hydra.gen.core.TypeUnion(hydra.gen.core.RowType("test_type_hydra_type_name", tuple([
  hydra.gen.core.FieldType(hydra.gen.core.Name("literal"), hydra.gen.core.TypeVariable("test_type_hydra_literal_type_name")),
  hydra.gen.core.FieldType(hydra.gen.core.Name("list"), hydra.gen.core.TypeVariable("test_type_hydra_type_name"))])))

test_type_hydra_type_name = hydra.gen.core.Name("HydraType")

test_type_list = hydra.gen.core.TypeForall(hydra.gen.core.ForallType(hydra.gen.core.Name("a"), hydra.gen.core.TypeRecord(hydra.gen.core.RowType("test_type_list_name", tuple([
  hydra.gen.core.FieldType(hydra.gen.core.Name("head"), hydra.gen.core.TypeVariable(hydra.gen.core.Name("a"))),
  hydra.gen.core.FieldType(hydra.gen.core.Name("tail"), hydra.gen.core.TypeOptional(hydra.gen.core.TypeApplication(hydra.gen.core.ApplicationType(hydra.gen.core.TypeVariable("test_type_list_name"), hydra.gen.core.TypeVariable(hydra.gen.core.Name("a"))))))])))))

test_type_list_name = hydra.gen.core.Name("List")

test_type_number = hydra.gen.core.TypeUnion(hydra.gen.core.RowType("test_type_number_name", tuple([
  hydra.gen.core.FieldType(hydra.gen.core.Name("int"), hydra.gen.core.TypeLiteral(hydra.gen.core.LiteralTypeInteger(hydra.gen.core.IntegerType.INT32))),
  hydra.gen.core.FieldType(hydra.gen.core.Name("float"), hydra.gen.core.TypeLiteral(hydra.gen.core.LiteralTypeFloat(hydra.gen.core.FloatType.FLOAT32)))])))

test_type_number_name = hydra.gen.core.Name("Number")

test_type_person = hydra.gen.core.TypeRecord(hydra.gen.core.RowType("test_type_person_name", tuple([
  hydra.gen.core.FieldType(hydra.gen.core.Name("firstName"), hydra.gen.core.TypeLiteral(hydra.gen.core.LiteralTypeString(hydra.gen.core.Unit()))),
  hydra.gen.core.FieldType(hydra.gen.core.Name("lastName"), hydra.gen.core.TypeLiteral(hydra.gen.core.LiteralTypeString(hydra.gen.core.Unit()))),
  hydra.gen.core.FieldType(hydra.gen.core.Name("age"), hydra.gen.core.TypeLiteral(hydra.gen.core.LiteralTypeInteger(hydra.gen.core.IntegerType.INT32)))])))

test_type_person_name = hydra.gen.core.Name("Person")

test_type_person_or_something = hydra.gen.core.TypeForall(hydra.gen.core.ForallType(hydra.gen.core.Name("a"), hydra.gen.core.TypeUnion(hydra.gen.core.RowType("test_type_person_or_something_name", tuple([
  hydra.gen.core.FieldType(hydra.gen.core.Name("person"), hydra.gen.core.TypeVariable("test_type_person_name")),
  hydra.gen.core.FieldType(hydra.gen.core.Name("other"), hydra.gen.core.TypeVariable(hydra.gen.core.Name("a")))])))))

test_type_person_or_something_name = hydra.gen.core.Name("PersonOrSomething")

test_type_simple_number = hydra.gen.core.TypeUnion(hydra.gen.core.RowType("test_type_simple_number_name", tuple([
  hydra.gen.core.FieldType(hydra.gen.core.Name("int"), hydra.gen.core.TypeLiteral(hydra.gen.core.LiteralTypeInteger(hydra.gen.core.IntegerType.INT32))),
  hydra.gen.core.FieldType(hydra.gen.core.Name("float"), hydra.gen.core.TypeLiteral(hydra.gen.core.LiteralTypeFloat(hydra.gen.core.FloatType.FLOAT32)))])))

test_type_simple_number_name = hydra.gen.core.Name("SimpleNumber")

test_type_timestamp = hydra.gen.core.TypeUnion(hydra.gen.core.RowType("test_type_timestamp_name", tuple([
  hydra.gen.core.FieldType(hydra.gen.core.Name("unixTimeMillis"), hydra.gen.core.TypeLiteral(hydra.gen.core.LiteralTypeInteger(hydra.gen.core.IntegerType.UINT64))),
  hydra.gen.core.FieldType(hydra.gen.core.Name("date"), hydra.gen.core.TypeLiteral(hydra.gen.core.LiteralTypeString(hydra.gen.core.Unit())))])))

test_type_timestamp_name = hydra.gen.core.Name("Timestamp")

test_type_union_monomorphic = hydra.gen.core.TypeUnion(hydra.gen.core.RowType("test_type_union_monomorphic_name", tuple([
  hydra.gen.core.FieldType(hydra.gen.core.Name("bool"), hydra.gen.core.TypeLiteral(hydra.gen.core.LiteralTypeBoolean(hydra.gen.core.Unit()))),
  hydra.gen.core.FieldType(hydra.gen.core.Name("string"), hydra.gen.core.TypeLiteral(hydra.gen.core.LiteralTypeString(hydra.gen.core.Unit()))),
  hydra.gen.core.FieldType(hydra.gen.core.Name("unit"), hydra.gen.core.TypeRecord(hydra.gen.core.RowType(hydra.gen.core.Name("hydra.gen.core.Unit"), tuple([]))))])))

test_type_union_monomorphic_name = hydra.gen.core.Name("UnionMonomorphic")

test_type_union_polymorphic_recursive = hydra.gen.core.TypeForall(hydra.gen.core.ForallType(hydra.gen.core.Name("a"), hydra.gen.core.TypeUnion(hydra.gen.core.RowType("test_type_union_polymorphic_recursive_name", tuple([
  hydra.gen.core.FieldType(hydra.gen.core.Name("bool"), hydra.gen.core.TypeLiteral(hydra.gen.core.LiteralTypeBoolean(hydra.gen.core.Unit()))),
  hydra.gen.core.FieldType(hydra.gen.core.Name("value"), hydra.gen.core.TypeVariable(hydra.gen.core.Name("a"))),
  hydra.gen.core.FieldType(hydra.gen.core.Name("other"), hydra.gen.core.TypeApplication(hydra.gen.core.ApplicationType(hydra.gen.core.TypeVariable("test_type_union_polymorphic_recursive_name"), hydra.gen.core.TypeVariable(hydra.gen.core.Name("a")))))])))))

test_type_union_polymorphic_recursive_name = hydra.gen.core.Name("UnionPolymorphicRecursive")
