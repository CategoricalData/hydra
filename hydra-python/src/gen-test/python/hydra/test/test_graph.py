"""A module defining the graph used in the test suite."""

from __future__ import annotations
import hydra.core
import hydra.graph
import hydra.module

test_type_lat_lon_name = hydra.core.Name("LatLon")

test_type_lat_lon_poly_name = hydra.core.Name("LatLonPoly")

def latlon_record(lat: float, lon: float) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record("test_type_lat_lon_name", tuple([
      hydra.core.Field(hydra.core.Name("lat"), hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat32(lat)))),
      hydra.core.Field(hydra.core.Name("lon"), hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat32(lon))))])))

test_type_lat_lon = hydra.core.TypeRecord(hydra.core.RowType("test_type_lat_lon_name", tuple([
  hydra.core.FieldType(hydra.core.Name("lat"), hydra.core.TypeLiteral(hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT32))),
  hydra.core.FieldType(hydra.core.Name("lon"), hydra.core.TypeLiteral(hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT32)))])))

test_type_lat_lon_poly = hydra.core.TypeForall(hydra.core.ForallType(hydra.core.Name("a"), hydra.core.TypeRecord(hydra.core.RowType("test_type_lat_lon_poly_name", tuple([
  hydra.core.FieldType(hydra.core.Name("lat"), hydra.core.TypeVariable(hydra.core.Name("a"))),
  hydra.core.FieldType(hydra.core.Name("lon"), hydra.core.TypeVariable(hydra.core.Name("a")))])))))

test_type_string_alias = hydra.core.TypeWrap(hydra.core.WrappedType("test_type_string_alias_name", hydra.core.TypeLiteral(hydra.core.LiteralTypeString(None))))

test_type_string_alias_name = hydra.core.Name("StringTypeAlias")

test_type_polymorphic_wrapper = hydra.core.TypeForall(hydra.core.ForallType(hydra.core.Name("a"), hydra.core.TypeWrap(hydra.core.WrappedType("test_type_polymorphic_wrapper_name", hydra.core.TypeList(hydra.core.TypeVariable(hydra.core.Name("a")))))))

test_type_polymorphic_wrapper_name = hydra.core.Name("PolymorphicWrapper")

test_element_arthur = hydra.core.Binding(hydra.core.Name("firstName"), "test_data_arthur", hydra.core.TypeScheme(tuple([]), hydra.core.TypeVariable("test_type_person_name")))

test_element_first_name = hydra.core.Binding(hydra.core.Name("firstName"), hydra.core.TermFunction(hydra.core.FunctionElimination(hydra.core.EliminationRecord(hydra.core.Projection("test_type_person_name", hydra.core.Name("firstName"))))), hydra.core.TypeScheme(tuple([]), hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable("test_type_person_name"), hydra.core.TypeLiteral(hydra.core.LiteralTypeString(None))))))

test_namespace = hydra.module.Namespace("testGraph")

test_schema_namespace = hydra.module.Namespace("testSchemaGraph")

test_data_arthur = hydra.core.TermRecord(hydra.core.Record("test_type_person_name", tuple([
  hydra.core.Field(hydra.core.Name("firstName"), hydra.core.TermLiteral(hydra.core.LiteralString("Arthur"))),
  hydra.core.Field(hydra.core.Name("lastName"), hydra.core.TermLiteral(hydra.core.LiteralString("Dent"))),
  hydra.core.Field(hydra.core.Name("age"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42))))])))

test_type_buddy_list_a = hydra.core.TypeForall(hydra.core.ForallType(hydra.core.Name("a"), hydra.core.TypeRecord(hydra.core.RowType("test_type_buddy_list_a_name", tuple([
  hydra.core.FieldType(hydra.core.Name("head"), hydra.core.TypeVariable(hydra.core.Name("a"))),
  hydra.core.FieldType(hydra.core.Name("tail"), hydra.core.TypeMaybe(hydra.core.TypeApplication(hydra.core.ApplicationType(hydra.core.TypeVariable("test_type_buddy_list_b_name"), hydra.core.TypeVariable(hydra.core.Name("a"))))))])))))

test_type_buddy_list_a_name = hydra.core.Name("BuddyListA")

test_type_buddy_list_b = hydra.core.TypeForall(hydra.core.ForallType(hydra.core.Name("a"), hydra.core.TypeRecord(hydra.core.RowType("test_type_buddy_list_b_name", tuple([
  hydra.core.FieldType(hydra.core.Name("head"), hydra.core.TypeVariable(hydra.core.Name("a"))),
  hydra.core.FieldType(hydra.core.Name("tail"), hydra.core.TypeMaybe(hydra.core.TypeApplication(hydra.core.ApplicationType(hydra.core.TypeVariable("test_type_buddy_list_a_name"), hydra.core.TypeVariable(hydra.core.Name("a"))))))])))))

test_type_buddy_list_b_name = hydra.core.Name("BuddyListB")

test_type_comparison = hydra.core.TypeUnion(hydra.core.RowType("test_type_comparison_name", tuple([
  hydra.core.FieldType(hydra.core.Name("lessThan"), hydra.core.TypeUnit(None)),
  hydra.core.FieldType(hydra.core.Name("equalTo"), hydra.core.TypeUnit(None)),
  hydra.core.FieldType(hydra.core.Name("greaterThan"), hydra.core.TypeUnit(None))])))

test_type_comparison_name = hydra.core.Name("Comparison")

test_type_int_list = hydra.core.TypeRecord(hydra.core.RowType("test_type_int_list_name", tuple([
  hydra.core.FieldType(hydra.core.Name("head"), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))),
  hydra.core.FieldType(hydra.core.Name("tail"), hydra.core.TypeMaybe(hydra.core.TypeVariable("test_type_int_list_name")))])))

test_type_int_list_name = hydra.core.Name("IntList")

test_type_hydra_literal_type = hydra.core.TypeUnion(hydra.core.RowType("test_type_hydra_literal_type_name", tuple([
  hydra.core.FieldType(hydra.core.Name("boolean"), hydra.core.TypeLiteral(hydra.core.LiteralTypeBoolean(None))),
  hydra.core.FieldType(hydra.core.Name("string"), hydra.core.TypeLiteral(hydra.core.LiteralTypeString(None)))])))

test_type_hydra_literal_type_name = hydra.core.Name("HydraLiteralType")

test_type_hydra_type = hydra.core.TypeUnion(hydra.core.RowType("test_type_hydra_type_name", tuple([
  hydra.core.FieldType(hydra.core.Name("literal"), hydra.core.TypeVariable("test_type_hydra_literal_type_name")),
  hydra.core.FieldType(hydra.core.Name("list"), hydra.core.TypeVariable("test_type_hydra_type_name"))])))

test_type_hydra_type_name = hydra.core.Name("HydraType")

test_type_list = hydra.core.TypeForall(hydra.core.ForallType(hydra.core.Name("a"), hydra.core.TypeRecord(hydra.core.RowType("test_type_list_name", tuple([
  hydra.core.FieldType(hydra.core.Name("head"), hydra.core.TypeVariable(hydra.core.Name("a"))),
  hydra.core.FieldType(hydra.core.Name("tail"), hydra.core.TypeMaybe(hydra.core.TypeApplication(hydra.core.ApplicationType(hydra.core.TypeVariable("test_type_list_name"), hydra.core.TypeVariable(hydra.core.Name("a"))))))])))))

test_type_list_name = hydra.core.Name("List")

test_type_number = hydra.core.TypeUnion(hydra.core.RowType("test_type_number_name", tuple([
  hydra.core.FieldType(hydra.core.Name("int"), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))),
  hydra.core.FieldType(hydra.core.Name("float"), hydra.core.TypeLiteral(hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT32)))])))

test_type_number_name = hydra.core.Name("Number")

test_type_person = hydra.core.TypeRecord(hydra.core.RowType("test_type_person_name", tuple([
  hydra.core.FieldType(hydra.core.Name("firstName"), hydra.core.TypeLiteral(hydra.core.LiteralTypeString(None))),
  hydra.core.FieldType(hydra.core.Name("lastName"), hydra.core.TypeLiteral(hydra.core.LiteralTypeString(None))),
  hydra.core.FieldType(hydra.core.Name("age"), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)))])))

test_type_person_name = hydra.core.Name("Person")

test_type_person_or_something = hydra.core.TypeForall(hydra.core.ForallType(hydra.core.Name("a"), hydra.core.TypeUnion(hydra.core.RowType("test_type_person_or_something_name", tuple([
  hydra.core.FieldType(hydra.core.Name("person"), hydra.core.TypeVariable("test_type_person_name")),
  hydra.core.FieldType(hydra.core.Name("other"), hydra.core.TypeVariable(hydra.core.Name("a")))])))))

test_type_person_or_something_name = hydra.core.Name("PersonOrSomething")

test_type_simple_number = hydra.core.TypeUnion(hydra.core.RowType("test_type_simple_number_name", tuple([
  hydra.core.FieldType(hydra.core.Name("int"), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))),
  hydra.core.FieldType(hydra.core.Name("float"), hydra.core.TypeLiteral(hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT32)))])))

test_type_simple_number_name = hydra.core.Name("SimpleNumber")

test_type_timestamp = hydra.core.TypeUnion(hydra.core.RowType("test_type_timestamp_name", tuple([
  hydra.core.FieldType(hydra.core.Name("unixTimeMillis"), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.UINT64))),
  hydra.core.FieldType(hydra.core.Name("date"), hydra.core.TypeLiteral(hydra.core.LiteralTypeString(None)))])))

test_type_timestamp_name = hydra.core.Name("Timestamp")

test_type_union_monomorphic = hydra.core.TypeUnion(hydra.core.RowType("test_type_union_monomorphic_name", tuple([
  hydra.core.FieldType(hydra.core.Name("bool"), hydra.core.TypeLiteral(hydra.core.LiteralTypeBoolean(None))),
  hydra.core.FieldType(hydra.core.Name("string"), hydra.core.TypeLiteral(hydra.core.LiteralTypeString(None))),
  hydra.core.FieldType(hydra.core.Name("unit"), hydra.core.TypeUnit(None))])))

test_type_union_monomorphic_name = hydra.core.Name("UnionMonomorphic")

test_type_union_polymorphic_recursive = hydra.core.TypeForall(hydra.core.ForallType(hydra.core.Name("a"), hydra.core.TypeUnion(hydra.core.RowType("test_type_union_polymorphic_recursive_name", tuple([
  hydra.core.FieldType(hydra.core.Name("bool"), hydra.core.TypeLiteral(hydra.core.LiteralTypeBoolean(None))),
  hydra.core.FieldType(hydra.core.Name("value"), hydra.core.TypeVariable(hydra.core.Name("a"))),
  hydra.core.FieldType(hydra.core.Name("other"), hydra.core.TypeApplication(hydra.core.ApplicationType(hydra.core.TypeVariable("test_type_union_polymorphic_recursive_name"), hydra.core.TypeVariable(hydra.core.Name("a")))))])))))

test_type_union_polymorphic_recursive_name = hydra.core.Name("UnionPolymorphicRecursive")

test_type_unit = hydra.core.TypeRecord(hydra.core.RowType("test_type_unit_name", tuple([])))

test_type_unit_name = hydra.core.Name("Unit")
