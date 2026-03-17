(defpackage :hydra.test.testTypes
(:use :cl :hydra.core)
(:export :hydra_test_test_types_compare_strings_type :hydra_test_test_types_concat_type :hydra_test_test_types_either_string_or_int8_type :hydra_test_test_types_either_string_or_int8_type_name :hydra_test_test_types_test_type_person_name :hydra_test_test_types_example_projection_type :hydra_test_test_types_list_of_int16s_type :hydra_test_test_types_list_of_int8s_type :hydra_test_test_types_list_of_lists_of_strings_type :hydra_test_test_types_list_of_set_of_strings_type :hydra_test_test_types_list_of_strings_type :hydra_test_test_types_map_of_strings_to_ints_type :hydra_test_test_types_optional_int16_type :hydra_test_test_types_optional_int8_type :hydra_test_test_types_optional_string_type :hydra_test_test_types_set_of_strings_type :hydra_test_test_types_string_or_int_name :hydra_test_test_types_string_or_int_type :hydra_test_test_types_test_type_buddy_list_b_name :hydra_test_test_types_test_type_buddy_list_a :hydra_test_test_types_test_type_buddy_list_a_name :hydra_test_test_types_test_type_buddy_list_b :hydra_test_test_types_test_type_comparison :hydra_test_test_types_test_type_comparison_name :hydra_test_test_types_test_type_either :hydra_test_test_types_test_type_either_name :hydra_test_test_types_test_type_hydra_literal_type :hydra_test_test_types_test_type_hydra_literal_type_name :hydra_test_test_types_test_type_hydra_type_name :hydra_test_test_types_test_type_hydra_type :hydra_test_test_types_test_type_int_list_name :hydra_test_test_types_test_type_int_list :hydra_test_test_types_test_type_lat_lon :hydra_test_test_types_test_type_lat_lon_name :hydra_test_test_types_test_type_lat_lon_poly :hydra_test_test_types_test_type_lat_lon_poly_name :hydra_test_test_types_test_type_list_name :hydra_test_test_types_test_type_list :hydra_test_test_types_test_type_name :hydra_test_test_types_test_type_number :hydra_test_test_types_test_type_number_name :hydra_test_test_types_test_type_person :hydra_test_test_types_test_type_person_or_something :hydra_test_test_types_test_type_person_or_something_name :hydra_test_test_types_test_type_polymorphic_wrapper :hydra_test_test_types_test_type_polymorphic_wrapper_name :hydra_test_test_types_test_type_simple_number :hydra_test_test_types_test_type_simple_number_name :hydra_test_test_types_test_type_string_alias :hydra_test_test_types_test_type_string_alias_name :hydra_test_test_types_test_type_triple_name :hydra_test_test_types_test_type_symmetric_triple :hydra_test_test_types_test_type_symmetric_triple_name :hydra_test_test_types_test_type_timestamp :hydra_test_test_types_test_type_timestamp_name :hydra_test_test_types_test_type_triple :hydra_test_test_types_test_type_union_monomorphic :hydra_test_test_types_test_type_union_monomorphic_name :hydra_test_test_types_test_type_union_polymorphic_recursive_name :hydra_test_test_types_test_type_union_polymorphic_recursive :hydra_test_test_types_test_type_unit :hydra_test_test_types_test_type_unit_name))

(in-package :hydra.test.testTypes)

(cl:defvar hydra_test_test_types_compare_strings_type (list :function (make-hydra_core_function_type (list :literal (list :string cl:nil)) (list :literal (list :string cl:nil)))))

(cl:defvar hydra_test_test_types_concat_type (list :function (make-hydra_core_function_type (list :literal (list :string cl:nil)) (list :function (make-hydra_core_function_type (list :literal (list :string cl:nil)) (list :literal (list :string cl:nil)))))))

(cl:defvar hydra_test_test_types_either_string_or_int8_type (list :union (cl:list (make-hydra_core_field_type "left" (list :literal (list :string cl:nil))) (make-hydra_core_field_type "right" (list :literal (list :integer (list :int8 cl:nil)))))))

(cl:defvar hydra_test_test_types_either_string_or_int8_type_name "EitherStringOrInt8")

(cl:defvar hydra_test_test_types_test_type_person_name "Person")

(cl:defvar hydra_test_test_types_example_projection_type (list :function (make-hydra_core_function_type (list :variable hydra_test_test_types_test_type_person_name) (list :literal (list :string cl:nil)))))

(cl:defvar hydra_test_test_types_list_of_int16s_type (list :list (list :literal (list :integer (list :int16 cl:nil)))))

(cl:defvar hydra_test_test_types_list_of_int8s_type (list :list (list :literal (list :integer (list :int8 cl:nil)))))

(cl:defvar hydra_test_test_types_list_of_lists_of_strings_type (list :list (list :list (list :literal (list :string cl:nil)))))

(cl:defvar hydra_test_test_types_list_of_set_of_strings_type (list :list (list :set (list :literal (list :string cl:nil)))))

(cl:defvar hydra_test_test_types_list_of_strings_type (list :list (list :literal (list :string cl:nil))))

(cl:defvar hydra_test_test_types_map_of_strings_to_ints_type (list :map (make-hydra_core_map_type (list :literal (list :string cl:nil)) (list :literal (list :integer (list :int32 cl:nil))))))

(cl:defvar hydra_test_test_types_optional_int16_type (list :maybe (list :literal (list :integer (list :int16 cl:nil)))))

(cl:defvar hydra_test_test_types_optional_int8_type (list :maybe (list :literal (list :integer (list :int8 cl:nil)))))

(cl:defvar hydra_test_test_types_optional_string_type (list :maybe (list :literal (list :string cl:nil))))

(cl:defvar hydra_test_test_types_set_of_strings_type (list :set (list :literal (list :string cl:nil))))

(cl:defvar hydra_test_test_types_string_or_int_name "StringOrInt")

(cl:defvar hydra_test_test_types_string_or_int_type (list :union (cl:list (make-hydra_core_field_type "left" (list :literal (list :string cl:nil))) (make-hydra_core_field_type "right" (list :literal (list :integer (list :int32 cl:nil)))))))

(cl:defvar hydra_test_test_types_test_type_buddy_list_b_name "BuddyListB")

(cl:defvar hydra_test_test_types_test_type_buddy_list_a (list :forall (make-hydra_core_forall_type "a" (list :record (cl:list (make-hydra_core_field_type "head" (list :variable "a")) (make-hydra_core_field_type "tail" (list :maybe (list :application (make-hydra_core_application_type (list :variable hydra_test_test_types_test_type_buddy_list_b_name) (list :variable "a"))))))))))

(cl:defvar hydra_test_test_types_test_type_buddy_list_a_name "BuddyListA")

(cl:defvar hydra_test_test_types_test_type_buddy_list_b (list :forall (make-hydra_core_forall_type "a" (list :record (cl:list (make-hydra_core_field_type "head" (list :variable "a")) (make-hydra_core_field_type "tail" (list :maybe (list :application (make-hydra_core_application_type (list :variable hydra_test_test_types_test_type_buddy_list_a_name) (list :variable "a"))))))))))

(cl:defvar hydra_test_test_types_test_type_comparison (list :union (cl:list (make-hydra_core_field_type "lessThan" (list :unit cl:nil)) (make-hydra_core_field_type "equalTo" (list :unit cl:nil)) (make-hydra_core_field_type "greaterThan" (list :unit cl:nil)))))

(cl:defvar hydra_test_test_types_test_type_comparison_name "Comparison")

(cl:defvar hydra_test_test_types_test_type_either (list :forall (make-hydra_core_forall_type "a" (list :forall (make-hydra_core_forall_type "b" (list :union (cl:list (make-hydra_core_field_type "left" (list :variable "a")) (make-hydra_core_field_type "right" (list :variable "b")))))))))

(cl:defvar hydra_test_test_types_test_type_either_name "Either")

(cl:defvar hydra_test_test_types_test_type_hydra_literal_type (list :union (cl:list (make-hydra_core_field_type "boolean" (list :literal (list :boolean cl:nil))) (make-hydra_core_field_type "string" (list :literal (list :string cl:nil))))))

(cl:defvar hydra_test_test_types_test_type_hydra_literal_type_name "HydraLiteralType")

(cl:defvar hydra_test_test_types_test_type_hydra_type_name "HydraType")

(cl:defvar hydra_test_test_types_test_type_hydra_type (list :union (cl:list (make-hydra_core_field_type "literal" (list :variable hydra_test_test_types_test_type_hydra_literal_type_name)) (make-hydra_core_field_type "list" (list :variable hydra_test_test_types_test_type_hydra_type_name)))))

(cl:defvar hydra_test_test_types_test_type_int_list_name "IntList")

(cl:defvar hydra_test_test_types_test_type_int_list (list :record (cl:list (make-hydra_core_field_type "head" (list :literal (list :integer (list :int32 cl:nil)))) (make-hydra_core_field_type "tail" (list :maybe (list :variable hydra_test_test_types_test_type_int_list_name))))))

(cl:defvar hydra_test_test_types_test_type_lat_lon (list :record (cl:list (make-hydra_core_field_type "lat" (list :literal (list :float (list :float32 cl:nil)))) (make-hydra_core_field_type "lon" (list :literal (list :float (list :float32 cl:nil)))))))

(cl:defvar hydra_test_test_types_test_type_lat_lon_name "LatLon")

(cl:defvar hydra_test_test_types_test_type_lat_lon_poly (list :forall (make-hydra_core_forall_type "a" (list :record (cl:list (make-hydra_core_field_type "lat" (list :variable "a")) (make-hydra_core_field_type "lon" (list :variable "a")))))))

(cl:defvar hydra_test_test_types_test_type_lat_lon_poly_name "LatLonPoly")

(cl:defvar hydra_test_test_types_test_type_list_name "List")

(cl:defvar hydra_test_test_types_test_type_list (list :forall (make-hydra_core_forall_type "a" (list :record (cl:list (make-hydra_core_field_type "head" (list :variable "a")) (make-hydra_core_field_type "tail" (list :maybe (list :application (make-hydra_core_application_type (list :variable hydra_test_test_types_test_type_list_name) (list :variable "a"))))))))))

(cl:defvar hydra_test_test_types_test_type_name "Test")

(cl:defvar hydra_test_test_types_test_type_number (list :union (cl:list (make-hydra_core_field_type "int" (list :literal (list :integer (list :int32 cl:nil)))) (make-hydra_core_field_type "float" (list :literal (list :float (list :float32 cl:nil)))))))

(cl:defvar hydra_test_test_types_test_type_number_name "Number")

(cl:defvar hydra_test_test_types_test_type_person (list :record (cl:list (make-hydra_core_field_type "firstName" (list :literal (list :string cl:nil))) (make-hydra_core_field_type "lastName" (list :literal (list :string cl:nil))) (make-hydra_core_field_type "age" (list :literal (list :integer (list :int32 cl:nil)))))))

(cl:defvar hydra_test_test_types_test_type_person_or_something (list :forall (make-hydra_core_forall_type "a" (list :union (cl:list (make-hydra_core_field_type "person" (list :variable hydra_test_test_types_test_type_person_name)) (make-hydra_core_field_type "other" (list :variable "a")))))))

(cl:defvar hydra_test_test_types_test_type_person_or_something_name "PersonOrSomething")

(cl:defvar hydra_test_test_types_test_type_polymorphic_wrapper (list :forall (make-hydra_core_forall_type "a" (list :wrap (list :list (list :variable "a"))))))

(cl:defvar hydra_test_test_types_test_type_polymorphic_wrapper_name "PolymorphicWrapper")

(cl:defvar hydra_test_test_types_test_type_simple_number (list :union (cl:list (make-hydra_core_field_type "int" (list :literal (list :integer (list :int32 cl:nil)))) (make-hydra_core_field_type "float" (list :literal (list :float (list :float32 cl:nil)))))))

(cl:defvar hydra_test_test_types_test_type_simple_number_name "SimpleNumber")

(cl:defvar hydra_test_test_types_test_type_string_alias (list :wrap (list :literal (list :string cl:nil))))

(cl:defvar hydra_test_test_types_test_type_string_alias_name "StringAlias")

(cl:defvar hydra_test_test_types_test_type_triple_name "Triple")

(cl:defvar hydra_test_test_types_test_type_symmetric_triple (list :forall (make-hydra_core_forall_type "v" (list :forall (make-hydra_core_forall_type "e" (list :wrap (list :application (make-hydra_core_application_type (list :application (make-hydra_core_application_type (list :application (make-hydra_core_application_type (list :variable hydra_test_test_types_test_type_triple_name) (list :variable "v"))) (list :variable "e"))) (list :variable "v")))))))))

(cl:defvar hydra_test_test_types_test_type_symmetric_triple_name "SymmetricTriple")

(cl:defvar hydra_test_test_types_test_type_timestamp (list :union (cl:list (make-hydra_core_field_type "unixTimeMillis" (list :literal (list :integer (list :uint64 cl:nil)))) (make-hydra_core_field_type "date" (list :literal (list :string cl:nil))))))

(cl:defvar hydra_test_test_types_test_type_timestamp_name "Timestamp")

(cl:defvar hydra_test_test_types_test_type_triple (list :forall (make-hydra_core_forall_type "a" (list :forall (make-hydra_core_forall_type "b" (list :forall (make-hydra_core_forall_type "c" (list :record (cl:list (make-hydra_core_field_type "first" (list :variable "a")) (make-hydra_core_field_type "second" (list :variable "b")) (make-hydra_core_field_type "third" (list :variable "c")))))))))))

(cl:defvar hydra_test_test_types_test_type_union_monomorphic (list :union (cl:list (make-hydra_core_field_type "bool" (list :literal (list :boolean cl:nil))) (make-hydra_core_field_type "string" (list :literal (list :string cl:nil))) (make-hydra_core_field_type "unit" (list :unit cl:nil)))))

(cl:defvar hydra_test_test_types_test_type_union_monomorphic_name "UnionMonomorphic")

(cl:defvar hydra_test_test_types_test_type_union_polymorphic_recursive_name "UnionPolymorphicRecursive")

(cl:defvar hydra_test_test_types_test_type_union_polymorphic_recursive (list :forall (make-hydra_core_forall_type "a" (list :union (cl:list (make-hydra_core_field_type "bool" (list :literal (list :boolean cl:nil))) (make-hydra_core_field_type "value" (list :variable "a")) (make-hydra_core_field_type "other" (list :application (make-hydra_core_application_type (list :variable hydra_test_test_types_test_type_union_polymorphic_recursive_name) (list :variable "a")))))))))

(cl:defvar hydra_test_test_types_test_type_unit (list :record (cl:list)))

(cl:defvar hydra_test_test_types_test_type_unit_name "Unit")
