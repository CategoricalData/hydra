(require 'cl-lib)

(require 'hydra.core)

(defvar hydra_test_test_types_compare_strings_type (list :function (make-hydra_core_function_type (list :literal (list :string nil)) (list :literal (list :string nil)))))

(defvar hydra_test_test_types_concat_type (list :function (make-hydra_core_function_type (list :literal (list :string nil)) (list :function (make-hydra_core_function_type (list :literal (list :string nil)) (list :literal (list :string nil)))))))

(defvar hydra_test_test_types_either_string_or_int8_type (list :union (list (make-hydra_core_field_type "left" (list :literal (list :string nil))) (make-hydra_core_field_type "right" (list :literal (list :integer (list :int8 nil)))))))

(defvar hydra_test_test_types_either_string_or_int8_type_name "EitherStringOrInt8")

(defvar hydra_test_test_types_test_type_person_name "Person")

(defvar hydra_test_test_types_example_projection_type (list :function (make-hydra_core_function_type (list :variable hydra_test_test_types_test_type_person_name) (list :literal (list :string nil)))))

(defvar hydra_test_test_types_list_of_int16s_type (list :list (list :literal (list :integer (list :int16 nil)))))

(defvar hydra_test_test_types_list_of_int8s_type (list :list (list :literal (list :integer (list :int8 nil)))))

(defvar hydra_test_test_types_list_of_lists_of_strings_type (list :list (list :list (list :literal (list :string nil)))))

(defvar hydra_test_test_types_list_of_set_of_strings_type (list :list (list :set (list :literal (list :string nil)))))

(defvar hydra_test_test_types_list_of_strings_type (list :list (list :literal (list :string nil))))

(defvar hydra_test_test_types_map_of_strings_to_ints_type (list :map (make-hydra_core_map_type (list :literal (list :string nil)) (list :literal (list :integer (list :int32 nil))))))

(defvar hydra_test_test_types_optional_int16_type (list :maybe (list :literal (list :integer (list :int16 nil)))))

(defvar hydra_test_test_types_optional_int8_type (list :maybe (list :literal (list :integer (list :int8 nil)))))

(defvar hydra_test_test_types_optional_string_type (list :maybe (list :literal (list :string nil))))

(defvar hydra_test_test_types_set_of_strings_type (list :set (list :literal (list :string nil))))

(defvar hydra_test_test_types_string_or_int_name "StringOrInt")

(defvar hydra_test_test_types_string_or_int_type (list :union (list (make-hydra_core_field_type "left" (list :literal (list :string nil))) (make-hydra_core_field_type "right" (list :literal (list :integer (list :int32 nil)))))))

(defvar hydra_test_test_types_test_type_buddy_list_b_name "BuddyListB")

(defvar hydra_test_test_types_test_type_buddy_list_a (list :forall (make-hydra_core_forall_type "a" (list :record (list (make-hydra_core_field_type "head" (list :variable "a")) (make-hydra_core_field_type "tail" (list :maybe (list :application (make-hydra_core_application_type (list :variable hydra_test_test_types_test_type_buddy_list_b_name) (list :variable "a"))))))))))

(defvar hydra_test_test_types_test_type_buddy_list_a_name "BuddyListA")

(defvar hydra_test_test_types_test_type_buddy_list_b (list :forall (make-hydra_core_forall_type "a" (list :record (list (make-hydra_core_field_type "head" (list :variable "a")) (make-hydra_core_field_type "tail" (list :maybe (list :application (make-hydra_core_application_type (list :variable hydra_test_test_types_test_type_buddy_list_a_name) (list :variable "a"))))))))))

(defvar hydra_test_test_types_test_type_comparison (list :union (list (make-hydra_core_field_type "lessThan" (list :unit nil)) (make-hydra_core_field_type "equalTo" (list :unit nil)) (make-hydra_core_field_type "greaterThan" (list :unit nil)))))

(defvar hydra_test_test_types_test_type_comparison_name "Comparison")

(defvar hydra_test_test_types_test_type_either (list :forall (make-hydra_core_forall_type "a" (list :forall (make-hydra_core_forall_type "b" (list :union (list (make-hydra_core_field_type "left" (list :variable "a")) (make-hydra_core_field_type "right" (list :variable "b")))))))))

(defvar hydra_test_test_types_test_type_either_name "Either")

(defvar hydra_test_test_types_test_type_hydra_literal_type (list :union (list (make-hydra_core_field_type "boolean" (list :literal (list :boolean nil))) (make-hydra_core_field_type "string" (list :literal (list :string nil))))))

(defvar hydra_test_test_types_test_type_hydra_literal_type_name "HydraLiteralType")

(defvar hydra_test_test_types_test_type_hydra_type_name "HydraType")

(defvar hydra_test_test_types_test_type_hydra_type (list :union (list (make-hydra_core_field_type "literal" (list :variable hydra_test_test_types_test_type_hydra_literal_type_name)) (make-hydra_core_field_type "list" (list :variable hydra_test_test_types_test_type_hydra_type_name)))))

(defvar hydra_test_test_types_test_type_int_list_name "IntList")

(defvar hydra_test_test_types_test_type_int_list (list :record (list (make-hydra_core_field_type "head" (list :literal (list :integer (list :int32 nil)))) (make-hydra_core_field_type "tail" (list :maybe (list :variable hydra_test_test_types_test_type_int_list_name))))))

(defvar hydra_test_test_types_test_type_lat_lon (list :record (list (make-hydra_core_field_type "lat" (list :literal (list :float (list :float32 nil)))) (make-hydra_core_field_type "lon" (list :literal (list :float (list :float32 nil)))))))

(defvar hydra_test_test_types_test_type_lat_lon_name "LatLon")

(defvar hydra_test_test_types_test_type_lat_lon_poly (list :forall (make-hydra_core_forall_type "a" (list :record (list (make-hydra_core_field_type "lat" (list :variable "a")) (make-hydra_core_field_type "lon" (list :variable "a")))))))

(defvar hydra_test_test_types_test_type_lat_lon_poly_name "LatLonPoly")

(defvar hydra_test_test_types_test_type_list_name "List")

(defvar hydra_test_test_types_test_type_list (list :forall (make-hydra_core_forall_type "a" (list :record (list (make-hydra_core_field_type "head" (list :variable "a")) (make-hydra_core_field_type "tail" (list :maybe (list :application (make-hydra_core_application_type (list :variable hydra_test_test_types_test_type_list_name) (list :variable "a"))))))))))

(defvar hydra_test_test_types_test_type_name "Test")

(defvar hydra_test_test_types_test_type_number (list :union (list (make-hydra_core_field_type "int" (list :literal (list :integer (list :int32 nil)))) (make-hydra_core_field_type "float" (list :literal (list :float (list :float32 nil)))))))

(defvar hydra_test_test_types_test_type_number_name "Number")

(defvar hydra_test_test_types_test_type_person (list :record (list (make-hydra_core_field_type "firstName" (list :literal (list :string nil))) (make-hydra_core_field_type "lastName" (list :literal (list :string nil))) (make-hydra_core_field_type "age" (list :literal (list :integer (list :int32 nil)))))))

(defvar hydra_test_test_types_test_type_person_or_something (list :forall (make-hydra_core_forall_type "a" (list :union (list (make-hydra_core_field_type "person" (list :variable hydra_test_test_types_test_type_person_name)) (make-hydra_core_field_type "other" (list :variable "a")))))))

(defvar hydra_test_test_types_test_type_person_or_something_name "PersonOrSomething")

(defvar hydra_test_test_types_test_type_polymorphic_wrapper (list :forall (make-hydra_core_forall_type "a" (list :wrap (list :list (list :variable "a"))))))

(defvar hydra_test_test_types_test_type_polymorphic_wrapper_name "PolymorphicWrapper")

(defvar hydra_test_test_types_test_type_simple_number (list :union (list (make-hydra_core_field_type "int" (list :literal (list :integer (list :int32 nil)))) (make-hydra_core_field_type "float" (list :literal (list :float (list :float32 nil)))))))

(defvar hydra_test_test_types_test_type_simple_number_name "SimpleNumber")

(defvar hydra_test_test_types_test_type_string_alias (list :wrap (list :literal (list :string nil))))

(defvar hydra_test_test_types_test_type_string_alias_name "StringAlias")

(defvar hydra_test_test_types_test_type_triple_name "Triple")

(defvar hydra_test_test_types_test_type_symmetric_triple (list :forall (make-hydra_core_forall_type "v" (list :forall (make-hydra_core_forall_type "e" (list :wrap (list :application (make-hydra_core_application_type (list :application (make-hydra_core_application_type (list :application (make-hydra_core_application_type (list :variable hydra_test_test_types_test_type_triple_name) (list :variable "v"))) (list :variable "e"))) (list :variable "v")))))))))

(defvar hydra_test_test_types_test_type_symmetric_triple_name "SymmetricTriple")

(defvar hydra_test_test_types_test_type_timestamp (list :union (list (make-hydra_core_field_type "unixTimeMillis" (list :literal (list :integer (list :uint64 nil)))) (make-hydra_core_field_type "date" (list :literal (list :string nil))))))

(defvar hydra_test_test_types_test_type_timestamp_name "Timestamp")

(defvar hydra_test_test_types_test_type_triple (list :forall (make-hydra_core_forall_type "a" (list :forall (make-hydra_core_forall_type "b" (list :forall (make-hydra_core_forall_type "c" (list :record (list (make-hydra_core_field_type "first" (list :variable "a")) (make-hydra_core_field_type "second" (list :variable "b")) (make-hydra_core_field_type "third" (list :variable "c")))))))))))

(defvar hydra_test_test_types_test_type_union_monomorphic (list :union (list (make-hydra_core_field_type "bool" (list :literal (list :boolean nil))) (make-hydra_core_field_type "string" (list :literal (list :string nil))) (make-hydra_core_field_type "unit" (list :unit nil)))))

(defvar hydra_test_test_types_test_type_union_monomorphic_name "UnionMonomorphic")

(defvar hydra_test_test_types_test_type_union_polymorphic_recursive_name "UnionPolymorphicRecursive")

(defvar hydra_test_test_types_test_type_union_polymorphic_recursive (list :forall (make-hydra_core_forall_type "a" (list :union (list (make-hydra_core_field_type "bool" (list :literal (list :boolean nil))) (make-hydra_core_field_type "value" (list :variable "a")) (make-hydra_core_field_type "other" (list :application (make-hydra_core_application_type (list :variable hydra_test_test_types_test_type_union_polymorphic_recursive_name) (list :variable "a")))))))))

(defvar hydra_test_test_types_test_type_unit (list :record (list)))

(defvar hydra_test_test_types_test_type_unit_name "Unit")

(provide 'hydra.test.testTypes)
