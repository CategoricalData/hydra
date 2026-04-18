(ns hydra.dsl.error.core
  (:require [hydra.core :refer :all] [hydra.phantoms :refer :all]
))

(declare hydra_dsl_error_core_constant_condition_error hydra_dsl_error_core_constant_condition_error_location hydra_dsl_error_core_constant_condition_error_value hydra_dsl_error_core_constant_condition_error_with_location hydra_dsl_error_core_constant_condition_error_with_value hydra_dsl_error_core_duplicate_binding_error hydra_dsl_error_core_duplicate_binding_error_location hydra_dsl_error_core_duplicate_binding_error_name hydra_dsl_error_core_duplicate_binding_error_with_location hydra_dsl_error_core_duplicate_binding_error_with_name hydra_dsl_error_core_duplicate_field_error hydra_dsl_error_core_duplicate_field_error_location hydra_dsl_error_core_duplicate_field_error_name hydra_dsl_error_core_duplicate_field_error_with_location hydra_dsl_error_core_duplicate_field_error_with_name hydra_dsl_error_core_duplicate_record_type_field_names_error hydra_dsl_error_core_duplicate_record_type_field_names_error_location hydra_dsl_error_core_duplicate_record_type_field_names_error_name hydra_dsl_error_core_duplicate_record_type_field_names_error_with_location hydra_dsl_error_core_duplicate_record_type_field_names_error_with_name hydra_dsl_error_core_duplicate_union_type_field_names_error hydra_dsl_error_core_duplicate_union_type_field_names_error_location hydra_dsl_error_core_duplicate_union_type_field_names_error_name hydra_dsl_error_core_duplicate_union_type_field_names_error_with_location hydra_dsl_error_core_duplicate_union_type_field_names_error_with_name hydra_dsl_error_core_empty_case_statement_error hydra_dsl_error_core_empty_case_statement_error_location hydra_dsl_error_core_empty_case_statement_error_type_name hydra_dsl_error_core_empty_case_statement_error_with_location hydra_dsl_error_core_empty_case_statement_error_with_type_name hydra_dsl_error_core_empty_let_bindings_error hydra_dsl_error_core_empty_let_bindings_error_location hydra_dsl_error_core_empty_let_bindings_error_with_location hydra_dsl_error_core_empty_record_type_error hydra_dsl_error_core_empty_record_type_error_location hydra_dsl_error_core_empty_record_type_error_with_location hydra_dsl_error_core_empty_term_annotation_error hydra_dsl_error_core_empty_term_annotation_error_location hydra_dsl_error_core_empty_term_annotation_error_with_location hydra_dsl_error_core_empty_type_annotation_error hydra_dsl_error_core_empty_type_annotation_error_location hydra_dsl_error_core_empty_type_annotation_error_with_location hydra_dsl_error_core_empty_type_name_in_term_error hydra_dsl_error_core_empty_type_name_in_term_error_location hydra_dsl_error_core_empty_type_name_in_term_error_with_location hydra_dsl_error_core_empty_union_type_error hydra_dsl_error_core_empty_union_type_error_location hydra_dsl_error_core_empty_union_type_error_with_location hydra_dsl_error_core_invalid_forall_parameter_name_error hydra_dsl_error_core_invalid_forall_parameter_name_error_location hydra_dsl_error_core_invalid_forall_parameter_name_error_name hydra_dsl_error_core_invalid_forall_parameter_name_error_with_location hydra_dsl_error_core_invalid_forall_parameter_name_error_with_name hydra_dsl_error_core_invalid_lambda_parameter_name_error hydra_dsl_error_core_invalid_lambda_parameter_name_error_location hydra_dsl_error_core_invalid_lambda_parameter_name_error_name hydra_dsl_error_core_invalid_lambda_parameter_name_error_with_location hydra_dsl_error_core_invalid_lambda_parameter_name_error_with_name hydra_dsl_error_core_invalid_let_binding_name_error hydra_dsl_error_core_invalid_let_binding_name_error_location hydra_dsl_error_core_invalid_let_binding_name_error_name hydra_dsl_error_core_invalid_let_binding_name_error_with_location hydra_dsl_error_core_invalid_let_binding_name_error_with_name hydra_dsl_error_core_invalid_term_error_constant_condition hydra_dsl_error_core_invalid_term_error_duplicate_binding hydra_dsl_error_core_invalid_term_error_duplicate_field hydra_dsl_error_core_invalid_term_error_empty_case_statement hydra_dsl_error_core_invalid_term_error_empty_let_bindings hydra_dsl_error_core_invalid_term_error_empty_term_annotation hydra_dsl_error_core_invalid_term_error_empty_type_name_in_term hydra_dsl_error_core_invalid_term_error_invalid_lambda_parameter_name hydra_dsl_error_core_invalid_term_error_invalid_let_binding_name hydra_dsl_error_core_invalid_term_error_invalid_type_lambda_parameter_name hydra_dsl_error_core_invalid_term_error_nested_term_annotation hydra_dsl_error_core_invalid_term_error_redundant_wrap_unwrap hydra_dsl_error_core_invalid_term_error_self_application hydra_dsl_error_core_invalid_term_error_term_variable_shadowing hydra_dsl_error_core_invalid_term_error_type_variable_shadowing_in_type_lambda hydra_dsl_error_core_invalid_term_error_undefined_term_variable hydra_dsl_error_core_invalid_term_error_undefined_type_variable_in_binding_type hydra_dsl_error_core_invalid_term_error_undefined_type_variable_in_lambda_domain hydra_dsl_error_core_invalid_term_error_undefined_type_variable_in_type_application hydra_dsl_error_core_invalid_term_error_unknown_primitive_name hydra_dsl_error_core_invalid_term_error_unnecessary_identity_application hydra_dsl_error_core_invalid_term_error_untyped_term_variable hydra_dsl_error_core_invalid_type_error_duplicate_record_type_field_names hydra_dsl_error_core_invalid_type_error_duplicate_union_type_field_names hydra_dsl_error_core_invalid_type_error_empty_record_type hydra_dsl_error_core_invalid_type_error_empty_type_annotation hydra_dsl_error_core_invalid_type_error_empty_union_type hydra_dsl_error_core_invalid_type_error_invalid_forall_parameter_name hydra_dsl_error_core_invalid_type_error_invalid_type_scheme_variable_name hydra_dsl_error_core_invalid_type_error_nested_type_annotation hydra_dsl_error_core_invalid_type_error_non_comparable_map_key_type hydra_dsl_error_core_invalid_type_error_non_comparable_set_element_type hydra_dsl_error_core_invalid_type_error_single_variant_union hydra_dsl_error_core_invalid_type_error_type_variable_shadowing_in_forall hydra_dsl_error_core_invalid_type_error_undefined_type_variable hydra_dsl_error_core_invalid_type_error_void_in_non_bottom_position hydra_dsl_error_core_invalid_type_lambda_parameter_name_error hydra_dsl_error_core_invalid_type_lambda_parameter_name_error_location hydra_dsl_error_core_invalid_type_lambda_parameter_name_error_name hydra_dsl_error_core_invalid_type_lambda_parameter_name_error_with_location hydra_dsl_error_core_invalid_type_lambda_parameter_name_error_with_name hydra_dsl_error_core_invalid_type_scheme_variable_name_error hydra_dsl_error_core_invalid_type_scheme_variable_name_error_location hydra_dsl_error_core_invalid_type_scheme_variable_name_error_name hydra_dsl_error_core_invalid_type_scheme_variable_name_error_with_location hydra_dsl_error_core_invalid_type_scheme_variable_name_error_with_name hydra_dsl_error_core_nested_term_annotation_error hydra_dsl_error_core_nested_term_annotation_error_location hydra_dsl_error_core_nested_term_annotation_error_with_location hydra_dsl_error_core_nested_type_annotation_error hydra_dsl_error_core_nested_type_annotation_error_location hydra_dsl_error_core_nested_type_annotation_error_with_location hydra_dsl_error_core_non_comparable_map_key_type_error hydra_dsl_error_core_non_comparable_map_key_type_error_key_type hydra_dsl_error_core_non_comparable_map_key_type_error_location hydra_dsl_error_core_non_comparable_map_key_type_error_with_key_type hydra_dsl_error_core_non_comparable_map_key_type_error_with_location hydra_dsl_error_core_non_comparable_set_element_type_error hydra_dsl_error_core_non_comparable_set_element_type_error_element_type hydra_dsl_error_core_non_comparable_set_element_type_error_location hydra_dsl_error_core_non_comparable_set_element_type_error_with_element_type hydra_dsl_error_core_non_comparable_set_element_type_error_with_location hydra_dsl_error_core_redundant_wrap_unwrap_error hydra_dsl_error_core_redundant_wrap_unwrap_error_location hydra_dsl_error_core_redundant_wrap_unwrap_error_type_name hydra_dsl_error_core_redundant_wrap_unwrap_error_with_location hydra_dsl_error_core_redundant_wrap_unwrap_error_with_type_name hydra_dsl_error_core_self_application_error hydra_dsl_error_core_self_application_error_location hydra_dsl_error_core_self_application_error_name hydra_dsl_error_core_self_application_error_with_location hydra_dsl_error_core_self_application_error_with_name hydra_dsl_error_core_single_variant_union_error hydra_dsl_error_core_single_variant_union_error_field_name hydra_dsl_error_core_single_variant_union_error_location hydra_dsl_error_core_single_variant_union_error_with_field_name hydra_dsl_error_core_single_variant_union_error_with_location hydra_dsl_error_core_term_variable_shadowing_error hydra_dsl_error_core_term_variable_shadowing_error_location hydra_dsl_error_core_term_variable_shadowing_error_name hydra_dsl_error_core_term_variable_shadowing_error_with_location hydra_dsl_error_core_term_variable_shadowing_error_with_name hydra_dsl_error_core_type_variable_shadowing_in_forall_error hydra_dsl_error_core_type_variable_shadowing_in_forall_error_location hydra_dsl_error_core_type_variable_shadowing_in_forall_error_name hydra_dsl_error_core_type_variable_shadowing_in_forall_error_with_location hydra_dsl_error_core_type_variable_shadowing_in_forall_error_with_name hydra_dsl_error_core_type_variable_shadowing_in_type_lambda_error hydra_dsl_error_core_type_variable_shadowing_in_type_lambda_error_location hydra_dsl_error_core_type_variable_shadowing_in_type_lambda_error_name hydra_dsl_error_core_type_variable_shadowing_in_type_lambda_error_with_location hydra_dsl_error_core_type_variable_shadowing_in_type_lambda_error_with_name hydra_dsl_error_core_undefined_field_error hydra_dsl_error_core_undefined_field_error_field_name hydra_dsl_error_core_undefined_field_error_type_name hydra_dsl_error_core_undefined_field_error_with_field_name hydra_dsl_error_core_undefined_field_error_with_type_name hydra_dsl_error_core_undefined_term_variable_error hydra_dsl_error_core_undefined_term_variable_error_location hydra_dsl_error_core_undefined_term_variable_error_name hydra_dsl_error_core_undefined_term_variable_error_with_location hydra_dsl_error_core_undefined_term_variable_error_with_name hydra_dsl_error_core_undefined_type_variable_error hydra_dsl_error_core_undefined_type_variable_error_location hydra_dsl_error_core_undefined_type_variable_error_name hydra_dsl_error_core_undefined_type_variable_error_with_location hydra_dsl_error_core_undefined_type_variable_error_with_name hydra_dsl_error_core_undefined_type_variable_in_binding_type_error hydra_dsl_error_core_undefined_type_variable_in_binding_type_error_location hydra_dsl_error_core_undefined_type_variable_in_binding_type_error_name hydra_dsl_error_core_undefined_type_variable_in_binding_type_error_with_location hydra_dsl_error_core_undefined_type_variable_in_binding_type_error_with_name hydra_dsl_error_core_undefined_type_variable_in_lambda_domain_error hydra_dsl_error_core_undefined_type_variable_in_lambda_domain_error_location hydra_dsl_error_core_undefined_type_variable_in_lambda_domain_error_name hydra_dsl_error_core_undefined_type_variable_in_lambda_domain_error_with_location hydra_dsl_error_core_undefined_type_variable_in_lambda_domain_error_with_name hydra_dsl_error_core_undefined_type_variable_in_type_application_error hydra_dsl_error_core_undefined_type_variable_in_type_application_error_location hydra_dsl_error_core_undefined_type_variable_in_type_application_error_name hydra_dsl_error_core_undefined_type_variable_in_type_application_error_with_location hydra_dsl_error_core_undefined_type_variable_in_type_application_error_with_name hydra_dsl_error_core_unexpected_term_variant_error hydra_dsl_error_core_unexpected_term_variant_error_actual_term hydra_dsl_error_core_unexpected_term_variant_error_expected_variant hydra_dsl_error_core_unexpected_term_variant_error_with_actual_term hydra_dsl_error_core_unexpected_term_variant_error_with_expected_variant hydra_dsl_error_core_unexpected_type_variant_error hydra_dsl_error_core_unexpected_type_variant_error_actual_type hydra_dsl_error_core_unexpected_type_variant_error_expected_variant hydra_dsl_error_core_unexpected_type_variant_error_with_actual_type hydra_dsl_error_core_unexpected_type_variant_error_with_expected_variant hydra_dsl_error_core_unknown_primitive_name_error hydra_dsl_error_core_unknown_primitive_name_error_location hydra_dsl_error_core_unknown_primitive_name_error_name hydra_dsl_error_core_unknown_primitive_name_error_with_location hydra_dsl_error_core_unknown_primitive_name_error_with_name hydra_dsl_error_core_unnecessary_identity_application_error hydra_dsl_error_core_unnecessary_identity_application_error_location hydra_dsl_error_core_unnecessary_identity_application_error_with_location hydra_dsl_error_core_untyped_term_variable_error hydra_dsl_error_core_untyped_term_variable_error_location hydra_dsl_error_core_untyped_term_variable_error_name hydra_dsl_error_core_untyped_term_variable_error_with_location hydra_dsl_error_core_untyped_term_variable_error_with_name hydra_dsl_error_core_void_in_non_bottom_position_error hydra_dsl_error_core_void_in_non_bottom_position_error_location hydra_dsl_error_core_void_in_non_bottom_position_error_with_location)

(def hydra_dsl_error_core_constant_condition_error (fn [location] (fn [value] (list :record (->hydra_core_record "hydra.error.core.ConstantConditionError" (list (->hydra_core_field "location" ((fn [v] v) location)) (->hydra_core_field "value" ((fn [v] v) value))))))))

(def hydra_dsl_error_core_constant_condition_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.ConstantConditionError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_constant_condition_error_value (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.ConstantConditionError" "value")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_constant_condition_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.ConstantConditionError" (list (->hydra_core_field "location" ((fn [v] v) new_val)) (->hydra_core_field "value" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.ConstantConditionError" "value")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_constant_condition_error_with_value (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.ConstantConditionError" (list (->hydra_core_field "location" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.ConstantConditionError" "location")) ((fn [v] v) original)))) (->hydra_core_field "value" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_duplicate_binding_error (fn [location] (fn [name] (list :record (->hydra_core_record "hydra.error.core.DuplicateBindingError" (list (->hydra_core_field "location" ((fn [v] v) location)) (->hydra_core_field "name" ((fn [v] v) name))))))))

(def hydra_dsl_error_core_duplicate_binding_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.DuplicateBindingError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_duplicate_binding_error_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.DuplicateBindingError" "name")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_duplicate_binding_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.DuplicateBindingError" (list (->hydra_core_field "location" ((fn [v] v) new_val)) (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.DuplicateBindingError" "name")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_duplicate_binding_error_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.DuplicateBindingError" (list (->hydra_core_field "location" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.DuplicateBindingError" "location")) ((fn [v] v) original)))) (->hydra_core_field "name" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_duplicate_field_error (fn [location] (fn [name] (list :record (->hydra_core_record "hydra.error.core.DuplicateFieldError" (list (->hydra_core_field "location" ((fn [v] v) location)) (->hydra_core_field "name" ((fn [v] v) name))))))))

(def hydra_dsl_error_core_duplicate_field_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.DuplicateFieldError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_duplicate_field_error_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.DuplicateFieldError" "name")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_duplicate_field_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.DuplicateFieldError" (list (->hydra_core_field "location" ((fn [v] v) new_val)) (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.DuplicateFieldError" "name")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_duplicate_field_error_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.DuplicateFieldError" (list (->hydra_core_field "location" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.DuplicateFieldError" "location")) ((fn [v] v) original)))) (->hydra_core_field "name" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_duplicate_record_type_field_names_error (fn [location] (fn [name] (list :record (->hydra_core_record "hydra.error.core.DuplicateRecordTypeFieldNamesError" (list (->hydra_core_field "location" ((fn [v] v) location)) (->hydra_core_field "name" ((fn [v] v) name))))))))

(def hydra_dsl_error_core_duplicate_record_type_field_names_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.DuplicateRecordTypeFieldNamesError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_duplicate_record_type_field_names_error_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.DuplicateRecordTypeFieldNamesError" "name")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_duplicate_record_type_field_names_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.DuplicateRecordTypeFieldNamesError" (list (->hydra_core_field "location" ((fn [v] v) new_val)) (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.DuplicateRecordTypeFieldNamesError" "name")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_duplicate_record_type_field_names_error_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.DuplicateRecordTypeFieldNamesError" (list (->hydra_core_field "location" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.DuplicateRecordTypeFieldNamesError" "location")) ((fn [v] v) original)))) (->hydra_core_field "name" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_duplicate_union_type_field_names_error (fn [location] (fn [name] (list :record (->hydra_core_record "hydra.error.core.DuplicateUnionTypeFieldNamesError" (list (->hydra_core_field "location" ((fn [v] v) location)) (->hydra_core_field "name" ((fn [v] v) name))))))))

(def hydra_dsl_error_core_duplicate_union_type_field_names_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.DuplicateUnionTypeFieldNamesError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_duplicate_union_type_field_names_error_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.DuplicateUnionTypeFieldNamesError" "name")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_duplicate_union_type_field_names_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.DuplicateUnionTypeFieldNamesError" (list (->hydra_core_field "location" ((fn [v] v) new_val)) (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.DuplicateUnionTypeFieldNamesError" "name")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_duplicate_union_type_field_names_error_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.DuplicateUnionTypeFieldNamesError" (list (->hydra_core_field "location" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.DuplicateUnionTypeFieldNamesError" "location")) ((fn [v] v) original)))) (->hydra_core_field "name" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_empty_case_statement_error (fn [location] (fn [type_name] (list :record (->hydra_core_record "hydra.error.core.EmptyCaseStatementError" (list (->hydra_core_field "location" ((fn [v] v) location)) (->hydra_core_field "typeName" ((fn [v] v) type_name))))))))

(def hydra_dsl_error_core_empty_case_statement_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.EmptyCaseStatementError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_empty_case_statement_error_type_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.EmptyCaseStatementError" "typeName")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_empty_case_statement_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.EmptyCaseStatementError" (list (->hydra_core_field "location" ((fn [v] v) new_val)) (->hydra_core_field "typeName" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.EmptyCaseStatementError" "typeName")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_empty_case_statement_error_with_type_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.EmptyCaseStatementError" (list (->hydra_core_field "location" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.EmptyCaseStatementError" "location")) ((fn [v] v) original)))) (->hydra_core_field "typeName" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_empty_let_bindings_error (fn [location] (list :record (->hydra_core_record "hydra.error.core.EmptyLetBindingsError" (list (->hydra_core_field "location" ((fn [v] v) location)))))))

(def hydra_dsl_error_core_empty_let_bindings_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.EmptyLetBindingsError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_empty_let_bindings_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.EmptyLetBindingsError" (list (->hydra_core_field "location" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_empty_record_type_error (fn [location] (list :record (->hydra_core_record "hydra.error.core.EmptyRecordTypeError" (list (->hydra_core_field "location" ((fn [v] v) location)))))))

(def hydra_dsl_error_core_empty_record_type_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.EmptyRecordTypeError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_empty_record_type_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.EmptyRecordTypeError" (list (->hydra_core_field "location" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_empty_term_annotation_error (fn [location] (list :record (->hydra_core_record "hydra.error.core.EmptyTermAnnotationError" (list (->hydra_core_field "location" ((fn [v] v) location)))))))

(def hydra_dsl_error_core_empty_term_annotation_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.EmptyTermAnnotationError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_empty_term_annotation_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.EmptyTermAnnotationError" (list (->hydra_core_field "location" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_empty_type_annotation_error (fn [location] (list :record (->hydra_core_record "hydra.error.core.EmptyTypeAnnotationError" (list (->hydra_core_field "location" ((fn [v] v) location)))))))

(def hydra_dsl_error_core_empty_type_annotation_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.EmptyTypeAnnotationError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_empty_type_annotation_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.EmptyTypeAnnotationError" (list (->hydra_core_field "location" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_empty_type_name_in_term_error (fn [location] (list :record (->hydra_core_record "hydra.error.core.EmptyTypeNameInTermError" (list (->hydra_core_field "location" ((fn [v] v) location)))))))

(def hydra_dsl_error_core_empty_type_name_in_term_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.EmptyTypeNameInTermError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_empty_type_name_in_term_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.EmptyTypeNameInTermError" (list (->hydra_core_field "location" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_empty_union_type_error (fn [location] (list :record (->hydra_core_record "hydra.error.core.EmptyUnionTypeError" (list (->hydra_core_field "location" ((fn [v] v) location)))))))

(def hydra_dsl_error_core_empty_union_type_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.EmptyUnionTypeError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_empty_union_type_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.EmptyUnionTypeError" (list (->hydra_core_field "location" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_invalid_forall_parameter_name_error (fn [location] (fn [name] (list :record (->hydra_core_record "hydra.error.core.InvalidForallParameterNameError" (list (->hydra_core_field "location" ((fn [v] v) location)) (->hydra_core_field "name" ((fn [v] v) name))))))))

(def hydra_dsl_error_core_invalid_forall_parameter_name_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.InvalidForallParameterNameError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_invalid_forall_parameter_name_error_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.InvalidForallParameterNameError" "name")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_invalid_forall_parameter_name_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.InvalidForallParameterNameError" (list (->hydra_core_field "location" ((fn [v] v) new_val)) (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.InvalidForallParameterNameError" "name")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_invalid_forall_parameter_name_error_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.InvalidForallParameterNameError" (list (->hydra_core_field "location" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.InvalidForallParameterNameError" "location")) ((fn [v] v) original)))) (->hydra_core_field "name" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_invalid_lambda_parameter_name_error (fn [location] (fn [name] (list :record (->hydra_core_record "hydra.error.core.InvalidLambdaParameterNameError" (list (->hydra_core_field "location" ((fn [v] v) location)) (->hydra_core_field "name" ((fn [v] v) name))))))))

(def hydra_dsl_error_core_invalid_lambda_parameter_name_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.InvalidLambdaParameterNameError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_invalid_lambda_parameter_name_error_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.InvalidLambdaParameterNameError" "name")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_invalid_lambda_parameter_name_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.InvalidLambdaParameterNameError" (list (->hydra_core_field "location" ((fn [v] v) new_val)) (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.InvalidLambdaParameterNameError" "name")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_invalid_lambda_parameter_name_error_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.InvalidLambdaParameterNameError" (list (->hydra_core_field "location" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.InvalidLambdaParameterNameError" "location")) ((fn [v] v) original)))) (->hydra_core_field "name" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_invalid_let_binding_name_error (fn [location] (fn [name] (list :record (->hydra_core_record "hydra.error.core.InvalidLetBindingNameError" (list (->hydra_core_field "location" ((fn [v] v) location)) (->hydra_core_field "name" ((fn [v] v) name))))))))

(def hydra_dsl_error_core_invalid_let_binding_name_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.InvalidLetBindingNameError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_invalid_let_binding_name_error_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.InvalidLetBindingNameError" "name")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_invalid_let_binding_name_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.InvalidLetBindingNameError" (list (->hydra_core_field "location" ((fn [v] v) new_val)) (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.InvalidLetBindingNameError" "name")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_invalid_let_binding_name_error_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.InvalidLetBindingNameError" (list (->hydra_core_field "location" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.InvalidLetBindingNameError" "location")) ((fn [v] v) original)))) (->hydra_core_field "name" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_invalid_term_error_constant_condition (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTermError" (->hydra_core_field "constantCondition" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_term_error_duplicate_binding (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTermError" (->hydra_core_field "duplicateBinding" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_term_error_duplicate_field (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTermError" (->hydra_core_field "duplicateField" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_term_error_empty_case_statement (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTermError" (->hydra_core_field "emptyCaseStatement" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_term_error_empty_let_bindings (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTermError" (->hydra_core_field "emptyLetBindings" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_term_error_empty_term_annotation (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTermError" (->hydra_core_field "emptyTermAnnotation" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_term_error_empty_type_name_in_term (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTermError" (->hydra_core_field "emptyTypeNameInTerm" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_term_error_invalid_lambda_parameter_name (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTermError" (->hydra_core_field "invalidLambdaParameterName" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_term_error_invalid_let_binding_name (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTermError" (->hydra_core_field "invalidLetBindingName" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_term_error_invalid_type_lambda_parameter_name (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTermError" (->hydra_core_field "invalidTypeLambdaParameterName" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_term_error_nested_term_annotation (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTermError" (->hydra_core_field "nestedTermAnnotation" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_term_error_redundant_wrap_unwrap (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTermError" (->hydra_core_field "redundantWrapUnwrap" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_term_error_self_application (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTermError" (->hydra_core_field "selfApplication" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_term_error_term_variable_shadowing (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTermError" (->hydra_core_field "termVariableShadowing" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_term_error_type_variable_shadowing_in_type_lambda (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTermError" (->hydra_core_field "typeVariableShadowingInTypeLambda" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_term_error_undefined_term_variable (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTermError" (->hydra_core_field "undefinedTermVariable" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_term_error_undefined_type_variable_in_binding_type (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTermError" (->hydra_core_field "undefinedTypeVariableInBindingType" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_term_error_undefined_type_variable_in_lambda_domain (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTermError" (->hydra_core_field "undefinedTypeVariableInLambdaDomain" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_term_error_undefined_type_variable_in_type_application (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTermError" (->hydra_core_field "undefinedTypeVariableInTypeApplication" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_term_error_unknown_primitive_name (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTermError" (->hydra_core_field "unknownPrimitiveName" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_term_error_unnecessary_identity_application (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTermError" (->hydra_core_field "unnecessaryIdentityApplication" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_term_error_untyped_term_variable (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTermError" (->hydra_core_field "untypedTermVariable" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_type_error_duplicate_record_type_field_names (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTypeError" (->hydra_core_field "duplicateRecordTypeFieldNames" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_type_error_duplicate_union_type_field_names (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTypeError" (->hydra_core_field "duplicateUnionTypeFieldNames" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_type_error_empty_record_type (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTypeError" (->hydra_core_field "emptyRecordType" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_type_error_empty_type_annotation (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTypeError" (->hydra_core_field "emptyTypeAnnotation" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_type_error_empty_union_type (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTypeError" (->hydra_core_field "emptyUnionType" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_type_error_invalid_forall_parameter_name (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTypeError" (->hydra_core_field "invalidForallParameterName" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_type_error_invalid_type_scheme_variable_name (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTypeError" (->hydra_core_field "invalidTypeSchemeVariableName" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_type_error_nested_type_annotation (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTypeError" (->hydra_core_field "nestedTypeAnnotation" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_type_error_non_comparable_map_key_type (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTypeError" (->hydra_core_field "nonComparableMapKeyType" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_type_error_non_comparable_set_element_type (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTypeError" (->hydra_core_field "nonComparableSetElementType" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_type_error_single_variant_union (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTypeError" (->hydra_core_field "singleVariantUnion" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_type_error_type_variable_shadowing_in_forall (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTypeError" (->hydra_core_field "typeVariableShadowingInForall" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_type_error_undefined_type_variable (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTypeError" (->hydra_core_field "undefinedTypeVariable" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_type_error_void_in_non_bottom_position (fn [x] (list :inject (->hydra_core_injection "hydra.error.core.InvalidTypeError" (->hydra_core_field "voidInNonBottomPosition" ((fn [v] v) x))))))

(def hydra_dsl_error_core_invalid_type_lambda_parameter_name_error (fn [location] (fn [name] (list :record (->hydra_core_record "hydra.error.core.InvalidTypeLambdaParameterNameError" (list (->hydra_core_field "location" ((fn [v] v) location)) (->hydra_core_field "name" ((fn [v] v) name))))))))

(def hydra_dsl_error_core_invalid_type_lambda_parameter_name_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.InvalidTypeLambdaParameterNameError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_invalid_type_lambda_parameter_name_error_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.InvalidTypeLambdaParameterNameError" "name")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_invalid_type_lambda_parameter_name_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.InvalidTypeLambdaParameterNameError" (list (->hydra_core_field "location" ((fn [v] v) new_val)) (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.InvalidTypeLambdaParameterNameError" "name")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_invalid_type_lambda_parameter_name_error_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.InvalidTypeLambdaParameterNameError" (list (->hydra_core_field "location" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.InvalidTypeLambdaParameterNameError" "location")) ((fn [v] v) original)))) (->hydra_core_field "name" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_invalid_type_scheme_variable_name_error (fn [location] (fn [name] (list :record (->hydra_core_record "hydra.error.core.InvalidTypeSchemeVariableNameError" (list (->hydra_core_field "location" ((fn [v] v) location)) (->hydra_core_field "name" ((fn [v] v) name))))))))

(def hydra_dsl_error_core_invalid_type_scheme_variable_name_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.InvalidTypeSchemeVariableNameError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_invalid_type_scheme_variable_name_error_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.InvalidTypeSchemeVariableNameError" "name")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_invalid_type_scheme_variable_name_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.InvalidTypeSchemeVariableNameError" (list (->hydra_core_field "location" ((fn [v] v) new_val)) (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.InvalidTypeSchemeVariableNameError" "name")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_invalid_type_scheme_variable_name_error_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.InvalidTypeSchemeVariableNameError" (list (->hydra_core_field "location" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.InvalidTypeSchemeVariableNameError" "location")) ((fn [v] v) original)))) (->hydra_core_field "name" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_nested_term_annotation_error (fn [location] (list :record (->hydra_core_record "hydra.error.core.NestedTermAnnotationError" (list (->hydra_core_field "location" ((fn [v] v) location)))))))

(def hydra_dsl_error_core_nested_term_annotation_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.NestedTermAnnotationError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_nested_term_annotation_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.NestedTermAnnotationError" (list (->hydra_core_field "location" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_nested_type_annotation_error (fn [location] (list :record (->hydra_core_record "hydra.error.core.NestedTypeAnnotationError" (list (->hydra_core_field "location" ((fn [v] v) location)))))))

(def hydra_dsl_error_core_nested_type_annotation_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.NestedTypeAnnotationError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_nested_type_annotation_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.NestedTypeAnnotationError" (list (->hydra_core_field "location" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_non_comparable_map_key_type_error (fn [location] (fn [key_type] (list :record (->hydra_core_record "hydra.error.core.NonComparableMapKeyTypeError" (list (->hydra_core_field "location" ((fn [v] v) location)) (->hydra_core_field "keyType" ((fn [v] v) key_type))))))))

(def hydra_dsl_error_core_non_comparable_map_key_type_error_key_type (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.NonComparableMapKeyTypeError" "keyType")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_non_comparable_map_key_type_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.NonComparableMapKeyTypeError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_non_comparable_map_key_type_error_with_key_type (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.NonComparableMapKeyTypeError" (list (->hydra_core_field "location" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.NonComparableMapKeyTypeError" "location")) ((fn [v] v) original)))) (->hydra_core_field "keyType" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_non_comparable_map_key_type_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.NonComparableMapKeyTypeError" (list (->hydra_core_field "location" ((fn [v] v) new_val)) (->hydra_core_field "keyType" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.NonComparableMapKeyTypeError" "keyType")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_non_comparable_set_element_type_error (fn [location] (fn [element_type] (list :record (->hydra_core_record "hydra.error.core.NonComparableSetElementTypeError" (list (->hydra_core_field "location" ((fn [v] v) location)) (->hydra_core_field "elementType" ((fn [v] v) element_type))))))))

(def hydra_dsl_error_core_non_comparable_set_element_type_error_element_type (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.NonComparableSetElementTypeError" "elementType")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_non_comparable_set_element_type_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.NonComparableSetElementTypeError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_non_comparable_set_element_type_error_with_element_type (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.NonComparableSetElementTypeError" (list (->hydra_core_field "location" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.NonComparableSetElementTypeError" "location")) ((fn [v] v) original)))) (->hydra_core_field "elementType" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_non_comparable_set_element_type_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.NonComparableSetElementTypeError" (list (->hydra_core_field "location" ((fn [v] v) new_val)) (->hydra_core_field "elementType" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.NonComparableSetElementTypeError" "elementType")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_redundant_wrap_unwrap_error (fn [location] (fn [type_name] (list :record (->hydra_core_record "hydra.error.core.RedundantWrapUnwrapError" (list (->hydra_core_field "location" ((fn [v] v) location)) (->hydra_core_field "typeName" ((fn [v] v) type_name))))))))

(def hydra_dsl_error_core_redundant_wrap_unwrap_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.RedundantWrapUnwrapError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_redundant_wrap_unwrap_error_type_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.RedundantWrapUnwrapError" "typeName")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_redundant_wrap_unwrap_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.RedundantWrapUnwrapError" (list (->hydra_core_field "location" ((fn [v] v) new_val)) (->hydra_core_field "typeName" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.RedundantWrapUnwrapError" "typeName")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_redundant_wrap_unwrap_error_with_type_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.RedundantWrapUnwrapError" (list (->hydra_core_field "location" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.RedundantWrapUnwrapError" "location")) ((fn [v] v) original)))) (->hydra_core_field "typeName" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_self_application_error (fn [location] (fn [name] (list :record (->hydra_core_record "hydra.error.core.SelfApplicationError" (list (->hydra_core_field "location" ((fn [v] v) location)) (->hydra_core_field "name" ((fn [v] v) name))))))))

(def hydra_dsl_error_core_self_application_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.SelfApplicationError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_self_application_error_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.SelfApplicationError" "name")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_self_application_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.SelfApplicationError" (list (->hydra_core_field "location" ((fn [v] v) new_val)) (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.SelfApplicationError" "name")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_self_application_error_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.SelfApplicationError" (list (->hydra_core_field "location" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.SelfApplicationError" "location")) ((fn [v] v) original)))) (->hydra_core_field "name" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_single_variant_union_error (fn [location] (fn [field_name] (list :record (->hydra_core_record "hydra.error.core.SingleVariantUnionError" (list (->hydra_core_field "location" ((fn [v] v) location)) (->hydra_core_field "fieldName" ((fn [v] v) field_name))))))))

(def hydra_dsl_error_core_single_variant_union_error_field_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.SingleVariantUnionError" "fieldName")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_single_variant_union_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.SingleVariantUnionError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_single_variant_union_error_with_field_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.SingleVariantUnionError" (list (->hydra_core_field "location" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.SingleVariantUnionError" "location")) ((fn [v] v) original)))) (->hydra_core_field "fieldName" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_single_variant_union_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.SingleVariantUnionError" (list (->hydra_core_field "location" ((fn [v] v) new_val)) (->hydra_core_field "fieldName" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.SingleVariantUnionError" "fieldName")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_term_variable_shadowing_error (fn [location] (fn [name] (list :record (->hydra_core_record "hydra.error.core.TermVariableShadowingError" (list (->hydra_core_field "location" ((fn [v] v) location)) (->hydra_core_field "name" ((fn [v] v) name))))))))

(def hydra_dsl_error_core_term_variable_shadowing_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.TermVariableShadowingError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_term_variable_shadowing_error_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.TermVariableShadowingError" "name")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_term_variable_shadowing_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.TermVariableShadowingError" (list (->hydra_core_field "location" ((fn [v] v) new_val)) (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.TermVariableShadowingError" "name")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_term_variable_shadowing_error_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.TermVariableShadowingError" (list (->hydra_core_field "location" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.TermVariableShadowingError" "location")) ((fn [v] v) original)))) (->hydra_core_field "name" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_type_variable_shadowing_in_forall_error (fn [location] (fn [name] (list :record (->hydra_core_record "hydra.error.core.TypeVariableShadowingInForallError" (list (->hydra_core_field "location" ((fn [v] v) location)) (->hydra_core_field "name" ((fn [v] v) name))))))))

(def hydra_dsl_error_core_type_variable_shadowing_in_forall_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.TypeVariableShadowingInForallError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_type_variable_shadowing_in_forall_error_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.TypeVariableShadowingInForallError" "name")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_type_variable_shadowing_in_forall_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.TypeVariableShadowingInForallError" (list (->hydra_core_field "location" ((fn [v] v) new_val)) (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.TypeVariableShadowingInForallError" "name")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_type_variable_shadowing_in_forall_error_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.TypeVariableShadowingInForallError" (list (->hydra_core_field "location" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.TypeVariableShadowingInForallError" "location")) ((fn [v] v) original)))) (->hydra_core_field "name" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_type_variable_shadowing_in_type_lambda_error (fn [location] (fn [name] (list :record (->hydra_core_record "hydra.error.core.TypeVariableShadowingInTypeLambdaError" (list (->hydra_core_field "location" ((fn [v] v) location)) (->hydra_core_field "name" ((fn [v] v) name))))))))

(def hydra_dsl_error_core_type_variable_shadowing_in_type_lambda_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.TypeVariableShadowingInTypeLambdaError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_type_variable_shadowing_in_type_lambda_error_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.TypeVariableShadowingInTypeLambdaError" "name")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_type_variable_shadowing_in_type_lambda_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.TypeVariableShadowingInTypeLambdaError" (list (->hydra_core_field "location" ((fn [v] v) new_val)) (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.TypeVariableShadowingInTypeLambdaError" "name")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_type_variable_shadowing_in_type_lambda_error_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.TypeVariableShadowingInTypeLambdaError" (list (->hydra_core_field "location" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.TypeVariableShadowingInTypeLambdaError" "location")) ((fn [v] v) original)))) (->hydra_core_field "name" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_undefined_field_error (fn [field_name] (fn [type_name] (list :record (->hydra_core_record "hydra.error.core.UndefinedFieldError" (list (->hydra_core_field "fieldName" ((fn [v] v) field_name)) (->hydra_core_field "typeName" ((fn [v] v) type_name))))))))

(def hydra_dsl_error_core_undefined_field_error_field_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UndefinedFieldError" "fieldName")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_undefined_field_error_type_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UndefinedFieldError" "typeName")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_undefined_field_error_with_field_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.UndefinedFieldError" (list (->hydra_core_field "fieldName" ((fn [v] v) new_val)) (->hydra_core_field "typeName" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UndefinedFieldError" "typeName")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_undefined_field_error_with_type_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.UndefinedFieldError" (list (->hydra_core_field "fieldName" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UndefinedFieldError" "fieldName")) ((fn [v] v) original)))) (->hydra_core_field "typeName" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_undefined_term_variable_error (fn [location] (fn [name] (list :record (->hydra_core_record "hydra.error.core.UndefinedTermVariableError" (list (->hydra_core_field "location" ((fn [v] v) location)) (->hydra_core_field "name" ((fn [v] v) name))))))))

(def hydra_dsl_error_core_undefined_term_variable_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UndefinedTermVariableError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_undefined_term_variable_error_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UndefinedTermVariableError" "name")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_undefined_term_variable_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.UndefinedTermVariableError" (list (->hydra_core_field "location" ((fn [v] v) new_val)) (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UndefinedTermVariableError" "name")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_undefined_term_variable_error_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.UndefinedTermVariableError" (list (->hydra_core_field "location" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UndefinedTermVariableError" "location")) ((fn [v] v) original)))) (->hydra_core_field "name" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_undefined_type_variable_error (fn [location] (fn [name] (list :record (->hydra_core_record "hydra.error.core.UndefinedTypeVariableError" (list (->hydra_core_field "location" ((fn [v] v) location)) (->hydra_core_field "name" ((fn [v] v) name))))))))

(def hydra_dsl_error_core_undefined_type_variable_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UndefinedTypeVariableError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_undefined_type_variable_error_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UndefinedTypeVariableError" "name")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_undefined_type_variable_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.UndefinedTypeVariableError" (list (->hydra_core_field "location" ((fn [v] v) new_val)) (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UndefinedTypeVariableError" "name")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_undefined_type_variable_error_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.UndefinedTypeVariableError" (list (->hydra_core_field "location" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UndefinedTypeVariableError" "location")) ((fn [v] v) original)))) (->hydra_core_field "name" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_undefined_type_variable_in_binding_type_error (fn [location] (fn [name] (list :record (->hydra_core_record "hydra.error.core.UndefinedTypeVariableInBindingTypeError" (list (->hydra_core_field "location" ((fn [v] v) location)) (->hydra_core_field "name" ((fn [v] v) name))))))))

(def hydra_dsl_error_core_undefined_type_variable_in_binding_type_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UndefinedTypeVariableInBindingTypeError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_undefined_type_variable_in_binding_type_error_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UndefinedTypeVariableInBindingTypeError" "name")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_undefined_type_variable_in_binding_type_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.UndefinedTypeVariableInBindingTypeError" (list (->hydra_core_field "location" ((fn [v] v) new_val)) (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UndefinedTypeVariableInBindingTypeError" "name")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_undefined_type_variable_in_binding_type_error_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.UndefinedTypeVariableInBindingTypeError" (list (->hydra_core_field "location" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UndefinedTypeVariableInBindingTypeError" "location")) ((fn [v] v) original)))) (->hydra_core_field "name" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_undefined_type_variable_in_lambda_domain_error (fn [location] (fn [name] (list :record (->hydra_core_record "hydra.error.core.UndefinedTypeVariableInLambdaDomainError" (list (->hydra_core_field "location" ((fn [v] v) location)) (->hydra_core_field "name" ((fn [v] v) name))))))))

(def hydra_dsl_error_core_undefined_type_variable_in_lambda_domain_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UndefinedTypeVariableInLambdaDomainError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_undefined_type_variable_in_lambda_domain_error_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UndefinedTypeVariableInLambdaDomainError" "name")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_undefined_type_variable_in_lambda_domain_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.UndefinedTypeVariableInLambdaDomainError" (list (->hydra_core_field "location" ((fn [v] v) new_val)) (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UndefinedTypeVariableInLambdaDomainError" "name")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_undefined_type_variable_in_lambda_domain_error_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.UndefinedTypeVariableInLambdaDomainError" (list (->hydra_core_field "location" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UndefinedTypeVariableInLambdaDomainError" "location")) ((fn [v] v) original)))) (->hydra_core_field "name" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_undefined_type_variable_in_type_application_error (fn [location] (fn [name] (list :record (->hydra_core_record "hydra.error.core.UndefinedTypeVariableInTypeApplicationError" (list (->hydra_core_field "location" ((fn [v] v) location)) (->hydra_core_field "name" ((fn [v] v) name))))))))

(def hydra_dsl_error_core_undefined_type_variable_in_type_application_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UndefinedTypeVariableInTypeApplicationError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_undefined_type_variable_in_type_application_error_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UndefinedTypeVariableInTypeApplicationError" "name")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_undefined_type_variable_in_type_application_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.UndefinedTypeVariableInTypeApplicationError" (list (->hydra_core_field "location" ((fn [v] v) new_val)) (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UndefinedTypeVariableInTypeApplicationError" "name")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_undefined_type_variable_in_type_application_error_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.UndefinedTypeVariableInTypeApplicationError" (list (->hydra_core_field "location" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UndefinedTypeVariableInTypeApplicationError" "location")) ((fn [v] v) original)))) (->hydra_core_field "name" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_unexpected_term_variant_error (fn [expected_variant] (fn [actual_term] (list :record (->hydra_core_record "hydra.error.core.UnexpectedTermVariantError" (list (->hydra_core_field "expectedVariant" ((fn [v] v) expected_variant)) (->hydra_core_field "actualTerm" ((fn [v] v) actual_term))))))))

(def hydra_dsl_error_core_unexpected_term_variant_error_actual_term (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UnexpectedTermVariantError" "actualTerm")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_unexpected_term_variant_error_expected_variant (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UnexpectedTermVariantError" "expectedVariant")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_unexpected_term_variant_error_with_actual_term (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.UnexpectedTermVariantError" (list (->hydra_core_field "expectedVariant" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UnexpectedTermVariantError" "expectedVariant")) ((fn [v] v) original)))) (->hydra_core_field "actualTerm" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_unexpected_term_variant_error_with_expected_variant (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.UnexpectedTermVariantError" (list (->hydra_core_field "expectedVariant" ((fn [v] v) new_val)) (->hydra_core_field "actualTerm" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UnexpectedTermVariantError" "actualTerm")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_unexpected_type_variant_error (fn [expected_variant] (fn [actual_type] (list :record (->hydra_core_record "hydra.error.core.UnexpectedTypeVariantError" (list (->hydra_core_field "expectedVariant" ((fn [v] v) expected_variant)) (->hydra_core_field "actualType" ((fn [v] v) actual_type))))))))

(def hydra_dsl_error_core_unexpected_type_variant_error_actual_type (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UnexpectedTypeVariantError" "actualType")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_unexpected_type_variant_error_expected_variant (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UnexpectedTypeVariantError" "expectedVariant")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_unexpected_type_variant_error_with_actual_type (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.UnexpectedTypeVariantError" (list (->hydra_core_field "expectedVariant" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UnexpectedTypeVariantError" "expectedVariant")) ((fn [v] v) original)))) (->hydra_core_field "actualType" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_unexpected_type_variant_error_with_expected_variant (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.UnexpectedTypeVariantError" (list (->hydra_core_field "expectedVariant" ((fn [v] v) new_val)) (->hydra_core_field "actualType" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UnexpectedTypeVariantError" "actualType")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_unknown_primitive_name_error (fn [location] (fn [name] (list :record (->hydra_core_record "hydra.error.core.UnknownPrimitiveNameError" (list (->hydra_core_field "location" ((fn [v] v) location)) (->hydra_core_field "name" ((fn [v] v) name))))))))

(def hydra_dsl_error_core_unknown_primitive_name_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UnknownPrimitiveNameError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_unknown_primitive_name_error_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UnknownPrimitiveNameError" "name")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_unknown_primitive_name_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.UnknownPrimitiveNameError" (list (->hydra_core_field "location" ((fn [v] v) new_val)) (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UnknownPrimitiveNameError" "name")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_unknown_primitive_name_error_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.UnknownPrimitiveNameError" (list (->hydra_core_field "location" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UnknownPrimitiveNameError" "location")) ((fn [v] v) original)))) (->hydra_core_field "name" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_unnecessary_identity_application_error (fn [location] (list :record (->hydra_core_record "hydra.error.core.UnnecessaryIdentityApplicationError" (list (->hydra_core_field "location" ((fn [v] v) location)))))))

(def hydra_dsl_error_core_unnecessary_identity_application_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UnnecessaryIdentityApplicationError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_unnecessary_identity_application_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.UnnecessaryIdentityApplicationError" (list (->hydra_core_field "location" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_untyped_term_variable_error (fn [location] (fn [name] (list :record (->hydra_core_record "hydra.error.core.UntypedTermVariableError" (list (->hydra_core_field "location" ((fn [v] v) location)) (->hydra_core_field "name" ((fn [v] v) name))))))))

(def hydra_dsl_error_core_untyped_term_variable_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UntypedTermVariableError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_untyped_term_variable_error_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UntypedTermVariableError" "name")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_untyped_term_variable_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.UntypedTermVariableError" (list (->hydra_core_field "location" ((fn [v] v) new_val)) (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UntypedTermVariableError" "name")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_core_untyped_term_variable_error_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.UntypedTermVariableError" (list (->hydra_core_field "location" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.UntypedTermVariableError" "location")) ((fn [v] v) original)))) (->hydra_core_field "name" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_core_void_in_non_bottom_position_error (fn [location] (list :record (->hydra_core_record "hydra.error.core.VoidInNonBottomPositionError" (list (->hydra_core_field "location" ((fn [v] v) location)))))))

(def hydra_dsl_error_core_void_in_non_bottom_position_error_location (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.core.VoidInNonBottomPositionError" "location")) ((fn [v] v) x)))))

(def hydra_dsl_error_core_void_in_non_bottom_position_error_with_location (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.core.VoidInNonBottomPositionError" (list (->hydra_core_field "location" ((fn [v] v) new_val))))))))
