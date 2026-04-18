(ns hydra.dsl.core
  (:require [hydra.core :refer :all] [hydra.phantoms :refer :all]
))

(declare hydra_dsl_core_annotated_term hydra_dsl_core_annotated_term_annotation hydra_dsl_core_annotated_term_body hydra_dsl_core_annotated_term_with_annotation hydra_dsl_core_annotated_term_with_body hydra_dsl_core_annotated_type hydra_dsl_core_annotated_type_annotation hydra_dsl_core_annotated_type_body hydra_dsl_core_annotated_type_with_annotation hydra_dsl_core_annotated_type_with_body hydra_dsl_core_application hydra_dsl_core_application_argument hydra_dsl_core_application_function hydra_dsl_core_application_type hydra_dsl_core_application_type_argument hydra_dsl_core_application_type_function hydra_dsl_core_application_type_with_argument hydra_dsl_core_application_type_with_function hydra_dsl_core_application_with_argument hydra_dsl_core_application_with_function hydra_dsl_core_binding hydra_dsl_core_binding_name hydra_dsl_core_binding_term hydra_dsl_core_binding_type hydra_dsl_core_binding_with_name hydra_dsl_core_binding_with_term hydra_dsl_core_binding_with_type hydra_dsl_core_case_statement hydra_dsl_core_case_statement_cases hydra_dsl_core_case_statement_default hydra_dsl_core_case_statement_type_name hydra_dsl_core_case_statement_with_cases hydra_dsl_core_case_statement_with_default hydra_dsl_core_case_statement_with_type_name hydra_dsl_core_either_type hydra_dsl_core_either_type_left hydra_dsl_core_either_type_right hydra_dsl_core_either_type_with_left hydra_dsl_core_either_type_with_right hydra_dsl_core_field hydra_dsl_core_field_name hydra_dsl_core_field_term hydra_dsl_core_field_type hydra_dsl_core_field_type_name hydra_dsl_core_field_type_type hydra_dsl_core_field_type_with_name hydra_dsl_core_field_type_with_type hydra_dsl_core_field_with_name hydra_dsl_core_field_with_term hydra_dsl_core_float_type_bigfloat hydra_dsl_core_float_type_float32 hydra_dsl_core_float_type_float64 hydra_dsl_core_float_value_bigfloat hydra_dsl_core_float_value_float32 hydra_dsl_core_float_value_float64 hydra_dsl_core_forall_type hydra_dsl_core_forall_type_body hydra_dsl_core_forall_type_parameter hydra_dsl_core_forall_type_with_body hydra_dsl_core_forall_type_with_parameter hydra_dsl_core_function_type hydra_dsl_core_function_type_codomain hydra_dsl_core_function_type_domain hydra_dsl_core_function_type_with_codomain hydra_dsl_core_function_type_with_domain hydra_dsl_core_injection hydra_dsl_core_injection_field hydra_dsl_core_injection_type_name hydra_dsl_core_injection_with_field hydra_dsl_core_injection_with_type_name hydra_dsl_core_integer_type_bigint hydra_dsl_core_integer_type_int16 hydra_dsl_core_integer_type_int32 hydra_dsl_core_integer_type_int64 hydra_dsl_core_integer_type_int8 hydra_dsl_core_integer_type_uint16 hydra_dsl_core_integer_type_uint32 hydra_dsl_core_integer_type_uint64 hydra_dsl_core_integer_type_uint8 hydra_dsl_core_integer_value_bigint hydra_dsl_core_integer_value_int16 hydra_dsl_core_integer_value_int32 hydra_dsl_core_integer_value_int64 hydra_dsl_core_integer_value_int8 hydra_dsl_core_integer_value_uint16 hydra_dsl_core_integer_value_uint32 hydra_dsl_core_integer_value_uint64 hydra_dsl_core_integer_value_uint8 hydra_dsl_core_lambda hydra_dsl_core_lambda_body hydra_dsl_core_lambda_domain hydra_dsl_core_lambda_parameter hydra_dsl_core_lambda_with_body hydra_dsl_core_lambda_with_domain hydra_dsl_core_lambda_with_parameter hydra_dsl_core_let hydra_dsl_core_let_bindings hydra_dsl_core_let_body hydra_dsl_core_let_with_bindings hydra_dsl_core_let_with_body hydra_dsl_core_literal_binary hydra_dsl_core_literal_boolean hydra_dsl_core_literal_decimal hydra_dsl_core_literal_float hydra_dsl_core_literal_integer hydra_dsl_core_literal_string hydra_dsl_core_literal_type_binary hydra_dsl_core_literal_type_boolean hydra_dsl_core_literal_type_decimal hydra_dsl_core_literal_type_float hydra_dsl_core_literal_type_integer hydra_dsl_core_literal_type_string hydra_dsl_core_map_type hydra_dsl_core_map_type_keys hydra_dsl_core_map_type_values hydra_dsl_core_map_type_with_keys hydra_dsl_core_map_type_with_values hydra_dsl_core_name hydra_dsl_core_pair_type hydra_dsl_core_pair_type_first hydra_dsl_core_pair_type_second hydra_dsl_core_pair_type_with_first hydra_dsl_core_pair_type_with_second hydra_dsl_core_projection hydra_dsl_core_projection_field hydra_dsl_core_projection_type_name hydra_dsl_core_projection_with_field hydra_dsl_core_projection_with_type_name hydra_dsl_core_record hydra_dsl_core_record_fields hydra_dsl_core_record_type_name hydra_dsl_core_record_with_fields hydra_dsl_core_record_with_type_name hydra_dsl_core_term_annotated hydra_dsl_core_term_application hydra_dsl_core_term_cases hydra_dsl_core_term_either hydra_dsl_core_term_inject hydra_dsl_core_term_lambda hydra_dsl_core_term_let hydra_dsl_core_term_list hydra_dsl_core_term_literal hydra_dsl_core_term_map hydra_dsl_core_term_maybe hydra_dsl_core_term_pair hydra_dsl_core_term_project hydra_dsl_core_term_record hydra_dsl_core_term_set hydra_dsl_core_term_type_application hydra_dsl_core_term_type_lambda hydra_dsl_core_term_unit hydra_dsl_core_term_unwrap hydra_dsl_core_term_variable hydra_dsl_core_term_wrap hydra_dsl_core_type_annotated hydra_dsl_core_type_application hydra_dsl_core_type_application_term hydra_dsl_core_type_application_term_body hydra_dsl_core_type_application_term_type hydra_dsl_core_type_application_term_with_body hydra_dsl_core_type_application_term_with_type hydra_dsl_core_type_either hydra_dsl_core_type_forall hydra_dsl_core_type_function hydra_dsl_core_type_lambda hydra_dsl_core_type_lambda_body hydra_dsl_core_type_lambda_parameter hydra_dsl_core_type_lambda_with_body hydra_dsl_core_type_lambda_with_parameter hydra_dsl_core_type_list hydra_dsl_core_type_literal hydra_dsl_core_type_map hydra_dsl_core_type_maybe hydra_dsl_core_type_pair hydra_dsl_core_type_record hydra_dsl_core_type_scheme hydra_dsl_core_type_scheme_constraints hydra_dsl_core_type_scheme_type hydra_dsl_core_type_scheme_variables hydra_dsl_core_type_scheme_with_constraints hydra_dsl_core_type_scheme_with_type hydra_dsl_core_type_scheme_with_variables hydra_dsl_core_type_set hydra_dsl_core_type_union hydra_dsl_core_type_unit hydra_dsl_core_type_variable hydra_dsl_core_type_variable_metadata hydra_dsl_core_type_variable_metadata_classes hydra_dsl_core_type_variable_metadata_with_classes hydra_dsl_core_type_void hydra_dsl_core_type_wrap hydra_dsl_core_un_name hydra_dsl_core_wrapped_term hydra_dsl_core_wrapped_term_body hydra_dsl_core_wrapped_term_type_name hydra_dsl_core_wrapped_term_with_body hydra_dsl_core_wrapped_term_with_type_name)

(def hydra_dsl_core_annotated_term (fn [body] (fn [annotation] (list :record (->hydra_core_record "hydra.core.AnnotatedTerm" (list (->hydra_core_field "body" ((fn [v] v) body)) (->hydra_core_field "annotation" ((fn [v] v) annotation))))))))

(def hydra_dsl_core_annotated_term_annotation (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.AnnotatedTerm" "annotation")) ((fn [v] v) x)))))

(def hydra_dsl_core_annotated_term_body (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.AnnotatedTerm" "body")) ((fn [v] v) x)))))

(def hydra_dsl_core_annotated_term_with_annotation (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.AnnotatedTerm" (list (->hydra_core_field "body" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.AnnotatedTerm" "body")) ((fn [v] v) original)))) (->hydra_core_field "annotation" ((fn [v] v) new_val))))))))

(def hydra_dsl_core_annotated_term_with_body (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.AnnotatedTerm" (list (->hydra_core_field "body" ((fn [v] v) new_val)) (->hydra_core_field "annotation" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.AnnotatedTerm" "annotation")) ((fn [v] v) original))))))))))

(def hydra_dsl_core_annotated_type (fn [body] (fn [annotation] (list :record (->hydra_core_record "hydra.core.AnnotatedType" (list (->hydra_core_field "body" ((fn [v] v) body)) (->hydra_core_field "annotation" ((fn [v] v) annotation))))))))

(def hydra_dsl_core_annotated_type_annotation (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.AnnotatedType" "annotation")) ((fn [v] v) x)))))

(def hydra_dsl_core_annotated_type_body (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.AnnotatedType" "body")) ((fn [v] v) x)))))

(def hydra_dsl_core_annotated_type_with_annotation (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.AnnotatedType" (list (->hydra_core_field "body" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.AnnotatedType" "body")) ((fn [v] v) original)))) (->hydra_core_field "annotation" ((fn [v] v) new_val))))))))

(def hydra_dsl_core_annotated_type_with_body (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.AnnotatedType" (list (->hydra_core_field "body" ((fn [v] v) new_val)) (->hydra_core_field "annotation" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.AnnotatedType" "annotation")) ((fn [v] v) original))))))))))

(def hydra_dsl_core_application (fn [function_] (fn [argument] (list :record (->hydra_core_record "hydra.core.Application" (list (->hydra_core_field "function" ((fn [v] v) function_)) (->hydra_core_field "argument" ((fn [v] v) argument))))))))

(def hydra_dsl_core_application_argument (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Application" "argument")) ((fn [v] v) x)))))

(def hydra_dsl_core_application_function (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Application" "function")) ((fn [v] v) x)))))

(def hydra_dsl_core_application_type (fn [function_] (fn [argument] (list :record (->hydra_core_record "hydra.core.ApplicationType" (list (->hydra_core_field "function" ((fn [v] v) function_)) (->hydra_core_field "argument" ((fn [v] v) argument))))))))

(def hydra_dsl_core_application_type_argument (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.ApplicationType" "argument")) ((fn [v] v) x)))))

(def hydra_dsl_core_application_type_function (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.ApplicationType" "function")) ((fn [v] v) x)))))

(def hydra_dsl_core_application_type_with_argument (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.ApplicationType" (list (->hydra_core_field "function" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.ApplicationType" "function")) ((fn [v] v) original)))) (->hydra_core_field "argument" ((fn [v] v) new_val))))))))

(def hydra_dsl_core_application_type_with_function (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.ApplicationType" (list (->hydra_core_field "function" ((fn [v] v) new_val)) (->hydra_core_field "argument" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.ApplicationType" "argument")) ((fn [v] v) original))))))))))

(def hydra_dsl_core_application_with_argument (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.Application" (list (->hydra_core_field "function" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Application" "function")) ((fn [v] v) original)))) (->hydra_core_field "argument" ((fn [v] v) new_val))))))))

(def hydra_dsl_core_application_with_function (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.Application" (list (->hydra_core_field "function" ((fn [v] v) new_val)) (->hydra_core_field "argument" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Application" "argument")) ((fn [v] v) original))))))))))

(def hydra_dsl_core_binding (fn [name] (fn [term] (fn [type] (list :record (->hydra_core_record "hydra.core.Binding" (list (->hydra_core_field "name" ((fn [v] v) name)) (->hydra_core_field "term" ((fn [v] v) term)) (->hydra_core_field "type" ((fn [v] v) type)))))))))

(def hydra_dsl_core_binding_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Binding" "name")) ((fn [v] v) x)))))

(def hydra_dsl_core_binding_term (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Binding" "term")) ((fn [v] v) x)))))

(def hydra_dsl_core_binding_type (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Binding" "type")) ((fn [v] v) x)))))

(def hydra_dsl_core_binding_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.Binding" (list (->hydra_core_field "name" ((fn [v] v) new_val)) (->hydra_core_field "term" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Binding" "term")) ((fn [v] v) original)))) (->hydra_core_field "type" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Binding" "type")) ((fn [v] v) original))))))))))

(def hydra_dsl_core_binding_with_term (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.Binding" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Binding" "name")) ((fn [v] v) original)))) (->hydra_core_field "term" ((fn [v] v) new_val)) (->hydra_core_field "type" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Binding" "type")) ((fn [v] v) original))))))))))

(def hydra_dsl_core_binding_with_type (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.Binding" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Binding" "name")) ((fn [v] v) original)))) (->hydra_core_field "term" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Binding" "term")) ((fn [v] v) original)))) (->hydra_core_field "type" ((fn [v] v) new_val))))))))

(def hydra_dsl_core_case_statement (fn [type_name] (fn [default] (fn [cases] (list :record (->hydra_core_record "hydra.core.CaseStatement" (list (->hydra_core_field "typeName" ((fn [v] v) type_name)) (->hydra_core_field "default" ((fn [v] v) default)) (->hydra_core_field "cases" ((fn [v] v) cases)))))))))

(def hydra_dsl_core_case_statement_cases (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.CaseStatement" "cases")) ((fn [v] v) x)))))

(def hydra_dsl_core_case_statement_default (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.CaseStatement" "default")) ((fn [v] v) x)))))

(def hydra_dsl_core_case_statement_type_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.CaseStatement" "typeName")) ((fn [v] v) x)))))

(def hydra_dsl_core_case_statement_with_cases (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.CaseStatement" (list (->hydra_core_field "typeName" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.CaseStatement" "typeName")) ((fn [v] v) original)))) (->hydra_core_field "default" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.CaseStatement" "default")) ((fn [v] v) original)))) (->hydra_core_field "cases" ((fn [v] v) new_val))))))))

(def hydra_dsl_core_case_statement_with_default (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.CaseStatement" (list (->hydra_core_field "typeName" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.CaseStatement" "typeName")) ((fn [v] v) original)))) (->hydra_core_field "default" ((fn [v] v) new_val)) (->hydra_core_field "cases" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.CaseStatement" "cases")) ((fn [v] v) original))))))))))

(def hydra_dsl_core_case_statement_with_type_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.CaseStatement" (list (->hydra_core_field "typeName" ((fn [v] v) new_val)) (->hydra_core_field "default" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.CaseStatement" "default")) ((fn [v] v) original)))) (->hydra_core_field "cases" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.CaseStatement" "cases")) ((fn [v] v) original))))))))))

(def hydra_dsl_core_either_type (fn [left] (fn [right] (list :record (->hydra_core_record "hydra.core.EitherType" (list (->hydra_core_field "left" ((fn [v] v) left)) (->hydra_core_field "right" ((fn [v] v) right))))))))

(def hydra_dsl_core_either_type_left (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.EitherType" "left")) ((fn [v] v) x)))))

(def hydra_dsl_core_either_type_right (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.EitherType" "right")) ((fn [v] v) x)))))

(def hydra_dsl_core_either_type_with_left (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.EitherType" (list (->hydra_core_field "left" ((fn [v] v) new_val)) (->hydra_core_field "right" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.EitherType" "right")) ((fn [v] v) original))))))))))

(def hydra_dsl_core_either_type_with_right (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.EitherType" (list (->hydra_core_field "left" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.EitherType" "left")) ((fn [v] v) original)))) (->hydra_core_field "right" ((fn [v] v) new_val))))))))

(def hydra_dsl_core_field (fn [name] (fn [term] (list :record (->hydra_core_record "hydra.core.Field" (list (->hydra_core_field "name" ((fn [v] v) name)) (->hydra_core_field "term" ((fn [v] v) term))))))))

(def hydra_dsl_core_field_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Field" "name")) ((fn [v] v) x)))))

(def hydra_dsl_core_field_term (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Field" "term")) ((fn [v] v) x)))))

(def hydra_dsl_core_field_type (fn [name] (fn [type] (list :record (->hydra_core_record "hydra.core.FieldType" (list (->hydra_core_field "name" ((fn [v] v) name)) (->hydra_core_field "type" ((fn [v] v) type))))))))

(def hydra_dsl_core_field_type_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.FieldType" "name")) ((fn [v] v) x)))))

(def hydra_dsl_core_field_type_type (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.FieldType" "type")) ((fn [v] v) x)))))

(def hydra_dsl_core_field_type_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.FieldType" (list (->hydra_core_field "name" ((fn [v] v) new_val)) (->hydra_core_field "type" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.FieldType" "type")) ((fn [v] v) original))))))))))

(def hydra_dsl_core_field_type_with_type (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.FieldType" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.FieldType" "name")) ((fn [v] v) original)))) (->hydra_core_field "type" ((fn [v] v) new_val))))))))

(def hydra_dsl_core_field_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.Field" (list (->hydra_core_field "name" ((fn [v] v) new_val)) (->hydra_core_field "term" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Field" "term")) ((fn [v] v) original))))))))))

(def hydra_dsl_core_field_with_term (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.Field" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Field" "name")) ((fn [v] v) original)))) (->hydra_core_field "term" ((fn [v] v) new_val))))))))

(def hydra_dsl_core_float_type_bigfloat (list :inject (->hydra_core_injection "hydra.core.FloatType" (->hydra_core_field "bigfloat" (list :unit nil)))))

(def hydra_dsl_core_float_type_float32 (list :inject (->hydra_core_injection "hydra.core.FloatType" (->hydra_core_field "float32" (list :unit nil)))))

(def hydra_dsl_core_float_type_float64 (list :inject (->hydra_core_injection "hydra.core.FloatType" (->hydra_core_field "float64" (list :unit nil)))))

(def hydra_dsl_core_float_value_bigfloat (fn [x] (list :inject (->hydra_core_injection "hydra.core.FloatValue" (->hydra_core_field "bigfloat" ((fn [v] v) x))))))

(def hydra_dsl_core_float_value_float32 (fn [x] (list :inject (->hydra_core_injection "hydra.core.FloatValue" (->hydra_core_field "float32" ((fn [v] v) x))))))

(def hydra_dsl_core_float_value_float64 (fn [x] (list :inject (->hydra_core_injection "hydra.core.FloatValue" (->hydra_core_field "float64" ((fn [v] v) x))))))

(def hydra_dsl_core_forall_type (fn [parameter] (fn [body] (list :record (->hydra_core_record "hydra.core.ForallType" (list (->hydra_core_field "parameter" ((fn [v] v) parameter)) (->hydra_core_field "body" ((fn [v] v) body))))))))

(def hydra_dsl_core_forall_type_body (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.ForallType" "body")) ((fn [v] v) x)))))

(def hydra_dsl_core_forall_type_parameter (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.ForallType" "parameter")) ((fn [v] v) x)))))

(def hydra_dsl_core_forall_type_with_body (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.ForallType" (list (->hydra_core_field "parameter" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.ForallType" "parameter")) ((fn [v] v) original)))) (->hydra_core_field "body" ((fn [v] v) new_val))))))))

(def hydra_dsl_core_forall_type_with_parameter (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.ForallType" (list (->hydra_core_field "parameter" ((fn [v] v) new_val)) (->hydra_core_field "body" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.ForallType" "body")) ((fn [v] v) original))))))))))

(def hydra_dsl_core_function_type (fn [domain] (fn [codomain] (list :record (->hydra_core_record "hydra.core.FunctionType" (list (->hydra_core_field "domain" ((fn [v] v) domain)) (->hydra_core_field "codomain" ((fn [v] v) codomain))))))))

(def hydra_dsl_core_function_type_codomain (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.FunctionType" "codomain")) ((fn [v] v) x)))))

(def hydra_dsl_core_function_type_domain (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.FunctionType" "domain")) ((fn [v] v) x)))))

(def hydra_dsl_core_function_type_with_codomain (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.FunctionType" (list (->hydra_core_field "domain" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.FunctionType" "domain")) ((fn [v] v) original)))) (->hydra_core_field "codomain" ((fn [v] v) new_val))))))))

(def hydra_dsl_core_function_type_with_domain (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.FunctionType" (list (->hydra_core_field "domain" ((fn [v] v) new_val)) (->hydra_core_field "codomain" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.FunctionType" "codomain")) ((fn [v] v) original))))))))))

(def hydra_dsl_core_injection (fn [type_name] (fn [field] (list :record (->hydra_core_record "hydra.core.Injection" (list (->hydra_core_field "typeName" ((fn [v] v) type_name)) (->hydra_core_field "field" ((fn [v] v) field))))))))

(def hydra_dsl_core_injection_field (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Injection" "field")) ((fn [v] v) x)))))

(def hydra_dsl_core_injection_type_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Injection" "typeName")) ((fn [v] v) x)))))

(def hydra_dsl_core_injection_with_field (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.Injection" (list (->hydra_core_field "typeName" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Injection" "typeName")) ((fn [v] v) original)))) (->hydra_core_field "field" ((fn [v] v) new_val))))))))

(def hydra_dsl_core_injection_with_type_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.Injection" (list (->hydra_core_field "typeName" ((fn [v] v) new_val)) (->hydra_core_field "field" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Injection" "field")) ((fn [v] v) original))))))))))

(def hydra_dsl_core_integer_type_bigint (list :inject (->hydra_core_injection "hydra.core.IntegerType" (->hydra_core_field "bigint" (list :unit nil)))))

(def hydra_dsl_core_integer_type_int16 (list :inject (->hydra_core_injection "hydra.core.IntegerType" (->hydra_core_field "int16" (list :unit nil)))))

(def hydra_dsl_core_integer_type_int32 (list :inject (->hydra_core_injection "hydra.core.IntegerType" (->hydra_core_field "int32" (list :unit nil)))))

(def hydra_dsl_core_integer_type_int64 (list :inject (->hydra_core_injection "hydra.core.IntegerType" (->hydra_core_field "int64" (list :unit nil)))))

(def hydra_dsl_core_integer_type_int8 (list :inject (->hydra_core_injection "hydra.core.IntegerType" (->hydra_core_field "int8" (list :unit nil)))))

(def hydra_dsl_core_integer_type_uint16 (list :inject (->hydra_core_injection "hydra.core.IntegerType" (->hydra_core_field "uint16" (list :unit nil)))))

(def hydra_dsl_core_integer_type_uint32 (list :inject (->hydra_core_injection "hydra.core.IntegerType" (->hydra_core_field "uint32" (list :unit nil)))))

(def hydra_dsl_core_integer_type_uint64 (list :inject (->hydra_core_injection "hydra.core.IntegerType" (->hydra_core_field "uint64" (list :unit nil)))))

(def hydra_dsl_core_integer_type_uint8 (list :inject (->hydra_core_injection "hydra.core.IntegerType" (->hydra_core_field "uint8" (list :unit nil)))))

(def hydra_dsl_core_integer_value_bigint (fn [x] (list :inject (->hydra_core_injection "hydra.core.IntegerValue" (->hydra_core_field "bigint" ((fn [v] v) x))))))

(def hydra_dsl_core_integer_value_int16 (fn [x] (list :inject (->hydra_core_injection "hydra.core.IntegerValue" (->hydra_core_field "int16" ((fn [v] v) x))))))

(def hydra_dsl_core_integer_value_int32 (fn [x] (list :inject (->hydra_core_injection "hydra.core.IntegerValue" (->hydra_core_field "int32" ((fn [v] v) x))))))

(def hydra_dsl_core_integer_value_int64 (fn [x] (list :inject (->hydra_core_injection "hydra.core.IntegerValue" (->hydra_core_field "int64" ((fn [v] v) x))))))

(def hydra_dsl_core_integer_value_int8 (fn [x] (list :inject (->hydra_core_injection "hydra.core.IntegerValue" (->hydra_core_field "int8" ((fn [v] v) x))))))

(def hydra_dsl_core_integer_value_uint16 (fn [x] (list :inject (->hydra_core_injection "hydra.core.IntegerValue" (->hydra_core_field "uint16" ((fn [v] v) x))))))

(def hydra_dsl_core_integer_value_uint32 (fn [x] (list :inject (->hydra_core_injection "hydra.core.IntegerValue" (->hydra_core_field "uint32" ((fn [v] v) x))))))

(def hydra_dsl_core_integer_value_uint64 (fn [x] (list :inject (->hydra_core_injection "hydra.core.IntegerValue" (->hydra_core_field "uint64" ((fn [v] v) x))))))

(def hydra_dsl_core_integer_value_uint8 (fn [x] (list :inject (->hydra_core_injection "hydra.core.IntegerValue" (->hydra_core_field "uint8" ((fn [v] v) x))))))

(def hydra_dsl_core_lambda (fn [parameter] (fn [domain] (fn [body] (list :record (->hydra_core_record "hydra.core.Lambda" (list (->hydra_core_field "parameter" ((fn [v] v) parameter)) (->hydra_core_field "domain" ((fn [v] v) domain)) (->hydra_core_field "body" ((fn [v] v) body)))))))))

(def hydra_dsl_core_lambda_body (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Lambda" "body")) ((fn [v] v) x)))))

(def hydra_dsl_core_lambda_domain (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Lambda" "domain")) ((fn [v] v) x)))))

(def hydra_dsl_core_lambda_parameter (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Lambda" "parameter")) ((fn [v] v) x)))))

(def hydra_dsl_core_lambda_with_body (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.Lambda" (list (->hydra_core_field "parameter" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Lambda" "parameter")) ((fn [v] v) original)))) (->hydra_core_field "domain" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Lambda" "domain")) ((fn [v] v) original)))) (->hydra_core_field "body" ((fn [v] v) new_val))))))))

(def hydra_dsl_core_lambda_with_domain (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.Lambda" (list (->hydra_core_field "parameter" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Lambda" "parameter")) ((fn [v] v) original)))) (->hydra_core_field "domain" ((fn [v] v) new_val)) (->hydra_core_field "body" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Lambda" "body")) ((fn [v] v) original))))))))))

(def hydra_dsl_core_lambda_with_parameter (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.Lambda" (list (->hydra_core_field "parameter" ((fn [v] v) new_val)) (->hydra_core_field "domain" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Lambda" "domain")) ((fn [v] v) original)))) (->hydra_core_field "body" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Lambda" "body")) ((fn [v] v) original))))))))))

(def hydra_dsl_core_let (fn [bindings] (fn [body] (list :record (->hydra_core_record "hydra.core.Let" (list (->hydra_core_field "bindings" ((fn [v] v) bindings)) (->hydra_core_field "body" ((fn [v] v) body))))))))

(def hydra_dsl_core_let_bindings (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Let" "bindings")) ((fn [v] v) x)))))

(def hydra_dsl_core_let_body (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Let" "body")) ((fn [v] v) x)))))

(def hydra_dsl_core_let_with_bindings (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.Let" (list (->hydra_core_field "bindings" ((fn [v] v) new_val)) (->hydra_core_field "body" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Let" "body")) ((fn [v] v) original))))))))))

(def hydra_dsl_core_let_with_body (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.Let" (list (->hydra_core_field "bindings" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Let" "bindings")) ((fn [v] v) original)))) (->hydra_core_field "body" ((fn [v] v) new_val))))))))

(def hydra_dsl_core_literal_binary (fn [x] (list :inject (->hydra_core_injection "hydra.core.Literal" (->hydra_core_field "binary" ((fn [v] v) x))))))

(def hydra_dsl_core_literal_boolean (fn [x] (list :inject (->hydra_core_injection "hydra.core.Literal" (->hydra_core_field "boolean" ((fn [v] v) x))))))

(def hydra_dsl_core_literal_decimal (fn [x] (list :inject (->hydra_core_injection "hydra.core.Literal" (->hydra_core_field "decimal" ((fn [v] v) x))))))

(def hydra_dsl_core_literal_float (fn [x] (list :inject (->hydra_core_injection "hydra.core.Literal" (->hydra_core_field "float" ((fn [v] v) x))))))

(def hydra_dsl_core_literal_integer (fn [x] (list :inject (->hydra_core_injection "hydra.core.Literal" (->hydra_core_field "integer" ((fn [v] v) x))))))

(def hydra_dsl_core_literal_string (fn [x] (list :inject (->hydra_core_injection "hydra.core.Literal" (->hydra_core_field "string" ((fn [v] v) x))))))

(def hydra_dsl_core_literal_type_binary (list :inject (->hydra_core_injection "hydra.core.LiteralType" (->hydra_core_field "binary" (list :unit nil)))))

(def hydra_dsl_core_literal_type_boolean (list :inject (->hydra_core_injection "hydra.core.LiteralType" (->hydra_core_field "boolean" (list :unit nil)))))

(def hydra_dsl_core_literal_type_decimal (list :inject (->hydra_core_injection "hydra.core.LiteralType" (->hydra_core_field "decimal" (list :unit nil)))))

(def hydra_dsl_core_literal_type_float (fn [x] (list :inject (->hydra_core_injection "hydra.core.LiteralType" (->hydra_core_field "float" ((fn [v] v) x))))))

(def hydra_dsl_core_literal_type_integer (fn [x] (list :inject (->hydra_core_injection "hydra.core.LiteralType" (->hydra_core_field "integer" ((fn [v] v) x))))))

(def hydra_dsl_core_literal_type_string (list :inject (->hydra_core_injection "hydra.core.LiteralType" (->hydra_core_field "string" (list :unit nil)))))

(def hydra_dsl_core_map_type (fn [keys] (fn [values_] (list :record (->hydra_core_record "hydra.core.MapType" (list (->hydra_core_field "keys" ((fn [v] v) keys)) (->hydra_core_field "values" ((fn [v] v) values_))))))))

(def hydra_dsl_core_map_type_keys (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.MapType" "keys")) ((fn [v] v) x)))))

(def hydra_dsl_core_map_type_values (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.MapType" "values")) ((fn [v] v) x)))))

(def hydra_dsl_core_map_type_with_keys (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.MapType" (list (->hydra_core_field "keys" ((fn [v] v) new_val)) (->hydra_core_field "values" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.MapType" "values")) ((fn [v] v) original))))))))))

(def hydra_dsl_core_map_type_with_values (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.MapType" (list (->hydra_core_field "keys" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.MapType" "keys")) ((fn [v] v) original)))) (->hydra_core_field "values" ((fn [v] v) new_val))))))))

(def hydra_dsl_core_name (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.core.Name" ((fn [v] v) x)))))

(def hydra_dsl_core_pair_type (fn [first] (fn [second] (list :record (->hydra_core_record "hydra.core.PairType" (list (->hydra_core_field "first" ((fn [v] v) first)) (->hydra_core_field "second" ((fn [v] v) second))))))))

(def hydra_dsl_core_pair_type_first (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.PairType" "first")) ((fn [v] v) x)))))

(def hydra_dsl_core_pair_type_second (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.PairType" "second")) ((fn [v] v) x)))))

(def hydra_dsl_core_pair_type_with_first (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.PairType" (list (->hydra_core_field "first" ((fn [v] v) new_val)) (->hydra_core_field "second" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.PairType" "second")) ((fn [v] v) original))))))))))

(def hydra_dsl_core_pair_type_with_second (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.PairType" (list (->hydra_core_field "first" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.PairType" "first")) ((fn [v] v) original)))) (->hydra_core_field "second" ((fn [v] v) new_val))))))))

(def hydra_dsl_core_projection (fn [type_name] (fn [field] (list :record (->hydra_core_record "hydra.core.Projection" (list (->hydra_core_field "typeName" ((fn [v] v) type_name)) (->hydra_core_field "field" ((fn [v] v) field))))))))

(def hydra_dsl_core_projection_field (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Projection" "field")) ((fn [v] v) x)))))

(def hydra_dsl_core_projection_type_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Projection" "typeName")) ((fn [v] v) x)))))

(def hydra_dsl_core_projection_with_field (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.Projection" (list (->hydra_core_field "typeName" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Projection" "typeName")) ((fn [v] v) original)))) (->hydra_core_field "field" ((fn [v] v) new_val))))))))

(def hydra_dsl_core_projection_with_type_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.Projection" (list (->hydra_core_field "typeName" ((fn [v] v) new_val)) (->hydra_core_field "field" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Projection" "field")) ((fn [v] v) original))))))))))

(def hydra_dsl_core_record (fn [type_name] (fn [fields] (list :record (->hydra_core_record "hydra.core.Record" (list (->hydra_core_field "typeName" ((fn [v] v) type_name)) (->hydra_core_field "fields" ((fn [v] v) fields))))))))

(def hydra_dsl_core_record_fields (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Record" "fields")) ((fn [v] v) x)))))

(def hydra_dsl_core_record_type_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Record" "typeName")) ((fn [v] v) x)))))

(def hydra_dsl_core_record_with_fields (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.Record" (list (->hydra_core_field "typeName" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Record" "typeName")) ((fn [v] v) original)))) (->hydra_core_field "fields" ((fn [v] v) new_val))))))))

(def hydra_dsl_core_record_with_type_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.Record" (list (->hydra_core_field "typeName" ((fn [v] v) new_val)) (->hydra_core_field "fields" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.Record" "fields")) ((fn [v] v) original))))))))))

(def hydra_dsl_core_term_annotated (fn [x] (list :inject (->hydra_core_injection "hydra.core.Term" (->hydra_core_field "annotated" ((fn [v] v) x))))))

(def hydra_dsl_core_term_application (fn [x] (list :inject (->hydra_core_injection "hydra.core.Term" (->hydra_core_field "application" ((fn [v] v) x))))))

(def hydra_dsl_core_term_cases (fn [x] (list :inject (->hydra_core_injection "hydra.core.Term" (->hydra_core_field "cases" ((fn [v] v) x))))))

(def hydra_dsl_core_term_either (fn [x] (list :inject (->hydra_core_injection "hydra.core.Term" (->hydra_core_field "either" ((fn [v] v) x))))))

(def hydra_dsl_core_term_inject (fn [x] (list :inject (->hydra_core_injection "hydra.core.Term" (->hydra_core_field "inject" ((fn [v] v) x))))))

(def hydra_dsl_core_term_lambda (fn [x] (list :inject (->hydra_core_injection "hydra.core.Term" (->hydra_core_field "lambda" ((fn [v] v) x))))))

(def hydra_dsl_core_term_let (fn [x] (list :inject (->hydra_core_injection "hydra.core.Term" (->hydra_core_field "let" ((fn [v] v) x))))))

(def hydra_dsl_core_term_list (fn [x] (list :inject (->hydra_core_injection "hydra.core.Term" (->hydra_core_field "list" ((fn [v] v) x))))))

(def hydra_dsl_core_term_literal (fn [x] (list :inject (->hydra_core_injection "hydra.core.Term" (->hydra_core_field "literal" ((fn [v] v) x))))))

(def hydra_dsl_core_term_map (fn [x] (list :inject (->hydra_core_injection "hydra.core.Term" (->hydra_core_field "map" ((fn [v] v) x))))))

(def hydra_dsl_core_term_maybe (fn [x] (list :inject (->hydra_core_injection "hydra.core.Term" (->hydra_core_field "maybe" ((fn [v] v) x))))))

(def hydra_dsl_core_term_pair (fn [x] (list :inject (->hydra_core_injection "hydra.core.Term" (->hydra_core_field "pair" ((fn [v] v) x))))))

(def hydra_dsl_core_term_project (fn [x] (list :inject (->hydra_core_injection "hydra.core.Term" (->hydra_core_field "project" ((fn [v] v) x))))))

(def hydra_dsl_core_term_record (fn [x] (list :inject (->hydra_core_injection "hydra.core.Term" (->hydra_core_field "record" ((fn [v] v) x))))))

(def hydra_dsl_core_term_set (fn [x] (list :inject (->hydra_core_injection "hydra.core.Term" (->hydra_core_field "set" ((fn [v] v) x))))))

(def hydra_dsl_core_term_type_application (fn [x] (list :inject (->hydra_core_injection "hydra.core.Term" (->hydra_core_field "typeApplication" ((fn [v] v) x))))))

(def hydra_dsl_core_term_type_lambda (fn [x] (list :inject (->hydra_core_injection "hydra.core.Term" (->hydra_core_field "typeLambda" ((fn [v] v) x))))))

(def hydra_dsl_core_term_unit (list :inject (->hydra_core_injection "hydra.core.Term" (->hydra_core_field "unit" (list :unit nil)))))

(def hydra_dsl_core_term_unwrap (fn [x] (list :inject (->hydra_core_injection "hydra.core.Term" (->hydra_core_field "unwrap" ((fn [v] v) x))))))

(def hydra_dsl_core_term_variable (fn [x] (list :inject (->hydra_core_injection "hydra.core.Term" (->hydra_core_field "variable" ((fn [v] v) x))))))

(def hydra_dsl_core_term_wrap (fn [x] (list :inject (->hydra_core_injection "hydra.core.Term" (->hydra_core_field "wrap" ((fn [v] v) x))))))

(def hydra_dsl_core_type_annotated (fn [x] (list :inject (->hydra_core_injection "hydra.core.Type" (->hydra_core_field "annotated" ((fn [v] v) x))))))

(def hydra_dsl_core_type_application (fn [x] (list :inject (->hydra_core_injection "hydra.core.Type" (->hydra_core_field "application" ((fn [v] v) x))))))

(def hydra_dsl_core_type_application_term (fn [body] (fn [type] (list :record (->hydra_core_record "hydra.core.TypeApplicationTerm" (list (->hydra_core_field "body" ((fn [v] v) body)) (->hydra_core_field "type" ((fn [v] v) type))))))))

(def hydra_dsl_core_type_application_term_body (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.TypeApplicationTerm" "body")) ((fn [v] v) x)))))

(def hydra_dsl_core_type_application_term_type (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.TypeApplicationTerm" "type")) ((fn [v] v) x)))))

(def hydra_dsl_core_type_application_term_with_body (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.TypeApplicationTerm" (list (->hydra_core_field "body" ((fn [v] v) new_val)) (->hydra_core_field "type" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.TypeApplicationTerm" "type")) ((fn [v] v) original))))))))))

(def hydra_dsl_core_type_application_term_with_type (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.TypeApplicationTerm" (list (->hydra_core_field "body" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.TypeApplicationTerm" "body")) ((fn [v] v) original)))) (->hydra_core_field "type" ((fn [v] v) new_val))))))))

(def hydra_dsl_core_type_either (fn [x] (list :inject (->hydra_core_injection "hydra.core.Type" (->hydra_core_field "either" ((fn [v] v) x))))))

(def hydra_dsl_core_type_forall (fn [x] (list :inject (->hydra_core_injection "hydra.core.Type" (->hydra_core_field "forall" ((fn [v] v) x))))))

(def hydra_dsl_core_type_function (fn [x] (list :inject (->hydra_core_injection "hydra.core.Type" (->hydra_core_field "function" ((fn [v] v) x))))))

(def hydra_dsl_core_type_lambda (fn [parameter] (fn [body] (list :record (->hydra_core_record "hydra.core.TypeLambda" (list (->hydra_core_field "parameter" ((fn [v] v) parameter)) (->hydra_core_field "body" ((fn [v] v) body))))))))

(def hydra_dsl_core_type_lambda_body (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.TypeLambda" "body")) ((fn [v] v) x)))))

(def hydra_dsl_core_type_lambda_parameter (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.TypeLambda" "parameter")) ((fn [v] v) x)))))

(def hydra_dsl_core_type_lambda_with_body (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.TypeLambda" (list (->hydra_core_field "parameter" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.TypeLambda" "parameter")) ((fn [v] v) original)))) (->hydra_core_field "body" ((fn [v] v) new_val))))))))

(def hydra_dsl_core_type_lambda_with_parameter (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.TypeLambda" (list (->hydra_core_field "parameter" ((fn [v] v) new_val)) (->hydra_core_field "body" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.TypeLambda" "body")) ((fn [v] v) original))))))))))

(def hydra_dsl_core_type_list (fn [x] (list :inject (->hydra_core_injection "hydra.core.Type" (->hydra_core_field "list" ((fn [v] v) x))))))

(def hydra_dsl_core_type_literal (fn [x] (list :inject (->hydra_core_injection "hydra.core.Type" (->hydra_core_field "literal" ((fn [v] v) x))))))

(def hydra_dsl_core_type_map (fn [x] (list :inject (->hydra_core_injection "hydra.core.Type" (->hydra_core_field "map" ((fn [v] v) x))))))

(def hydra_dsl_core_type_maybe (fn [x] (list :inject (->hydra_core_injection "hydra.core.Type" (->hydra_core_field "maybe" ((fn [v] v) x))))))

(def hydra_dsl_core_type_pair (fn [x] (list :inject (->hydra_core_injection "hydra.core.Type" (->hydra_core_field "pair" ((fn [v] v) x))))))

(def hydra_dsl_core_type_record (fn [x] (list :inject (->hydra_core_injection "hydra.core.Type" (->hydra_core_field "record" ((fn [v] v) x))))))

(def hydra_dsl_core_type_scheme (fn [variables] (fn [type] (fn [constraints] (list :record (->hydra_core_record "hydra.core.TypeScheme" (list (->hydra_core_field "variables" ((fn [v] v) variables)) (->hydra_core_field "type" ((fn [v] v) type)) (->hydra_core_field "constraints" ((fn [v] v) constraints)))))))))

(def hydra_dsl_core_type_scheme_constraints (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.TypeScheme" "constraints")) ((fn [v] v) x)))))

(def hydra_dsl_core_type_scheme_type (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.TypeScheme" "type")) ((fn [v] v) x)))))

(def hydra_dsl_core_type_scheme_variables (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.TypeScheme" "variables")) ((fn [v] v) x)))))

(def hydra_dsl_core_type_scheme_with_constraints (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.TypeScheme" (list (->hydra_core_field "variables" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.TypeScheme" "variables")) ((fn [v] v) original)))) (->hydra_core_field "type" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.TypeScheme" "type")) ((fn [v] v) original)))) (->hydra_core_field "constraints" ((fn [v] v) new_val))))))))

(def hydra_dsl_core_type_scheme_with_type (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.TypeScheme" (list (->hydra_core_field "variables" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.TypeScheme" "variables")) ((fn [v] v) original)))) (->hydra_core_field "type" ((fn [v] v) new_val)) (->hydra_core_field "constraints" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.TypeScheme" "constraints")) ((fn [v] v) original))))))))))

(def hydra_dsl_core_type_scheme_with_variables (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.TypeScheme" (list (->hydra_core_field "variables" ((fn [v] v) new_val)) (->hydra_core_field "type" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.TypeScheme" "type")) ((fn [v] v) original)))) (->hydra_core_field "constraints" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.TypeScheme" "constraints")) ((fn [v] v) original))))))))))

(def hydra_dsl_core_type_set (fn [x] (list :inject (->hydra_core_injection "hydra.core.Type" (->hydra_core_field "set" ((fn [v] v) x))))))

(def hydra_dsl_core_type_union (fn [x] (list :inject (->hydra_core_injection "hydra.core.Type" (->hydra_core_field "union" ((fn [v] v) x))))))

(def hydra_dsl_core_type_unit (list :inject (->hydra_core_injection "hydra.core.Type" (->hydra_core_field "unit" (list :unit nil)))))

(def hydra_dsl_core_type_variable (fn [x] (list :inject (->hydra_core_injection "hydra.core.Type" (->hydra_core_field "variable" ((fn [v] v) x))))))

(def hydra_dsl_core_type_variable_metadata (fn [classes] (list :record (->hydra_core_record "hydra.core.TypeVariableMetadata" (list (->hydra_core_field "classes" ((fn [v] v) classes)))))))

(def hydra_dsl_core_type_variable_metadata_classes (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.TypeVariableMetadata" "classes")) ((fn [v] v) x)))))

(def hydra_dsl_core_type_variable_metadata_with_classes (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.TypeVariableMetadata" (list (->hydra_core_field "classes" ((fn [v] v) new_val))))))))

(def hydra_dsl_core_type_void (list :inject (->hydra_core_injection "hydra.core.Type" (->hydra_core_field "void" (list :unit nil)))))

(def hydra_dsl_core_type_wrap (fn [x] (list :inject (->hydra_core_injection "hydra.core.Type" (->hydra_core_field "wrap" ((fn [v] v) x))))))

(def hydra_dsl_core_un_name (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.core.Name") ((fn [v] v) x)))))

(def hydra_dsl_core_wrapped_term (fn [type_name] (fn [body] (list :record (->hydra_core_record "hydra.core.WrappedTerm" (list (->hydra_core_field "typeName" ((fn [v] v) type_name)) (->hydra_core_field "body" ((fn [v] v) body))))))))

(def hydra_dsl_core_wrapped_term_body (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.WrappedTerm" "body")) ((fn [v] v) x)))))

(def hydra_dsl_core_wrapped_term_type_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.WrappedTerm" "typeName")) ((fn [v] v) x)))))

(def hydra_dsl_core_wrapped_term_with_body (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.WrappedTerm" (list (->hydra_core_field "typeName" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.WrappedTerm" "typeName")) ((fn [v] v) original)))) (->hydra_core_field "body" ((fn [v] v) new_val))))))))

(def hydra_dsl_core_wrapped_term_with_type_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.core.WrappedTerm" (list (->hydra_core_field "typeName" ((fn [v] v) new_val)) (->hydra_core_field "body" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.core.WrappedTerm" "body")) ((fn [v] v) original))))))))))
