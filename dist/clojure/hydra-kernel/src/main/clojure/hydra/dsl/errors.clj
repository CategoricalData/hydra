(ns hydra.dsl.errors
  (:require [hydra.core :refer :all] [hydra.phantoms :refer :all]
))

(declare hydra_dsl_errors_decoding_error hydra_dsl_errors_error_checking hydra_dsl_errors_error_decoding hydra_dsl_errors_error_duplicate_binding hydra_dsl_errors_error_duplicate_field hydra_dsl_errors_error_extraction hydra_dsl_errors_error_inference hydra_dsl_errors_error_other hydra_dsl_errors_error_resolution hydra_dsl_errors_error_undefined_field hydra_dsl_errors_error_undefined_term_variable hydra_dsl_errors_error_unexpected_term_variant hydra_dsl_errors_error_unexpected_type_variant hydra_dsl_errors_error_unification hydra_dsl_errors_error_untyped_term_variable hydra_dsl_errors_extraction_error_empty_list hydra_dsl_errors_extraction_error_multiple_bindings hydra_dsl_errors_extraction_error_multiple_fields hydra_dsl_errors_extraction_error_no_matching_field hydra_dsl_errors_extraction_error_no_such_binding hydra_dsl_errors_extraction_error_not_enough_cases hydra_dsl_errors_extraction_error_unexpected_shape hydra_dsl_errors_inference_error_checking hydra_dsl_errors_inference_error_other hydra_dsl_errors_inference_error_unification hydra_dsl_errors_multiple_bindings_error hydra_dsl_errors_multiple_bindings_error_name hydra_dsl_errors_multiple_bindings_error_with_name hydra_dsl_errors_multiple_fields_error hydra_dsl_errors_multiple_fields_error_field_name hydra_dsl_errors_multiple_fields_error_with_field_name hydra_dsl_errors_no_matching_field_error hydra_dsl_errors_no_matching_field_error_field_name hydra_dsl_errors_no_matching_field_error_with_field_name hydra_dsl_errors_no_such_binding_error hydra_dsl_errors_no_such_binding_error_name hydra_dsl_errors_no_such_binding_error_with_name hydra_dsl_errors_no_such_primitive_error hydra_dsl_errors_no_such_primitive_error_name hydra_dsl_errors_no_such_primitive_error_with_name hydra_dsl_errors_other_error hydra_dsl_errors_other_inference_error hydra_dsl_errors_other_inference_error_message hydra_dsl_errors_other_inference_error_path hydra_dsl_errors_other_inference_error_with_message hydra_dsl_errors_other_inference_error_with_path hydra_dsl_errors_other_resolution_error hydra_dsl_errors_resolution_error_no_matching_field hydra_dsl_errors_resolution_error_no_such_binding hydra_dsl_errors_resolution_error_no_such_primitive hydra_dsl_errors_resolution_error_other hydra_dsl_errors_resolution_error_unexpected_shape hydra_dsl_errors_un_decoding_error hydra_dsl_errors_un_other_error hydra_dsl_errors_un_other_resolution_error hydra_dsl_errors_unexpected_shape_error hydra_dsl_errors_unexpected_shape_error_actual hydra_dsl_errors_unexpected_shape_error_expected hydra_dsl_errors_unexpected_shape_error_with_actual hydra_dsl_errors_unexpected_shape_error_with_expected hydra_dsl_errors_unification_error hydra_dsl_errors_unification_error_left_type hydra_dsl_errors_unification_error_message hydra_dsl_errors_unification_error_right_type hydra_dsl_errors_unification_error_with_left_type hydra_dsl_errors_unification_error_with_message hydra_dsl_errors_unification_error_with_right_type hydra_dsl_errors_unification_inference_error hydra_dsl_errors_unification_inference_error_cause hydra_dsl_errors_unification_inference_error_path hydra_dsl_errors_unification_inference_error_with_cause hydra_dsl_errors_unification_inference_error_with_path)

(def hydra_dsl_errors_decoding_error (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.errors.DecodingError" ((fn [v] v) x)))))

(def hydra_dsl_errors_error_checking (fn [x] (list :inject (->hydra_core_injection "hydra.errors.Error" (->hydra_core_field "checking" ((fn [v] v) x))))))

(def hydra_dsl_errors_error_decoding (fn [x] (list :inject (->hydra_core_injection "hydra.errors.Error" (->hydra_core_field "decoding" ((fn [v] v) x))))))

(def hydra_dsl_errors_error_duplicate_binding (fn [x] (list :inject (->hydra_core_injection "hydra.errors.Error" (->hydra_core_field "duplicateBinding" ((fn [v] v) x))))))

(def hydra_dsl_errors_error_duplicate_field (fn [x] (list :inject (->hydra_core_injection "hydra.errors.Error" (->hydra_core_field "duplicateField" ((fn [v] v) x))))))

(def hydra_dsl_errors_error_extraction (fn [x] (list :inject (->hydra_core_injection "hydra.errors.Error" (->hydra_core_field "extraction" ((fn [v] v) x))))))

(def hydra_dsl_errors_error_inference (fn [x] (list :inject (->hydra_core_injection "hydra.errors.Error" (->hydra_core_field "inference" ((fn [v] v) x))))))

(def hydra_dsl_errors_error_other (fn [x] (list :inject (->hydra_core_injection "hydra.errors.Error" (->hydra_core_field "other" ((fn [v] v) x))))))

(def hydra_dsl_errors_error_resolution (fn [x] (list :inject (->hydra_core_injection "hydra.errors.Error" (->hydra_core_field "resolution" ((fn [v] v) x))))))

(def hydra_dsl_errors_error_undefined_field (fn [x] (list :inject (->hydra_core_injection "hydra.errors.Error" (->hydra_core_field "undefinedField" ((fn [v] v) x))))))

(def hydra_dsl_errors_error_undefined_term_variable (fn [x] (list :inject (->hydra_core_injection "hydra.errors.Error" (->hydra_core_field "undefinedTermVariable" ((fn [v] v) x))))))

(def hydra_dsl_errors_error_unexpected_term_variant (fn [x] (list :inject (->hydra_core_injection "hydra.errors.Error" (->hydra_core_field "unexpectedTermVariant" ((fn [v] v) x))))))

(def hydra_dsl_errors_error_unexpected_type_variant (fn [x] (list :inject (->hydra_core_injection "hydra.errors.Error" (->hydra_core_field "unexpectedTypeVariant" ((fn [v] v) x))))))

(def hydra_dsl_errors_error_unification (fn [x] (list :inject (->hydra_core_injection "hydra.errors.Error" (->hydra_core_field "unification" ((fn [v] v) x))))))

(def hydra_dsl_errors_error_untyped_term_variable (fn [x] (list :inject (->hydra_core_injection "hydra.errors.Error" (->hydra_core_field "untypedTermVariable" ((fn [v] v) x))))))

(def hydra_dsl_errors_extraction_error_empty_list (fn [x] (list :inject (->hydra_core_injection "hydra.errors.ExtractionError" (->hydra_core_field "emptyList" ((fn [v] v) x))))))

(def hydra_dsl_errors_extraction_error_multiple_bindings (fn [x] (list :inject (->hydra_core_injection "hydra.errors.ExtractionError" (->hydra_core_field "multipleBindings" ((fn [v] v) x))))))

(def hydra_dsl_errors_extraction_error_multiple_fields (fn [x] (list :inject (->hydra_core_injection "hydra.errors.ExtractionError" (->hydra_core_field "multipleFields" ((fn [v] v) x))))))

(def hydra_dsl_errors_extraction_error_no_matching_field (fn [x] (list :inject (->hydra_core_injection "hydra.errors.ExtractionError" (->hydra_core_field "noMatchingField" ((fn [v] v) x))))))

(def hydra_dsl_errors_extraction_error_no_such_binding (fn [x] (list :inject (->hydra_core_injection "hydra.errors.ExtractionError" (->hydra_core_field "noSuchBinding" ((fn [v] v) x))))))

(def hydra_dsl_errors_extraction_error_not_enough_cases (fn [x] (list :inject (->hydra_core_injection "hydra.errors.ExtractionError" (->hydra_core_field "notEnoughCases" ((fn [v] v) x))))))

(def hydra_dsl_errors_extraction_error_unexpected_shape (fn [x] (list :inject (->hydra_core_injection "hydra.errors.ExtractionError" (->hydra_core_field "unexpectedShape" ((fn [v] v) x))))))

(def hydra_dsl_errors_inference_error_checking (fn [x] (list :inject (->hydra_core_injection "hydra.errors.InferenceError" (->hydra_core_field "checking" ((fn [v] v) x))))))

(def hydra_dsl_errors_inference_error_other (fn [x] (list :inject (->hydra_core_injection "hydra.errors.InferenceError" (->hydra_core_field "other" ((fn [v] v) x))))))

(def hydra_dsl_errors_inference_error_unification (fn [x] (list :inject (->hydra_core_injection "hydra.errors.InferenceError" (->hydra_core_field "unification" ((fn [v] v) x))))))

(def hydra_dsl_errors_multiple_bindings_error (fn [name] (list :record (->hydra_core_record "hydra.errors.MultipleBindingsError" (list (->hydra_core_field "name" ((fn [v] v) name)))))))

(def hydra_dsl_errors_multiple_bindings_error_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.errors.MultipleBindingsError" "name")) ((fn [v] v) x)))))

(def hydra_dsl_errors_multiple_bindings_error_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.errors.MultipleBindingsError" (list (->hydra_core_field "name" ((fn [v] v) new_val))))))))

(def hydra_dsl_errors_multiple_fields_error (fn [field_name] (list :record (->hydra_core_record "hydra.errors.MultipleFieldsError" (list (->hydra_core_field "fieldName" ((fn [v] v) field_name)))))))

(def hydra_dsl_errors_multiple_fields_error_field_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.errors.MultipleFieldsError" "fieldName")) ((fn [v] v) x)))))

(def hydra_dsl_errors_multiple_fields_error_with_field_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.errors.MultipleFieldsError" (list (->hydra_core_field "fieldName" ((fn [v] v) new_val))))))))

(def hydra_dsl_errors_no_matching_field_error (fn [field_name] (list :record (->hydra_core_record "hydra.errors.NoMatchingFieldError" (list (->hydra_core_field "fieldName" ((fn [v] v) field_name)))))))

(def hydra_dsl_errors_no_matching_field_error_field_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.errors.NoMatchingFieldError" "fieldName")) ((fn [v] v) x)))))

(def hydra_dsl_errors_no_matching_field_error_with_field_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.errors.NoMatchingFieldError" (list (->hydra_core_field "fieldName" ((fn [v] v) new_val))))))))

(def hydra_dsl_errors_no_such_binding_error (fn [name] (list :record (->hydra_core_record "hydra.errors.NoSuchBindingError" (list (->hydra_core_field "name" ((fn [v] v) name)))))))

(def hydra_dsl_errors_no_such_binding_error_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.errors.NoSuchBindingError" "name")) ((fn [v] v) x)))))

(def hydra_dsl_errors_no_such_binding_error_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.errors.NoSuchBindingError" (list (->hydra_core_field "name" ((fn [v] v) new_val))))))))

(def hydra_dsl_errors_no_such_primitive_error (fn [name] (list :record (->hydra_core_record "hydra.errors.NoSuchPrimitiveError" (list (->hydra_core_field "name" ((fn [v] v) name)))))))

(def hydra_dsl_errors_no_such_primitive_error_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.errors.NoSuchPrimitiveError" "name")) ((fn [v] v) x)))))

(def hydra_dsl_errors_no_such_primitive_error_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.errors.NoSuchPrimitiveError" (list (->hydra_core_field "name" ((fn [v] v) new_val))))))))

(def hydra_dsl_errors_other_error (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.errors.OtherError" ((fn [v] v) x)))))

(def hydra_dsl_errors_other_inference_error (fn [path] (fn [message] (list :record (->hydra_core_record "hydra.errors.OtherInferenceError" (list (->hydra_core_field "path" ((fn [v] v) path)) (->hydra_core_field "message" ((fn [v] v) message))))))))

(def hydra_dsl_errors_other_inference_error_message (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.errors.OtherInferenceError" "message")) ((fn [v] v) x)))))

(def hydra_dsl_errors_other_inference_error_path (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.errors.OtherInferenceError" "path")) ((fn [v] v) x)))))

(def hydra_dsl_errors_other_inference_error_with_message (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.errors.OtherInferenceError" (list (->hydra_core_field "path" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.errors.OtherInferenceError" "path")) ((fn [v] v) original)))) (->hydra_core_field "message" ((fn [v] v) new_val))))))))

(def hydra_dsl_errors_other_inference_error_with_path (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.errors.OtherInferenceError" (list (->hydra_core_field "path" ((fn [v] v) new_val)) (->hydra_core_field "message" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.errors.OtherInferenceError" "message")) ((fn [v] v) original))))))))))

(def hydra_dsl_errors_other_resolution_error (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.errors.OtherResolutionError" ((fn [v] v) x)))))

(def hydra_dsl_errors_resolution_error_no_matching_field (fn [x] (list :inject (->hydra_core_injection "hydra.errors.ResolutionError" (->hydra_core_field "noMatchingField" ((fn [v] v) x))))))

(def hydra_dsl_errors_resolution_error_no_such_binding (fn [x] (list :inject (->hydra_core_injection "hydra.errors.ResolutionError" (->hydra_core_field "noSuchBinding" ((fn [v] v) x))))))

(def hydra_dsl_errors_resolution_error_no_such_primitive (fn [x] (list :inject (->hydra_core_injection "hydra.errors.ResolutionError" (->hydra_core_field "noSuchPrimitive" ((fn [v] v) x))))))

(def hydra_dsl_errors_resolution_error_other (fn [x] (list :inject (->hydra_core_injection "hydra.errors.ResolutionError" (->hydra_core_field "other" ((fn [v] v) x))))))

(def hydra_dsl_errors_resolution_error_unexpected_shape (fn [x] (list :inject (->hydra_core_injection "hydra.errors.ResolutionError" (->hydra_core_field "unexpectedShape" ((fn [v] v) x))))))

(def hydra_dsl_errors_un_decoding_error (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.errors.DecodingError") ((fn [v] v) x)))))

(def hydra_dsl_errors_un_other_error (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.errors.OtherError") ((fn [v] v) x)))))

(def hydra_dsl_errors_un_other_resolution_error (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.errors.OtherResolutionError") ((fn [v] v) x)))))

(def hydra_dsl_errors_unexpected_shape_error (fn [expected] (fn [actual] (list :record (->hydra_core_record "hydra.errors.UnexpectedShapeError" (list (->hydra_core_field "expected" ((fn [v] v) expected)) (->hydra_core_field "actual" ((fn [v] v) actual))))))))

(def hydra_dsl_errors_unexpected_shape_error_actual (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.errors.UnexpectedShapeError" "actual")) ((fn [v] v) x)))))

(def hydra_dsl_errors_unexpected_shape_error_expected (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.errors.UnexpectedShapeError" "expected")) ((fn [v] v) x)))))

(def hydra_dsl_errors_unexpected_shape_error_with_actual (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.errors.UnexpectedShapeError" (list (->hydra_core_field "expected" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.errors.UnexpectedShapeError" "expected")) ((fn [v] v) original)))) (->hydra_core_field "actual" ((fn [v] v) new_val))))))))

(def hydra_dsl_errors_unexpected_shape_error_with_expected (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.errors.UnexpectedShapeError" (list (->hydra_core_field "expected" ((fn [v] v) new_val)) (->hydra_core_field "actual" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.errors.UnexpectedShapeError" "actual")) ((fn [v] v) original))))))))))

(def hydra_dsl_errors_unification_error (fn [left_type] (fn [right_type] (fn [message] (list :record (->hydra_core_record "hydra.errors.UnificationError" (list (->hydra_core_field "leftType" ((fn [v] v) left_type)) (->hydra_core_field "rightType" ((fn [v] v) right_type)) (->hydra_core_field "message" ((fn [v] v) message)))))))))

(def hydra_dsl_errors_unification_error_left_type (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.errors.UnificationError" "leftType")) ((fn [v] v) x)))))

(def hydra_dsl_errors_unification_error_message (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.errors.UnificationError" "message")) ((fn [v] v) x)))))

(def hydra_dsl_errors_unification_error_right_type (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.errors.UnificationError" "rightType")) ((fn [v] v) x)))))

(def hydra_dsl_errors_unification_error_with_left_type (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.errors.UnificationError" (list (->hydra_core_field "leftType" ((fn [v] v) new_val)) (->hydra_core_field "rightType" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.errors.UnificationError" "rightType")) ((fn [v] v) original)))) (->hydra_core_field "message" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.errors.UnificationError" "message")) ((fn [v] v) original))))))))))

(def hydra_dsl_errors_unification_error_with_message (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.errors.UnificationError" (list (->hydra_core_field "leftType" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.errors.UnificationError" "leftType")) ((fn [v] v) original)))) (->hydra_core_field "rightType" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.errors.UnificationError" "rightType")) ((fn [v] v) original)))) (->hydra_core_field "message" ((fn [v] v) new_val))))))))

(def hydra_dsl_errors_unification_error_with_right_type (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.errors.UnificationError" (list (->hydra_core_field "leftType" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.errors.UnificationError" "leftType")) ((fn [v] v) original)))) (->hydra_core_field "rightType" ((fn [v] v) new_val)) (->hydra_core_field "message" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.errors.UnificationError" "message")) ((fn [v] v) original))))))))))

(def hydra_dsl_errors_unification_inference_error (fn [path] (fn [cause] (list :record (->hydra_core_record "hydra.errors.UnificationInferenceError" (list (->hydra_core_field "path" ((fn [v] v) path)) (->hydra_core_field "cause" ((fn [v] v) cause))))))))

(def hydra_dsl_errors_unification_inference_error_cause (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.errors.UnificationInferenceError" "cause")) ((fn [v] v) x)))))

(def hydra_dsl_errors_unification_inference_error_path (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.errors.UnificationInferenceError" "path")) ((fn [v] v) x)))))

(def hydra_dsl_errors_unification_inference_error_with_cause (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.errors.UnificationInferenceError" (list (->hydra_core_field "path" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.errors.UnificationInferenceError" "path")) ((fn [v] v) original)))) (->hydra_core_field "cause" ((fn [v] v) new_val))))))))

(def hydra_dsl_errors_unification_inference_error_with_path (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.errors.UnificationInferenceError" (list (->hydra_core_field "path" ((fn [v] v) new_val)) (->hydra_core_field "cause" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.errors.UnificationInferenceError" "cause")) ((fn [v] v) original))))))))))
