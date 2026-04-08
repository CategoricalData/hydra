(defpackage :hydra.errors
(:use :cl :hydra.core :hydra.error.checking :hydra.error.core :hydra.paths)
(:export :make-hydra_errors_decoding_error :hydra_errors_decoding_error? :hydra_errors_decoding_error-value :hydra_errors_error-variants :hydra_errors_extraction_error-variants :hydra_errors_inference_error-variants :make-hydra_errors_multiple_bindings_error :hydra_errors_multiple_bindings_error? :hydra_errors_multiple_bindings_error-name :make-hydra_errors_multiple_fields_error :hydra_errors_multiple_fields_error? :hydra_errors_multiple_fields_error-field_name :make-hydra_errors_no_matching_field_error :hydra_errors_no_matching_field_error? :hydra_errors_no_matching_field_error-field_name :make-hydra_errors_no_such_binding_error :hydra_errors_no_such_binding_error? :hydra_errors_no_such_binding_error-name :make-hydra_errors_no_such_primitive_error :hydra_errors_no_such_primitive_error? :hydra_errors_no_such_primitive_error-name :make-hydra_errors_other_error :hydra_errors_other_error? :hydra_errors_other_error-value :make-hydra_errors_other_inference_error :hydra_errors_other_inference_error? :hydra_errors_other_inference_error-path :hydra_errors_other_inference_error-message :make-hydra_errors_other_resolution_error :hydra_errors_other_resolution_error? :hydra_errors_other_resolution_error-value :hydra_errors_resolution_error-variants :make-hydra_errors_unexpected_shape_error :hydra_errors_unexpected_shape_error? :hydra_errors_unexpected_shape_error-expected :hydra_errors_unexpected_shape_error-actual :make-hydra_errors_unification_error :hydra_errors_unification_error? :hydra_errors_unification_error-left_type :hydra_errors_unification_error-right_type :hydra_errors_unification_error-message :make-hydra_errors_unification_inference_error :hydra_errors_unification_inference_error? :hydra_errors_unification_inference_error-path :hydra_errors_unification_inference_error-cause))

(in-package :hydra.errors)

(cl:defstruct hydra_errors_decoding_error value)

(cl:defvar hydra_errors_error-variants (cl:list :checking :decoding :duplicate_binding :duplicate_field :extraction :inference :other :resolution :undefined_field :undefined_term_variable :untyped_term_variable :unexpected_term_variant :unexpected_type_variant :unification))

(cl:defvar hydra_errors_extraction_error-variants (cl:list :empty_list :multiple_bindings :multiple_fields :no_matching_field :no_such_binding :not_enough_cases :unexpected_shape))

(cl:defvar hydra_errors_inference_error-variants (cl:list :checking :other :unification))

(cl:defstruct hydra_errors_multiple_bindings_error name)

(cl:defstruct hydra_errors_multiple_fields_error field_name)

(cl:defstruct hydra_errors_no_matching_field_error field_name)

(cl:defstruct hydra_errors_no_such_binding_error name)

(cl:defstruct hydra_errors_no_such_primitive_error name)

(cl:defstruct hydra_errors_other_error value)

(cl:defstruct hydra_errors_other_inference_error path message)

(cl:defstruct hydra_errors_other_resolution_error value)

(cl:defvar hydra_errors_resolution_error-variants (cl:list :no_such_binding :no_such_primitive :no_matching_field :other :unexpected_shape))

(cl:defstruct hydra_errors_unexpected_shape_error expected actual)

(cl:defstruct hydra_errors_unification_error left_type right_type message)

(cl:defstruct hydra_errors_unification_inference_error path cause)
