(defpackage :hydra.encode.error.core
(:use :cl :hydra.core :hydra.encode.accessors :hydra.encode.core :hydra.encode.variants :hydra.error.core)
(:export :hydra_encode_error_core_duplicate_binding_error :hydra_encode_error_core_duplicate_field_error :hydra_encode_error_core_invalid_term_error :hydra_encode_error_core_undefined_field_error :hydra_encode_error_core_undefined_term_error :hydra_encode_error_core_undefined_type_error :hydra_encode_error_core_unexpected_term_variant_error :hydra_encode_error_core_unexpected_type_variant_error))

(in-package :hydra.encode.error.core)

(cl:defvar hydra_encode_error_core_duplicate_binding_error (cl:lambda (x) (list :record (make-hydra_core_record "hydra.error.core.DuplicateBindingError" (cl:list (make-hydra_core_field "location" (hydra_encode_accessors_accessor_path ((cl:lambda (v) (hydra_error_core_duplicate_binding_error-location v)) x))) (make-hydra_core_field "name" (hydra_encode_core_name ((cl:lambda (v) (hydra_error_core_duplicate_binding_error-name v)) x))))))))

(cl:defvar hydra_encode_error_core_duplicate_field_error (cl:lambda (x) (list :record (make-hydra_core_record "hydra.error.core.DuplicateFieldError" (cl:list (make-hydra_core_field "location" (hydra_encode_accessors_accessor_path ((cl:lambda (v) (hydra_error_core_duplicate_field_error-location v)) x))) (make-hydra_core_field "name" (hydra_encode_core_name ((cl:lambda (v) (hydra_error_core_duplicate_field_error-name v)) x))))))))

(cl:defvar hydra_encode_error_core_invalid_term_error (cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :duplicate_binding) ((cl:lambda (y) (list :union (make-hydra_core_injection "hydra.error.core.InvalidTermError" (make-hydra_core_field "duplicateBinding" (hydra_encode_error_core_duplicate_binding_error y))))) match_value)) ((equal (car match_target) :duplicate_field) ((cl:lambda (y) (list :union (make-hydra_core_injection "hydra.error.core.InvalidTermError" (make-hydra_core_field "duplicateField" (hydra_encode_error_core_duplicate_field_error y))))) match_value)))) (cadr match_target))))

(cl:defvar hydra_encode_error_core_undefined_field_error (cl:lambda (x) (list :record (make-hydra_core_record "hydra.error.core.UndefinedFieldError" (cl:list (make-hydra_core_field "fieldName" (hydra_encode_core_name ((cl:lambda (v) (hydra_error_core_undefined_field_error-field_name v)) x))) (make-hydra_core_field "typeName" (hydra_encode_core_name ((cl:lambda (v) (hydra_error_core_undefined_field_error-type_name v)) x))))))))

(cl:defvar hydra_encode_error_core_undefined_term_error (cl:lambda (x) (list :record (make-hydra_core_record "hydra.error.core.UndefinedTermError" (cl:list (make-hydra_core_field "name" (hydra_encode_core_name ((cl:lambda (v) (hydra_error_core_undefined_term_error-name v)) x))))))))

(cl:defvar hydra_encode_error_core_undefined_type_error (cl:lambda (x) (list :record (make-hydra_core_record "hydra.error.core.UndefinedTypeError" (cl:list (make-hydra_core_field "name" (hydra_encode_core_name ((cl:lambda (v) (hydra_error_core_undefined_type_error-name v)) x))))))))

(cl:defvar hydra_encode_error_core_unexpected_term_variant_error (cl:lambda (x) (list :record (make-hydra_core_record "hydra.error.core.UnexpectedTermVariantError" (cl:list (make-hydra_core_field "expectedVariant" (hydra_encode_variants_term_variant ((cl:lambda (v) (hydra_error_core_unexpected_term_variant_error-expected_variant v)) x))) (make-hydra_core_field "actualTerm" (hydra_encode_core_term ((cl:lambda (v) (hydra_error_core_unexpected_term_variant_error-actual_term v)) x))))))))

(cl:defvar hydra_encode_error_core_unexpected_type_variant_error (cl:lambda (x) (list :record (make-hydra_core_record "hydra.error.core.UnexpectedTypeVariantError" (cl:list (make-hydra_core_field "expectedVariant" (hydra_encode_variants_type_variant ((cl:lambda (v) (hydra_error_core_unexpected_type_variant_error-expected_variant v)) x))) (make-hydra_core_field "actualType" (hydra_encode_core_type ((cl:lambda (v) (hydra_error_core_unexpected_type_variant_error-actual_type v)) x))))))))
