(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.encode.accessors)

(require 'hydra.encode.core)

(require 'hydra.encode.variants)

(require 'hydra.error.core)

(defvar hydra_encode_error_core_duplicate_binding_error (lambda (x) (list :record (make-hydra_core_record "hydra.error.core.DuplicateBindingError" (list (make-hydra_core_field "location" (hydra_encode_accessors_accessor_path (funcall (lambda (v) (hydra_error_core_duplicate_binding_error-location v)) x))) (make-hydra_core_field "name" (hydra_encode_core_name (funcall (lambda (v) (hydra_error_core_duplicate_binding_error-name v)) x))))))))

(defvar hydra_encode_error_core_duplicate_field_error (lambda (x) (list :record (make-hydra_core_record "hydra.error.core.DuplicateFieldError" (list (make-hydra_core_field "location" (hydra_encode_accessors_accessor_path (funcall (lambda (v) (hydra_error_core_duplicate_field_error-location v)) x))) (make-hydra_core_field "name" (hydra_encode_core_name (funcall (lambda (v) (hydra_error_core_duplicate_field_error-name v)) x))))))))

(defvar hydra_encode_error_core_invalid_term_error (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :duplicate_binding) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.error.core.InvalidTermError" (make-hydra_core_field "duplicateBinding" (hydra_encode_error_core_duplicate_binding_error y))))) match_value)) ((equal (car match_target) :duplicate_field) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.error.core.InvalidTermError" (make-hydra_core_field "duplicateField" (hydra_encode_error_core_duplicate_field_error y))))) match_value)))) (cadr match_target))))

(defvar hydra_encode_error_core_undefined_field_error (lambda (x) (list :record (make-hydra_core_record "hydra.error.core.UndefinedFieldError" (list (make-hydra_core_field "fieldName" (hydra_encode_core_name (funcall (lambda (v) (hydra_error_core_undefined_field_error-field_name v)) x))) (make-hydra_core_field "typeName" (hydra_encode_core_name (funcall (lambda (v) (hydra_error_core_undefined_field_error-type_name v)) x))))))))

(defvar hydra_encode_error_core_undefined_term_error (lambda (x) (list :record (make-hydra_core_record "hydra.error.core.UndefinedTermError" (list (make-hydra_core_field "name" (hydra_encode_core_name (funcall (lambda (v) (hydra_error_core_undefined_term_error-name v)) x))))))))

(defvar hydra_encode_error_core_undefined_type_error (lambda (x) (list :record (make-hydra_core_record "hydra.error.core.UndefinedTypeError" (list (make-hydra_core_field "name" (hydra_encode_core_name (funcall (lambda (v) (hydra_error_core_undefined_type_error-name v)) x))))))))

(defvar hydra_encode_error_core_unexpected_term_variant_error (lambda (x) (list :record (make-hydra_core_record "hydra.error.core.UnexpectedTermVariantError" (list (make-hydra_core_field "expectedVariant" (hydra_encode_variants_term_variant (funcall (lambda (v) (hydra_error_core_unexpected_term_variant_error-expected_variant v)) x))) (make-hydra_core_field "actualTerm" (hydra_encode_core_term (funcall (lambda (v) (hydra_error_core_unexpected_term_variant_error-actual_term v)) x))))))))

(defvar hydra_encode_error_core_unexpected_type_variant_error (lambda (x) (list :record (make-hydra_core_record "hydra.error.core.UnexpectedTypeVariantError" (list (make-hydra_core_field "expectedVariant" (hydra_encode_variants_type_variant (funcall (lambda (v) (hydra_error_core_unexpected_type_variant_error-expected_variant v)) x))) (make-hydra_core_field "actualType" (hydra_encode_core_type (funcall (lambda (v) (hydra_error_core_unexpected_type_variant_error-actual_type v)) x))))))))

(provide 'hydra.encode.error.core)
