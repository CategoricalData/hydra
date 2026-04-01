(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.encode.core)

(require 'hydra.encode.error.checking)

(require 'hydra.encode.error.core)

(require 'hydra.errors)

(defvar hydra_encode_errors_decoding_error (lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.errors.DecodingError" (funcall (lambda (x2) (list :literal (list :string x2))) (funcall (lambda (v) v) x))))))

(defvar hydra_encode_errors_other_error (lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.errors.OtherError" (funcall (lambda (x2) (list :literal (list :string x2))) (funcall (lambda (v) v) x))))))

(defvar hydra_encode_errors_unification_error (lambda (x) (list :record (make-hydra_core_record "hydra.errors.UnificationError" (list (make-hydra_core_field "leftType" (hydra_encode_core_type (funcall (lambda (v) (hydra_errors_unification_error-left_type v)) x))) (make-hydra_core_field "rightType" (hydra_encode_core_type (funcall (lambda (v) (hydra_errors_unification_error-right_type v)) x))) (make-hydra_core_field "message" (funcall (lambda (x2) (list :literal (list :string x2))) (funcall (lambda (v) (hydra_errors_unification_error-message v)) x))))))))

(defvar hydra_encode_errors_error (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :checking) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.errors.Error" (make-hydra_core_field "checking" (hydra_encode_error_checking_checking_error y))))) match_value)) ((equal (car match_target) :decoding) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.errors.Error" (make-hydra_core_field "decoding" (hydra_encode_errors_decoding_error y))))) match_value)) ((equal (car match_target) :duplicate_binding) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.errors.Error" (make-hydra_core_field "duplicateBinding" (hydra_encode_error_core_duplicate_binding_error y))))) match_value)) ((equal (car match_target) :duplicate_field) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.errors.Error" (make-hydra_core_field "duplicateField" (hydra_encode_error_core_duplicate_field_error y))))) match_value)) ((equal (car match_target) :other) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.errors.Error" (make-hydra_core_field "other" (hydra_encode_errors_other_error y))))) match_value)) ((equal (car match_target) :undefined_field) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.errors.Error" (make-hydra_core_field "undefinedField" (hydra_encode_error_core_undefined_field_error y))))) match_value)) ((equal (car match_target) :undefined_term_variable) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.errors.Error" (make-hydra_core_field "undefinedTermVariable" (hydra_encode_error_core_undefined_term_variable_error y))))) match_value)) ((equal (car match_target) :untyped_term_variable) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.errors.Error" (make-hydra_core_field "untypedTermVariable" (hydra_encode_error_core_untyped_term_variable_error y))))) match_value)) ((equal (car match_target) :unexpected_term_variant) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.errors.Error" (make-hydra_core_field "unexpectedTermVariant" (hydra_encode_error_core_unexpected_term_variant_error y))))) match_value)) ((equal (car match_target) :unexpected_type_variant) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.errors.Error" (make-hydra_core_field "unexpectedTypeVariant" (hydra_encode_error_core_unexpected_type_variant_error y))))) match_value)) ((equal (car match_target) :unification) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.errors.Error" (make-hydra_core_field "unification" (hydra_encode_errors_unification_error y))))) match_value)))) (cadr match_target))))

(provide 'hydra.encode.errors)
