(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.encode.core)

(require 'hydra.encode.typing)

(require 'hydra.encode.variants)

(require 'hydra.error.checking)

(require 'hydra.lib.lists)

(require 'hydra.lib.sets)

(defvar hydra_encode_error_checking_incorrect_unification_error (lambda (x) (list :record (make-hydra_core_record "hydra.error.checking.IncorrectUnificationError" (list (make-hydra_core_field "substitution" (hydra_encode_typing_type_subst (funcall (lambda (v) (hydra_error_checking_incorrect_unification_error-substitution v)) x))))))))

(defvar hydra_encode_error_checking_not_a_forall_type_error (lambda (x) (list :record (make-hydra_core_record "hydra.error.checking.NotAForallTypeError" (list (make-hydra_core_field "type" (hydra_encode_core_type (funcall (lambda (v) (hydra_error_checking_not_a_forall_type_error-type v)) x))) (make-hydra_core_field "typeArguments" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map hydra_encode_core_type) xs))) (funcall (lambda (v) (hydra_error_checking_not_a_forall_type_error-type_arguments v)) x))))))))

(defvar hydra_encode_error_checking_not_a_function_type_error (lambda (x) (list :record (make-hydra_core_record "hydra.error.checking.NotAFunctionTypeError" (list (make-hydra_core_field "type" (hydra_encode_core_type (funcall (lambda (v) (hydra_error_checking_not_a_function_type_error-type v)) x))))))))

(defvar hydra_encode_error_checking_type_arity_mismatch_error (lambda (x) (list :record (make-hydra_core_record "hydra.error.checking.TypeArityMismatchError" (list (make-hydra_core_field "type" (hydra_encode_core_type (funcall (lambda (v) (hydra_error_checking_type_arity_mismatch_error-type v)) x))) (make-hydra_core_field "expectedArity" (funcall (lambda (x) (list :literal (list :integer (list :int32 x)))) (funcall (lambda (v) (hydra_error_checking_type_arity_mismatch_error-expected_arity v)) x))) (make-hydra_core_field "actualArity" (funcall (lambda (x) (list :literal (list :integer (list :int32 x)))) (funcall (lambda (v) (hydra_error_checking_type_arity_mismatch_error-actual_arity v)) x))) (make-hydra_core_field "typeArguments" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map hydra_encode_core_type) xs))) (funcall (lambda (v) (hydra_error_checking_type_arity_mismatch_error-type_arguments v)) x))))))))

(defvar hydra_encode_error_checking_type_mismatch_error (lambda (x) (list :record (make-hydra_core_record "hydra.error.checking.TypeMismatchError" (list (make-hydra_core_field "expectedType" (hydra_encode_core_type (funcall (lambda (v) (hydra_error_checking_type_mismatch_error-expected_type v)) x))) (make-hydra_core_field "actualType" (hydra_encode_core_type (funcall (lambda (v) (hydra_error_checking_type_mismatch_error-actual_type v)) x))))))))

(defvar hydra_encode_error_checking_unbound_type_variables_error (lambda (x) (list :record (make-hydra_core_record "hydra.error.checking.UnboundTypeVariablesError" (list (make-hydra_core_field "variables" (funcall (lambda (s) (list :set (funcall (hydra_lib_sets_map hydra_encode_core_name) s))) (funcall (lambda (v) (hydra_error_checking_unbound_type_variables_error-variables v)) x))) (make-hydra_core_field "type" (hydra_encode_core_type (funcall (lambda (v) (hydra_error_checking_unbound_type_variables_error-type v)) x))))))))

(defvar hydra_encode_error_checking_unequal_types_error (lambda (x) (list :record (make-hydra_core_record "hydra.error.checking.UnequalTypesError" (list (make-hydra_core_field "types" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map hydra_encode_core_type) xs))) (funcall (lambda (v) (hydra_error_checking_unequal_types_error-types v)) x))) (make-hydra_core_field "description" (funcall (lambda (x) (list :literal (list :string x))) (funcall (lambda (v) (hydra_error_checking_unequal_types_error-description v)) x))))))))

(defvar hydra_encode_error_checking_unsupported_term_variant_error (lambda (x) (list :record (make-hydra_core_record "hydra.error.checking.UnsupportedTermVariantError" (list (make-hydra_core_field "termVariant" (hydra_encode_variants_term_variant (funcall (lambda (v) (hydra_error_checking_unsupported_term_variant_error-term_variant v)) x))))))))

(defvar hydra_encode_error_checking_untyped_lambda_error (lambda (x) (list :record (make-hydra_core_record "hydra.error.checking.UntypedLambdaError" (list)))))

(defvar hydra_encode_error_checking_untyped_let_binding_error (lambda (x) (list :record (make-hydra_core_record "hydra.error.checking.UntypedLetBindingError" (list (make-hydra_core_field "binding" (hydra_encode_core_binding (funcall (lambda (v) (hydra_error_checking_untyped_let_binding_error-binding v)) x))))))))

(defvar hydra_encode_error_checking_checking_error (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :incorrect_unification) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.error.checking.CheckingError" (make-hydra_core_field "incorrectUnification" (hydra_encode_error_checking_incorrect_unification_error y))))) match_value)) ((equal (car match_target) :not_a_forall_type) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.error.checking.CheckingError" (make-hydra_core_field "notAForallType" (hydra_encode_error_checking_not_a_forall_type_error y))))) match_value)) ((equal (car match_target) :not_a_function_type) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.error.checking.CheckingError" (make-hydra_core_field "notAFunctionType" (hydra_encode_error_checking_not_a_function_type_error y))))) match_value)) ((equal (car match_target) :type_arity_mismatch) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.error.checking.CheckingError" (make-hydra_core_field "typeArityMismatch" (hydra_encode_error_checking_type_arity_mismatch_error y))))) match_value)) ((equal (car match_target) :type_mismatch) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.error.checking.CheckingError" (make-hydra_core_field "typeMismatch" (hydra_encode_error_checking_type_mismatch_error y))))) match_value)) ((equal (car match_target) :unbound_type_variables) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.error.checking.CheckingError" (make-hydra_core_field "unboundTypeVariables" (hydra_encode_error_checking_unbound_type_variables_error y))))) match_value)) ((equal (car match_target) :unequal_types) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.error.checking.CheckingError" (make-hydra_core_field "unequalTypes" (hydra_encode_error_checking_unequal_types_error y))))) match_value)) ((equal (car match_target) :unsupported_term_variant) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.error.checking.CheckingError" (make-hydra_core_field "unsupportedTermVariant" (hydra_encode_error_checking_unsupported_term_variant_error y))))) match_value)) ((equal (car match_target) :untyped_lambda) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.error.checking.CheckingError" (make-hydra_core_field "untypedLambda" (hydra_encode_error_checking_untyped_lambda_error y))))) match_value)) ((equal (car match_target) :untyped_let_binding) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.error.checking.CheckingError" (make-hydra_core_field "untypedLetBinding" (hydra_encode_error_checking_untyped_let_binding_error y))))) match_value)))) (cadr match_target))))

(provide 'hydra.encode.error.checking)
