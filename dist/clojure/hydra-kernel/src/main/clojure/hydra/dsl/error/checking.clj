(ns hydra.dsl.error.checking
  (:require [hydra.core :refer :all] [hydra.phantoms :refer :all]
))

(declare hydra_dsl_error_checking_checking_error_incorrect_unification hydra_dsl_error_checking_checking_error_not_a_forall_type hydra_dsl_error_checking_checking_error_not_a_function_type hydra_dsl_error_checking_checking_error_other hydra_dsl_error_checking_checking_error_type_arity_mismatch hydra_dsl_error_checking_checking_error_type_mismatch hydra_dsl_error_checking_checking_error_unbound_type_variables hydra_dsl_error_checking_checking_error_undefined_term_variable hydra_dsl_error_checking_checking_error_unequal_types hydra_dsl_error_checking_checking_error_unsupported_term_variant hydra_dsl_error_checking_checking_error_untyped_lambda hydra_dsl_error_checking_checking_error_untyped_let_binding hydra_dsl_error_checking_checking_error_untyped_term_variable hydra_dsl_error_checking_incorrect_unification_error hydra_dsl_error_checking_incorrect_unification_error_substitution hydra_dsl_error_checking_incorrect_unification_error_with_substitution hydra_dsl_error_checking_not_a_forall_type_error hydra_dsl_error_checking_not_a_forall_type_error_type hydra_dsl_error_checking_not_a_forall_type_error_type_arguments hydra_dsl_error_checking_not_a_forall_type_error_with_type hydra_dsl_error_checking_not_a_forall_type_error_with_type_arguments hydra_dsl_error_checking_not_a_function_type_error hydra_dsl_error_checking_not_a_function_type_error_type hydra_dsl_error_checking_not_a_function_type_error_with_type hydra_dsl_error_checking_other_checking_error hydra_dsl_error_checking_other_checking_error_message hydra_dsl_error_checking_other_checking_error_path hydra_dsl_error_checking_other_checking_error_with_message hydra_dsl_error_checking_other_checking_error_with_path hydra_dsl_error_checking_type_arity_mismatch_error hydra_dsl_error_checking_type_arity_mismatch_error_actual_arity hydra_dsl_error_checking_type_arity_mismatch_error_expected_arity hydra_dsl_error_checking_type_arity_mismatch_error_type hydra_dsl_error_checking_type_arity_mismatch_error_type_arguments hydra_dsl_error_checking_type_arity_mismatch_error_with_actual_arity hydra_dsl_error_checking_type_arity_mismatch_error_with_expected_arity hydra_dsl_error_checking_type_arity_mismatch_error_with_type hydra_dsl_error_checking_type_arity_mismatch_error_with_type_arguments hydra_dsl_error_checking_type_mismatch_error hydra_dsl_error_checking_type_mismatch_error_actual_type hydra_dsl_error_checking_type_mismatch_error_expected_type hydra_dsl_error_checking_type_mismatch_error_with_actual_type hydra_dsl_error_checking_type_mismatch_error_with_expected_type hydra_dsl_error_checking_unbound_type_variables_error hydra_dsl_error_checking_unbound_type_variables_error_type hydra_dsl_error_checking_unbound_type_variables_error_variables hydra_dsl_error_checking_unbound_type_variables_error_with_type hydra_dsl_error_checking_unbound_type_variables_error_with_variables hydra_dsl_error_checking_undefined_term_variable_checking_error hydra_dsl_error_checking_undefined_term_variable_checking_error_name hydra_dsl_error_checking_undefined_term_variable_checking_error_path hydra_dsl_error_checking_undefined_term_variable_checking_error_with_name hydra_dsl_error_checking_undefined_term_variable_checking_error_with_path hydra_dsl_error_checking_unequal_types_error hydra_dsl_error_checking_unequal_types_error_description hydra_dsl_error_checking_unequal_types_error_types hydra_dsl_error_checking_unequal_types_error_with_description hydra_dsl_error_checking_unequal_types_error_with_types hydra_dsl_error_checking_unsupported_term_variant_error hydra_dsl_error_checking_unsupported_term_variant_error_term_variant hydra_dsl_error_checking_unsupported_term_variant_error_with_term_variant hydra_dsl_error_checking_untyped_lambda_error hydra_dsl_error_checking_untyped_let_binding_error hydra_dsl_error_checking_untyped_let_binding_error_binding hydra_dsl_error_checking_untyped_let_binding_error_with_binding hydra_dsl_error_checking_untyped_term_variable_checking_error hydra_dsl_error_checking_untyped_term_variable_checking_error_name hydra_dsl_error_checking_untyped_term_variable_checking_error_path hydra_dsl_error_checking_untyped_term_variable_checking_error_with_name hydra_dsl_error_checking_untyped_term_variable_checking_error_with_path)

(def hydra_dsl_error_checking_checking_error_incorrect_unification (fn [x] (list :inject (->hydra_core_injection "hydra.error.checking.CheckingError" (->hydra_core_field "incorrectUnification" ((fn [v] v) x))))))

(def hydra_dsl_error_checking_checking_error_not_a_forall_type (fn [x] (list :inject (->hydra_core_injection "hydra.error.checking.CheckingError" (->hydra_core_field "notAForallType" ((fn [v] v) x))))))

(def hydra_dsl_error_checking_checking_error_not_a_function_type (fn [x] (list :inject (->hydra_core_injection "hydra.error.checking.CheckingError" (->hydra_core_field "notAFunctionType" ((fn [v] v) x))))))

(def hydra_dsl_error_checking_checking_error_other (fn [x] (list :inject (->hydra_core_injection "hydra.error.checking.CheckingError" (->hydra_core_field "other" ((fn [v] v) x))))))

(def hydra_dsl_error_checking_checking_error_type_arity_mismatch (fn [x] (list :inject (->hydra_core_injection "hydra.error.checking.CheckingError" (->hydra_core_field "typeArityMismatch" ((fn [v] v) x))))))

(def hydra_dsl_error_checking_checking_error_type_mismatch (fn [x] (list :inject (->hydra_core_injection "hydra.error.checking.CheckingError" (->hydra_core_field "typeMismatch" ((fn [v] v) x))))))

(def hydra_dsl_error_checking_checking_error_unbound_type_variables (fn [x] (list :inject (->hydra_core_injection "hydra.error.checking.CheckingError" (->hydra_core_field "unboundTypeVariables" ((fn [v] v) x))))))

(def hydra_dsl_error_checking_checking_error_undefined_term_variable (fn [x] (list :inject (->hydra_core_injection "hydra.error.checking.CheckingError" (->hydra_core_field "undefinedTermVariable" ((fn [v] v) x))))))

(def hydra_dsl_error_checking_checking_error_unequal_types (fn [x] (list :inject (->hydra_core_injection "hydra.error.checking.CheckingError" (->hydra_core_field "unequalTypes" ((fn [v] v) x))))))

(def hydra_dsl_error_checking_checking_error_unsupported_term_variant (fn [x] (list :inject (->hydra_core_injection "hydra.error.checking.CheckingError" (->hydra_core_field "unsupportedTermVariant" ((fn [v] v) x))))))

(def hydra_dsl_error_checking_checking_error_untyped_lambda (fn [x] (list :inject (->hydra_core_injection "hydra.error.checking.CheckingError" (->hydra_core_field "untypedLambda" ((fn [v] v) x))))))

(def hydra_dsl_error_checking_checking_error_untyped_let_binding (fn [x] (list :inject (->hydra_core_injection "hydra.error.checking.CheckingError" (->hydra_core_field "untypedLetBinding" ((fn [v] v) x))))))

(def hydra_dsl_error_checking_checking_error_untyped_term_variable (fn [x] (list :inject (->hydra_core_injection "hydra.error.checking.CheckingError" (->hydra_core_field "untypedTermVariable" ((fn [v] v) x))))))

(def hydra_dsl_error_checking_incorrect_unification_error (fn [substitution] (list :record (->hydra_core_record "hydra.error.checking.IncorrectUnificationError" (list (->hydra_core_field "substitution" ((fn [v] v) substitution)))))))

(def hydra_dsl_error_checking_incorrect_unification_error_substitution (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.IncorrectUnificationError" "substitution")) ((fn [v] v) x)))))

(def hydra_dsl_error_checking_incorrect_unification_error_with_substitution (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.checking.IncorrectUnificationError" (list (->hydra_core_field "substitution" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_checking_not_a_forall_type_error (fn [type] (fn [type_arguments] (list :record (->hydra_core_record "hydra.error.checking.NotAForallTypeError" (list (->hydra_core_field "type" ((fn [v] v) type)) (->hydra_core_field "typeArguments" ((fn [v] v) type_arguments))))))))

(def hydra_dsl_error_checking_not_a_forall_type_error_type (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.NotAForallTypeError" "type")) ((fn [v] v) x)))))

(def hydra_dsl_error_checking_not_a_forall_type_error_type_arguments (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.NotAForallTypeError" "typeArguments")) ((fn [v] v) x)))))

(def hydra_dsl_error_checking_not_a_forall_type_error_with_type (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.checking.NotAForallTypeError" (list (->hydra_core_field "type" ((fn [v] v) new_val)) (->hydra_core_field "typeArguments" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.NotAForallTypeError" "typeArguments")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_checking_not_a_forall_type_error_with_type_arguments (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.checking.NotAForallTypeError" (list (->hydra_core_field "type" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.NotAForallTypeError" "type")) ((fn [v] v) original)))) (->hydra_core_field "typeArguments" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_checking_not_a_function_type_error (fn [type] (list :record (->hydra_core_record "hydra.error.checking.NotAFunctionTypeError" (list (->hydra_core_field "type" ((fn [v] v) type)))))))

(def hydra_dsl_error_checking_not_a_function_type_error_type (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.NotAFunctionTypeError" "type")) ((fn [v] v) x)))))

(def hydra_dsl_error_checking_not_a_function_type_error_with_type (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.checking.NotAFunctionTypeError" (list (->hydra_core_field "type" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_checking_other_checking_error (fn [path] (fn [message] (list :record (->hydra_core_record "hydra.error.checking.OtherCheckingError" (list (->hydra_core_field "path" ((fn [v] v) path)) (->hydra_core_field "message" ((fn [v] v) message))))))))

(def hydra_dsl_error_checking_other_checking_error_message (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.OtherCheckingError" "message")) ((fn [v] v) x)))))

(def hydra_dsl_error_checking_other_checking_error_path (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.OtherCheckingError" "path")) ((fn [v] v) x)))))

(def hydra_dsl_error_checking_other_checking_error_with_message (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.checking.OtherCheckingError" (list (->hydra_core_field "path" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.OtherCheckingError" "path")) ((fn [v] v) original)))) (->hydra_core_field "message" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_checking_other_checking_error_with_path (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.checking.OtherCheckingError" (list (->hydra_core_field "path" ((fn [v] v) new_val)) (->hydra_core_field "message" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.OtherCheckingError" "message")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_checking_type_arity_mismatch_error (fn [type] (fn [expected_arity] (fn [actual_arity] (fn [type_arguments] (list :record (->hydra_core_record "hydra.error.checking.TypeArityMismatchError" (list (->hydra_core_field "type" ((fn [v] v) type)) (->hydra_core_field "expectedArity" ((fn [v] v) expected_arity)) (->hydra_core_field "actualArity" ((fn [v] v) actual_arity)) (->hydra_core_field "typeArguments" ((fn [v] v) type_arguments))))))))))

(def hydra_dsl_error_checking_type_arity_mismatch_error_actual_arity (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.TypeArityMismatchError" "actualArity")) ((fn [v] v) x)))))

(def hydra_dsl_error_checking_type_arity_mismatch_error_expected_arity (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.TypeArityMismatchError" "expectedArity")) ((fn [v] v) x)))))

(def hydra_dsl_error_checking_type_arity_mismatch_error_type (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.TypeArityMismatchError" "type")) ((fn [v] v) x)))))

(def hydra_dsl_error_checking_type_arity_mismatch_error_type_arguments (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.TypeArityMismatchError" "typeArguments")) ((fn [v] v) x)))))

(def hydra_dsl_error_checking_type_arity_mismatch_error_with_actual_arity (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.checking.TypeArityMismatchError" (list (->hydra_core_field "type" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.TypeArityMismatchError" "type")) ((fn [v] v) original)))) (->hydra_core_field "expectedArity" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.TypeArityMismatchError" "expectedArity")) ((fn [v] v) original)))) (->hydra_core_field "actualArity" ((fn [v] v) new_val)) (->hydra_core_field "typeArguments" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.TypeArityMismatchError" "typeArguments")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_checking_type_arity_mismatch_error_with_expected_arity (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.checking.TypeArityMismatchError" (list (->hydra_core_field "type" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.TypeArityMismatchError" "type")) ((fn [v] v) original)))) (->hydra_core_field "expectedArity" ((fn [v] v) new_val)) (->hydra_core_field "actualArity" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.TypeArityMismatchError" "actualArity")) ((fn [v] v) original)))) (->hydra_core_field "typeArguments" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.TypeArityMismatchError" "typeArguments")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_checking_type_arity_mismatch_error_with_type (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.checking.TypeArityMismatchError" (list (->hydra_core_field "type" ((fn [v] v) new_val)) (->hydra_core_field "expectedArity" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.TypeArityMismatchError" "expectedArity")) ((fn [v] v) original)))) (->hydra_core_field "actualArity" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.TypeArityMismatchError" "actualArity")) ((fn [v] v) original)))) (->hydra_core_field "typeArguments" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.TypeArityMismatchError" "typeArguments")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_checking_type_arity_mismatch_error_with_type_arguments (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.checking.TypeArityMismatchError" (list (->hydra_core_field "type" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.TypeArityMismatchError" "type")) ((fn [v] v) original)))) (->hydra_core_field "expectedArity" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.TypeArityMismatchError" "expectedArity")) ((fn [v] v) original)))) (->hydra_core_field "actualArity" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.TypeArityMismatchError" "actualArity")) ((fn [v] v) original)))) (->hydra_core_field "typeArguments" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_checking_type_mismatch_error (fn [expected_type] (fn [actual_type] (list :record (->hydra_core_record "hydra.error.checking.TypeMismatchError" (list (->hydra_core_field "expectedType" ((fn [v] v) expected_type)) (->hydra_core_field "actualType" ((fn [v] v) actual_type))))))))

(def hydra_dsl_error_checking_type_mismatch_error_actual_type (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.TypeMismatchError" "actualType")) ((fn [v] v) x)))))

(def hydra_dsl_error_checking_type_mismatch_error_expected_type (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.TypeMismatchError" "expectedType")) ((fn [v] v) x)))))

(def hydra_dsl_error_checking_type_mismatch_error_with_actual_type (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.checking.TypeMismatchError" (list (->hydra_core_field "expectedType" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.TypeMismatchError" "expectedType")) ((fn [v] v) original)))) (->hydra_core_field "actualType" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_checking_type_mismatch_error_with_expected_type (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.checking.TypeMismatchError" (list (->hydra_core_field "expectedType" ((fn [v] v) new_val)) (->hydra_core_field "actualType" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.TypeMismatchError" "actualType")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_checking_unbound_type_variables_error (fn [variables] (fn [type] (list :record (->hydra_core_record "hydra.error.checking.UnboundTypeVariablesError" (list (->hydra_core_field "variables" ((fn [v] v) variables)) (->hydra_core_field "type" ((fn [v] v) type))))))))

(def hydra_dsl_error_checking_unbound_type_variables_error_type (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.UnboundTypeVariablesError" "type")) ((fn [v] v) x)))))

(def hydra_dsl_error_checking_unbound_type_variables_error_variables (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.UnboundTypeVariablesError" "variables")) ((fn [v] v) x)))))

(def hydra_dsl_error_checking_unbound_type_variables_error_with_type (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.checking.UnboundTypeVariablesError" (list (->hydra_core_field "variables" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.UnboundTypeVariablesError" "variables")) ((fn [v] v) original)))) (->hydra_core_field "type" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_checking_unbound_type_variables_error_with_variables (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.checking.UnboundTypeVariablesError" (list (->hydra_core_field "variables" ((fn [v] v) new_val)) (->hydra_core_field "type" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.UnboundTypeVariablesError" "type")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_checking_undefined_term_variable_checking_error (fn [path] (fn [name] (list :record (->hydra_core_record "hydra.error.checking.UndefinedTermVariableCheckingError" (list (->hydra_core_field "path" ((fn [v] v) path)) (->hydra_core_field "name" ((fn [v] v) name))))))))

(def hydra_dsl_error_checking_undefined_term_variable_checking_error_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.UndefinedTermVariableCheckingError" "name")) ((fn [v] v) x)))))

(def hydra_dsl_error_checking_undefined_term_variable_checking_error_path (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.UndefinedTermVariableCheckingError" "path")) ((fn [v] v) x)))))

(def hydra_dsl_error_checking_undefined_term_variable_checking_error_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.checking.UndefinedTermVariableCheckingError" (list (->hydra_core_field "path" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.UndefinedTermVariableCheckingError" "path")) ((fn [v] v) original)))) (->hydra_core_field "name" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_checking_undefined_term_variable_checking_error_with_path (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.checking.UndefinedTermVariableCheckingError" (list (->hydra_core_field "path" ((fn [v] v) new_val)) (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.UndefinedTermVariableCheckingError" "name")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_checking_unequal_types_error (fn [types] (fn [description] (list :record (->hydra_core_record "hydra.error.checking.UnequalTypesError" (list (->hydra_core_field "types" ((fn [v] v) types)) (->hydra_core_field "description" ((fn [v] v) description))))))))

(def hydra_dsl_error_checking_unequal_types_error_description (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.UnequalTypesError" "description")) ((fn [v] v) x)))))

(def hydra_dsl_error_checking_unequal_types_error_types (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.UnequalTypesError" "types")) ((fn [v] v) x)))))

(def hydra_dsl_error_checking_unequal_types_error_with_description (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.checking.UnequalTypesError" (list (->hydra_core_field "types" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.UnequalTypesError" "types")) ((fn [v] v) original)))) (->hydra_core_field "description" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_checking_unequal_types_error_with_types (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.checking.UnequalTypesError" (list (->hydra_core_field "types" ((fn [v] v) new_val)) (->hydra_core_field "description" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.UnequalTypesError" "description")) ((fn [v] v) original))))))))))

(def hydra_dsl_error_checking_unsupported_term_variant_error (fn [term_variant] (list :record (->hydra_core_record "hydra.error.checking.UnsupportedTermVariantError" (list (->hydra_core_field "termVariant" ((fn [v] v) term_variant)))))))

(def hydra_dsl_error_checking_unsupported_term_variant_error_term_variant (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.UnsupportedTermVariantError" "termVariant")) ((fn [v] v) x)))))

(def hydra_dsl_error_checking_unsupported_term_variant_error_with_term_variant (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.checking.UnsupportedTermVariantError" (list (->hydra_core_field "termVariant" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_checking_untyped_lambda_error (list :record (->hydra_core_record "hydra.error.checking.UntypedLambdaError" (list))))

(def hydra_dsl_error_checking_untyped_let_binding_error (fn [binding] (list :record (->hydra_core_record "hydra.error.checking.UntypedLetBindingError" (list (->hydra_core_field "binding" ((fn [v] v) binding)))))))

(def hydra_dsl_error_checking_untyped_let_binding_error_binding (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.UntypedLetBindingError" "binding")) ((fn [v] v) x)))))

(def hydra_dsl_error_checking_untyped_let_binding_error_with_binding (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.checking.UntypedLetBindingError" (list (->hydra_core_field "binding" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_checking_untyped_term_variable_checking_error (fn [path] (fn [name] (list :record (->hydra_core_record "hydra.error.checking.UntypedTermVariableCheckingError" (list (->hydra_core_field "path" ((fn [v] v) path)) (->hydra_core_field "name" ((fn [v] v) name))))))))

(def hydra_dsl_error_checking_untyped_term_variable_checking_error_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.UntypedTermVariableCheckingError" "name")) ((fn [v] v) x)))))

(def hydra_dsl_error_checking_untyped_term_variable_checking_error_path (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.UntypedTermVariableCheckingError" "path")) ((fn [v] v) x)))))

(def hydra_dsl_error_checking_untyped_term_variable_checking_error_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.checking.UntypedTermVariableCheckingError" (list (->hydra_core_field "path" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.UntypedTermVariableCheckingError" "path")) ((fn [v] v) original)))) (->hydra_core_field "name" ((fn [v] v) new_val))))))))

(def hydra_dsl_error_checking_untyped_term_variable_checking_error_with_path (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.error.checking.UntypedTermVariableCheckingError" (list (->hydra_core_field "path" ((fn [v] v) new_val)) (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.error.checking.UntypedTermVariableCheckingError" "name")) ((fn [v] v) original))))))))))
