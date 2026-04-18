(ns hydra.dsl.haskell.syntax
  (:require [hydra.core :refer :all] [hydra.phantoms :refer :all]
))

(declare hydra_dsl_haskell_syntax_alternative hydra_dsl_haskell_syntax_alternative_binds hydra_dsl_haskell_syntax_alternative_pattern hydra_dsl_haskell_syntax_alternative_rhs hydra_dsl_haskell_syntax_alternative_with_binds hydra_dsl_haskell_syntax_alternative_with_pattern hydra_dsl_haskell_syntax_alternative_with_rhs hydra_dsl_haskell_syntax_application_declaration_head hydra_dsl_haskell_syntax_application_declaration_head_function hydra_dsl_haskell_syntax_application_declaration_head_operand hydra_dsl_haskell_syntax_application_declaration_head_with_function hydra_dsl_haskell_syntax_application_declaration_head_with_operand hydra_dsl_haskell_syntax_application_expression hydra_dsl_haskell_syntax_application_expression_argument hydra_dsl_haskell_syntax_application_expression_function hydra_dsl_haskell_syntax_application_expression_with_argument hydra_dsl_haskell_syntax_application_expression_with_function hydra_dsl_haskell_syntax_application_pattern hydra_dsl_haskell_syntax_application_pattern_args hydra_dsl_haskell_syntax_application_pattern_name hydra_dsl_haskell_syntax_application_pattern_with_args hydra_dsl_haskell_syntax_application_pattern_with_name hydra_dsl_haskell_syntax_application_type hydra_dsl_haskell_syntax_application_type_argument hydra_dsl_haskell_syntax_application_type_context hydra_dsl_haskell_syntax_application_type_with_argument hydra_dsl_haskell_syntax_application_type_with_context hydra_dsl_haskell_syntax_as_pattern hydra_dsl_haskell_syntax_as_pattern_inner hydra_dsl_haskell_syntax_as_pattern_name hydra_dsl_haskell_syntax_as_pattern_with_inner hydra_dsl_haskell_syntax_as_pattern_with_name hydra_dsl_haskell_syntax_assertion_class hydra_dsl_haskell_syntax_assertion_tuple hydra_dsl_haskell_syntax_case_expression hydra_dsl_haskell_syntax_case_expression_alternatives hydra_dsl_haskell_syntax_case_expression_case hydra_dsl_haskell_syntax_case_expression_with_alternatives hydra_dsl_haskell_syntax_case_expression_with_case hydra_dsl_haskell_syntax_case_rhs hydra_dsl_haskell_syntax_class_assertion hydra_dsl_haskell_syntax_class_assertion_name hydra_dsl_haskell_syntax_class_assertion_types hydra_dsl_haskell_syntax_class_assertion_with_name hydra_dsl_haskell_syntax_class_assertion_with_types hydra_dsl_haskell_syntax_construct_record_expression hydra_dsl_haskell_syntax_construct_record_expression_fields hydra_dsl_haskell_syntax_construct_record_expression_name hydra_dsl_haskell_syntax_construct_record_expression_with_fields hydra_dsl_haskell_syntax_construct_record_expression_with_name hydra_dsl_haskell_syntax_constructor_ordinary hydra_dsl_haskell_syntax_constructor_record hydra_dsl_haskell_syntax_constructor_with_comments hydra_dsl_haskell_syntax_constructor_with_comments_body hydra_dsl_haskell_syntax_constructor_with_comments_comments hydra_dsl_haskell_syntax_constructor_with_comments_with_body hydra_dsl_haskell_syntax_constructor_with_comments_with_comments hydra_dsl_haskell_syntax_context_type hydra_dsl_haskell_syntax_context_type_ctx hydra_dsl_haskell_syntax_context_type_type hydra_dsl_haskell_syntax_context_type_with_ctx hydra_dsl_haskell_syntax_context_type_with_type hydra_dsl_haskell_syntax_data_declaration hydra_dsl_haskell_syntax_data_declaration_constructors hydra_dsl_haskell_syntax_data_declaration_context hydra_dsl_haskell_syntax_data_declaration_deriving hydra_dsl_haskell_syntax_data_declaration_head hydra_dsl_haskell_syntax_data_declaration_keyword hydra_dsl_haskell_syntax_data_declaration_with_constructors hydra_dsl_haskell_syntax_data_declaration_with_context hydra_dsl_haskell_syntax_data_declaration_with_deriving hydra_dsl_haskell_syntax_data_declaration_with_head hydra_dsl_haskell_syntax_data_declaration_with_keyword hydra_dsl_haskell_syntax_data_or_newtype_data hydra_dsl_haskell_syntax_data_or_newtype_newtype hydra_dsl_haskell_syntax_declaration_data hydra_dsl_haskell_syntax_declaration_head_application hydra_dsl_haskell_syntax_declaration_head_parens hydra_dsl_haskell_syntax_declaration_head_simple hydra_dsl_haskell_syntax_declaration_type hydra_dsl_haskell_syntax_declaration_typed_binding hydra_dsl_haskell_syntax_declaration_value_binding hydra_dsl_haskell_syntax_declaration_with_comments hydra_dsl_haskell_syntax_declaration_with_comments_body hydra_dsl_haskell_syntax_declaration_with_comments_comments hydra_dsl_haskell_syntax_declaration_with_comments_with_body hydra_dsl_haskell_syntax_declaration_with_comments_with_comments hydra_dsl_haskell_syntax_deriving hydra_dsl_haskell_syntax_export_declaration hydra_dsl_haskell_syntax_export_module hydra_dsl_haskell_syntax_expression_application hydra_dsl_haskell_syntax_expression_case hydra_dsl_haskell_syntax_expression_construct_record hydra_dsl_haskell_syntax_expression_do hydra_dsl_haskell_syntax_expression_if hydra_dsl_haskell_syntax_expression_infix_application hydra_dsl_haskell_syntax_expression_lambda hydra_dsl_haskell_syntax_expression_left_section hydra_dsl_haskell_syntax_expression_let hydra_dsl_haskell_syntax_expression_list hydra_dsl_haskell_syntax_expression_literal hydra_dsl_haskell_syntax_expression_parens hydra_dsl_haskell_syntax_expression_prefix_application hydra_dsl_haskell_syntax_expression_right_section hydra_dsl_haskell_syntax_expression_tuple hydra_dsl_haskell_syntax_expression_type_signature hydra_dsl_haskell_syntax_expression_update_record hydra_dsl_haskell_syntax_expression_variable hydra_dsl_haskell_syntax_field hydra_dsl_haskell_syntax_field_name hydra_dsl_haskell_syntax_field_type hydra_dsl_haskell_syntax_field_update hydra_dsl_haskell_syntax_field_update_name hydra_dsl_haskell_syntax_field_update_value hydra_dsl_haskell_syntax_field_update_with_name hydra_dsl_haskell_syntax_field_update_with_value hydra_dsl_haskell_syntax_field_with_comments hydra_dsl_haskell_syntax_field_with_comments_comments hydra_dsl_haskell_syntax_field_with_comments_field hydra_dsl_haskell_syntax_field_with_comments_with_comments hydra_dsl_haskell_syntax_field_with_comments_with_field hydra_dsl_haskell_syntax_field_with_name hydra_dsl_haskell_syntax_field_with_type hydra_dsl_haskell_syntax_function_type hydra_dsl_haskell_syntax_function_type_codomain hydra_dsl_haskell_syntax_function_type_domain hydra_dsl_haskell_syntax_function_type_with_codomain hydra_dsl_haskell_syntax_function_type_with_domain hydra_dsl_haskell_syntax_if_expression hydra_dsl_haskell_syntax_if_expression_condition hydra_dsl_haskell_syntax_if_expression_else hydra_dsl_haskell_syntax_if_expression_then hydra_dsl_haskell_syntax_if_expression_with_condition hydra_dsl_haskell_syntax_if_expression_with_else hydra_dsl_haskell_syntax_if_expression_with_then hydra_dsl_haskell_syntax_import hydra_dsl_haskell_syntax_import_as hydra_dsl_haskell_syntax_import_export_spec hydra_dsl_haskell_syntax_import_export_spec_modifier hydra_dsl_haskell_syntax_import_export_spec_name hydra_dsl_haskell_syntax_import_export_spec_subspec hydra_dsl_haskell_syntax_import_export_spec_with_modifier hydra_dsl_haskell_syntax_import_export_spec_with_name hydra_dsl_haskell_syntax_import_export_spec_with_subspec hydra_dsl_haskell_syntax_import_modifier_pattern hydra_dsl_haskell_syntax_import_modifier_type hydra_dsl_haskell_syntax_import_module hydra_dsl_haskell_syntax_import_qualified hydra_dsl_haskell_syntax_import_spec hydra_dsl_haskell_syntax_import_with_as hydra_dsl_haskell_syntax_import_with_module hydra_dsl_haskell_syntax_import_with_qualified hydra_dsl_haskell_syntax_import_with_spec hydra_dsl_haskell_syntax_infix_application_expression hydra_dsl_haskell_syntax_infix_application_expression_lhs hydra_dsl_haskell_syntax_infix_application_expression_operator hydra_dsl_haskell_syntax_infix_application_expression_rhs hydra_dsl_haskell_syntax_infix_application_expression_with_lhs hydra_dsl_haskell_syntax_infix_application_expression_with_operator hydra_dsl_haskell_syntax_infix_application_expression_with_rhs hydra_dsl_haskell_syntax_infix_type hydra_dsl_haskell_syntax_infix_type_lhs hydra_dsl_haskell_syntax_infix_type_operator hydra_dsl_haskell_syntax_infix_type_rhs hydra_dsl_haskell_syntax_infix_type_with_lhs hydra_dsl_haskell_syntax_infix_type_with_operator hydra_dsl_haskell_syntax_infix_type_with_rhs hydra_dsl_haskell_syntax_lambda_expression hydra_dsl_haskell_syntax_lambda_expression_bindings hydra_dsl_haskell_syntax_lambda_expression_inner hydra_dsl_haskell_syntax_lambda_expression_with_bindings hydra_dsl_haskell_syntax_lambda_expression_with_inner hydra_dsl_haskell_syntax_let_expression hydra_dsl_haskell_syntax_let_expression_bindings hydra_dsl_haskell_syntax_let_expression_inner hydra_dsl_haskell_syntax_let_expression_with_bindings hydra_dsl_haskell_syntax_let_expression_with_inner hydra_dsl_haskell_syntax_literal_char hydra_dsl_haskell_syntax_literal_double hydra_dsl_haskell_syntax_literal_float hydra_dsl_haskell_syntax_literal_int hydra_dsl_haskell_syntax_literal_integer hydra_dsl_haskell_syntax_literal_string hydra_dsl_haskell_syntax_local_binding_signature hydra_dsl_haskell_syntax_local_binding_value hydra_dsl_haskell_syntax_local_bindings hydra_dsl_haskell_syntax_module hydra_dsl_haskell_syntax_module_declarations hydra_dsl_haskell_syntax_module_head hydra_dsl_haskell_syntax_module_head_comments hydra_dsl_haskell_syntax_module_head_exports hydra_dsl_haskell_syntax_module_head_name hydra_dsl_haskell_syntax_module_head_with_comments hydra_dsl_haskell_syntax_module_head_with_exports hydra_dsl_haskell_syntax_module_head_with_name hydra_dsl_haskell_syntax_module_head_ hydra_dsl_haskell_syntax_module_imports hydra_dsl_haskell_syntax_module_name hydra_dsl_haskell_syntax_module_with_declarations hydra_dsl_haskell_syntax_module_with_head hydra_dsl_haskell_syntax_module_with_imports hydra_dsl_haskell_syntax_name_implicit hydra_dsl_haskell_syntax_name_normal hydra_dsl_haskell_syntax_name_parens hydra_dsl_haskell_syntax_name_part hydra_dsl_haskell_syntax_operator_backtick hydra_dsl_haskell_syntax_operator_normal hydra_dsl_haskell_syntax_ordinary_constructor hydra_dsl_haskell_syntax_ordinary_constructor_fields hydra_dsl_haskell_syntax_ordinary_constructor_name hydra_dsl_haskell_syntax_ordinary_constructor_with_fields hydra_dsl_haskell_syntax_ordinary_constructor_with_name hydra_dsl_haskell_syntax_pattern_application hydra_dsl_haskell_syntax_pattern_as hydra_dsl_haskell_syntax_pattern_field hydra_dsl_haskell_syntax_pattern_field_name hydra_dsl_haskell_syntax_pattern_field_pattern hydra_dsl_haskell_syntax_pattern_field_with_name hydra_dsl_haskell_syntax_pattern_field_with_pattern hydra_dsl_haskell_syntax_pattern_list hydra_dsl_haskell_syntax_pattern_literal hydra_dsl_haskell_syntax_pattern_name hydra_dsl_haskell_syntax_pattern_parens hydra_dsl_haskell_syntax_pattern_record hydra_dsl_haskell_syntax_pattern_tuple hydra_dsl_haskell_syntax_pattern_typed hydra_dsl_haskell_syntax_pattern_wildcard hydra_dsl_haskell_syntax_prefix_application_expression hydra_dsl_haskell_syntax_prefix_application_expression_operator hydra_dsl_haskell_syntax_prefix_application_expression_rhs hydra_dsl_haskell_syntax_prefix_application_expression_with_operator hydra_dsl_haskell_syntax_prefix_application_expression_with_rhs hydra_dsl_haskell_syntax_qualified_name hydra_dsl_haskell_syntax_qualified_name_qualifiers hydra_dsl_haskell_syntax_qualified_name_unqualified hydra_dsl_haskell_syntax_qualified_name_with_qualifiers hydra_dsl_haskell_syntax_qualified_name_with_unqualified hydra_dsl_haskell_syntax_record_constructor hydra_dsl_haskell_syntax_record_constructor_fields hydra_dsl_haskell_syntax_record_constructor_name hydra_dsl_haskell_syntax_record_constructor_with_fields hydra_dsl_haskell_syntax_record_constructor_with_name hydra_dsl_haskell_syntax_record_pattern hydra_dsl_haskell_syntax_record_pattern_fields hydra_dsl_haskell_syntax_record_pattern_name hydra_dsl_haskell_syntax_record_pattern_with_fields hydra_dsl_haskell_syntax_record_pattern_with_name hydra_dsl_haskell_syntax_right_hand_side hydra_dsl_haskell_syntax_section_expression hydra_dsl_haskell_syntax_section_expression_expression hydra_dsl_haskell_syntax_section_expression_operator hydra_dsl_haskell_syntax_section_expression_with_expression hydra_dsl_haskell_syntax_section_expression_with_operator hydra_dsl_haskell_syntax_simple_value_binding hydra_dsl_haskell_syntax_simple_value_binding_local_bindings hydra_dsl_haskell_syntax_simple_value_binding_pattern hydra_dsl_haskell_syntax_simple_value_binding_rhs hydra_dsl_haskell_syntax_simple_value_binding_with_local_bindings hydra_dsl_haskell_syntax_simple_value_binding_with_pattern hydra_dsl_haskell_syntax_simple_value_binding_with_rhs hydra_dsl_haskell_syntax_spec_import_hiding hydra_dsl_haskell_syntax_spec_import_list hydra_dsl_haskell_syntax_statement hydra_dsl_haskell_syntax_subspec_import_export_spec_all hydra_dsl_haskell_syntax_subspec_import_export_spec_list hydra_dsl_haskell_syntax_type_application hydra_dsl_haskell_syntax_type_ctx hydra_dsl_haskell_syntax_type_declaration hydra_dsl_haskell_syntax_type_declaration_name hydra_dsl_haskell_syntax_type_declaration_type hydra_dsl_haskell_syntax_type_declaration_with_name hydra_dsl_haskell_syntax_type_declaration_with_type hydra_dsl_haskell_syntax_type_function hydra_dsl_haskell_syntax_type_infix hydra_dsl_haskell_syntax_type_list hydra_dsl_haskell_syntax_type_parens hydra_dsl_haskell_syntax_type_signature hydra_dsl_haskell_syntax_type_signature_expression hydra_dsl_haskell_syntax_type_signature_expression_inner hydra_dsl_haskell_syntax_type_signature_expression_type hydra_dsl_haskell_syntax_type_signature_expression_with_inner hydra_dsl_haskell_syntax_type_signature_expression_with_type hydra_dsl_haskell_syntax_type_signature_name hydra_dsl_haskell_syntax_type_signature_type hydra_dsl_haskell_syntax_type_signature_with_name hydra_dsl_haskell_syntax_type_signature_with_type hydra_dsl_haskell_syntax_type_tuple hydra_dsl_haskell_syntax_type_variable hydra_dsl_haskell_syntax_typed_binding hydra_dsl_haskell_syntax_typed_binding_type_signature hydra_dsl_haskell_syntax_typed_binding_value_binding hydra_dsl_haskell_syntax_typed_binding_with_type_signature hydra_dsl_haskell_syntax_typed_binding_with_value_binding hydra_dsl_haskell_syntax_typed_pattern hydra_dsl_haskell_syntax_typed_pattern_inner hydra_dsl_haskell_syntax_typed_pattern_type hydra_dsl_haskell_syntax_typed_pattern_with_inner hydra_dsl_haskell_syntax_typed_pattern_with_type hydra_dsl_haskell_syntax_un_case_rhs hydra_dsl_haskell_syntax_un_deriving hydra_dsl_haskell_syntax_un_local_bindings hydra_dsl_haskell_syntax_un_module_name hydra_dsl_haskell_syntax_un_name_part hydra_dsl_haskell_syntax_un_right_hand_side hydra_dsl_haskell_syntax_un_statement hydra_dsl_haskell_syntax_un_variable hydra_dsl_haskell_syntax_update_record_expression hydra_dsl_haskell_syntax_update_record_expression_fields hydra_dsl_haskell_syntax_update_record_expression_inner hydra_dsl_haskell_syntax_update_record_expression_with_fields hydra_dsl_haskell_syntax_update_record_expression_with_inner hydra_dsl_haskell_syntax_value_binding_simple hydra_dsl_haskell_syntax_variable)

(def hydra_dsl_haskell_syntax_alternative (fn [pattern] (fn [rhs] (fn [binds] (list :record (->hydra_core_record "hydra.haskell.syntax.Alternative" (list (->hydra_core_field "pattern" ((fn [v] v) pattern)) (->hydra_core_field "rhs" ((fn [v] v) rhs)) (->hydra_core_field "binds" ((fn [v] v) binds)))))))))

(def hydra_dsl_haskell_syntax_alternative_binds (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Alternative" "binds")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_alternative_pattern (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Alternative" "pattern")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_alternative_rhs (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Alternative" "rhs")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_alternative_with_binds (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.Alternative" (list (->hydra_core_field "pattern" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Alternative" "pattern")) ((fn [v] v) original)))) (->hydra_core_field "rhs" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Alternative" "rhs")) ((fn [v] v) original)))) (->hydra_core_field "binds" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_alternative_with_pattern (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.Alternative" (list (->hydra_core_field "pattern" ((fn [v] v) new_val)) (->hydra_core_field "rhs" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Alternative" "rhs")) ((fn [v] v) original)))) (->hydra_core_field "binds" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Alternative" "binds")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_alternative_with_rhs (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.Alternative" (list (->hydra_core_field "pattern" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Alternative" "pattern")) ((fn [v] v) original)))) (->hydra_core_field "rhs" ((fn [v] v) new_val)) (->hydra_core_field "binds" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Alternative" "binds")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_application_declaration_head (fn [function_] (fn [operand] (list :record (->hydra_core_record "hydra.haskell.syntax.ApplicationDeclarationHead" (list (->hydra_core_field "function" ((fn [v] v) function_)) (->hydra_core_field "operand" ((fn [v] v) operand))))))))

(def hydra_dsl_haskell_syntax_application_declaration_head_function (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ApplicationDeclarationHead" "function")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_application_declaration_head_operand (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ApplicationDeclarationHead" "operand")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_application_declaration_head_with_function (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.ApplicationDeclarationHead" (list (->hydra_core_field "function" ((fn [v] v) new_val)) (->hydra_core_field "operand" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ApplicationDeclarationHead" "operand")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_application_declaration_head_with_operand (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.ApplicationDeclarationHead" (list (->hydra_core_field "function" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ApplicationDeclarationHead" "function")) ((fn [v] v) original)))) (->hydra_core_field "operand" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_application_expression (fn [function_] (fn [argument] (list :record (->hydra_core_record "hydra.haskell.syntax.ApplicationExpression" (list (->hydra_core_field "function" ((fn [v] v) function_)) (->hydra_core_field "argument" ((fn [v] v) argument))))))))

(def hydra_dsl_haskell_syntax_application_expression_argument (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ApplicationExpression" "argument")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_application_expression_function (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ApplicationExpression" "function")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_application_expression_with_argument (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.ApplicationExpression" (list (->hydra_core_field "function" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ApplicationExpression" "function")) ((fn [v] v) original)))) (->hydra_core_field "argument" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_application_expression_with_function (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.ApplicationExpression" (list (->hydra_core_field "function" ((fn [v] v) new_val)) (->hydra_core_field "argument" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ApplicationExpression" "argument")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_application_pattern (fn [name] (fn [args] (list :record (->hydra_core_record "hydra.haskell.syntax.ApplicationPattern" (list (->hydra_core_field "name" ((fn [v] v) name)) (->hydra_core_field "args" ((fn [v] v) args))))))))

(def hydra_dsl_haskell_syntax_application_pattern_args (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ApplicationPattern" "args")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_application_pattern_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ApplicationPattern" "name")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_application_pattern_with_args (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.ApplicationPattern" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ApplicationPattern" "name")) ((fn [v] v) original)))) (->hydra_core_field "args" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_application_pattern_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.ApplicationPattern" (list (->hydra_core_field "name" ((fn [v] v) new_val)) (->hydra_core_field "args" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ApplicationPattern" "args")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_application_type (fn [context] (fn [argument] (list :record (->hydra_core_record "hydra.haskell.syntax.ApplicationType" (list (->hydra_core_field "context" ((fn [v] v) context)) (->hydra_core_field "argument" ((fn [v] v) argument))))))))

(def hydra_dsl_haskell_syntax_application_type_argument (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ApplicationType" "argument")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_application_type_context (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ApplicationType" "context")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_application_type_with_argument (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.ApplicationType" (list (->hydra_core_field "context" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ApplicationType" "context")) ((fn [v] v) original)))) (->hydra_core_field "argument" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_application_type_with_context (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.ApplicationType" (list (->hydra_core_field "context" ((fn [v] v) new_val)) (->hydra_core_field "argument" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ApplicationType" "argument")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_as_pattern (fn [name] (fn [inner] (list :record (->hydra_core_record "hydra.haskell.syntax.AsPattern" (list (->hydra_core_field "name" ((fn [v] v) name)) (->hydra_core_field "inner" ((fn [v] v) inner))))))))

(def hydra_dsl_haskell_syntax_as_pattern_inner (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.AsPattern" "inner")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_as_pattern_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.AsPattern" "name")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_as_pattern_with_inner (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.AsPattern" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.AsPattern" "name")) ((fn [v] v) original)))) (->hydra_core_field "inner" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_as_pattern_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.AsPattern" (list (->hydra_core_field "name" ((fn [v] v) new_val)) (->hydra_core_field "inner" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.AsPattern" "inner")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_assertion_class (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Assertion" (->hydra_core_field "class" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_assertion_tuple (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Assertion" (->hydra_core_field "tuple" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_case_expression (fn [case_] (fn [alternatives] (list :record (->hydra_core_record "hydra.haskell.syntax.CaseExpression" (list (->hydra_core_field "case" ((fn [v] v) case_)) (->hydra_core_field "alternatives" ((fn [v] v) alternatives))))))))

(def hydra_dsl_haskell_syntax_case_expression_alternatives (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.CaseExpression" "alternatives")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_case_expression_case (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.CaseExpression" "case")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_case_expression_with_alternatives (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.CaseExpression" (list (->hydra_core_field "case" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.CaseExpression" "case")) ((fn [v] v) original)))) (->hydra_core_field "alternatives" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_case_expression_with_case (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.CaseExpression" (list (->hydra_core_field "case" ((fn [v] v) new_val)) (->hydra_core_field "alternatives" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.CaseExpression" "alternatives")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_case_rhs (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.haskell.syntax.CaseRhs" ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_class_assertion (fn [name] (fn [types] (list :record (->hydra_core_record "hydra.haskell.syntax.ClassAssertion" (list (->hydra_core_field "name" ((fn [v] v) name)) (->hydra_core_field "types" ((fn [v] v) types))))))))

(def hydra_dsl_haskell_syntax_class_assertion_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ClassAssertion" "name")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_class_assertion_types (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ClassAssertion" "types")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_class_assertion_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.ClassAssertion" (list (->hydra_core_field "name" ((fn [v] v) new_val)) (->hydra_core_field "types" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ClassAssertion" "types")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_class_assertion_with_types (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.ClassAssertion" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ClassAssertion" "name")) ((fn [v] v) original)))) (->hydra_core_field "types" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_construct_record_expression (fn [name] (fn [fields] (list :record (->hydra_core_record "hydra.haskell.syntax.ConstructRecordExpression" (list (->hydra_core_field "name" ((fn [v] v) name)) (->hydra_core_field "fields" ((fn [v] v) fields))))))))

(def hydra_dsl_haskell_syntax_construct_record_expression_fields (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ConstructRecordExpression" "fields")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_construct_record_expression_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ConstructRecordExpression" "name")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_construct_record_expression_with_fields (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.ConstructRecordExpression" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ConstructRecordExpression" "name")) ((fn [v] v) original)))) (->hydra_core_field "fields" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_construct_record_expression_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.ConstructRecordExpression" (list (->hydra_core_field "name" ((fn [v] v) new_val)) (->hydra_core_field "fields" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ConstructRecordExpression" "fields")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_constructor_ordinary (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Constructor" (->hydra_core_field "ordinary" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_constructor_record (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Constructor" (->hydra_core_field "record" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_constructor_with_comments (fn [body] (fn [comments] (list :record (->hydra_core_record "hydra.haskell.syntax.ConstructorWithComments" (list (->hydra_core_field "body" ((fn [v] v) body)) (->hydra_core_field "comments" ((fn [v] v) comments))))))))

(def hydra_dsl_haskell_syntax_constructor_with_comments_body (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ConstructorWithComments" "body")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_constructor_with_comments_comments (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ConstructorWithComments" "comments")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_constructor_with_comments_with_body (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.ConstructorWithComments" (list (->hydra_core_field "body" ((fn [v] v) new_val)) (->hydra_core_field "comments" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ConstructorWithComments" "comments")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_constructor_with_comments_with_comments (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.ConstructorWithComments" (list (->hydra_core_field "body" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ConstructorWithComments" "body")) ((fn [v] v) original)))) (->hydra_core_field "comments" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_context_type (fn [ctx] (fn [type] (list :record (->hydra_core_record "hydra.haskell.syntax.ContextType" (list (->hydra_core_field "ctx" ((fn [v] v) ctx)) (->hydra_core_field "type" ((fn [v] v) type))))))))

(def hydra_dsl_haskell_syntax_context_type_ctx (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ContextType" "ctx")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_context_type_type (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ContextType" "type")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_context_type_with_ctx (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.ContextType" (list (->hydra_core_field "ctx" ((fn [v] v) new_val)) (->hydra_core_field "type" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ContextType" "type")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_context_type_with_type (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.ContextType" (list (->hydra_core_field "ctx" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ContextType" "ctx")) ((fn [v] v) original)))) (->hydra_core_field "type" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_data_declaration (fn [keyword] (fn [context] (fn [head] (fn [constructors] (fn [deriving] (list :record (->hydra_core_record "hydra.haskell.syntax.DataDeclaration" (list (->hydra_core_field "keyword" ((fn [v] v) keyword)) (->hydra_core_field "context" ((fn [v] v) context)) (->hydra_core_field "head" ((fn [v] v) head)) (->hydra_core_field "constructors" ((fn [v] v) constructors)) (->hydra_core_field "deriving" ((fn [v] v) deriving)))))))))))

(def hydra_dsl_haskell_syntax_data_declaration_constructors (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DataDeclaration" "constructors")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_data_declaration_context (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DataDeclaration" "context")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_data_declaration_deriving (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DataDeclaration" "deriving")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_data_declaration_head (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DataDeclaration" "head")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_data_declaration_keyword (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DataDeclaration" "keyword")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_data_declaration_with_constructors (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.DataDeclaration" (list (->hydra_core_field "keyword" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DataDeclaration" "keyword")) ((fn [v] v) original)))) (->hydra_core_field "context" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DataDeclaration" "context")) ((fn [v] v) original)))) (->hydra_core_field "head" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DataDeclaration" "head")) ((fn [v] v) original)))) (->hydra_core_field "constructors" ((fn [v] v) new_val)) (->hydra_core_field "deriving" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DataDeclaration" "deriving")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_data_declaration_with_context (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.DataDeclaration" (list (->hydra_core_field "keyword" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DataDeclaration" "keyword")) ((fn [v] v) original)))) (->hydra_core_field "context" ((fn [v] v) new_val)) (->hydra_core_field "head" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DataDeclaration" "head")) ((fn [v] v) original)))) (->hydra_core_field "constructors" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DataDeclaration" "constructors")) ((fn [v] v) original)))) (->hydra_core_field "deriving" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DataDeclaration" "deriving")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_data_declaration_with_deriving (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.DataDeclaration" (list (->hydra_core_field "keyword" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DataDeclaration" "keyword")) ((fn [v] v) original)))) (->hydra_core_field "context" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DataDeclaration" "context")) ((fn [v] v) original)))) (->hydra_core_field "head" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DataDeclaration" "head")) ((fn [v] v) original)))) (->hydra_core_field "constructors" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DataDeclaration" "constructors")) ((fn [v] v) original)))) (->hydra_core_field "deriving" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_data_declaration_with_head (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.DataDeclaration" (list (->hydra_core_field "keyword" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DataDeclaration" "keyword")) ((fn [v] v) original)))) (->hydra_core_field "context" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DataDeclaration" "context")) ((fn [v] v) original)))) (->hydra_core_field "head" ((fn [v] v) new_val)) (->hydra_core_field "constructors" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DataDeclaration" "constructors")) ((fn [v] v) original)))) (->hydra_core_field "deriving" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DataDeclaration" "deriving")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_data_declaration_with_keyword (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.DataDeclaration" (list (->hydra_core_field "keyword" ((fn [v] v) new_val)) (->hydra_core_field "context" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DataDeclaration" "context")) ((fn [v] v) original)))) (->hydra_core_field "head" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DataDeclaration" "head")) ((fn [v] v) original)))) (->hydra_core_field "constructors" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DataDeclaration" "constructors")) ((fn [v] v) original)))) (->hydra_core_field "deriving" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DataDeclaration" "deriving")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_data_or_newtype_data (list :inject (->hydra_core_injection "hydra.haskell.syntax.DataOrNewtype" (->hydra_core_field "data" (list :unit nil)))))

(def hydra_dsl_haskell_syntax_data_or_newtype_newtype (list :inject (->hydra_core_injection "hydra.haskell.syntax.DataOrNewtype" (->hydra_core_field "newtype" (list :unit nil)))))

(def hydra_dsl_haskell_syntax_declaration_data (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Declaration" (->hydra_core_field "data" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_declaration_head_application (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.DeclarationHead" (->hydra_core_field "application" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_declaration_head_parens (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.DeclarationHead" (->hydra_core_field "parens" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_declaration_head_simple (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.DeclarationHead" (->hydra_core_field "simple" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_declaration_type (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Declaration" (->hydra_core_field "type" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_declaration_typed_binding (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Declaration" (->hydra_core_field "typedBinding" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_declaration_value_binding (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Declaration" (->hydra_core_field "valueBinding" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_declaration_with_comments (fn [body] (fn [comments] (list :record (->hydra_core_record "hydra.haskell.syntax.DeclarationWithComments" (list (->hydra_core_field "body" ((fn [v] v) body)) (->hydra_core_field "comments" ((fn [v] v) comments))))))))

(def hydra_dsl_haskell_syntax_declaration_with_comments_body (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DeclarationWithComments" "body")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_declaration_with_comments_comments (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DeclarationWithComments" "comments")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_declaration_with_comments_with_body (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.DeclarationWithComments" (list (->hydra_core_field "body" ((fn [v] v) new_val)) (->hydra_core_field "comments" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DeclarationWithComments" "comments")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_declaration_with_comments_with_comments (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.DeclarationWithComments" (list (->hydra_core_field "body" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.DeclarationWithComments" "body")) ((fn [v] v) original)))) (->hydra_core_field "comments" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_deriving (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.haskell.syntax.Deriving" ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_export_declaration (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Export" (->hydra_core_field "declaration" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_export_module (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Export" (->hydra_core_field "module" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_expression_application (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Expression" (->hydra_core_field "application" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_expression_case (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Expression" (->hydra_core_field "case" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_expression_construct_record (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Expression" (->hydra_core_field "constructRecord" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_expression_do (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Expression" (->hydra_core_field "do" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_expression_if (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Expression" (->hydra_core_field "if" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_expression_infix_application (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Expression" (->hydra_core_field "infixApplication" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_expression_lambda (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Expression" (->hydra_core_field "lambda" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_expression_left_section (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Expression" (->hydra_core_field "leftSection" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_expression_let (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Expression" (->hydra_core_field "let" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_expression_list (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Expression" (->hydra_core_field "list" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_expression_literal (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Expression" (->hydra_core_field "literal" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_expression_parens (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Expression" (->hydra_core_field "parens" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_expression_prefix_application (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Expression" (->hydra_core_field "prefixApplication" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_expression_right_section (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Expression" (->hydra_core_field "rightSection" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_expression_tuple (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Expression" (->hydra_core_field "tuple" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_expression_type_signature (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Expression" (->hydra_core_field "typeSignature" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_expression_update_record (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Expression" (->hydra_core_field "updateRecord" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_expression_variable (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Expression" (->hydra_core_field "variable" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_field (fn [name] (fn [type] (list :record (->hydra_core_record "hydra.haskell.syntax.Field" (list (->hydra_core_field "name" ((fn [v] v) name)) (->hydra_core_field "type" ((fn [v] v) type))))))))

(def hydra_dsl_haskell_syntax_field_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Field" "name")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_field_type (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Field" "type")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_field_update (fn [name] (fn [value] (list :record (->hydra_core_record "hydra.haskell.syntax.FieldUpdate" (list (->hydra_core_field "name" ((fn [v] v) name)) (->hydra_core_field "value" ((fn [v] v) value))))))))

(def hydra_dsl_haskell_syntax_field_update_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.FieldUpdate" "name")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_field_update_value (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.FieldUpdate" "value")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_field_update_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.FieldUpdate" (list (->hydra_core_field "name" ((fn [v] v) new_val)) (->hydra_core_field "value" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.FieldUpdate" "value")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_field_update_with_value (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.FieldUpdate" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.FieldUpdate" "name")) ((fn [v] v) original)))) (->hydra_core_field "value" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_field_with_comments (fn [field] (fn [comments] (list :record (->hydra_core_record "hydra.haskell.syntax.FieldWithComments" (list (->hydra_core_field "field" ((fn [v] v) field)) (->hydra_core_field "comments" ((fn [v] v) comments))))))))

(def hydra_dsl_haskell_syntax_field_with_comments_comments (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.FieldWithComments" "comments")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_field_with_comments_field (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.FieldWithComments" "field")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_field_with_comments_with_comments (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.FieldWithComments" (list (->hydra_core_field "field" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.FieldWithComments" "field")) ((fn [v] v) original)))) (->hydra_core_field "comments" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_field_with_comments_with_field (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.FieldWithComments" (list (->hydra_core_field "field" ((fn [v] v) new_val)) (->hydra_core_field "comments" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.FieldWithComments" "comments")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_field_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.Field" (list (->hydra_core_field "name" ((fn [v] v) new_val)) (->hydra_core_field "type" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Field" "type")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_field_with_type (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.Field" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Field" "name")) ((fn [v] v) original)))) (->hydra_core_field "type" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_function_type (fn [domain] (fn [codomain] (list :record (->hydra_core_record "hydra.haskell.syntax.FunctionType" (list (->hydra_core_field "domain" ((fn [v] v) domain)) (->hydra_core_field "codomain" ((fn [v] v) codomain))))))))

(def hydra_dsl_haskell_syntax_function_type_codomain (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.FunctionType" "codomain")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_function_type_domain (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.FunctionType" "domain")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_function_type_with_codomain (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.FunctionType" (list (->hydra_core_field "domain" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.FunctionType" "domain")) ((fn [v] v) original)))) (->hydra_core_field "codomain" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_function_type_with_domain (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.FunctionType" (list (->hydra_core_field "domain" ((fn [v] v) new_val)) (->hydra_core_field "codomain" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.FunctionType" "codomain")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_if_expression (fn [condition_] (fn [then] (fn [else_] (list :record (->hydra_core_record "hydra.haskell.syntax.IfExpression" (list (->hydra_core_field "condition" ((fn [v] v) condition_)) (->hydra_core_field "then" ((fn [v] v) then)) (->hydra_core_field "else" ((fn [v] v) else_)))))))))

(def hydra_dsl_haskell_syntax_if_expression_condition (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.IfExpression" "condition")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_if_expression_else (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.IfExpression" "else")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_if_expression_then (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.IfExpression" "then")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_if_expression_with_condition (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.IfExpression" (list (->hydra_core_field "condition" ((fn [v] v) new_val)) (->hydra_core_field "then" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.IfExpression" "then")) ((fn [v] v) original)))) (->hydra_core_field "else" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.IfExpression" "else")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_if_expression_with_else (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.IfExpression" (list (->hydra_core_field "condition" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.IfExpression" "condition")) ((fn [v] v) original)))) (->hydra_core_field "then" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.IfExpression" "then")) ((fn [v] v) original)))) (->hydra_core_field "else" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_if_expression_with_then (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.IfExpression" (list (->hydra_core_field "condition" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.IfExpression" "condition")) ((fn [v] v) original)))) (->hydra_core_field "then" ((fn [v] v) new_val)) (->hydra_core_field "else" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.IfExpression" "else")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_import (fn [qualified] (fn [module] (fn [as] (fn [spec] (list :record (->hydra_core_record "hydra.haskell.syntax.Import" (list (->hydra_core_field "qualified" ((fn [v] v) qualified)) (->hydra_core_field "module" ((fn [v] v) module)) (->hydra_core_field "as" ((fn [v] v) as)) (->hydra_core_field "spec" ((fn [v] v) spec))))))))))

(def hydra_dsl_haskell_syntax_import_as (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Import" "as")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_import_export_spec (fn [modifier] (fn [name] (fn [subspec] (list :record (->hydra_core_record "hydra.haskell.syntax.ImportExportSpec" (list (->hydra_core_field "modifier" ((fn [v] v) modifier)) (->hydra_core_field "name" ((fn [v] v) name)) (->hydra_core_field "subspec" ((fn [v] v) subspec)))))))))

(def hydra_dsl_haskell_syntax_import_export_spec_modifier (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ImportExportSpec" "modifier")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_import_export_spec_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ImportExportSpec" "name")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_import_export_spec_subspec (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ImportExportSpec" "subspec")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_import_export_spec_with_modifier (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.ImportExportSpec" (list (->hydra_core_field "modifier" ((fn [v] v) new_val)) (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ImportExportSpec" "name")) ((fn [v] v) original)))) (->hydra_core_field "subspec" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ImportExportSpec" "subspec")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_import_export_spec_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.ImportExportSpec" (list (->hydra_core_field "modifier" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ImportExportSpec" "modifier")) ((fn [v] v) original)))) (->hydra_core_field "name" ((fn [v] v) new_val)) (->hydra_core_field "subspec" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ImportExportSpec" "subspec")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_import_export_spec_with_subspec (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.ImportExportSpec" (list (->hydra_core_field "modifier" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ImportExportSpec" "modifier")) ((fn [v] v) original)))) (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ImportExportSpec" "name")) ((fn [v] v) original)))) (->hydra_core_field "subspec" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_import_modifier_pattern (list :inject (->hydra_core_injection "hydra.haskell.syntax.ImportModifier" (->hydra_core_field "pattern" (list :unit nil)))))

(def hydra_dsl_haskell_syntax_import_modifier_type (list :inject (->hydra_core_injection "hydra.haskell.syntax.ImportModifier" (->hydra_core_field "type" (list :unit nil)))))

(def hydra_dsl_haskell_syntax_import_module (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Import" "module")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_import_qualified (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Import" "qualified")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_import_spec (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Import" "spec")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_import_with_as (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.Import" (list (->hydra_core_field "qualified" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Import" "qualified")) ((fn [v] v) original)))) (->hydra_core_field "module" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Import" "module")) ((fn [v] v) original)))) (->hydra_core_field "as" ((fn [v] v) new_val)) (->hydra_core_field "spec" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Import" "spec")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_import_with_module (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.Import" (list (->hydra_core_field "qualified" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Import" "qualified")) ((fn [v] v) original)))) (->hydra_core_field "module" ((fn [v] v) new_val)) (->hydra_core_field "as" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Import" "as")) ((fn [v] v) original)))) (->hydra_core_field "spec" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Import" "spec")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_import_with_qualified (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.Import" (list (->hydra_core_field "qualified" ((fn [v] v) new_val)) (->hydra_core_field "module" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Import" "module")) ((fn [v] v) original)))) (->hydra_core_field "as" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Import" "as")) ((fn [v] v) original)))) (->hydra_core_field "spec" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Import" "spec")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_import_with_spec (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.Import" (list (->hydra_core_field "qualified" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Import" "qualified")) ((fn [v] v) original)))) (->hydra_core_field "module" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Import" "module")) ((fn [v] v) original)))) (->hydra_core_field "as" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Import" "as")) ((fn [v] v) original)))) (->hydra_core_field "spec" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_infix_application_expression (fn [lhs] (fn [operator] (fn [rhs] (list :record (->hydra_core_record "hydra.haskell.syntax.InfixApplicationExpression" (list (->hydra_core_field "lhs" ((fn [v] v) lhs)) (->hydra_core_field "operator" ((fn [v] v) operator)) (->hydra_core_field "rhs" ((fn [v] v) rhs)))))))))

(def hydra_dsl_haskell_syntax_infix_application_expression_lhs (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.InfixApplicationExpression" "lhs")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_infix_application_expression_operator (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.InfixApplicationExpression" "operator")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_infix_application_expression_rhs (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.InfixApplicationExpression" "rhs")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_infix_application_expression_with_lhs (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.InfixApplicationExpression" (list (->hydra_core_field "lhs" ((fn [v] v) new_val)) (->hydra_core_field "operator" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.InfixApplicationExpression" "operator")) ((fn [v] v) original)))) (->hydra_core_field "rhs" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.InfixApplicationExpression" "rhs")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_infix_application_expression_with_operator (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.InfixApplicationExpression" (list (->hydra_core_field "lhs" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.InfixApplicationExpression" "lhs")) ((fn [v] v) original)))) (->hydra_core_field "operator" ((fn [v] v) new_val)) (->hydra_core_field "rhs" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.InfixApplicationExpression" "rhs")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_infix_application_expression_with_rhs (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.InfixApplicationExpression" (list (->hydra_core_field "lhs" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.InfixApplicationExpression" "lhs")) ((fn [v] v) original)))) (->hydra_core_field "operator" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.InfixApplicationExpression" "operator")) ((fn [v] v) original)))) (->hydra_core_field "rhs" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_infix_type (fn [lhs] (fn [operator] (fn [rhs] (list :record (->hydra_core_record "hydra.haskell.syntax.InfixType" (list (->hydra_core_field "lhs" ((fn [v] v) lhs)) (->hydra_core_field "operator" ((fn [v] v) operator)) (->hydra_core_field "rhs" ((fn [v] v) rhs)))))))))

(def hydra_dsl_haskell_syntax_infix_type_lhs (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.InfixType" "lhs")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_infix_type_operator (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.InfixType" "operator")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_infix_type_rhs (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.InfixType" "rhs")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_infix_type_with_lhs (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.InfixType" (list (->hydra_core_field "lhs" ((fn [v] v) new_val)) (->hydra_core_field "operator" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.InfixType" "operator")) ((fn [v] v) original)))) (->hydra_core_field "rhs" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.InfixType" "rhs")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_infix_type_with_operator (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.InfixType" (list (->hydra_core_field "lhs" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.InfixType" "lhs")) ((fn [v] v) original)))) (->hydra_core_field "operator" ((fn [v] v) new_val)) (->hydra_core_field "rhs" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.InfixType" "rhs")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_infix_type_with_rhs (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.InfixType" (list (->hydra_core_field "lhs" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.InfixType" "lhs")) ((fn [v] v) original)))) (->hydra_core_field "operator" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.InfixType" "operator")) ((fn [v] v) original)))) (->hydra_core_field "rhs" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_lambda_expression (fn [bindings] (fn [inner] (list :record (->hydra_core_record "hydra.haskell.syntax.LambdaExpression" (list (->hydra_core_field "bindings" ((fn [v] v) bindings)) (->hydra_core_field "inner" ((fn [v] v) inner))))))))

(def hydra_dsl_haskell_syntax_lambda_expression_bindings (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.LambdaExpression" "bindings")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_lambda_expression_inner (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.LambdaExpression" "inner")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_lambda_expression_with_bindings (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.LambdaExpression" (list (->hydra_core_field "bindings" ((fn [v] v) new_val)) (->hydra_core_field "inner" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.LambdaExpression" "inner")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_lambda_expression_with_inner (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.LambdaExpression" (list (->hydra_core_field "bindings" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.LambdaExpression" "bindings")) ((fn [v] v) original)))) (->hydra_core_field "inner" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_let_expression (fn [bindings] (fn [inner] (list :record (->hydra_core_record "hydra.haskell.syntax.LetExpression" (list (->hydra_core_field "bindings" ((fn [v] v) bindings)) (->hydra_core_field "inner" ((fn [v] v) inner))))))))

(def hydra_dsl_haskell_syntax_let_expression_bindings (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.LetExpression" "bindings")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_let_expression_inner (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.LetExpression" "inner")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_let_expression_with_bindings (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.LetExpression" (list (->hydra_core_field "bindings" ((fn [v] v) new_val)) (->hydra_core_field "inner" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.LetExpression" "inner")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_let_expression_with_inner (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.LetExpression" (list (->hydra_core_field "bindings" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.LetExpression" "bindings")) ((fn [v] v) original)))) (->hydra_core_field "inner" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_literal_char (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Literal" (->hydra_core_field "char" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_literal_double (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Literal" (->hydra_core_field "double" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_literal_float (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Literal" (->hydra_core_field "float" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_literal_int (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Literal" (->hydra_core_field "int" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_literal_integer (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Literal" (->hydra_core_field "integer" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_literal_string (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Literal" (->hydra_core_field "string" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_local_binding_signature (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.LocalBinding" (->hydra_core_field "signature" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_local_binding_value (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.LocalBinding" (->hydra_core_field "value" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_local_bindings (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.haskell.syntax.LocalBindings" ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_module (fn [head] (fn [imports] (fn [declarations] (list :record (->hydra_core_record "hydra.haskell.syntax.Module" (list (->hydra_core_field "head" ((fn [v] v) head)) (->hydra_core_field "imports" ((fn [v] v) imports)) (->hydra_core_field "declarations" ((fn [v] v) declarations)))))))))

(def hydra_dsl_haskell_syntax_module_declarations (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Module" "declarations")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_module_head (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Module" "head")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_module_head_comments (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ModuleHead" "comments")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_module_head_exports (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ModuleHead" "exports")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_module_head_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ModuleHead" "name")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_module_head_with_comments (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.ModuleHead" (list (->hydra_core_field "comments" ((fn [v] v) new_val)) (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ModuleHead" "name")) ((fn [v] v) original)))) (->hydra_core_field "exports" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ModuleHead" "exports")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_module_head_with_exports (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.ModuleHead" (list (->hydra_core_field "comments" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ModuleHead" "comments")) ((fn [v] v) original)))) (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ModuleHead" "name")) ((fn [v] v) original)))) (->hydra_core_field "exports" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_module_head_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.ModuleHead" (list (->hydra_core_field "comments" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ModuleHead" "comments")) ((fn [v] v) original)))) (->hydra_core_field "name" ((fn [v] v) new_val)) (->hydra_core_field "exports" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.ModuleHead" "exports")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_module_head_ (fn [comments] (fn [name] (fn [exports] (list :record (->hydra_core_record "hydra.haskell.syntax.ModuleHead" (list (->hydra_core_field "comments" ((fn [v] v) comments)) (->hydra_core_field "name" ((fn [v] v) name)) (->hydra_core_field "exports" ((fn [v] v) exports)))))))))

(def hydra_dsl_haskell_syntax_module_imports (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Module" "imports")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_module_name (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.haskell.syntax.ModuleName" ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_module_with_declarations (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.Module" (list (->hydra_core_field "head" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Module" "head")) ((fn [v] v) original)))) (->hydra_core_field "imports" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Module" "imports")) ((fn [v] v) original)))) (->hydra_core_field "declarations" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_module_with_head (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.Module" (list (->hydra_core_field "head" ((fn [v] v) new_val)) (->hydra_core_field "imports" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Module" "imports")) ((fn [v] v) original)))) (->hydra_core_field "declarations" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Module" "declarations")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_module_with_imports (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.Module" (list (->hydra_core_field "head" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Module" "head")) ((fn [v] v) original)))) (->hydra_core_field "imports" ((fn [v] v) new_val)) (->hydra_core_field "declarations" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.Module" "declarations")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_name_implicit (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Name" (->hydra_core_field "implicit" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_name_normal (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Name" (->hydra_core_field "normal" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_name_parens (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Name" (->hydra_core_field "parens" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_name_part (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.haskell.syntax.NamePart" ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_operator_backtick (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Operator" (->hydra_core_field "backtick" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_operator_normal (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Operator" (->hydra_core_field "normal" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_ordinary_constructor (fn [name] (fn [fields] (list :record (->hydra_core_record "hydra.haskell.syntax.OrdinaryConstructor" (list (->hydra_core_field "name" ((fn [v] v) name)) (->hydra_core_field "fields" ((fn [v] v) fields))))))))

(def hydra_dsl_haskell_syntax_ordinary_constructor_fields (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.OrdinaryConstructor" "fields")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_ordinary_constructor_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.OrdinaryConstructor" "name")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_ordinary_constructor_with_fields (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.OrdinaryConstructor" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.OrdinaryConstructor" "name")) ((fn [v] v) original)))) (->hydra_core_field "fields" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_ordinary_constructor_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.OrdinaryConstructor" (list (->hydra_core_field "name" ((fn [v] v) new_val)) (->hydra_core_field "fields" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.OrdinaryConstructor" "fields")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_pattern_application (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Pattern" (->hydra_core_field "application" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_pattern_as (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Pattern" (->hydra_core_field "as" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_pattern_field (fn [name] (fn [pattern] (list :record (->hydra_core_record "hydra.haskell.syntax.PatternField" (list (->hydra_core_field "name" ((fn [v] v) name)) (->hydra_core_field "pattern" ((fn [v] v) pattern))))))))

(def hydra_dsl_haskell_syntax_pattern_field_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.PatternField" "name")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_pattern_field_pattern (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.PatternField" "pattern")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_pattern_field_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.PatternField" (list (->hydra_core_field "name" ((fn [v] v) new_val)) (->hydra_core_field "pattern" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.PatternField" "pattern")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_pattern_field_with_pattern (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.PatternField" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.PatternField" "name")) ((fn [v] v) original)))) (->hydra_core_field "pattern" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_pattern_list (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Pattern" (->hydra_core_field "list" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_pattern_literal (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Pattern" (->hydra_core_field "literal" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_pattern_name (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Pattern" (->hydra_core_field "name" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_pattern_parens (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Pattern" (->hydra_core_field "parens" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_pattern_record (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Pattern" (->hydra_core_field "record" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_pattern_tuple (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Pattern" (->hydra_core_field "tuple" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_pattern_typed (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Pattern" (->hydra_core_field "typed" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_pattern_wildcard (list :inject (->hydra_core_injection "hydra.haskell.syntax.Pattern" (->hydra_core_field "wildcard" (list :unit nil)))))

(def hydra_dsl_haskell_syntax_prefix_application_expression (fn [operator] (fn [rhs] (list :record (->hydra_core_record "hydra.haskell.syntax.PrefixApplicationExpression" (list (->hydra_core_field "operator" ((fn [v] v) operator)) (->hydra_core_field "rhs" ((fn [v] v) rhs))))))))

(def hydra_dsl_haskell_syntax_prefix_application_expression_operator (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.PrefixApplicationExpression" "operator")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_prefix_application_expression_rhs (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.PrefixApplicationExpression" "rhs")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_prefix_application_expression_with_operator (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.PrefixApplicationExpression" (list (->hydra_core_field "operator" ((fn [v] v) new_val)) (->hydra_core_field "rhs" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.PrefixApplicationExpression" "rhs")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_prefix_application_expression_with_rhs (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.PrefixApplicationExpression" (list (->hydra_core_field "operator" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.PrefixApplicationExpression" "operator")) ((fn [v] v) original)))) (->hydra_core_field "rhs" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_qualified_name (fn [qualifiers] (fn [unqualified] (list :record (->hydra_core_record "hydra.haskell.syntax.QualifiedName" (list (->hydra_core_field "qualifiers" ((fn [v] v) qualifiers)) (->hydra_core_field "unqualified" ((fn [v] v) unqualified))))))))

(def hydra_dsl_haskell_syntax_qualified_name_qualifiers (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.QualifiedName" "qualifiers")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_qualified_name_unqualified (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.QualifiedName" "unqualified")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_qualified_name_with_qualifiers (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.QualifiedName" (list (->hydra_core_field "qualifiers" ((fn [v] v) new_val)) (->hydra_core_field "unqualified" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.QualifiedName" "unqualified")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_qualified_name_with_unqualified (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.QualifiedName" (list (->hydra_core_field "qualifiers" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.QualifiedName" "qualifiers")) ((fn [v] v) original)))) (->hydra_core_field "unqualified" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_record_constructor (fn [name] (fn [fields] (list :record (->hydra_core_record "hydra.haskell.syntax.RecordConstructor" (list (->hydra_core_field "name" ((fn [v] v) name)) (->hydra_core_field "fields" ((fn [v] v) fields))))))))

(def hydra_dsl_haskell_syntax_record_constructor_fields (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.RecordConstructor" "fields")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_record_constructor_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.RecordConstructor" "name")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_record_constructor_with_fields (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.RecordConstructor" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.RecordConstructor" "name")) ((fn [v] v) original)))) (->hydra_core_field "fields" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_record_constructor_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.RecordConstructor" (list (->hydra_core_field "name" ((fn [v] v) new_val)) (->hydra_core_field "fields" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.RecordConstructor" "fields")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_record_pattern (fn [name] (fn [fields] (list :record (->hydra_core_record "hydra.haskell.syntax.RecordPattern" (list (->hydra_core_field "name" ((fn [v] v) name)) (->hydra_core_field "fields" ((fn [v] v) fields))))))))

(def hydra_dsl_haskell_syntax_record_pattern_fields (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.RecordPattern" "fields")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_record_pattern_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.RecordPattern" "name")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_record_pattern_with_fields (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.RecordPattern" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.RecordPattern" "name")) ((fn [v] v) original)))) (->hydra_core_field "fields" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_record_pattern_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.RecordPattern" (list (->hydra_core_field "name" ((fn [v] v) new_val)) (->hydra_core_field "fields" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.RecordPattern" "fields")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_right_hand_side (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.haskell.syntax.RightHandSide" ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_section_expression (fn [operator] (fn [expression] (list :record (->hydra_core_record "hydra.haskell.syntax.SectionExpression" (list (->hydra_core_field "operator" ((fn [v] v) operator)) (->hydra_core_field "expression" ((fn [v] v) expression))))))))

(def hydra_dsl_haskell_syntax_section_expression_expression (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.SectionExpression" "expression")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_section_expression_operator (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.SectionExpression" "operator")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_section_expression_with_expression (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.SectionExpression" (list (->hydra_core_field "operator" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.SectionExpression" "operator")) ((fn [v] v) original)))) (->hydra_core_field "expression" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_section_expression_with_operator (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.SectionExpression" (list (->hydra_core_field "operator" ((fn [v] v) new_val)) (->hydra_core_field "expression" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.SectionExpression" "expression")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_simple_value_binding (fn [pattern] (fn [rhs] (fn [local_bindings] (list :record (->hydra_core_record "hydra.haskell.syntax.SimpleValueBinding" (list (->hydra_core_field "pattern" ((fn [v] v) pattern)) (->hydra_core_field "rhs" ((fn [v] v) rhs)) (->hydra_core_field "localBindings" ((fn [v] v) local_bindings)))))))))

(def hydra_dsl_haskell_syntax_simple_value_binding_local_bindings (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.SimpleValueBinding" "localBindings")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_simple_value_binding_pattern (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.SimpleValueBinding" "pattern")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_simple_value_binding_rhs (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.SimpleValueBinding" "rhs")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_simple_value_binding_with_local_bindings (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.SimpleValueBinding" (list (->hydra_core_field "pattern" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.SimpleValueBinding" "pattern")) ((fn [v] v) original)))) (->hydra_core_field "rhs" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.SimpleValueBinding" "rhs")) ((fn [v] v) original)))) (->hydra_core_field "localBindings" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_simple_value_binding_with_pattern (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.SimpleValueBinding" (list (->hydra_core_field "pattern" ((fn [v] v) new_val)) (->hydra_core_field "rhs" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.SimpleValueBinding" "rhs")) ((fn [v] v) original)))) (->hydra_core_field "localBindings" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.SimpleValueBinding" "localBindings")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_simple_value_binding_with_rhs (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.SimpleValueBinding" (list (->hydra_core_field "pattern" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.SimpleValueBinding" "pattern")) ((fn [v] v) original)))) (->hydra_core_field "rhs" ((fn [v] v) new_val)) (->hydra_core_field "localBindings" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.SimpleValueBinding" "localBindings")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_spec_import_hiding (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.SpecImport" (->hydra_core_field "hiding" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_spec_import_list (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.SpecImport" (->hydra_core_field "list" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_statement (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.haskell.syntax.Statement" ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_subspec_import_export_spec_all (list :inject (->hydra_core_injection "hydra.haskell.syntax.SubspecImportExportSpec" (->hydra_core_field "all" (list :unit nil)))))

(def hydra_dsl_haskell_syntax_subspec_import_export_spec_list (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.SubspecImportExportSpec" (->hydra_core_field "list" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_type_application (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Type" (->hydra_core_field "application" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_type_ctx (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Type" (->hydra_core_field "ctx" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_type_declaration (fn [name] (fn [type] (list :record (->hydra_core_record "hydra.haskell.syntax.TypeDeclaration" (list (->hydra_core_field "name" ((fn [v] v) name)) (->hydra_core_field "type" ((fn [v] v) type))))))))

(def hydra_dsl_haskell_syntax_type_declaration_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.TypeDeclaration" "name")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_type_declaration_type (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.TypeDeclaration" "type")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_type_declaration_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.TypeDeclaration" (list (->hydra_core_field "name" ((fn [v] v) new_val)) (->hydra_core_field "type" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.TypeDeclaration" "type")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_type_declaration_with_type (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.TypeDeclaration" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.TypeDeclaration" "name")) ((fn [v] v) original)))) (->hydra_core_field "type" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_type_function (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Type" (->hydra_core_field "function" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_type_infix (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Type" (->hydra_core_field "infix" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_type_list (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Type" (->hydra_core_field "list" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_type_parens (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Type" (->hydra_core_field "parens" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_type_signature (fn [name] (fn [type] (list :record (->hydra_core_record "hydra.haskell.syntax.TypeSignature" (list (->hydra_core_field "name" ((fn [v] v) name)) (->hydra_core_field "type" ((fn [v] v) type))))))))

(def hydra_dsl_haskell_syntax_type_signature_expression (fn [inner] (fn [type] (list :record (->hydra_core_record "hydra.haskell.syntax.TypeSignatureExpression" (list (->hydra_core_field "inner" ((fn [v] v) inner)) (->hydra_core_field "type" ((fn [v] v) type))))))))

(def hydra_dsl_haskell_syntax_type_signature_expression_inner (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.TypeSignatureExpression" "inner")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_type_signature_expression_type (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.TypeSignatureExpression" "type")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_type_signature_expression_with_inner (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.TypeSignatureExpression" (list (->hydra_core_field "inner" ((fn [v] v) new_val)) (->hydra_core_field "type" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.TypeSignatureExpression" "type")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_type_signature_expression_with_type (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.TypeSignatureExpression" (list (->hydra_core_field "inner" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.TypeSignatureExpression" "inner")) ((fn [v] v) original)))) (->hydra_core_field "type" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_type_signature_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.TypeSignature" "name")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_type_signature_type (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.TypeSignature" "type")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_type_signature_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.TypeSignature" (list (->hydra_core_field "name" ((fn [v] v) new_val)) (->hydra_core_field "type" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.TypeSignature" "type")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_type_signature_with_type (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.TypeSignature" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.TypeSignature" "name")) ((fn [v] v) original)))) (->hydra_core_field "type" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_type_tuple (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Type" (->hydra_core_field "tuple" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_type_variable (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.Type" (->hydra_core_field "variable" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_typed_binding (fn [type_signature] (fn [value_binding] (list :record (->hydra_core_record "hydra.haskell.syntax.TypedBinding" (list (->hydra_core_field "typeSignature" ((fn [v] v) type_signature)) (->hydra_core_field "valueBinding" ((fn [v] v) value_binding))))))))

(def hydra_dsl_haskell_syntax_typed_binding_type_signature (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.TypedBinding" "typeSignature")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_typed_binding_value_binding (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.TypedBinding" "valueBinding")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_typed_binding_with_type_signature (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.TypedBinding" (list (->hydra_core_field "typeSignature" ((fn [v] v) new_val)) (->hydra_core_field "valueBinding" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.TypedBinding" "valueBinding")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_typed_binding_with_value_binding (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.TypedBinding" (list (->hydra_core_field "typeSignature" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.TypedBinding" "typeSignature")) ((fn [v] v) original)))) (->hydra_core_field "valueBinding" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_typed_pattern (fn [inner] (fn [type] (list :record (->hydra_core_record "hydra.haskell.syntax.TypedPattern" (list (->hydra_core_field "inner" ((fn [v] v) inner)) (->hydra_core_field "type" ((fn [v] v) type))))))))

(def hydra_dsl_haskell_syntax_typed_pattern_inner (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.TypedPattern" "inner")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_typed_pattern_type (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.TypedPattern" "type")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_typed_pattern_with_inner (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.TypedPattern" (list (->hydra_core_field "inner" ((fn [v] v) new_val)) (->hydra_core_field "type" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.TypedPattern" "type")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_typed_pattern_with_type (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.TypedPattern" (list (->hydra_core_field "inner" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.TypedPattern" "inner")) ((fn [v] v) original)))) (->hydra_core_field "type" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_un_case_rhs (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.haskell.syntax.CaseRhs") ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_un_deriving (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.haskell.syntax.Deriving") ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_un_local_bindings (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.haskell.syntax.LocalBindings") ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_un_module_name (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.haskell.syntax.ModuleName") ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_un_name_part (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.haskell.syntax.NamePart") ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_un_right_hand_side (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.haskell.syntax.RightHandSide") ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_un_statement (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.haskell.syntax.Statement") ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_un_variable (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.haskell.syntax.Variable") ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_update_record_expression (fn [inner] (fn [fields] (list :record (->hydra_core_record "hydra.haskell.syntax.UpdateRecordExpression" (list (->hydra_core_field "inner" ((fn [v] v) inner)) (->hydra_core_field "fields" ((fn [v] v) fields))))))))

(def hydra_dsl_haskell_syntax_update_record_expression_fields (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.UpdateRecordExpression" "fields")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_update_record_expression_inner (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.UpdateRecordExpression" "inner")) ((fn [v] v) x)))))

(def hydra_dsl_haskell_syntax_update_record_expression_with_fields (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.UpdateRecordExpression" (list (->hydra_core_field "inner" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.UpdateRecordExpression" "inner")) ((fn [v] v) original)))) (->hydra_core_field "fields" ((fn [v] v) new_val))))))))

(def hydra_dsl_haskell_syntax_update_record_expression_with_inner (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.haskell.syntax.UpdateRecordExpression" (list (->hydra_core_field "inner" ((fn [v] v) new_val)) (->hydra_core_field "fields" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.haskell.syntax.UpdateRecordExpression" "fields")) ((fn [v] v) original))))))))))

(def hydra_dsl_haskell_syntax_value_binding_simple (fn [x] (list :inject (->hydra_core_injection "hydra.haskell.syntax.ValueBinding" (->hydra_core_field "simple" ((fn [v] v) x))))))

(def hydra_dsl_haskell_syntax_variable (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.haskell.syntax.Variable" ((fn [v] v) x)))))
