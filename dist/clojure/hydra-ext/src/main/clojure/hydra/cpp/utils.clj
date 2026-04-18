(ns hydra.cpp.utils
  (:require [hydra.cpp.syntax :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.logic :refer :all]
))

(declare hydra_cpp_utils_const_parameter hydra_cpp_utils_cpp_class_declaration hydra_cpp_utils_cpp_enum_declaration hydra_cpp_utils_cpp_enum_forward_declaration hydra_cpp_utils_cpp_unary_expression_to_cpp_logical_or_expression hydra_cpp_utils_cpp_unary_expression_to_cpp_expression hydra_cpp_utils_cpp_postfix_expression_to_cpp_expression hydra_cpp_utils_cpp_primary_expression_to_cpp_expression hydra_cpp_utils_create_cast_expr hydra_cpp_utils_create_compound_stmt hydra_cpp_utils_create_qualified_type hydra_cpp_utils_create_const_type hydra_cpp_utils_create_reference_type hydra_cpp_utils_create_const_ref_type hydra_cpp_utils_empty_function_body hydra_cpp_utils_create_constructor_body hydra_cpp_utils_create_enum_access_expr hydra_cpp_utils_create_function_call_expr hydra_cpp_utils_create_header_file hydra_cpp_utils_create_identifier_expr hydra_cpp_utils_create_lambda_expr hydra_cpp_utils_create_literal_bool_expr hydra_cpp_utils_create_literal_int_expr hydra_cpp_utils_create_literal_string_expr hydra_cpp_utils_extract_postfix_expression hydra_cpp_utils_create_member_access_expr hydra_cpp_utils_create_return_stmt hydra_cpp_utils_create_return_void_stmt hydra_cpp_utils_create_template_type hydra_cpp_utils_create_this_expr hydra_cpp_utils_create_throw_stmt hydra_cpp_utils_create_type_id_name_call hydra_cpp_utils_create_type_name_expr hydra_cpp_utils_create_variant_expr hydra_cpp_utils_member_specification_protected hydra_cpp_utils_member_specification_public hydra_cpp_utils_string_expression hydra_cpp_utils_to_const_type hydra_cpp_utils_unnamed_parameter)

(def hydra_cpp_utils_const_parameter (fn [name] (fn [typ] (->hydra_cpp_syntax_parameter (list :qualified (->hydra_cpp_syntax_qualified_type (list :qualified (->hydra_cpp_syntax_qualified_type typ (list :const nil))) (list :lvalue_ref nil))) name false (list :nothing)))))

(def hydra_cpp_utils_cpp_class_declaration (fn [name] (fn [base_specs] (fn [mbody] (list :class (->hydra_cpp_syntax_class_declaration (->hydra_cpp_syntax_class_specifier (list :class nil) name base_specs) mbody))))))

(def hydra_cpp_utils_cpp_enum_declaration (fn [name] (fn [mbody] (list :class (->hydra_cpp_syntax_class_declaration (->hydra_cpp_syntax_class_specifier (list :enum_class nil) name (list)) mbody)))))

(def hydra_cpp_utils_cpp_enum_forward_declaration (fn [name] (list :class (->hydra_cpp_syntax_class_declaration (->hydra_cpp_syntax_class_specifier (list :enum_class nil) name (list)) (list :nothing)))))

(def hydra_cpp_utils_cpp_unary_expression_to_cpp_logical_or_expression (fn [ue] (list :logical_and (list :inclusive_or (list :exclusive_or (list :and (list :equality (list :relational (list :shift (list :additive (list :multiplicative (list :unary ue))))))))))))

(def hydra_cpp_utils_cpp_unary_expression_to_cpp_expression (fn [ue] (list :assignment (list :conditional (list :logical_or (hydra_cpp_utils_cpp_unary_expression_to_cpp_logical_or_expression ue))))))

(def hydra_cpp_utils_cpp_postfix_expression_to_cpp_expression (fn [pf] (hydra_cpp_utils_cpp_unary_expression_to_cpp_expression (list :postfix pf))))

(def hydra_cpp_utils_cpp_primary_expression_to_cpp_expression (fn [prim] (hydra_cpp_utils_cpp_postfix_expression_to_cpp_expression (list :primary prim))))

(def hydra_cpp_utils_create_cast_expr (fn [target_type] (fn [expr] (hydra_cpp_utils_cpp_primary_expression_to_cpp_expression (list :parenthesized (hydra_cpp_utils_cpp_primary_expression_to_cpp_expression (list :parenthesized expr)))))))

(def hydra_cpp_utils_create_compound_stmt (fn [stmts] stmts))

(def hydra_cpp_utils_create_qualified_type (fn [base_type] (fn [qualifier] (list :qualified (->hydra_cpp_syntax_qualified_type base_type qualifier)))))

(def hydra_cpp_utils_create_const_type (fn [base_type] ((hydra_cpp_utils_create_qualified_type base_type) (list :const nil))))

(def hydra_cpp_utils_create_reference_type (fn [base_type] ((hydra_cpp_utils_create_qualified_type base_type) (list :lvalue_ref nil))))

(def hydra_cpp_utils_create_const_ref_type (fn [base_type] (hydra_cpp_utils_create_reference_type (hydra_cpp_utils_create_const_type base_type))))

(def hydra_cpp_utils_empty_function_body (list :compound (list)))

(def hydra_cpp_utils_create_constructor_body (fn [params] (if (hydra_lib_lists_null params) (list :default nil) hydra_cpp_utils_empty_function_body)))

(def hydra_cpp_utils_create_enum_access_expr (fn [enum_name] (fn [value_name] (hydra_cpp_utils_cpp_postfix_expression_to_cpp_expression (list :member_access (->hydra_cpp_syntax_member_access_operation (list :primary (list :identifier enum_name)) value_name))))))

(def hydra_cpp_utils_create_function_call_expr (fn [func_name] (fn [args] (hydra_cpp_utils_cpp_postfix_expression_to_cpp_expression (list :function_call (->hydra_cpp_syntax_function_call_operation (list :primary (list :identifier func_name)) args))))))

(def hydra_cpp_utils_create_header_file (fn [includes] (fn [decls] (->hydra_cpp_syntax_program (list (list :pragma (->hydra_cpp_syntax_pragma_directive "once"))) includes decls))))

(def hydra_cpp_utils_create_identifier_expr (fn [name] (hydra_cpp_utils_cpp_primary_expression_to_cpp_expression (list :identifier name))))

(def hydra_cpp_utils_create_lambda_expr (fn [params] (fn [body] (hydra_cpp_utils_cpp_postfix_expression_to_cpp_expression (list :primary (list :lambda (->hydra_cpp_syntax_lambda_expression (list :captures (list)) params (list :nothing) (list (list :expression body)))))))))

(def hydra_cpp_utils_create_literal_bool_expr (fn [val] (hydra_cpp_utils_cpp_primary_expression_to_cpp_expression (list :literal (list :boolean val)))))

(def hydra_cpp_utils_create_literal_int_expr (fn [val] (hydra_cpp_utils_cpp_primary_expression_to_cpp_expression (list :literal (list :integer (list :decimal val))))))

(def hydra_cpp_utils_create_literal_string_expr (fn [val] (hydra_cpp_utils_cpp_primary_expression_to_cpp_expression (list :literal (list :string val)))))

(def hydra_cpp_utils_extract_postfix_expression (fn [expr] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :assignment) ((fn [a] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :conditional) ((fn [c] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :logical_or) ((fn [lo] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :logical_and) ((fn [la] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :inclusive_or) ((fn [io] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :exclusive_or) ((fn [xo] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :and) ((fn [ae] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :equality) ((fn [eq] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :relational) ((fn [re] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :shift) ((fn [sh] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :additive) ((fn [ad] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :multiplicative) ((fn [mu] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :unary) ((fn [ue] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :postfix) ((fn [pf] pf) match_value) :else (list :primary (list :identifier "error")))) (second match_target))) ue)) match_value) :else (list :primary (list :identifier "error")))) (second match_target))) mu)) match_value) :else (list :primary (list :identifier "error")))) (second match_target))) ad)) match_value) :else (list :primary (list :identifier "error")))) (second match_target))) sh)) match_value) :else (list :primary (list :identifier "error")))) (second match_target))) re)) match_value) :else (list :primary (list :identifier "error")))) (second match_target))) eq)) match_value) :else (list :primary (list :identifier "error")))) (second match_target))) ae)) match_value) :else (list :primary (list :identifier "error")))) (second match_target))) xo)) match_value) :else (list :primary (list :identifier "error")))) (second match_target))) io)) match_value) :else (list :primary (list :identifier "error")))) (second match_target))) la)) match_value) :else (list :primary (list :identifier "error")))) (second match_target))) lo)) match_value) :else (list :primary (list :identifier "error")))) (second match_target))) c)) match_value) :else (list :primary (list :identifier "error")))) (second match_target))) a)) match_value) :else (list :primary (list :identifier "error")))) (second match_target))) expr)))

(def hydra_cpp_utils_create_member_access_expr (fn [obj_expr] (fn [member] (hydra_cpp_utils_cpp_postfix_expression_to_cpp_expression (list :member_access (->hydra_cpp_syntax_member_access_operation (hydra_cpp_utils_extract_postfix_expression obj_expr) member))))))

(def hydra_cpp_utils_create_return_stmt (fn [expr] (list :jump (list :return_value expr))))

(def hydra_cpp_utils_create_return_void_stmt (list :jump (list :return_void nil)))

(def hydra_cpp_utils_create_template_type (fn [name] (fn [args] (list :template (->hydra_cpp_syntax_template_type name ((hydra_lib_lists_map (fn [a] (list :type a))) args))))))

(def hydra_cpp_utils_create_this_expr (hydra_cpp_utils_cpp_postfix_expression_to_cpp_expression (list :primary (list :identifier "*this"))))

(def hydra_cpp_utils_create_throw_stmt (fn [exception_type] (fn [arg] (list :jump (list :throw ((hydra_cpp_utils_create_function_call_expr exception_type) (list arg)))))))

(def hydra_cpp_utils_create_type_id_name_call (hydra_cpp_utils_cpp_postfix_expression_to_cpp_expression (list :function_call (->hydra_cpp_syntax_function_call_operation (list :member_access (->hydra_cpp_syntax_member_access_operation (list :function_call (->hydra_cpp_syntax_function_call_operation (list :primary (list :identifier "typeid")) (list (hydra_cpp_utils_cpp_primary_expression_to_cpp_expression (list :parenthesized hydra_cpp_utils_create_this_expr))))) "name")) (list)))))

(def hydra_cpp_utils_create_type_name_expr (fn [type_name] (hydra_cpp_utils_cpp_postfix_expression_to_cpp_expression (list :primary (list :identifier type_name)))))

(def hydra_cpp_utils_create_variant_expr (hydra_cpp_utils_cpp_postfix_expression_to_cpp_expression (list :primary (list :identifier "variant"))))

(def hydra_cpp_utils_member_specification_protected (list :access_label (list :protected nil)))

(def hydra_cpp_utils_member_specification_public (list :access_label (list :public nil)))

(def hydra_cpp_utils_string_expression (fn [s] (hydra_cpp_utils_cpp_primary_expression_to_cpp_expression (list :literal (list :string s)))))

(def hydra_cpp_utils_to_const_type (fn [base_type] (list :qualified (->hydra_cpp_syntax_qualified_type base_type (list :const nil)))))

(def hydra_cpp_utils_unnamed_parameter (fn [name] (fn [typ] (->hydra_cpp_syntax_parameter typ name true (list :nothing)))))
