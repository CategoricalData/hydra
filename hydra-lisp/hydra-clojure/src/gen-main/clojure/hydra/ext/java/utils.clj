(ns hydra.ext.java.utils
  (:require [hydra.core :refer :all] [hydra.errors :refer :all] [hydra.ext.java.environment :refer :all] [hydra.ext.java.language :refer :all] [hydra.ext.java.names :refer :all] [hydra.ext.java.syntax :refer :all] [hydra.formatting :refer :all] [hydra.lib.eithers :refer :all] [hydra.lib.equality :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.literals :refer :all] [hydra.lib.logic :refer :all] [hydra.lib.maps :refer :all] [hydra.lib.math :refer :all] [hydra.lib.maybes :refer :all] [hydra.lib.pairs :refer :all] [hydra.lib.sets :refer :all] [hydra.lib.strings :refer :all] [hydra.names :refer :all] [hydra.packaging :refer :all]
))

(declare hydra_ext_java_utils_add_expressions hydra_ext_java_utils_add_in_scope_var hydra_ext_java_utils_add_in_scope_vars hydra_ext_java_utils_java_type_variable_to_type hydra_ext_java_utils_add_java_type_parameter hydra_ext_java_utils_add_var_rename hydra_ext_java_utils_field_expression hydra_ext_java_utils_is_escaped hydra_ext_java_utils_unescape hydra_ext_java_utils_sanitize_java_name hydra_ext_java_utils_java_identifier hydra_ext_java_utils_field_name_to_java_identifier hydra_ext_java_utils_java_identifier_to_java_expression_name hydra_ext_java_utils_field_name_to_java_expression hydra_ext_java_utils_java_variable_declarator_id hydra_ext_java_utils_java_variable_declarator hydra_ext_java_utils_field_name_to_java_variable_declarator hydra_ext_java_utils_field_name_to_java_variable_declarator_id hydra_ext_java_utils_final_var_declaration_statement hydra_ext_java_utils_import_aliases_for_module hydra_ext_java_utils_java_method_body hydra_ext_java_utils_java_method_header hydra_ext_java_utils_interface_method_declaration hydra_ext_java_utils_java_additive_expression_to_java_expression hydra_ext_java_utils_java_primary_to_java_expression hydra_ext_java_utils_java_array_creation hydra_ext_java_utils_java_array_initializer hydra_ext_java_utils_java_assignment_statement hydra_ext_java_utils_java_boolean hydra_ext_java_utils_java_literal_to_java_primary hydra_ext_java_utils_java_boolean_expression hydra_ext_java_utils_java_primitive_type_to_java_type hydra_ext_java_utils_java_boolean_type hydra_ext_java_utils_java_byte_primitive_type hydra_ext_java_utils_java_cast_expression hydra_ext_java_utils_java_cast_expression_to_java_expression hydra_ext_java_utils_java_cast_primitive hydra_ext_java_utils_java_variable_name hydra_ext_java_utils_java_decl_name hydra_ext_java_utils_java_type_identifier hydra_ext_java_utils_name_to_qualified_java_name hydra_ext_java_utils_name_to_java_class_type hydra_ext_java_utils_java_class_declaration hydra_ext_java_utils_java_class_type hydra_ext_java_utils_java_class_type_to_java_type hydra_ext_java_utils_java_conditional_and_expression_to_java_expression hydra_ext_java_utils_java_constructor_call hydra_ext_java_utils_java_constructor_name hydra_ext_java_utils_java_expression_to_java_unary_expression hydra_ext_java_utils_java_double_cast_expression hydra_ext_java_utils_java_double_cast_expression_to_java_expression hydra_ext_java_utils_java_empty_statement hydra_ext_java_utils_java_equality_expression_to_java_expression hydra_ext_java_utils_java_equality_expression_to_java_inclusive_or_expression hydra_ext_java_utils_java_equals hydra_ext_java_utils_java_literal_to_java_relational_expression hydra_ext_java_utils_java_equals_null hydra_ext_java_utils_java_expression_name_to_java_expression hydra_ext_java_utils_java_expression_to_java_primary hydra_ext_java_utils_java_field_access_to_java_expression hydra_ext_java_utils_java_identifier_to_java_expression hydra_ext_java_utils_java_identifier_to_java_relational_expression hydra_ext_java_utils_java_identifier_to_java_unary_expression hydra_ext_java_utils_java_instance_of hydra_ext_java_utils_java_int hydra_ext_java_utils_java_int_expression hydra_ext_java_utils_java_int_type hydra_ext_java_utils_java_interface_declaration_to_java_class_body_declaration hydra_ext_java_utils_variable_to_java_identifier hydra_ext_java_utils_java_lambda hydra_ext_java_utils_java_lambda_from_block hydra_ext_java_utils_java_literal_to_java_expression hydra_ext_java_utils_java_literal_to_java_multiplicative_expression hydra_ext_java_utils_java_member_field hydra_ext_java_utils_java_method_declaration_to_java_class_body_declaration hydra_ext_java_utils_java_method_invocation_to_java_expression hydra_ext_java_utils_java_method_invocation_to_java_postfix_expression hydra_ext_java_utils_java_method_invocation_to_java_primary hydra_ext_java_utils_java_method_invocation_to_java_statement hydra_ext_java_utils_java_multiplicative_expression_to_java_relational_expression hydra_ext_java_utils_java_package_declaration hydra_ext_java_utils_java_postfix_expression_to_java_equality_expression hydra_ext_java_utils_java_postfix_expression_to_java_expression hydra_ext_java_utils_java_postfix_expression_to_java_inclusive_or_expression hydra_ext_java_utils_java_postfix_expression_to_java_relational_expression hydra_ext_java_utils_java_postfix_expression_to_java_unary_expression hydra_ext_java_utils_java_primary_to_java_unary_expression hydra_ext_java_utils_java_ref_type hydra_ext_java_utils_java_reference_type_to_raw_type hydra_ext_java_utils_java_relational_expression_to_java_equality_expression hydra_ext_java_utils_java_relational_expression_to_java_expression hydra_ext_java_utils_java_relational_expression_to_java_unary_expression hydra_ext_java_utils_java_return_statement hydra_ext_java_utils_java_statements_to_block hydra_ext_java_utils_java_string hydra_ext_java_utils_java_string_multiplicative_expression hydra_ext_java_utils_java_this hydra_ext_java_utils_java_throw_statement hydra_ext_java_utils_java_throw_illegal_argument_exception hydra_ext_java_utils_java_throw_illegal_state_exception hydra_ext_java_utils_name_to_java_type_identifier hydra_ext_java_utils_java_type_from_type_name hydra_ext_java_utils_java_type_identifier_to_java_type_argument hydra_ext_java_utils_java_type_name hydra_ext_java_utils_java_type_parameter hydra_ext_java_utils_java_type_to_java_formal_parameter hydra_ext_java_utils_java_type_to_java_reference_type hydra_ext_java_utils_java_type_to_java_result hydra_ext_java_utils_java_type_to_java_type_argument hydra_ext_java_utils_java_type_variable hydra_ext_java_utils_java_unary_expression_to_java_expression hydra_ext_java_utils_java_unary_expression_to_java_relational_expression hydra_ext_java_utils_lookup_java_var_name hydra_ext_java_utils_make_constructor hydra_ext_java_utils_method_declaration hydra_ext_java_utils_method_invocation hydra_ext_java_utils_method_invocation_static hydra_ext_java_utils_method_invocation_static_with_type_args hydra_ext_java_utils_name_to_java_name hydra_ext_java_utils_name_to_java_reference_type hydra_ext_java_utils_override_annotation hydra_ext_java_utils_reference_type_to_result hydra_ext_java_utils_suppress_warnings_unchecked_annotation hydra_ext_java_utils_type_parameter_to_reference_type hydra_ext_java_utils_visitor_type_variable hydra_ext_java_utils_to_accept_method hydra_ext_java_utils_to_assign_stmt hydra_ext_java_utils_to_java_array_type hydra_ext_java_utils_type_parameter_to_type_argument hydra_ext_java_utils_un_type_parameter hydra_ext_java_utils_unique_var_name_go hydra_ext_java_utils_unique_var_name hydra_ext_java_utils_var_declaration_statement hydra_ext_java_utils_variable_declaration_statement hydra_ext_java_utils_variant_class_name)

(def hydra_ext_java_utils_add_expressions (fn [exprs] (let [first (list :unary (hydra_lib_lists_head exprs)) rest (hydra_lib_lists_tail exprs)] (((hydra_lib_lists_foldl (fn [ae] (fn [me] (list :plus (->hydra_ext_java_syntax_additive_expression_binary ae me))))) first) rest))))

(def hydra_ext_java_utils_add_in_scope_var (fn [name] (fn [aliases] (->hydra_ext_java_environment_aliases ((fn [v] (:current_namespace v)) aliases) ((fn [v] (:packages v)) aliases) ((fn [v] (:branch_vars v)) aliases) ((fn [v] (:recursive_vars v)) aliases) ((fn [v] (:in_scope_type_params v)) aliases) ((fn [v] (:polymorphic_locals v)) aliases) ((hydra_lib_sets_insert name) ((fn [v] (:in_scope_java_vars v)) aliases)) ((fn [v] (:var_renames v)) aliases) ((fn [v] (:lambda_vars v)) aliases) ((fn [v] (:type_var_subst v)) aliases) ((fn [v] (:trusted_type_vars v)) aliases) ((fn [v] (:method_codomain v)) aliases) ((fn [v] (:thunked_vars v)) aliases)))))

(def hydra_ext_java_utils_add_in_scope_vars (fn [names] (fn [aliases] (((hydra_lib_lists_foldl (fn [a] (fn [n] ((hydra_ext_java_utils_add_in_scope_var n) a)))) aliases) names))))

(def hydra_ext_java_utils_java_type_variable_to_type (fn [tv] (list :reference (list :variable tv))))

(def hydra_ext_java_utils_add_java_type_parameter (fn [rt] (fn [t_] (fn [cx] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :reference) ((fn [rt1] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :class_or_interface) ((fn [cit] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :class) ((fn [ct] (let [anns ((fn [v] (:annotations v)) ct) args ((fn [v] (:arguments v)) ct) id ((fn [v] (:identifier v)) ct) qual ((fn [v] (:qualifier v)) ct)] (list :right (list :reference (list :class_or_interface (list :class (->hydra_ext_java_syntax_class_type anns qual id ((hydra_lib_lists_concat2 args) (list (list :reference rt)))))))))) match_value) (= (first match_target) :interface) ((fn [_] (list :left (list :other "expected a Java class type"))) match_value))) (second match_target))) cit)) match_value) (= (first match_target) :variable) ((fn [tv] (list :right (hydra_ext_java_utils_java_type_variable_to_type tv))) match_value) (= (first match_target) :array) ((fn [_] (list :left (list :other "expected a Java class or interface type, or a variable"))) match_value))) (second match_target))) rt1)) match_value) (= (first match_target) :primitive) ((fn [_] (list :left (list :other "expected a reference type"))) match_value))) (second match_target))) t_)))))

(def hydra_ext_java_utils_add_var_rename (fn [original] (fn [renamed] (fn [aliases] (->hydra_ext_java_environment_aliases ((fn [v] (:current_namespace v)) aliases) ((fn [v] (:packages v)) aliases) ((fn [v] (:branch_vars v)) aliases) ((fn [v] (:recursive_vars v)) aliases) ((fn [v] (:in_scope_type_params v)) aliases) ((fn [v] (:polymorphic_locals v)) aliases) ((fn [v] (:in_scope_java_vars v)) aliases) (((hydra_lib_maps_insert original) renamed) ((fn [v] (:var_renames v)) aliases)) ((fn [v] (:lambda_vars v)) aliases) ((fn [v] (:type_var_subst v)) aliases) ((fn [v] (:trusted_type_vars v)) aliases) ((fn [v] (:method_codomain v)) aliases) ((fn [v] (:thunked_vars v)) aliases))))))

(def hydra_ext_java_utils_field_expression (fn [var_id] (fn [field_id] (->hydra_ext_java_syntax_expression_name (list :just (list var_id)) field_id))))

(def hydra_ext_java_utils_is_escaped (fn [s] ((hydra_lib_equality_equal ((hydra_lib_strings_char_at 0) s)) 36)))

(def hydra_ext_java_utils_unescape (fn [s] (hydra_lib_strings_from_list (hydra_lib_lists_tail (hydra_lib_strings_to_list s)))))

(def hydra_ext_java_utils_sanitize_java_name (fn [name] (if (hydra_ext_java_utils_is_escaped name) (hydra_ext_java_utils_unescape name) (if ((hydra_lib_equality_equal name) "_") "ignored" ((hydra_formatting_sanitize_with_underscores hydra_ext_java_language_reserved_words) name)))))

(def hydra_ext_java_utils_java_identifier (fn [s] (hydra_ext_java_utils_sanitize_java_name s)))

(def hydra_ext_java_utils_field_name_to_java_identifier (fn [fname] (hydra_ext_java_utils_java_identifier ((fn [v] v) fname))))

(def hydra_ext_java_utils_java_identifier_to_java_expression_name (fn [id] (->hydra_ext_java_syntax_expression_name (list :nothing) id)))

(def hydra_ext_java_utils_field_name_to_java_expression (fn [fname] (list :assignment (list :conditional (list :simple (list (list (list (list (list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :name (hydra_ext_java_utils_java_identifier_to_java_expression_name (hydra_ext_java_utils_field_name_to_java_identifier fname))))))))))))))))))))

(def hydra_ext_java_utils_java_variable_declarator_id (fn [id] (->hydra_ext_java_syntax_variable_declarator_id id (list :nothing))))

(def hydra_ext_java_utils_java_variable_declarator (fn [id] (fn [minit] (->hydra_ext_java_syntax_variable_declarator (hydra_ext_java_utils_java_variable_declarator_id id) minit))))

(def hydra_ext_java_utils_field_name_to_java_variable_declarator (fn [fname] ((hydra_ext_java_utils_java_variable_declarator (hydra_ext_java_utils_java_identifier ((fn [v] v) fname))) (list :nothing))))

(def hydra_ext_java_utils_field_name_to_java_variable_declarator_id (fn [fname] (hydra_ext_java_utils_java_variable_declarator_id (hydra_ext_java_utils_java_identifier ((fn [v] v) fname)))))

(def hydra_ext_java_utils_final_var_declaration_statement (fn [id] (fn [rhs] (list :local_variable_declaration (->hydra_ext_java_syntax_local_variable_declaration (list (list :final nil)) (list :var nil) (list ((hydra_ext_java_utils_java_variable_declarator id) (list :just (list :expression rhs)))))))))

(def hydra_ext_java_utils_import_aliases_for_module (fn [mod] (->hydra_ext_java_environment_aliases ((fn [v] (:namespace v)) mod) hydra_lib_maps_empty hydra_lib_sets_empty hydra_lib_sets_empty hydra_lib_sets_empty hydra_lib_sets_empty hydra_lib_sets_empty hydra_lib_maps_empty hydra_lib_sets_empty hydra_lib_maps_empty hydra_lib_sets_empty (list :nothing) hydra_lib_sets_empty)))

(def hydra_ext_java_utils_java_method_body (fn [mstmts] (((hydra_lib_maybes_cases mstmts) (fn [] (list :none nil))) (fn [stmts] (list :block stmts)))))

(def hydra_ext_java_utils_java_method_header (fn [tparams] (fn [method_name] (fn [params] (fn [result] (->hydra_ext_java_syntax_method_header tparams result (->hydra_ext_java_syntax_method_declarator method_name (list :nothing) params) (list :nothing)))))))

(def hydra_ext_java_utils_interface_method_declaration (fn [mods] (fn [tparams] (fn [method_name] (fn [params] (fn [result] (fn [stmts] (list :interface_method (->hydra_ext_java_syntax_interface_method_declaration mods ((((hydra_ext_java_utils_java_method_header tparams) method_name) params) result) (hydra_ext_java_utils_java_method_body stmts))))))))))

(def hydra_ext_java_utils_java_additive_expression_to_java_expression (fn [ae] (list :assignment (list :conditional (list :simple (list (list (list (list (list (list :unary (list :simple (list :unary ae)))))))))))))

(def hydra_ext_java_utils_java_primary_to_java_expression (fn [p] (list :assignment (list :conditional (list :simple (list (list (list (list (list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :primary p))))))))))))))))))

(def hydra_ext_java_utils_java_array_creation (fn [prim_type] (fn [minit] (let [init_ (((hydra_lib_maybes_cases minit) (fn [] (list))) (fn [i] i))] (hydra_ext_java_utils_java_primary_to_java_expression (list :array_creation (list :primitive_array (->hydra_ext_java_syntax_array_creation_expression_primitive_array prim_type (list) init_))))))))

(def hydra_ext_java_utils_java_array_initializer (fn [exprs] (list ((hydra_lib_lists_map (fn [e] (list :expression e))) exprs))))

(def hydra_ext_java_utils_java_assignment_statement (fn [lhs] (fn [rhs] (list :without_trailing (list :expression (list :assignment (->hydra_ext_java_syntax_assignment lhs (list :simple nil) rhs)))))))

(def hydra_ext_java_utils_java_boolean (fn [b] (list :boolean b)))

(def hydra_ext_java_utils_java_literal_to_java_primary (fn [lit] (list :no_new_array (list :literal lit))))

(def hydra_ext_java_utils_java_boolean_expression (fn [b] (hydra_ext_java_utils_java_primary_to_java_expression (hydra_ext_java_utils_java_literal_to_java_primary (hydra_ext_java_utils_java_boolean b)))))

(def hydra_ext_java_utils_java_primitive_type_to_java_type (fn [pt] (list :primitive (->hydra_ext_java_syntax_primitive_type_with_annotations pt (list)))))

(def hydra_ext_java_utils_java_boolean_type (hydra_ext_java_utils_java_primitive_type_to_java_type (list :boolean nil)))

(def hydra_ext_java_utils_java_byte_primitive_type (->hydra_ext_java_syntax_primitive_type_with_annotations (list :numeric (list :integral (list :byte nil))) (list)))

(def hydra_ext_java_utils_java_cast_expression (fn [rt] (fn [expr] (list :not_plus_minus (->hydra_ext_java_syntax_cast_expression_not_plus_minus (->hydra_ext_java_syntax_cast_expression_ref_and_bounds rt (list)) expr)))))

(def hydra_ext_java_utils_java_cast_expression_to_java_expression (fn [ce] (list :assignment (list :conditional (list :simple (list (list (list (list (list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :cast ce)))))))))))))))))

(def hydra_ext_java_utils_java_cast_primitive (fn [pt] (fn [expr] (list :primitive (->hydra_ext_java_syntax_cast_expression_primitive (->hydra_ext_java_syntax_primitive_type_with_annotations pt (list)) expr)))))

(def hydra_ext_java_utils_java_variable_name (fn [name] (hydra_ext_java_utils_java_identifier (hydra_names_local_name_of name))))

(def hydra_ext_java_utils_java_decl_name (fn [name] (hydra_ext_java_utils_java_variable_name name)))

(def hydra_ext_java_utils_java_type_identifier (fn [s] s))

(def hydra_ext_java_utils_name_to_qualified_java_name (fn [aliases] (fn [qualify] (fn [name] (fn [mlocal] (let [qn (hydra_names_qualify_name name) ns_ ((fn [v] (:namespace v)) qn) alias (((hydra_lib_maybes_cases ns_) (fn [] (list :nothing))) (fn [n] (list :just (((hydra_lib_maybes_cases ((hydra_lib_maps_lookup n) ((fn [v] (:packages v)) aliases))) (fn [] (hydra_ext_java_names_java_package_name ((hydra_lib_strings_split_on ".") ((fn [v] v) n))))) (fn [id] id))))) local ((fn [v] (:local v)) qn) jid (hydra_ext_java_utils_java_type_identifier (((hydra_lib_maybes_cases mlocal) (fn [] (hydra_ext_java_utils_sanitize_java_name local))) (fn [l] ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 (hydra_ext_java_utils_sanitize_java_name local)) ".")) (hydra_ext_java_utils_sanitize_java_name l))))) pkg (if qualify (((hydra_lib_maybes_cases alias) (fn [] (list :none nil))) (fn [p] (list :package p))) (list :none nil))] (list jid pkg)))))))

(def hydra_ext_java_utils_name_to_java_class_type (fn [aliases] (fn [qualify] (fn [args] (fn [name] (fn [mlocal] (let [result ((((hydra_ext_java_utils_name_to_qualified_java_name aliases) qualify) name) mlocal) id (hydra_lib_pairs_first result) pkg (hydra_lib_pairs_second result)] (->hydra_ext_java_syntax_class_type (list) pkg id args))))))))

(def hydra_ext_java_utils_java_class_declaration (fn [aliases] (fn [tparams] (fn [el_name] (fn [mods] (fn [supname] (fn [impls] (fn [body_decls] (let [extends_ ((hydra_lib_maybes_map (fn [n] (((((hydra_ext_java_utils_name_to_java_class_type aliases) true) (list)) n) (list :nothing)))) supname)] (list :normal (->hydra_ext_java_syntax_normal_class_declaration mods (hydra_ext_java_utils_java_decl_name el_name) tparams extends_ impls body_decls)))))))))))

(def hydra_ext_java_utils_java_class_type (fn [args] (fn [pkg] (fn [id] (let [qual (((hydra_lib_maybes_cases pkg) (fn [] (list :none nil))) (fn [p] (list :package p))) targs ((hydra_lib_lists_map (fn [rt] (list :reference rt))) args)] (->hydra_ext_java_syntax_class_type (list) qual (hydra_ext_java_utils_java_type_identifier id) targs))))))

(def hydra_ext_java_utils_java_class_type_to_java_type (fn [ct] (list :reference (list :class_or_interface (list :class ct)))))

(def hydra_ext_java_utils_java_conditional_and_expression_to_java_expression (fn [cae] (list :assignment (list :conditional (list :simple (list cae))))))

(def hydra_ext_java_utils_java_constructor_call (fn [ci] (fn [args] (fn [mbody] (list :assignment (list :conditional (list :simple (list (list (list (list (list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :primary (list :no_new_array (list :class_instance (->hydra_ext_java_syntax_class_instance_creation_expression (list :nothing) (->hydra_ext_java_syntax_unqualified_class_instance_creation_expression (list) ci args mbody))))))))))))))))))))))))

(def hydra_ext_java_utils_java_constructor_name (fn [id] (fn [targs] (->hydra_ext_java_syntax_class_or_interface_type_to_instantiate (list (->hydra_ext_java_syntax_annotated_identifier (list) id)) targs))))

(def hydra_ext_java_utils_java_expression_to_java_unary_expression (fn [e] (list :other (list :postfix (list :primary (list :no_new_array (list :parens e)))))))

(def hydra_ext_java_utils_java_double_cast_expression (fn [raw_rt] (fn [target_rt] (fn [expr] (let [first_cast (hydra_ext_java_utils_java_cast_expression_to_java_expression ((hydra_ext_java_utils_java_cast_expression raw_rt) expr))] ((hydra_ext_java_utils_java_cast_expression target_rt) (hydra_ext_java_utils_java_expression_to_java_unary_expression first_cast)))))))

(def hydra_ext_java_utils_java_double_cast_expression_to_java_expression (fn [raw_rt] (fn [target_rt] (fn [expr] (hydra_ext_java_utils_java_cast_expression_to_java_expression (((hydra_ext_java_utils_java_double_cast_expression raw_rt) target_rt) expr))))))

(def hydra_ext_java_utils_java_empty_statement (list :without_trailing (list :empty nil)))

(def hydra_ext_java_utils_java_equality_expression_to_java_expression (fn [ee] (list :assignment (list :conditional (list :simple (list (list (list (list (list ee))))))))))

(def hydra_ext_java_utils_java_equality_expression_to_java_inclusive_or_expression (fn [ee] (list (list (list ee)))))

(def hydra_ext_java_utils_java_equals (fn [lhs] (fn [rhs] (list :equal (->hydra_ext_java_syntax_equality_expression_binary lhs rhs)))))

(def hydra_ext_java_utils_java_literal_to_java_relational_expression (fn [lit] (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :primary (list :no_new_array (list :literal lit)))))))))))

(def hydra_ext_java_utils_java_equals_null (fn [lhs] ((hydra_ext_java_utils_java_equals lhs) (hydra_ext_java_utils_java_literal_to_java_relational_expression (list :null nil)))))

(def hydra_ext_java_utils_java_expression_name_to_java_expression (fn [en] (list :assignment (list :conditional (list :simple (list (list (list (list (list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :name en))))))))))))))))))

(def hydra_ext_java_utils_java_expression_to_java_primary (fn [e] (let [fallback (list :no_new_array (list :parens e))] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :assignment) ((fn [ae] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :conditional) ((fn [ce] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :simple) ((fn [cor] (let [cands ((fn [v] v) cor)] (if ((hydra_lib_equality_equal (hydra_lib_lists_length cands)) 1) (let [iors ((fn [v] v) (hydra_lib_lists_head cands))] (if ((hydra_lib_equality_equal (hydra_lib_lists_length iors)) 1) (let [xors ((fn [v] v) (hydra_lib_lists_head iors))] (if ((hydra_lib_equality_equal (hydra_lib_lists_length xors)) 1) (let [ands ((fn [v] v) (hydra_lib_lists_head xors))] (if ((hydra_lib_equality_equal (hydra_lib_lists_length ands)) 1) (let [eqs ((fn [v] v) (hydra_lib_lists_head ands))] (if ((hydra_lib_equality_equal (hydra_lib_lists_length eqs)) 1) ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :unary) ((fn [rel] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :simple) ((fn [shift] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :unary) ((fn [add] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :unary) ((fn [mul] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :unary) ((fn [unary] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :other) ((fn [npm] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :postfix) ((fn [pf] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :primary) ((fn [p] p) match_value) :else fallback)) (second match_target))) pf)) match_value) :else fallback)) (second match_target))) npm)) match_value) :else fallback)) (second match_target))) unary)) match_value) :else fallback)) (second match_target))) mul)) match_value) :else fallback)) (second match_target))) add)) match_value) :else fallback)) (second match_target))) shift)) match_value) :else fallback)) (second match_target))) rel)) match_value) :else fallback)) (second match_target))) (hydra_lib_lists_head eqs)) fallback)) fallback)) fallback)) fallback)) fallback))) match_value) :else fallback)) (second match_target))) ce)) match_value) :else fallback)) (second match_target))) ae)) match_value) :else fallback)) (second match_target))) e))))

(def hydra_ext_java_utils_java_field_access_to_java_expression (fn [fa] (list :assignment (list :conditional (list :simple (list (list (list (list (list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :primary (list :no_new_array (list :field_access fa))))))))))))))))))))

(def hydra_ext_java_utils_java_identifier_to_java_expression (fn [id] (list :assignment (list :conditional (list :simple (list (list (list (list (list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :name (->hydra_ext_java_syntax_expression_name (list :nothing) id)))))))))))))))))))

(def hydra_ext_java_utils_java_identifier_to_java_relational_expression (fn [id] (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :name (->hydra_ext_java_syntax_expression_name (list :nothing) id))))))))))

(def hydra_ext_java_utils_java_identifier_to_java_unary_expression (fn [id] (list :other (list :postfix (list :name (->hydra_ext_java_syntax_expression_name (list :nothing) id))))))

(def hydra_ext_java_utils_java_instance_of (fn [lhs] (fn [rhs] (list :instanceof (->hydra_ext_java_syntax_relational_expression_instance_of lhs rhs)))))

(def hydra_ext_java_utils_java_int (fn [i] (list :integer i)))

(def hydra_ext_java_utils_java_int_expression (fn [i] (hydra_ext_java_utils_java_primary_to_java_expression (hydra_ext_java_utils_java_literal_to_java_primary (hydra_ext_java_utils_java_int i)))))

(def hydra_ext_java_utils_java_int_type (hydra_ext_java_utils_java_primitive_type_to_java_type (list :numeric (list :integral (list :int nil)))))

(def hydra_ext_java_utils_java_interface_declaration_to_java_class_body_declaration (fn [nid] (list :class_member (list :interface (list :normal_interface nid)))))

(def hydra_ext_java_utils_variable_to_java_identifier (fn [name] (let [v ((fn [v] v) name)] (if ((hydra_lib_equality_equal v) "_") "ignored" (hydra_ext_java_utils_sanitize_java_name v)))))

(def hydra_ext_java_utils_java_lambda (fn [v] (fn [body] (list :lambda (->hydra_ext_java_syntax_lambda_expression (list :single (hydra_ext_java_utils_variable_to_java_identifier v)) (list :expression body))))))

(def hydra_ext_java_utils_java_lambda_from_block (fn [v] (fn [block_] (list :lambda (->hydra_ext_java_syntax_lambda_expression (list :single (hydra_ext_java_utils_variable_to_java_identifier v)) (list :block block_))))))

(def hydra_ext_java_utils_java_literal_to_java_expression (fn [lit] (list :assignment (list :conditional (list :simple (list (list (list (list (list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :primary (list :no_new_array (list :literal lit))))))))))))))))))))

(def hydra_ext_java_utils_java_literal_to_java_multiplicative_expression (fn [lit] (list :unary (list :other (list :postfix (list :primary (list :no_new_array (list :literal lit))))))))

(def hydra_ext_java_utils_java_member_field (fn [mods] (fn [jt] (fn [v] (list :class_member (list :field (->hydra_ext_java_syntax_field_declaration mods jt (list v))))))))

(def hydra_ext_java_utils_java_method_declaration_to_java_class_body_declaration (fn [md] (list :class_member (list :method md))))

(def hydra_ext_java_utils_java_method_invocation_to_java_expression (fn [mi] (list :assignment (list :conditional (list :simple (list (list (list (list (list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :primary (list :no_new_array (list :method_invocation mi))))))))))))))))))))

(def hydra_ext_java_utils_java_method_invocation_to_java_postfix_expression (fn [mi] (list :primary (list :no_new_array (list :method_invocation mi)))))

(def hydra_ext_java_utils_java_method_invocation_to_java_primary (fn [mi] (list :no_new_array (list :method_invocation mi))))

(def hydra_ext_java_utils_java_method_invocation_to_java_statement (fn [mi] (list :without_trailing (list :expression (list :method_invocation mi)))))

(def hydra_ext_java_utils_java_multiplicative_expression_to_java_relational_expression (fn [me] (list :simple (list :unary (list :unary me)))))

(def hydra_ext_java_utils_java_package_declaration (fn [ns_] (->hydra_ext_java_syntax_package_declaration (list) ((hydra_lib_lists_map (fn [s] s)) ((hydra_lib_strings_split_on ".") ((fn [v] v) ns_))))))

(def hydra_ext_java_utils_java_postfix_expression_to_java_equality_expression (fn [pe] (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix pe)))))))))

(def hydra_ext_java_utils_java_postfix_expression_to_java_expression (fn [pe] (list :assignment (list :conditional (list :simple (list (list (list (list (list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix pe)))))))))))))))))

(def hydra_ext_java_utils_java_postfix_expression_to_java_inclusive_or_expression (fn [pe] (list (list (list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix pe))))))))))))

(def hydra_ext_java_utils_java_postfix_expression_to_java_relational_expression (fn [pe] (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix pe))))))))

(def hydra_ext_java_utils_java_postfix_expression_to_java_unary_expression (fn [pe] (list :other (list :postfix pe))))

(def hydra_ext_java_utils_java_primary_to_java_unary_expression (fn [p] (list :other (list :postfix (list :primary p)))))

(def hydra_ext_java_utils_java_ref_type (fn [args] (fn [pkg] (fn [id] (list :reference (list :class_or_interface (list :class (((hydra_ext_java_utils_java_class_type args) pkg) id))))))))

(def hydra_ext_java_utils_java_reference_type_to_raw_type (fn [rt] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :class_or_interface) ((fn [cit] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :class) ((fn [ct] (let [anns ((fn [v] (:annotations v)) ct) id ((fn [v] (:identifier v)) ct) qual ((fn [v] (:qualifier v)) ct)] (list :class_or_interface (list :class (->hydra_ext_java_syntax_class_type anns qual id (list)))))) match_value) (= (first match_target) :interface) ((fn [it] (let [ct ((fn [v] v) it) anns ((fn [v] (:annotations v)) ct) id ((fn [v] (:identifier v)) ct) qual ((fn [v] (:qualifier v)) ct)] (list :class_or_interface (list :interface (->hydra_ext_java_syntax_class_type anns qual id (list)))))) match_value))) (second match_target))) cit)) match_value) :else rt)) (second match_target))) rt)))

(def hydra_ext_java_utils_java_relational_expression_to_java_equality_expression (fn [re] (list :unary re)))

(def hydra_ext_java_utils_java_relational_expression_to_java_expression (fn [re] (hydra_ext_java_utils_java_equality_expression_to_java_expression (list :unary re))))

(def hydra_ext_java_utils_java_relational_expression_to_java_unary_expression (fn [re] (list :other (list :postfix (list :primary (list :no_new_array (list :parens (list :assignment (list :conditional (list :simple (list (list (list (list (list (list :unary re))))))))))))))))

(def hydra_ext_java_utils_java_return_statement (fn [mex] (list :without_trailing (list :return mex))))

(def hydra_ext_java_utils_java_statements_to_block (fn [stmts] ((hydra_lib_lists_map (fn [s] (list :statement s))) stmts)))

(def hydra_ext_java_utils_java_string (fn [s] (list :string s)))

(def hydra_ext_java_utils_java_string_multiplicative_expression (fn [s] (hydra_ext_java_utils_java_literal_to_java_multiplicative_expression (hydra_ext_java_utils_java_string s))))

(def hydra_ext_java_utils_java_this (list :assignment (list :conditional (list :simple (list (list (list (list (list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :primary (list :no_new_array (list :this nil)))))))))))))))))))

(def hydra_ext_java_utils_java_throw_statement (fn [e] (list :without_trailing (list :throw e))))

(def hydra_ext_java_utils_java_throw_illegal_argument_exception (fn [args] (hydra_ext_java_utils_java_throw_statement (((hydra_ext_java_utils_java_constructor_call ((hydra_ext_java_utils_java_constructor_name "IllegalArgumentException") (list :nothing))) args) (list :nothing)))))

(def hydra_ext_java_utils_java_throw_illegal_state_exception (fn [args] (hydra_ext_java_utils_java_throw_statement (((hydra_ext_java_utils_java_constructor_call ((hydra_ext_java_utils_java_constructor_name "IllegalStateException") (list :nothing))) args) (list :nothing)))))

(def hydra_ext_java_utils_name_to_java_type_identifier (fn [aliases] (fn [qualify] (fn [name] (hydra_lib_pairs_first ((((hydra_ext_java_utils_name_to_qualified_java_name aliases) qualify) name) (list :nothing)))))))

(def hydra_ext_java_utils_java_type_from_type_name (fn [aliases] (fn [el_name] (hydra_ext_java_utils_java_type_variable_to_type (->hydra_ext_java_syntax_type_variable (list) (((hydra_ext_java_utils_name_to_java_type_identifier aliases) false) el_name))))))

(def hydra_ext_java_utils_java_type_identifier_to_java_type_argument (fn [id] (list :reference (list :variable (->hydra_ext_java_syntax_type_variable (list) id)))))

(def hydra_ext_java_utils_java_type_name (fn [id] (->hydra_ext_java_syntax_type_name id (list :nothing))))

(def hydra_ext_java_utils_java_type_parameter (fn [v] (->hydra_ext_java_syntax_type_parameter (list) (hydra_ext_java_utils_java_type_identifier v) (list :nothing))))

(def hydra_ext_java_utils_java_type_to_java_formal_parameter (fn [jt] (fn [fname] (list :simple (->hydra_ext_java_syntax_formal_parameter_simple (list) jt (hydra_ext_java_utils_field_name_to_java_variable_declarator_id fname))))))

(def hydra_ext_java_utils_java_type_to_java_reference_type (fn [t_] (fn [cx] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :reference) ((fn [rt] (list :right rt)) match_value) (= (first match_target) :primitive) ((fn [_] (list :left (list :other "expected a Java reference type"))) match_value))) (second match_target))) t_))))

(def hydra_ext_java_utils_java_type_to_java_result (fn [jt] (list :type jt)))

(def hydra_ext_java_utils_java_type_to_java_type_argument (fn [t_] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :reference) ((fn [rt] (list :reference rt)) match_value) (= (first match_target) :primitive) ((fn [_] (list :wildcard (->hydra_ext_java_syntax_wildcard (list) (list :nothing)))) match_value))) (second match_target))) t_)))

(def hydra_ext_java_utils_java_type_variable (fn [v] (list :variable (->hydra_ext_java_syntax_type_variable (list) (hydra_ext_java_utils_java_type_identifier (hydra_formatting_capitalize v))))))

(def hydra_ext_java_utils_java_unary_expression_to_java_expression (fn [ue] (list :assignment (list :conditional (list :simple (list (list (list (list (list (list :unary (list :simple (list :unary (list :unary (list :unary ue)))))))))))))))

(def hydra_ext_java_utils_java_unary_expression_to_java_relational_expression (fn [ue] (list :simple (list :unary (list :unary (list :unary ue))))))

(def hydra_ext_java_utils_lookup_java_var_name (fn [aliases] (fn [name] (((hydra_lib_maybes_cases ((hydra_lib_maps_lookup name) ((fn [v] (:var_renames v)) aliases))) (fn [] name)) (fn [renamed] renamed)))))

(def hydra_ext_java_utils_make_constructor (fn [aliases] (fn [el_name] (fn [private] (fn [params] (fn [stmts] (let [body (->hydra_ext_java_syntax_constructor_body (list :nothing) stmts) nm (((hydra_ext_java_utils_name_to_java_type_identifier aliases) false) el_name) cons (->hydra_ext_java_syntax_constructor_declarator (list) nm (list :nothing) params) mods (list (if private (list :private nil) (list :public nil)))] (list :constructor_declaration (->hydra_ext_java_syntax_constructor_declaration mods cons (list :nothing) body)))))))))

(def hydra_ext_java_utils_method_declaration (fn [mods] (fn [tparams] (fn [anns] (fn [method_name] (fn [params] (fn [result] (fn [stmts] (hydra_ext_java_utils_java_method_declaration_to_java_class_body_declaration (->hydra_ext_java_syntax_method_declaration anns mods ((((hydra_ext_java_utils_java_method_header tparams) method_name) params) result) (hydra_ext_java_utils_java_method_body stmts)))))))))))

(def hydra_ext_java_utils_method_invocation (fn [lhs] (fn [method_name] (fn [args] (let [header (((hydra_lib_maybes_cases lhs) (fn [] (list :simple method_name))) (fn [either] (list :complex (->hydra_ext_java_syntax_method_invocation_complex (((hydra_lib_eithers_either (fn [en] (list :expression en))) (fn [p] (list :primary p))) either) (list) method_name))))] (->hydra_ext_java_syntax_method_invocation header args))))))

(def hydra_ext_java_utils_method_invocation_static (fn [self] (fn [method_name] (fn [args] (((hydra_ext_java_utils_method_invocation (list :just (list :left (hydra_ext_java_utils_java_identifier_to_java_expression_name self)))) method_name) args)))))

(def hydra_ext_java_utils_method_invocation_static_with_type_args (fn [self] (fn [method_name] (fn [targs] (fn [args] (let [header (list :complex (->hydra_ext_java_syntax_method_invocation_complex (list :expression (hydra_ext_java_utils_java_identifier_to_java_expression_name self)) targs method_name))] (->hydra_ext_java_syntax_method_invocation header args)))))))

(def hydra_ext_java_utils_name_to_java_name (fn [aliases] (fn [name] (let [qn (hydra_names_qualify_name name) local ((fn [v] (:local v)) qn) ns_ ((fn [v] (:namespace v)) qn)] (if (hydra_ext_java_utils_is_escaped ((fn [v] v) name)) (hydra_ext_java_utils_sanitize_java_name local) (((hydra_lib_maybes_cases ns_) (fn [] local)) (fn [gname] (let [parts (((hydra_lib_maybes_cases ((hydra_lib_maps_lookup gname) ((fn [v] (:packages v)) aliases))) (fn [] ((hydra_lib_strings_split_on ".") ((fn [v] v) gname)))) (fn [pkg_name] ((hydra_lib_lists_map (fn [i] ((fn [v] v) i))) ((fn [v] v) pkg_name)))) all_parts ((hydra_lib_lists_concat2 parts) (list (hydra_ext_java_utils_sanitize_java_name local)))] ((hydra_lib_strings_intercalate ".") all_parts)))))))))

(def hydra_ext_java_utils_name_to_java_reference_type (fn [aliases] (fn [qualify] (fn [args] (fn [name] (fn [mlocal] (list :class_or_interface (list :class (((((hydra_ext_java_utils_name_to_java_class_type aliases) qualify) args) name) mlocal)))))))))

(def hydra_ext_java_utils_override_annotation (list :marker (hydra_ext_java_utils_java_type_name "Override")))

(def hydra_ext_java_utils_reference_type_to_result (fn [rt] (hydra_ext_java_utils_java_type_to_java_result (list :reference rt))))

(def hydra_ext_java_utils_suppress_warnings_unchecked_annotation (list :single_element (->hydra_ext_java_syntax_single_element_annotation (hydra_ext_java_utils_java_type_name "SuppressWarnings") (list :just (list :conditional_expression (list :simple (list (list (hydra_ext_java_utils_java_postfix_expression_to_java_inclusive_or_expression (list :primary (hydra_ext_java_utils_java_literal_to_java_primary (hydra_ext_java_utils_java_string "unchecked"))))))))))))

(def hydra_ext_java_utils_type_parameter_to_reference_type (fn [tp] (hydra_ext_java_utils_java_type_variable ((fn [v] v) ((fn [v] v) ((fn [v] (:identifier v)) tp))))))

(def hydra_ext_java_utils_visitor_type_variable (hydra_ext_java_utils_java_type_variable "r"))

(def hydra_ext_java_utils_to_accept_method (fn [abstract] (fn [vtparams] (let [anns (if abstract (list) (list hydra_ext_java_utils_override_annotation)) return_expr (hydra_ext_java_utils_java_method_invocation_to_java_expression (((hydra_ext_java_utils_method_invocation_static "visitor") hydra_ext_java_names_visit_method_name) (list hydra_ext_java_utils_java_this))) body (if abstract (list :nothing) (list :just (list (list :statement (hydra_ext_java_utils_java_return_statement (list :just return_expr)))))) mods (if abstract (list (list :public nil) (list :abstract nil)) (list (list :public nil))) type_args ((hydra_lib_lists_map (fn [tp] (list :reference (hydra_ext_java_utils_type_parameter_to_reference_type tp)))) vtparams) ref (hydra_ext_java_utils_java_class_type_to_java_type (->hydra_ext_java_syntax_class_type (list) (list :none nil) (hydra_ext_java_utils_java_type_identifier hydra_ext_java_names_visitor_name) ((hydra_lib_lists_concat2 type_args) (list (list :reference hydra_ext_java_utils_visitor_type_variable))))) param ((hydra_ext_java_utils_java_type_to_java_formal_parameter ref) "visitor") result (hydra_ext_java_utils_java_type_to_java_result (list :reference hydra_ext_java_utils_visitor_type_variable)) tparams (list (hydra_ext_java_utils_java_type_parameter hydra_ext_java_names_visitor_return_parameter))] (((((((hydra_ext_java_utils_method_declaration mods) tparams) anns) hydra_ext_java_names_accept_method_name) (list param)) result) body)))))

(def hydra_ext_java_utils_to_assign_stmt (fn [fname] (let [id (hydra_ext_java_utils_field_name_to_java_identifier fname) lhs (list :field_access (->hydra_ext_java_syntax_field_access (list :primary (list :no_new_array (list :this nil))) id)) rhs (hydra_ext_java_utils_field_name_to_java_expression fname)] ((hydra_ext_java_utils_java_assignment_statement lhs) rhs))))

(def hydra_ext_java_utils_to_java_array_type (fn [t_] (fn [cx] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :reference) ((fn [rt] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :class_or_interface) ((fn [cit] (list :right (list :reference (list :array (->hydra_ext_java_syntax_array_type (list (list)) (list :class_or_interface cit)))))) match_value) (= (first match_target) :array) ((fn [at] (let [old_dims ((fn [v] v) ((fn [v] (:dims v)) at)) new_dims ((hydra_lib_lists_concat2 old_dims) (list (list))) variant ((fn [v] (:variant v)) at)] (list :right (list :reference (list :array (->hydra_ext_java_syntax_array_type new_dims variant)))))) match_value) (= (first match_target) :variable) ((fn [_] (list :left (list :other "don't know how to make Java reference type into array type"))) match_value))) (second match_target))) rt)) match_value) (= (first match_target) :primitive) ((fn [_] (list :left (list :other "don't know how to make Java type into array type"))) match_value))) (second match_target))) t_))))

(def hydra_ext_java_utils_type_parameter_to_type_argument (fn [tp] (hydra_ext_java_utils_java_type_identifier_to_java_type_argument ((fn [v] (:identifier v)) tp))))

(def hydra_ext_java_utils_un_type_parameter (fn [tp] ((fn [v] v) ((fn [v] v) ((fn [v] (:identifier v)) tp)))))

(def hydra_ext_java_utils_unique_var_name_go (fn [aliases] (fn [base] (fn [n] (let [candidate ((hydra_lib_strings_cat2 base) (hydra_lib_literals_show_int32 n))] (if ((hydra_lib_sets_member candidate) ((fn [v] (:in_scope_java_vars v)) aliases)) (((hydra_ext_java_utils_unique_var_name_go aliases) base) ((hydra_lib_math_add n) 1)) candidate))))))

(def hydra_ext_java_utils_unique_var_name (fn [aliases] (fn [name] (if ((hydra_lib_sets_member name) ((fn [v] (:in_scope_java_vars v)) aliases)) (((hydra_ext_java_utils_unique_var_name_go aliases) ((fn [v] v) name)) 2) name))))

(def hydra_ext_java_utils_var_declaration_statement (fn [id] (fn [rhs] (list :local_variable_declaration (->hydra_ext_java_syntax_local_variable_declaration (list) (list :var nil) (list ((hydra_ext_java_utils_java_variable_declarator id) (list :just (list :expression rhs)))))))))

(def hydra_ext_java_utils_variable_declaration_statement (fn [aliases] (fn [jtype] (fn [id] (fn [rhs] (let [init_ (list :expression rhs) vdec ((hydra_ext_java_utils_java_variable_declarator id) (list :just init_))] (list :local_variable_declaration (->hydra_ext_java_syntax_local_variable_declaration (list) (list :type jtype) (list vdec)))))))))

(def hydra_ext_java_utils_variant_class_name (fn [qualify] (fn [el_name] (fn [fname] (let [flocal (hydra_formatting_capitalize ((fn [v] v) fname)) qn (hydra_names_qualify_name el_name) local ((fn [v] (:local v)) qn) local1 (if qualify ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 local) ".")) flocal) (if ((hydra_lib_equality_equal flocal) local) ((hydra_lib_strings_cat2 flocal) "_") flocal)) ns_ ((fn [v] (:namespace v)) qn)] (hydra_names_unqualify_name (->hydra_packaging_qualified_name ns_ local1)))))))
