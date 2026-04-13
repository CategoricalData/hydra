(defpackage :hydra.java.utils
(:use :cl :hydra.core :hydra.errors :hydra.formatting :hydra.java.environment :hydra.java.language :hydra.java.names :hydra.java.syntax :hydra.lib.eithers :hydra.lib.equality :hydra.lib.lists :hydra.lib.literals :hydra.lib.logic :hydra.lib.maps :hydra.lib.math :hydra.lib.maybes :hydra.lib.pairs :hydra.lib.sets :hydra.lib.strings :hydra.names :hydra.packaging)
(:export :hydra_java_utils_add_expressions :hydra_java_utils_add_in_scope_var :hydra_java_utils_add_in_scope_vars :hydra_java_utils_java_type_variable_to_type :hydra_java_utils_add_java_type_parameter :hydra_java_utils_add_var_rename :hydra_java_utils_field_expression :hydra_java_utils_is_escaped :hydra_java_utils_unescape :hydra_java_utils_sanitize_java_name :hydra_java_utils_java_identifier :hydra_java_utils_field_name_to_java_identifier :hydra_java_utils_java_identifier_to_java_expression_name :hydra_java_utils_field_name_to_java_expression :hydra_java_utils_java_variable_declarator_id :hydra_java_utils_java_variable_declarator :hydra_java_utils_field_name_to_java_variable_declarator :hydra_java_utils_field_name_to_java_variable_declarator_id :hydra_java_utils_final_var_declaration_statement :hydra_java_utils_import_aliases_for_module :hydra_java_utils_java_method_body :hydra_java_utils_java_method_header :hydra_java_utils_interface_method_declaration :hydra_java_utils_java_additive_expression_to_java_expression :hydra_java_utils_java_primary_to_java_expression :hydra_java_utils_java_array_creation :hydra_java_utils_java_array_initializer :hydra_java_utils_java_assignment_statement :hydra_java_utils_java_boolean :hydra_java_utils_java_literal_to_java_primary :hydra_java_utils_java_boolean_expression :hydra_java_utils_java_primitive_type_to_java_type :hydra_java_utils_java_boolean_type :hydra_java_utils_java_byte_primitive_type :hydra_java_utils_java_cast_expression :hydra_java_utils_java_cast_expression_to_java_expression :hydra_java_utils_java_cast_primitive :hydra_java_utils_java_variable_name :hydra_java_utils_java_decl_name :hydra_java_utils_java_type_identifier :hydra_java_utils_name_to_qualified_java_name :hydra_java_utils_name_to_java_class_type :hydra_java_utils_java_class_declaration :hydra_java_utils_java_class_type :hydra_java_utils_java_class_type_to_java_type :hydra_java_utils_java_conditional_and_expression_to_java_expression :hydra_java_utils_java_constructor_call :hydra_java_utils_java_constructor_name :hydra_java_utils_java_expression_to_java_unary_expression :hydra_java_utils_java_double_cast_expression :hydra_java_utils_java_double_cast_expression_to_java_expression :hydra_java_utils_java_empty_statement :hydra_java_utils_java_equality_expression_to_java_expression :hydra_java_utils_java_equality_expression_to_java_inclusive_or_expression :hydra_java_utils_java_equals :hydra_java_utils_java_literal_to_java_relational_expression :hydra_java_utils_java_equals_null :hydra_java_utils_java_expression_name_to_java_expression :hydra_java_utils_java_expression_to_java_primary :hydra_java_utils_java_field_access_to_java_expression :hydra_java_utils_java_identifier_to_java_expression :hydra_java_utils_java_identifier_to_java_relational_expression :hydra_java_utils_java_identifier_to_java_unary_expression :hydra_java_utils_java_instance_of :hydra_java_utils_java_int :hydra_java_utils_java_int_expression :hydra_java_utils_java_int_type :hydra_java_utils_java_interface_declaration_to_java_class_body_declaration :hydra_java_utils_variable_to_java_identifier :hydra_java_utils_java_lambda :hydra_java_utils_java_lambda_from_block :hydra_java_utils_java_literal_to_java_expression :hydra_java_utils_java_literal_to_java_multiplicative_expression :hydra_java_utils_java_member_field :hydra_java_utils_java_method_declaration_to_java_class_body_declaration :hydra_java_utils_java_method_invocation_to_java_expression :hydra_java_utils_java_method_invocation_to_java_postfix_expression :hydra_java_utils_java_method_invocation_to_java_primary :hydra_java_utils_java_method_invocation_to_java_statement :hydra_java_utils_java_multiplicative_expression_to_java_relational_expression :hydra_java_utils_java_package_declaration :hydra_java_utils_java_postfix_expression_to_java_equality_expression :hydra_java_utils_java_postfix_expression_to_java_expression :hydra_java_utils_java_postfix_expression_to_java_inclusive_or_expression :hydra_java_utils_java_postfix_expression_to_java_relational_expression :hydra_java_utils_java_postfix_expression_to_java_unary_expression :hydra_java_utils_java_primary_to_java_unary_expression :hydra_java_utils_java_ref_type :hydra_java_utils_java_reference_type_to_raw_type :hydra_java_utils_java_relational_expression_to_java_equality_expression :hydra_java_utils_java_relational_expression_to_java_expression :hydra_java_utils_java_relational_expression_to_java_unary_expression :hydra_java_utils_java_return_statement :hydra_java_utils_java_statements_to_block :hydra_java_utils_java_string :hydra_java_utils_java_string_multiplicative_expression :hydra_java_utils_java_this :hydra_java_utils_java_throw_statement :hydra_java_utils_java_throw_illegal_argument_exception :hydra_java_utils_java_throw_illegal_state_exception :hydra_java_utils_name_to_java_type_identifier :hydra_java_utils_java_type_from_type_name :hydra_java_utils_java_type_identifier_to_java_type_argument :hydra_java_utils_java_type_name :hydra_java_utils_java_type_parameter :hydra_java_utils_java_type_to_java_formal_parameter :hydra_java_utils_java_type_to_java_reference_type :hydra_java_utils_java_type_to_java_result :hydra_java_utils_java_type_to_java_type_argument :hydra_java_utils_java_type_variable :hydra_java_utils_java_unary_expression_to_java_expression :hydra_java_utils_java_unary_expression_to_java_relational_expression :hydra_java_utils_lookup_java_var_name :hydra_java_utils_make_constructor :hydra_java_utils_method_declaration :hydra_java_utils_method_invocation :hydra_java_utils_method_invocation_static :hydra_java_utils_method_invocation_static_with_type_args :hydra_java_utils_name_to_java_name :hydra_java_utils_name_to_java_reference_type :hydra_java_utils_override_annotation :hydra_java_utils_reference_type_to_result :hydra_java_utils_suppress_warnings_unchecked_annotation :hydra_java_utils_type_parameter_to_reference_type :hydra_java_utils_visitor_type_variable :hydra_java_utils_to_accept_method :hydra_java_utils_to_assign_stmt :hydra_java_utils_to_java_array_type :hydra_java_utils_type_parameter_to_type_argument :hydra_java_utils_un_type_parameter :hydra_java_utils_unique_var_name_go :hydra_java_utils_unique_var_name :hydra_java_utils_var_declaration_statement :hydra_java_utils_variable_declaration_statement :hydra_java_utils_variant_class_name))

(in-package :hydra.java.utils)

(cl:defvar hydra_java_utils_add_expressions (cl:lambda (exprs) (let* ((first (list :unary (hydra_lib_lists_head exprs))) (rest (hydra_lib_lists_tail exprs))) (((hydra_lib_lists_foldl (cl:lambda (ae) (cl:lambda (me) (list :plus (make-hydra_java_syntax_additive_expression_binary ae me))))) first) rest))))

(cl:defvar hydra_java_utils_add_in_scope_var (cl:lambda (name) (cl:lambda (aliases) (make-hydra_java_environment_aliases ((cl:lambda (v) (hydra_java_environment_aliases-current_namespace v)) aliases) ((cl:lambda (v) (hydra_java_environment_aliases-packages v)) aliases) ((cl:lambda (v) (hydra_java_environment_aliases-branch_vars v)) aliases) ((cl:lambda (v) (hydra_java_environment_aliases-recursive_vars v)) aliases) ((cl:lambda (v) (hydra_java_environment_aliases-in_scope_type_params v)) aliases) ((cl:lambda (v) (hydra_java_environment_aliases-polymorphic_locals v)) aliases) ((hydra_lib_sets_insert name) ((cl:lambda (v) (hydra_java_environment_aliases-in_scope_java_vars v)) aliases)) ((cl:lambda (v) (hydra_java_environment_aliases-var_renames v)) aliases) ((cl:lambda (v) (hydra_java_environment_aliases-lambda_vars v)) aliases) ((cl:lambda (v) (hydra_java_environment_aliases-type_var_subst v)) aliases) ((cl:lambda (v) (hydra_java_environment_aliases-trusted_type_vars v)) aliases) ((cl:lambda (v) (hydra_java_environment_aliases-method_codomain v)) aliases) ((cl:lambda (v) (hydra_java_environment_aliases-thunked_vars v)) aliases)))))

(cl:defvar hydra_java_utils_add_in_scope_vars (cl:lambda (names) (cl:lambda (aliases) (((hydra_lib_lists_foldl (cl:lambda (a) (cl:lambda (n) ((hydra_java_utils_add_in_scope_var n) a)))) aliases) names))))

(cl:defvar hydra_java_utils_java_type_variable_to_type (cl:lambda (tv) (list :reference (list :variable tv))))

(cl:defvar hydra_java_utils_add_java_type_parameter (cl:lambda (rt) (cl:lambda (t_) (cl:lambda (cx) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :reference) ((cl:lambda (rt1) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :class_or_interface) ((cl:lambda (cit) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :class) ((cl:lambda (ct) (let* ((anns ((cl:lambda (v) (hydra_java_syntax_class_type-annotations v)) ct)) (args ((cl:lambda (v) (hydra_java_syntax_class_type-arguments v)) ct)) (id ((cl:lambda (v) (hydra_java_syntax_class_type-identifier v)) ct)) (qual ((cl:lambda (v) (hydra_java_syntax_class_type-qualifier v)) ct))) (list :right (list :reference (list :class_or_interface (list :class (make-hydra_java_syntax_class_type anns qual id ((hydra_lib_lists_concat2 args) (cl:list (list :reference rt)))))))))) match_value)) ((equal (car match_target) :interface) ((cl:lambda (_) (list :left (list :other "expected a Java class type"))) match_value)))) (cadr match_target))) cit)) match_value)) ((equal (car match_target) :variable) ((cl:lambda (tv) (list :right (hydra_java_utils_java_type_variable_to_type tv))) match_value)) ((equal (car match_target) :array) ((cl:lambda (_) (list :left (list :other "expected a Java class or interface type, or a variable"))) match_value)))) (cadr match_target))) rt1)) match_value)) ((equal (car match_target) :primitive) ((cl:lambda (_) (list :left (list :other "expected a reference type"))) match_value)))) (cadr match_target))) t_)))))

(cl:defvar hydra_java_utils_add_var_rename (cl:lambda (original) (cl:lambda (renamed) (cl:lambda (aliases) (make-hydra_java_environment_aliases ((cl:lambda (v) (hydra_java_environment_aliases-current_namespace v)) aliases) ((cl:lambda (v) (hydra_java_environment_aliases-packages v)) aliases) ((cl:lambda (v) (hydra_java_environment_aliases-branch_vars v)) aliases) ((cl:lambda (v) (hydra_java_environment_aliases-recursive_vars v)) aliases) ((cl:lambda (v) (hydra_java_environment_aliases-in_scope_type_params v)) aliases) ((cl:lambda (v) (hydra_java_environment_aliases-polymorphic_locals v)) aliases) ((cl:lambda (v) (hydra_java_environment_aliases-in_scope_java_vars v)) aliases) (((hydra_lib_maps_insert original) renamed) ((cl:lambda (v) (hydra_java_environment_aliases-var_renames v)) aliases)) ((cl:lambda (v) (hydra_java_environment_aliases-lambda_vars v)) aliases) ((cl:lambda (v) (hydra_java_environment_aliases-type_var_subst v)) aliases) ((cl:lambda (v) (hydra_java_environment_aliases-trusted_type_vars v)) aliases) ((cl:lambda (v) (hydra_java_environment_aliases-method_codomain v)) aliases) ((cl:lambda (v) (hydra_java_environment_aliases-thunked_vars v)) aliases))))))

(cl:defvar hydra_java_utils_field_expression (cl:lambda (var_id) (cl:lambda (field_id) (make-hydra_java_syntax_expression_name (list :just (cl:list var_id)) field_id))))

(cl:defvar hydra_java_utils_is_escaped (cl:lambda (s) ((hydra_lib_equality_equal ((hydra_lib_strings_char_at 0) s)) 36)))

(cl:defvar hydra_java_utils_unescape (cl:lambda (s) (hydra_lib_strings_from_list (hydra_lib_lists_tail (hydra_lib_strings_to_list s)))))

(cl:defvar hydra_java_utils_sanitize_java_name (cl:lambda (name) (if (hydra_java_utils_is_escaped name) (hydra_java_utils_unescape name) (if ((hydra_lib_equality_equal name) "_") "ignored" ((hydra_formatting_sanitize_with_underscores hydra_java_language_reserved_words) name)))))

(cl:defvar hydra_java_utils_java_identifier (cl:lambda (s) (hydra_java_utils_sanitize_java_name s)))

(cl:defvar hydra_java_utils_field_name_to_java_identifier (cl:lambda (fname) (hydra_java_utils_java_identifier ((cl:lambda (v) v) fname))))

(cl:defvar hydra_java_utils_java_identifier_to_java_expression_name (cl:lambda (id) (make-hydra_java_syntax_expression_name (list :nothing) id)))

(cl:defvar hydra_java_utils_field_name_to_java_expression (cl:lambda (fname) (list :assignment (list :conditional (list :simple (cl:list (cl:list (cl:list (cl:list (cl:list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :name (hydra_java_utils_java_identifier_to_java_expression_name (hydra_java_utils_field_name_to_java_identifier fname))))))))))))))))))))

(cl:defvar hydra_java_utils_java_variable_declarator_id (cl:lambda (id) (make-hydra_java_syntax_variable_declarator_id id (list :nothing))))

(cl:defvar hydra_java_utils_java_variable_declarator (cl:lambda (id) (cl:lambda (minit) (make-hydra_java_syntax_variable_declarator (hydra_java_utils_java_variable_declarator_id id) minit))))

(cl:defvar hydra_java_utils_field_name_to_java_variable_declarator (cl:lambda (fname) ((hydra_java_utils_java_variable_declarator (hydra_java_utils_java_identifier ((cl:lambda (v) v) fname))) (list :nothing))))

(cl:defvar hydra_java_utils_field_name_to_java_variable_declarator_id (cl:lambda (fname) (hydra_java_utils_java_variable_declarator_id (hydra_java_utils_java_identifier ((cl:lambda (v) v) fname)))))

(cl:defvar hydra_java_utils_final_var_declaration_statement (cl:lambda (id) (cl:lambda (rhs) (list :local_variable_declaration (make-hydra_java_syntax_local_variable_declaration (cl:list (list :final cl:nil)) (list :var cl:nil) (cl:list ((hydra_java_utils_java_variable_declarator id) (list :just (list :expression rhs)))))))))

(cl:defvar hydra_java_utils_import_aliases_for_module (cl:lambda (mod) (make-hydra_java_environment_aliases ((cl:lambda (v) (hydra_packaging_module-namespace v)) mod) hydra_lib_maps_empty hydra_lib_sets_empty hydra_lib_sets_empty hydra_lib_sets_empty hydra_lib_sets_empty hydra_lib_sets_empty hydra_lib_maps_empty hydra_lib_sets_empty hydra_lib_maps_empty hydra_lib_sets_empty (list :nothing) hydra_lib_sets_empty)))

(cl:defvar hydra_java_utils_java_method_body (cl:lambda (mstmts) (((hydra_lib_maybes_cases mstmts) (cl:lambda () (list :none cl:nil))) (cl:lambda (stmts) (list :block stmts)))))

(cl:defvar hydra_java_utils_java_method_header (cl:lambda (tparams) (cl:lambda (method_name) (cl:lambda (params) (cl:lambda (result) (make-hydra_java_syntax_method_header tparams result (make-hydra_java_syntax_method_declarator method_name (list :nothing) params) (list :nothing)))))))

(cl:defvar hydra_java_utils_interface_method_declaration (cl:lambda (mods) (cl:lambda (tparams) (cl:lambda (method_name) (cl:lambda (params) (cl:lambda (result) (cl:lambda (stmts) (list :interface_method (make-hydra_java_syntax_interface_method_declaration mods ((((hydra_java_utils_java_method_header tparams) method_name) params) result) (hydra_java_utils_java_method_body stmts))))))))))

(cl:defvar hydra_java_utils_java_additive_expression_to_java_expression (cl:lambda (ae) (list :assignment (list :conditional (list :simple (cl:list (cl:list (cl:list (cl:list (cl:list (list :unary (list :simple (list :unary ae)))))))))))))

(cl:defvar hydra_java_utils_java_primary_to_java_expression (cl:lambda (p) (list :assignment (list :conditional (list :simple (cl:list (cl:list (cl:list (cl:list (cl:list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :primary p))))))))))))))))))

(cl:defvar hydra_java_utils_java_array_creation (cl:lambda (prim_type) (cl:lambda (minit) (let ((init_ (((hydra_lib_maybes_cases minit) (cl:lambda () (cl:list))) (cl:lambda (i) i)))) (hydra_java_utils_java_primary_to_java_expression (list :array_creation (list :primitive_array (make-hydra_java_syntax_array_creation_expression_primitive_array prim_type (cl:list) init_))))))))

(cl:defvar hydra_java_utils_java_array_initializer (cl:lambda (exprs) (cl:list ((hydra_lib_lists_map (cl:lambda (e) (list :expression e))) exprs))))

(cl:defvar hydra_java_utils_java_assignment_statement (cl:lambda (lhs) (cl:lambda (rhs) (list :without_trailing (list :expression (list :assignment (make-hydra_java_syntax_assignment lhs (list :simple cl:nil) rhs)))))))

(cl:defvar hydra_java_utils_java_boolean (cl:lambda (b) (list :boolean b)))

(cl:defvar hydra_java_utils_java_literal_to_java_primary (cl:lambda (lit) (list :no_new_array (list :literal lit))))

(cl:defvar hydra_java_utils_java_boolean_expression (cl:lambda (b) (hydra_java_utils_java_primary_to_java_expression (hydra_java_utils_java_literal_to_java_primary (hydra_java_utils_java_boolean b)))))

(cl:defvar hydra_java_utils_java_primitive_type_to_java_type (cl:lambda (pt) (list :primitive (make-hydra_java_syntax_primitive_type_with_annotations pt (cl:list)))))

(cl:defvar hydra_java_utils_java_boolean_type (hydra_java_utils_java_primitive_type_to_java_type (list :boolean cl:nil)))

(cl:defvar hydra_java_utils_java_byte_primitive_type (make-hydra_java_syntax_primitive_type_with_annotations (list :numeric (list :integral (list :byte cl:nil))) (cl:list)))

(cl:defvar hydra_java_utils_java_cast_expression (cl:lambda (rt) (cl:lambda (expr) (list :not_plus_minus (make-hydra_java_syntax_cast_expression_not_plus_minus (make-hydra_java_syntax_cast_expression_ref_and_bounds rt (cl:list)) expr)))))

(cl:defvar hydra_java_utils_java_cast_expression_to_java_expression (cl:lambda (ce) (list :assignment (list :conditional (list :simple (cl:list (cl:list (cl:list (cl:list (cl:list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :cast ce)))))))))))))))))

(cl:defvar hydra_java_utils_java_cast_primitive (cl:lambda (pt) (cl:lambda (expr) (list :primitive (make-hydra_java_syntax_cast_expression_primitive (make-hydra_java_syntax_primitive_type_with_annotations pt (cl:list)) expr)))))

(cl:defvar hydra_java_utils_java_variable_name (cl:lambda (name) (hydra_java_utils_java_identifier (hydra_names_local_name_of name))))

(cl:defvar hydra_java_utils_java_decl_name (cl:lambda (name) (hydra_java_utils_java_variable_name name)))

(cl:defvar hydra_java_utils_java_type_identifier (cl:lambda (s) s))

(cl:defvar hydra_java_utils_name_to_qualified_java_name (cl:lambda (aliases) (cl:lambda (qualify) (cl:lambda (name) (cl:lambda (mlocal) (let* ((qn (hydra_names_qualify_name name)) (ns_ ((cl:lambda (v) (hydra_packaging_qualified_name-namespace v)) qn)) (alias (((hydra_lib_maybes_cases ns_) (cl:lambda () (list :nothing))) (cl:lambda (n) (list :just (((hydra_lib_maybes_cases ((hydra_lib_maps_lookup n) ((cl:lambda (v) (hydra_java_environment_aliases-packages v)) aliases))) (cl:lambda () (hydra_java_names_java_package_name ((hydra_lib_strings_split_on ".") ((cl:lambda (v) v) n))))) (cl:lambda (id) id)))))) (local ((cl:lambda (v) (hydra_packaging_qualified_name-local v)) qn)) (jid (hydra_java_utils_java_type_identifier (((hydra_lib_maybes_cases mlocal) (cl:lambda () (hydra_java_utils_sanitize_java_name local))) (cl:lambda (l) ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 (hydra_java_utils_sanitize_java_name local)) ".")) (hydra_java_utils_sanitize_java_name l)))))) (pkg (if qualify (((hydra_lib_maybes_cases alias) (cl:lambda () (list :none cl:nil))) (cl:lambda (p) (list :package p))) (list :none cl:nil)))) (cl:list jid pkg)))))))

(cl:defvar hydra_java_utils_name_to_java_class_type (cl:lambda (aliases) (cl:lambda (qualify) (cl:lambda (args) (cl:lambda (name) (cl:lambda (mlocal) (let* ((result ((((hydra_java_utils_name_to_qualified_java_name aliases) qualify) name) mlocal)) (id (hydra_lib_pairs_first result)) (pkg (hydra_lib_pairs_second result))) (make-hydra_java_syntax_class_type (cl:list) pkg id args))))))))

(cl:defvar hydra_java_utils_java_class_declaration (cl:lambda (aliases) (cl:lambda (tparams) (cl:lambda (el_name) (cl:lambda (mods) (cl:lambda (supname) (cl:lambda (impls) (cl:lambda (body_decls) (let ((extends_ ((hydra_lib_maybes_map (cl:lambda (n) (((((hydra_java_utils_name_to_java_class_type aliases) cl:t) (cl:list)) n) (list :nothing)))) supname))) (list :normal (make-hydra_java_syntax_normal_class_declaration mods (hydra_java_utils_java_decl_name el_name) tparams extends_ impls body_decls)))))))))))

(cl:defvar hydra_java_utils_java_class_type (cl:lambda (args) (cl:lambda (pkg) (cl:lambda (id) (let* ((qual (((hydra_lib_maybes_cases pkg) (cl:lambda () (list :none cl:nil))) (cl:lambda (p) (list :package p)))) (targs ((hydra_lib_lists_map (cl:lambda (rt) (list :reference rt))) args))) (make-hydra_java_syntax_class_type (cl:list) qual (hydra_java_utils_java_type_identifier id) targs))))))

(cl:defvar hydra_java_utils_java_class_type_to_java_type (cl:lambda (ct) (list :reference (list :class_or_interface (list :class ct)))))

(cl:defvar hydra_java_utils_java_conditional_and_expression_to_java_expression (cl:lambda (cae) (list :assignment (list :conditional (list :simple (cl:list cae))))))

(cl:defvar hydra_java_utils_java_constructor_call (cl:lambda (ci) (cl:lambda (args) (cl:lambda (mbody) (list :assignment (list :conditional (list :simple (cl:list (cl:list (cl:list (cl:list (cl:list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :primary (list :no_new_array (list :class_instance (make-hydra_java_syntax_class_instance_creation_expression (list :nothing) (make-hydra_java_syntax_unqualified_class_instance_creation_expression (cl:list) ci args mbody))))))))))))))))))))))))

(cl:defvar hydra_java_utils_java_constructor_name (cl:lambda (id) (cl:lambda (targs) (make-hydra_java_syntax_class_or_interface_type_to_instantiate (cl:list (make-hydra_java_syntax_annotated_identifier (cl:list) id)) targs))))

(cl:defvar hydra_java_utils_java_expression_to_java_unary_expression (cl:lambda (e) (list :other (list :postfix (list :primary (list :no_new_array (list :parens e)))))))

(cl:defvar hydra_java_utils_java_double_cast_expression (cl:lambda (raw_rt) (cl:lambda (target_rt) (cl:lambda (expr) (let ((first_cast (hydra_java_utils_java_cast_expression_to_java_expression ((hydra_java_utils_java_cast_expression raw_rt) expr)))) ((hydra_java_utils_java_cast_expression target_rt) (hydra_java_utils_java_expression_to_java_unary_expression first_cast)))))))

(cl:defvar hydra_java_utils_java_double_cast_expression_to_java_expression (cl:lambda (raw_rt) (cl:lambda (target_rt) (cl:lambda (expr) (hydra_java_utils_java_cast_expression_to_java_expression (((hydra_java_utils_java_double_cast_expression raw_rt) target_rt) expr))))))

(cl:defvar hydra_java_utils_java_empty_statement (list :without_trailing (list :empty cl:nil)))

(cl:defvar hydra_java_utils_java_equality_expression_to_java_expression (cl:lambda (ee) (list :assignment (list :conditional (list :simple (cl:list (cl:list (cl:list (cl:list (cl:list ee))))))))))

(cl:defvar hydra_java_utils_java_equality_expression_to_java_inclusive_or_expression (cl:lambda (ee) (cl:list (cl:list (cl:list ee)))))

(cl:defvar hydra_java_utils_java_equals (cl:lambda (lhs) (cl:lambda (rhs) (list :equal (make-hydra_java_syntax_equality_expression_binary lhs rhs)))))

(cl:defvar hydra_java_utils_java_literal_to_java_relational_expression (cl:lambda (lit) (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :primary (list :no_new_array (list :literal lit)))))))))))

(cl:defvar hydra_java_utils_java_equals_null (cl:lambda (lhs) ((hydra_java_utils_java_equals lhs) (hydra_java_utils_java_literal_to_java_relational_expression (list :null cl:nil)))))

(cl:defvar hydra_java_utils_java_expression_name_to_java_expression (cl:lambda (en) (list :assignment (list :conditional (list :simple (cl:list (cl:list (cl:list (cl:list (cl:list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :name en))))))))))))))))))

(cl:defvar hydra_java_utils_java_expression_to_java_primary (cl:lambda (e) (let ((fallback (list :no_new_array (list :parens e)))) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :assignment) ((cl:lambda (ae) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :conditional) ((cl:lambda (ce) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :simple) ((cl:lambda (cor) (let ((cands ((cl:lambda (v) v) cor))) (if ((hydra_lib_equality_equal (hydra_lib_lists_length cands)) 1) (let ((iors ((cl:lambda (v) v) (hydra_lib_lists_head cands)))) (if ((hydra_lib_equality_equal (hydra_lib_lists_length iors)) 1) (let ((xors ((cl:lambda (v) v) (hydra_lib_lists_head iors)))) (if ((hydra_lib_equality_equal (hydra_lib_lists_length xors)) 1) (let ((ands ((cl:lambda (v) v) (hydra_lib_lists_head xors)))) (if ((hydra_lib_equality_equal (hydra_lib_lists_length ands)) 1) (let ((eqs ((cl:lambda (v) v) (hydra_lib_lists_head ands)))) (if ((hydra_lib_equality_equal (hydra_lib_lists_length eqs)) 1) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :unary) ((cl:lambda (rel) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :simple) ((cl:lambda (shift) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :unary) ((cl:lambda (add) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :unary) ((cl:lambda (mul) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :unary) ((cl:lambda (unary) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :other) ((cl:lambda (npm) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :postfix) ((cl:lambda (pf) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :primary) ((cl:lambda (p) p) match_value)) (t fallback))) (cadr match_target))) pf)) match_value)) (t fallback))) (cadr match_target))) npm)) match_value)) (t fallback))) (cadr match_target))) unary)) match_value)) (t fallback))) (cadr match_target))) mul)) match_value)) (t fallback))) (cadr match_target))) add)) match_value)) (t fallback))) (cadr match_target))) shift)) match_value)) (t fallback))) (cadr match_target))) rel)) match_value)) (t fallback))) (cadr match_target))) (hydra_lib_lists_head eqs)) fallback)) fallback)) fallback)) fallback)) fallback))) match_value)) (t fallback))) (cadr match_target))) ce)) match_value)) (t fallback))) (cadr match_target))) ae)) match_value)) (t fallback))) (cadr match_target))) e))))

(cl:defvar hydra_java_utils_java_field_access_to_java_expression (cl:lambda (fa) (list :assignment (list :conditional (list :simple (cl:list (cl:list (cl:list (cl:list (cl:list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :primary (list :no_new_array (list :field_access fa))))))))))))))))))))

(cl:defvar hydra_java_utils_java_identifier_to_java_expression (cl:lambda (id) (list :assignment (list :conditional (list :simple (cl:list (cl:list (cl:list (cl:list (cl:list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :name (make-hydra_java_syntax_expression_name (list :nothing) id)))))))))))))))))))

(cl:defvar hydra_java_utils_java_identifier_to_java_relational_expression (cl:lambda (id) (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :name (make-hydra_java_syntax_expression_name (list :nothing) id))))))))))

(cl:defvar hydra_java_utils_java_identifier_to_java_unary_expression (cl:lambda (id) (list :other (list :postfix (list :name (make-hydra_java_syntax_expression_name (list :nothing) id))))))

(cl:defvar hydra_java_utils_java_instance_of (cl:lambda (lhs) (cl:lambda (rhs) (list :instanceof (make-hydra_java_syntax_relational_expression_instance_of lhs rhs)))))

(cl:defvar hydra_java_utils_java_int (cl:lambda (i) (list :integer i)))

(cl:defvar hydra_java_utils_java_int_expression (cl:lambda (i) (hydra_java_utils_java_primary_to_java_expression (hydra_java_utils_java_literal_to_java_primary (hydra_java_utils_java_int i)))))

(cl:defvar hydra_java_utils_java_int_type (hydra_java_utils_java_primitive_type_to_java_type (list :numeric (list :integral (list :int cl:nil)))))

(cl:defvar hydra_java_utils_java_interface_declaration_to_java_class_body_declaration (cl:lambda (nid) (list :class_member (list :interface (list :normal_interface nid)))))

(cl:defvar hydra_java_utils_variable_to_java_identifier (cl:lambda (name) (let ((v ((cl:lambda (v) v) name))) (if ((hydra_lib_equality_equal v) "_") "ignored" (hydra_java_utils_sanitize_java_name v)))))

(cl:defvar hydra_java_utils_java_lambda (cl:lambda (v) (cl:lambda (body) (list :lambda (make-hydra_java_syntax_lambda_expression (list :single (hydra_java_utils_variable_to_java_identifier v)) (list :expression body))))))

(cl:defvar hydra_java_utils_java_lambda_from_block (cl:lambda (v) (cl:lambda (block_) (list :lambda (make-hydra_java_syntax_lambda_expression (list :single (hydra_java_utils_variable_to_java_identifier v)) (list :block block_))))))

(cl:defvar hydra_java_utils_java_literal_to_java_expression (cl:lambda (lit) (list :assignment (list :conditional (list :simple (cl:list (cl:list (cl:list (cl:list (cl:list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :primary (list :no_new_array (list :literal lit))))))))))))))))))))

(cl:defvar hydra_java_utils_java_literal_to_java_multiplicative_expression (cl:lambda (lit) (list :unary (list :other (list :postfix (list :primary (list :no_new_array (list :literal lit))))))))

(cl:defvar hydra_java_utils_java_member_field (cl:lambda (mods) (cl:lambda (jt) (cl:lambda (v) (list :class_member (list :field (make-hydra_java_syntax_field_declaration mods jt (cl:list v))))))))

(cl:defvar hydra_java_utils_java_method_declaration_to_java_class_body_declaration (cl:lambda (md) (list :class_member (list :method md))))

(cl:defvar hydra_java_utils_java_method_invocation_to_java_expression (cl:lambda (mi) (list :assignment (list :conditional (list :simple (cl:list (cl:list (cl:list (cl:list (cl:list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :primary (list :no_new_array (list :method_invocation mi))))))))))))))))))))

(cl:defvar hydra_java_utils_java_method_invocation_to_java_postfix_expression (cl:lambda (mi) (list :primary (list :no_new_array (list :method_invocation mi)))))

(cl:defvar hydra_java_utils_java_method_invocation_to_java_primary (cl:lambda (mi) (list :no_new_array (list :method_invocation mi))))

(cl:defvar hydra_java_utils_java_method_invocation_to_java_statement (cl:lambda (mi) (list :without_trailing (list :expression (list :method_invocation mi)))))

(cl:defvar hydra_java_utils_java_multiplicative_expression_to_java_relational_expression (cl:lambda (me) (list :simple (list :unary (list :unary me)))))

(cl:defvar hydra_java_utils_java_package_declaration (cl:lambda (ns_) (make-hydra_java_syntax_package_declaration (cl:list) ((hydra_lib_lists_map (cl:lambda (s) s)) ((hydra_lib_strings_split_on ".") ((cl:lambda (v) v) ns_))))))

(cl:defvar hydra_java_utils_java_postfix_expression_to_java_equality_expression (cl:lambda (pe) (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix pe)))))))))

(cl:defvar hydra_java_utils_java_postfix_expression_to_java_expression (cl:lambda (pe) (list :assignment (list :conditional (list :simple (cl:list (cl:list (cl:list (cl:list (cl:list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix pe)))))))))))))))))

(cl:defvar hydra_java_utils_java_postfix_expression_to_java_inclusive_or_expression (cl:lambda (pe) (cl:list (cl:list (cl:list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix pe))))))))))))

(cl:defvar hydra_java_utils_java_postfix_expression_to_java_relational_expression (cl:lambda (pe) (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix pe))))))))

(cl:defvar hydra_java_utils_java_postfix_expression_to_java_unary_expression (cl:lambda (pe) (list :other (list :postfix pe))))

(cl:defvar hydra_java_utils_java_primary_to_java_unary_expression (cl:lambda (p) (list :other (list :postfix (list :primary p)))))

(cl:defvar hydra_java_utils_java_ref_type (cl:lambda (args) (cl:lambda (pkg) (cl:lambda (id) (list :reference (list :class_or_interface (list :class (((hydra_java_utils_java_class_type args) pkg) id))))))))

(cl:defvar hydra_java_utils_java_reference_type_to_raw_type (cl:lambda (rt) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :class_or_interface) ((cl:lambda (cit) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :class) ((cl:lambda (ct) (let* ((anns ((cl:lambda (v) (hydra_java_syntax_class_type-annotations v)) ct)) (id ((cl:lambda (v) (hydra_java_syntax_class_type-identifier v)) ct)) (qual ((cl:lambda (v) (hydra_java_syntax_class_type-qualifier v)) ct))) (list :class_or_interface (list :class (make-hydra_java_syntax_class_type anns qual id (cl:list)))))) match_value)) ((equal (car match_target) :interface) ((cl:lambda (it) (let* ((ct ((cl:lambda (v) v) it)) (anns ((cl:lambda (v) (hydra_java_syntax_class_type-annotations v)) ct)) (id ((cl:lambda (v) (hydra_java_syntax_class_type-identifier v)) ct)) (qual ((cl:lambda (v) (hydra_java_syntax_class_type-qualifier v)) ct))) (list :class_or_interface (list :interface (make-hydra_java_syntax_class_type anns qual id (cl:list)))))) match_value)))) (cadr match_target))) cit)) match_value)) (t rt))) (cadr match_target))) rt)))

(cl:defvar hydra_java_utils_java_relational_expression_to_java_equality_expression (cl:lambda (re) (list :unary re)))

(cl:defvar hydra_java_utils_java_relational_expression_to_java_expression (cl:lambda (re) (hydra_java_utils_java_equality_expression_to_java_expression (list :unary re))))

(cl:defvar hydra_java_utils_java_relational_expression_to_java_unary_expression (cl:lambda (re) (list :other (list :postfix (list :primary (list :no_new_array (list :parens (list :assignment (list :conditional (list :simple (cl:list (cl:list (cl:list (cl:list (cl:list (list :unary re))))))))))))))))

(cl:defvar hydra_java_utils_java_return_statement (cl:lambda (mex) (list :without_trailing (list :return mex))))

(cl:defvar hydra_java_utils_java_statements_to_block (cl:lambda (stmts) ((hydra_lib_lists_map (cl:lambda (s) (list :statement s))) stmts)))

(cl:defvar hydra_java_utils_java_string (cl:lambda (s) (list :string s)))

(cl:defvar hydra_java_utils_java_string_multiplicative_expression (cl:lambda (s) (hydra_java_utils_java_literal_to_java_multiplicative_expression (hydra_java_utils_java_string s))))

(cl:defvar hydra_java_utils_java_this (list :assignment (list :conditional (list :simple (cl:list (cl:list (cl:list (cl:list (cl:list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :primary (list :no_new_array (list :this cl:nil)))))))))))))))))))

(cl:defvar hydra_java_utils_java_throw_statement (cl:lambda (e) (list :without_trailing (list :throw e))))

(cl:defvar hydra_java_utils_java_throw_illegal_argument_exception (cl:lambda (args) (hydra_java_utils_java_throw_statement (((hydra_java_utils_java_constructor_call ((hydra_java_utils_java_constructor_name "IllegalArgumentException") (list :nothing))) args) (list :nothing)))))

(cl:defvar hydra_java_utils_java_throw_illegal_state_exception (cl:lambda (args) (hydra_java_utils_java_throw_statement (((hydra_java_utils_java_constructor_call ((hydra_java_utils_java_constructor_name "IllegalStateException") (list :nothing))) args) (list :nothing)))))

(cl:defvar hydra_java_utils_name_to_java_type_identifier (cl:lambda (aliases) (cl:lambda (qualify) (cl:lambda (name) (hydra_lib_pairs_first ((((hydra_java_utils_name_to_qualified_java_name aliases) qualify) name) (list :nothing)))))))

(cl:defvar hydra_java_utils_java_type_from_type_name (cl:lambda (aliases) (cl:lambda (el_name) (hydra_java_utils_java_type_variable_to_type (make-hydra_java_syntax_type_variable (cl:list) (((hydra_java_utils_name_to_java_type_identifier aliases) cl:nil) el_name))))))

(cl:defvar hydra_java_utils_java_type_identifier_to_java_type_argument (cl:lambda (id) (list :reference (list :variable (make-hydra_java_syntax_type_variable (cl:list) id)))))

(cl:defvar hydra_java_utils_java_type_name (cl:lambda (id) (make-hydra_java_syntax_type_name id (list :nothing))))

(cl:defvar hydra_java_utils_java_type_parameter (cl:lambda (v) (make-hydra_java_syntax_type_parameter (cl:list) (hydra_java_utils_java_type_identifier v) (list :nothing))))

(cl:defvar hydra_java_utils_java_type_to_java_formal_parameter (cl:lambda (jt) (cl:lambda (fname) (list :simple (make-hydra_java_syntax_formal_parameter_simple (cl:list) jt (hydra_java_utils_field_name_to_java_variable_declarator_id fname))))))

(cl:defvar hydra_java_utils_java_type_to_java_reference_type (cl:lambda (t_) (cl:lambda (cx) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :reference) ((cl:lambda (rt) (list :right rt)) match_value)) ((equal (car match_target) :primitive) ((cl:lambda (_) (list :left (list :other "expected a Java reference type"))) match_value)))) (cadr match_target))) t_))))

(cl:defvar hydra_java_utils_java_type_to_java_result (cl:lambda (jt) (list :type jt)))

(cl:defvar hydra_java_utils_java_type_to_java_type_argument (cl:lambda (t_) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :reference) ((cl:lambda (rt) (list :reference rt)) match_value)) ((equal (car match_target) :primitive) ((cl:lambda (_) (list :wildcard (make-hydra_java_syntax_wildcard (cl:list) (list :nothing)))) match_value)))) (cadr match_target))) t_)))

(cl:defvar hydra_java_utils_java_type_variable (cl:lambda (v) (list :variable (make-hydra_java_syntax_type_variable (cl:list) (hydra_java_utils_java_type_identifier (hydra_formatting_capitalize v))))))

(cl:defvar hydra_java_utils_java_unary_expression_to_java_expression (cl:lambda (ue) (list :assignment (list :conditional (list :simple (cl:list (cl:list (cl:list (cl:list (cl:list (list :unary (list :simple (list :unary (list :unary (list :unary ue)))))))))))))))

(cl:defvar hydra_java_utils_java_unary_expression_to_java_relational_expression (cl:lambda (ue) (list :simple (list :unary (list :unary (list :unary ue))))))

(cl:defvar hydra_java_utils_lookup_java_var_name (cl:lambda (aliases) (cl:lambda (name) (((hydra_lib_maybes_cases ((hydra_lib_maps_lookup name) ((cl:lambda (v) (hydra_java_environment_aliases-var_renames v)) aliases))) (cl:lambda () name)) (cl:lambda (renamed) renamed)))))

(cl:defvar hydra_java_utils_make_constructor (cl:lambda (aliases) (cl:lambda (el_name) (cl:lambda (private) (cl:lambda (params) (cl:lambda (stmts) (let* ((body (make-hydra_java_syntax_constructor_body (list :nothing) stmts)) (nm (((hydra_java_utils_name_to_java_type_identifier aliases) cl:nil) el_name)) (cons (make-hydra_java_syntax_constructor_declarator (cl:list) nm (list :nothing) params)) (mods (cl:list (if private (list :private cl:nil) (list :public cl:nil))))) (list :constructor_declaration (make-hydra_java_syntax_constructor_declaration mods cons (list :nothing) body)))))))))

(cl:defvar hydra_java_utils_method_declaration (cl:lambda (mods) (cl:lambda (tparams) (cl:lambda (anns) (cl:lambda (method_name) (cl:lambda (params) (cl:lambda (result) (cl:lambda (stmts) (hydra_java_utils_java_method_declaration_to_java_class_body_declaration (make-hydra_java_syntax_method_declaration anns mods ((((hydra_java_utils_java_method_header tparams) method_name) params) result) (hydra_java_utils_java_method_body stmts)))))))))))

(cl:defvar hydra_java_utils_method_invocation (cl:lambda (lhs) (cl:lambda (method_name) (cl:lambda (args) (let ((header (((hydra_lib_maybes_cases lhs) (cl:lambda () (list :simple method_name))) (cl:lambda (either) (list :complex (make-hydra_java_syntax_method_invocation_complex (((hydra_lib_eithers_either (cl:lambda (en) (list :expression en))) (cl:lambda (p) (list :primary p))) either) (cl:list) method_name)))))) (make-hydra_java_syntax_method_invocation header args))))))

(cl:defvar hydra_java_utils_method_invocation_static (cl:lambda (self) (cl:lambda (method_name) (cl:lambda (args) (((hydra_java_utils_method_invocation (list :just (list :left (hydra_java_utils_java_identifier_to_java_expression_name self)))) method_name) args)))))

(cl:defvar hydra_java_utils_method_invocation_static_with_type_args (cl:lambda (self) (cl:lambda (method_name) (cl:lambda (targs) (cl:lambda (args) (let ((header (list :complex (make-hydra_java_syntax_method_invocation_complex (list :expression (hydra_java_utils_java_identifier_to_java_expression_name self)) targs method_name)))) (make-hydra_java_syntax_method_invocation header args)))))))

(cl:defvar hydra_java_utils_name_to_java_name (cl:lambda (aliases) (cl:lambda (name) (let* ((qn (hydra_names_qualify_name name)) (local ((cl:lambda (v) (hydra_packaging_qualified_name-local v)) qn)) (ns_ ((cl:lambda (v) (hydra_packaging_qualified_name-namespace v)) qn))) (if (hydra_java_utils_is_escaped ((cl:lambda (v) v) name)) (hydra_java_utils_sanitize_java_name local) (((hydra_lib_maybes_cases ns_) (cl:lambda () local)) (cl:lambda (gname) (let* ((parts (((hydra_lib_maybes_cases ((hydra_lib_maps_lookup gname) ((cl:lambda (v) (hydra_java_environment_aliases-packages v)) aliases))) (cl:lambda () ((hydra_lib_strings_split_on ".") ((cl:lambda (v) v) gname)))) (cl:lambda (pkg_name) ((hydra_lib_lists_map (cl:lambda (i) ((cl:lambda (v) v) i))) ((cl:lambda (v) v) pkg_name))))) (all_parts ((hydra_lib_lists_concat2 parts) (cl:list (hydra_java_utils_sanitize_java_name local))))) ((hydra_lib_strings_intercalate ".") all_parts)))))))))

(cl:defvar hydra_java_utils_name_to_java_reference_type (cl:lambda (aliases) (cl:lambda (qualify) (cl:lambda (args) (cl:lambda (name) (cl:lambda (mlocal) (list :class_or_interface (list :class (((((hydra_java_utils_name_to_java_class_type aliases) qualify) args) name) mlocal)))))))))

(cl:defvar hydra_java_utils_override_annotation (list :marker (hydra_java_utils_java_type_name "Override")))

(cl:defvar hydra_java_utils_reference_type_to_result (cl:lambda (rt) (hydra_java_utils_java_type_to_java_result (list :reference rt))))

(cl:defvar hydra_java_utils_suppress_warnings_unchecked_annotation (list :single_element (make-hydra_java_syntax_single_element_annotation (hydra_java_utils_java_type_name "SuppressWarnings") (list :just (list :conditional_expression (list :simple (cl:list (cl:list (hydra_java_utils_java_postfix_expression_to_java_inclusive_or_expression (list :primary (hydra_java_utils_java_literal_to_java_primary (hydra_java_utils_java_string "unchecked"))))))))))))

(cl:defvar hydra_java_utils_type_parameter_to_reference_type (cl:lambda (tp) (hydra_java_utils_java_type_variable ((cl:lambda (v) v) ((cl:lambda (v) v) ((cl:lambda (v) (hydra_java_syntax_type_parameter-identifier v)) tp))))))

(cl:defvar hydra_java_utils_visitor_type_variable (hydra_java_utils_java_type_variable "r"))

(cl:defvar hydra_java_utils_to_accept_method (cl:lambda (abstract) (cl:lambda (vtparams) (let* ((anns (if abstract (cl:list) (cl:list hydra_java_utils_override_annotation))) (return_expr (hydra_java_utils_java_method_invocation_to_java_expression (((hydra_java_utils_method_invocation_static "visitor") hydra_java_names_visit_method_name) (cl:list hydra_java_utils_java_this)))) (body (if abstract (list :nothing) (list :just (cl:list (list :statement (hydra_java_utils_java_return_statement (list :just return_expr))))))) (mods (if abstract (cl:list (list :public cl:nil) (list :abstract cl:nil)) (cl:list (list :public cl:nil)))) (type_args ((hydra_lib_lists_map (cl:lambda (tp) (list :reference (hydra_java_utils_type_parameter_to_reference_type tp)))) vtparams)) (ref (hydra_java_utils_java_class_type_to_java_type (make-hydra_java_syntax_class_type (cl:list) (list :none cl:nil) (hydra_java_utils_java_type_identifier hydra_java_names_visitor_name) ((hydra_lib_lists_concat2 type_args) (cl:list (list :reference hydra_java_utils_visitor_type_variable)))))) (param ((hydra_java_utils_java_type_to_java_formal_parameter ref) "visitor")) (result (hydra_java_utils_java_type_to_java_result (list :reference hydra_java_utils_visitor_type_variable))) (tparams (cl:list (hydra_java_utils_java_type_parameter hydra_java_names_visitor_return_parameter)))) (((((((hydra_java_utils_method_declaration mods) tparams) anns) hydra_java_names_accept_method_name) (cl:list param)) result) body)))))

(cl:defvar hydra_java_utils_to_assign_stmt (cl:lambda (fname) (let* ((id (hydra_java_utils_field_name_to_java_identifier fname)) (lhs (list :field_access (make-hydra_java_syntax_field_access (list :primary (list :no_new_array (list :this cl:nil))) id))) (rhs (hydra_java_utils_field_name_to_java_expression fname))) ((hydra_java_utils_java_assignment_statement lhs) rhs))))

(cl:defvar hydra_java_utils_to_java_array_type (cl:lambda (t_) (cl:lambda (cx) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :reference) ((cl:lambda (rt) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :class_or_interface) ((cl:lambda (cit) (list :right (list :reference (list :array (make-hydra_java_syntax_array_type (cl:list (cl:list)) (list :class_or_interface cit)))))) match_value)) ((equal (car match_target) :array) ((cl:lambda (at) (let* ((old_dims ((cl:lambda (v) v) ((cl:lambda (v) (hydra_java_syntax_array_type-dims v)) at))) (new_dims ((hydra_lib_lists_concat2 old_dims) (cl:list (cl:list)))) (variant ((cl:lambda (v) (hydra_java_syntax_array_type-variant v)) at))) (list :right (list :reference (list :array (make-hydra_java_syntax_array_type new_dims variant)))))) match_value)) ((equal (car match_target) :variable) ((cl:lambda (_) (list :left (list :other "don't know how to make Java reference type into array type"))) match_value)))) (cadr match_target))) rt)) match_value)) ((equal (car match_target) :primitive) ((cl:lambda (_) (list :left (list :other "don't know how to make Java type into array type"))) match_value)))) (cadr match_target))) t_))))

(cl:defvar hydra_java_utils_type_parameter_to_type_argument (cl:lambda (tp) (hydra_java_utils_java_type_identifier_to_java_type_argument ((cl:lambda (v) (hydra_java_syntax_type_parameter-identifier v)) tp))))

(cl:defvar hydra_java_utils_un_type_parameter (cl:lambda (tp) ((cl:lambda (v) v) ((cl:lambda (v) v) ((cl:lambda (v) (hydra_java_syntax_type_parameter-identifier v)) tp)))))

(cl:defvar hydra_java_utils_unique_var_name_go (cl:lambda (aliases) (cl:lambda (base) (cl:lambda (n) (let ((candidate ((hydra_lib_strings_cat2 base) (hydra_lib_literals_show_int32 n)))) (if ((hydra_lib_sets_member candidate) ((cl:lambda (v) (hydra_java_environment_aliases-in_scope_java_vars v)) aliases)) (((hydra_java_utils_unique_var_name_go aliases) base) ((hydra_lib_math_add n) 1)) candidate))))))

(cl:defvar hydra_java_utils_unique_var_name (cl:lambda (aliases) (cl:lambda (name) (if ((hydra_lib_sets_member name) ((cl:lambda (v) (hydra_java_environment_aliases-in_scope_java_vars v)) aliases)) (((hydra_java_utils_unique_var_name_go aliases) ((cl:lambda (v) v) name)) 2) name))))

(cl:defvar hydra_java_utils_var_declaration_statement (cl:lambda (id) (cl:lambda (rhs) (list :local_variable_declaration (make-hydra_java_syntax_local_variable_declaration (cl:list) (list :var cl:nil) (cl:list ((hydra_java_utils_java_variable_declarator id) (list :just (list :expression rhs)))))))))

(cl:defvar hydra_java_utils_variable_declaration_statement (cl:lambda (aliases) (cl:lambda (jtype) (cl:lambda (id) (cl:lambda (rhs) (let* ((init_ (list :expression rhs)) (vdec ((hydra_java_utils_java_variable_declarator id) (list :just init_)))) (list :local_variable_declaration (make-hydra_java_syntax_local_variable_declaration (cl:list) (list :type jtype) (cl:list vdec)))))))))

(cl:defvar hydra_java_utils_variant_class_name (cl:lambda (qualify) (cl:lambda (el_name) (cl:lambda (fname) (let* ((flocal (hydra_formatting_capitalize ((cl:lambda (v) v) fname))) (qn (hydra_names_qualify_name el_name)) (local ((cl:lambda (v) (hydra_packaging_qualified_name-local v)) qn)) (local1 (if qualify ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 local) ".")) flocal) (if ((hydra_lib_equality_equal flocal) local) ((hydra_lib_strings_cat2 flocal) "_") flocal))) (ns_ ((cl:lambda (v) (hydra_packaging_qualified_name-namespace v)) qn))) (hydra_names_unqualify_name (make-hydra_packaging_qualified_name ns_ local1)))))))
