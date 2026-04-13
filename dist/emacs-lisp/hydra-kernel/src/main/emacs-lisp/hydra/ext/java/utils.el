(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.errors)

(require 'hydra.ext.java.environment)

(require 'hydra.ext.java.language)

(require 'hydra.ext.java.names)

(require 'hydra.ext.java.syntax)

(require 'hydra.formatting)

(require 'hydra.lib.eithers)

(require 'hydra.lib.equality)

(require 'hydra.lib.lists)

(require 'hydra.lib.literals)

(require 'hydra.lib.logic)

(require 'hydra.lib.maps)

(require 'hydra.lib.math)

(require 'hydra.lib.maybes)

(require 'hydra.lib.pairs)

(require 'hydra.lib.sets)

(require 'hydra.lib.strings)

(require 'hydra.names)

(require 'hydra.packaging)

(defvar hydra_ext_java_utils_add_expressions (lambda (exprs) (let* ((first (list :unary (hydra_lib_lists_head exprs))) (rest (hydra_lib_lists_tail exprs))) (funcall (funcall (hydra_lib_lists_foldl (lambda (ae) (lambda (me) (list :plus (make-hydra_ext_java_syntax_additive_expression_binary ae me))))) first) rest))))

(defvar hydra_ext_java_utils_add_in_scope_var (lambda (name) (lambda (aliases) (make-hydra_ext_java_environment_aliases (funcall (lambda (v) (hydra_ext_java_environment_aliases-current_namespace v)) aliases) (funcall (lambda (v) (hydra_ext_java_environment_aliases-packages v)) aliases) (funcall (lambda (v) (hydra_ext_java_environment_aliases-branch_vars v)) aliases) (funcall (lambda (v) (hydra_ext_java_environment_aliases-recursive_vars v)) aliases) (funcall (lambda (v) (hydra_ext_java_environment_aliases-in_scope_type_params v)) aliases) (funcall (lambda (v) (hydra_ext_java_environment_aliases-polymorphic_locals v)) aliases) (funcall (hydra_lib_sets_insert name) (funcall (lambda (v) (hydra_ext_java_environment_aliases-in_scope_java_vars v)) aliases)) (funcall (lambda (v) (hydra_ext_java_environment_aliases-var_renames v)) aliases) (funcall (lambda (v) (hydra_ext_java_environment_aliases-lambda_vars v)) aliases) (funcall (lambda (v) (hydra_ext_java_environment_aliases-type_var_subst v)) aliases) (funcall (lambda (v) (hydra_ext_java_environment_aliases-trusted_type_vars v)) aliases) (funcall (lambda (v) (hydra_ext_java_environment_aliases-method_codomain v)) aliases) (funcall (lambda (v) (hydra_ext_java_environment_aliases-thunked_vars v)) aliases)))))

(defvar hydra_ext_java_utils_add_in_scope_vars (lambda (names) (lambda (aliases) (funcall (funcall (hydra_lib_lists_foldl (lambda (a) (lambda (n) (funcall (hydra_ext_java_utils_add_in_scope_var n) a)))) aliases) names))))

(defvar hydra_ext_java_utils_java_type_variable_to_type (lambda (tv) (list :reference (list :variable tv))))

(defvar hydra_ext_java_utils_add_java_type_parameter (lambda (rt) (lambda (t_) (lambda (cx) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :reference) (funcall (lambda (rt1) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :class_or_interface) (funcall (lambda (cit) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :class) (funcall (lambda (ct) (let* ((anns (funcall (lambda (v) (hydra_ext_java_syntax_class_type-annotations v)) ct)) (args (funcall (lambda (v) (hydra_ext_java_syntax_class_type-arguments v)) ct)) (id (funcall (lambda (v) (hydra_ext_java_syntax_class_type-identifier v)) ct)) (qual (funcall (lambda (v) (hydra_ext_java_syntax_class_type-qualifier v)) ct))) (list :right (list :reference (list :class_or_interface (list :class (make-hydra_ext_java_syntax_class_type anns qual id (funcall (hydra_lib_lists_concat2 args) (list (list :reference rt)))))))))) match_value)) ((equal (car match_target) :interface) (funcall (lambda (_) (list :left (list :other "expected a Java class type"))) match_value)))) (cadr match_target))) cit)) match_value)) ((equal (car match_target) :variable) (funcall (lambda (tv) (list :right (hydra_ext_java_utils_java_type_variable_to_type tv))) match_value)) ((equal (car match_target) :array) (funcall (lambda (_) (list :left (list :other "expected a Java class or interface type, or a variable"))) match_value)))) (cadr match_target))) rt1)) match_value)) ((equal (car match_target) :primitive) (funcall (lambda (_) (list :left (list :other "expected a reference type"))) match_value)))) (cadr match_target))) t_)))))

(defvar hydra_ext_java_utils_add_var_rename (lambda (original) (lambda (renamed) (lambda (aliases) (make-hydra_ext_java_environment_aliases (funcall (lambda (v) (hydra_ext_java_environment_aliases-current_namespace v)) aliases) (funcall (lambda (v) (hydra_ext_java_environment_aliases-packages v)) aliases) (funcall (lambda (v) (hydra_ext_java_environment_aliases-branch_vars v)) aliases) (funcall (lambda (v) (hydra_ext_java_environment_aliases-recursive_vars v)) aliases) (funcall (lambda (v) (hydra_ext_java_environment_aliases-in_scope_type_params v)) aliases) (funcall (lambda (v) (hydra_ext_java_environment_aliases-polymorphic_locals v)) aliases) (funcall (lambda (v) (hydra_ext_java_environment_aliases-in_scope_java_vars v)) aliases) (funcall (funcall (hydra_lib_maps_insert original) renamed) (funcall (lambda (v) (hydra_ext_java_environment_aliases-var_renames v)) aliases)) (funcall (lambda (v) (hydra_ext_java_environment_aliases-lambda_vars v)) aliases) (funcall (lambda (v) (hydra_ext_java_environment_aliases-type_var_subst v)) aliases) (funcall (lambda (v) (hydra_ext_java_environment_aliases-trusted_type_vars v)) aliases) (funcall (lambda (v) (hydra_ext_java_environment_aliases-method_codomain v)) aliases) (funcall (lambda (v) (hydra_ext_java_environment_aliases-thunked_vars v)) aliases))))))

(defvar hydra_ext_java_utils_field_expression (lambda (var_id) (lambda (field_id) (make-hydra_ext_java_syntax_expression_name (list :just (list var_id)) field_id))))

(defvar hydra_ext_java_utils_is_escaped (lambda (s) (funcall (hydra_lib_equality_equal (funcall (hydra_lib_strings_char_at 0) s)) 36)))

(defvar hydra_ext_java_utils_unescape (lambda (s) (hydra_lib_strings_from_list (hydra_lib_lists_tail (hydra_lib_strings_to_list s)))))

(defvar hydra_ext_java_utils_sanitize_java_name (lambda (name) (if (hydra_ext_java_utils_is_escaped name) (hydra_ext_java_utils_unescape name) (if (funcall (hydra_lib_equality_equal name) "_") "ignored" (funcall (hydra_formatting_sanitize_with_underscores hydra_ext_java_language_reserved_words) name)))))

(defvar hydra_ext_java_utils_java_identifier (lambda (s) (hydra_ext_java_utils_sanitize_java_name s)))

(defvar hydra_ext_java_utils_field_name_to_java_identifier (lambda (fname) (hydra_ext_java_utils_java_identifier (funcall (lambda (v) v) fname))))

(defvar hydra_ext_java_utils_java_identifier_to_java_expression_name (lambda (id) (make-hydra_ext_java_syntax_expression_name (list :nothing) id)))

(defvar hydra_ext_java_utils_field_name_to_java_expression (lambda (fname) (list :assignment (list :conditional (list :simple (list (list (list (list (list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :name (hydra_ext_java_utils_java_identifier_to_java_expression_name (hydra_ext_java_utils_field_name_to_java_identifier fname))))))))))))))))))))

(defvar hydra_ext_java_utils_java_variable_declarator_id (lambda (id) (make-hydra_ext_java_syntax_variable_declarator_id id (list :nothing))))

(defvar hydra_ext_java_utils_java_variable_declarator (lambda (id) (lambda (minit) (make-hydra_ext_java_syntax_variable_declarator (hydra_ext_java_utils_java_variable_declarator_id id) minit))))

(defvar hydra_ext_java_utils_field_name_to_java_variable_declarator (lambda (fname) (funcall (hydra_ext_java_utils_java_variable_declarator (hydra_ext_java_utils_java_identifier (funcall (lambda (v) v) fname))) (list :nothing))))

(defvar hydra_ext_java_utils_field_name_to_java_variable_declarator_id (lambda (fname) (hydra_ext_java_utils_java_variable_declarator_id (hydra_ext_java_utils_java_identifier (funcall (lambda (v) v) fname)))))

(defvar hydra_ext_java_utils_final_var_declaration_statement (lambda (id) (lambda (rhs) (list :local_variable_declaration (make-hydra_ext_java_syntax_local_variable_declaration (list (list :final nil)) (list :var nil) (list (funcall (hydra_ext_java_utils_java_variable_declarator id) (list :just (list :expression rhs)))))))))

(defvar hydra_ext_java_utils_import_aliases_for_module (lambda (mod) (make-hydra_ext_java_environment_aliases (funcall (lambda (v) (hydra_packaging_module-namespace v)) mod) hydra_lib_maps_empty hydra_lib_sets_empty hydra_lib_sets_empty hydra_lib_sets_empty hydra_lib_sets_empty hydra_lib_sets_empty hydra_lib_maps_empty hydra_lib_sets_empty hydra_lib_maps_empty hydra_lib_sets_empty (list :nothing) hydra_lib_sets_empty)))

(defvar hydra_ext_java_utils_java_method_body (lambda (mstmts) (funcall (funcall (hydra_lib_maybes_cases mstmts) (lambda () (list :none nil))) (lambda (stmts) (list :block stmts)))))

(defvar hydra_ext_java_utils_java_method_header (lambda (tparams) (lambda (method_name) (lambda (params) (lambda (result) (make-hydra_ext_java_syntax_method_header tparams result (make-hydra_ext_java_syntax_method_declarator method_name (list :nothing) params) (list :nothing)))))))

(defvar hydra_ext_java_utils_interface_method_declaration (lambda (mods) (lambda (tparams) (lambda (method_name) (lambda (params) (lambda (result) (lambda (stmts) (list :interface_method (make-hydra_ext_java_syntax_interface_method_declaration mods (funcall (funcall (funcall (hydra_ext_java_utils_java_method_header tparams) method_name) params) result) (hydra_ext_java_utils_java_method_body stmts))))))))))

(defvar hydra_ext_java_utils_java_additive_expression_to_java_expression (lambda (ae) (list :assignment (list :conditional (list :simple (list (list (list (list (list (list :unary (list :simple (list :unary ae)))))))))))))

(defvar hydra_ext_java_utils_java_primary_to_java_expression (lambda (p) (list :assignment (list :conditional (list :simple (list (list (list (list (list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :primary p))))))))))))))))))

(defvar hydra_ext_java_utils_java_array_creation (lambda (prim_type) (lambda (minit) (let ((init_ (funcall (funcall (hydra_lib_maybes_cases minit) (lambda () (list))) (lambda (i) i)))) (hydra_ext_java_utils_java_primary_to_java_expression (list :array_creation (list :primitive_array (make-hydra_ext_java_syntax_array_creation_expression_primitive_array prim_type (list) init_))))))))

(defvar hydra_ext_java_utils_java_array_initializer (lambda (exprs) (list (funcall (hydra_lib_lists_map (lambda (e) (list :expression e))) exprs))))

(defvar hydra_ext_java_utils_java_assignment_statement (lambda (lhs) (lambda (rhs) (list :without_trailing (list :expression (list :assignment (make-hydra_ext_java_syntax_assignment lhs (list :simple nil) rhs)))))))

(defvar hydra_ext_java_utils_java_boolean (lambda (b) (list :boolean b)))

(defvar hydra_ext_java_utils_java_literal_to_java_primary (lambda (lit) (list :no_new_array (list :literal lit))))

(defvar hydra_ext_java_utils_java_boolean_expression (lambda (b) (hydra_ext_java_utils_java_primary_to_java_expression (hydra_ext_java_utils_java_literal_to_java_primary (hydra_ext_java_utils_java_boolean b)))))

(defvar hydra_ext_java_utils_java_primitive_type_to_java_type (lambda (pt) (list :primitive (make-hydra_ext_java_syntax_primitive_type_with_annotations pt (list)))))

(defvar hydra_ext_java_utils_java_boolean_type (hydra_ext_java_utils_java_primitive_type_to_java_type (list :boolean nil)))

(defvar hydra_ext_java_utils_java_byte_primitive_type (make-hydra_ext_java_syntax_primitive_type_with_annotations (list :numeric (list :integral (list :byte nil))) (list)))

(defvar hydra_ext_java_utils_java_cast_expression (lambda (rt) (lambda (expr) (list :not_plus_minus (make-hydra_ext_java_syntax_cast_expression_not_plus_minus (make-hydra_ext_java_syntax_cast_expression_ref_and_bounds rt (list)) expr)))))

(defvar hydra_ext_java_utils_java_cast_expression_to_java_expression (lambda (ce) (list :assignment (list :conditional (list :simple (list (list (list (list (list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :cast ce)))))))))))))))))

(defvar hydra_ext_java_utils_java_cast_primitive (lambda (pt) (lambda (expr) (list :primitive (make-hydra_ext_java_syntax_cast_expression_primitive (make-hydra_ext_java_syntax_primitive_type_with_annotations pt (list)) expr)))))

(defvar hydra_ext_java_utils_java_variable_name (lambda (name) (hydra_ext_java_utils_java_identifier (hydra_names_local_name_of name))))

(defvar hydra_ext_java_utils_java_decl_name (lambda (name) (hydra_ext_java_utils_java_variable_name name)))

(defvar hydra_ext_java_utils_java_type_identifier (lambda (s) s))

(defvar hydra_ext_java_utils_name_to_qualified_java_name (lambda (aliases) (lambda (qualify) (lambda (name) (lambda (mlocal) (let* ((qn (hydra_names_qualify_name name)) (ns_ (funcall (lambda (v) (hydra_packaging_qualified_name-namespace v)) qn)) (alias (funcall (funcall (hydra_lib_maybes_cases ns_) (lambda () (list :nothing))) (lambda (n) (list :just (funcall (funcall (hydra_lib_maybes_cases (funcall (hydra_lib_maps_lookup n) (funcall (lambda (v) (hydra_ext_java_environment_aliases-packages v)) aliases))) (lambda () (hydra_ext_java_names_java_package_name (funcall (hydra_lib_strings_split_on ".") (funcall (lambda (v) v) n))))) (lambda (id) id)))))) (local (funcall (lambda (v) (hydra_packaging_qualified_name-local v)) qn)) (jid (hydra_ext_java_utils_java_type_identifier (funcall (funcall (hydra_lib_maybes_cases mlocal) (lambda () (hydra_ext_java_utils_sanitize_java_name local))) (lambda (l) (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 (hydra_ext_java_utils_sanitize_java_name local)) ".")) (hydra_ext_java_utils_sanitize_java_name l)))))) (pkg (if qualify (funcall (funcall (hydra_lib_maybes_cases alias) (lambda () (list :none nil))) (lambda (p) (list :package p))) (list :none nil)))) (list jid pkg)))))))

(defvar hydra_ext_java_utils_name_to_java_class_type (lambda (aliases) (lambda (qualify) (lambda (args) (lambda (name) (lambda (mlocal) (let* ((result (funcall (funcall (funcall (hydra_ext_java_utils_name_to_qualified_java_name aliases) qualify) name) mlocal)) (id (hydra_lib_pairs_first result)) (pkg (hydra_lib_pairs_second result))) (make-hydra_ext_java_syntax_class_type (list) pkg id args))))))))

(defvar hydra_ext_java_utils_java_class_declaration (lambda (aliases) (lambda (tparams) (lambda (el_name) (lambda (mods) (lambda (supname) (lambda (impls) (lambda (body_decls) (let ((extends_ (funcall (hydra_lib_maybes_map (lambda (n) (funcall (funcall (funcall (funcall (hydra_ext_java_utils_name_to_java_class_type aliases) t) (list)) n) (list :nothing)))) supname))) (list :normal (make-hydra_ext_java_syntax_normal_class_declaration mods (hydra_ext_java_utils_java_decl_name el_name) tparams extends_ impls body_decls)))))))))))

(defvar hydra_ext_java_utils_java_class_type (lambda (args) (lambda (pkg) (lambda (id) (let* ((qual (funcall (funcall (hydra_lib_maybes_cases pkg) (lambda () (list :none nil))) (lambda (p) (list :package p)))) (targs (funcall (hydra_lib_lists_map (lambda (rt) (list :reference rt))) args))) (make-hydra_ext_java_syntax_class_type (list) qual (hydra_ext_java_utils_java_type_identifier id) targs))))))

(defvar hydra_ext_java_utils_java_class_type_to_java_type (lambda (ct) (list :reference (list :class_or_interface (list :class ct)))))

(defvar hydra_ext_java_utils_java_conditional_and_expression_to_java_expression (lambda (cae) (list :assignment (list :conditional (list :simple (list cae))))))

(defvar hydra_ext_java_utils_java_constructor_call (lambda (ci) (lambda (args) (lambda (mbody) (list :assignment (list :conditional (list :simple (list (list (list (list (list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :primary (list :no_new_array (list :class_instance (make-hydra_ext_java_syntax_class_instance_creation_expression (list :nothing) (make-hydra_ext_java_syntax_unqualified_class_instance_creation_expression (list) ci args mbody))))))))))))))))))))))))

(defvar hydra_ext_java_utils_java_constructor_name (lambda (id) (lambda (targs) (make-hydra_ext_java_syntax_class_or_interface_type_to_instantiate (list (make-hydra_ext_java_syntax_annotated_identifier (list) id)) targs))))

(defvar hydra_ext_java_utils_java_expression_to_java_unary_expression (lambda (e) (list :other (list :postfix (list :primary (list :no_new_array (list :parens e)))))))

(defvar hydra_ext_java_utils_java_double_cast_expression (lambda (raw_rt) (lambda (target_rt) (lambda (expr) (let ((first_cast (hydra_ext_java_utils_java_cast_expression_to_java_expression (funcall (hydra_ext_java_utils_java_cast_expression raw_rt) expr)))) (funcall (hydra_ext_java_utils_java_cast_expression target_rt) (hydra_ext_java_utils_java_expression_to_java_unary_expression first_cast)))))))

(defvar hydra_ext_java_utils_java_double_cast_expression_to_java_expression (lambda (raw_rt) (lambda (target_rt) (lambda (expr) (hydra_ext_java_utils_java_cast_expression_to_java_expression (funcall (funcall (hydra_ext_java_utils_java_double_cast_expression raw_rt) target_rt) expr))))))

(defvar hydra_ext_java_utils_java_empty_statement (list :without_trailing (list :empty nil)))

(defvar hydra_ext_java_utils_java_equality_expression_to_java_expression (lambda (ee) (list :assignment (list :conditional (list :simple (list (list (list (list (list ee))))))))))

(defvar hydra_ext_java_utils_java_equality_expression_to_java_inclusive_or_expression (lambda (ee) (list (list (list ee)))))

(defvar hydra_ext_java_utils_java_equals (lambda (lhs) (lambda (rhs) (list :equal (make-hydra_ext_java_syntax_equality_expression_binary lhs rhs)))))

(defvar hydra_ext_java_utils_java_literal_to_java_relational_expression (lambda (lit) (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :primary (list :no_new_array (list :literal lit)))))))))))

(defvar hydra_ext_java_utils_java_equals_null (lambda (lhs) (funcall (hydra_ext_java_utils_java_equals lhs) (hydra_ext_java_utils_java_literal_to_java_relational_expression (list :null nil)))))

(defvar hydra_ext_java_utils_java_expression_name_to_java_expression (lambda (en) (list :assignment (list :conditional (list :simple (list (list (list (list (list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :name en))))))))))))))))))

(defvar hydra_ext_java_utils_java_expression_to_java_primary (lambda (e) (let ((fallback (list :no_new_array (list :parens e)))) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :assignment) (funcall (lambda (ae) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :conditional) (funcall (lambda (ce) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :simple) (funcall (lambda (cor) (let ((cands (funcall (lambda (v) v) cor))) (if (funcall (hydra_lib_equality_equal (hydra_lib_lists_length cands)) 1) (let ((iors (funcall (lambda (v) v) (hydra_lib_lists_head cands)))) (if (funcall (hydra_lib_equality_equal (hydra_lib_lists_length iors)) 1) (let ((xors (funcall (lambda (v) v) (hydra_lib_lists_head iors)))) (if (funcall (hydra_lib_equality_equal (hydra_lib_lists_length xors)) 1) (let ((ands (funcall (lambda (v) v) (hydra_lib_lists_head xors)))) (if (funcall (hydra_lib_equality_equal (hydra_lib_lists_length ands)) 1) (let ((eqs (funcall (lambda (v) v) (hydra_lib_lists_head ands)))) (if (funcall (hydra_lib_equality_equal (hydra_lib_lists_length eqs)) 1) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :unary) (funcall (lambda (rel) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :simple) (funcall (lambda (shift) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :unary) (funcall (lambda (add) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :unary) (funcall (lambda (mul) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :unary) (funcall (lambda (unary) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :other) (funcall (lambda (npm) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :postfix) (funcall (lambda (pf) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :primary) (funcall (lambda (p) p) match_value)) (t fallback))) (cadr match_target))) pf)) match_value)) (t fallback))) (cadr match_target))) npm)) match_value)) (t fallback))) (cadr match_target))) unary)) match_value)) (t fallback))) (cadr match_target))) mul)) match_value)) (t fallback))) (cadr match_target))) add)) match_value)) (t fallback))) (cadr match_target))) shift)) match_value)) (t fallback))) (cadr match_target))) rel)) match_value)) (t fallback))) (cadr match_target))) (hydra_lib_lists_head eqs)) fallback)) fallback)) fallback)) fallback)) fallback))) match_value)) (t fallback))) (cadr match_target))) ce)) match_value)) (t fallback))) (cadr match_target))) ae)) match_value)) (t fallback))) (cadr match_target))) e))))

(defvar hydra_ext_java_utils_java_field_access_to_java_expression (lambda (fa) (list :assignment (list :conditional (list :simple (list (list (list (list (list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :primary (list :no_new_array (list :field_access fa))))))))))))))))))))

(defvar hydra_ext_java_utils_java_identifier_to_java_expression (lambda (id) (list :assignment (list :conditional (list :simple (list (list (list (list (list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :name (make-hydra_ext_java_syntax_expression_name (list :nothing) id)))))))))))))))))))

(defvar hydra_ext_java_utils_java_identifier_to_java_relational_expression (lambda (id) (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :name (make-hydra_ext_java_syntax_expression_name (list :nothing) id))))))))))

(defvar hydra_ext_java_utils_java_identifier_to_java_unary_expression (lambda (id) (list :other (list :postfix (list :name (make-hydra_ext_java_syntax_expression_name (list :nothing) id))))))

(defvar hydra_ext_java_utils_java_instance_of (lambda (lhs) (lambda (rhs) (list :instanceof (make-hydra_ext_java_syntax_relational_expression_instance_of lhs rhs)))))

(defvar hydra_ext_java_utils_java_int (lambda (i) (list :integer i)))

(defvar hydra_ext_java_utils_java_int_expression (lambda (i) (hydra_ext_java_utils_java_primary_to_java_expression (hydra_ext_java_utils_java_literal_to_java_primary (hydra_ext_java_utils_java_int i)))))

(defvar hydra_ext_java_utils_java_int_type (hydra_ext_java_utils_java_primitive_type_to_java_type (list :numeric (list :integral (list :int nil)))))

(defvar hydra_ext_java_utils_java_interface_declaration_to_java_class_body_declaration (lambda (nid) (list :class_member (list :interface (list :normal_interface nid)))))

(defvar hydra_ext_java_utils_variable_to_java_identifier (lambda (name) (let ((v (funcall (lambda (v) v) name))) (if (funcall (hydra_lib_equality_equal v) "_") "ignored" (hydra_ext_java_utils_sanitize_java_name v)))))

(defvar hydra_ext_java_utils_java_lambda (lambda (v) (lambda (body) (list :lambda (make-hydra_ext_java_syntax_lambda_expression (list :single (hydra_ext_java_utils_variable_to_java_identifier v)) (list :expression body))))))

(defvar hydra_ext_java_utils_java_lambda_from_block (lambda (v) (lambda (block_) (list :lambda (make-hydra_ext_java_syntax_lambda_expression (list :single (hydra_ext_java_utils_variable_to_java_identifier v)) (list :block block_))))))

(defvar hydra_ext_java_utils_java_literal_to_java_expression (lambda (lit) (list :assignment (list :conditional (list :simple (list (list (list (list (list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :primary (list :no_new_array (list :literal lit))))))))))))))))))))

(defvar hydra_ext_java_utils_java_literal_to_java_multiplicative_expression (lambda (lit) (list :unary (list :other (list :postfix (list :primary (list :no_new_array (list :literal lit))))))))

(defvar hydra_ext_java_utils_java_member_field (lambda (mods) (lambda (jt) (lambda (v) (list :class_member (list :field (make-hydra_ext_java_syntax_field_declaration mods jt (list v))))))))

(defvar hydra_ext_java_utils_java_method_declaration_to_java_class_body_declaration (lambda (md) (list :class_member (list :method md))))

(defvar hydra_ext_java_utils_java_method_invocation_to_java_expression (lambda (mi) (list :assignment (list :conditional (list :simple (list (list (list (list (list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :primary (list :no_new_array (list :method_invocation mi))))))))))))))))))))

(defvar hydra_ext_java_utils_java_method_invocation_to_java_postfix_expression (lambda (mi) (list :primary (list :no_new_array (list :method_invocation mi)))))

(defvar hydra_ext_java_utils_java_method_invocation_to_java_primary (lambda (mi) (list :no_new_array (list :method_invocation mi))))

(defvar hydra_ext_java_utils_java_method_invocation_to_java_statement (lambda (mi) (list :without_trailing (list :expression (list :method_invocation mi)))))

(defvar hydra_ext_java_utils_java_multiplicative_expression_to_java_relational_expression (lambda (me) (list :simple (list :unary (list :unary me)))))

(defvar hydra_ext_java_utils_java_package_declaration (lambda (ns_) (make-hydra_ext_java_syntax_package_declaration (list) (funcall (hydra_lib_lists_map (lambda (s) s)) (funcall (hydra_lib_strings_split_on ".") (funcall (lambda (v) v) ns_))))))

(defvar hydra_ext_java_utils_java_postfix_expression_to_java_equality_expression (lambda (pe) (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix pe)))))))))

(defvar hydra_ext_java_utils_java_postfix_expression_to_java_expression (lambda (pe) (list :assignment (list :conditional (list :simple (list (list (list (list (list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix pe)))))))))))))))))

(defvar hydra_ext_java_utils_java_postfix_expression_to_java_inclusive_or_expression (lambda (pe) (list (list (list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix pe))))))))))))

(defvar hydra_ext_java_utils_java_postfix_expression_to_java_relational_expression (lambda (pe) (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix pe))))))))

(defvar hydra_ext_java_utils_java_postfix_expression_to_java_unary_expression (lambda (pe) (list :other (list :postfix pe))))

(defvar hydra_ext_java_utils_java_primary_to_java_unary_expression (lambda (p) (list :other (list :postfix (list :primary p)))))

(defvar hydra_ext_java_utils_java_ref_type (lambda (args) (lambda (pkg) (lambda (id) (list :reference (list :class_or_interface (list :class (funcall (funcall (hydra_ext_java_utils_java_class_type args) pkg) id))))))))

(defvar hydra_ext_java_utils_java_reference_type_to_raw_type (lambda (rt) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :class_or_interface) (funcall (lambda (cit) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :class) (funcall (lambda (ct) (let* ((anns (funcall (lambda (v) (hydra_ext_java_syntax_class_type-annotations v)) ct)) (id (funcall (lambda (v) (hydra_ext_java_syntax_class_type-identifier v)) ct)) (qual (funcall (lambda (v) (hydra_ext_java_syntax_class_type-qualifier v)) ct))) (list :class_or_interface (list :class (make-hydra_ext_java_syntax_class_type anns qual id (list)))))) match_value)) ((equal (car match_target) :interface) (funcall (lambda (it) (let* ((ct (funcall (lambda (v) v) it)) (anns (funcall (lambda (v) (hydra_ext_java_syntax_class_type-annotations v)) ct)) (id (funcall (lambda (v) (hydra_ext_java_syntax_class_type-identifier v)) ct)) (qual (funcall (lambda (v) (hydra_ext_java_syntax_class_type-qualifier v)) ct))) (list :class_or_interface (list :interface (make-hydra_ext_java_syntax_class_type anns qual id (list)))))) match_value)))) (cadr match_target))) cit)) match_value)) (t rt))) (cadr match_target))) rt)))

(defvar hydra_ext_java_utils_java_relational_expression_to_java_equality_expression (lambda (re) (list :unary re)))

(defvar hydra_ext_java_utils_java_relational_expression_to_java_expression (lambda (re) (hydra_ext_java_utils_java_equality_expression_to_java_expression (list :unary re))))

(defvar hydra_ext_java_utils_java_relational_expression_to_java_unary_expression (lambda (re) (list :other (list :postfix (list :primary (list :no_new_array (list :parens (list :assignment (list :conditional (list :simple (list (list (list (list (list (list :unary re))))))))))))))))

(defvar hydra_ext_java_utils_java_return_statement (lambda (mex) (list :without_trailing (list :return mex))))

(defvar hydra_ext_java_utils_java_statements_to_block (lambda (stmts) (funcall (hydra_lib_lists_map (lambda (s) (list :statement s))) stmts)))

(defvar hydra_ext_java_utils_java_string (lambda (s) (list :string s)))

(defvar hydra_ext_java_utils_java_string_multiplicative_expression (lambda (s) (hydra_ext_java_utils_java_literal_to_java_multiplicative_expression (hydra_ext_java_utils_java_string s))))

(defvar hydra_ext_java_utils_java_this (list :assignment (list :conditional (list :simple (list (list (list (list (list (list :unary (list :simple (list :unary (list :unary (list :unary (list :other (list :postfix (list :primary (list :no_new_array (list :this nil)))))))))))))))))))

(defvar hydra_ext_java_utils_java_throw_statement (lambda (e) (list :without_trailing (list :throw e))))

(defvar hydra_ext_java_utils_java_throw_illegal_argument_exception (lambda (args) (hydra_ext_java_utils_java_throw_statement (funcall (funcall (hydra_ext_java_utils_java_constructor_call (funcall (hydra_ext_java_utils_java_constructor_name "IllegalArgumentException") (list :nothing))) args) (list :nothing)))))

(defvar hydra_ext_java_utils_java_throw_illegal_state_exception (lambda (args) (hydra_ext_java_utils_java_throw_statement (funcall (funcall (hydra_ext_java_utils_java_constructor_call (funcall (hydra_ext_java_utils_java_constructor_name "IllegalStateException") (list :nothing))) args) (list :nothing)))))

(defvar hydra_ext_java_utils_name_to_java_type_identifier (lambda (aliases) (lambda (qualify) (lambda (name) (hydra_lib_pairs_first (funcall (funcall (funcall (hydra_ext_java_utils_name_to_qualified_java_name aliases) qualify) name) (list :nothing)))))))

(defvar hydra_ext_java_utils_java_type_from_type_name (lambda (aliases) (lambda (el_name) (hydra_ext_java_utils_java_type_variable_to_type (make-hydra_ext_java_syntax_type_variable (list) (funcall (funcall (hydra_ext_java_utils_name_to_java_type_identifier aliases) nil) el_name))))))

(defvar hydra_ext_java_utils_java_type_identifier_to_java_type_argument (lambda (id) (list :reference (list :variable (make-hydra_ext_java_syntax_type_variable (list) id)))))

(defvar hydra_ext_java_utils_java_type_name (lambda (id) (make-hydra_ext_java_syntax_type_name id (list :nothing))))

(defvar hydra_ext_java_utils_java_type_parameter (lambda (v) (make-hydra_ext_java_syntax_type_parameter (list) (hydra_ext_java_utils_java_type_identifier v) (list :nothing))))

(defvar hydra_ext_java_utils_java_type_to_java_formal_parameter (lambda (jt) (lambda (fname) (list :simple (make-hydra_ext_java_syntax_formal_parameter_simple (list) jt (hydra_ext_java_utils_field_name_to_java_variable_declarator_id fname))))))

(defvar hydra_ext_java_utils_java_type_to_java_reference_type (lambda (t_) (lambda (cx) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :reference) (funcall (lambda (rt) (list :right rt)) match_value)) ((equal (car match_target) :primitive) (funcall (lambda (_) (list :left (list :other "expected a Java reference type"))) match_value)))) (cadr match_target))) t_))))

(defvar hydra_ext_java_utils_java_type_to_java_result (lambda (jt) (list :type jt)))

(defvar hydra_ext_java_utils_java_type_to_java_type_argument (lambda (t_) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :reference) (funcall (lambda (rt) (list :reference rt)) match_value)) ((equal (car match_target) :primitive) (funcall (lambda (_) (list :wildcard (make-hydra_ext_java_syntax_wildcard (list) (list :nothing)))) match_value)))) (cadr match_target))) t_)))

(defvar hydra_ext_java_utils_java_type_variable (lambda (v) (list :variable (make-hydra_ext_java_syntax_type_variable (list) (hydra_ext_java_utils_java_type_identifier (hydra_formatting_capitalize v))))))

(defvar hydra_ext_java_utils_java_unary_expression_to_java_expression (lambda (ue) (list :assignment (list :conditional (list :simple (list (list (list (list (list (list :unary (list :simple (list :unary (list :unary (list :unary ue)))))))))))))))

(defvar hydra_ext_java_utils_java_unary_expression_to_java_relational_expression (lambda (ue) (list :simple (list :unary (list :unary (list :unary ue))))))

(defvar hydra_ext_java_utils_lookup_java_var_name (lambda (aliases) (lambda (name) (funcall (funcall (hydra_lib_maybes_cases (funcall (hydra_lib_maps_lookup name) (funcall (lambda (v) (hydra_ext_java_environment_aliases-var_renames v)) aliases))) (lambda () name)) (lambda (renamed) renamed)))))

(defvar hydra_ext_java_utils_make_constructor (lambda (aliases) (lambda (el_name) (lambda (private) (lambda (params) (lambda (stmts) (let* ((body (make-hydra_ext_java_syntax_constructor_body (list :nothing) stmts)) (nm (funcall (funcall (hydra_ext_java_utils_name_to_java_type_identifier aliases) nil) el_name)) (cons (make-hydra_ext_java_syntax_constructor_declarator (list) nm (list :nothing) params)) (mods (list (if private (list :private nil) (list :public nil))))) (list :constructor_declaration (make-hydra_ext_java_syntax_constructor_declaration mods cons (list :nothing) body)))))))))

(defvar hydra_ext_java_utils_method_declaration (lambda (mods) (lambda (tparams) (lambda (anns) (lambda (method_name) (lambda (params) (lambda (result) (lambda (stmts) (hydra_ext_java_utils_java_method_declaration_to_java_class_body_declaration (make-hydra_ext_java_syntax_method_declaration anns mods (funcall (funcall (funcall (hydra_ext_java_utils_java_method_header tparams) method_name) params) result) (hydra_ext_java_utils_java_method_body stmts)))))))))))

(defvar hydra_ext_java_utils_method_invocation (lambda (lhs) (lambda (method_name) (lambda (args) (let ((header (funcall (funcall (hydra_lib_maybes_cases lhs) (lambda () (list :simple method_name))) (lambda (either) (list :complex (make-hydra_ext_java_syntax_method_invocation_complex (funcall (funcall (hydra_lib_eithers_either (lambda (en) (list :expression en))) (lambda (p) (list :primary p))) either) (list) method_name)))))) (make-hydra_ext_java_syntax_method_invocation header args))))))

(defvar hydra_ext_java_utils_method_invocation_static (lambda (self) (lambda (method_name) (lambda (args) (funcall (funcall (hydra_ext_java_utils_method_invocation (list :just (list :left (hydra_ext_java_utils_java_identifier_to_java_expression_name self)))) method_name) args)))))

(defvar hydra_ext_java_utils_method_invocation_static_with_type_args (lambda (self) (lambda (method_name) (lambda (targs) (lambda (args) (let ((header (list :complex (make-hydra_ext_java_syntax_method_invocation_complex (list :expression (hydra_ext_java_utils_java_identifier_to_java_expression_name self)) targs method_name)))) (make-hydra_ext_java_syntax_method_invocation header args)))))))

(defvar hydra_ext_java_utils_name_to_java_name (lambda (aliases) (lambda (name) (let* ((qn (hydra_names_qualify_name name)) (local (funcall (lambda (v) (hydra_packaging_qualified_name-local v)) qn)) (ns_ (funcall (lambda (v) (hydra_packaging_qualified_name-namespace v)) qn))) (if (hydra_ext_java_utils_is_escaped (funcall (lambda (v) v) name)) (hydra_ext_java_utils_sanitize_java_name local) (funcall (funcall (hydra_lib_maybes_cases ns_) (lambda () local)) (lambda (gname) (let* ((parts (funcall (funcall (hydra_lib_maybes_cases (funcall (hydra_lib_maps_lookup gname) (funcall (lambda (v) (hydra_ext_java_environment_aliases-packages v)) aliases))) (lambda () (funcall (hydra_lib_strings_split_on ".") (funcall (lambda (v) v) gname)))) (lambda (pkg_name) (funcall (hydra_lib_lists_map (lambda (i) (funcall (lambda (v) v) i))) (funcall (lambda (v) v) pkg_name))))) (all_parts (funcall (hydra_lib_lists_concat2 parts) (list (hydra_ext_java_utils_sanitize_java_name local))))) (funcall (hydra_lib_strings_intercalate ".") all_parts)))))))))

(defvar hydra_ext_java_utils_name_to_java_reference_type (lambda (aliases) (lambda (qualify) (lambda (args) (lambda (name) (lambda (mlocal) (list :class_or_interface (list :class (funcall (funcall (funcall (funcall (hydra_ext_java_utils_name_to_java_class_type aliases) qualify) args) name) mlocal)))))))))

(defvar hydra_ext_java_utils_override_annotation (list :marker (hydra_ext_java_utils_java_type_name "Override")))

(defvar hydra_ext_java_utils_reference_type_to_result (lambda (rt) (hydra_ext_java_utils_java_type_to_java_result (list :reference rt))))

(defvar hydra_ext_java_utils_suppress_warnings_unchecked_annotation (list :single_element (make-hydra_ext_java_syntax_single_element_annotation (hydra_ext_java_utils_java_type_name "SuppressWarnings") (list :just (list :conditional_expression (list :simple (list (list (hydra_ext_java_utils_java_postfix_expression_to_java_inclusive_or_expression (list :primary (hydra_ext_java_utils_java_literal_to_java_primary (hydra_ext_java_utils_java_string "unchecked"))))))))))))

(defvar hydra_ext_java_utils_type_parameter_to_reference_type (lambda (tp) (hydra_ext_java_utils_java_type_variable (funcall (lambda (v) v) (funcall (lambda (v) v) (funcall (lambda (v) (hydra_ext_java_syntax_type_parameter-identifier v)) tp))))))

(defvar hydra_ext_java_utils_visitor_type_variable (hydra_ext_java_utils_java_type_variable "r"))

(defvar hydra_ext_java_utils_to_accept_method (lambda (abstract) (lambda (vtparams) (let* ((anns (if abstract (list) (list hydra_ext_java_utils_override_annotation))) (return_expr (hydra_ext_java_utils_java_method_invocation_to_java_expression (funcall (funcall (hydra_ext_java_utils_method_invocation_static "visitor") hydra_ext_java_names_visit_method_name) (list hydra_ext_java_utils_java_this)))) (body (if abstract (list :nothing) (list :just (list (list :statement (hydra_ext_java_utils_java_return_statement (list :just return_expr))))))) (mods (if abstract (list (list :public nil) (list :abstract nil)) (list (list :public nil)))) (type_args (funcall (hydra_lib_lists_map (lambda (tp) (list :reference (hydra_ext_java_utils_type_parameter_to_reference_type tp)))) vtparams)) (ref (hydra_ext_java_utils_java_class_type_to_java_type (make-hydra_ext_java_syntax_class_type (list) (list :none nil) (hydra_ext_java_utils_java_type_identifier hydra_ext_java_names_visitor_name) (funcall (hydra_lib_lists_concat2 type_args) (list (list :reference hydra_ext_java_utils_visitor_type_variable)))))) (param (funcall (hydra_ext_java_utils_java_type_to_java_formal_parameter ref) "visitor")) (result (hydra_ext_java_utils_java_type_to_java_result (list :reference hydra_ext_java_utils_visitor_type_variable))) (tparams (list (hydra_ext_java_utils_java_type_parameter hydra_ext_java_names_visitor_return_parameter)))) (funcall (funcall (funcall (funcall (funcall (funcall (hydra_ext_java_utils_method_declaration mods) tparams) anns) hydra_ext_java_names_accept_method_name) (list param)) result) body)))))

(defvar hydra_ext_java_utils_to_assign_stmt (lambda (fname) (let* ((id (hydra_ext_java_utils_field_name_to_java_identifier fname)) (lhs (list :field_access (make-hydra_ext_java_syntax_field_access (list :primary (list :no_new_array (list :this nil))) id))) (rhs (hydra_ext_java_utils_field_name_to_java_expression fname))) (funcall (hydra_ext_java_utils_java_assignment_statement lhs) rhs))))

(defvar hydra_ext_java_utils_to_java_array_type (lambda (t_) (lambda (cx) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :reference) (funcall (lambda (rt) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :class_or_interface) (funcall (lambda (cit) (list :right (list :reference (list :array (make-hydra_ext_java_syntax_array_type (list (list)) (list :class_or_interface cit)))))) match_value)) ((equal (car match_target) :array) (funcall (lambda (at) (let* ((old_dims (funcall (lambda (v) v) (funcall (lambda (v) (hydra_ext_java_syntax_array_type-dims v)) at))) (new_dims (funcall (hydra_lib_lists_concat2 old_dims) (list (list)))) (variant (funcall (lambda (v) (hydra_ext_java_syntax_array_type-variant v)) at))) (list :right (list :reference (list :array (make-hydra_ext_java_syntax_array_type new_dims variant)))))) match_value)) ((equal (car match_target) :variable) (funcall (lambda (_) (list :left (list :other "don't know how to make Java reference type into array type"))) match_value)))) (cadr match_target))) rt)) match_value)) ((equal (car match_target) :primitive) (funcall (lambda (_) (list :left (list :other "don't know how to make Java type into array type"))) match_value)))) (cadr match_target))) t_))))

(defvar hydra_ext_java_utils_type_parameter_to_type_argument (lambda (tp) (hydra_ext_java_utils_java_type_identifier_to_java_type_argument (funcall (lambda (v) (hydra_ext_java_syntax_type_parameter-identifier v)) tp))))

(defvar hydra_ext_java_utils_un_type_parameter (lambda (tp) (funcall (lambda (v) v) (funcall (lambda (v) v) (funcall (lambda (v) (hydra_ext_java_syntax_type_parameter-identifier v)) tp)))))

(defvar hydra_ext_java_utils_unique_var_name_go (lambda (aliases) (lambda (base) (lambda (n) (let ((candidate (funcall (hydra_lib_strings_cat2 base) (hydra_lib_literals_show_int32 n)))) (if (funcall (hydra_lib_sets_member candidate) (funcall (lambda (v) (hydra_ext_java_environment_aliases-in_scope_java_vars v)) aliases)) (funcall (funcall (hydra_ext_java_utils_unique_var_name_go aliases) base) (funcall (hydra_lib_math_add n) 1)) candidate))))))

(defvar hydra_ext_java_utils_unique_var_name (lambda (aliases) (lambda (name) (if (funcall (hydra_lib_sets_member name) (funcall (lambda (v) (hydra_ext_java_environment_aliases-in_scope_java_vars v)) aliases)) (funcall (funcall (hydra_ext_java_utils_unique_var_name_go aliases) (funcall (lambda (v) v) name)) 2) name))))

(defvar hydra_ext_java_utils_var_declaration_statement (lambda (id) (lambda (rhs) (list :local_variable_declaration (make-hydra_ext_java_syntax_local_variable_declaration (list) (list :var nil) (list (funcall (hydra_ext_java_utils_java_variable_declarator id) (list :just (list :expression rhs)))))))))

(defvar hydra_ext_java_utils_variable_declaration_statement (lambda (aliases) (lambda (jtype) (lambda (id) (lambda (rhs) (let* ((init_ (list :expression rhs)) (vdec (funcall (hydra_ext_java_utils_java_variable_declarator id) (list :just init_)))) (list :local_variable_declaration (make-hydra_ext_java_syntax_local_variable_declaration (list) (list :type jtype) (list vdec)))))))))

(defvar hydra_ext_java_utils_variant_class_name (lambda (qualify) (lambda (el_name) (lambda (fname) (let* ((flocal (hydra_formatting_capitalize (funcall (lambda (v) v) fname))) (qn (hydra_names_qualify_name el_name)) (local (funcall (lambda (v) (hydra_packaging_qualified_name-local v)) qn)) (local1 (if qualify (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 local) ".")) flocal) (if (funcall (hydra_lib_equality_equal flocal) local) (funcall (hydra_lib_strings_cat2 flocal) "_") flocal))) (ns_ (funcall (lambda (v) (hydra_packaging_qualified_name-namespace v)) qn))) (hydra_names_unqualify_name (make-hydra_packaging_qualified_name ns_ local1)))))))

(provide 'hydra.ext.java.utils)
