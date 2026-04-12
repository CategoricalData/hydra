(require 'cl-lib)

(require 'hydra.analysis)

(require 'hydra.ext.python.environment)

(require 'hydra.ext.python.names)

(require 'hydra.ext.python.serde)

(require 'hydra.ext.python.syntax)

(require 'hydra.lib.equality)

(require 'hydra.lib.lists)

(require 'hydra.lib.logic)

(require 'hydra.lib.maps)

(require 'hydra.lib.maybes)

(require 'hydra.lib.pairs)

(require 'hydra.lib.strings)

(require 'hydra.packaging)

(require 'hydra.serialization)

(defvar hydra_ext_python_utils_py_conjunction_to_py_expression (lambda (conj) (list :simple (list conj))))

(defvar hydra_ext_python_utils_py_bitwise_or_to_py_conjunction (lambda (bor) (list (list :simple (make-hydra_ext_python_syntax_comparison bor (list))))))

(defvar hydra_ext_python_utils_py_primary_to_py_bitwise_or (lambda (prim) (make-hydra_ext_python_syntax_bitwise_or (list :nothing) (make-hydra_ext_python_syntax_bitwise_xor (list :nothing) (make-hydra_ext_python_syntax_bitwise_and (list :nothing) (make-hydra_ext_python_syntax_shift_expression (list :nothing) (make-hydra_ext_python_syntax_sum (list :nothing) (make-hydra_ext_python_syntax_term (list :nothing) (list :simple (make-hydra_ext_python_syntax_power (make-hydra_ext_python_syntax_await_primary nil prim) (list :nothing)))))))))))

(defvar hydra_ext_python_utils_py_primary_to_py_conjunction (lambda (prim) (hydra_ext_python_utils_py_bitwise_or_to_py_conjunction (hydra_ext_python_utils_py_primary_to_py_bitwise_or prim))))

(defvar hydra_ext_python_utils_py_primary_to_py_expression (lambda (prim) (hydra_ext_python_utils_py_conjunction_to_py_expression (hydra_ext_python_utils_py_primary_to_py_conjunction prim))))

(defvar hydra_ext_python_utils_py_atom_to_py_expression (lambda (atom) (hydra_ext_python_utils_py_primary_to_py_expression (list :simple atom))))

(defvar hydra_ext_python_utils_string_to_py_expression (lambda (style) (lambda (s) (hydra_ext_python_utils_py_atom_to_py_expression (list :string (make-hydra_ext_python_syntax_string s style))))))

(defvar hydra_ext_python_utils_double_quoted_string (lambda (s) (funcall (hydra_ext_python_utils_string_to_py_expression (list :double nil)) s)))

(defvar hydra_ext_python_utils_primary_with_rhs (lambda (prim) (lambda (rhs) (list :compound (make-hydra_ext_python_syntax_primary_with_rhs prim rhs)))))

(defvar hydra_ext_python_utils_primary_with_slices (lambda (prim) (lambda (first) (lambda (rest) (funcall (hydra_ext_python_utils_primary_with_rhs prim) (list :slices (make-hydra_ext_python_syntax_slices first rest)))))))

(defvar hydra_ext_python_utils_py_expression_to_py_slice (lambda (expr) (list :named (list :simple expr))))

(defvar hydra_ext_python_utils_primary_with_expression_slices (lambda (prim) (lambda (exprs) (funcall (funcall (hydra_ext_python_utils_primary_with_slices prim) (hydra_ext_python_utils_py_expression_to_py_slice (hydra_lib_lists_head exprs))) (funcall (hydra_lib_lists_map (lambda (e) (list :slice (hydra_ext_python_utils_py_expression_to_py_slice e)))) (hydra_lib_lists_tail exprs))))))

(defvar hydra_ext_python_utils_py_name_to_py_primary (lambda (name) (list :simple (list :name name))))

(defvar hydra_ext_python_utils_annotated_expression (lambda (mcomment) (lambda (expr) (funcall (funcall (hydra_lib_maybes_maybe (lambda () expr)) (lambda (c) (hydra_ext_python_utils_py_primary_to_py_expression (funcall (hydra_ext_python_utils_primary_with_expression_slices (hydra_ext_python_utils_py_name_to_py_primary "Annotated")) (list expr (hydra_ext_python_utils_double_quoted_string c)))))) mcomment))))

(defvar hydra_ext_python_utils_annotated_statement (lambda (mcomment) (lambda (stmt) (funcall (funcall (hydra_lib_maybes_maybe (lambda () stmt)) (lambda (c) (list :annotated (make-hydra_ext_python_syntax_annotated_statement c stmt)))) mcomment))))

(defvar hydra_ext_python_utils_py_simple_statement_to_py_statement (lambda (s) (list :simple (list s))))

(defvar hydra_ext_python_utils_py_assignment_to_py_statement (lambda (a) (hydra_ext_python_utils_py_simple_statement_to_py_statement (list :assignment a))))

(defvar hydra_ext_python_utils_py_name_to_py_star_target (lambda (name) (list :unstarred (list :atom (list :name name)))))

(defvar hydra_ext_python_utils_assignment (lambda (name) (lambda (rhs) (hydra_ext_python_utils_py_assignment_to_py_statement (list :untyped (make-hydra_ext_python_syntax_untyped_assignment (list (hydra_ext_python_utils_py_name_to_py_star_target name)) rhs (list :nothing)))))))

(defvar hydra_ext_python_utils_py_expression_to_py_annotated_rhs (lambda (expr) (list :star (list (list :simple expr)))))

(defvar hydra_ext_python_utils_assignment_statement (lambda (name) (lambda (expr) (funcall (hydra_ext_python_utils_assignment name) (hydra_ext_python_utils_py_expression_to_py_annotated_rhs expr)))))

(defvar hydra_ext_python_utils_py_expressions_to_py_args (lambda (exprs) (make-hydra_ext_python_syntax_args (funcall (hydra_lib_lists_map (lambda (e) (list :expression e))) exprs) (list) (list))))

(defvar hydra_ext_python_utils_function_call (lambda (func) (lambda (args) (hydra_ext_python_utils_py_primary_to_py_expression (funcall (hydra_ext_python_utils_primary_with_rhs func) (list :call (hydra_ext_python_utils_py_expressions_to_py_args args)))))))

(defvar hydra_ext_python_utils_cast_to (lambda (pytype) (lambda (pyexpr) (funcall (hydra_ext_python_utils_function_call (hydra_ext_python_utils_py_name_to_py_primary "cast")) (list pytype pyexpr)))))

(defvar hydra_ext_python_utils_py_expression_to_py_simple_statement (lambda (expr) (list :star_expressions (list (list :simple expr)))))

(defvar hydra_ext_python_utils_py_expression_to_py_statement (lambda (expr) (hydra_ext_python_utils_py_simple_statement_to_py_statement (hydra_ext_python_utils_py_expression_to_py_simple_statement expr))))

(defvar hydra_ext_python_utils_triple_quoted_string (lambda (s) (funcall (hydra_ext_python_utils_string_to_py_expression (list :triple nil)) s)))

(defvar hydra_ext_python_utils_comment_statement (lambda (s) (hydra_ext_python_utils_py_expression_to_py_statement (hydra_ext_python_utils_triple_quoted_string s))))

(defvar hydra_ext_python_utils_decode_py_power_to_py_primary (lambda (p) (let* ((lhs (funcall (lambda (v) (hydra_ext_python_syntax_power-lhs v)) p)) (await (funcall (lambda (v) (hydra_ext_python_syntax_await_primary-await v)) lhs)) (prim (funcall (lambda (v) (hydra_ext_python_syntax_await_primary-primary v)) lhs))) (if await (list :nothing) (list :just prim)))))

(defvar hydra_ext_python_utils_decode_py_comparison_to_py_await_primary (lambda (c) (let* ((lhs (funcall (lambda (v) (hydra_ext_python_syntax_comparison-lhs v)) c)) (or_rhs (funcall (lambda (v) (hydra_ext_python_syntax_bitwise_or-rhs v)) lhs)) (xor_rhs (funcall (lambda (v) (hydra_ext_python_syntax_bitwise_xor-rhs v)) or_rhs)) (and_lhs (funcall (lambda (v) (hydra_ext_python_syntax_bitwise_and-lhs v)) xor_rhs)) (and_rhs (funcall (lambda (v) (hydra_ext_python_syntax_bitwise_and-rhs v)) xor_rhs)) (or_lhs (funcall (lambda (v) (hydra_ext_python_syntax_bitwise_or-lhs v)) lhs)) (rhs (funcall (lambda (v) (hydra_ext_python_syntax_comparison-rhs v)) c)) (shift_lhs (funcall (lambda (v) (hydra_ext_python_syntax_shift_expression-lhs v)) and_rhs)) (shift_rhs (funcall (lambda (v) (hydra_ext_python_syntax_shift_expression-rhs v)) and_rhs)) (sum_lhs (funcall (lambda (v) (hydra_ext_python_syntax_sum-lhs v)) shift_rhs)) (sum_rhs (funcall (lambda (v) (hydra_ext_python_syntax_sum-rhs v)) shift_rhs)) (term_lhs (funcall (lambda (v) (hydra_ext_python_syntax_term-lhs v)) sum_rhs)) (term_rhs (funcall (lambda (v) (hydra_ext_python_syntax_term-rhs v)) sum_rhs)) (xor_lhs (funcall (lambda (v) (hydra_ext_python_syntax_bitwise_xor-lhs v)) or_rhs))) (if (hydra_lib_logic_not (hydra_lib_lists_null rhs)) (list :nothing) (if (hydra_lib_maybes_is_just or_lhs) (list :nothing) (if (hydra_lib_maybes_is_just xor_lhs) (list :nothing) (if (hydra_lib_maybes_is_just and_lhs) (list :nothing) (if (hydra_lib_maybes_is_just shift_lhs) (list :nothing) (if (hydra_lib_maybes_is_just sum_lhs) (list :nothing) (if (hydra_lib_maybes_is_just term_lhs) (list :nothing) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :simple) (funcall (lambda (power) (hydra_ext_python_utils_decode_py_power_to_py_primary power)) match_value)) (t (list :nothing)))) (cadr match_target))) term_rhs)))))))))))

(defvar hydra_ext_python_utils_decode_py_inversion_to_py_primary (lambda (i) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :simple) (funcall (lambda (comparison) (hydra_ext_python_utils_decode_py_comparison_to_py_await_primary comparison)) match_value)) (t (list :nothing)))) (cadr match_target))) i)))

(defvar hydra_ext_python_utils_decode_py_conjunction_to_py_primary (lambda (c) (let ((inversions (funcall (lambda (v) v) c))) (if (funcall (hydra_lib_equality_equal (hydra_lib_lists_length inversions)) 1) (hydra_ext_python_utils_decode_py_inversion_to_py_primary (hydra_lib_lists_head inversions)) (list :nothing)))))

(defvar hydra_ext_python_utils_decode_py_expression_to_py_primary (lambda (e) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :simple) (funcall (lambda (disj) (let ((conjunctions (funcall (lambda (v) v) disj))) (if (funcall (hydra_lib_equality_equal (hydra_lib_lists_length conjunctions)) 1) (hydra_ext_python_utils_decode_py_conjunction_to_py_primary (hydra_lib_lists_head conjunctions)) (list :nothing)))) match_value)) (t (list :nothing)))) (cadr match_target))) e)))

(defvar hydra_ext_python_utils_dotted_assignment_statement (lambda (obj) (lambda (attr) (lambda (expr) (let ((target (list :unstarred (list :project (make-hydra_ext_python_syntax_t_primary_and_name (list :atom (list :name obj)) attr))))) (hydra_ext_python_utils_py_assignment_to_py_statement (list :untyped (make-hydra_ext_python_syntax_untyped_assignment (list target) (hydra_ext_python_utils_py_expression_to_py_annotated_rhs expr) (list :nothing)))))))))

(defvar hydra_ext_python_utils_find_namespaces (lambda (focus_ns) (lambda (defs) (let* ((core_ns "hydra.core") (namespaces (funcall (funcall (hydra_analysis_namespaces_for_definitions hydra_ext_python_names_encode_namespace) focus_ns) defs))) (if (funcall (hydra_lib_equality_equal (funcall (lambda (v) v) (hydra_lib_pairs_first (funcall (lambda (v) (hydra_packaging_namespaces-focus v)) namespaces)))) (funcall (lambda (v) v) core_ns)) namespaces (make-hydra_packaging_namespaces (funcall (lambda (v) (hydra_packaging_namespaces-focus v)) namespaces) (funcall (funcall (hydra_lib_maps_insert core_ns) (hydra_ext_python_names_encode_namespace core_ns)) (funcall (lambda (v) (hydra_packaging_namespaces-mapping v)) namespaces))))))))

(defvar hydra_ext_python_utils_get_item_params (list :param_no_default (make-hydra_ext_python_syntax_param_no_default_parameters (list (make-hydra_ext_python_syntax_param_no_default (make-hydra_ext_python_syntax_param "cls" (list :nothing)) (list :nothing)) (make-hydra_ext_python_syntax_param_no_default (make-hydra_ext_python_syntax_param "item" (list :nothing)) (list :nothing))) (list) (list :nothing))))

(defvar hydra_ext_python_utils_indented_block (lambda (mcomment) (lambda (stmts) (let* ((comment_group (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list))) (lambda (s) (list (hydra_ext_python_utils_comment_statement s)))) mcomment)) (groups (funcall (hydra_lib_lists_filter (lambda (g) (hydra_lib_logic_not (hydra_lib_lists_null g)))) (funcall (hydra_lib_lists_cons comment_group) stmts)))) (if (hydra_lib_lists_null groups) (list :indented (list (list (list :simple (list (hydra_ext_python_utils_py_expression_to_py_simple_statement (hydra_ext_python_utils_py_atom_to_py_expression (list :ellipsis nil)))))))) (list :indented groups))))))

(defvar hydra_ext_python_utils_primary_and_params (lambda (prim) (lambda (params) (hydra_ext_python_utils_py_primary_to_py_expression (funcall (hydra_ext_python_utils_primary_with_expression_slices prim) params)))))

(defvar hydra_ext_python_utils_name_and_params (lambda (py_name) (lambda (params) (funcall (hydra_ext_python_utils_primary_and_params (hydra_ext_python_utils_py_name_to_py_primary py_name)) params))))

(defvar hydra_ext_python_utils_newtype_statement (lambda (name) (lambda (mcomment) (lambda (expr) (funcall (hydra_ext_python_utils_annotated_statement mcomment) (funcall (hydra_ext_python_utils_assignment_statement name) (funcall (hydra_ext_python_utils_function_call (list :simple (list :name "NewType"))) (list (hydra_ext_python_utils_double_quoted_string (funcall (lambda (v) v) name)) expr))))))))

(defvar hydra_ext_python_utils_py_bitwise_or_to_py_expression (lambda (bor) (hydra_ext_python_utils_py_conjunction_to_py_expression (hydra_ext_python_utils_py_bitwise_or_to_py_conjunction bor))))

(defvar hydra_ext_python_utils_py_primary_to_py_bitwise_xor (lambda (prim) (make-hydra_ext_python_syntax_bitwise_xor (list :nothing) (make-hydra_ext_python_syntax_bitwise_and (list :nothing) (make-hydra_ext_python_syntax_shift_expression (list :nothing) (make-hydra_ext_python_syntax_sum (list :nothing) (make-hydra_ext_python_syntax_term (list :nothing) (list :simple (make-hydra_ext_python_syntax_power (make-hydra_ext_python_syntax_await_primary nil prim) (list :nothing))))))))))

(defvar hydra_ext_python_utils_or_expression (lambda (prims) (letrec ((build (lambda (prev) (lambda (ps) (if (hydra_lib_lists_null (hydra_lib_lists_tail ps)) (make-hydra_ext_python_syntax_bitwise_or prev (hydra_ext_python_utils_py_primary_to_py_bitwise_xor (hydra_lib_lists_head ps))) (funcall (build (list :just (make-hydra_ext_python_syntax_bitwise_or prev (hydra_ext_python_utils_py_primary_to_py_bitwise_xor (hydra_lib_lists_head ps))))) (hydra_lib_lists_tail ps))))))) (hydra_ext_python_utils_py_bitwise_or_to_py_expression (funcall (build (list :nothing)) prims)))))

(defvar hydra_ext_python_utils_project_from_expression (lambda (exp) (lambda (name) (let ((prim (list :simple (list :group (list :expression (list :simple exp)))))) (hydra_ext_python_utils_py_primary_to_py_expression (list :compound (make-hydra_ext_python_syntax_primary_with_rhs prim (list :project name))))))))

(defvar hydra_ext_python_utils_py_class_definition_to_py_statement (lambda (cd) (list :compound (list :class_def cd))))

(defvar hydra_ext_python_utils_py_closed_pattern_to_py_patterns (lambda (p) (list :pattern (list :or (list p)))))

(defvar hydra_ext_python_utils_py_expression_to_bitwise_or (lambda (e) (make-hydra_ext_python_syntax_bitwise_or (list :nothing) (make-hydra_ext_python_syntax_bitwise_xor (list :nothing) (make-hydra_ext_python_syntax_bitwise_and (list :nothing) (make-hydra_ext_python_syntax_shift_expression (list :nothing) (make-hydra_ext_python_syntax_sum (list :nothing) (make-hydra_ext_python_syntax_term (list :nothing) (list :simple (make-hydra_ext_python_syntax_power (make-hydra_ext_python_syntax_await_primary nil (list :simple (list :group (list :expression (list :simple e))))) (list :nothing)))))))))))

(defvar hydra_ext_python_utils_py_expression_to_disjunction (lambda (e) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :simple) (funcall (lambda (disj) disj) match_value)) (t (list (hydra_ext_python_utils_py_primary_to_py_conjunction (list :simple (list :group (list :expression (list :simple e))))))))) (cadr match_target))) e)))

(defvar hydra_ext_python_utils_py_expression_to_py_primary (lambda (e) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :simple (list :group (list :expression (list :simple e)))))) (lambda (prim) prim)) (hydra_ext_python_utils_decode_py_expression_to_py_primary e))))

(defvar hydra_ext_python_utils_py_expression_to_py_star_named_expression (lambda (expr) (list :simple (list :simple expr))))

(defvar hydra_ext_python_utils_py_list (lambda (exprs) (funcall (hydra_lib_lists_map hydra_ext_python_utils_py_expression_to_py_star_named_expression) exprs)))

(defvar hydra_ext_python_utils_py_name_to_py_expression (lambda (name) (hydra_ext_python_utils_py_primary_to_py_expression (hydra_ext_python_utils_py_name_to_py_primary name))))

(defvar hydra_ext_python_utils_py_name_to_py_named_expression (lambda (name) (list :simple (hydra_ext_python_utils_py_name_to_py_expression name))))

(defvar hydra_ext_python_utils_py_name_to_py_type_parameter (lambda (name) (list :simple (make-hydra_ext_python_syntax_simple_type_parameter name (list :nothing) (list :nothing)))))

(defvar hydra_ext_python_utils_py_none "None")

(defvar hydra_ext_python_utils_py_primary_to_py_slice (lambda (prim) (hydra_ext_python_utils_py_expression_to_py_slice (hydra_ext_python_utils_py_primary_to_py_expression prim))))

(defvar hydra_ext_python_utils_raise_assertion_error (lambda (msg) (hydra_ext_python_utils_py_simple_statement_to_py_statement (list :raise (list :just (make-hydra_ext_python_syntax_raise_expression (funcall (hydra_ext_python_utils_function_call (list :simple (list :name "AssertionError"))) (list (hydra_ext_python_utils_double_quoted_string msg))) (list :nothing)))))))

(defvar hydra_ext_python_utils_raise_type_error (lambda (msg) (hydra_ext_python_utils_py_simple_statement_to_py_statement (list :raise (list :just (make-hydra_ext_python_syntax_raise_expression (funcall (hydra_ext_python_utils_function_call (list :simple (list :name "TypeError"))) (list (hydra_ext_python_utils_double_quoted_string msg))) (list :nothing)))))))

(defvar hydra_ext_python_utils_return_single (lambda (expr) (hydra_ext_python_utils_py_simple_statement_to_py_statement (list :return (list (list :simple expr))))))

(defvar hydra_ext_python_utils_self_only_params (list :param_no_default (make-hydra_ext_python_syntax_param_no_default_parameters (list (make-hydra_ext_python_syntax_param_no_default (make-hydra_ext_python_syntax_param "self" (list :nothing)) (list :nothing))) (list) (list :nothing))))

(defvar hydra_ext_python_utils_self_other_params (list :param_no_default (make-hydra_ext_python_syntax_param_no_default_parameters (list (make-hydra_ext_python_syntax_param_no_default (make-hydra_ext_python_syntax_param "self" (list :nothing)) (list :nothing)) (make-hydra_ext_python_syntax_param_no_default (make-hydra_ext_python_syntax_param "other" (list :nothing)) (list :nothing))) (list) (list :nothing))))

(defvar hydra_ext_python_utils_single_quoted_string (lambda (s) (funcall (hydra_ext_python_utils_string_to_py_expression (list :single nil)) s)))

(defvar hydra_ext_python_utils_target_python_version (list :python310 nil))

(defvar hydra_ext_python_utils_type_alias_statement (lambda (name) (lambda (tparams) (lambda (mcomment) (lambda (tyexpr) (funcall (hydra_ext_python_utils_annotated_statement mcomment) (hydra_ext_python_utils_py_simple_statement_to_py_statement (list :type_alias (make-hydra_ext_python_syntax_type_alias name tparams tyexpr)))))))))

(defvar hydra_ext_python_utils_type_alias_statement310 (lambda (name) (lambda (_tparams) (lambda (mcomment) (lambda (tyexpr) (let ((quoted_expr (hydra_ext_python_utils_double_quoted_string (hydra_serialization_print_expr (hydra_ext_python_serde_encode_expression tyexpr))))) (funcall (hydra_ext_python_utils_annotated_statement mcomment) (hydra_ext_python_utils_py_assignment_to_py_statement (list :typed (make-hydra_ext_python_syntax_typed_assignment (list :name name) (list :simple (list (list (list :simple (make-hydra_ext_python_syntax_comparison (make-hydra_ext_python_syntax_bitwise_or (list :nothing) (make-hydra_ext_python_syntax_bitwise_xor (list :nothing) (make-hydra_ext_python_syntax_bitwise_and (list :nothing) (make-hydra_ext_python_syntax_shift_expression (list :nothing) (make-hydra_ext_python_syntax_sum (list :nothing) (make-hydra_ext_python_syntax_term (list :nothing) (list :simple (make-hydra_ext_python_syntax_power (make-hydra_ext_python_syntax_await_primary nil (list :simple (list :name "TypeAlias"))) (list :nothing))))))))) (list)))))) (list :just (hydra_ext_python_utils_py_expression_to_py_annotated_rhs quoted_expr))))))))))))

(defvar hydra_ext_python_utils_union_type_class_statements310 (lambda (name) (lambda (mcomment) (lambda (tyexpr) (lambda (extra_stmts) (let ((name_str (funcall (lambda (v) v) name))) (let ((meta_name (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 "_") name_str)) "Meta"))) (let ((doc_string (hydra_serialization_print_expr (hydra_ext_python_serde_encode_expression tyexpr)))) (let ((return_object (hydra_ext_python_utils_py_simple_statement_to_py_statement (list :return (list (list :simple (list :simple (list (list (list :simple (make-hydra_ext_python_syntax_comparison (make-hydra_ext_python_syntax_bitwise_or (list :nothing) (make-hydra_ext_python_syntax_bitwise_xor (list :nothing) (make-hydra_ext_python_syntax_bitwise_and (list :nothing) (make-hydra_ext_python_syntax_shift_expression (list :nothing) (make-hydra_ext_python_syntax_sum (list :nothing) (make-hydra_ext_python_syntax_term (list :nothing) (list :simple (make-hydra_ext_python_syntax_power (make-hydra_ext_python_syntax_await_primary nil (list :simple (list :name "object"))) (list :nothing))))))))) (list)))))))))))) (let ((get_item_method (list :compound (list :function (make-hydra_ext_python_syntax_function_definition (list :nothing) (make-hydra_ext_python_syntax_function_def_raw nil "__getitem__" (list) (list :just hydra_ext_python_utils_get_item_params) (list :nothing) (list :nothing) (funcall (hydra_ext_python_utils_indented_block (list :nothing)) (list (list return_object))))))))) (let ((meta_class (hydra_ext_python_utils_py_class_definition_to_py_statement (make-hydra_ext_python_syntax_class_definition (list :nothing) meta_name (list) (list :just (hydra_ext_python_utils_py_expressions_to_py_args (list (list :simple (list (list (list :simple (make-hydra_ext_python_syntax_comparison (make-hydra_ext_python_syntax_bitwise_or (list :nothing) (make-hydra_ext_python_syntax_bitwise_xor (list :nothing) (make-hydra_ext_python_syntax_bitwise_and (list :nothing) (make-hydra_ext_python_syntax_shift_expression (list :nothing) (make-hydra_ext_python_syntax_sum (list :nothing) (make-hydra_ext_python_syntax_term (list :nothing) (list :simple (make-hydra_ext_python_syntax_power (make-hydra_ext_python_syntax_await_primary nil (list :simple (list :name "type"))) (list :nothing))))))))) (list))))))))) (funcall (hydra_ext_python_utils_indented_block (list :nothing)) (list (list get_item_method))))))) (let ((doc_stmt (hydra_ext_python_utils_py_expression_to_py_statement (hydra_ext_python_utils_triple_quoted_string doc_string)))) (let ((body_groups (if (hydra_lib_lists_null extra_stmts) (let ((pass_stmt (hydra_ext_python_utils_py_simple_statement_to_py_statement (list :pass nil)))) (list (list doc_stmt) (list pass_stmt))) (list (list doc_stmt) extra_stmts)))) (let ((metaclass_arg (make-hydra_ext_python_syntax_kwarg "metaclass" (list :simple (list (list (list :simple (make-hydra_ext_python_syntax_comparison (make-hydra_ext_python_syntax_bitwise_or (list :nothing) (make-hydra_ext_python_syntax_bitwise_xor (list :nothing) (make-hydra_ext_python_syntax_bitwise_and (list :nothing) (make-hydra_ext_python_syntax_shift_expression (list :nothing) (make-hydra_ext_python_syntax_sum (list :nothing) (make-hydra_ext_python_syntax_term (list :nothing) (list :simple (make-hydra_ext_python_syntax_power (make-hydra_ext_python_syntax_await_primary nil (list :simple (list :name meta_name))) (list :nothing))))))))) (list))))))))) (let ((union_class (funcall (hydra_ext_python_utils_annotated_statement mcomment) (hydra_ext_python_utils_py_class_definition_to_py_statement (make-hydra_ext_python_syntax_class_definition (list :nothing) name (list) (list :just (make-hydra_ext_python_syntax_args (list) (list (list :kwarg metaclass_arg)) (list))) (funcall (hydra_ext_python_utils_indented_block (list :nothing)) body_groups)))))) (list meta_class union_class))))))))))))))))

(defvar hydra_ext_python_utils_unit_variant_methods (lambda (class_name) (let ((class_name_str (funcall (lambda (v) v) class_name))) (let ((slots_stmt (funcall (hydra_ext_python_utils_assignment_statement "__slots__") (hydra_ext_python_utils_py_primary_to_py_expression (list :simple (list :tuple (list))))))) (let ((return_isinstance (hydra_ext_python_utils_py_simple_statement_to_py_statement (list :return (list (list :simple (funcall (hydra_ext_python_utils_function_call (list :simple (list :name "isinstance"))) (list (list :simple (list (list (list :simple (make-hydra_ext_python_syntax_comparison (make-hydra_ext_python_syntax_bitwise_or (list :nothing) (make-hydra_ext_python_syntax_bitwise_xor (list :nothing) (make-hydra_ext_python_syntax_bitwise_and (list :nothing) (make-hydra_ext_python_syntax_shift_expression (list :nothing) (make-hydra_ext_python_syntax_sum (list :nothing) (make-hydra_ext_python_syntax_term (list :nothing) (list :simple (make-hydra_ext_python_syntax_power (make-hydra_ext_python_syntax_await_primary nil (list :simple (list :name "other"))) (list :nothing))))))))) (list)))))) (list :simple (list (list (list :simple (make-hydra_ext_python_syntax_comparison (make-hydra_ext_python_syntax_bitwise_or (list :nothing) (make-hydra_ext_python_syntax_bitwise_xor (list :nothing) (make-hydra_ext_python_syntax_bitwise_and (list :nothing) (make-hydra_ext_python_syntax_shift_expression (list :nothing) (make-hydra_ext_python_syntax_sum (list :nothing) (make-hydra_ext_python_syntax_term (list :nothing) (list :simple (make-hydra_ext_python_syntax_power (make-hydra_ext_python_syntax_await_primary nil (list :simple (list :name class_name))) (list :nothing))))))))) (list)))))))))))))) (let ((eq_method (list :compound (list :function (make-hydra_ext_python_syntax_function_definition (list :nothing) (make-hydra_ext_python_syntax_function_def_raw nil "__eq__" (list) (list :just hydra_ext_python_utils_self_other_params) (list :nothing) (list :nothing) (funcall (hydra_ext_python_utils_indented_block (list :nothing)) (list (list return_isinstance))))))))) (let ((return_hash (hydra_ext_python_utils_py_simple_statement_to_py_statement (list :return (list (list :simple (funcall (hydra_ext_python_utils_function_call (list :simple (list :name "hash"))) (list (hydra_ext_python_utils_double_quoted_string class_name_str))))))))) (let ((hash_method (list :compound (list :function (make-hydra_ext_python_syntax_function_definition (list :nothing) (make-hydra_ext_python_syntax_function_def_raw nil "__hash__" (list) (list :just hydra_ext_python_utils_self_only_params) (list :nothing) (list :nothing) (funcall (hydra_ext_python_utils_indented_block (list :nothing)) (list (list return_hash))))))))) (list slots_stmt eq_method hash_method)))))))))

(provide 'hydra.ext.python.utils)
