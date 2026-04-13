(ns hydra.python.utils
  (:require [hydra.analysis :refer :all] [hydra.lib.equality :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.logic :refer :all] [hydra.lib.maps :refer :all] [hydra.lib.maybes :refer :all] [hydra.lib.pairs :refer :all] [hydra.lib.strings :refer :all] [hydra.packaging :refer :all] [hydra.python.environment :refer :all] [hydra.python.names :refer :all] [hydra.python.serde :refer :all] [hydra.python.syntax :refer :all] [hydra.serialization :refer :all]
))

(declare hydra_python_utils_py_conjunction_to_py_expression hydra_python_utils_py_bitwise_or_to_py_conjunction hydra_python_utils_py_primary_to_py_bitwise_or hydra_python_utils_py_primary_to_py_conjunction hydra_python_utils_py_primary_to_py_expression hydra_python_utils_py_atom_to_py_expression hydra_python_utils_string_to_py_expression hydra_python_utils_double_quoted_string hydra_python_utils_primary_with_rhs hydra_python_utils_primary_with_slices hydra_python_utils_py_expression_to_py_slice hydra_python_utils_primary_with_expression_slices hydra_python_utils_py_name_to_py_primary hydra_python_utils_annotated_expression hydra_python_utils_annotated_statement hydra_python_utils_py_simple_statement_to_py_statement hydra_python_utils_py_assignment_to_py_statement hydra_python_utils_py_name_to_py_star_target hydra_python_utils_assignment hydra_python_utils_py_expression_to_py_annotated_rhs hydra_python_utils_assignment_statement hydra_python_utils_py_expressions_to_py_args hydra_python_utils_function_call hydra_python_utils_cast_to hydra_python_utils_py_expression_to_py_simple_statement hydra_python_utils_py_expression_to_py_statement hydra_python_utils_triple_quoted_string hydra_python_utils_comment_statement hydra_python_utils_decode_py_power_to_py_primary hydra_python_utils_decode_py_comparison_to_py_await_primary hydra_python_utils_decode_py_inversion_to_py_primary hydra_python_utils_decode_py_conjunction_to_py_primary hydra_python_utils_decode_py_expression_to_py_primary hydra_python_utils_dotted_assignment_statement hydra_python_utils_find_namespaces hydra_python_utils_get_item_params hydra_python_utils_indented_block hydra_python_utils_primary_and_params hydra_python_utils_name_and_params hydra_python_utils_newtype_statement hydra_python_utils_py_bitwise_or_to_py_expression hydra_python_utils_py_primary_to_py_bitwise_xor hydra_python_utils_or_expression hydra_python_utils_project_from_expression hydra_python_utils_py_class_definition_to_py_statement hydra_python_utils_py_closed_pattern_to_py_patterns hydra_python_utils_py_expression_to_bitwise_or hydra_python_utils_py_expression_to_disjunction hydra_python_utils_py_expression_to_py_primary hydra_python_utils_py_expression_to_py_star_named_expression hydra_python_utils_py_list hydra_python_utils_py_name_to_py_expression hydra_python_utils_py_name_to_py_named_expression hydra_python_utils_py_name_to_py_type_parameter hydra_python_utils_py_none hydra_python_utils_py_primary_to_py_slice hydra_python_utils_raise_assertion_error hydra_python_utils_raise_type_error hydra_python_utils_return_single hydra_python_utils_self_only_params hydra_python_utils_self_other_params hydra_python_utils_single_quoted_string hydra_python_utils_target_python_version hydra_python_utils_type_alias_statement hydra_python_utils_type_alias_statement310 hydra_python_utils_union_type_class_statements310 hydra_python_utils_unit_variant_methods)

(def hydra_python_utils_py_conjunction_to_py_expression (fn [conj] (list :simple (list conj))))

(def hydra_python_utils_py_bitwise_or_to_py_conjunction (fn [bor] (list (list :simple (->hydra_python_syntax_comparison bor (list))))))

(def hydra_python_utils_py_primary_to_py_bitwise_or (fn [prim] (->hydra_python_syntax_bitwise_or (list :nothing) (->hydra_python_syntax_bitwise_xor (list :nothing) (->hydra_python_syntax_bitwise_and (list :nothing) (->hydra_python_syntax_shift_expression (list :nothing) (->hydra_python_syntax_sum (list :nothing) (->hydra_python_syntax_term (list :nothing) (list :simple (->hydra_python_syntax_power (->hydra_python_syntax_await_primary false prim) (list :nothing)))))))))))

(def hydra_python_utils_py_primary_to_py_conjunction (fn [prim] (hydra_python_utils_py_bitwise_or_to_py_conjunction (hydra_python_utils_py_primary_to_py_bitwise_or prim))))

(def hydra_python_utils_py_primary_to_py_expression (fn [prim] (hydra_python_utils_py_conjunction_to_py_expression (hydra_python_utils_py_primary_to_py_conjunction prim))))

(def hydra_python_utils_py_atom_to_py_expression (fn [atom] (hydra_python_utils_py_primary_to_py_expression (list :simple atom))))

(def hydra_python_utils_string_to_py_expression (fn [style] (fn [s] (hydra_python_utils_py_atom_to_py_expression (list :string (->hydra_python_syntax_string s style))))))

(def hydra_python_utils_double_quoted_string (fn [s] ((hydra_python_utils_string_to_py_expression (list :double nil)) s)))

(def hydra_python_utils_primary_with_rhs (fn [prim] (fn [rhs] (list :compound (->hydra_python_syntax_primary_with_rhs prim rhs)))))

(def hydra_python_utils_primary_with_slices (fn [prim] (fn [first] (fn [rest] ((hydra_python_utils_primary_with_rhs prim) (list :slices (->hydra_python_syntax_slices first rest)))))))

(def hydra_python_utils_py_expression_to_py_slice (fn [expr] (list :named (list :simple expr))))

(def hydra_python_utils_primary_with_expression_slices (fn [prim] (fn [exprs] (((hydra_python_utils_primary_with_slices prim) (hydra_python_utils_py_expression_to_py_slice (hydra_lib_lists_head exprs))) ((hydra_lib_lists_map (fn [e] (list :slice (hydra_python_utils_py_expression_to_py_slice e)))) (hydra_lib_lists_tail exprs))))))

(def hydra_python_utils_py_name_to_py_primary (fn [name] (list :simple (list :name name))))

(def hydra_python_utils_annotated_expression (fn [mcomment] (fn [expr] (((hydra_lib_maybes_maybe (fn [] expr)) (fn [c] (hydra_python_utils_py_primary_to_py_expression ((hydra_python_utils_primary_with_expression_slices (hydra_python_utils_py_name_to_py_primary "Annotated")) (list expr (hydra_python_utils_double_quoted_string c)))))) mcomment))))

(def hydra_python_utils_annotated_statement (fn [mcomment] (fn [stmt] (((hydra_lib_maybes_maybe (fn [] stmt)) (fn [c] (list :annotated (->hydra_python_syntax_annotated_statement c stmt)))) mcomment))))

(def hydra_python_utils_py_simple_statement_to_py_statement (fn [s] (list :simple (list s))))

(def hydra_python_utils_py_assignment_to_py_statement (fn [a] (hydra_python_utils_py_simple_statement_to_py_statement (list :assignment a))))

(def hydra_python_utils_py_name_to_py_star_target (fn [name] (list :unstarred (list :atom (list :name name)))))

(def hydra_python_utils_assignment (fn [name] (fn [rhs] (hydra_python_utils_py_assignment_to_py_statement (list :untyped (->hydra_python_syntax_untyped_assignment (list (hydra_python_utils_py_name_to_py_star_target name)) rhs (list :nothing)))))))

(def hydra_python_utils_py_expression_to_py_annotated_rhs (fn [expr] (list :star (list (list :simple expr)))))

(def hydra_python_utils_assignment_statement (fn [name] (fn [expr] ((hydra_python_utils_assignment name) (hydra_python_utils_py_expression_to_py_annotated_rhs expr)))))

(def hydra_python_utils_py_expressions_to_py_args (fn [exprs] (->hydra_python_syntax_args ((hydra_lib_lists_map (fn [e] (list :expression e))) exprs) (list) (list))))

(def hydra_python_utils_function_call (fn [func] (fn [args] (hydra_python_utils_py_primary_to_py_expression ((hydra_python_utils_primary_with_rhs func) (list :call (hydra_python_utils_py_expressions_to_py_args args)))))))

(def hydra_python_utils_cast_to (fn [pytype] (fn [pyexpr] ((hydra_python_utils_function_call (hydra_python_utils_py_name_to_py_primary "cast")) (list pytype pyexpr)))))

(def hydra_python_utils_py_expression_to_py_simple_statement (fn [expr] (list :star_expressions (list (list :simple expr)))))

(def hydra_python_utils_py_expression_to_py_statement (fn [expr] (hydra_python_utils_py_simple_statement_to_py_statement (hydra_python_utils_py_expression_to_py_simple_statement expr))))

(def hydra_python_utils_triple_quoted_string (fn [s] ((hydra_python_utils_string_to_py_expression (list :triple nil)) s)))

(def hydra_python_utils_comment_statement (fn [s] (hydra_python_utils_py_expression_to_py_statement (hydra_python_utils_triple_quoted_string s))))

(def hydra_python_utils_decode_py_power_to_py_primary (fn [p] (let [lhs ((fn [v] (:lhs v)) p) await ((fn [v] (:await v)) lhs) prim ((fn [v] (:primary v)) lhs)] (if await (list :nothing) (list :just prim)))))

(def hydra_python_utils_decode_py_comparison_to_py_await_primary (fn [c] (let [lhs ((fn [v] (:lhs v)) c) or_rhs ((fn [v] (:rhs v)) lhs) xor_rhs ((fn [v] (:rhs v)) or_rhs) and_lhs ((fn [v] (:lhs v)) xor_rhs) and_rhs ((fn [v] (:rhs v)) xor_rhs) or_lhs ((fn [v] (:lhs v)) lhs) rhs ((fn [v] (:rhs v)) c) shift_lhs ((fn [v] (:lhs v)) and_rhs) shift_rhs ((fn [v] (:rhs v)) and_rhs) sum_lhs ((fn [v] (:lhs v)) shift_rhs) sum_rhs ((fn [v] (:rhs v)) shift_rhs) term_lhs ((fn [v] (:lhs v)) sum_rhs) term_rhs ((fn [v] (:rhs v)) sum_rhs) xor_lhs ((fn [v] (:lhs v)) or_rhs)] (if (hydra_lib_logic_not (hydra_lib_lists_null rhs)) (list :nothing) (if (hydra_lib_maybes_is_just or_lhs) (list :nothing) (if (hydra_lib_maybes_is_just xor_lhs) (list :nothing) (if (hydra_lib_maybes_is_just and_lhs) (list :nothing) (if (hydra_lib_maybes_is_just shift_lhs) (list :nothing) (if (hydra_lib_maybes_is_just sum_lhs) (list :nothing) (if (hydra_lib_maybes_is_just term_lhs) (list :nothing) ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :simple) ((fn [power] (hydra_python_utils_decode_py_power_to_py_primary power)) match_value) :else (list :nothing))) (second match_target))) term_rhs)))))))))))

(def hydra_python_utils_decode_py_inversion_to_py_primary (fn [i] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :simple) ((fn [comparison] (hydra_python_utils_decode_py_comparison_to_py_await_primary comparison)) match_value) :else (list :nothing))) (second match_target))) i)))

(def hydra_python_utils_decode_py_conjunction_to_py_primary (fn [c] (let [inversions ((fn [v] v) c)] (if ((hydra_lib_equality_equal (hydra_lib_lists_length inversions)) 1) (hydra_python_utils_decode_py_inversion_to_py_primary (hydra_lib_lists_head inversions)) (list :nothing)))))

(def hydra_python_utils_decode_py_expression_to_py_primary (fn [e] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :simple) ((fn [disj] (let [conjunctions ((fn [v] v) disj)] (if ((hydra_lib_equality_equal (hydra_lib_lists_length conjunctions)) 1) (hydra_python_utils_decode_py_conjunction_to_py_primary (hydra_lib_lists_head conjunctions)) (list :nothing)))) match_value) :else (list :nothing))) (second match_target))) e)))

(def hydra_python_utils_dotted_assignment_statement (fn [obj] (fn [attr] (fn [expr] (let [target (list :unstarred (list :project (->hydra_python_syntax_t_primary_and_name (list :atom (list :name obj)) attr)))] (hydra_python_utils_py_assignment_to_py_statement (list :untyped (->hydra_python_syntax_untyped_assignment (list target) (hydra_python_utils_py_expression_to_py_annotated_rhs expr) (list :nothing)))))))))

(def hydra_python_utils_find_namespaces (fn [focus_ns] (fn [defs] (let [core_ns "hydra.core" namespaces (((hydra_analysis_namespaces_for_definitions hydra_python_names_encode_namespace) focus_ns) defs)] (if ((hydra_lib_equality_equal ((fn [v] v) (hydra_lib_pairs_first ((fn [v] (:focus v)) namespaces)))) ((fn [v] v) core_ns)) namespaces (->hydra_packaging_namespaces ((fn [v] (:focus v)) namespaces) (((hydra_lib_maps_insert core_ns) (hydra_python_names_encode_namespace core_ns)) ((fn [v] (:mapping v)) namespaces))))))))

(def hydra_python_utils_get_item_params (list :param_no_default (->hydra_python_syntax_param_no_default_parameters (list (->hydra_python_syntax_param_no_default (->hydra_python_syntax_param "cls" (list :nothing)) (list :nothing)) (->hydra_python_syntax_param_no_default (->hydra_python_syntax_param "item" (list :nothing)) (list :nothing))) (list) (list :nothing))))

(def hydra_python_utils_indented_block (fn [mcomment] (fn [stmts] (let [comment_group (((hydra_lib_maybes_maybe (fn [] (list))) (fn [s] (list (hydra_python_utils_comment_statement s)))) mcomment) groups ((hydra_lib_lists_filter (fn [g] (hydra_lib_logic_not (hydra_lib_lists_null g)))) ((hydra_lib_lists_cons comment_group) stmts))] (if (hydra_lib_lists_null groups) (list :indented (list (list (list :simple (list (hydra_python_utils_py_expression_to_py_simple_statement (hydra_python_utils_py_atom_to_py_expression (list :ellipsis nil)))))))) (list :indented groups))))))

(def hydra_python_utils_primary_and_params (fn [prim] (fn [params] (hydra_python_utils_py_primary_to_py_expression ((hydra_python_utils_primary_with_expression_slices prim) params)))))

(def hydra_python_utils_name_and_params (fn [py_name] (fn [params] ((hydra_python_utils_primary_and_params (hydra_python_utils_py_name_to_py_primary py_name)) params))))

(def hydra_python_utils_newtype_statement (fn [name] (fn [mcomment] (fn [expr] ((hydra_python_utils_annotated_statement mcomment) ((hydra_python_utils_assignment_statement name) ((hydra_python_utils_function_call (list :simple (list :name "NewType"))) (list (hydra_python_utils_double_quoted_string ((fn [v] v) name)) expr))))))))

(def hydra_python_utils_py_bitwise_or_to_py_expression (fn [bor] (hydra_python_utils_py_conjunction_to_py_expression (hydra_python_utils_py_bitwise_or_to_py_conjunction bor))))

(def hydra_python_utils_py_primary_to_py_bitwise_xor (fn [prim] (->hydra_python_syntax_bitwise_xor (list :nothing) (->hydra_python_syntax_bitwise_and (list :nothing) (->hydra_python_syntax_shift_expression (list :nothing) (->hydra_python_syntax_sum (list :nothing) (->hydra_python_syntax_term (list :nothing) (list :simple (->hydra_python_syntax_power (->hydra_python_syntax_await_primary false prim) (list :nothing))))))))))

(def hydra_python_utils_or_expression (fn [prims] (let [build (fn build [prev] (fn [ps] (if (hydra_lib_lists_null (hydra_lib_lists_tail ps)) (->hydra_python_syntax_bitwise_or prev (hydra_python_utils_py_primary_to_py_bitwise_xor (hydra_lib_lists_head ps))) ((build (list :just (->hydra_python_syntax_bitwise_or prev (hydra_python_utils_py_primary_to_py_bitwise_xor (hydra_lib_lists_head ps))))) (hydra_lib_lists_tail ps)))))] (hydra_python_utils_py_bitwise_or_to_py_expression ((build (list :nothing)) prims)))))

(def hydra_python_utils_project_from_expression (fn [exp] (fn [name] (let [prim (list :simple (list :group (list :expression (list :simple exp))))] (hydra_python_utils_py_primary_to_py_expression (list :compound (->hydra_python_syntax_primary_with_rhs prim (list :project name))))))))

(def hydra_python_utils_py_class_definition_to_py_statement (fn [cd] (list :compound (list :class_def cd))))

(def hydra_python_utils_py_closed_pattern_to_py_patterns (fn [p] (list :pattern (list :or (list p)))))

(def hydra_python_utils_py_expression_to_bitwise_or (fn [e] (->hydra_python_syntax_bitwise_or (list :nothing) (->hydra_python_syntax_bitwise_xor (list :nothing) (->hydra_python_syntax_bitwise_and (list :nothing) (->hydra_python_syntax_shift_expression (list :nothing) (->hydra_python_syntax_sum (list :nothing) (->hydra_python_syntax_term (list :nothing) (list :simple (->hydra_python_syntax_power (->hydra_python_syntax_await_primary false (list :simple (list :group (list :expression (list :simple e))))) (list :nothing)))))))))))

(def hydra_python_utils_py_expression_to_disjunction (fn [e] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :simple) ((fn [disj] disj) match_value) :else (list (hydra_python_utils_py_primary_to_py_conjunction (list :simple (list :group (list :expression (list :simple e)))))))) (second match_target))) e)))

(def hydra_python_utils_py_expression_to_py_primary (fn [e] (((hydra_lib_maybes_maybe (fn [] (list :simple (list :group (list :expression (list :simple e)))))) (fn [prim] prim)) (hydra_python_utils_decode_py_expression_to_py_primary e))))

(def hydra_python_utils_py_expression_to_py_star_named_expression (fn [expr] (list :simple (list :simple expr))))

(def hydra_python_utils_py_list (fn [exprs] ((hydra_lib_lists_map hydra_python_utils_py_expression_to_py_star_named_expression) exprs)))

(def hydra_python_utils_py_name_to_py_expression (fn [name] (hydra_python_utils_py_primary_to_py_expression (hydra_python_utils_py_name_to_py_primary name))))

(def hydra_python_utils_py_name_to_py_named_expression (fn [name] (list :simple (hydra_python_utils_py_name_to_py_expression name))))

(def hydra_python_utils_py_name_to_py_type_parameter (fn [name] (list :simple (->hydra_python_syntax_simple_type_parameter name (list :nothing) (list :nothing)))))

(def hydra_python_utils_py_none "None")

(def hydra_python_utils_py_primary_to_py_slice (fn [prim] (hydra_python_utils_py_expression_to_py_slice (hydra_python_utils_py_primary_to_py_expression prim))))

(def hydra_python_utils_raise_assertion_error (fn [msg] (hydra_python_utils_py_simple_statement_to_py_statement (list :raise (list :just (->hydra_python_syntax_raise_expression ((hydra_python_utils_function_call (list :simple (list :name "AssertionError"))) (list (hydra_python_utils_double_quoted_string msg))) (list :nothing)))))))

(def hydra_python_utils_raise_type_error (fn [msg] (hydra_python_utils_py_simple_statement_to_py_statement (list :raise (list :just (->hydra_python_syntax_raise_expression ((hydra_python_utils_function_call (list :simple (list :name "TypeError"))) (list (hydra_python_utils_double_quoted_string msg))) (list :nothing)))))))

(def hydra_python_utils_return_single (fn [expr] (hydra_python_utils_py_simple_statement_to_py_statement (list :return (list (list :simple expr))))))

(def hydra_python_utils_self_only_params (list :param_no_default (->hydra_python_syntax_param_no_default_parameters (list (->hydra_python_syntax_param_no_default (->hydra_python_syntax_param "self" (list :nothing)) (list :nothing))) (list) (list :nothing))))

(def hydra_python_utils_self_other_params (list :param_no_default (->hydra_python_syntax_param_no_default_parameters (list (->hydra_python_syntax_param_no_default (->hydra_python_syntax_param "self" (list :nothing)) (list :nothing)) (->hydra_python_syntax_param_no_default (->hydra_python_syntax_param "other" (list :nothing)) (list :nothing))) (list) (list :nothing))))

(def hydra_python_utils_single_quoted_string (fn [s] ((hydra_python_utils_string_to_py_expression (list :single nil)) s)))

(def hydra_python_utils_target_python_version (list :python310 nil))

(def hydra_python_utils_type_alias_statement (fn [name] (fn [tparams] (fn [mcomment] (fn [tyexpr] ((hydra_python_utils_annotated_statement mcomment) (hydra_python_utils_py_simple_statement_to_py_statement (list :type_alias (->hydra_python_syntax_type_alias name tparams tyexpr)))))))))

(def hydra_python_utils_type_alias_statement310 (fn [name] (fn [_tparams] (fn [mcomment] (fn [tyexpr] (let [quoted_expr (hydra_python_utils_double_quoted_string (hydra_serialization_print_expr (hydra_python_serde_encode_expression tyexpr)))] ((hydra_python_utils_annotated_statement mcomment) (hydra_python_utils_py_assignment_to_py_statement (list :typed (->hydra_python_syntax_typed_assignment (list :name name) (list :simple (list (list (list :simple (->hydra_python_syntax_comparison (->hydra_python_syntax_bitwise_or (list :nothing) (->hydra_python_syntax_bitwise_xor (list :nothing) (->hydra_python_syntax_bitwise_and (list :nothing) (->hydra_python_syntax_shift_expression (list :nothing) (->hydra_python_syntax_sum (list :nothing) (->hydra_python_syntax_term (list :nothing) (list :simple (->hydra_python_syntax_power (->hydra_python_syntax_await_primary false (list :simple (list :name "TypeAlias"))) (list :nothing))))))))) (list)))))) (list :just (hydra_python_utils_py_expression_to_py_annotated_rhs quoted_expr))))))))))))

(def hydra_python_utils_union_type_class_statements310 (fn [name] (fn [mcomment] (fn [tyexpr] (fn [extra_stmts] (let [name_str ((fn [v] v) name)] (let [meta_name ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "_") name_str)) "Meta")] (let [doc_string (hydra_serialization_print_expr (hydra_python_serde_encode_expression tyexpr))] (let [return_object (hydra_python_utils_py_simple_statement_to_py_statement (list :return (list (list :simple (list :simple (list (list (list :simple (->hydra_python_syntax_comparison (->hydra_python_syntax_bitwise_or (list :nothing) (->hydra_python_syntax_bitwise_xor (list :nothing) (->hydra_python_syntax_bitwise_and (list :nothing) (->hydra_python_syntax_shift_expression (list :nothing) (->hydra_python_syntax_sum (list :nothing) (->hydra_python_syntax_term (list :nothing) (list :simple (->hydra_python_syntax_power (->hydra_python_syntax_await_primary false (list :simple (list :name "object"))) (list :nothing))))))))) (list))))))))))] (let [get_item_method (list :compound (list :function (->hydra_python_syntax_function_definition (list :nothing) (->hydra_python_syntax_function_def_raw false "__getitem__" (list) (list :just hydra_python_utils_get_item_params) (list :nothing) (list :nothing) ((hydra_python_utils_indented_block (list :nothing)) (list (list return_object)))))))] (let [meta_class (hydra_python_utils_py_class_definition_to_py_statement (->hydra_python_syntax_class_definition (list :nothing) meta_name (list) (list :just (hydra_python_utils_py_expressions_to_py_args (list (list :simple (list (list (list :simple (->hydra_python_syntax_comparison (->hydra_python_syntax_bitwise_or (list :nothing) (->hydra_python_syntax_bitwise_xor (list :nothing) (->hydra_python_syntax_bitwise_and (list :nothing) (->hydra_python_syntax_shift_expression (list :nothing) (->hydra_python_syntax_sum (list :nothing) (->hydra_python_syntax_term (list :nothing) (list :simple (->hydra_python_syntax_power (->hydra_python_syntax_await_primary false (list :simple (list :name "type"))) (list :nothing))))))))) (list))))))))) ((hydra_python_utils_indented_block (list :nothing)) (list (list get_item_method)))))] (let [doc_stmt (hydra_python_utils_py_expression_to_py_statement (hydra_python_utils_triple_quoted_string doc_string))] (let [body_groups (if (hydra_lib_lists_null extra_stmts) (let [pass_stmt (hydra_python_utils_py_simple_statement_to_py_statement (list :pass nil))] (list (list doc_stmt) (list pass_stmt))) (list (list doc_stmt) extra_stmts))] (let [metaclass_arg (->hydra_python_syntax_kwarg "metaclass" (list :simple (list (list (list :simple (->hydra_python_syntax_comparison (->hydra_python_syntax_bitwise_or (list :nothing) (->hydra_python_syntax_bitwise_xor (list :nothing) (->hydra_python_syntax_bitwise_and (list :nothing) (->hydra_python_syntax_shift_expression (list :nothing) (->hydra_python_syntax_sum (list :nothing) (->hydra_python_syntax_term (list :nothing) (list :simple (->hydra_python_syntax_power (->hydra_python_syntax_await_primary false (list :simple (list :name meta_name))) (list :nothing))))))))) (list)))))))] (let [union_class ((hydra_python_utils_annotated_statement mcomment) (hydra_python_utils_py_class_definition_to_py_statement (->hydra_python_syntax_class_definition (list :nothing) name (list) (list :just (->hydra_python_syntax_args (list) (list (list :kwarg metaclass_arg)) (list))) ((hydra_python_utils_indented_block (list :nothing)) body_groups))))] (list meta_class union_class))))))))))))))))

(def hydra_python_utils_unit_variant_methods (fn [class_name] (let [class_name_str ((fn [v] v) class_name)] (let [slots_stmt ((hydra_python_utils_assignment_statement "__slots__") (hydra_python_utils_py_primary_to_py_expression (list :simple (list :tuple (list)))))] (let [return_isinstance (hydra_python_utils_py_simple_statement_to_py_statement (list :return (list (list :simple ((hydra_python_utils_function_call (list :simple (list :name "isinstance"))) (list (list :simple (list (list (list :simple (->hydra_python_syntax_comparison (->hydra_python_syntax_bitwise_or (list :nothing) (->hydra_python_syntax_bitwise_xor (list :nothing) (->hydra_python_syntax_bitwise_and (list :nothing) (->hydra_python_syntax_shift_expression (list :nothing) (->hydra_python_syntax_sum (list :nothing) (->hydra_python_syntax_term (list :nothing) (list :simple (->hydra_python_syntax_power (->hydra_python_syntax_await_primary false (list :simple (list :name "other"))) (list :nothing))))))))) (list)))))) (list :simple (list (list (list :simple (->hydra_python_syntax_comparison (->hydra_python_syntax_bitwise_or (list :nothing) (->hydra_python_syntax_bitwise_xor (list :nothing) (->hydra_python_syntax_bitwise_and (list :nothing) (->hydra_python_syntax_shift_expression (list :nothing) (->hydra_python_syntax_sum (list :nothing) (->hydra_python_syntax_term (list :nothing) (list :simple (->hydra_python_syntax_power (->hydra_python_syntax_await_primary false (list :simple (list :name class_name))) (list :nothing))))))))) (list))))))))))))] (let [eq_method (list :compound (list :function (->hydra_python_syntax_function_definition (list :nothing) (->hydra_python_syntax_function_def_raw false "__eq__" (list) (list :just hydra_python_utils_self_other_params) (list :nothing) (list :nothing) ((hydra_python_utils_indented_block (list :nothing)) (list (list return_isinstance)))))))] (let [return_hash (hydra_python_utils_py_simple_statement_to_py_statement (list :return (list (list :simple ((hydra_python_utils_function_call (list :simple (list :name "hash"))) (list (hydra_python_utils_double_quoted_string class_name_str)))))))] (let [hash_method (list :compound (list :function (->hydra_python_syntax_function_definition (list :nothing) (->hydra_python_syntax_function_def_raw false "__hash__" (list) (list :just hydra_python_utils_self_only_params) (list :nothing) (list :nothing) ((hydra_python_utils_indented_block (list :nothing)) (list (list return_hash)))))))] (list slots_stmt eq_method hash_method)))))))))
