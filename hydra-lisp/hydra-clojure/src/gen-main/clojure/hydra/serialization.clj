(ns hydra.serialization
  (:require [hydra.ast :refer :all] [hydra.lib.equality :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.literals :refer :all] [hydra.lib.logic :refer :all] [hydra.lib.math :refer :all] [hydra.lib.maybes :refer :all] [hydra.lib.strings :refer :all] [hydra.util :refer :all]
))

(declare hydra_serialization_angle_braces hydra_serialization_brackets hydra_serialization_sym hydra_serialization_cst hydra_serialization_ifx hydra_serialization_symbol_sep hydra_serialization_comma_sep hydra_serialization_angle_braces_list hydra_serialization_curly_braces hydra_serialization_curly_braces_list hydra_serialization_expression_length hydra_serialization_double_space hydra_serialization_half_block_style hydra_serialization_inline_style hydra_serialization_braces_list_adaptive hydra_serialization_square_brackets hydra_serialization_bracket_list hydra_serialization_bracket_list_adaptive hydra_serialization_curly_block hydra_serialization_custom_indent hydra_serialization_sep hydra_serialization_newline_sep hydra_serialization_custom_indent_block hydra_serialization_dot_sep hydra_serialization_double_newline_sep hydra_serialization_full_block_style hydra_serialization_indent hydra_serialization_indent_block hydra_serialization_indent_subsequent_lines hydra_serialization_space_sep hydra_serialization_infix_ws hydra_serialization_infix_ws_list hydra_serialization_no_padding hydra_serialization_no_sep hydra_serialization_num hydra_serialization_op hydra_serialization_or_op hydra_serialization_or_sep hydra_serialization_parentheses hydra_serialization_paren_list hydra_serialization_parens hydra_serialization_parenthesize hydra_serialization_prefix hydra_serialization_print_expr hydra_serialization_semicolon_sep hydra_serialization_structural_sep hydra_serialization_structural_space_sep hydra_serialization_suffix hydra_serialization_tab_indent hydra_serialization_tab_indent_double_space hydra_serialization_tab_indent_single_space hydra_serialization_unsupported_type hydra_serialization_unsupported_variant hydra_serialization_with_comma hydra_serialization_with_semi)

(def hydra_serialization_angle_braces (->hydra_ast_brackets "<" ">"))

(def hydra_serialization_brackets (fn [br] (fn [style] (fn [e] (list :brackets (->hydra_ast_bracket_expr br e style))))))

(def hydra_serialization_sym (fn [s] s))

(def hydra_serialization_cst (fn [s] (list :const (hydra_serialization_sym s))))

(def hydra_serialization_ifx (fn [op] (fn [lhs] (fn [rhs] (list :op (->hydra_ast_op_expr op lhs rhs))))))

(def hydra_serialization_symbol_sep (fn [symb] (fn [style] (fn [l] (let [break_count (hydra_lib_lists_length ((hydra_lib_lists_filter (fn [x_] x_)) (list ((fn [v] (:newline_before_content v)) style) ((fn [v] (:newline_after_content v)) style))))] (let [break (if ((hydra_lib_equality_equal break_count) 0) (list :space nil) (if ((hydra_lib_equality_equal break_count) 1) (list :break nil) (list :double_break nil)))] (let [comma_op (->hydra_ast_op (hydra_serialization_sym symb) (->hydra_ast_padding (list :none nil) break) 0 (list :none nil))] (((hydra_lib_maybes_maybe (hydra_serialization_cst "")) (fn [h] (((hydra_lib_lists_foldl (fn [acc] (fn [el] (((hydra_serialization_ifx comma_op) acc) el)))) h) ((hydra_lib_lists_drop 1) l)))) (hydra_lib_lists_safe_head l)))))))))

(def hydra_serialization_comma_sep (hydra_serialization_symbol_sep ","))

(def hydra_serialization_angle_braces_list (fn [style] (fn [els] (if (hydra_lib_lists_null els) (hydra_serialization_cst "<>") (((hydra_serialization_brackets hydra_serialization_angle_braces) style) ((hydra_serialization_comma_sep style) els))))))

(def hydra_serialization_curly_braces (->hydra_ast_brackets "{" "}"))

(def hydra_serialization_curly_braces_list (fn [msymb] (fn [style] (fn [els] (if (hydra_lib_lists_null els) (hydra_serialization_cst "{}") (((hydra_serialization_brackets hydra_serialization_curly_braces) style) (((hydra_serialization_symbol_sep ((hydra_lib_maybes_from_maybe ",") msymb)) style) els)))))))

(def hydra_serialization_expression_length (fn [e] (let [symbol_length (fn [s] (hydra_lib_strings_length ((fn [v] v) s)))] (let [ws_length (fn [ws] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :none) ((fn [_] 0) match_value) (= (first match_target) :space) ((fn [_] 1) match_value) (= (first match_target) :break) ((fn [_] 10000) match_value) (= (first match_target) :break_and_indent) ((fn [s] 10000) match_value) (= (first match_target) :double_break) ((fn [_] 10000) match_value))) (second match_target))) ws))] (let [block_style_length (fn [style] (let [mindent_len (((hydra_lib_maybes_maybe 0) hydra_lib_strings_length) ((fn [v] (:indent v)) style))] (let [nl_before_len (if ((fn [v] (:newline_before_content v)) style) 1 0)] (let [nl_after_len (if ((fn [v] (:newline_after_content v)) style) 1 0)] ((hydra_lib_math_add mindent_len) ((hydra_lib_math_add nl_before_len) nl_after_len))))))] (let [brackets_length (fn [brackets] ((hydra_lib_math_add (symbol_length ((fn [v] (:open v)) brackets))) (symbol_length ((fn [v] (:close v)) brackets))))] (let [bracket_expr_length (fn [be] ((hydra_lib_math_add (brackets_length ((fn [v] (:brackets v)) be))) ((hydra_lib_math_add (hydra_serialization_expression_length ((fn [v] (:enclosed v)) be))) (block_style_length ((fn [v] (:style v)) be)))))] (let [indented_expression_length (fn [ie] (let [base_len (hydra_serialization_expression_length ((fn [v] (:expr v)) ie))] (let [indent_len ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :all_lines) ((fn [s] (hydra_lib_strings_length s)) match_value) (= (first match_target) :subsequent_lines) ((fn [s] (hydra_lib_strings_length s)) match_value))) (second match_target))) ((fn [v] (:style v)) ie))] ((hydra_lib_math_add base_len) indent_len))))] (let [op_length (fn [op] (let [sym_len (symbol_length ((fn [v] (:symbol v)) op))] (let [padding ((fn [v] (:padding v)) op)] (let [left_len (ws_length ((fn [v] (:left v)) padding))] (let [right_len (ws_length ((fn [v] (:right v)) padding))] ((hydra_lib_math_add sym_len) ((hydra_lib_math_add left_len) right_len)))))))] (let [op_expr_length (fn [oe] (let [op_len (op_length ((fn [v] (:op v)) oe))] (let [left_len (hydra_serialization_expression_length ((fn [v] (:lhs v)) oe))] (let [right_len (hydra_serialization_expression_length ((fn [v] (:rhs v)) oe))] ((hydra_lib_math_add op_len) ((hydra_lib_math_add left_len) right_len))))))] (let [seq_expr_length (fn [se] (let [sop_len (op_length ((fn [v] (:op v)) se))] (let [element_lens ((hydra_lib_lists_map hydra_serialization_expression_length) ((fn [v] (:elements v)) se))] (let [total_el_len (((hydra_lib_lists_foldl hydra_lib_math_add) 0) element_lens)] (let [num_seps ((hydra_lib_math_sub (hydra_lib_lists_length ((fn [v] (:elements v)) se))) 1)] ((hydra_lib_math_add total_el_len) ((hydra_lib_math_mul sop_len) (if ((hydra_lib_equality_gt num_seps) 0) num_seps 0))))))))] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :const) ((fn [s] (symbol_length s)) match_value) (= (first match_target) :indent) ((fn [ie] (indented_expression_length ie)) match_value) (= (first match_target) :op) ((fn [oe] (op_expr_length oe)) match_value) (= (first match_target) :brackets) ((fn [be] (bracket_expr_length be)) match_value) (= (first match_target) :seq) ((fn [se] (seq_expr_length se)) match_value))) (second match_target))) e))))))))))))

(def hydra_serialization_double_space "  ")

(def hydra_serialization_half_block_style (->hydra_ast_block_style hydra_serialization_double_space true false))

(def hydra_serialization_inline_style (->hydra_ast_block_style nil false false))

(def hydra_serialization_braces_list_adaptive (fn [els] (let [inline_list (((hydra_serialization_curly_braces_list nil) hydra_serialization_inline_style) els)] (if ((hydra_lib_equality_gt (hydra_serialization_expression_length inline_list)) 70) (((hydra_serialization_curly_braces_list nil) hydra_serialization_half_block_style) els) inline_list))))

(def hydra_serialization_square_brackets (->hydra_ast_brackets "[" "]"))

(def hydra_serialization_bracket_list (fn [style] (fn [els] (if (hydra_lib_lists_null els) (hydra_serialization_cst "[]") (((hydra_serialization_brackets hydra_serialization_square_brackets) style) ((hydra_serialization_comma_sep style) els))))))

(def hydra_serialization_bracket_list_adaptive (fn [els] (let [inline_list ((hydra_serialization_bracket_list hydra_serialization_inline_style) els)] (if ((hydra_lib_equality_gt (hydra_serialization_expression_length inline_list)) 70) ((hydra_serialization_bracket_list hydra_serialization_half_block_style) els) inline_list))))

(def hydra_serialization_curly_block (fn [style] (fn [e] (((hydra_serialization_curly_braces_list nil) style) (list e)))))

(def hydra_serialization_custom_indent (fn [idt] (fn [s] (hydra_lib_strings_cat ((hydra_lib_lists_intersperse "\n") ((hydra_lib_lists_map (fn [line] ((hydra_lib_strings_cat2 idt) line))) (hydra_lib_strings_lines s)))))))

(def hydra_serialization_sep (fn [op] (fn [els] (((hydra_lib_maybes_maybe (hydra_serialization_cst "")) (fn [h] (((hydra_lib_lists_foldl (fn [acc] (fn [el] (((hydra_serialization_ifx op) acc) el)))) h) ((hydra_lib_lists_drop 1) els)))) (hydra_lib_lists_safe_head els)))))

(def hydra_serialization_newline_sep (hydra_serialization_sep (->hydra_ast_op (hydra_serialization_sym "") (->hydra_ast_padding (list :none nil) (list :break nil)) 0 (list :none nil))))

(def hydra_serialization_custom_indent_block (fn [idt] (fn [els] (let [idt_op (->hydra_ast_op (hydra_serialization_sym "") (->hydra_ast_padding (list :space nil) (list :break_and_indent idt)) 0 (list :none nil))] (((hydra_lib_maybes_maybe (hydra_serialization_cst "")) (fn [head] (if ((hydra_lib_equality_equal (hydra_lib_lists_length els)) 1) head (((hydra_serialization_ifx idt_op) head) (hydra_serialization_newline_sep ((hydra_lib_lists_drop 1) els)))))) (hydra_lib_lists_safe_head els))))))

(def hydra_serialization_dot_sep (hydra_serialization_sep (->hydra_ast_op (hydra_serialization_sym ".") (->hydra_ast_padding (list :none nil) (list :none nil)) 0 (list :none nil))))

(def hydra_serialization_double_newline_sep (hydra_serialization_sep (->hydra_ast_op (hydra_serialization_sym "") (->hydra_ast_padding (list :break nil) (list :break nil)) 0 (list :none nil))))

(def hydra_serialization_full_block_style (->hydra_ast_block_style hydra_serialization_double_space true true))

(def hydra_serialization_indent (hydra_serialization_custom_indent hydra_serialization_double_space))

(def hydra_serialization_indent_block (hydra_serialization_custom_indent_block hydra_serialization_double_space))

(def hydra_serialization_indent_subsequent_lines (fn [idt] (fn [e] (list :indent (->hydra_ast_indented_expression (list :subsequent_lines idt) e)))))

(def hydra_serialization_space_sep (hydra_serialization_sep (->hydra_ast_op (hydra_serialization_sym "") (->hydra_ast_padding (list :space nil) (list :none nil)) 0 (list :none nil))))

(def hydra_serialization_infix_ws (fn [op] (fn [l] (fn [r] (hydra_serialization_space_sep (list l (hydra_serialization_cst op) r))))))

(def hydra_serialization_infix_ws_list (fn [op] (fn [opers] (let [op_expr (hydra_serialization_cst op)] (let [fold_fun (fn [e] (fn [r] (if (hydra_lib_lists_null e) (list r) ((hydra_lib_lists_cons r) ((hydra_lib_lists_cons op_expr) e)))))] (hydra_serialization_space_sep (((hydra_lib_lists_foldl fold_fun) (list)) (hydra_lib_lists_reverse opers))))))))

(def hydra_serialization_no_padding (->hydra_ast_padding (list :none nil) (list :none nil)))

(def hydra_serialization_no_sep (hydra_serialization_sep (->hydra_ast_op (hydra_serialization_sym "") (->hydra_ast_padding (list :none nil) (list :none nil)) 0 (list :none nil))))

(def hydra_serialization_num (fn [i] (hydra_serialization_cst (hydra_lib_literals_show_int32 i))))

(def hydra_serialization_op (fn [s] (fn [p] (fn [assoc] (->hydra_ast_op (hydra_serialization_sym s) (->hydra_ast_padding (list :space nil) (list :space nil)) p assoc)))))

(def hydra_serialization_or_op (fn [newlines] (->hydra_ast_op (hydra_serialization_sym "|") (->hydra_ast_padding (list :space nil) (if newlines (list :break nil) (list :space nil))) 0 (list :none nil))))

(def hydra_serialization_or_sep (fn [style] (fn [l] (let [newlines ((fn [v] (:newline_before_content v)) style)] (((hydra_lib_maybes_maybe (hydra_serialization_cst "")) (fn [h] (((hydra_lib_lists_foldl (fn [acc] (fn [el] (((hydra_serialization_ifx (hydra_serialization_or_op newlines)) acc) el)))) h) ((hydra_lib_lists_drop 1) l)))) (hydra_lib_lists_safe_head l))))))

(def hydra_serialization_parentheses (->hydra_ast_brackets "(" ")"))

(def hydra_serialization_paren_list (fn [newlines] (fn [els] (let [style (if ((hydra_lib_logic_and newlines) ((hydra_lib_equality_gt (hydra_lib_lists_length els)) 1)) hydra_serialization_half_block_style hydra_serialization_inline_style)] (if (hydra_lib_lists_null els) (hydra_serialization_cst "()") (((hydra_serialization_brackets hydra_serialization_parentheses) style) ((hydra_serialization_comma_sep style) els)))))))

(def hydra_serialization_parens ((hydra_serialization_brackets hydra_serialization_parentheses) hydra_serialization_inline_style))

(def hydra_serialization_parenthesize (fn [exp] (let [assoc_left (fn [a] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :right) ((fn [_] false) match_value) :else true)) (second match_target))) a))] (let [assoc_right (fn [a] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :left) ((fn [_] false) match_value) :else true)) (second match_target))) a))] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :brackets) ((fn [bracket_expr] (list :brackets (->hydra_ast_bracket_expr ((fn [v] (:brackets v)) bracket_expr) (hydra_serialization_parenthesize ((fn [v] (:enclosed v)) bracket_expr)) ((fn [v] (:style v)) bracket_expr)))) match_value) (= (first match_target) :const) ((fn [ignored] exp) match_value) (= (first match_target) :indent) ((fn [indent_expr] (list :indent (->hydra_ast_indented_expression ((fn [v] (:style v)) indent_expr) (hydra_serialization_parenthesize ((fn [v] (:expr v)) indent_expr))))) match_value) (= (first match_target) :seq) ((fn [seq_expr] (list :seq (->hydra_ast_seq_expr ((fn [v] (:op v)) seq_expr) ((hydra_lib_lists_map hydra_serialization_parenthesize) ((fn [v] (:elements v)) seq_expr))))) match_value) (= (first match_target) :op) ((fn [op_expr] (let [op ((fn [v] (:op v)) op_expr)] (let [prec ((fn [v] v) ((fn [v] (:precedence v)) op))] (let [assoc ((fn [v] (:associativity v)) op)] (let [lhs ((fn [v] (:lhs v)) op_expr)] (let [rhs ((fn [v] (:rhs v)) op_expr)] (let [lhs_ (hydra_serialization_parenthesize lhs)] (let [rhs_ (hydra_serialization_parenthesize rhs)] (let [lhs2 ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :op) ((fn [lop_expr] (let [lop ((fn [v] (:op v)) lop_expr)] (let [lprec ((fn [v] v) ((fn [v] (:precedence v)) lop))] (let [lassoc ((fn [v] (:associativity v)) lop)] (let [comparison ((hydra_lib_equality_compare prec) lprec)] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :less_than) ((fn [_] lhs_) match_value) (= (first match_target) :greater_than) ((fn [_] (hydra_serialization_parens lhs_)) match_value) (= (first match_target) :equal_to) ((fn [_] (if ((hydra_lib_logic_and (assoc_left assoc)) (assoc_left lassoc)) lhs_ (hydra_serialization_parens lhs_))) match_value))) (second match_target))) comparison)))))) match_value) :else lhs_)) (second match_target))) lhs_)] (let [rhs2 ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :op) ((fn [rop_expr] (let [rop ((fn [v] (:op v)) rop_expr)] (let [rprec ((fn [v] v) ((fn [v] (:precedence v)) rop))] (let [rassoc ((fn [v] (:associativity v)) rop)] (let [comparison ((hydra_lib_equality_compare prec) rprec)] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :less_than) ((fn [_] rhs_) match_value) (= (first match_target) :greater_than) ((fn [_] (hydra_serialization_parens rhs_)) match_value) (= (first match_target) :equal_to) ((fn [_] (if ((hydra_lib_logic_and (assoc_right assoc)) (assoc_right rassoc)) rhs_ (hydra_serialization_parens rhs_))) match_value))) (second match_target))) comparison)))))) match_value) :else rhs_)) (second match_target))) rhs_)] (list :op (->hydra_ast_op_expr op lhs2 rhs2)))))))))))) match_value))) (second match_target))) exp)))))

(def hydra_serialization_prefix (fn [p] (fn [expr] (let [pre_op (->hydra_ast_op (hydra_serialization_sym p) (->hydra_ast_padding (list :none nil) (list :none nil)) 0 (list :none nil))] (((hydra_serialization_ifx pre_op) (hydra_serialization_cst "")) expr)))))

(def hydra_serialization_print_expr (fn [e] (let [pad (fn [ws] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :none) ((fn [_] "") match_value) (= (first match_target) :space) ((fn [_] " ") match_value) (= (first match_target) :break) ((fn [_] "\n") match_value) (= (first match_target) :break_and_indent) ((fn [ignored] "\n") match_value) (= (first match_target) :double_break) ((fn [_] "\n\n") match_value))) (second match_target))) ws))] (let [idt (fn [ws] (fn [s] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :break_and_indent) ((fn [indent_str] ((hydra_serialization_custom_indent indent_str) s)) match_value) :else s)) (second match_target))) ws)))] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :const) ((fn [symbol] ((fn [v] v) symbol)) match_value) (= (first match_target) :indent) ((fn [indent_expr] (let [style ((fn [v] (:style v)) indent_expr)] (let [expr ((fn [v] (:expr v)) indent_expr)] (let [lns (hydra_lib_strings_lines (hydra_serialization_print_expr expr))] (let [ilns ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :all_lines) ((fn [idt] ((hydra_lib_lists_map (fn [line] ((hydra_lib_strings_cat2 idt) line))) lns)) match_value) (= (first match_target) :subsequent_lines) ((fn [idt] (if ((hydra_lib_equality_equal (hydra_lib_lists_length lns)) 1) lns ((hydra_lib_lists_cons (hydra_lib_lists_head lns)) ((hydra_lib_lists_map (fn [line] ((hydra_lib_strings_cat2 idt) line))) (hydra_lib_lists_tail lns))))) match_value))) (second match_target))) style)] ((hydra_lib_strings_intercalate "\n") ilns)))))) match_value) (= (first match_target) :seq) ((fn [seq_expr] (let [sop ((fn [v] (:op v)) seq_expr)] (let [ssym ((fn [v] v) ((fn [v] (:symbol v)) sop))] (let [spadding ((fn [v] (:padding v)) sop)] (let [spadl ((fn [v] (:left v)) spadding)] (let [spadr ((fn [v] (:right v)) spadding)] (let [selements ((fn [v] (:elements v)) seq_expr)] (let [separator ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 (pad spadl)) ssym)) (pad spadr))] (let [printed_elements ((hydra_lib_lists_map (fn [el] ((idt spadr) (hydra_serialization_print_expr el)))) selements)] ((hydra_lib_strings_intercalate separator) printed_elements)))))))))) match_value) (= (first match_target) :op) ((fn [op_expr] (let [op ((fn [v] (:op v)) op_expr)] (let [sym ((fn [v] v) ((fn [v] (:symbol v)) op))] (let [padding ((fn [v] (:padding v)) op)] (let [padl ((fn [v] (:left v)) padding)] (let [padr ((fn [v] (:right v)) padding)] (let [l ((fn [v] (:lhs v)) op_expr)] (let [r ((fn [v] (:rhs v)) op_expr)] (let [lhs ((idt padl) (hydra_serialization_print_expr l))] (let [rhs ((idt padr) (hydra_serialization_print_expr r))] ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 lhs) (pad padl))) sym)) (pad padr))) rhs))))))))))) match_value) (= (first match_target) :brackets) ((fn [bracket_expr] (let [brs ((fn [v] (:brackets v)) bracket_expr)] (let [l ((fn [v] v) ((fn [v] (:open v)) brs))] (let [r ((fn [v] v) ((fn [v] (:close v)) brs))] (let [e ((fn [v] (:enclosed v)) bracket_expr)] (let [style ((fn [v] (:style v)) bracket_expr)] (let [body (hydra_serialization_print_expr e)] (let [do_indent ((fn [v] (:indent v)) style)] (let [nl_before ((fn [v] (:newline_before_content v)) style)] (let [nl_after ((fn [v] (:newline_after_content v)) style)] (let [ibody (((hydra_lib_maybes_maybe body) (fn [idt] ((hydra_serialization_custom_indent idt) body))) do_indent)] (let [pre (if nl_before "\n" "")] (let [suf (if nl_after "\n" "")] ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 l) pre)) ibody)) suf)) r)))))))))))))) match_value))) (second match_target))) e)))))

(def hydra_serialization_semicolon_sep ((hydra_serialization_symbol_sep ";") hydra_serialization_inline_style))

(def hydra_serialization_structural_sep (fn [op] (fn [els] (if (hydra_lib_lists_null els) (hydra_serialization_cst "") (if ((hydra_lib_equality_equal (hydra_lib_lists_length els)) 1) (hydra_lib_lists_head els) (list :seq (->hydra_ast_seq_expr op els)))))))

(def hydra_serialization_structural_space_sep (hydra_serialization_structural_sep (->hydra_ast_op (hydra_serialization_sym "") (->hydra_ast_padding (list :space nil) (list :none nil)) 0 (list :none nil))))

(def hydra_serialization_suffix (fn [s] (fn [expr] (let [suf_op (->hydra_ast_op (hydra_serialization_sym s) (->hydra_ast_padding (list :none nil) (list :none nil)) 0 (list :none nil))] (((hydra_serialization_ifx suf_op) expr) (hydra_serialization_cst ""))))))

(def hydra_serialization_tab_indent (fn [e] (list :indent (->hydra_ast_indented_expression (list :all_lines "    ") e))))

(def hydra_serialization_tab_indent_double_space (fn [exprs] (hydra_serialization_tab_indent (hydra_serialization_double_newline_sep exprs))))

(def hydra_serialization_tab_indent_single_space (fn [exprs] (hydra_serialization_tab_indent (hydra_serialization_newline_sep exprs))))

(def hydra_serialization_unsupported_type (fn [label] (hydra_serialization_cst ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "[") label)) "]"))))

(def hydra_serialization_unsupported_variant (fn [label] (fn [obj] (hydra_serialization_cst ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "[unsupported ") label)) ": ")) (hydra_lib_literals_show_string obj))) "]")))))

(def hydra_serialization_with_comma (fn [e] (hydra_serialization_no_sep (list e (hydra_serialization_cst ",")))))

(def hydra_serialization_with_semi (fn [e] (hydra_serialization_no_sep (list e (hydra_serialization_cst ";")))))
