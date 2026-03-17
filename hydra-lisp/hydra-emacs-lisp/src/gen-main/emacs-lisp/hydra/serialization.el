(require 'cl-lib)

(require 'hydra.ast)

(require 'hydra.lib.equality)

(require 'hydra.lib.lists)

(require 'hydra.lib.literals)

(require 'hydra.lib.logic)

(require 'hydra.lib.math)

(require 'hydra.lib.maybes)

(require 'hydra.lib.strings)

(require 'hydra.util)

(defvar hydra_serialization_angle_braces (make-hydra_ast_brackets "<" ">"))

(defvar hydra_serialization_brackets (lambda (br) (lambda (style) (lambda (e) (list :brackets (make-hydra_ast_bracket_expr br e style))))))

(defvar hydra_serialization_sym (lambda (s) s))

(defvar hydra_serialization_cst (lambda (s) (list :const (hydra_serialization_sym s))))

(defvar hydra_serialization_ifx (lambda (op) (lambda (lhs) (lambda (rhs) (list :op (make-hydra_ast_op_expr op lhs rhs))))))

(defvar hydra_serialization_symbol_sep (lambda (symb) (lambda (style) (lambda (l) (let ((break_count (hydra_lib_lists_length ((hydra_lib_lists_filter (lambda (x_) x_)) (list ((lambda (v) (hydra_ast_block_style-newline_before_content v)) style) ((lambda (v) (hydra_ast_block_style-newline_after_content v)) style)))))) (let ((break (if ((hydra_lib_equality_equal break_count) 0) (list :space nil) (if ((hydra_lib_equality_equal break_count) 1) (list :break nil) (list :double_break nil))))) (let ((comma_op (make-hydra_ast_op (hydra_serialization_sym symb) (make-hydra_ast_padding (list :none nil) break) 0 (list :none nil)))) (((hydra_lib_maybes_maybe (hydra_serialization_cst "")) (lambda (h) (((hydra_lib_lists_foldl (lambda (acc) (lambda (el) (((hydra_serialization_ifx comma_op) acc) el)))) h) ((hydra_lib_lists_drop 1) l)))) (hydra_lib_lists_safe_head l)))))))))

(defvar hydra_serialization_comma_sep (hydra_serialization_symbol_sep ","))

(defvar hydra_serialization_angle_braces_list (lambda (style) (lambda (els) (if (hydra_lib_lists_null els) (hydra_serialization_cst "<>") (((hydra_serialization_brackets hydra_serialization_angle_braces) style) ((hydra_serialization_comma_sep style) els))))))

(defvar hydra_serialization_curly_braces (make-hydra_ast_brackets "{" "}"))

(defvar hydra_serialization_curly_braces_list (lambda (msymb) (lambda (style) (lambda (els) (if (hydra_lib_lists_null els) (hydra_serialization_cst "{}") (((hydra_serialization_brackets hydra_serialization_curly_braces) style) (((hydra_serialization_symbol_sep ((hydra_lib_maybes_from_maybe ",") msymb)) style) els)))))))

(defvar hydra_serialization_expression_length (lambda (e) (let ((symbol_length (lambda (s) (hydra_lib_strings_length ((lambda (v) v) s))))) (let ((ws_length (lambda (ws) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :none) ((lambda (_) 0) match_value)) ((equal (car match_target) :space) ((lambda (_) 1) match_value)) ((equal (car match_target) :break) ((lambda (_) 10000) match_value)) ((equal (car match_target) :break_and_indent) ((lambda (s) 10000) match_value)) ((equal (car match_target) :double_break) ((lambda (_) 10000) match_value)))) (cadr match_target))) ws)))) (let ((block_style_length (lambda (style) (let ((mindent_len (((hydra_lib_maybes_maybe 0) hydra_lib_strings_length) ((lambda (v) (hydra_ast_block_style-indent v)) style)))) (let ((nl_before_len (if ((lambda (v) (hydra_ast_block_style-newline_before_content v)) style) 1 0))) (let ((nl_after_len (if ((lambda (v) (hydra_ast_block_style-newline_after_content v)) style) 1 0))) ((hydra_lib_math_add mindent_len) ((hydra_lib_math_add nl_before_len) nl_after_len)))))))) (let ((brackets_length (lambda (brackets) ((hydra_lib_math_add (symbol_length ((lambda (v) (hydra_ast_brackets-open v)) brackets))) (symbol_length ((lambda (v) (hydra_ast_brackets-close v)) brackets)))))) (let ((bracket_expr_length (lambda (be) ((hydra_lib_math_add (brackets_length ((lambda (v) (hydra_ast_bracket_expr-brackets v)) be))) ((hydra_lib_math_add (hydra_serialization_expression_length ((lambda (v) (hydra_ast_bracket_expr-enclosed v)) be))) (block_style_length ((lambda (v) (hydra_ast_bracket_expr-style v)) be))))))) (let ((indented_expression_length (lambda (ie) (let ((base_len (hydra_serialization_expression_length ((lambda (v) (hydra_ast_indented_expression-expr v)) ie)))) (let ((indent_len ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :all_lines) ((lambda (s) (hydra_lib_strings_length s)) match_value)) ((equal (car match_target) :subsequent_lines) ((lambda (s) (hydra_lib_strings_length s)) match_value)))) (cadr match_target))) ((lambda (v) (hydra_ast_indented_expression-style v)) ie)))) ((hydra_lib_math_add base_len) indent_len)))))) (let ((op_length (lambda (op) (let ((sym_len (symbol_length ((lambda (v) (hydra_ast_op-symbol v)) op)))) (let ((padding ((lambda (v) (hydra_ast_op-padding v)) op))) (let ((left_len (ws_length ((lambda (v) (hydra_ast_padding-left v)) padding)))) (let ((right_len (ws_length ((lambda (v) (hydra_ast_padding-right v)) padding)))) ((hydra_lib_math_add sym_len) ((hydra_lib_math_add left_len) right_len))))))))) (let ((op_expr_length (lambda (oe) (let ((op_len (op_length ((lambda (v) (hydra_ast_op_expr-op v)) oe)))) (let ((left_len (hydra_serialization_expression_length ((lambda (v) (hydra_ast_op_expr-lhs v)) oe)))) (let ((right_len (hydra_serialization_expression_length ((lambda (v) (hydra_ast_op_expr-rhs v)) oe)))) ((hydra_lib_math_add op_len) ((hydra_lib_math_add left_len) right_len)))))))) (let ((seq_expr_length (lambda (se) (let ((sop_len (op_length ((lambda (v) (hydra_ast_seq_expr-op v)) se)))) (let ((element_lens ((hydra_lib_lists_map hydra_serialization_expression_length) ((lambda (v) (hydra_ast_seq_expr-elements v)) se)))) (let ((total_el_len (((hydra_lib_lists_foldl hydra_lib_math_add) 0) element_lens))) (let ((num_seps ((hydra_lib_math_sub (hydra_lib_lists_length ((lambda (v) (hydra_ast_seq_expr-elements v)) se))) 1))) ((hydra_lib_math_add total_el_len) ((hydra_lib_math_mul sop_len) (if ((hydra_lib_equality_gt num_seps) 0) num_seps 0)))))))))) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :const) ((lambda (s) (symbol_length s)) match_value)) ((equal (car match_target) :indent) ((lambda (ie) (indented_expression_length ie)) match_value)) ((equal (car match_target) :op) ((lambda (oe) (op_expr_length oe)) match_value)) ((equal (car match_target) :brackets) ((lambda (be) (bracket_expr_length be)) match_value)) ((equal (car match_target) :seq) ((lambda (se) (seq_expr_length se)) match_value)))) (cadr match_target))) e))))))))))))

(defvar hydra_serialization_double_space "  ")

(defvar hydra_serialization_half_block_style (make-hydra_ast_block_style hydra_serialization_double_space t nil))

(defvar hydra_serialization_inline_style (make-hydra_ast_block_style nil nil nil))

(defvar hydra_serialization_braces_list_adaptive (lambda (els) (let ((inline_list (((hydra_serialization_curly_braces_list nil) hydra_serialization_inline_style) els))) (if ((hydra_lib_equality_gt (hydra_serialization_expression_length inline_list)) 70) (((hydra_serialization_curly_braces_list nil) hydra_serialization_half_block_style) els) inline_list))))

(defvar hydra_serialization_square_brackets (make-hydra_ast_brackets "[" "]"))

(defvar hydra_serialization_bracket_list (lambda (style) (lambda (els) (if (hydra_lib_lists_null els) (hydra_serialization_cst "[]") (((hydra_serialization_brackets hydra_serialization_square_brackets) style) ((hydra_serialization_comma_sep style) els))))))

(defvar hydra_serialization_bracket_list_adaptive (lambda (els) (let ((inline_list ((hydra_serialization_bracket_list hydra_serialization_inline_style) els))) (if ((hydra_lib_equality_gt (hydra_serialization_expression_length inline_list)) 70) ((hydra_serialization_bracket_list hydra_serialization_half_block_style) els) inline_list))))

(defvar hydra_serialization_curly_block (lambda (style) (lambda (e) (((hydra_serialization_curly_braces_list nil) style) (list e)))))

(defvar hydra_serialization_custom_indent (lambda (idt) (lambda (s) (hydra_lib_strings_cat ((hydra_lib_lists_intersperse "\n") ((hydra_lib_lists_map (lambda (line) ((hydra_lib_strings_cat2 idt) line))) (hydra_lib_strings_lines s)))))))

(defvar hydra_serialization_sep (lambda (op) (lambda (els) (((hydra_lib_maybes_maybe (hydra_serialization_cst "")) (lambda (h) (((hydra_lib_lists_foldl (lambda (acc) (lambda (el) (((hydra_serialization_ifx op) acc) el)))) h) ((hydra_lib_lists_drop 1) els)))) (hydra_lib_lists_safe_head els)))))

(defvar hydra_serialization_newline_sep (hydra_serialization_sep (make-hydra_ast_op (hydra_serialization_sym "") (make-hydra_ast_padding (list :none nil) (list :break nil)) 0 (list :none nil))))

(defvar hydra_serialization_custom_indent_block (lambda (idt) (lambda (els) (let ((idt_op (make-hydra_ast_op (hydra_serialization_sym "") (make-hydra_ast_padding (list :space nil) (list :break_and_indent idt)) 0 (list :none nil)))) (((hydra_lib_maybes_maybe (hydra_serialization_cst "")) (lambda (head) (if ((hydra_lib_equality_equal (hydra_lib_lists_length els)) 1) head (((hydra_serialization_ifx idt_op) head) (hydra_serialization_newline_sep ((hydra_lib_lists_drop 1) els)))))) (hydra_lib_lists_safe_head els))))))

(defvar hydra_serialization_dot_sep (hydra_serialization_sep (make-hydra_ast_op (hydra_serialization_sym ".") (make-hydra_ast_padding (list :none nil) (list :none nil)) 0 (list :none nil))))

(defvar hydra_serialization_double_newline_sep (hydra_serialization_sep (make-hydra_ast_op (hydra_serialization_sym "") (make-hydra_ast_padding (list :break nil) (list :break nil)) 0 (list :none nil))))

(defvar hydra_serialization_full_block_style (make-hydra_ast_block_style hydra_serialization_double_space t t))

(defvar hydra_serialization_indent (hydra_serialization_custom_indent hydra_serialization_double_space))

(defvar hydra_serialization_indent_block (hydra_serialization_custom_indent_block hydra_serialization_double_space))

(defvar hydra_serialization_indent_subsequent_lines (lambda (idt) (lambda (e) (list :indent (make-hydra_ast_indented_expression (list :subsequent_lines idt) e)))))

(defvar hydra_serialization_space_sep (hydra_serialization_sep (make-hydra_ast_op (hydra_serialization_sym "") (make-hydra_ast_padding (list :space nil) (list :none nil)) 0 (list :none nil))))

(defvar hydra_serialization_infix_ws (lambda (op) (lambda (l) (lambda (r) (hydra_serialization_space_sep (list l (hydra_serialization_cst op) r))))))

(defvar hydra_serialization_infix_ws_list (lambda (op) (lambda (opers) (let ((op_expr (hydra_serialization_cst op))) (let ((fold_fun (lambda (e) (lambda (r) (if (hydra_lib_lists_null e) (list r) ((hydra_lib_lists_cons r) ((hydra_lib_lists_cons op_expr) e))))))) (hydra_serialization_space_sep (((hydra_lib_lists_foldl fold_fun) (list)) (hydra_lib_lists_reverse opers))))))))

(defvar hydra_serialization_no_padding (make-hydra_ast_padding (list :none nil) (list :none nil)))

(defvar hydra_serialization_no_sep (hydra_serialization_sep (make-hydra_ast_op (hydra_serialization_sym "") (make-hydra_ast_padding (list :none nil) (list :none nil)) 0 (list :none nil))))

(defvar hydra_serialization_num (lambda (i) (hydra_serialization_cst (hydra_lib_literals_show_int32 i))))

(defvar hydra_serialization_op (lambda (s) (lambda (p) (lambda (assoc) (make-hydra_ast_op (hydra_serialization_sym s) (make-hydra_ast_padding (list :space nil) (list :space nil)) p assoc)))))

(defvar hydra_serialization_or_op (lambda (newlines) (make-hydra_ast_op (hydra_serialization_sym "|") (make-hydra_ast_padding (list :space nil) (if newlines (list :break nil) (list :space nil))) 0 (list :none nil))))

(defvar hydra_serialization_or_sep (lambda (style) (lambda (l) (let ((newlines ((lambda (v) (hydra_ast_block_style-newline_before_content v)) style))) (((hydra_lib_maybes_maybe (hydra_serialization_cst "")) (lambda (h) (((hydra_lib_lists_foldl (lambda (acc) (lambda (el) (((hydra_serialization_ifx (hydra_serialization_or_op newlines)) acc) el)))) h) ((hydra_lib_lists_drop 1) l)))) (hydra_lib_lists_safe_head l))))))

(defvar hydra_serialization_parentheses (make-hydra_ast_brackets "(" ")"))

(defvar hydra_serialization_paren_list (lambda (newlines) (lambda (els) (let ((style (if ((hydra_lib_logic_and newlines) ((hydra_lib_equality_gt (hydra_lib_lists_length els)) 1)) hydra_serialization_half_block_style hydra_serialization_inline_style))) (if (hydra_lib_lists_null els) (hydra_serialization_cst "()") (((hydra_serialization_brackets hydra_serialization_parentheses) style) ((hydra_serialization_comma_sep style) els)))))))

(defvar hydra_serialization_parens ((hydra_serialization_brackets hydra_serialization_parentheses) hydra_serialization_inline_style))

(defvar hydra_serialization_parenthesize (lambda (exp) (let ((assoc_left (lambda (a) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :right) ((lambda (_) nil) match_value)) (t t))) (cadr match_target))) a)))) (let ((assoc_right (lambda (a) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :left) ((lambda (_) nil) match_value)) (t t))) (cadr match_target))) a)))) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :brackets) ((lambda (bracket_expr) (list :brackets (make-hydra_ast_bracket_expr ((lambda (v) (hydra_ast_bracket_expr-brackets v)) bracket_expr) (hydra_serialization_parenthesize ((lambda (v) (hydra_ast_bracket_expr-enclosed v)) bracket_expr)) ((lambda (v) (hydra_ast_bracket_expr-style v)) bracket_expr)))) match_value)) ((equal (car match_target) :const) ((lambda (ignored) exp) match_value)) ((equal (car match_target) :indent) ((lambda (indent_expr) (list :indent (make-hydra_ast_indented_expression ((lambda (v) (hydra_ast_indented_expression-style v)) indent_expr) (hydra_serialization_parenthesize ((lambda (v) (hydra_ast_indented_expression-expr v)) indent_expr))))) match_value)) ((equal (car match_target) :seq) ((lambda (seq_expr) (list :seq (make-hydra_ast_seq_expr ((lambda (v) (hydra_ast_seq_expr-op v)) seq_expr) ((hydra_lib_lists_map hydra_serialization_parenthesize) ((lambda (v) (hydra_ast_seq_expr-elements v)) seq_expr))))) match_value)) ((equal (car match_target) :op) ((lambda (op_expr) (let ((op ((lambda (v) (hydra_ast_op_expr-op v)) op_expr))) (let ((prec ((lambda (v) v) ((lambda (v) (hydra_ast_op-precedence v)) op)))) (let ((assoc ((lambda (v) (hydra_ast_op-associativity v)) op))) (let ((lhs ((lambda (v) (hydra_ast_op_expr-lhs v)) op_expr))) (let ((rhs ((lambda (v) (hydra_ast_op_expr-rhs v)) op_expr))) (let ((lhs_ (hydra_serialization_parenthesize lhs))) (let ((rhs_ (hydra_serialization_parenthesize rhs))) (let ((lhs2 ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :op) ((lambda (lop_expr) (let ((lop ((lambda (v) (hydra_ast_op_expr-op v)) lop_expr))) (let ((lprec ((lambda (v) v) ((lambda (v) (hydra_ast_op-precedence v)) lop)))) (let ((lassoc ((lambda (v) (hydra_ast_op-associativity v)) lop))) (let ((comparison ((hydra_lib_equality_compare prec) lprec))) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :less_than) ((lambda (_) lhs_) match_value)) ((equal (car match_target) :greater_than) ((lambda (_) (hydra_serialization_parens lhs_)) match_value)) ((equal (car match_target) :equal_to) ((lambda (_) (if ((hydra_lib_logic_and (assoc_left assoc)) (assoc_left lassoc)) lhs_ (hydra_serialization_parens lhs_))) match_value)))) (cadr match_target))) comparison)))))) match_value)) (t lhs_))) (cadr match_target))) lhs_))) (let ((rhs2 ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :op) ((lambda (rop_expr) (let ((rop ((lambda (v) (hydra_ast_op_expr-op v)) rop_expr))) (let ((rprec ((lambda (v) v) ((lambda (v) (hydra_ast_op-precedence v)) rop)))) (let ((rassoc ((lambda (v) (hydra_ast_op-associativity v)) rop))) (let ((comparison ((hydra_lib_equality_compare prec) rprec))) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :less_than) ((lambda (_) rhs_) match_value)) ((equal (car match_target) :greater_than) ((lambda (_) (hydra_serialization_parens rhs_)) match_value)) ((equal (car match_target) :equal_to) ((lambda (_) (if ((hydra_lib_logic_and (assoc_right assoc)) (assoc_right rassoc)) rhs_ (hydra_serialization_parens rhs_))) match_value)))) (cadr match_target))) comparison)))))) match_value)) (t rhs_))) (cadr match_target))) rhs_))) (list :op (make-hydra_ast_op_expr op lhs2 rhs2)))))))))))) match_value)))) (cadr match_target))) exp)))))

(defvar hydra_serialization_prefix (lambda (p) (lambda (expr) (let ((pre_op (make-hydra_ast_op (hydra_serialization_sym p) (make-hydra_ast_padding (list :none nil) (list :none nil)) 0 (list :none nil)))) (((hydra_serialization_ifx pre_op) (hydra_serialization_cst "")) expr)))))

(defvar hydra_serialization_print_expr (lambda (e) (let ((pad (lambda (ws) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :none) ((lambda (_) "") match_value)) ((equal (car match_target) :space) ((lambda (_) " ") match_value)) ((equal (car match_target) :break) ((lambda (_) "\n") match_value)) ((equal (car match_target) :break_and_indent) ((lambda (ignored) "\n") match_value)) ((equal (car match_target) :double_break) ((lambda (_) "\n\n") match_value)))) (cadr match_target))) ws)))) (let ((idt (lambda (ws) (lambda (s) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :break_and_indent) ((lambda (indent_str) ((hydra_serialization_custom_indent indent_str) s)) match_value)) (t s))) (cadr match_target))) ws))))) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :const) ((lambda (symbol) ((lambda (v) v) symbol)) match_value)) ((equal (car match_target) :indent) ((lambda (indent_expr) (let ((style ((lambda (v) (hydra_ast_indented_expression-style v)) indent_expr))) (let ((expr ((lambda (v) (hydra_ast_indented_expression-expr v)) indent_expr))) (let ((lns (hydra_lib_strings_lines (hydra_serialization_print_expr expr)))) (let ((ilns ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :all_lines) ((lambda (idt) ((hydra_lib_lists_map (lambda (line) ((hydra_lib_strings_cat2 idt) line))) lns)) match_value)) ((equal (car match_target) :subsequent_lines) ((lambda (idt) (if ((hydra_lib_equality_equal (hydra_lib_lists_length lns)) 1) lns ((hydra_lib_lists_cons (hydra_lib_lists_head lns)) ((hydra_lib_lists_map (lambda (line) ((hydra_lib_strings_cat2 idt) line))) (hydra_lib_lists_tail lns))))) match_value)))) (cadr match_target))) style))) ((hydra_lib_strings_intercalate "\n") ilns)))))) match_value)) ((equal (car match_target) :seq) ((lambda (seq_expr) (let ((sop ((lambda (v) (hydra_ast_seq_expr-op v)) seq_expr))) (let ((ssym ((lambda (v) v) ((lambda (v) (hydra_ast_op-symbol v)) sop)))) (let ((spadding ((lambda (v) (hydra_ast_op-padding v)) sop))) (let ((spadl ((lambda (v) (hydra_ast_padding-left v)) spadding))) (let ((spadr ((lambda (v) (hydra_ast_padding-right v)) spadding))) (let ((selements ((lambda (v) (hydra_ast_seq_expr-elements v)) seq_expr))) (let ((separator ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 (pad spadl)) ssym)) (pad spadr)))) (let ((printed_elements ((hydra_lib_lists_map (lambda (el) ((idt spadr) (hydra_serialization_print_expr el)))) selements))) ((hydra_lib_strings_intercalate separator) printed_elements)))))))))) match_value)) ((equal (car match_target) :op) ((lambda (op_expr) (let ((op ((lambda (v) (hydra_ast_op_expr-op v)) op_expr))) (let ((sym ((lambda (v) v) ((lambda (v) (hydra_ast_op-symbol v)) op)))) (let ((padding ((lambda (v) (hydra_ast_op-padding v)) op))) (let ((padl ((lambda (v) (hydra_ast_padding-left v)) padding))) (let ((padr ((lambda (v) (hydra_ast_padding-right v)) padding))) (let ((l ((lambda (v) (hydra_ast_op_expr-lhs v)) op_expr))) (let ((r ((lambda (v) (hydra_ast_op_expr-rhs v)) op_expr))) (let ((lhs ((idt padl) (hydra_serialization_print_expr l)))) (let ((rhs ((idt padr) (hydra_serialization_print_expr r)))) ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 lhs) (pad padl))) sym)) (pad padr))) rhs))))))))))) match_value)) ((equal (car match_target) :brackets) ((lambda (bracket_expr) (let ((brs ((lambda (v) (hydra_ast_bracket_expr-brackets v)) bracket_expr))) (let ((l ((lambda (v) v) ((lambda (v) (hydra_ast_brackets-open v)) brs)))) (let ((r ((lambda (v) v) ((lambda (v) (hydra_ast_brackets-close v)) brs)))) (let ((e ((lambda (v) (hydra_ast_bracket_expr-enclosed v)) bracket_expr))) (let ((style ((lambda (v) (hydra_ast_bracket_expr-style v)) bracket_expr))) (let ((body (hydra_serialization_print_expr e))) (let ((do_indent ((lambda (v) (hydra_ast_block_style-indent v)) style))) (let ((nl_before ((lambda (v) (hydra_ast_block_style-newline_before_content v)) style))) (let ((nl_after ((lambda (v) (hydra_ast_block_style-newline_after_content v)) style))) (let ((ibody (((hydra_lib_maybes_maybe body) (lambda (idt) ((hydra_serialization_custom_indent idt) body))) do_indent))) (let ((pre (if nl_before "\n" ""))) (let ((suf (if nl_after "\n" ""))) ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 l) pre)) ibody)) suf)) r)))))))))))))) match_value)))) (cadr match_target))) e)))))

(defvar hydra_serialization_semicolon_sep ((hydra_serialization_symbol_sep ";") hydra_serialization_inline_style))

(defvar hydra_serialization_structural_sep (lambda (op) (lambda (els) (if (hydra_lib_lists_null els) (hydra_serialization_cst "") (if ((hydra_lib_equality_equal (hydra_lib_lists_length els)) 1) (hydra_lib_lists_head els) (list :seq (make-hydra_ast_seq_expr op els)))))))

(defvar hydra_serialization_structural_space_sep (hydra_serialization_structural_sep (make-hydra_ast_op (hydra_serialization_sym "") (make-hydra_ast_padding (list :space nil) (list :none nil)) 0 (list :none nil))))

(defvar hydra_serialization_suffix (lambda (s) (lambda (expr) (let ((suf_op (make-hydra_ast_op (hydra_serialization_sym s) (make-hydra_ast_padding (list :none nil) (list :none nil)) 0 (list :none nil)))) (((hydra_serialization_ifx suf_op) expr) (hydra_serialization_cst ""))))))

(defvar hydra_serialization_tab_indent (lambda (e) (list :indent (make-hydra_ast_indented_expression (list :all_lines "    ") e))))

(defvar hydra_serialization_tab_indent_double_space (lambda (exprs) (hydra_serialization_tab_indent (hydra_serialization_double_newline_sep exprs))))

(defvar hydra_serialization_tab_indent_single_space (lambda (exprs) (hydra_serialization_tab_indent (hydra_serialization_newline_sep exprs))))

(defvar hydra_serialization_unsupported_type (lambda (label) (hydra_serialization_cst ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "[") label)) "]"))))

(defvar hydra_serialization_unsupported_variant (lambda (label) (lambda (obj) (hydra_serialization_cst ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "[unsupported ") label)) ": ")) (hydra_lib_literals_show_string obj))) "]")))))

(defvar hydra_serialization_with_comma (lambda (e) (hydra_serialization_no_sep (list e (hydra_serialization_cst ",")))))

(defvar hydra_serialization_with_semi (lambda (e) (hydra_serialization_no_sep (list e (hydra_serialization_cst ";")))))

(provide 'hydra.serialization)
