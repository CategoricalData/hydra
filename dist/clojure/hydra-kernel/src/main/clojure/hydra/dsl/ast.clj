(ns hydra.dsl.ast
  (:require [hydra.core :refer :all] [hydra.phantoms :refer :all]
))

(declare hydra_dsl_ast_associativity_both hydra_dsl_ast_associativity_left hydra_dsl_ast_associativity_none hydra_dsl_ast_associativity_right hydra_dsl_ast_block_style hydra_dsl_ast_block_style_indent hydra_dsl_ast_block_style_newline_after_content hydra_dsl_ast_block_style_newline_before_content hydra_dsl_ast_block_style_with_indent hydra_dsl_ast_block_style_with_newline_after_content hydra_dsl_ast_block_style_with_newline_before_content hydra_dsl_ast_bracket_expr hydra_dsl_ast_bracket_expr_brackets hydra_dsl_ast_bracket_expr_enclosed hydra_dsl_ast_bracket_expr_style hydra_dsl_ast_bracket_expr_with_brackets hydra_dsl_ast_bracket_expr_with_enclosed hydra_dsl_ast_bracket_expr_with_style hydra_dsl_ast_brackets hydra_dsl_ast_brackets_close hydra_dsl_ast_brackets_open hydra_dsl_ast_brackets_with_close hydra_dsl_ast_brackets_with_open hydra_dsl_ast_expr_brackets hydra_dsl_ast_expr_const hydra_dsl_ast_expr_indent hydra_dsl_ast_expr_op hydra_dsl_ast_expr_seq hydra_dsl_ast_indent_style_all_lines hydra_dsl_ast_indent_style_subsequent_lines hydra_dsl_ast_indented_expression hydra_dsl_ast_indented_expression_expr hydra_dsl_ast_indented_expression_style hydra_dsl_ast_indented_expression_with_expr hydra_dsl_ast_indented_expression_with_style hydra_dsl_ast_op hydra_dsl_ast_op_associativity hydra_dsl_ast_op_expr hydra_dsl_ast_op_expr_lhs hydra_dsl_ast_op_expr_op hydra_dsl_ast_op_expr_rhs hydra_dsl_ast_op_expr_with_lhs hydra_dsl_ast_op_expr_with_op hydra_dsl_ast_op_expr_with_rhs hydra_dsl_ast_op_padding hydra_dsl_ast_op_precedence hydra_dsl_ast_op_symbol hydra_dsl_ast_op_with_associativity hydra_dsl_ast_op_with_padding hydra_dsl_ast_op_with_precedence hydra_dsl_ast_op_with_symbol hydra_dsl_ast_padding hydra_dsl_ast_padding_left hydra_dsl_ast_padding_right hydra_dsl_ast_padding_with_left hydra_dsl_ast_padding_with_right hydra_dsl_ast_precedence hydra_dsl_ast_seq_expr hydra_dsl_ast_seq_expr_elements hydra_dsl_ast_seq_expr_op hydra_dsl_ast_seq_expr_with_elements hydra_dsl_ast_seq_expr_with_op hydra_dsl_ast_symbol hydra_dsl_ast_un_precedence hydra_dsl_ast_un_symbol hydra_dsl_ast_ws_break hydra_dsl_ast_ws_break_and_indent hydra_dsl_ast_ws_double_break hydra_dsl_ast_ws_none hydra_dsl_ast_ws_space)

(def hydra_dsl_ast_associativity_both (list :inject (->hydra_core_injection "hydra.ast.Associativity" (->hydra_core_field "both" (list :unit nil)))))

(def hydra_dsl_ast_associativity_left (list :inject (->hydra_core_injection "hydra.ast.Associativity" (->hydra_core_field "left" (list :unit nil)))))

(def hydra_dsl_ast_associativity_none (list :inject (->hydra_core_injection "hydra.ast.Associativity" (->hydra_core_field "none" (list :unit nil)))))

(def hydra_dsl_ast_associativity_right (list :inject (->hydra_core_injection "hydra.ast.Associativity" (->hydra_core_field "right" (list :unit nil)))))

(def hydra_dsl_ast_block_style (fn [indent] (fn [newline_before_content] (fn [newline_after_content] (list :record (->hydra_core_record "hydra.ast.BlockStyle" (list (->hydra_core_field "indent" ((fn [v] v) indent)) (->hydra_core_field "newlineBeforeContent" ((fn [v] v) newline_before_content)) (->hydra_core_field "newlineAfterContent" ((fn [v] v) newline_after_content)))))))))

(def hydra_dsl_ast_block_style_indent (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.BlockStyle" "indent")) ((fn [v] v) x)))))

(def hydra_dsl_ast_block_style_newline_after_content (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.BlockStyle" "newlineAfterContent")) ((fn [v] v) x)))))

(def hydra_dsl_ast_block_style_newline_before_content (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.BlockStyle" "newlineBeforeContent")) ((fn [v] v) x)))))

(def hydra_dsl_ast_block_style_with_indent (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.ast.BlockStyle" (list (->hydra_core_field "indent" ((fn [v] v) new_val)) (->hydra_core_field "newlineBeforeContent" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.BlockStyle" "newlineBeforeContent")) ((fn [v] v) original)))) (->hydra_core_field "newlineAfterContent" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.BlockStyle" "newlineAfterContent")) ((fn [v] v) original))))))))))

(def hydra_dsl_ast_block_style_with_newline_after_content (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.ast.BlockStyle" (list (->hydra_core_field "indent" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.BlockStyle" "indent")) ((fn [v] v) original)))) (->hydra_core_field "newlineBeforeContent" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.BlockStyle" "newlineBeforeContent")) ((fn [v] v) original)))) (->hydra_core_field "newlineAfterContent" ((fn [v] v) new_val))))))))

(def hydra_dsl_ast_block_style_with_newline_before_content (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.ast.BlockStyle" (list (->hydra_core_field "indent" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.BlockStyle" "indent")) ((fn [v] v) original)))) (->hydra_core_field "newlineBeforeContent" ((fn [v] v) new_val)) (->hydra_core_field "newlineAfterContent" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.BlockStyle" "newlineAfterContent")) ((fn [v] v) original))))))))))

(def hydra_dsl_ast_bracket_expr (fn [brackets] (fn [enclosed] (fn [style] (list :record (->hydra_core_record "hydra.ast.BracketExpr" (list (->hydra_core_field "brackets" ((fn [v] v) brackets)) (->hydra_core_field "enclosed" ((fn [v] v) enclosed)) (->hydra_core_field "style" ((fn [v] v) style)))))))))

(def hydra_dsl_ast_bracket_expr_brackets (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.BracketExpr" "brackets")) ((fn [v] v) x)))))

(def hydra_dsl_ast_bracket_expr_enclosed (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.BracketExpr" "enclosed")) ((fn [v] v) x)))))

(def hydra_dsl_ast_bracket_expr_style (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.BracketExpr" "style")) ((fn [v] v) x)))))

(def hydra_dsl_ast_bracket_expr_with_brackets (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.ast.BracketExpr" (list (->hydra_core_field "brackets" ((fn [v] v) new_val)) (->hydra_core_field "enclosed" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.BracketExpr" "enclosed")) ((fn [v] v) original)))) (->hydra_core_field "style" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.BracketExpr" "style")) ((fn [v] v) original))))))))))

(def hydra_dsl_ast_bracket_expr_with_enclosed (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.ast.BracketExpr" (list (->hydra_core_field "brackets" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.BracketExpr" "brackets")) ((fn [v] v) original)))) (->hydra_core_field "enclosed" ((fn [v] v) new_val)) (->hydra_core_field "style" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.BracketExpr" "style")) ((fn [v] v) original))))))))))

(def hydra_dsl_ast_bracket_expr_with_style (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.ast.BracketExpr" (list (->hydra_core_field "brackets" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.BracketExpr" "brackets")) ((fn [v] v) original)))) (->hydra_core_field "enclosed" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.BracketExpr" "enclosed")) ((fn [v] v) original)))) (->hydra_core_field "style" ((fn [v] v) new_val))))))))

(def hydra_dsl_ast_brackets (fn [open] (fn [close] (list :record (->hydra_core_record "hydra.ast.Brackets" (list (->hydra_core_field "open" ((fn [v] v) open)) (->hydra_core_field "close" ((fn [v] v) close))))))))

(def hydra_dsl_ast_brackets_close (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.Brackets" "close")) ((fn [v] v) x)))))

(def hydra_dsl_ast_brackets_open (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.Brackets" "open")) ((fn [v] v) x)))))

(def hydra_dsl_ast_brackets_with_close (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.ast.Brackets" (list (->hydra_core_field "open" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.Brackets" "open")) ((fn [v] v) original)))) (->hydra_core_field "close" ((fn [v] v) new_val))))))))

(def hydra_dsl_ast_brackets_with_open (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.ast.Brackets" (list (->hydra_core_field "open" ((fn [v] v) new_val)) (->hydra_core_field "close" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.Brackets" "close")) ((fn [v] v) original))))))))))

(def hydra_dsl_ast_expr_brackets (fn [x] (list :inject (->hydra_core_injection "hydra.ast.Expr" (->hydra_core_field "brackets" ((fn [v] v) x))))))

(def hydra_dsl_ast_expr_const (fn [x] (list :inject (->hydra_core_injection "hydra.ast.Expr" (->hydra_core_field "const" ((fn [v] v) x))))))

(def hydra_dsl_ast_expr_indent (fn [x] (list :inject (->hydra_core_injection "hydra.ast.Expr" (->hydra_core_field "indent" ((fn [v] v) x))))))

(def hydra_dsl_ast_expr_op (fn [x] (list :inject (->hydra_core_injection "hydra.ast.Expr" (->hydra_core_field "op" ((fn [v] v) x))))))

(def hydra_dsl_ast_expr_seq (fn [x] (list :inject (->hydra_core_injection "hydra.ast.Expr" (->hydra_core_field "seq" ((fn [v] v) x))))))

(def hydra_dsl_ast_indent_style_all_lines (fn [x] (list :inject (->hydra_core_injection "hydra.ast.IndentStyle" (->hydra_core_field "allLines" ((fn [v] v) x))))))

(def hydra_dsl_ast_indent_style_subsequent_lines (fn [x] (list :inject (->hydra_core_injection "hydra.ast.IndentStyle" (->hydra_core_field "subsequentLines" ((fn [v] v) x))))))

(def hydra_dsl_ast_indented_expression (fn [style] (fn [expr] (list :record (->hydra_core_record "hydra.ast.IndentedExpression" (list (->hydra_core_field "style" ((fn [v] v) style)) (->hydra_core_field "expr" ((fn [v] v) expr))))))))

(def hydra_dsl_ast_indented_expression_expr (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.IndentedExpression" "expr")) ((fn [v] v) x)))))

(def hydra_dsl_ast_indented_expression_style (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.IndentedExpression" "style")) ((fn [v] v) x)))))

(def hydra_dsl_ast_indented_expression_with_expr (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.ast.IndentedExpression" (list (->hydra_core_field "style" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.IndentedExpression" "style")) ((fn [v] v) original)))) (->hydra_core_field "expr" ((fn [v] v) new_val))))))))

(def hydra_dsl_ast_indented_expression_with_style (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.ast.IndentedExpression" (list (->hydra_core_field "style" ((fn [v] v) new_val)) (->hydra_core_field "expr" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.IndentedExpression" "expr")) ((fn [v] v) original))))))))))

(def hydra_dsl_ast_op (fn [symbol] (fn [padding] (fn [precedence] (fn [associativity] (list :record (->hydra_core_record "hydra.ast.Op" (list (->hydra_core_field "symbol" ((fn [v] v) symbol)) (->hydra_core_field "padding" ((fn [v] v) padding)) (->hydra_core_field "precedence" ((fn [v] v) precedence)) (->hydra_core_field "associativity" ((fn [v] v) associativity))))))))))

(def hydra_dsl_ast_op_associativity (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.Op" "associativity")) ((fn [v] v) x)))))

(def hydra_dsl_ast_op_expr (fn [op] (fn [lhs] (fn [rhs] (list :record (->hydra_core_record "hydra.ast.OpExpr" (list (->hydra_core_field "op" ((fn [v] v) op)) (->hydra_core_field "lhs" ((fn [v] v) lhs)) (->hydra_core_field "rhs" ((fn [v] v) rhs)))))))))

(def hydra_dsl_ast_op_expr_lhs (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.OpExpr" "lhs")) ((fn [v] v) x)))))

(def hydra_dsl_ast_op_expr_op (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.OpExpr" "op")) ((fn [v] v) x)))))

(def hydra_dsl_ast_op_expr_rhs (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.OpExpr" "rhs")) ((fn [v] v) x)))))

(def hydra_dsl_ast_op_expr_with_lhs (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.ast.OpExpr" (list (->hydra_core_field "op" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.OpExpr" "op")) ((fn [v] v) original)))) (->hydra_core_field "lhs" ((fn [v] v) new_val)) (->hydra_core_field "rhs" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.OpExpr" "rhs")) ((fn [v] v) original))))))))))

(def hydra_dsl_ast_op_expr_with_op (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.ast.OpExpr" (list (->hydra_core_field "op" ((fn [v] v) new_val)) (->hydra_core_field "lhs" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.OpExpr" "lhs")) ((fn [v] v) original)))) (->hydra_core_field "rhs" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.OpExpr" "rhs")) ((fn [v] v) original))))))))))

(def hydra_dsl_ast_op_expr_with_rhs (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.ast.OpExpr" (list (->hydra_core_field "op" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.OpExpr" "op")) ((fn [v] v) original)))) (->hydra_core_field "lhs" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.OpExpr" "lhs")) ((fn [v] v) original)))) (->hydra_core_field "rhs" ((fn [v] v) new_val))))))))

(def hydra_dsl_ast_op_padding (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.Op" "padding")) ((fn [v] v) x)))))

(def hydra_dsl_ast_op_precedence (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.Op" "precedence")) ((fn [v] v) x)))))

(def hydra_dsl_ast_op_symbol (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.Op" "symbol")) ((fn [v] v) x)))))

(def hydra_dsl_ast_op_with_associativity (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.ast.Op" (list (->hydra_core_field "symbol" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.Op" "symbol")) ((fn [v] v) original)))) (->hydra_core_field "padding" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.Op" "padding")) ((fn [v] v) original)))) (->hydra_core_field "precedence" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.Op" "precedence")) ((fn [v] v) original)))) (->hydra_core_field "associativity" ((fn [v] v) new_val))))))))

(def hydra_dsl_ast_op_with_padding (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.ast.Op" (list (->hydra_core_field "symbol" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.Op" "symbol")) ((fn [v] v) original)))) (->hydra_core_field "padding" ((fn [v] v) new_val)) (->hydra_core_field "precedence" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.Op" "precedence")) ((fn [v] v) original)))) (->hydra_core_field "associativity" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.Op" "associativity")) ((fn [v] v) original))))))))))

(def hydra_dsl_ast_op_with_precedence (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.ast.Op" (list (->hydra_core_field "symbol" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.Op" "symbol")) ((fn [v] v) original)))) (->hydra_core_field "padding" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.Op" "padding")) ((fn [v] v) original)))) (->hydra_core_field "precedence" ((fn [v] v) new_val)) (->hydra_core_field "associativity" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.Op" "associativity")) ((fn [v] v) original))))))))))

(def hydra_dsl_ast_op_with_symbol (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.ast.Op" (list (->hydra_core_field "symbol" ((fn [v] v) new_val)) (->hydra_core_field "padding" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.Op" "padding")) ((fn [v] v) original)))) (->hydra_core_field "precedence" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.Op" "precedence")) ((fn [v] v) original)))) (->hydra_core_field "associativity" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.Op" "associativity")) ((fn [v] v) original))))))))))

(def hydra_dsl_ast_padding (fn [left] (fn [right] (list :record (->hydra_core_record "hydra.ast.Padding" (list (->hydra_core_field "left" ((fn [v] v) left)) (->hydra_core_field "right" ((fn [v] v) right))))))))

(def hydra_dsl_ast_padding_left (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.Padding" "left")) ((fn [v] v) x)))))

(def hydra_dsl_ast_padding_right (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.Padding" "right")) ((fn [v] v) x)))))

(def hydra_dsl_ast_padding_with_left (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.ast.Padding" (list (->hydra_core_field "left" ((fn [v] v) new_val)) (->hydra_core_field "right" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.Padding" "right")) ((fn [v] v) original))))))))))

(def hydra_dsl_ast_padding_with_right (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.ast.Padding" (list (->hydra_core_field "left" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.Padding" "left")) ((fn [v] v) original)))) (->hydra_core_field "right" ((fn [v] v) new_val))))))))

(def hydra_dsl_ast_precedence (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.ast.Precedence" ((fn [v] v) x)))))

(def hydra_dsl_ast_seq_expr (fn [op] (fn [elements] (list :record (->hydra_core_record "hydra.ast.SeqExpr" (list (->hydra_core_field "op" ((fn [v] v) op)) (->hydra_core_field "elements" ((fn [v] v) elements))))))))

(def hydra_dsl_ast_seq_expr_elements (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.SeqExpr" "elements")) ((fn [v] v) x)))))

(def hydra_dsl_ast_seq_expr_op (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.SeqExpr" "op")) ((fn [v] v) x)))))

(def hydra_dsl_ast_seq_expr_with_elements (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.ast.SeqExpr" (list (->hydra_core_field "op" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.SeqExpr" "op")) ((fn [v] v) original)))) (->hydra_core_field "elements" ((fn [v] v) new_val))))))))

(def hydra_dsl_ast_seq_expr_with_op (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.ast.SeqExpr" (list (->hydra_core_field "op" ((fn [v] v) new_val)) (->hydra_core_field "elements" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.ast.SeqExpr" "elements")) ((fn [v] v) original))))))))))

(def hydra_dsl_ast_symbol (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.ast.Symbol" ((fn [v] v) x)))))

(def hydra_dsl_ast_un_precedence (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.ast.Precedence") ((fn [v] v) x)))))

(def hydra_dsl_ast_un_symbol (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.ast.Symbol") ((fn [v] v) x)))))

(def hydra_dsl_ast_ws_break (list :inject (->hydra_core_injection "hydra.ast.Ws" (->hydra_core_field "break" (list :unit nil)))))

(def hydra_dsl_ast_ws_break_and_indent (fn [x] (list :inject (->hydra_core_injection "hydra.ast.Ws" (->hydra_core_field "breakAndIndent" ((fn [v] v) x))))))

(def hydra_dsl_ast_ws_double_break (list :inject (->hydra_core_injection "hydra.ast.Ws" (->hydra_core_field "doubleBreak" (list :unit nil)))))

(def hydra_dsl_ast_ws_none (list :inject (->hydra_core_injection "hydra.ast.Ws" (->hydra_core_field "none" (list :unit nil)))))

(def hydra_dsl_ast_ws_space (list :inject (->hydra_core_injection "hydra.ast.Ws" (->hydra_core_field "space" (list :unit nil)))))
