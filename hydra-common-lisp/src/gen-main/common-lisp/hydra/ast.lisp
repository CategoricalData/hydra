(defpackage :hydra.ast
(:use :cl)
(:export :hydra_ast_associativity-variants :make-hydra_ast_block_style :hydra_ast_block_style? :hydra_ast_block_style-indent :hydra_ast_block_style-newline_before_content :hydra_ast_block_style-newline_after_content :make-hydra_ast_bracket_expr :hydra_ast_bracket_expr? :hydra_ast_bracket_expr-brackets :hydra_ast_bracket_expr-enclosed :hydra_ast_bracket_expr-style :make-hydra_ast_brackets :hydra_ast_brackets? :hydra_ast_brackets-open :hydra_ast_brackets-close :hydra_ast_expr-variants :make-hydra_ast_indented_expression :hydra_ast_indented_expression? :hydra_ast_indented_expression-style :hydra_ast_indented_expression-expr :hydra_ast_indent_style-variants :make-hydra_ast_op :hydra_ast_op? :hydra_ast_op-symbol :hydra_ast_op-padding :hydra_ast_op-precedence :hydra_ast_op-associativity :make-hydra_ast_op_expr :hydra_ast_op_expr? :hydra_ast_op_expr-op :hydra_ast_op_expr-lhs :hydra_ast_op_expr-rhs :make-hydra_ast_padding :hydra_ast_padding? :hydra_ast_padding-left :hydra_ast_padding-right :make-hydra_ast_precedence :hydra_ast_precedence? :hydra_ast_precedence-value :make-hydra_ast_symbol :hydra_ast_symbol? :hydra_ast_symbol-value :hydra_ast_ws-variants))

(in-package :hydra.ast)

(cl:defvar hydra_ast_associativity-variants (cl:list :none :left :right :both))

(cl:defstruct hydra_ast_block_style indent newline_before_content newline_after_content)

(cl:defstruct hydra_ast_bracket_expr brackets enclosed style)

(cl:defstruct hydra_ast_brackets open close)

(cl:defvar hydra_ast_expr-variants (cl:list :const :indent :op :brackets))

(cl:defstruct hydra_ast_indented_expression style expr)

(cl:defvar hydra_ast_indent_style-variants (cl:list :all_lines :subsequent_lines))

(cl:defstruct hydra_ast_op symbol padding precedence associativity)

(cl:defstruct hydra_ast_op_expr op lhs rhs)

(cl:defstruct hydra_ast_padding left right)

(cl:defstruct hydra_ast_precedence value)

(cl:defstruct hydra_ast_symbol value)

(cl:defvar hydra_ast_ws-variants (cl:list :none :space :break :break_and_indent :double_break))
