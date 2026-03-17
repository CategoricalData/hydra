(ns hydra.ast)

(declare hydra_ast_associativity-variants hydra_ast_expr-variants hydra_ast_indent_style-variants hydra_ast_ws-variants)

(def hydra_ast_associativity-variants (list :none :left :right :both))

(defrecord hydra_ast_block_style [indent newline_before_content newline_after_content])
(defn make-hydra_ast_block_style [indent newline_before_content newline_after_content] (->hydra_ast_block_style indent newline_before_content newline_after_content))

(defrecord hydra_ast_bracket_expr [brackets enclosed style])
(defn make-hydra_ast_bracket_expr [brackets enclosed style] (->hydra_ast_bracket_expr brackets enclosed style))

(defrecord hydra_ast_brackets [open close])
(defn make-hydra_ast_brackets [open close] (->hydra_ast_brackets open close))

(def hydra_ast_expr-variants (list :const :indent :op :brackets :seq))

(defrecord hydra_ast_indented_expression [style expr])
(defn make-hydra_ast_indented_expression [style expr] (->hydra_ast_indented_expression style expr))

(def hydra_ast_indent_style-variants (list :all_lines :subsequent_lines))

(defrecord hydra_ast_op [symbol padding precedence associativity])
(defn make-hydra_ast_op [symbol padding precedence associativity] (->hydra_ast_op symbol padding precedence associativity))

(defrecord hydra_ast_op_expr [op lhs rhs])
(defn make-hydra_ast_op_expr [op lhs rhs] (->hydra_ast_op_expr op lhs rhs))

(defrecord hydra_ast_padding [left right])
(defn make-hydra_ast_padding [left right] (->hydra_ast_padding left right))

(defrecord hydra_ast_precedence [value])
(defn make-hydra_ast_precedence [value] (->hydra_ast_precedence value))

(defrecord hydra_ast_seq_expr [op elements])
(defn make-hydra_ast_seq_expr [op elements] (->hydra_ast_seq_expr op elements))

(defrecord hydra_ast_symbol [value])
(defn make-hydra_ast_symbol [value] (->hydra_ast_symbol value))

(def hydra_ast_ws-variants (list :none :space :break :break_and_indent :double_break))
