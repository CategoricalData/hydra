(require 'cl-lib)

(defvar hydra_ast_associativity-variants (list :none :left :right :both))

(cl-defstruct hydra_ast_block_style indent newline_before_content newline_after_content)

(cl-defstruct hydra_ast_bracket_expr brackets enclosed style)

(cl-defstruct hydra_ast_brackets open close)

(defvar hydra_ast_expr-variants (list :const :indent :op :brackets))

(cl-defstruct hydra_ast_indented_expression style expr)

(defvar hydra_ast_indent_style-variants (list :all_lines :subsequent_lines))

(cl-defstruct hydra_ast_op symbol padding precedence associativity)

(cl-defstruct hydra_ast_op_expr op lhs rhs)

(cl-defstruct hydra_ast_padding left right)

(cl-defstruct hydra_ast_precedence value)

(cl-defstruct hydra_ast_symbol value)

(defvar hydra_ast_ws-variants (list :none :space :break :break_and_indent :double_break))

(provide 'hydra.ast)
