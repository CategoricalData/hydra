(require 'cl-lib)

(require 'hydra.ast)

(require 'hydra.lib.math)

(require 'hydra.serialization)

(defvar hydra_haskell_operators_and_op (funcall (funcall (hydra_serialization_op "&&") 3) (list :right nil)))

(defvar hydra_haskell_operators_ap_op (funcall (funcall (hydra_serialization_op "<*>") 4) (list :left nil)))

(defvar hydra_haskell_operators_app_op (make-hydra_ast_op "" (make-hydra_ast_padding (list :none nil) (list :space nil)) 0 (list :left nil)))

(defvar hydra_haskell_operators_apply_op (funcall (funcall (hydra_serialization_op "$") 0) (list :right nil)))

(defvar hydra_haskell_operators_arrow_op (funcall (funcall (hydra_serialization_op "->") (hydra_lib_math_negate 1)) (list :right nil)))

(defvar hydra_haskell_operators_assert_op (funcall (funcall (hydra_serialization_op "=>") 0) (list :none nil)))

(defvar hydra_haskell_operators_bind_op (funcall (funcall (hydra_serialization_op ">>=") 1) (list :left nil)))

(defvar hydra_haskell_operators_case_op (funcall (funcall (hydra_serialization_op "->") 0) (list :none nil)))

(defvar hydra_haskell_operators_compose_op (funcall (funcall (hydra_serialization_op ".") 9) (list :left nil)))

(defvar hydra_haskell_operators_concat_op (funcall (funcall (hydra_serialization_op "++") 5) (list :right nil)))

(defvar hydra_haskell_operators_cons_op (funcall (funcall (hydra_serialization_op ":") 5) (list :right nil)))

(defvar hydra_haskell_operators_define_op (funcall (funcall (hydra_serialization_op "=") 0) (list :none nil)))

(defvar hydra_haskell_operators_diamond_op (funcall (funcall (hydra_serialization_op "<>") 6) (list :right nil)))

(defvar hydra_haskell_operators_div_op (funcall (funcall (hydra_serialization_op "`div`") 7) (list :left nil)))

(defvar hydra_haskell_operators_divide_op (funcall (funcall (hydra_serialization_op "/") 7) (list :left nil)))

(defvar hydra_haskell_operators_elem_op (funcall (funcall (hydra_serialization_op "`elem`") 4) (list :none nil)))

(defvar hydra_haskell_operators_equal_op (funcall (funcall (hydra_serialization_op "==") 4) (list :none nil)))

(defvar hydra_haskell_operators_fmap_op (funcall (funcall (hydra_serialization_op "<$>") 4) (list :left nil)))

(defvar hydra_haskell_operators_gt_op (funcall (funcall (hydra_serialization_op ">") 4) (list :none nil)))

(defvar hydra_haskell_operators_gte_op (funcall (funcall (hydra_serialization_op ">=") 4) (list :none nil)))

(defvar hydra_haskell_operators_index_op (funcall (funcall (hydra_serialization_op "!!") 9) (list :left nil)))

(defvar hydra_haskell_operators_lambda_op (funcall (funcall (hydra_serialization_op "->") (hydra_lib_math_negate 1)) (list :right nil)))

(defvar hydra_haskell_operators_lt_op (funcall (funcall (hydra_serialization_op "<") 4) (list :none nil)))

(defvar hydra_haskell_operators_lte_op (funcall (funcall (hydra_serialization_op ">=") 4) (list :none nil)))

(defvar hydra_haskell_operators_minus_op (funcall (funcall (hydra_serialization_op "-") 6) (list :both nil)))

(defvar hydra_haskell_operators_mod_op (funcall (funcall (hydra_serialization_op "`mod`") 7) (list :left nil)))

(defvar hydra_haskell_operators_mult_op (funcall (funcall (hydra_serialization_op "*") 7) (list :both nil)))

(defvar hydra_haskell_operators_neq_op (funcall (funcall (hydra_serialization_op "/=") 4) (list :none nil)))

(defvar hydra_haskell_operators_not_elem_op (funcall (funcall (hydra_serialization_op "`notElem`") 4) (list :none nil)))

(defvar hydra_haskell_operators_or_op (funcall (funcall (hydra_serialization_op "||") 2) (list :right nil)))

(defvar hydra_haskell_operators_plus_op (funcall (funcall (hydra_serialization_op "+") 6) (list :both nil)))

(defvar hydra_haskell_operators_quot_op (funcall (funcall (hydra_serialization_op "`quot`") 7) (list :left nil)))

(defvar hydra_haskell_operators_rem_op (funcall (funcall (hydra_serialization_op "`rem`") 7) (list :left nil)))

(defvar hydra_haskell_operators_type_op (funcall (funcall (hydra_serialization_op "::") 0) (list :none nil)))

(provide 'hydra.haskell.operators)
