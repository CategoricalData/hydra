(defpackage :hydra.haskell.operators
(:use :cl :hydra.ast :hydra.lib.math :hydra.serialization)
(:export :hydra_haskell_operators_and_op :hydra_haskell_operators_ap_op :hydra_haskell_operators_app_op :hydra_haskell_operators_apply_op :hydra_haskell_operators_arrow_op :hydra_haskell_operators_assert_op :hydra_haskell_operators_bind_op :hydra_haskell_operators_case_op :hydra_haskell_operators_compose_op :hydra_haskell_operators_concat_op :hydra_haskell_operators_cons_op :hydra_haskell_operators_define_op :hydra_haskell_operators_diamond_op :hydra_haskell_operators_div_op :hydra_haskell_operators_divide_op :hydra_haskell_operators_elem_op :hydra_haskell_operators_equal_op :hydra_haskell_operators_fmap_op :hydra_haskell_operators_gt_op :hydra_haskell_operators_gte_op :hydra_haskell_operators_index_op :hydra_haskell_operators_lambda_op :hydra_haskell_operators_lt_op :hydra_haskell_operators_lte_op :hydra_haskell_operators_minus_op :hydra_haskell_operators_mod_op :hydra_haskell_operators_mult_op :hydra_haskell_operators_neq_op :hydra_haskell_operators_not_elem_op :hydra_haskell_operators_or_op :hydra_haskell_operators_plus_op :hydra_haskell_operators_quot_op :hydra_haskell_operators_rem_op :hydra_haskell_operators_type_op))

(in-package :hydra.haskell.operators)

(cl:defvar hydra_haskell_operators_and_op (((hydra_serialization_op "&&") 3) (list :right cl:nil)))

(cl:defvar hydra_haskell_operators_ap_op (((hydra_serialization_op "<*>") 4) (list :left cl:nil)))

(cl:defvar hydra_haskell_operators_app_op (make-hydra_ast_op "" (make-hydra_ast_padding (list :none cl:nil) (list :space cl:nil)) 0 (list :left cl:nil)))

(cl:defvar hydra_haskell_operators_apply_op (((hydra_serialization_op "$") 0) (list :right cl:nil)))

(cl:defvar hydra_haskell_operators_arrow_op (((hydra_serialization_op "->") (hydra_lib_math_negate 1)) (list :right cl:nil)))

(cl:defvar hydra_haskell_operators_assert_op (((hydra_serialization_op "=>") 0) (list :none cl:nil)))

(cl:defvar hydra_haskell_operators_bind_op (((hydra_serialization_op ">>=") 1) (list :left cl:nil)))

(cl:defvar hydra_haskell_operators_case_op (((hydra_serialization_op "->") 0) (list :none cl:nil)))

(cl:defvar hydra_haskell_operators_compose_op (((hydra_serialization_op ".") 9) (list :left cl:nil)))

(cl:defvar hydra_haskell_operators_concat_op (((hydra_serialization_op "++") 5) (list :right cl:nil)))

(cl:defvar hydra_haskell_operators_cons_op (((hydra_serialization_op ":") 5) (list :right cl:nil)))

(cl:defvar hydra_haskell_operators_define_op (((hydra_serialization_op "=") 0) (list :none cl:nil)))

(cl:defvar hydra_haskell_operators_diamond_op (((hydra_serialization_op "<>") 6) (list :right cl:nil)))

(cl:defvar hydra_haskell_operators_div_op (((hydra_serialization_op "`div`") 7) (list :left cl:nil)))

(cl:defvar hydra_haskell_operators_divide_op (((hydra_serialization_op "/") 7) (list :left cl:nil)))

(cl:defvar hydra_haskell_operators_elem_op (((hydra_serialization_op "`elem`") 4) (list :none cl:nil)))

(cl:defvar hydra_haskell_operators_equal_op (((hydra_serialization_op "==") 4) (list :none cl:nil)))

(cl:defvar hydra_haskell_operators_fmap_op (((hydra_serialization_op "<$>") 4) (list :left cl:nil)))

(cl:defvar hydra_haskell_operators_gt_op (((hydra_serialization_op ">") 4) (list :none cl:nil)))

(cl:defvar hydra_haskell_operators_gte_op (((hydra_serialization_op ">=") 4) (list :none cl:nil)))

(cl:defvar hydra_haskell_operators_index_op (((hydra_serialization_op "!!") 9) (list :left cl:nil)))

(cl:defvar hydra_haskell_operators_lambda_op (((hydra_serialization_op "->") (hydra_lib_math_negate 1)) (list :right cl:nil)))

(cl:defvar hydra_haskell_operators_lt_op (((hydra_serialization_op "<") 4) (list :none cl:nil)))

(cl:defvar hydra_haskell_operators_lte_op (((hydra_serialization_op ">=") 4) (list :none cl:nil)))

(cl:defvar hydra_haskell_operators_minus_op (((hydra_serialization_op "-") 6) (list :both cl:nil)))

(cl:defvar hydra_haskell_operators_mod_op (((hydra_serialization_op "`mod`") 7) (list :left cl:nil)))

(cl:defvar hydra_haskell_operators_mult_op (((hydra_serialization_op "*") 7) (list :both cl:nil)))

(cl:defvar hydra_haskell_operators_neq_op (((hydra_serialization_op "/=") 4) (list :none cl:nil)))

(cl:defvar hydra_haskell_operators_not_elem_op (((hydra_serialization_op "`notElem`") 4) (list :none cl:nil)))

(cl:defvar hydra_haskell_operators_or_op (((hydra_serialization_op "||") 2) (list :right cl:nil)))

(cl:defvar hydra_haskell_operators_plus_op (((hydra_serialization_op "+") 6) (list :both cl:nil)))

(cl:defvar hydra_haskell_operators_quot_op (((hydra_serialization_op "`quot`") 7) (list :left cl:nil)))

(cl:defvar hydra_haskell_operators_rem_op (((hydra_serialization_op "`rem`") 7) (list :left cl:nil)))

(cl:defvar hydra_haskell_operators_type_op (((hydra_serialization_op "::") 0) (list :none cl:nil)))
