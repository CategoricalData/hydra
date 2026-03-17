(ns hydra.ext.haskell.operators
  (:require [hydra.ast :refer :all] [hydra.lib.math :refer :all] [hydra.serialization :refer :all]
))

(declare hydra_ext_haskell_operators_and_op hydra_ext_haskell_operators_ap_op hydra_ext_haskell_operators_app_op hydra_ext_haskell_operators_apply_op hydra_ext_haskell_operators_arrow_op hydra_ext_haskell_operators_assert_op hydra_ext_haskell_operators_bind_op hydra_ext_haskell_operators_case_op hydra_ext_haskell_operators_compose_op hydra_ext_haskell_operators_concat_op hydra_ext_haskell_operators_cons_op hydra_ext_haskell_operators_define_op hydra_ext_haskell_operators_diamond_op hydra_ext_haskell_operators_div_op hydra_ext_haskell_operators_divide_op hydra_ext_haskell_operators_elem_op hydra_ext_haskell_operators_equal_op hydra_ext_haskell_operators_fmap_op hydra_ext_haskell_operators_gt_op hydra_ext_haskell_operators_gte_op hydra_ext_haskell_operators_index_op hydra_ext_haskell_operators_lambda_op hydra_ext_haskell_operators_lt_op hydra_ext_haskell_operators_lte_op hydra_ext_haskell_operators_minus_op hydra_ext_haskell_operators_mod_op hydra_ext_haskell_operators_mult_op hydra_ext_haskell_operators_neq_op hydra_ext_haskell_operators_not_elem_op hydra_ext_haskell_operators_or_op hydra_ext_haskell_operators_plus_op hydra_ext_haskell_operators_quot_op hydra_ext_haskell_operators_rem_op hydra_ext_haskell_operators_type_op)

(def hydra_ext_haskell_operators_and_op (((hydra_serialization_op "&&") 3) (list :right nil)))

(def hydra_ext_haskell_operators_ap_op (((hydra_serialization_op "<*>") 4) (list :left nil)))

(def hydra_ext_haskell_operators_app_op (->hydra_ast_op "" (->hydra_ast_padding (list :none nil) (list :space nil)) 0 (list :left nil)))

(def hydra_ext_haskell_operators_apply_op (((hydra_serialization_op "$") 0) (list :right nil)))

(def hydra_ext_haskell_operators_arrow_op (((hydra_serialization_op "->") (hydra_lib_math_negate 1)) (list :right nil)))

(def hydra_ext_haskell_operators_assert_op (((hydra_serialization_op "=>") 0) (list :none nil)))

(def hydra_ext_haskell_operators_bind_op (((hydra_serialization_op ">>=") 1) (list :left nil)))

(def hydra_ext_haskell_operators_case_op (((hydra_serialization_op "->") 0) (list :none nil)))

(def hydra_ext_haskell_operators_compose_op (((hydra_serialization_op ".") 9) (list :left nil)))

(def hydra_ext_haskell_operators_concat_op (((hydra_serialization_op "++") 5) (list :right nil)))

(def hydra_ext_haskell_operators_cons_op (((hydra_serialization_op ":") 5) (list :right nil)))

(def hydra_ext_haskell_operators_define_op (((hydra_serialization_op "=") 0) (list :none nil)))

(def hydra_ext_haskell_operators_diamond_op (((hydra_serialization_op "<>") 6) (list :right nil)))

(def hydra_ext_haskell_operators_div_op (((hydra_serialization_op "`div`") 7) (list :left nil)))

(def hydra_ext_haskell_operators_divide_op (((hydra_serialization_op "/") 7) (list :left nil)))

(def hydra_ext_haskell_operators_elem_op (((hydra_serialization_op "`elem`") 4) (list :none nil)))

(def hydra_ext_haskell_operators_equal_op (((hydra_serialization_op "==") 4) (list :none nil)))

(def hydra_ext_haskell_operators_fmap_op (((hydra_serialization_op "<$>") 4) (list :left nil)))

(def hydra_ext_haskell_operators_gt_op (((hydra_serialization_op ">") 4) (list :none nil)))

(def hydra_ext_haskell_operators_gte_op (((hydra_serialization_op ">=") 4) (list :none nil)))

(def hydra_ext_haskell_operators_index_op (((hydra_serialization_op "!!") 9) (list :left nil)))

(def hydra_ext_haskell_operators_lambda_op (((hydra_serialization_op "->") (hydra_lib_math_negate 1)) (list :right nil)))

(def hydra_ext_haskell_operators_lt_op (((hydra_serialization_op "<") 4) (list :none nil)))

(def hydra_ext_haskell_operators_lte_op (((hydra_serialization_op ">=") 4) (list :none nil)))

(def hydra_ext_haskell_operators_minus_op (((hydra_serialization_op "-") 6) (list :both nil)))

(def hydra_ext_haskell_operators_mod_op (((hydra_serialization_op "`mod`") 7) (list :left nil)))

(def hydra_ext_haskell_operators_mult_op (((hydra_serialization_op "*") 7) (list :both nil)))

(def hydra_ext_haskell_operators_neq_op (((hydra_serialization_op "/=") 4) (list :none nil)))

(def hydra_ext_haskell_operators_not_elem_op (((hydra_serialization_op "`notElem`") 4) (list :none nil)))

(def hydra_ext_haskell_operators_or_op (((hydra_serialization_op "||") 2) (list :right nil)))

(def hydra_ext_haskell_operators_plus_op (((hydra_serialization_op "+") 6) (list :both nil)))

(def hydra_ext_haskell_operators_quot_op (((hydra_serialization_op "`quot`") 7) (list :left nil)))

(def hydra_ext_haskell_operators_rem_op (((hydra_serialization_op "`rem`") 7) (list :left nil)))

(def hydra_ext_haskell_operators_type_op (((hydra_serialization_op "::") 0) (list :none nil)))
