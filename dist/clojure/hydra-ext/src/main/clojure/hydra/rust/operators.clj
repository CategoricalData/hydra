(ns hydra.rust.operators
  (:require [hydra.ast :refer :all] [hydra.serialization :refer :all]
))

(declare hydra_rust_operators_add_assign_op hydra_rust_operators_add_op hydra_rust_operators_and_op hydra_rust_operators_app_op hydra_rust_operators_arrow_op hydra_rust_operators_as_op hydra_rust_operators_assign_op hydra_rust_operators_bit_and_assign_op hydra_rust_operators_bit_and_op hydra_rust_operators_bit_or_assign_op hydra_rust_operators_bit_or_op hydra_rust_operators_bit_xor_assign_op hydra_rust_operators_bit_xor_op hydra_rust_operators_colon_colon_op hydra_rust_operators_colon_op hydra_rust_operators_deref_op hydra_rust_operators_div_assign_op hydra_rust_operators_div_op hydra_rust_operators_double_colon_op hydra_rust_operators_eq_op hydra_rust_operators_fat_arrow_op hydra_rust_operators_field_op hydra_rust_operators_ge_op hydra_rust_operators_gt_op hydra_rust_operators_le_op hydra_rust_operators_lt_op hydra_rust_operators_method_op hydra_rust_operators_mul_assign_op hydra_rust_operators_mul_op hydra_rust_operators_ne_op hydra_rust_operators_neg_op hydra_rust_operators_not_op hydra_rust_operators_or_op hydra_rust_operators_range_inclusive_op hydra_rust_operators_range_op hydra_rust_operators_ref_op hydra_rust_operators_rem_assign_op hydra_rust_operators_rem_op hydra_rust_operators_shl_assign_op hydra_rust_operators_shl_op hydra_rust_operators_shr_assign_op hydra_rust_operators_shr_op hydra_rust_operators_sub_assign_op hydra_rust_operators_sub_op)

(def hydra_rust_operators_add_assign_op (((hydra_serialization_op "+=") 1) (list :right nil)))

(def hydra_rust_operators_add_op (((hydra_serialization_op "+") 10) (list :left nil)))

(def hydra_rust_operators_and_op (((hydra_serialization_op "&&") 4) (list :left nil)))

(def hydra_rust_operators_app_op (->hydra_ast_op "" (->hydra_ast_padding (list :none nil) (list :space nil)) 0 (list :left nil)))

(def hydra_rust_operators_arrow_op (((hydra_serialization_op "->") 0) (list :right nil)))

(def hydra_rust_operators_as_op (((hydra_serialization_op "as") 12) (list :left nil)))

(def hydra_rust_operators_assign_op (((hydra_serialization_op "=") 1) (list :right nil)))

(def hydra_rust_operators_bit_and_assign_op (((hydra_serialization_op "&=") 1) (list :right nil)))

(def hydra_rust_operators_bit_and_op (((hydra_serialization_op "&") 8) (list :left nil)))

(def hydra_rust_operators_bit_or_assign_op (((hydra_serialization_op "|=") 1) (list :right nil)))

(def hydra_rust_operators_bit_or_op (((hydra_serialization_op "|") 6) (list :left nil)))

(def hydra_rust_operators_bit_xor_assign_op (((hydra_serialization_op "^=") 1) (list :right nil)))

(def hydra_rust_operators_bit_xor_op (((hydra_serialization_op "^") 7) (list :left nil)))

(def hydra_rust_operators_colon_colon_op (((hydra_serialization_op ":") 0) (list :none nil)))

(def hydra_rust_operators_colon_op (((hydra_serialization_op ":") 12) (list :left nil)))

(def hydra_rust_operators_deref_op (->hydra_ast_op "*" (->hydra_ast_padding (list :none nil) (list :none nil)) 13 (list :none nil)))

(def hydra_rust_operators_div_assign_op (((hydra_serialization_op "/=") 1) (list :right nil)))

(def hydra_rust_operators_div_op (((hydra_serialization_op "/") 11) (list :left nil)))

(def hydra_rust_operators_double_colon_op (->hydra_ast_op "::" (->hydra_ast_padding (list :none nil) (list :none nil)) 15 (list :left nil)))

(def hydra_rust_operators_eq_op (((hydra_serialization_op "==") 5) (list :none nil)))

(def hydra_rust_operators_fat_arrow_op (((hydra_serialization_op "=>") 0) (list :none nil)))

(def hydra_rust_operators_field_op (->hydra_ast_op "." (->hydra_ast_padding (list :none nil) (list :none nil)) 14 (list :left nil)))

(def hydra_rust_operators_ge_op (((hydra_serialization_op ">=") 5) (list :none nil)))

(def hydra_rust_operators_gt_op (((hydra_serialization_op ">") 5) (list :none nil)))

(def hydra_rust_operators_le_op (((hydra_serialization_op "<=") 5) (list :none nil)))

(def hydra_rust_operators_lt_op (((hydra_serialization_op "<") 5) (list :none nil)))

(def hydra_rust_operators_method_op (->hydra_ast_op "." (->hydra_ast_padding (list :none nil) (list :none nil)) 14 (list :left nil)))

(def hydra_rust_operators_mul_assign_op (((hydra_serialization_op "*=") 1) (list :right nil)))

(def hydra_rust_operators_mul_op (((hydra_serialization_op "*") 11) (list :left nil)))

(def hydra_rust_operators_ne_op (((hydra_serialization_op "!=") 5) (list :none nil)))

(def hydra_rust_operators_neg_op (->hydra_ast_op "-" (->hydra_ast_padding (list :none nil) (list :none nil)) 13 (list :none nil)))

(def hydra_rust_operators_not_op (->hydra_ast_op "!" (->hydra_ast_padding (list :none nil) (list :none nil)) 13 (list :none nil)))

(def hydra_rust_operators_or_op (((hydra_serialization_op "||") 3) (list :left nil)))

(def hydra_rust_operators_range_inclusive_op (((hydra_serialization_op "..=") 2) (list :none nil)))

(def hydra_rust_operators_range_op (((hydra_serialization_op "..") 2) (list :none nil)))

(def hydra_rust_operators_ref_op (->hydra_ast_op "&" (->hydra_ast_padding (list :none nil) (list :none nil)) 13 (list :none nil)))

(def hydra_rust_operators_rem_assign_op (((hydra_serialization_op "%=") 1) (list :right nil)))

(def hydra_rust_operators_rem_op (((hydra_serialization_op "%") 11) (list :left nil)))

(def hydra_rust_operators_shl_assign_op (((hydra_serialization_op "<<=") 1) (list :right nil)))

(def hydra_rust_operators_shl_op (((hydra_serialization_op "<<") 9) (list :left nil)))

(def hydra_rust_operators_shr_assign_op (((hydra_serialization_op ">>=") 1) (list :right nil)))

(def hydra_rust_operators_shr_op (((hydra_serialization_op ">>") 9) (list :left nil)))

(def hydra_rust_operators_sub_assign_op (((hydra_serialization_op "-=") 1) (list :right nil)))

(def hydra_rust_operators_sub_op (((hydra_serialization_op "-") 10) (list :left nil)))
