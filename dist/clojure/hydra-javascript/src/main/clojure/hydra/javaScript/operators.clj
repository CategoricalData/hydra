(ns hydra.javaScript.operators
  (:require [hydra.ast :refer :all] [hydra.serialization :refer :all]
))

(declare hydra_java_script_operators_add_op hydra_java_script_operators_app_op hydra_java_script_operators_arrow_op hydra_java_script_operators_assign_op hydra_java_script_operators_bitwise_and_op hydra_java_script_operators_bitwise_or_op hydra_java_script_operators_bitwise_xor_op hydra_java_script_operators_colon_op hydra_java_script_operators_comma_op hydra_java_script_operators_define_op hydra_java_script_operators_divide_op hydra_java_script_operators_equal_op hydra_java_script_operators_exponentiate_op hydra_java_script_operators_greater_than_op hydra_java_script_operators_greater_than_or_equal_op hydra_java_script_operators_in_op hydra_java_script_operators_instance_of_op hydra_java_script_operators_left_shift_op hydra_java_script_operators_less_than_op hydra_java_script_operators_less_than_or_equal_op hydra_java_script_operators_logical_and_op hydra_java_script_operators_logical_or_op hydra_java_script_operators_member_op hydra_java_script_operators_modulo_op hydra_java_script_operators_multiply_op hydra_java_script_operators_not_equal_op hydra_java_script_operators_nullish_coalescing_op hydra_java_script_operators_optional_chain_op hydra_java_script_operators_right_shift_op hydra_java_script_operators_strict_equal_op hydra_java_script_operators_strict_not_equal_op hydra_java_script_operators_subtract_op hydra_java_script_operators_ternary_op hydra_java_script_operators_unsigned_right_shift_op)

(def hydra_java_script_operators_add_op (((hydra_serialization_op "+") 14) (list :left nil)))

(def hydra_java_script_operators_app_op (->hydra_ast_op "" (->hydra_ast_padding (list :none nil) (list :none nil)) 20 (list :left nil)))

(def hydra_java_script_operators_arrow_op (((hydra_serialization_op "=>") 2) (list :right nil)))

(def hydra_java_script_operators_assign_op (((hydra_serialization_op "=") 2) (list :right nil)))

(def hydra_java_script_operators_bitwise_and_op (((hydra_serialization_op "&") 10) (list :left nil)))

(def hydra_java_script_operators_bitwise_or_op (((hydra_serialization_op "|") 8) (list :left nil)))

(def hydra_java_script_operators_bitwise_xor_op (((hydra_serialization_op "^") 9) (list :left nil)))

(def hydra_java_script_operators_colon_op (((hydra_serialization_op ":") 0) (list :none nil)))

(def hydra_java_script_operators_comma_op (((hydra_serialization_op ",") 1) (list :left nil)))

(def hydra_java_script_operators_define_op (((hydra_serialization_op "=") 0) (list :none nil)))

(def hydra_java_script_operators_divide_op (((hydra_serialization_op "/") 15) (list :left nil)))

(def hydra_java_script_operators_equal_op (((hydra_serialization_op "==") 11) (list :left nil)))

(def hydra_java_script_operators_exponentiate_op (((hydra_serialization_op "**") 16) (list :right nil)))

(def hydra_java_script_operators_greater_than_op (((hydra_serialization_op ">") 12) (list :left nil)))

(def hydra_java_script_operators_greater_than_or_equal_op (((hydra_serialization_op ">=") 12) (list :left nil)))

(def hydra_java_script_operators_in_op (((hydra_serialization_op "in") 12) (list :left nil)))

(def hydra_java_script_operators_instance_of_op (((hydra_serialization_op "instanceof") 12) (list :left nil)))

(def hydra_java_script_operators_left_shift_op (((hydra_serialization_op "<<") 13) (list :left nil)))

(def hydra_java_script_operators_less_than_op (((hydra_serialization_op "<") 12) (list :left nil)))

(def hydra_java_script_operators_less_than_or_equal_op (((hydra_serialization_op "<=") 12) (list :left nil)))

(def hydra_java_script_operators_logical_and_op (((hydra_serialization_op "&&") 6) (list :left nil)))

(def hydra_java_script_operators_logical_or_op (((hydra_serialization_op "||") 5) (list :left nil)))

(def hydra_java_script_operators_member_op (->hydra_ast_op "." (->hydra_ast_padding (list :none nil) (list :none nil)) 20 (list :left nil)))

(def hydra_java_script_operators_modulo_op (((hydra_serialization_op "%") 15) (list :left nil)))

(def hydra_java_script_operators_multiply_op (((hydra_serialization_op "*") 15) (list :left nil)))

(def hydra_java_script_operators_not_equal_op (((hydra_serialization_op "!=") 11) (list :left nil)))

(def hydra_java_script_operators_nullish_coalescing_op (((hydra_serialization_op "??") 4) (list :left nil)))

(def hydra_java_script_operators_optional_chain_op (->hydra_ast_op "?." (->hydra_ast_padding (list :none nil) (list :none nil)) 20 (list :left nil)))

(def hydra_java_script_operators_right_shift_op (((hydra_serialization_op ">>") 13) (list :left nil)))

(def hydra_java_script_operators_strict_equal_op (((hydra_serialization_op "===") 11) (list :left nil)))

(def hydra_java_script_operators_strict_not_equal_op (((hydra_serialization_op "!==") 11) (list :left nil)))

(def hydra_java_script_operators_subtract_op (((hydra_serialization_op "-") 14) (list :left nil)))

(def hydra_java_script_operators_ternary_op (((hydra_serialization_op "?") 3) (list :right nil)))

(def hydra_java_script_operators_unsigned_right_shift_op (((hydra_serialization_op ">>>") 13) (list :left nil)))
