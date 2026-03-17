(require 'cl-lib)

(require 'hydra.ast)

(require 'hydra.json.model)

(require 'hydra.lib.equality)

(require 'hydra.lib.lists)

(require 'hydra.lib.literals)

(require 'hydra.lib.logic)

(require 'hydra.lib.maps)

(require 'hydra.lib.math)

(require 'hydra.lib.pairs)

(require 'hydra.lib.strings)

(require 'hydra.serialization)

(defvar hydra_json_writer_colon_op (make-hydra_ast_op ":" (make-hydra_ast_padding (list :none nil) (list :space nil)) 0 (list :none nil)))

(defvar hydra_json_writer_json_string (lambda (s) (let ((hex_escape (lambda (c) (let ((hi (hydra_lib_strings_from_list (hydra_lib_lists_pure ((hydra_lib_strings_char_at ((hydra_lib_math_div c) 16)) "0123456789abcdef"))))) (let ((lo (hydra_lib_strings_from_list (hydra_lib_lists_pure ((hydra_lib_strings_char_at ((hydra_lib_math_mod c) 16)) "0123456789abcdef"))))) ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "\\u00") hi)) lo)))))) (let ((escape (lambda (c) (if ((hydra_lib_equality_equal c) 34) "\\\"" (if ((hydra_lib_equality_equal c) 92) "\\\\" (if ((hydra_lib_equality_equal c) 8) "\\b" (if ((hydra_lib_equality_equal c) 12) "\\f" (if ((hydra_lib_equality_equal c) 10) "\\n" (if ((hydra_lib_equality_equal c) 13) "\\r" (if ((hydra_lib_equality_equal c) 9) "\\t" (if ((hydra_lib_equality_lt c) 32) (hex_escape c) (hydra_lib_strings_from_list (hydra_lib_lists_pure c))))))))))))) (let ((escaped (hydra_lib_strings_cat ((hydra_lib_lists_map escape) (hydra_lib_strings_to_list s))))) ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "\"") escaped)) "\""))))))

(defvar hydra_json_writer_key_value_to_expr (lambda (pair) (let ((key (hydra_lib_pairs_first pair))) (let ((value (hydra_lib_pairs_second pair))) (((hydra_serialization_ifx hydra_json_writer_colon_op) (hydra_serialization_cst (hydra_json_writer_json_string key))) (hydra_json_writer_value_to_expr value))))))

(defvar hydra_json_writer_value_to_expr (lambda (value) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :array) ((lambda (arr) (hydra_serialization_bracket_list_adaptive ((hydra_lib_lists_map hydra_json_writer_value_to_expr) arr))) match_value)) ((equal (car match_target) :boolean) ((lambda (b) (hydra_serialization_cst (if b "true" "false"))) match_value)) ((equal (car match_target) :null) ((lambda (_) (hydra_serialization_cst "null")) match_value)) ((equal (car match_target) :number) ((lambda (n) (let ((rounded (hydra_lib_literals_bigfloat_to_bigint n))) (hydra_serialization_cst (if ((hydra_lib_equality_equal n) (hydra_lib_literals_bigint_to_bigfloat rounded)) (hydra_lib_literals_show_bigint rounded) (hydra_lib_literals_show_bigfloat n))))) match_value)) ((equal (car match_target) :object) ((lambda (obj) (hydra_serialization_braces_list_adaptive ((hydra_lib_lists_map hydra_json_writer_key_value_to_expr) (hydra_lib_maps_to_list obj)))) match_value)) ((equal (car match_target) :string) ((lambda (s) (hydra_serialization_cst (hydra_json_writer_json_string s))) match_value)))) (cadr match_target))) value)))

(defvar hydra_json_writer_print_json (lambda (value) (hydra_serialization_print_expr (hydra_json_writer_value_to_expr value))))

(provide 'hydra.json.writer)
