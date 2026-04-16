(require 'cl-lib)

(require 'hydra.ast)

(require 'hydra.json.model)

(require 'hydra.lib.equality)

(require 'hydra.lib.lists)

(require 'hydra.lib.literals)

(require 'hydra.lib.logic)

(require 'hydra.lib.maps)

(require 'hydra.lib.math)

(require 'hydra.lib.maybes)

(require 'hydra.lib.pairs)

(require 'hydra.lib.strings)

(require 'hydra.serialization)

(defvar hydra_json_writer_colon_op (make-hydra_ast_op ":" (make-hydra_ast_padding (list :none nil) (list :space nil)) 0 (list :none nil)))

(defvar hydra_json_writer_hex_byte (lambda (c) (let ((nibble (lambda (i) (funcall (hydra_lib_maybes_from_maybe (lambda () "?")) (funcall (hydra_lib_maybes_map (lambda (ch) (hydra_lib_strings_from_list (hydra_lib_lists_pure ch)))) (funcall (hydra_lib_strings_maybe_char_at i) "0123456789abcdef")))))) (let ((hi (nibble (funcall (hydra_lib_maybes_from_maybe (lambda () 0)) (funcall (hydra_lib_math_maybe_div c) 16))))) (let ((lo (nibble (funcall (hydra_lib_maybes_from_maybe (lambda () 0)) (funcall (hydra_lib_math_maybe_mod c) 16))))) (funcall (hydra_lib_strings_cat2 hi) lo))))))

(defvar hydra_json_writer_json_string (lambda (s) (let ((hex_escape (lambda (c) (funcall (hydra_lib_strings_cat2 "\\u00") (hydra_json_writer_hex_byte c))))) (let ((escape (lambda (c) (if (funcall (hydra_lib_equality_equal c) 34) "\\\"" (if (funcall (hydra_lib_equality_equal c) 92) "\\\\" (if (funcall (hydra_lib_equality_equal c) 8) "\\b" (if (funcall (hydra_lib_equality_equal c) 12) "\\f" (if (funcall (hydra_lib_equality_equal c) 10) "\\n" (if (funcall (hydra_lib_equality_equal c) 13) "\\r" (if (funcall (hydra_lib_equality_equal c) 9) "\\t" (if (funcall (hydra_lib_equality_lt c) 32) (hex_escape c) (hydra_lib_strings_from_list (hydra_lib_lists_pure c))))))))))))) (let ((escaped (hydra_lib_strings_cat (funcall (hydra_lib_lists_map escape) (hydra_lib_strings_to_list s))))) (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 "\"") escaped)) "\""))))))

(defvar hydra_json_writer_key_value_to_expr (lambda (pair) (let ((key (hydra_lib_pairs_first pair))) (let ((value (hydra_lib_pairs_second pair))) (funcall (funcall (hydra_serialization_ifx hydra_json_writer_colon_op) (hydra_serialization_cst (hydra_json_writer_json_string key))) (hydra_json_writer_value_to_expr value))))))

(defvar hydra_json_writer_value_to_expr (lambda (value) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :array) (funcall (lambda (arr) (hydra_serialization_bracket_list_adaptive (funcall (hydra_lib_lists_map hydra_json_writer_value_to_expr) arr))) match_value)) ((equal (car match_target) :boolean) (funcall (lambda (b) (hydra_serialization_cst (if b "true" "false"))) match_value)) ((equal (car match_target) :null) (funcall (lambda (_) (hydra_serialization_cst "null")) match_value)) ((equal (car match_target) :number) (funcall (lambda (n) (let ((rounded (hydra_lib_literals_bigfloat_to_bigint n))) (let ((shown (hydra_lib_literals_show_bigfloat n))) (hydra_serialization_cst (if (funcall (hydra_lib_logic_and (funcall (hydra_lib_equality_equal n) (hydra_lib_literals_bigint_to_bigfloat rounded))) (hydra_lib_logic_not (funcall (hydra_lib_equality_equal shown) "-0.0"))) (hydra_lib_literals_show_bigint rounded) shown))))) match_value)) ((equal (car match_target) :object) (funcall (lambda (obj) (hydra_serialization_braces_list_adaptive (funcall (hydra_lib_lists_map hydra_json_writer_key_value_to_expr) (hydra_lib_maps_to_list obj)))) match_value)) ((equal (car match_target) :string) (funcall (lambda (s) (hydra_serialization_cst (hydra_json_writer_json_string s))) match_value)))) (cadr match_target))) value)))

(defvar hydra_json_writer_print_json (lambda (value) (hydra_serialization_print_expr (hydra_json_writer_value_to_expr value))))

(provide 'hydra.json.writer)
