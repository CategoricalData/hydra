(require 'cl-lib)

(require 'hydra.json.model)

(require 'hydra.lib.equality)

(require 'hydra.lib.lists)

(require 'hydra.lib.literals)

(require 'hydra.lib.logic)

(require 'hydra.lib.maps)

(require 'hydra.lib.maybes)

(require 'hydra.lib.strings)

(require 'hydra.parsers)

(require 'hydra.parsing)

(defvar hydra_json_parser_digit (hydra_parsers_satisfy (lambda (c) ((hydra_lib_logic_and ((hydra_lib_equality_gte c) 48)) ((hydra_lib_equality_lte c) 57)))))

(defvar hydra_json_parser_digits ((hydra_parsers_map hydra_lib_strings_from_list) (hydra_parsers_some hydra_json_parser_digit)))

(defvar hydra_json_parser_whitespace ((hydra_parsers_map (lambda (_) nil)) (hydra_parsers_many (hydra_parsers_satisfy (lambda (c) (((hydra_lib_lists_foldl hydra_lib_logic_or) nil) (list ((hydra_lib_equality_equal c) 32) ((hydra_lib_equality_equal c) 9) ((hydra_lib_equality_equal c) 10) ((hydra_lib_equality_equal c) 13))))))))

(defvar hydra_json_parser_token (lambda (p) ((hydra_parsers_bind p) (lambda (x) ((hydra_parsers_bind hydra_json_parser_whitespace) (lambda (_) (hydra_parsers_pure x)))))))

(defvar hydra_json_parser_json_bool ((hydra_parsers_alt ((hydra_parsers_map (lambda (_) (list :boolean t))) (hydra_json_parser_token (hydra_parsers_string "true")))) ((hydra_parsers_map (lambda (_) (list :boolean nil))) (hydra_json_parser_token (hydra_parsers_string "false")))))

(defvar hydra_json_parser_json_null ((hydra_parsers_map (lambda (_) (list :null nil))) (hydra_json_parser_token (hydra_parsers_string "null"))))

(defvar hydra_json_parser_json_exponent_part (hydra_parsers_optional ((hydra_parsers_bind (hydra_parsers_satisfy (lambda (c) ((hydra_lib_logic_or ((hydra_lib_equality_equal c) 101)) ((hydra_lib_equality_equal c) 69))))) (lambda (_) ((hydra_parsers_bind (hydra_parsers_optional (hydra_parsers_satisfy (lambda (c) ((hydra_lib_logic_or ((hydra_lib_equality_equal c) 43)) ((hydra_lib_equality_equal c) 45)))))) (lambda (sign) ((hydra_parsers_map (lambda (digits) ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "e") (((hydra_lib_maybes_maybe "") (lambda (arg_) (hydra_lib_strings_from_list (hydra_lib_lists_pure arg_)))) sign))) digits))) hydra_json_parser_digits)))))))

(defvar hydra_json_parser_json_fraction_part (hydra_parsers_optional ((hydra_parsers_bind (hydra_parsers_char 46)) (lambda (_) ((hydra_parsers_map (lambda (d) ((hydra_lib_strings_cat2 ".") d))) hydra_json_parser_digits)))))

(defvar hydra_json_parser_json_integer_part ((hydra_parsers_bind (hydra_parsers_optional (hydra_parsers_char 45))) (lambda (sign) ((hydra_parsers_bind hydra_json_parser_digits) (lambda (digits) (hydra_parsers_pure (((hydra_lib_maybes_maybe digits) (lambda (_) ((hydra_lib_strings_cat2 "-") digits))) sign)))))))

(defvar hydra_json_parser_json_number (hydra_json_parser_token ((hydra_parsers_bind hydra_json_parser_json_integer_part) (lambda (int_part) ((hydra_parsers_bind hydra_json_parser_json_fraction_part) (lambda (frac_part) ((hydra_parsers_bind hydra_json_parser_json_exponent_part) (lambda (exp_part) (let ((num_str ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 int_part) (((hydra_lib_maybes_maybe "") hydra_lib_equality_identity) frac_part))) (((hydra_lib_maybes_maybe "") hydra_lib_equality_identity) exp_part)))) (hydra_parsers_pure (list :number (((hydra_lib_maybes_maybe 0.0) hydra_lib_equality_identity) (hydra_lib_literals_read_bigfloat num_str)))))))))))))

(defvar hydra_json_parser_json_escape_char (hydra_parsers_choice (list ((hydra_parsers_map (lambda (_) 34)) (hydra_parsers_char 34)) ((hydra_parsers_map (lambda (_) 92)) (hydra_parsers_char 92)) ((hydra_parsers_map (lambda (_) 47)) (hydra_parsers_char 47)) ((hydra_parsers_map (lambda (_) 8)) (hydra_parsers_char 98)) ((hydra_parsers_map (lambda (_) 12)) (hydra_parsers_char 102)) ((hydra_parsers_map (lambda (_) 10)) (hydra_parsers_char 110)) ((hydra_parsers_map (lambda (_) 13)) (hydra_parsers_char 114)) ((hydra_parsers_map (lambda (_) 9)) (hydra_parsers_char 116)))))

(defvar hydra_json_parser_json_string_char ((hydra_parsers_alt ((hydra_parsers_bind (hydra_parsers_char 92)) (lambda (_) hydra_json_parser_json_escape_char))) (hydra_parsers_satisfy (lambda (c) ((hydra_lib_logic_and (hydra_lib_logic_not ((hydra_lib_equality_equal c) 34))) (hydra_lib_logic_not ((hydra_lib_equality_equal c) 92)))))))

(defvar hydra_json_parser_json_string (hydra_json_parser_token ((hydra_parsers_bind (hydra_parsers_char 34)) (lambda (_) ((hydra_parsers_bind (hydra_parsers_many hydra_json_parser_json_string_char)) (lambda (chars) ((hydra_parsers_bind (hydra_parsers_char 34)) (lambda (_) (hydra_parsers_pure (list :string (hydra_lib_strings_from_list chars)))))))))))

(defvar hydra_json_parser_json_array ((hydra_parsers_map (lambda (x) (list :array x))) (((hydra_parsers_between (hydra_json_parser_token (hydra_parsers_char 91))) (hydra_json_parser_token (hydra_parsers_char 93))) ((hydra_parsers_sep_by (hydra_parsers_lazy (lambda (_) hydra_json_parser_json_value))) (hydra_json_parser_token (hydra_parsers_char 44))))))

(defvar hydra_json_parser_json_key_value ((hydra_parsers_bind (hydra_json_parser_token ((hydra_parsers_bind (hydra_parsers_char 34)) (lambda (_) ((hydra_parsers_bind (hydra_parsers_many hydra_json_parser_json_string_char)) (lambda (chars) ((hydra_parsers_bind (hydra_parsers_char 34)) (lambda (_) (hydra_parsers_pure (hydra_lib_strings_from_list chars)))))))))) (lambda (key) ((hydra_parsers_bind (hydra_json_parser_token (hydra_parsers_char 58))) (lambda (_) ((hydra_parsers_map (lambda (v) (list key v))) (hydra_parsers_lazy (lambda (_) hydra_json_parser_json_value))))))))

(defvar hydra_json_parser_json_object ((hydra_parsers_map (lambda (arg_) ((lambda (x) (list :object x)) (hydra_lib_maps_from_list arg_)))) (((hydra_parsers_between (hydra_json_parser_token (hydra_parsers_char 123))) (hydra_json_parser_token (hydra_parsers_char 125))) ((hydra_parsers_sep_by hydra_json_parser_json_key_value) (hydra_json_parser_token (hydra_parsers_char 44))))))

(defvar hydra_json_parser_json_value (hydra_parsers_choice (list hydra_json_parser_json_null hydra_json_parser_json_bool hydra_json_parser_json_number hydra_json_parser_json_string hydra_json_parser_json_array hydra_json_parser_json_object)))

(defvar hydra_json_parser_parse_json (lambda (input) (((lambda (v) v) ((hydra_parsers_bind hydra_json_parser_whitespace) (lambda (_) ((hydra_parsers_bind hydra_json_parser_json_value) (lambda (v) ((hydra_parsers_bind hydra_json_parser_whitespace) (lambda (_) ((hydra_parsers_bind hydra_parsers_eof) (lambda (_) (hydra_parsers_pure v)))))))))) input)))

(provide 'hydra.json.parser)
