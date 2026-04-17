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

(defvar hydra_json_parser_digit (hydra_parsers_satisfy (lambda (c) (funcall (hydra_lib_logic_and (funcall (hydra_lib_equality_gte c) 48)) (funcall (hydra_lib_equality_lte c) 57)))))

(defvar hydra_json_parser_digits (funcall (hydra_parsers_map hydra_lib_strings_from_list) (hydra_parsers_some hydra_json_parser_digit)))

(defvar hydra_json_parser_whitespace (funcall (hydra_parsers_map (lambda (_) nil)) (hydra_parsers_many (hydra_parsers_satisfy (lambda (c) (funcall (funcall (hydra_lib_lists_foldl hydra_lib_logic_or) nil) (list (funcall (hydra_lib_equality_equal c) 32) (funcall (hydra_lib_equality_equal c) 9) (funcall (hydra_lib_equality_equal c) 10) (funcall (hydra_lib_equality_equal c) 13))))))))

(defvar hydra_json_parser_token (lambda (p) (funcall (hydra_parsers_bind p) (lambda (x) (funcall (hydra_parsers_bind hydra_json_parser_whitespace) (lambda (_) (hydra_parsers_pure x)))))))

(defvar hydra_json_parser_json_bool (funcall (hydra_parsers_alt (funcall (hydra_parsers_map (lambda (_) (list :boolean t))) (hydra_json_parser_token (hydra_parsers_string "true")))) (funcall (hydra_parsers_map (lambda (_) (list :boolean nil))) (hydra_json_parser_token (hydra_parsers_string "false")))))

(defvar hydra_json_parser_json_null (funcall (hydra_parsers_map (lambda (_) (list :null nil))) (hydra_json_parser_token (hydra_parsers_string "null"))))

(defvar hydra_json_parser_json_exponent_part (hydra_parsers_optional (funcall (hydra_parsers_bind (hydra_parsers_satisfy (lambda (c) (funcall (hydra_lib_logic_or (funcall (hydra_lib_equality_equal c) 101)) (funcall (hydra_lib_equality_equal c) 69))))) (lambda (_) (funcall (hydra_parsers_bind (hydra_parsers_optional (hydra_parsers_satisfy (lambda (c) (funcall (hydra_lib_logic_or (funcall (hydra_lib_equality_equal c) 43)) (funcall (hydra_lib_equality_equal c) 45)))))) (lambda (sign) (funcall (hydra_parsers_map (lambda (digits) (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 "e") (funcall (funcall (hydra_lib_maybes_maybe (lambda () "")) (lambda (arg_) (hydra_lib_strings_from_list (hydra_lib_lists_pure arg_)))) sign))) digits))) hydra_json_parser_digits)))))))

(defvar hydra_json_parser_json_fraction_part (hydra_parsers_optional (funcall (hydra_parsers_bind (hydra_parsers_char 46)) (lambda (_) (funcall (hydra_parsers_map (lambda (d) (funcall (hydra_lib_strings_cat2 ".") d))) hydra_json_parser_digits)))))

(defvar hydra_json_parser_json_integer_part (funcall (hydra_parsers_bind (hydra_parsers_optional (hydra_parsers_char 45))) (lambda (sign) (funcall (hydra_parsers_bind hydra_json_parser_digits) (lambda (digits) (hydra_parsers_pure (funcall (funcall (hydra_lib_maybes_maybe (lambda () digits)) (lambda (_) (funcall (hydra_lib_strings_cat2 "-") digits))) sign)))))))

(defvar hydra_json_parser_json_number (hydra_json_parser_token (funcall (hydra_parsers_bind hydra_json_parser_json_integer_part) (lambda (int_part) (funcall (hydra_parsers_bind hydra_json_parser_json_fraction_part) (lambda (frac_part) (funcall (hydra_parsers_bind hydra_json_parser_json_exponent_part) (lambda (exp_part) (let ((num_str (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 int_part) (funcall (funcall (hydra_lib_maybes_maybe (lambda () "")) hydra_lib_equality_identity) frac_part))) (funcall (funcall (hydra_lib_maybes_maybe (lambda () "")) hydra_lib_equality_identity) exp_part)))) (hydra_parsers_pure (list :number (funcall (funcall (hydra_lib_maybes_maybe (lambda () 0.0)) hydra_lib_equality_identity) (hydra_lib_literals_read_decimal num_str)))))))))))))

(defvar hydra_json_parser_json_escape_char (hydra_parsers_choice (list (funcall (hydra_parsers_map (lambda (_) 34)) (hydra_parsers_char 34)) (funcall (hydra_parsers_map (lambda (_) 92)) (hydra_parsers_char 92)) (funcall (hydra_parsers_map (lambda (_) 47)) (hydra_parsers_char 47)) (funcall (hydra_parsers_map (lambda (_) 8)) (hydra_parsers_char 98)) (funcall (hydra_parsers_map (lambda (_) 12)) (hydra_parsers_char 102)) (funcall (hydra_parsers_map (lambda (_) 10)) (hydra_parsers_char 110)) (funcall (hydra_parsers_map (lambda (_) 13)) (hydra_parsers_char 114)) (funcall (hydra_parsers_map (lambda (_) 9)) (hydra_parsers_char 116)))))

(defvar hydra_json_parser_json_string_char (funcall (hydra_parsers_alt (funcall (hydra_parsers_bind (hydra_parsers_char 92)) (lambda (_) hydra_json_parser_json_escape_char))) (hydra_parsers_satisfy (lambda (c) (funcall (hydra_lib_logic_and (hydra_lib_logic_not (funcall (hydra_lib_equality_equal c) 34))) (hydra_lib_logic_not (funcall (hydra_lib_equality_equal c) 92)))))))

(defvar hydra_json_parser_json_string (hydra_json_parser_token (funcall (hydra_parsers_bind (hydra_parsers_char 34)) (lambda (_) (funcall (hydra_parsers_bind (hydra_parsers_many hydra_json_parser_json_string_char)) (lambda (chars) (funcall (hydra_parsers_bind (hydra_parsers_char 34)) (lambda (_2) (hydra_parsers_pure (list :string (hydra_lib_strings_from_list chars)))))))))))

(defvar hydra_json_parser_json_array (funcall (hydra_parsers_map (lambda (x) (list :array x))) (funcall (funcall (hydra_parsers_between (hydra_json_parser_token (hydra_parsers_char 91))) (hydra_json_parser_token (hydra_parsers_char 93))) (funcall (hydra_parsers_sep_by (hydra_parsers_lazy (lambda (_) hydra_json_parser_json_value))) (hydra_json_parser_token (hydra_parsers_char 44))))))

(defvar hydra_json_parser_json_key_value (funcall (hydra_parsers_bind (hydra_json_parser_token (funcall (hydra_parsers_bind (hydra_parsers_char 34)) (lambda (_) (funcall (hydra_parsers_bind (hydra_parsers_many hydra_json_parser_json_string_char)) (lambda (chars) (funcall (hydra_parsers_bind (hydra_parsers_char 34)) (lambda (_2) (hydra_parsers_pure (hydra_lib_strings_from_list chars)))))))))) (lambda (key) (funcall (hydra_parsers_bind (hydra_json_parser_token (hydra_parsers_char 58))) (lambda (_) (funcall (hydra_parsers_map (lambda (v) (list key v))) (hydra_parsers_lazy (lambda (_2) hydra_json_parser_json_value))))))))

(defvar hydra_json_parser_json_object (funcall (hydra_parsers_map (lambda (arg_) (funcall (lambda (x) (list :object x)) (hydra_lib_maps_from_list arg_)))) (funcall (funcall (hydra_parsers_between (hydra_json_parser_token (hydra_parsers_char 123))) (hydra_json_parser_token (hydra_parsers_char 125))) (funcall (hydra_parsers_sep_by hydra_json_parser_json_key_value) (hydra_json_parser_token (hydra_parsers_char 44))))))

(defvar hydra_json_parser_json_value (hydra_parsers_choice (list hydra_json_parser_json_null hydra_json_parser_json_bool hydra_json_parser_json_number hydra_json_parser_json_string hydra_json_parser_json_array hydra_json_parser_json_object)))

(defvar hydra_json_parser_parse_json (lambda (input) (funcall (funcall (lambda (v) v) (funcall (hydra_parsers_bind hydra_json_parser_whitespace) (lambda (_) (funcall (hydra_parsers_bind hydra_json_parser_json_value) (lambda (v) (funcall (hydra_parsers_bind hydra_json_parser_whitespace) (lambda (_2) (funcall (hydra_parsers_bind hydra_parsers_eof) (lambda (_3) (hydra_parsers_pure v)))))))))) input)))

(provide 'hydra.json.parser)
