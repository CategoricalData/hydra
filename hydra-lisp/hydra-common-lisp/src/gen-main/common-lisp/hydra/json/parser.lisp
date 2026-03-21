(defpackage :hydra.json.parser
(:use :cl :hydra.json.model :hydra.lib.equality :hydra.lib.lists :hydra.lib.literals :hydra.lib.logic :hydra.lib.maps :hydra.lib.maybes :hydra.lib.strings :hydra.parsers :hydra.parsing)
(:export :hydra_json_parser_digit :hydra_json_parser_digits :hydra_json_parser_whitespace :hydra_json_parser_token :hydra_json_parser_json_bool :hydra_json_parser_json_null :hydra_json_parser_json_exponent_part :hydra_json_parser_json_fraction_part :hydra_json_parser_json_integer_part :hydra_json_parser_json_number :hydra_json_parser_json_escape_char :hydra_json_parser_json_string_char :hydra_json_parser_json_string :hydra_json_parser_json_array :hydra_json_parser_json_key_value :hydra_json_parser_json_object :hydra_json_parser_json_value :hydra_json_parser_parse_json))

(in-package :hydra.json.parser)

(cl:defvar hydra_json_parser_digit (hydra_parsers_satisfy (cl:lambda (c) ((hydra_lib_logic_and ((hydra_lib_equality_gte c) 48)) ((hydra_lib_equality_lte c) 57)))))

(cl:defvar hydra_json_parser_digits ((hydra_parsers_map hydra_lib_strings_from_list) (hydra_parsers_some hydra_json_parser_digit)))

(cl:defvar hydra_json_parser_whitespace ((hydra_parsers_map (cl:lambda (_) cl:nil)) (hydra_parsers_many (hydra_parsers_satisfy (cl:lambda (c) (((hydra_lib_lists_foldl hydra_lib_logic_or) cl:nil) (cl:list ((hydra_lib_equality_equal c) 32) ((hydra_lib_equality_equal c) 9) ((hydra_lib_equality_equal c) 10) ((hydra_lib_equality_equal c) 13))))))))

(cl:defvar hydra_json_parser_token (cl:lambda (p) ((hydra_parsers_bind p) (cl:lambda (x) ((hydra_parsers_bind hydra_json_parser_whitespace) (cl:lambda (_) (hydra_parsers_pure x)))))))

(cl:defvar hydra_json_parser_json_bool ((hydra_parsers_alt ((hydra_parsers_map (cl:lambda (_) (list :boolean cl:t))) (hydra_json_parser_token (hydra_parsers_string "true")))) ((hydra_parsers_map (cl:lambda (_) (list :boolean cl:nil))) (hydra_json_parser_token (hydra_parsers_string "false")))))

(cl:defvar hydra_json_parser_json_null ((hydra_parsers_map (cl:lambda (_) (list :null cl:nil))) (hydra_json_parser_token (hydra_parsers_string "null"))))

(cl:defvar hydra_json_parser_json_exponent_part (hydra_parsers_optional ((hydra_parsers_bind (hydra_parsers_satisfy (cl:lambda (c) ((hydra_lib_logic_or ((hydra_lib_equality_equal c) 101)) ((hydra_lib_equality_equal c) 69))))) (cl:lambda (_) ((hydra_parsers_bind (hydra_parsers_optional (hydra_parsers_satisfy (cl:lambda (c) ((hydra_lib_logic_or ((hydra_lib_equality_equal c) 43)) ((hydra_lib_equality_equal c) 45)))))) (cl:lambda (sign) ((hydra_parsers_map (cl:lambda (digits) ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "e") (((hydra_lib_maybes_maybe (cl:lambda () "")) (cl:lambda (arg_) (hydra_lib_strings_from_list (hydra_lib_lists_pure arg_)))) sign))) digits))) hydra_json_parser_digits)))))))

(cl:defvar hydra_json_parser_json_fraction_part (hydra_parsers_optional ((hydra_parsers_bind (hydra_parsers_char 46)) (cl:lambda (_) ((hydra_parsers_map (cl:lambda (d) ((hydra_lib_strings_cat2 ".") d))) hydra_json_parser_digits)))))

(cl:defvar hydra_json_parser_json_integer_part ((hydra_parsers_bind (hydra_parsers_optional (hydra_parsers_char 45))) (cl:lambda (sign) ((hydra_parsers_bind hydra_json_parser_digits) (cl:lambda (digits) (hydra_parsers_pure (((hydra_lib_maybes_maybe (cl:lambda () digits)) (cl:lambda (_) ((hydra_lib_strings_cat2 "-") digits))) sign)))))))

(cl:defvar hydra_json_parser_json_number (hydra_json_parser_token ((hydra_parsers_bind hydra_json_parser_json_integer_part) (cl:lambda (int_part) ((hydra_parsers_bind hydra_json_parser_json_fraction_part) (cl:lambda (frac_part) ((hydra_parsers_bind hydra_json_parser_json_exponent_part) (cl:lambda (exp_part) (let ((num_str ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 int_part) (((hydra_lib_maybes_maybe (cl:lambda () "")) hydra_lib_equality_identity) frac_part))) (((hydra_lib_maybes_maybe (cl:lambda () "")) hydra_lib_equality_identity) exp_part)))) (hydra_parsers_pure (list :number (((hydra_lib_maybes_maybe (cl:lambda () 0.0)) hydra_lib_equality_identity) (hydra_lib_literals_read_bigfloat num_str)))))))))))))

(cl:defvar hydra_json_parser_json_escape_char (hydra_parsers_choice (cl:list ((hydra_parsers_map (cl:lambda (_) 34)) (hydra_parsers_char 34)) ((hydra_parsers_map (cl:lambda (_) 92)) (hydra_parsers_char 92)) ((hydra_parsers_map (cl:lambda (_) 47)) (hydra_parsers_char 47)) ((hydra_parsers_map (cl:lambda (_) 8)) (hydra_parsers_char 98)) ((hydra_parsers_map (cl:lambda (_) 12)) (hydra_parsers_char 102)) ((hydra_parsers_map (cl:lambda (_) 10)) (hydra_parsers_char 110)) ((hydra_parsers_map (cl:lambda (_) 13)) (hydra_parsers_char 114)) ((hydra_parsers_map (cl:lambda (_) 9)) (hydra_parsers_char 116)))))

(cl:defvar hydra_json_parser_json_string_char ((hydra_parsers_alt ((hydra_parsers_bind (hydra_parsers_char 92)) (cl:lambda (_) hydra_json_parser_json_escape_char))) (hydra_parsers_satisfy (cl:lambda (c) ((hydra_lib_logic_and (hydra_lib_logic_not ((hydra_lib_equality_equal c) 34))) (hydra_lib_logic_not ((hydra_lib_equality_equal c) 92)))))))

(cl:defvar hydra_json_parser_json_string (hydra_json_parser_token ((hydra_parsers_bind (hydra_parsers_char 34)) (cl:lambda (_) ((hydra_parsers_bind (hydra_parsers_many hydra_json_parser_json_string_char)) (cl:lambda (chars) ((hydra_parsers_bind (hydra_parsers_char 34)) (cl:lambda (_) (hydra_parsers_pure (list :string (hydra_lib_strings_from_list chars)))))))))))

(cl:defvar hydra_json_parser_json_array ((hydra_parsers_map (cl:lambda (x) (list :array x))) (((hydra_parsers_between (hydra_json_parser_token (hydra_parsers_char 91))) (hydra_json_parser_token (hydra_parsers_char 93))) ((hydra_parsers_sep_by (hydra_parsers_lazy (cl:lambda (_) hydra_json_parser_json_value))) (hydra_json_parser_token (hydra_parsers_char 44))))))

(cl:defvar hydra_json_parser_json_key_value ((hydra_parsers_bind (hydra_json_parser_token ((hydra_parsers_bind (hydra_parsers_char 34)) (cl:lambda (_) ((hydra_parsers_bind (hydra_parsers_many hydra_json_parser_json_string_char)) (cl:lambda (chars) ((hydra_parsers_bind (hydra_parsers_char 34)) (cl:lambda (_) (hydra_parsers_pure (hydra_lib_strings_from_list chars)))))))))) (cl:lambda (key) ((hydra_parsers_bind (hydra_json_parser_token (hydra_parsers_char 58))) (cl:lambda (_) ((hydra_parsers_map (cl:lambda (v) (cl:list key v))) (hydra_parsers_lazy (cl:lambda (_) hydra_json_parser_json_value))))))))

(cl:defvar hydra_json_parser_json_object ((hydra_parsers_map (cl:lambda (arg_) ((cl:lambda (x) (list :object x)) (hydra_lib_maps_from_list arg_)))) (((hydra_parsers_between (hydra_json_parser_token (hydra_parsers_char 123))) (hydra_json_parser_token (hydra_parsers_char 125))) ((hydra_parsers_sep_by hydra_json_parser_json_key_value) (hydra_json_parser_token (hydra_parsers_char 44))))))

(cl:defvar hydra_json_parser_json_value (hydra_parsers_choice (cl:list hydra_json_parser_json_null hydra_json_parser_json_bool hydra_json_parser_json_number hydra_json_parser_json_string hydra_json_parser_json_array hydra_json_parser_json_object)))

(cl:defvar hydra_json_parser_parse_json (cl:lambda (input) (((cl:lambda (v) v) ((hydra_parsers_bind hydra_json_parser_whitespace) (cl:lambda (_) ((hydra_parsers_bind hydra_json_parser_json_value) (cl:lambda (v) ((hydra_parsers_bind hydra_json_parser_whitespace) (cl:lambda (_) ((hydra_parsers_bind hydra_parsers_eof) (cl:lambda (_) (hydra_parsers_pure v)))))))))) input)))
