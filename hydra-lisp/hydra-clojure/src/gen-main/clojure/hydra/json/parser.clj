(ns hydra.json.parser
  (:require [hydra.json.model :refer :all] [hydra.lib.equality :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.literals :refer :all] [hydra.lib.logic :refer :all] [hydra.lib.maps :refer :all] [hydra.lib.maybes :refer :all] [hydra.lib.strings :refer :all] [hydra.parsers :refer :all] [hydra.parsing :refer :all]
))

(declare hydra_json_parser_digit hydra_json_parser_digits hydra_json_parser_whitespace hydra_json_parser_token hydra_json_parser_json_bool hydra_json_parser_json_null hydra_json_parser_json_exponent_part hydra_json_parser_json_fraction_part hydra_json_parser_json_integer_part hydra_json_parser_json_number hydra_json_parser_json_escape_char hydra_json_parser_json_string_char hydra_json_parser_json_string hydra_json_parser_json_array hydra_json_parser_json_key_value hydra_json_parser_json_object hydra_json_parser_json_value hydra_json_parser_parse_json)

(def hydra_json_parser_digit (hydra_parsers_satisfy (fn [c] ((hydra_lib_logic_and ((hydra_lib_equality_gte c) 48)) ((hydra_lib_equality_lte c) 57)))))

(def hydra_json_parser_digits ((hydra_parsers_map hydra_lib_strings_from_list) (hydra_parsers_some hydra_json_parser_digit)))

(def hydra_json_parser_whitespace ((hydra_parsers_map (fn [_] nil)) (hydra_parsers_many (hydra_parsers_satisfy (fn [c] (((hydra_lib_lists_foldl hydra_lib_logic_or) false) (list ((hydra_lib_equality_equal c) 32) ((hydra_lib_equality_equal c) 9) ((hydra_lib_equality_equal c) 10) ((hydra_lib_equality_equal c) 13))))))))

(def hydra_json_parser_token (fn [p] ((hydra_parsers_bind p) (fn [x] ((hydra_parsers_bind hydra_json_parser_whitespace) (fn [_] (hydra_parsers_pure x)))))))

(def hydra_json_parser_json_bool ((hydra_parsers_alt ((hydra_parsers_map (fn [_] (list :boolean true))) (hydra_json_parser_token (hydra_parsers_string "true")))) ((hydra_parsers_map (fn [_] (list :boolean false))) (hydra_json_parser_token (hydra_parsers_string "false")))))

(def hydra_json_parser_json_null ((hydra_parsers_map (fn [_] (list :null nil))) (hydra_json_parser_token (hydra_parsers_string "null"))))

(def hydra_json_parser_json_exponent_part (hydra_parsers_optional ((hydra_parsers_bind (hydra_parsers_satisfy (fn [c] ((hydra_lib_logic_or ((hydra_lib_equality_equal c) 101)) ((hydra_lib_equality_equal c) 69))))) (fn [_] ((hydra_parsers_bind (hydra_parsers_optional (hydra_parsers_satisfy (fn [c] ((hydra_lib_logic_or ((hydra_lib_equality_equal c) 43)) ((hydra_lib_equality_equal c) 45)))))) (fn [sign] ((hydra_parsers_map (fn [digits] ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "e") (((hydra_lib_maybes_maybe (fn [] "")) (fn [arg_] (hydra_lib_strings_from_list (hydra_lib_lists_pure arg_)))) sign))) digits))) hydra_json_parser_digits)))))))

(def hydra_json_parser_json_fraction_part (hydra_parsers_optional ((hydra_parsers_bind (hydra_parsers_char 46)) (fn [_] ((hydra_parsers_map (fn [d] ((hydra_lib_strings_cat2 ".") d))) hydra_json_parser_digits)))))

(def hydra_json_parser_json_integer_part ((hydra_parsers_bind (hydra_parsers_optional (hydra_parsers_char 45))) (fn [sign] ((hydra_parsers_bind hydra_json_parser_digits) (fn [digits] (hydra_parsers_pure (((hydra_lib_maybes_maybe (fn [] digits)) (fn [_] ((hydra_lib_strings_cat2 "-") digits))) sign)))))))

(def hydra_json_parser_json_number (hydra_json_parser_token ((hydra_parsers_bind hydra_json_parser_json_integer_part) (fn [int_part] ((hydra_parsers_bind hydra_json_parser_json_fraction_part) (fn [frac_part] ((hydra_parsers_bind hydra_json_parser_json_exponent_part) (fn [exp_part] (let [num_str ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 int_part) (((hydra_lib_maybes_maybe (fn [] "")) hydra_lib_equality_identity) frac_part))) (((hydra_lib_maybes_maybe (fn [] "")) hydra_lib_equality_identity) exp_part))] (hydra_parsers_pure (list :number (((hydra_lib_maybes_maybe (fn [] 0.0)) hydra_lib_equality_identity) (hydra_lib_literals_read_bigfloat num_str)))))))))))))

(def hydra_json_parser_json_escape_char (hydra_parsers_choice (list ((hydra_parsers_map (fn [_] 34)) (hydra_parsers_char 34)) ((hydra_parsers_map (fn [_] 92)) (hydra_parsers_char 92)) ((hydra_parsers_map (fn [_] 47)) (hydra_parsers_char 47)) ((hydra_parsers_map (fn [_] 8)) (hydra_parsers_char 98)) ((hydra_parsers_map (fn [_] 12)) (hydra_parsers_char 102)) ((hydra_parsers_map (fn [_] 10)) (hydra_parsers_char 110)) ((hydra_parsers_map (fn [_] 13)) (hydra_parsers_char 114)) ((hydra_parsers_map (fn [_] 9)) (hydra_parsers_char 116)))))

(def hydra_json_parser_json_string_char ((hydra_parsers_alt ((hydra_parsers_bind (hydra_parsers_char 92)) (fn [_] hydra_json_parser_json_escape_char))) (hydra_parsers_satisfy (fn [c] ((hydra_lib_logic_and (hydra_lib_logic_not ((hydra_lib_equality_equal c) 34))) (hydra_lib_logic_not ((hydra_lib_equality_equal c) 92)))))))

(def hydra_json_parser_json_string (hydra_json_parser_token ((hydra_parsers_bind (hydra_parsers_char 34)) (fn [_] ((hydra_parsers_bind (hydra_parsers_many hydra_json_parser_json_string_char)) (fn [chars] ((hydra_parsers_bind (hydra_parsers_char 34)) (fn [_2] (hydra_parsers_pure (list :string (hydra_lib_strings_from_list chars)))))))))))

(def hydra_json_parser_json_array ((hydra_parsers_map (fn [x] (list :array x))) (((hydra_parsers_between (hydra_json_parser_token (hydra_parsers_char 91))) (hydra_json_parser_token (hydra_parsers_char 93))) ((hydra_parsers_sep_by (hydra_parsers_lazy (fn [_] hydra_json_parser_json_value))) (hydra_json_parser_token (hydra_parsers_char 44))))))

(def hydra_json_parser_json_key_value ((hydra_parsers_bind (hydra_json_parser_token ((hydra_parsers_bind (hydra_parsers_char 34)) (fn [_] ((hydra_parsers_bind (hydra_parsers_many hydra_json_parser_json_string_char)) (fn [chars] ((hydra_parsers_bind (hydra_parsers_char 34)) (fn [_2] (hydra_parsers_pure (hydra_lib_strings_from_list chars)))))))))) (fn [key] ((hydra_parsers_bind (hydra_json_parser_token (hydra_parsers_char 58))) (fn [_] ((hydra_parsers_map (fn [v] (list key v))) (hydra_parsers_lazy (fn [_2] hydra_json_parser_json_value))))))))

(def hydra_json_parser_json_object ((hydra_parsers_map (fn [arg_] ((fn [x] (list :object x)) (hydra_lib_maps_from_list arg_)))) (((hydra_parsers_between (hydra_json_parser_token (hydra_parsers_char 123))) (hydra_json_parser_token (hydra_parsers_char 125))) ((hydra_parsers_sep_by hydra_json_parser_json_key_value) (hydra_json_parser_token (hydra_parsers_char 44))))))

(def hydra_json_parser_json_value (hydra_parsers_choice (list hydra_json_parser_json_null hydra_json_parser_json_bool hydra_json_parser_json_number hydra_json_parser_json_string hydra_json_parser_json_array hydra_json_parser_json_object)))

(def hydra_json_parser_parse_json (fn [input] (((fn [v] v) ((hydra_parsers_bind hydra_json_parser_whitespace) (fn [_] ((hydra_parsers_bind hydra_json_parser_json_value) (fn [v] ((hydra_parsers_bind hydra_json_parser_whitespace) (fn [_2] ((hydra_parsers_bind hydra_parsers_eof) (fn [_3] (hydra_parsers_pure v)))))))))) input)))
