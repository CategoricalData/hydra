(require 'cl-lib)

(require 'hydra.lib.chars)

(require 'hydra.lib.equality)

(require 'hydra.lib.lists)

(require 'hydra.lib.logic)

(require 'hydra.lib.maps)

(require 'hydra.lib.math)

(require 'hydra.lib.maybes)

(require 'hydra.lib.pairs)

(require 'hydra.lib.sets)

(require 'hydra.lib.strings)

(require 'hydra.util)

(defvar hydra_formatting_map_first_letter (lambda (mapping) (lambda (s) (if (hydra_lib_strings_null s) s (let ((list (hydra_lib_strings_to_list s))) (funcall (hydra_lib_maybes_from_maybe (lambda () s)) (funcall (hydra_lib_maybes_map (lambda (uc) (let ((first_letter (mapping (hydra_lib_strings_from_list (hydra_lib_lists_pure (hydra_lib_pairs_first uc)))))) (funcall (hydra_lib_strings_cat2 first_letter) (hydra_lib_strings_from_list (hydra_lib_pairs_second uc)))))) (hydra_lib_lists_uncons list))))))))

(defvar hydra_formatting_capitalize (hydra_formatting_map_first_letter hydra_lib_strings_to_upper))

(defvar hydra_formatting_decapitalize (hydra_formatting_map_first_letter hydra_lib_strings_to_lower))

(defvar hydra_formatting_convert_case (lambda (from) (lambda (to) (lambda (original) (let ((parts (let* ((by_caps (let ((split_on_uppercase (lambda (acc) (lambda (c) (funcall (hydra_lib_lists_concat2 (if (hydra_lib_chars_is_upper c) (list (list)) (list))) (funcall (hydra_lib_maybes_from_maybe (lambda () acc)) (funcall (hydra_lib_maybes_map (lambda (uc) (funcall (hydra_lib_lists_cons (funcall (hydra_lib_lists_cons c) (hydra_lib_pairs_first uc))) (hydra_lib_pairs_second uc)))) (hydra_lib_lists_uncons acc)))))))) (funcall (hydra_lib_lists_map hydra_lib_strings_from_list) (funcall (funcall (hydra_lib_lists_foldl split_on_uppercase) (list (list))) (hydra_lib_lists_reverse (hydra_lib_strings_to_list (hydra_formatting_decapitalize original))))))) (by_underscores (funcall (hydra_lib_strings_split_on "_") original))) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :camel) (funcall (lambda (_) by_caps) match_value)) ((equal (car match_target) :pascal) (funcall (lambda (_) by_caps) match_value)) ((equal (car match_target) :lower_snake) (funcall (lambda (_) by_underscores) match_value)) ((equal (car match_target) :upper_snake) (funcall (lambda (_) by_underscores) match_value)))) (cadr match_target))) from)))) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :camel) (funcall (lambda (_) (hydra_formatting_decapitalize (hydra_lib_strings_cat (funcall (hydra_lib_lists_map (lambda (arg_) (hydra_formatting_capitalize (hydra_lib_strings_to_lower arg_)))) parts)))) match_value)) ((equal (car match_target) :pascal) (funcall (lambda (_) (hydra_lib_strings_cat (funcall (hydra_lib_lists_map (lambda (arg_) (hydra_formatting_capitalize (hydra_lib_strings_to_lower arg_)))) parts))) match_value)) ((equal (car match_target) :lower_snake) (funcall (lambda (_) (funcall (hydra_lib_strings_intercalate "_") (funcall (hydra_lib_lists_map hydra_lib_strings_to_lower) parts))) match_value)) ((equal (car match_target) :upper_snake) (funcall (lambda (_) (funcall (hydra_lib_strings_intercalate "_") (funcall (hydra_lib_lists_map hydra_lib_strings_to_upper) parts))) match_value)))) (cadr match_target))) to))))))

(defvar hydra_formatting_convert_case_camel_to_lower_snake (funcall (hydra_formatting_convert_case (list :camel nil)) (list :lower_snake nil)))

(defvar hydra_formatting_convert_case_camel_or_underscore_to_lower_snake (lambda (s) (let ((parts (funcall (hydra_lib_strings_split_on "_") s))) (let ((snake_parts (funcall (hydra_lib_lists_map (lambda (p) (hydra_formatting_convert_case_camel_to_lower_snake p))) parts))) (funcall (hydra_lib_strings_intercalate "_") snake_parts)))))

(defvar hydra_formatting_convert_case_camel_to_upper_snake (funcall (hydra_formatting_convert_case (list :camel nil)) (list :upper_snake nil)))

(defvar hydra_formatting_convert_case_pascal_to_upper_snake (funcall (hydra_formatting_convert_case (list :pascal nil)) (list :upper_snake nil)))

(defvar hydra_formatting_escape_with_underscore (lambda (reserved) (lambda (s) (if (funcall (hydra_lib_sets_member s) reserved) (funcall (hydra_lib_strings_cat2 s) "_") s))))

(defvar hydra_formatting_indent_lines (lambda (s) (let ((indent (lambda (l) (funcall (hydra_lib_strings_cat2 "    ") l)))) (hydra_lib_strings_unlines (funcall (hydra_lib_lists_map indent) (hydra_lib_strings_lines s))))))

(defvar hydra_formatting_java_style_comment (lambda (s) (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 "/**\n") " * ")) s)) "\n */")))

(defvar hydra_formatting_non_alnum_to_underscores (lambda (input) (let ((is_alnum (lambda (c) (funcall (hydra_lib_logic_or (funcall (hydra_lib_logic_and (funcall (hydra_lib_equality_gte c) 65)) (funcall (hydra_lib_equality_lte c) 90))) (funcall (hydra_lib_logic_or (funcall (hydra_lib_logic_and (funcall (hydra_lib_equality_gte c) 97)) (funcall (hydra_lib_equality_lte c) 122))) (funcall (hydra_lib_logic_and (funcall (hydra_lib_equality_gte c) 48)) (funcall (hydra_lib_equality_lte c) 57))))))) (let ((replace (lambda (p) (lambda (c) (let ((s (hydra_lib_pairs_first p))) (let ((b (hydra_lib_pairs_second p))) (if (is_alnum c) (list (funcall (hydra_lib_lists_cons c) s) nil) (if b (list s t) (list (funcall (hydra_lib_lists_cons 95) s) t))))))))) (let ((result (funcall (funcall (hydra_lib_lists_foldl replace) (list (list) nil)) (hydra_lib_strings_to_list input)))) (hydra_lib_strings_from_list (hydra_lib_lists_reverse (hydra_lib_pairs_first result))))))))

(defvar hydra_formatting_strip_leading_and_trailing_whitespace (lambda (s) (hydra_lib_strings_from_list (funcall (hydra_lib_lists_drop_while hydra_lib_chars_is_space) (hydra_lib_lists_reverse (funcall (hydra_lib_lists_drop_while hydra_lib_chars_is_space) (hydra_lib_lists_reverse (hydra_lib_strings_to_list s))))))))

(defvar hydra_formatting_normalize_comment (lambda (s) (let ((stripped (hydra_formatting_strip_leading_and_trailing_whitespace s))) (if (hydra_lib_strings_null stripped) "" (let ((last_idx (funcall (hydra_lib_math_sub (hydra_lib_strings_length stripped)) 1))) (let ((appended (funcall (hydra_lib_strings_cat2 stripped) "."))) (funcall (funcall (hydra_lib_maybes_maybe (lambda () appended)) (lambda (last_char) (if (funcall (hydra_lib_equality_equal last_char) 46) stripped appended))) (funcall (hydra_lib_strings_maybe_char_at last_idx) stripped))))))))

(defvar hydra_formatting_sanitize_with_underscores (lambda (reserved) (lambda (s) (funcall (hydra_formatting_escape_with_underscore reserved) (hydra_formatting_non_alnum_to_underscores s)))))

(defvar hydra_formatting_show_list (lambda (f) (lambda (els) (hydra_lib_strings_cat (list "[" (funcall (hydra_lib_strings_intercalate ", ") (funcall (hydra_lib_lists_map f) els)) "]")))))

(defvar hydra_formatting_with_character_aliases (lambda (original) (let* ((aliases (hydra_lib_maps_from_list (list (list 32 "sp") (list 33 "excl") (list 34 "quot") (list 35 "num") (list 36 "dollar") (list 37 "percnt") (list 38 "amp") (list 39 "apos") (list 40 "lpar") (list 41 "rpar") (list 42 "ast") (list 43 "plus") (list 44 "comma") (list 45 "minus") (list 46 "period") (list 47 "sol") (list 58 "colon") (list 59 "semi") (list 60 "lt") (list 61 "equals") (list 62 "gt") (list 63 "quest") (list 64 "commat") (list 91 "lsqb") (list 92 "bsol") (list 93 "rsqb") (list 94 "circ") (list 95 "lowbar") (list 96 "grave") (list 123 "lcub") (list 124 "verbar") (list 125 "rcub") (list 126 "tilde")))) (alias (lambda (c) (funcall (hydra_lib_maybes_from_maybe (lambda () (hydra_lib_lists_pure c))) (funcall (hydra_lib_maybes_map hydra_lib_strings_to_list) (funcall (hydra_lib_maps_lookup c) aliases)))))) (hydra_lib_strings_from_list (funcall (hydra_lib_lists_filter hydra_lib_chars_is_alpha_num) (hydra_lib_lists_concat (funcall (hydra_lib_lists_map alias) (hydra_lib_strings_to_list original))))))))

(defvar hydra_formatting_wrap_line (lambda (maxlen) (lambda (input) (letrec ((helper (lambda (prev) (lambda (rem) (let* ((trunc (funcall (hydra_lib_lists_take maxlen) rem)) (span_result (funcall (hydra_lib_lists_span (lambda (c) (funcall (hydra_lib_logic_and (hydra_lib_logic_not (funcall (hydra_lib_equality_equal c) 32))) (hydra_lib_logic_not (funcall (hydra_lib_equality_equal c) 9))))) (hydra_lib_lists_reverse trunc))) (prefix (hydra_lib_lists_reverse (hydra_lib_pairs_second span_result))) (suffix (hydra_lib_lists_reverse (hydra_lib_pairs_first span_result)))) (if (funcall (hydra_lib_equality_lte (hydra_lib_lists_length rem)) maxlen) (hydra_lib_lists_reverse (funcall (hydra_lib_lists_cons rem) prev)) (if (hydra_lib_lists_null prefix) (funcall (helper (funcall (hydra_lib_lists_cons trunc) prev)) (funcall (hydra_lib_lists_drop maxlen) rem)) (funcall (hydra_lib_maybes_from_maybe (lambda () (funcall (helper (funcall (hydra_lib_lists_cons trunc) prev)) (funcall (hydra_lib_lists_drop maxlen) rem)))) (funcall (hydra_lib_maybes_map (lambda (pfx_init) (funcall (helper (funcall (hydra_lib_lists_cons pfx_init) prev)) (funcall (hydra_lib_lists_concat2 suffix) (funcall (hydra_lib_lists_drop maxlen) rem))))) (hydra_lib_lists_maybe_init prefix)))))))))) (hydra_lib_strings_from_list (funcall (hydra_lib_lists_intercalate (list 10)) (funcall (helper (list)) (hydra_lib_strings_to_list input))))))))

(provide 'hydra.formatting)
