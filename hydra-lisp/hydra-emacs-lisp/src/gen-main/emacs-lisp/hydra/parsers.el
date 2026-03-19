(require 'cl-lib)

(require 'hydra.lib.equality)

(require 'hydra.lib.lists)

(require 'hydra.lib.logic)

(require 'hydra.lib.maybes)

(require 'hydra.lib.strings)

(require 'hydra.parsing)

(defvar hydra_parsers_alt (lambda (p1) (lambda (p2) (let ((parse (lambda (input) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :success) (funcall (lambda (s) (list :success s)) match_value)) ((equal (car match_target) :failure) (funcall (lambda (e) (if (funcall (hydra_lib_equality_equal (funcall (lambda (v) (hydra_parsing_parse_error-remainder v)) e)) input) (funcall (funcall (lambda (v) v) p2) input) (list :failure e))) match_value)))) (cadr match_target))) (funcall (funcall (lambda (v) v) p1) input))))) parse))))

(defvar hydra_parsers_satisfy (lambda (pred) (let ((parse (lambda (input) (let ((codes (hydra_lib_strings_to_list input))) (funcall (funcall (hydra_lib_maybes_maybe (list :failure (make-hydra_parsing_parse_error "unexpected end of input" input))) (lambda (c) (let ((rest (hydra_lib_strings_from_list (funcall (hydra_lib_lists_drop 1) codes)))) (if (pred c) (list :success (make-hydra_parsing_parse_success c rest)) (list :failure (make-hydra_parsing_parse_error "character did not satisfy predicate" input)))))) (hydra_lib_lists_safe_head codes)))))) parse)))

(defvar hydra_parsers_any_char (hydra_parsers_satisfy (lambda (_) t)))

(defvar hydra_parsers_apply (lambda (pf) (lambda (pa) (let ((parse (lambda (input) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :success) (funcall (lambda (sf) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :success) (funcall (lambda (sa) (list :success (make-hydra_parsing_parse_success (funcall (funcall (lambda (v) (hydra_parsing_parse_success-value v)) sf) (funcall (lambda (v) (hydra_parsing_parse_success-value v)) sa)) (funcall (lambda (v) (hydra_parsing_parse_success-remainder v)) sa)))) match_value)) ((equal (car match_target) :failure) (funcall (lambda (e) (list :failure e)) match_value)))) (cadr match_target))) (funcall (funcall (lambda (v) v) pa) (funcall (lambda (v) (hydra_parsing_parse_success-remainder v)) sf)))) match_value)) ((equal (car match_target) :failure) (funcall (lambda (e) (list :failure e)) match_value)))) (cadr match_target))) (funcall (funcall (lambda (v) v) pf) input))))) parse))))

(defvar hydra_parsers_bind (lambda (pa) (lambda (f) (let ((parse (lambda (input) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :success) (funcall (lambda (s) (funcall (funcall (lambda (v) v) (f (funcall (lambda (v) (hydra_parsing_parse_success-value v)) s))) (funcall (lambda (v) (hydra_parsing_parse_success-remainder v)) s))) match_value)) ((equal (car match_target) :failure) (funcall (lambda (e) (list :failure e)) match_value)))) (cadr match_target))) (funcall (funcall (lambda (v) v) pa) input))))) parse))))

(defvar hydra_parsers_pure (lambda (a) (lambda (input) (list :success (make-hydra_parsing_parse_success a input)))))

(defvar hydra_parsers_between (lambda (open) (lambda (close) (lambda (p) (funcall (hydra_parsers_bind open) (lambda (_) (funcall (hydra_parsers_bind p) (lambda (x) (funcall (hydra_parsers_bind close) (lambda (_) (hydra_parsers_pure x)))))))))))

(defvar hydra_parsers_char (lambda (c) (hydra_parsers_satisfy (lambda (x) (funcall (hydra_lib_equality_equal x) c)))))

(defvar hydra_parsers_fail (lambda (msg) (lambda (input) (list :failure (make-hydra_parsing_parse_error msg input)))))

(defvar hydra_parsers_choice (lambda (ps) (funcall (funcall (hydra_lib_lists_foldl hydra_parsers_alt) (hydra_parsers_fail "no choice matched")) ps)))

(defvar hydra_parsers_eof (lambda (input) (if (funcall (hydra_lib_equality_equal input) "") (list :success (make-hydra_parsing_parse_success nil "")) (list :failure (make-hydra_parsing_parse_error "expected end of input" input)))))

(defvar hydra_parsers_lazy (lambda (f) (lambda (input) (funcall (funcall (lambda (v) v) (f nil)) input))))

(defvar hydra_parsers_many (lambda (p) (funcall (hydra_parsers_alt (hydra_parsers_some p)) (hydra_parsers_pure (list)))))

(defvar hydra_parsers_some (lambda (p) (funcall (hydra_parsers_bind p) (lambda (x) (funcall (hydra_parsers_bind (hydra_parsers_many p)) (lambda (xs) (hydra_parsers_pure (funcall (hydra_lib_lists_cons x) xs))))))))

(defvar hydra_parsers_map (lambda (f) (lambda (pa) (let ((parse (lambda (input) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :success) (funcall (lambda (s) (list :success (make-hydra_parsing_parse_success (f (funcall (lambda (v) (hydra_parsing_parse_success-value v)) s)) (funcall (lambda (v) (hydra_parsing_parse_success-remainder v)) s)))) match_value)) ((equal (car match_target) :failure) (funcall (lambda (e) (list :failure e)) match_value)))) (cadr match_target))) (funcall (funcall (lambda (v) v) pa) input))))) parse))))

(defvar hydra_parsers_optional (lambda (p) (funcall (hydra_parsers_alt (funcall (hydra_parsers_map hydra_lib_maybes_pure) p)) (hydra_parsers_pure (list :nothing)))))

(defvar hydra_parsers_run_parser (lambda (p) (lambda (input) (funcall (funcall (lambda (v) v) p) input))))

(defvar hydra_parsers_sep_by1 (lambda (p) (lambda (sep) (funcall (hydra_parsers_bind p) (lambda (x) (funcall (hydra_parsers_bind (hydra_parsers_many (funcall (hydra_parsers_bind sep) (lambda (_) p)))) (lambda (xs) (hydra_parsers_pure (funcall (hydra_lib_lists_cons x) xs)))))))))

(defvar hydra_parsers_sep_by (lambda (p) (lambda (sep) (funcall (hydra_parsers_alt (funcall (hydra_parsers_sep_by1 p) sep)) (hydra_parsers_pure (list))))))

(defvar hydra_parsers_string (lambda (str) (lambda (input) (let ((str_codes (hydra_lib_strings_to_list str))) (let ((input_codes (hydra_lib_strings_to_list input))) (let ((str_len (hydra_lib_lists_length str_codes))) (let ((input_prefix (funcall (hydra_lib_lists_take str_len) input_codes))) (if (funcall (hydra_lib_equality_equal str_codes) input_prefix) (list :success (make-hydra_parsing_parse_success str (hydra_lib_strings_from_list (funcall (hydra_lib_lists_drop str_len) input_codes)))) (list :failure (make-hydra_parsing_parse_error (funcall (hydra_lib_strings_cat2 "expected: ") str) input))))))))))

(provide 'hydra.parsers)
