(require 'cl-lib)

(require 'hydra.lib.equality)

(require 'hydra.lib.lists)

(require 'hydra.lib.logic)

(require 'hydra.lib.maybes)

(require 'hydra.lib.strings)

(require 'hydra.parsing)

(defvar hydra_parsers_alt (lambda (p1) (lambda (p2) (let ((parse (lambda (input) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :success) ((lambda (s) (list :success s)) match_value)) ((equal (car match_target) :failure) ((lambda (e) (if ((hydra_lib_equality_equal ((lambda (v) (hydra_parsing_parse_error-remainder v)) e)) input) (((lambda (v) v) p2) input) (list :failure e))) match_value)))) (cadr match_target))) (((lambda (v) v) p1) input))))) parse))))

(defvar hydra_parsers_satisfy (lambda (pred) (let ((parse (lambda (input) (let ((codes (hydra_lib_strings_to_list input))) (((hydra_lib_maybes_maybe (list :failure (make-hydra_parsing_parse_error "unexpected end of input" input))) (lambda (c) (let ((rest (hydra_lib_strings_from_list ((hydra_lib_lists_drop 1) codes)))) (if (pred c) (list :success (make-hydra_parsing_parse_success c rest)) (list :failure (make-hydra_parsing_parse_error "character did not satisfy predicate" input)))))) (hydra_lib_lists_safe_head codes)))))) parse)))

(defvar hydra_parsers_any_char (hydra_parsers_satisfy (lambda (_) t)))

(defvar hydra_parsers_apply (lambda (pf) (lambda (pa) (let ((parse (lambda (input) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :success) ((lambda (sf) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :success) ((lambda (sa) (list :success (make-hydra_parsing_parse_success (((lambda (v) (hydra_parsing_parse_success-value v)) sf) ((lambda (v) (hydra_parsing_parse_success-value v)) sa)) ((lambda (v) (hydra_parsing_parse_success-remainder v)) sa)))) match_value)) ((equal (car match_target) :failure) ((lambda (e) (list :failure e)) match_value)))) (cadr match_target))) (((lambda (v) v) pa) ((lambda (v) (hydra_parsing_parse_success-remainder v)) sf)))) match_value)) ((equal (car match_target) :failure) ((lambda (e) (list :failure e)) match_value)))) (cadr match_target))) (((lambda (v) v) pf) input))))) parse))))

(defvar hydra_parsers_bind (lambda (pa) (lambda (f) (let ((parse (lambda (input) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :success) ((lambda (s) (((lambda (v) v) (f ((lambda (v) (hydra_parsing_parse_success-value v)) s))) ((lambda (v) (hydra_parsing_parse_success-remainder v)) s))) match_value)) ((equal (car match_target) :failure) ((lambda (e) (list :failure e)) match_value)))) (cadr match_target))) (((lambda (v) v) pa) input))))) parse))))

(defvar hydra_parsers_pure (lambda (a) (lambda (input) (list :success (make-hydra_parsing_parse_success a input)))))

(defvar hydra_parsers_between (lambda (open) (lambda (close) (lambda (p) ((hydra_parsers_bind open) (lambda (_) ((hydra_parsers_bind p) (lambda (x) ((hydra_parsers_bind close) (lambda (_) (hydra_parsers_pure x)))))))))))

(defvar hydra_parsers_char (lambda (c) (hydra_parsers_satisfy (lambda (x) ((hydra_lib_equality_equal x) c)))))

(defvar hydra_parsers_fail (lambda (msg) (lambda (input) (list :failure (make-hydra_parsing_parse_error msg input)))))

(defvar hydra_parsers_choice (lambda (ps) (((hydra_lib_lists_foldl hydra_parsers_alt) (hydra_parsers_fail "no choice matched")) ps)))

(defvar hydra_parsers_eof (lambda (input) (if ((hydra_lib_equality_equal input) "") (list :success (make-hydra_parsing_parse_success nil "")) (list :failure (make-hydra_parsing_parse_error "expected end of input" input)))))

(defvar hydra_parsers_lazy (lambda (f) (lambda (input) (((lambda (v) v) (f nil)) input))))

(defvar hydra_parsers_many (lambda (p) ((hydra_parsers_alt (hydra_parsers_some p)) (hydra_parsers_pure (list)))))

(defvar hydra_parsers_some (lambda (p) ((hydra_parsers_bind p) (lambda (x) ((hydra_parsers_bind (hydra_parsers_many p)) (lambda (xs) (hydra_parsers_pure ((hydra_lib_lists_cons x) xs))))))))

(defvar hydra_parsers_map (lambda (f) (lambda (pa) (let ((parse (lambda (input) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :success) ((lambda (s) (list :success (make-hydra_parsing_parse_success (f ((lambda (v) (hydra_parsing_parse_success-value v)) s)) ((lambda (v) (hydra_parsing_parse_success-remainder v)) s)))) match_value)) ((equal (car match_target) :failure) ((lambda (e) (list :failure e)) match_value)))) (cadr match_target))) (((lambda (v) v) pa) input))))) parse))))

(defvar hydra_parsers_optional (lambda (p) ((hydra_parsers_alt ((hydra_parsers_map hydra_lib_maybes_pure) p)) (hydra_parsers_pure (list :nothing)))))

(defvar hydra_parsers_run_parser (lambda (p) (lambda (input) (((lambda (v) v) p) input))))

(defvar hydra_parsers_sep_by1 (lambda (p) (lambda (sep) ((hydra_parsers_bind p) (lambda (x) ((hydra_parsers_bind (hydra_parsers_many ((hydra_parsers_bind sep) (lambda (_) p)))) (lambda (xs) (hydra_parsers_pure ((hydra_lib_lists_cons x) xs)))))))))

(defvar hydra_parsers_sep_by (lambda (p) (lambda (sep) ((hydra_parsers_alt ((hydra_parsers_sep_by1 p) sep)) (hydra_parsers_pure (list))))))

(defvar hydra_parsers_string (lambda (str) (lambda (input) (let ((str_codes (hydra_lib_strings_to_list str))) (let ((input_codes (hydra_lib_strings_to_list input))) (let ((str_len (hydra_lib_lists_length str_codes))) (let ((input_prefix ((hydra_lib_lists_take str_len) input_codes))) (if ((hydra_lib_equality_equal str_codes) input_prefix) (list :success (make-hydra_parsing_parse_success str (hydra_lib_strings_from_list ((hydra_lib_lists_drop str_len) input_codes)))) (list :failure (make-hydra_parsing_parse_error ((hydra_lib_strings_cat2 "expected: ") str) input))))))))))

(provide 'hydra.parsers)
