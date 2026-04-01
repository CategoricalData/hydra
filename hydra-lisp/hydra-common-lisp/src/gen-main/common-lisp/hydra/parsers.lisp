(defpackage :hydra.parsers
(:use :cl :hydra.lib.equality :hydra.lib.lists :hydra.lib.logic :hydra.lib.maybes :hydra.lib.strings :hydra.parsing)
(:export :hydra_parsers_alt :hydra_parsers_satisfy :hydra_parsers_any_char :hydra_parsers_apply :hydra_parsers_bind :hydra_parsers_pure :hydra_parsers_between :hydra_parsers_char :hydra_parsers_fail :hydra_parsers_choice :hydra_parsers_eof :hydra_parsers_lazy :hydra_parsers_many :hydra_parsers_some :hydra_parsers_map :hydra_parsers_optional :hydra_parsers_run_parser :hydra_parsers_sep_by1 :hydra_parsers_sep_by :hydra_parsers_string))

(in-package :hydra.parsers)

(cl:defvar hydra_parsers_alt (cl:lambda (p1) (cl:lambda (p2) (let ((parse (cl:lambda (input) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :success) ((cl:lambda (s) (list :success s)) match_value)) ((equal (car match_target) :failure) ((cl:lambda (e) (if ((hydra_lib_equality_equal ((cl:lambda (v) (hydra_parsing_parse_error-remainder v)) e)) input) (((cl:lambda (v) v) p2) input) (list :failure e))) match_value)))) (cadr match_target))) (((cl:lambda (v) v) p1) input))))) parse))))

(cl:defvar hydra_parsers_satisfy (cl:lambda (pred) (let ((parse (cl:lambda (input) (let ((codes (hydra_lib_strings_to_list input))) (((hydra_lib_maybes_maybe (cl:lambda () (list :failure (make-hydra_parsing_parse_error "unexpected end of input" input)))) (cl:lambda (c) (let ((rest (hydra_lib_strings_from_list ((hydra_lib_lists_drop 1) codes)))) (if (pred c) (list :success (make-hydra_parsing_parse_success c rest)) (list :failure (make-hydra_parsing_parse_error "character did not satisfy predicate" input)))))) (hydra_lib_lists_safe_head codes)))))) parse)))

(cl:defvar hydra_parsers_any_char (hydra_parsers_satisfy (cl:lambda (_) cl:t)))

(cl:defvar hydra_parsers_apply (cl:lambda (pf) (cl:lambda (pa) (let ((parse (cl:lambda (input) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :success) ((cl:lambda (sf) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :success) ((cl:lambda (sa) (list :success (make-hydra_parsing_parse_success (((cl:lambda (v) (hydra_parsing_parse_success-value v)) sf) ((cl:lambda (v) (hydra_parsing_parse_success-value v)) sa)) ((cl:lambda (v) (hydra_parsing_parse_success-remainder v)) sa)))) match_value)) ((equal (car match_target) :failure) ((cl:lambda (e) (list :failure e)) match_value)))) (cadr match_target))) (((cl:lambda (v) v) pa) ((cl:lambda (v) (hydra_parsing_parse_success-remainder v)) sf)))) match_value)) ((equal (car match_target) :failure) ((cl:lambda (e) (list :failure e)) match_value)))) (cadr match_target))) (((cl:lambda (v) v) pf) input))))) parse))))

(cl:defvar hydra_parsers_bind (cl:lambda (pa) (cl:lambda (f) (let ((parse (cl:lambda (input) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :success) ((cl:lambda (s) (((cl:lambda (v) v) (f ((cl:lambda (v) (hydra_parsing_parse_success-value v)) s))) ((cl:lambda (v) (hydra_parsing_parse_success-remainder v)) s))) match_value)) ((equal (car match_target) :failure) ((cl:lambda (e) (list :failure e)) match_value)))) (cadr match_target))) (((cl:lambda (v) v) pa) input))))) parse))))

(cl:defvar hydra_parsers_pure (cl:lambda (a) (cl:lambda (input) (list :success (make-hydra_parsing_parse_success a input)))))

(cl:defvar hydra_parsers_between (cl:lambda (open) (cl:lambda (close) (cl:lambda (p) ((hydra_parsers_bind open) (cl:lambda (_) ((hydra_parsers_bind p) (cl:lambda (x) ((hydra_parsers_bind close) (cl:lambda (_2) (hydra_parsers_pure x)))))))))))

(cl:defvar hydra_parsers_char (cl:lambda (c) (hydra_parsers_satisfy (cl:lambda (x) ((hydra_lib_equality_equal x) c)))))

(cl:defvar hydra_parsers_fail (cl:lambda (msg) (cl:lambda (input) (list :failure (make-hydra_parsing_parse_error msg input)))))

(cl:defvar hydra_parsers_choice (cl:lambda (ps) (((hydra_lib_lists_foldl hydra_parsers_alt) (hydra_parsers_fail "no choice matched")) ps)))

(cl:defvar hydra_parsers_eof (cl:lambda (input) (if ((hydra_lib_equality_equal input) "") (list :success (make-hydra_parsing_parse_success cl:nil "")) (list :failure (make-hydra_parsing_parse_error "expected end of input" input)))))

(cl:defvar hydra_parsers_lazy (cl:lambda (f) (cl:lambda (input) (((cl:lambda (v) v) (f cl:nil)) input))))

(cl:defvar hydra_parsers_many (cl:lambda (p) ((hydra_parsers_alt (hydra_parsers_some p)) (hydra_parsers_pure (cl:list)))))

(cl:defvar hydra_parsers_some (cl:lambda (p) ((hydra_parsers_bind p) (cl:lambda (x) ((hydra_parsers_bind (hydra_parsers_many p)) (cl:lambda (xs) (hydra_parsers_pure ((hydra_lib_lists_cons x) xs))))))))

(cl:defvar hydra_parsers_map (cl:lambda (f) (cl:lambda (pa) (let ((parse (cl:lambda (input) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :success) ((cl:lambda (s) (list :success (make-hydra_parsing_parse_success (f ((cl:lambda (v) (hydra_parsing_parse_success-value v)) s)) ((cl:lambda (v) (hydra_parsing_parse_success-remainder v)) s)))) match_value)) ((equal (car match_target) :failure) ((cl:lambda (e) (list :failure e)) match_value)))) (cadr match_target))) (((cl:lambda (v) v) pa) input))))) parse))))

(cl:defvar hydra_parsers_optional (cl:lambda (p) ((hydra_parsers_alt ((hydra_parsers_map hydra_lib_maybes_pure) p)) (hydra_parsers_pure (list :nothing)))))

(cl:defvar hydra_parsers_run_parser (cl:lambda (p) (cl:lambda (input) (((cl:lambda (v) v) p) input))))

(cl:defvar hydra_parsers_sep_by1 (cl:lambda (p) (cl:lambda (sep) ((hydra_parsers_bind p) (cl:lambda (x) ((hydra_parsers_bind (hydra_parsers_many ((hydra_parsers_bind sep) (cl:lambda (_) p)))) (cl:lambda (xs) (hydra_parsers_pure ((hydra_lib_lists_cons x) xs)))))))))

(cl:defvar hydra_parsers_sep_by (cl:lambda (p) (cl:lambda (sep) ((hydra_parsers_alt ((hydra_parsers_sep_by1 p) sep)) (hydra_parsers_pure (cl:list))))))

(cl:defvar hydra_parsers_string (cl:lambda (str) (cl:lambda (input) (let ((str_codes (hydra_lib_strings_to_list str))) (let ((input_codes (hydra_lib_strings_to_list input))) (let ((str_len (hydra_lib_lists_length str_codes))) (let ((input_prefix ((hydra_lib_lists_take str_len) input_codes))) (if ((hydra_lib_equality_equal str_codes) input_prefix) (list :success (make-hydra_parsing_parse_success str (hydra_lib_strings_from_list ((hydra_lib_lists_drop str_len) input_codes)))) (list :failure (make-hydra_parsing_parse_error ((hydra_lib_strings_cat2 "expected: ") str) input))))))))))
