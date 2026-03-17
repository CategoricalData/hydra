(defpackage :hydra.encode.parsing
(:use :cl :hydra.core :hydra.parsing)
(:export :hydra_encode_parsing_parse_error :hydra_encode_parsing_parse_success :hydra_encode_parsing_parse_result))

(in-package :hydra.encode.parsing)

(cl:defvar hydra_encode_parsing_parse_error (cl:lambda (x) (list :record (make-hydra_core_record "hydra.parsing.ParseError" (cl:list (make-hydra_core_field "message" ((cl:lambda (x) (list :literal (list :string x))) ((cl:lambda (v) (hydra_parsing_parse_error-message v)) x))) (make-hydra_core_field "remainder" ((cl:lambda (x) (list :literal (list :string x))) ((cl:lambda (v) (hydra_parsing_parse_error-remainder v)) x))))))))

(cl:defvar hydra_encode_parsing_parse_success (cl:lambda (a) (cl:lambda (x) (list :record (make-hydra_core_record "hydra.parsing.ParseSuccess" (cl:list (make-hydra_core_field "value" (a ((cl:lambda (v) (hydra_parsing_parse_success-value v)) x))) (make-hydra_core_field "remainder" ((cl:lambda (x) (list :literal (list :string x))) ((cl:lambda (v) (hydra_parsing_parse_success-remainder v)) x)))))))))

(cl:defvar hydra_encode_parsing_parse_result (cl:lambda (a) (cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :success) ((cl:lambda (y) (list :union (make-hydra_core_injection "hydra.parsing.ParseResult" (make-hydra_core_field "success" ((hydra_encode_parsing_parse_success a) y))))) match_value)) ((equal (car match_target) :failure) ((cl:lambda (y) (list :union (make-hydra_core_injection "hydra.parsing.ParseResult" (make-hydra_core_field "failure" (hydra_encode_parsing_parse_error y))))) match_value)))) (cadr match_target)))))
