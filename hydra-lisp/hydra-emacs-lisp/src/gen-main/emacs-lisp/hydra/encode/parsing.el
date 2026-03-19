(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.parsing)

(defvar hydra_encode_parsing_parse_error (lambda (x) (list :record (make-hydra_core_record "hydra.parsing.ParseError" (list (make-hydra_core_field "message" (funcall (lambda (x) (list :literal (list :string x))) (funcall (lambda (v) (hydra_parsing_parse_error-message v)) x))) (make-hydra_core_field "remainder" (funcall (lambda (x) (list :literal (list :string x))) (funcall (lambda (v) (hydra_parsing_parse_error-remainder v)) x))))))))

(defvar hydra_encode_parsing_parse_success (lambda (a) (lambda (x) (list :record (make-hydra_core_record "hydra.parsing.ParseSuccess" (list (make-hydra_core_field "value" (a (funcall (lambda (v) (hydra_parsing_parse_success-value v)) x))) (make-hydra_core_field "remainder" (funcall (lambda (x) (list :literal (list :string x))) (funcall (lambda (v) (hydra_parsing_parse_success-remainder v)) x)))))))))

(defvar hydra_encode_parsing_parse_result (lambda (a) (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :success) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.parsing.ParseResult" (make-hydra_core_field "success" (funcall (hydra_encode_parsing_parse_success a) y))))) match_value)) ((equal (car match_target) :failure) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.parsing.ParseResult" (make-hydra_core_field "failure" (hydra_encode_parsing_parse_error y))))) match_value)))) (cadr match_target)))))

(provide 'hydra.encode.parsing)
