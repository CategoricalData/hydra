(require 'cl-lib)

(require 'hydra.context)

(require 'hydra.core)

(require 'hydra.decode.core)

(require 'hydra.error)

(require 'hydra.extract.helpers)

(require 'hydra.lexical)

(require 'hydra.lib.eithers)

(defvar hydra_decode_context_context (lambda (cx) (lambda (raw) (((hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :record) ((lambda (record) (let ((field_map (hydra_extract_helpers_to_field_map record))) ((hydra_lib_eithers_bind ((((hydra_extract_helpers_require_field "trace") (hydra_extract_helpers_decode_list (lambda (cx) (lambda (raw) (((hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :literal) ((lambda (v) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :string) ((lambda (s) (list :right s)) match_value)) (t (list :left "expected string literal")))) (cadr match_target))) v)) match_value)) (t (list :left "expected literal")))) (cadr match_target))) stripped))) ((hydra_lexical_strip_and_dereference_term_either cx) raw)))))) field_map) cx)) (lambda (field_trace) ((hydra_lib_eithers_bind ((((hydra_extract_helpers_require_field "messages") (hydra_extract_helpers_decode_list (lambda (cx) (lambda (raw) (((hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :literal) ((lambda (v) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :string) ((lambda (s) (list :right s)) match_value)) (t (list :left "expected string literal")))) (cadr match_target))) v)) match_value)) (t (list :left "expected literal")))) (cadr match_target))) stripped))) ((hydra_lexical_strip_and_dereference_term_either cx) raw)))))) field_map) cx)) (lambda (field_messages) ((hydra_lib_eithers_bind ((((hydra_extract_helpers_require_field "other") ((hydra_extract_helpers_decode_map hydra_decode_core_name) hydra_decode_core_term)) field_map) cx)) (lambda (field_other) (list :right (make-hydra_context_context field_trace field_messages field_other)))))))))) match_value)) (t (list :left "expected record")))) (cadr match_target))) stripped))) ((hydra_lexical_strip_and_dereference_term_either cx) raw)))))

(defvar hydra_decode_context_in_context (lambda (e) (lambda (cx) (lambda (raw) (((hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :record) ((lambda (record) (let ((field_map (hydra_extract_helpers_to_field_map record))) ((hydra_lib_eithers_bind ((((hydra_extract_helpers_require_field "object") e) field_map) cx)) (lambda (field_object) ((hydra_lib_eithers_bind ((((hydra_extract_helpers_require_field "context") hydra_decode_context_context) field_map) cx)) (lambda (field_context) (list :right (make-hydra_context_in_context field_object field_context)))))))) match_value)) (t (list :left "expected record")))) (cadr match_target))) stripped))) ((hydra_lexical_strip_and_dereference_term_either cx) raw))))))

(provide 'hydra.decode.context)
