(require 'cl-lib)

(require 'hydra.context)

(require 'hydra.core)

(require 'hydra.decode.core)

(require 'hydra.errors)

(require 'hydra.extract.core)

(require 'hydra.lexical)

(require 'hydra.lib.eithers)

(defvar hydra_decode_context_context (lambda (cx) (lambda (raw) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :record) (funcall (lambda (record) (let ((field_map (hydra_extract_core_to_field_map record))) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_core_require_field "trace") (hydra_extract_core_decode_list (lambda (cx) (lambda (raw) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :literal) (funcall (lambda (v) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :string) (funcall (lambda (s) (list :right s)) match_value)) (t (list :left "expected string literal")))) (cadr match_target))) v)) match_value)) (t (list :left "expected literal")))) (cadr match_target))) stripped))) (funcall (hydra_lexical_strip_and_dereference_term_either cx) raw)))))) field_map) cx)) (lambda (field_trace) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_core_require_field "messages") (hydra_extract_core_decode_list (lambda (cx) (lambda (raw) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :literal) (funcall (lambda (v) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :string) (funcall (lambda (s) (list :right s)) match_value)) (t (list :left "expected string literal")))) (cadr match_target))) v)) match_value)) (t (list :left "expected literal")))) (cadr match_target))) stripped))) (funcall (hydra_lexical_strip_and_dereference_term_either cx) raw)))))) field_map) cx)) (lambda (field_messages) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_core_require_field "other") (funcall (hydra_extract_core_decode_map hydra_decode_core_name) hydra_decode_core_term)) field_map) cx)) (lambda (field_other) (list :right (make-hydra_context_context field_trace field_messages field_other)))))))))) match_value)) (t (list :left "expected record")))) (cadr match_target))) stripped))) (funcall (hydra_lexical_strip_and_dereference_term_either cx) raw)))))

(defvar hydra_decode_context_in_context (lambda (e) (lambda (cx) (lambda (raw) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :record) (funcall (lambda (record) (let ((field_map (hydra_extract_core_to_field_map record))) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_core_require_field "object") e) field_map) cx)) (lambda (field_object) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_core_require_field "context") hydra_decode_context_context) field_map) cx)) (lambda (field_context) (list :right (make-hydra_context_in_context field_object field_context)))))))) match_value)) (t (list :left "expected record")))) (cadr match_target))) stripped))) (funcall (hydra_lexical_strip_and_dereference_term_either cx) raw))))))

(provide 'hydra.decode.context)
