(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.decode.core)

(require 'hydra.errors)

(require 'hydra.extract.core)

(require 'hydra.lib.eithers)

(require 'hydra.phantoms)

(defvar hydra_decode_phantoms_t_term (lambda (a) (lambda (cx) (lambda (raw) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :wrap) (funcall (lambda (wrapped_term) (funcall (hydra_lib_eithers_map (lambda (b) b)) (funcall (hydra_decode_core_term cx) (funcall (lambda (v) (hydra_core_wrapped_term-body v)) wrapped_term)))) match_value)) (t (list :left "expected wrapped type")))) (cadr match_target))) stripped))) (funcall (hydra_extract_core_strip_with_decoding_error cx) raw))))))

(defvar hydra_decode_phantoms_t_binding (lambda (a) (lambda (cx) (lambda (raw) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :record) (funcall (lambda (record) (let ((field_map (hydra_extract_core_to_field_map record))) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_core_require_field "name") hydra_decode_core_name) field_map) cx)) (lambda (field_name) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_core_require_field "term") (hydra_decode_phantoms_t_term a)) field_map) cx)) (lambda (field_term) (list :right (make-hydra_phantoms_t_binding field_name field_term)))))))) match_value)) (t (list :left "expected record")))) (cadr match_target))) stripped))) (funcall (hydra_extract_core_strip_with_decoding_error cx) raw))))))

(defvar hydra_decode_phantoms_t_term_definition (lambda (a) (lambda (cx) (lambda (raw) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :record) (funcall (lambda (record) (let ((field_map (hydra_extract_core_to_field_map record))) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_core_require_field "name") hydra_decode_core_name) field_map) cx)) (lambda (field_name) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_core_require_field "term") (hydra_decode_phantoms_t_term a)) field_map) cx)) (lambda (field_term) (list :right (make-hydra_phantoms_t_term_definition field_name field_term)))))))) match_value)) (t (list :left "expected record")))) (cadr match_target))) stripped))) (funcall (hydra_extract_core_strip_with_decoding_error cx) raw))))))

(provide 'hydra.decode.phantoms)
