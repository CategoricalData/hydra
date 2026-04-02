(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.errors)

(require 'hydra.extract.core)

(require 'hydra.lexical)

(require 'hydra.lib.eithers)

(require 'hydra.lib.maps)

(require 'hydra.lib.maybes)

(require 'hydra.lib.strings)

(require 'hydra.parsing)

(defvar hydra_decode_parsing_parse_error (lambda (cx) (lambda (raw) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :record) (funcall (lambda (record) (let ((field_map (hydra_extract_core_to_field_map record))) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_core_require_field "message") (lambda (cx2) (lambda (raw2) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped2) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :literal) (funcall (lambda (v) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :string) (funcall (lambda (s) (list :right s)) match_value)) (t (list :left "expected string literal")))) (cadr match_target))) v)) match_value)) (t (list :left "expected literal")))) (cadr match_target))) stripped2))) (funcall (hydra_lexical_strip_and_dereference_term_either cx2) raw2))))) field_map) cx)) (lambda (field_message) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_core_require_field "remainder") (lambda (cx2) (lambda (raw2) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped2) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :literal) (funcall (lambda (v) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :string) (funcall (lambda (s) (list :right s)) match_value)) (t (list :left "expected string literal")))) (cadr match_target))) v)) match_value)) (t (list :left "expected literal")))) (cadr match_target))) stripped2))) (funcall (hydra_lexical_strip_and_dereference_term_either cx2) raw2))))) field_map) cx)) (lambda (field_remainder) (list :right (make-hydra_parsing_parse_error field_message field_remainder)))))))) match_value)) (t (list :left "expected record")))) (cadr match_target))) stripped))) (funcall (hydra_lexical_strip_and_dereference_term_either cx) raw)))))

(defvar hydra_decode_parsing_parse_success (lambda (a) (lambda (cx) (lambda (raw) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :record) (funcall (lambda (record) (let ((field_map (hydra_extract_core_to_field_map record))) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_core_require_field "value") a) field_map) cx)) (lambda (field_value) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_core_require_field "remainder") (lambda (cx2) (lambda (raw2) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped2) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :literal) (funcall (lambda (v) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :string) (funcall (lambda (s) (list :right s)) match_value)) (t (list :left "expected string literal")))) (cadr match_target))) v)) match_value)) (t (list :left "expected literal")))) (cadr match_target))) stripped2))) (funcall (hydra_lexical_strip_and_dereference_term_either cx2) raw2))))) field_map) cx)) (lambda (field_remainder) (list :right (make-hydra_parsing_parse_success field_value field_remainder)))))))) match_value)) (t (list :left "expected record")))) (cadr match_target))) stripped))) (funcall (hydra_lexical_strip_and_dereference_term_either cx) raw))))))

(defvar hydra_decode_parsing_parse_result (lambda (a) (lambda (cx) (lambda (raw) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :union) (funcall (lambda (inj) (let* ((field (funcall (lambda (v) (hydra_core_injection-field v)) inj)) (fname (funcall (lambda (v) (hydra_core_field-name v)) field)) (fterm (funcall (lambda (v) (hydra_core_field-term v)) field)) (variant_map (hydra_lib_maps_from_list (list (list "success" (lambda (input) (funcall (hydra_lib_eithers_map (lambda (t_) (list :success t_))) (funcall (funcall (hydra_decode_parsing_parse_success a) cx) input)))) (list "failure" (lambda (input) (funcall (hydra_lib_eithers_map (lambda (t_) (list :failure t_))) (funcall (hydra_decode_parsing_parse_error cx) input)))))))) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :left (hydra_lib_strings_cat (list "no such field " (funcall (lambda (v) v) fname) " in union"))))) (lambda (f) (f fterm))) (funcall (hydra_lib_maps_lookup fname) variant_map)))) match_value)) (t (list :left "expected union")))) (cadr match_target))) stripped))) (funcall (hydra_lexical_strip_and_dereference_term_either cx) raw))))))

(provide 'hydra.decode.parsing)
