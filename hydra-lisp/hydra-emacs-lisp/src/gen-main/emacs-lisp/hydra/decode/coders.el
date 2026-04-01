(require 'cl-lib)

(require 'hydra.coders)

(require 'hydra.core)

(require 'hydra.errors)

(require 'hydra.extract.helpers)

(require 'hydra.lexical)

(require 'hydra.lib.eithers)

(require 'hydra.lib.maps)

(require 'hydra.lib.maybes)

(require 'hydra.lib.strings)

(defvar hydra_decode_coders_coder_direction (lambda (cx) (lambda (raw) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :union) (funcall (lambda (inj) (let* ((field (funcall (lambda (v) (hydra_core_injection-field v)) inj)) (fname (funcall (lambda (v) (hydra_core_field-name v)) field)) (fterm (funcall (lambda (v) (hydra_core_field-term v)) field)) (variant_map (hydra_lib_maps_from_list (list (list "encode" (lambda (input) (funcall (hydra_lib_eithers_map (lambda (t_) (list :encode t_))) (funcall (hydra_extract_helpers_decode_unit cx) input)))) (list "decode" (lambda (input) (funcall (hydra_lib_eithers_map (lambda (t_) (list :decode t_))) (funcall (hydra_extract_helpers_decode_unit cx) input)))))))) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :left (hydra_lib_strings_cat (list "no such field " (funcall (lambda (v) v) fname) " in union"))))) (lambda (f) (f fterm))) (funcall (hydra_lib_maps_lookup fname) variant_map)))) match_value)) (t (list :left "expected union")))) (cadr match_target))) stripped))) (funcall (hydra_lexical_strip_and_dereference_term_either cx) raw)))))

(defvar hydra_decode_coders_language_name (lambda (cx) (lambda (raw) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :wrap) (funcall (lambda (wrapped_term) (funcall (hydra_lib_eithers_map (lambda (b) b)) (funcall (funcall (lambda (cx2) (lambda (raw2) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped2) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :literal) (funcall (lambda (v) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :string) (funcall (lambda (s) (list :right s)) match_value)) (t (list :left "expected string literal")))) (cadr match_target))) v)) match_value)) (t (list :left "expected literal")))) (cadr match_target))) stripped2))) (funcall (hydra_lexical_strip_and_dereference_term_either cx2) raw2)))) cx) (funcall (lambda (v) (hydra_core_wrapped_term-body v)) wrapped_term)))) match_value)) (t (list :left "expected wrapped type")))) (cadr match_target))) stripped))) (funcall (hydra_lexical_strip_and_dereference_term_either cx) raw)))))

(defvar hydra_decode_coders_traversal_order (lambda (cx) (lambda (raw) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :union) (funcall (lambda (inj) (let* ((field (funcall (lambda (v) (hydra_core_injection-field v)) inj)) (fname (funcall (lambda (v) (hydra_core_field-name v)) field)) (fterm (funcall (lambda (v) (hydra_core_field-term v)) field)) (variant_map (hydra_lib_maps_from_list (list (list "pre" (lambda (input) (funcall (hydra_lib_eithers_map (lambda (t_) (list :pre t_))) (funcall (hydra_extract_helpers_decode_unit cx) input)))) (list "post" (lambda (input) (funcall (hydra_lib_eithers_map (lambda (t_) (list :post t_))) (funcall (hydra_extract_helpers_decode_unit cx) input)))))))) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :left (hydra_lib_strings_cat (list "no such field " (funcall (lambda (v) v) fname) " in union"))))) (lambda (f) (f fterm))) (funcall (hydra_lib_maps_lookup fname) variant_map)))) match_value)) (t (list :left "expected union")))) (cadr match_target))) stripped))) (funcall (hydra_lexical_strip_and_dereference_term_either cx) raw)))))

(provide 'hydra.decode.coders)
