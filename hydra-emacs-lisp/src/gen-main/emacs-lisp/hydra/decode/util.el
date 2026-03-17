(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.error)

(require 'hydra.extract.helpers)

(require 'hydra.lexical)

(require 'hydra.lib.eithers)

(require 'hydra.lib.maps)

(require 'hydra.lib.maybes)

(require 'hydra.lib.strings)

(require 'hydra.util)

(defvar hydra_decode_util_case_convention (lambda (cx) (lambda (raw) (((hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :union) ((lambda (inj) (let* ((field ((lambda (v) (hydra_core_injection-field v)) inj)) (fname ((lambda (v) (hydra_core_field-name v)) field)) (fterm ((lambda (v) (hydra_core_field-term v)) field)) (variant_map (hydra_lib_maps_from_list (list (list "camel" (lambda (input) ((hydra_lib_eithers_map (lambda (t_) (list :camel t_))) ((hydra_extract_helpers_decode_unit cx) input)))) (list "pascal" (lambda (input) ((hydra_lib_eithers_map (lambda (t_) (list :pascal t_))) ((hydra_extract_helpers_decode_unit cx) input)))) (list "lowerSnake" (lambda (input) ((hydra_lib_eithers_map (lambda (t_) (list :lower_snake t_))) ((hydra_extract_helpers_decode_unit cx) input)))) (list "upperSnake" (lambda (input) ((hydra_lib_eithers_map (lambda (t_) (list :upper_snake t_))) ((hydra_extract_helpers_decode_unit cx) input)))))))) (((hydra_lib_maybes_maybe (list :left (hydra_lib_strings_cat (list "no such field " ((lambda (v) v) fname) " in union")))) (lambda (f) (f fterm))) ((hydra_lib_maps_lookup fname) variant_map)))) match_value)) (t (list :left "expected union")))) (cadr match_target))) stripped))) ((hydra_lexical_strip_and_dereference_term_either cx) raw)))))

(defvar hydra_decode_util_comparison (lambda (cx) (lambda (raw) (((hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :union) ((lambda (inj) (let* ((field ((lambda (v) (hydra_core_injection-field v)) inj)) (fname ((lambda (v) (hydra_core_field-name v)) field)) (fterm ((lambda (v) (hydra_core_field-term v)) field)) (variant_map (hydra_lib_maps_from_list (list (list "lessThan" (lambda (input) ((hydra_lib_eithers_map (lambda (t_) (list :less_than t_))) ((hydra_extract_helpers_decode_unit cx) input)))) (list "equalTo" (lambda (input) ((hydra_lib_eithers_map (lambda (t_) (list :equal_to t_))) ((hydra_extract_helpers_decode_unit cx) input)))) (list "greaterThan" (lambda (input) ((hydra_lib_eithers_map (lambda (t_) (list :greater_than t_))) ((hydra_extract_helpers_decode_unit cx) input)))))))) (((hydra_lib_maybes_maybe (list :left (hydra_lib_strings_cat (list "no such field " ((lambda (v) v) fname) " in union")))) (lambda (f) (f fterm))) ((hydra_lib_maps_lookup fname) variant_map)))) match_value)) (t (list :left "expected union")))) (cadr match_target))) stripped))) ((hydra_lexical_strip_and_dereference_term_either cx) raw)))))

(defvar hydra_decode_util_precision (lambda (cx) (lambda (raw) (((hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :union) ((lambda (inj) (let* ((field ((lambda (v) (hydra_core_injection-field v)) inj)) (fname ((lambda (v) (hydra_core_field-name v)) field)) (fterm ((lambda (v) (hydra_core_field-term v)) field)) (variant_map (hydra_lib_maps_from_list (list (list "arbitrary" (lambda (input) ((hydra_lib_eithers_map (lambda (t_) (list :arbitrary t_))) ((hydra_extract_helpers_decode_unit cx) input)))) (list "bits" (lambda (input) ((hydra_lib_eithers_map (lambda (t_) (list :bits t_))) (((lambda (cx) (lambda (raw) (((hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :literal) ((lambda (v) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :integer) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :int32) ((lambda (i) (list :right i)) match_value)) (t (list :left "expected int32 value")))) (cadr match_target))) match_value)) (t (list :left "expected int32 literal")))) (cadr match_target))) v)) match_value)) (t (list :left "expected literal")))) (cadr match_target))) stripped))) ((hydra_lexical_strip_and_dereference_term_either cx) raw)))) cx) input)))))))) (((hydra_lib_maybes_maybe (list :left (hydra_lib_strings_cat (list "no such field " ((lambda (v) v) fname) " in union")))) (lambda (f) (f fterm))) ((hydra_lib_maps_lookup fname) variant_map)))) match_value)) (t (list :left "expected union")))) (cadr match_target))) stripped))) ((hydra_lexical_strip_and_dereference_term_either cx) raw)))))

(provide 'hydra.decode.util)
