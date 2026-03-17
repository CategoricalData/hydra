(require 'cl-lib)

(require 'hydra.classes)

(require 'hydra.core)

(require 'hydra.error)

(require 'hydra.extract.helpers)

(require 'hydra.lexical)

(require 'hydra.lib.eithers)

(require 'hydra.lib.maps)

(require 'hydra.lib.maybes)

(require 'hydra.lib.strings)

(defvar hydra_decode_classes_type_class (lambda (cx) (lambda (raw) (((hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :union) ((lambda (inj) (let* ((field ((lambda (v) (hydra_core_injection-field v)) inj)) (fname ((lambda (v) (hydra_core_field-name v)) field)) (fterm ((lambda (v) (hydra_core_field-term v)) field)) (variant_map (hydra_lib_maps_from_list (list (list "equality" (lambda (input) ((hydra_lib_eithers_map (lambda (t_) (list :equality t_))) ((hydra_extract_helpers_decode_unit cx) input)))) (list "ordering" (lambda (input) ((hydra_lib_eithers_map (lambda (t_) (list :ordering t_))) ((hydra_extract_helpers_decode_unit cx) input)))))))) (((hydra_lib_maybes_maybe (list :left (hydra_lib_strings_cat (list "no such field " ((lambda (v) v) fname) " in union")))) (lambda (f) (f fterm))) ((hydra_lib_maps_lookup fname) variant_map)))) match_value)) (t (list :left "expected union")))) (cadr match_target))) stripped))) ((hydra_lexical_strip_and_dereference_term_either cx) raw)))))

(provide 'hydra.decode.classes)
