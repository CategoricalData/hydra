(require 'cl-lib)

(require 'hydra.classes)

(require 'hydra.core)

(require 'hydra.errors)

(require 'hydra.extract.helpers)

(require 'hydra.lexical)

(require 'hydra.lib.eithers)

(require 'hydra.lib.maps)

(require 'hydra.lib.maybes)

(require 'hydra.lib.strings)

(defvar hydra_decode_classes_type_class (lambda (cx) (lambda (raw) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :union) (funcall (lambda (inj) (let* ((field (funcall (lambda (v) (hydra_core_injection-field v)) inj)) (fname (funcall (lambda (v) (hydra_core_field-name v)) field)) (fterm (funcall (lambda (v) (hydra_core_field-term v)) field)) (variant_map (hydra_lib_maps_from_list (list (list "equality" (lambda (input) (funcall (hydra_lib_eithers_map (lambda (t_) (list :equality t_))) (funcall (hydra_extract_helpers_decode_unit cx) input)))) (list "ordering" (lambda (input) (funcall (hydra_lib_eithers_map (lambda (t_) (list :ordering t_))) (funcall (hydra_extract_helpers_decode_unit cx) input)))))))) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :left (hydra_lib_strings_cat (list "no such field " (funcall (lambda (v) v) fname) " in union"))))) (lambda (f) (f fterm))) (funcall (hydra_lib_maps_lookup fname) variant_map)))) match_value)) (t (list :left "expected union")))) (cadr match_target))) stripped))) (funcall (hydra_lexical_strip_and_dereference_term_either cx) raw)))))

(provide 'hydra.decode.classes)
