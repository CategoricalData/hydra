(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.decode.accessors)

(require 'hydra.decode.core)

(require 'hydra.decode.variants)

(require 'hydra.error.core)

(require 'hydra.errors)

(require 'hydra.extract.helpers)

(require 'hydra.lexical)

(require 'hydra.lib.eithers)

(require 'hydra.lib.maps)

(require 'hydra.lib.maybes)

(require 'hydra.lib.strings)

(defvar hydra_decode_error_core_duplicate_binding_error (lambda (cx) (lambda (raw) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :record) (funcall (lambda (record) (let ((field_map (hydra_extract_helpers_to_field_map record))) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_helpers_require_field "location") hydra_decode_accessors_accessor_path) field_map) cx)) (lambda (field_location) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_helpers_require_field "name") hydra_decode_core_name) field_map) cx)) (lambda (field_name) (list :right (make-hydra_error_core_duplicate_binding_error field_location field_name)))))))) match_value)) (t (list :left "expected record")))) (cadr match_target))) stripped))) (funcall (hydra_lexical_strip_and_dereference_term_either cx) raw)))))

(defvar hydra_decode_error_core_duplicate_field_error (lambda (cx) (lambda (raw) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :record) (funcall (lambda (record) (let ((field_map (hydra_extract_helpers_to_field_map record))) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_helpers_require_field "location") hydra_decode_accessors_accessor_path) field_map) cx)) (lambda (field_location) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_helpers_require_field "name") hydra_decode_core_name) field_map) cx)) (lambda (field_name) (list :right (make-hydra_error_core_duplicate_field_error field_location field_name)))))))) match_value)) (t (list :left "expected record")))) (cadr match_target))) stripped))) (funcall (hydra_lexical_strip_and_dereference_term_either cx) raw)))))

(defvar hydra_decode_error_core_invalid_term_error (lambda (cx) (lambda (raw) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :union) (funcall (lambda (inj) (let* ((field (funcall (lambda (v) (hydra_core_injection-field v)) inj)) (fname (funcall (lambda (v) (hydra_core_field-name v)) field)) (fterm (funcall (lambda (v) (hydra_core_field-term v)) field)) (variant_map (hydra_lib_maps_from_list (list (list "duplicateBinding" (lambda (input) (funcall (hydra_lib_eithers_map (lambda (t_) (list :duplicate_binding t_))) (funcall (hydra_decode_error_core_duplicate_binding_error cx) input)))) (list "duplicateField" (lambda (input) (funcall (hydra_lib_eithers_map (lambda (t_) (list :duplicate_field t_))) (funcall (hydra_decode_error_core_duplicate_field_error cx) input)))))))) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :left (hydra_lib_strings_cat (list "no such field " (funcall (lambda (v) v) fname) " in union"))))) (lambda (f) (f fterm))) (funcall (hydra_lib_maps_lookup fname) variant_map)))) match_value)) (t (list :left "expected union")))) (cadr match_target))) stripped))) (funcall (hydra_lexical_strip_and_dereference_term_either cx) raw)))))

(defvar hydra_decode_error_core_undefined_field_error (lambda (cx) (lambda (raw) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :record) (funcall (lambda (record) (let ((field_map (hydra_extract_helpers_to_field_map record))) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_helpers_require_field "fieldName") hydra_decode_core_name) field_map) cx)) (lambda (field_field_name) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_helpers_require_field "typeName") hydra_decode_core_name) field_map) cx)) (lambda (field_type_name) (list :right (make-hydra_error_core_undefined_field_error field_field_name field_type_name)))))))) match_value)) (t (list :left "expected record")))) (cadr match_target))) stripped))) (funcall (hydra_lexical_strip_and_dereference_term_either cx) raw)))))

(defvar hydra_decode_error_core_undefined_term_error (lambda (cx) (lambda (raw) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :record) (funcall (lambda (record) (let ((field_map (hydra_extract_helpers_to_field_map record))) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_helpers_require_field "name") hydra_decode_core_name) field_map) cx)) (lambda (field_name) (list :right (make-hydra_error_core_undefined_term_error field_name)))))) match_value)) (t (list :left "expected record")))) (cadr match_target))) stripped))) (funcall (hydra_lexical_strip_and_dereference_term_either cx) raw)))))

(defvar hydra_decode_error_core_undefined_type_error (lambda (cx) (lambda (raw) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :record) (funcall (lambda (record) (let ((field_map (hydra_extract_helpers_to_field_map record))) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_helpers_require_field "name") hydra_decode_core_name) field_map) cx)) (lambda (field_name) (list :right (make-hydra_error_core_undefined_type_error field_name)))))) match_value)) (t (list :left "expected record")))) (cadr match_target))) stripped))) (funcall (hydra_lexical_strip_and_dereference_term_either cx) raw)))))

(defvar hydra_decode_error_core_unexpected_term_variant_error (lambda (cx) (lambda (raw) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :record) (funcall (lambda (record) (let ((field_map (hydra_extract_helpers_to_field_map record))) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_helpers_require_field "expectedVariant") hydra_decode_variants_term_variant) field_map) cx)) (lambda (field_expected_variant) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_helpers_require_field "actualTerm") hydra_decode_core_term) field_map) cx)) (lambda (field_actual_term) (list :right (make-hydra_error_core_unexpected_term_variant_error field_expected_variant field_actual_term)))))))) match_value)) (t (list :left "expected record")))) (cadr match_target))) stripped))) (funcall (hydra_lexical_strip_and_dereference_term_either cx) raw)))))

(defvar hydra_decode_error_core_unexpected_type_variant_error (lambda (cx) (lambda (raw) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :record) (funcall (lambda (record) (let ((field_map (hydra_extract_helpers_to_field_map record))) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_helpers_require_field "expectedVariant") hydra_decode_variants_type_variant) field_map) cx)) (lambda (field_expected_variant) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_helpers_require_field "actualType") hydra_decode_core_type) field_map) cx)) (lambda (field_actual_type) (list :right (make-hydra_error_core_unexpected_type_variant_error field_expected_variant field_actual_type)))))))) match_value)) (t (list :left "expected record")))) (cadr match_target))) stripped))) (funcall (hydra_lexical_strip_and_dereference_term_either cx) raw)))))

(provide 'hydra.decode.error.core)
