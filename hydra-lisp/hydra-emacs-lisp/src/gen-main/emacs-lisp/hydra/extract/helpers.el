(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.error)

(require 'hydra.lexical)

(require 'hydra.lib.eithers)

(require 'hydra.lib.lists)

(require 'hydra.lib.maps)

(require 'hydra.lib.maybes)

(require 'hydra.lib.pairs)

(require 'hydra.lib.sets)

(require 'hydra.lib.strings)

(defvar hydra_extract_helpers_decode_either (lambda (left_decoder) (lambda (right_decoder) (lambda (g) (lambda (term) (funcall (hydra_lib_eithers_bind (funcall (funcall (hydra_lib_eithers_bimap (lambda (x) x)) (lambda (x) x)) (funcall (hydra_lexical_strip_and_dereference_term_either g) term))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :either) (funcall (lambda (e) (funcall (funcall (hydra_lib_eithers_either (lambda (lv) (funcall (hydra_lib_eithers_map (lambda (x) (list :left x))) (funcall (left_decoder g) lv)))) (lambda (rv) (funcall (hydra_lib_eithers_map (lambda (x) (list :right x))) (funcall (right_decoder g) rv)))) e)) match_value)) (t (list :left "expected either value")))) (cadr match_target))) stripped))))))))

(defvar hydra_extract_helpers_decode_list (lambda (elem_decoder) (lambda (g) (lambda (term) (funcall (hydra_lib_eithers_bind (funcall (funcall (hydra_lib_eithers_bimap (lambda (x) x)) (lambda (x) x)) (funcall (hydra_lexical_strip_and_dereference_term_either g) term))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :list) (funcall (lambda (els) (funcall (hydra_lib_eithers_map_list (elem_decoder g)) els)) match_value)) (t (list :left "expected list")))) (cadr match_target))) stripped)))))))

(defvar hydra_extract_helpers_decode_map (lambda (key_decoder) (lambda (val_decoder) (lambda (g) (lambda (term) (funcall (hydra_lib_eithers_bind (funcall (funcall (hydra_lib_eithers_bimap (lambda (x) x)) (lambda (x) x)) (funcall (hydra_lexical_strip_and_dereference_term_either g) term))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :map) (funcall (lambda (m) (funcall (hydra_lib_eithers_map hydra_lib_maps_from_list) (funcall (hydra_lib_eithers_map_list (lambda (kv) (funcall (hydra_lib_eithers_bind (funcall (key_decoder g) (hydra_lib_pairs_first kv))) (lambda (k) (funcall (hydra_lib_eithers_map (lambda (v) (list k v))) (funcall (val_decoder g) (hydra_lib_pairs_second kv))))))) (hydra_lib_maps_to_list m)))) match_value)) (t (list :left "expected map")))) (cadr match_target))) stripped))))))))

(defvar hydra_extract_helpers_decode_maybe (lambda (elem_decoder) (lambda (g) (lambda (term) (funcall (hydra_lib_eithers_bind (funcall (funcall (hydra_lib_eithers_bimap (lambda (x) x)) (lambda (x) x)) (funcall (hydra_lexical_strip_and_dereference_term_either g) term))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :maybe) (funcall (lambda (opt) (funcall (hydra_lib_eithers_map_maybe (elem_decoder g)) opt)) match_value)) (t (list :left "expected optional value")))) (cadr match_target))) stripped)))))))

(defvar hydra_extract_helpers_decode_pair (lambda (first_decoder) (lambda (second_decoder) (lambda (g) (lambda (term) (funcall (hydra_lib_eithers_bind (funcall (funcall (hydra_lib_eithers_bimap (lambda (x) x)) (lambda (x) x)) (funcall (hydra_lexical_strip_and_dereference_term_either g) term))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :pair) (funcall (lambda (p) (funcall (hydra_lib_eithers_bind (funcall (first_decoder g) (hydra_lib_pairs_first p))) (lambda (f) (funcall (hydra_lib_eithers_map (lambda (s) (list f s))) (funcall (second_decoder g) (hydra_lib_pairs_second p)))))) match_value)) (t (list :left "expected pair")))) (cadr match_target))) stripped))))))))

(defvar hydra_extract_helpers_decode_set (lambda (elem_decoder) (lambda (g) (lambda (term) (funcall (hydra_lib_eithers_bind (funcall (funcall (hydra_lib_eithers_bimap (lambda (x) x)) (lambda (x) x)) (funcall (hydra_lexical_strip_and_dereference_term_either g) term))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :set) (funcall (lambda (s) (funcall (hydra_lib_eithers_map hydra_lib_sets_from_list) (funcall (hydra_lib_eithers_map_list (elem_decoder g)) (hydra_lib_sets_to_list s)))) match_value)) (t (list :left "expected set")))) (cadr match_target))) stripped)))))))

(defvar hydra_extract_helpers_decode_unit (lambda (g) (lambda (term) (funcall (hydra_lib_eithers_bind (funcall (funcall (hydra_lib_eithers_bimap (lambda (x) x)) (lambda (x) x)) (funcall (hydra_lexical_strip_and_dereference_term_either g) term))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :unit) (funcall (lambda (_) (list :right nil)) match_value)) (t (list :left "expected a unit value")))) (cadr match_target))) stripped))))))

(defvar hydra_extract_helpers_decode_wrapped (lambda (body_decoder) (lambda (g) (lambda (term) (funcall (hydra_lib_eithers_bind (funcall (funcall (hydra_lib_eithers_bimap (lambda (x) x)) (lambda (x) x)) (funcall (hydra_lexical_strip_and_dereference_term_either g) term))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :wrap) (funcall (lambda (wt) (funcall (body_decoder g) (funcall (lambda (v) (hydra_core_wrapped_term-body v)) wt))) match_value)) (t (list :left "expected wrapped value")))) (cadr match_target))) stripped)))))))

(defvar hydra_extract_helpers_require_field (lambda (field_name) (lambda (decoder) (lambda (field_map) (lambda (g) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :left (hydra_lib_strings_cat (list "missing field " field_name " in record"))))) (lambda (field_term) (funcall (decoder g) field_term))) (funcall (hydra_lib_maps_lookup field_name) field_map)))))))

(defvar hydra_extract_helpers_to_field_map (lambda (record) (hydra_lib_maps_from_list (funcall (hydra_lib_lists_map (lambda (f) (list (funcall (lambda (v) (hydra_core_field-name v)) f) (funcall (lambda (v) (hydra_core_field-term v)) f)))) (funcall (lambda (v) (hydra_core_record-fields v)) record)))))

(provide 'hydra.extract.helpers)
