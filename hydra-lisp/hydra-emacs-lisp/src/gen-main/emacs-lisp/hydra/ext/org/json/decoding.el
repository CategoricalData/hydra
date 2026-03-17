(require 'cl-lib)

(require 'hydra.json.model)

(require 'hydra.lib.eithers)

(require 'hydra.lib.maps)

(require 'hydra.lib.maybes)

(require 'hydra.lib.strings)

(defvar hydra_ext_org_json_decoding_decode_array (lambda (decode_elem) (lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :array) ((lambda (a) ((hydra_lib_eithers_map_list decode_elem) a)) match_value)) (t (list :left "expected an array")))) (cadr match_target)))))

(defvar hydra_ext_org_json_decoding_decode_boolean (lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :boolean) ((lambda (b) (list :right b)) match_value)) (t (list :left "expected a boolean")))) (cadr match_target))))

(defvar hydra_ext_org_json_decoding_decode_optional_field (lambda (decode_value) (lambda (name) (lambda (m) (((hydra_lib_maybes_maybe (list :right nil)) (lambda (v) ((hydra_lib_eithers_map (lambda (x) x)) (decode_value v)))) ((hydra_lib_maps_lookup name) m))))))

(defvar hydra_ext_org_json_decoding_decode_field (lambda (decode_value) (lambda (name) (lambda (m) ((hydra_lib_eithers_bind (((hydra_ext_org_json_decoding_decode_optional_field decode_value) name) m)) ((hydra_lib_maybes_maybe (list :left ((hydra_lib_strings_cat2 "missing field: ") name))) (lambda (f) (list :right f))))))))

(defvar hydra_ext_org_json_decoding_decode_object (lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :object) ((lambda (o) (list :right o)) match_value)) (t (list :left "expected an object")))) (cadr match_target))))

(defvar hydra_ext_org_json_decoding_decode_string (lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :string) ((lambda (s) (list :right s)) match_value)) (t (list :left "expected a string")))) (cadr match_target))))

(provide 'hydra.ext.org.json.decoding)
