(defpackage :hydra.ext.org.json.decoding
(:use :cl :hydra.json.model :hydra.lib.eithers :hydra.lib.maps :hydra.lib.maybes :hydra.lib.strings)
(:export :hydra_ext_org_json_decoding_decode_array :hydra_ext_org_json_decoding_decode_boolean :hydra_ext_org_json_decoding_decode_optional_field :hydra_ext_org_json_decoding_decode_field :hydra_ext_org_json_decoding_decode_object :hydra_ext_org_json_decoding_decode_string))

(in-package :hydra.ext.org.json.decoding)

(cl:defvar hydra_ext_org_json_decoding_decode_array (cl:lambda (decode_elem) (cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :array) ((cl:lambda (a) ((hydra_lib_eithers_map_list decode_elem) a)) match_value)) (t (list :left "expected an array")))) (cadr match_target)))))

(cl:defvar hydra_ext_org_json_decoding_decode_boolean (cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :boolean) ((cl:lambda (b) (list :right b)) match_value)) (t (list :left "expected a boolean")))) (cadr match_target))))

(cl:defvar hydra_ext_org_json_decoding_decode_optional_field (cl:lambda (decode_value) (cl:lambda (name) (cl:lambda (m) (((hydra_lib_maybes_maybe (list :right cl:nil)) (cl:lambda (v) ((hydra_lib_eithers_map (cl:lambda (x) x)) (decode_value v)))) ((hydra_lib_maps_lookup name) m))))))

(cl:defvar hydra_ext_org_json_decoding_decode_field (cl:lambda (decode_value) (cl:lambda (name) (cl:lambda (m) ((hydra_lib_eithers_bind (((hydra_ext_org_json_decoding_decode_optional_field decode_value) name) m)) ((hydra_lib_maybes_maybe (list :left ((hydra_lib_strings_cat2 "missing field: ") name))) (cl:lambda (f) (list :right f))))))))

(cl:defvar hydra_ext_org_json_decoding_decode_object (cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :object) ((cl:lambda (o) (list :right o)) match_value)) (t (list :left "expected an object")))) (cadr match_target))))

(cl:defvar hydra_ext_org_json_decoding_decode_string (cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :string) ((cl:lambda (s) (list :right s)) match_value)) (t (list :left "expected a string")))) (cadr match_target))))
