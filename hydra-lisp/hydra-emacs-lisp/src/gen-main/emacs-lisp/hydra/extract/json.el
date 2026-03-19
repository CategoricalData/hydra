(require 'cl-lib)

(require 'hydra.json.model)

(require 'hydra.lib.eithers)

(require 'hydra.lib.maps)

(require 'hydra.lib.maybes)

(require 'hydra.lib.strings)

(defvar hydra_extract_json_show_value (lambda (value) "TODO: implement showValue"))

(defvar hydra_extract_json_expect_array (lambda (value) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :array) ((lambda (els) (list :right els)) match_value)) (t (list :left ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "expected ") "JSON array")) ((hydra_lib_strings_cat2 " but found ") (hydra_extract_json_show_value value))))))) (cadr match_target))) value)))

(defvar hydra_extract_json_expect_number (lambda (value) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :number) ((lambda (d) (list :right d)) match_value)) (t (list :left ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "expected ") "JSON number")) ((hydra_lib_strings_cat2 " but found ") (hydra_extract_json_show_value value))))))) (cadr match_target))) value)))

(defvar hydra_extract_json_expect_object (lambda (value) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :object) ((lambda (m) (list :right m)) match_value)) (t (list :left ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "expected ") "JSON object")) ((hydra_lib_strings_cat2 " but found ") (hydra_extract_json_show_value value))))))) (cadr match_target))) value)))

(defvar hydra_extract_json_expect_string (lambda (value) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :string) ((lambda (s) (list :right s)) match_value)) (t (list :left ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "expected ") "JSON string")) ((hydra_lib_strings_cat2 " but found ") (hydra_extract_json_show_value value))))))) (cadr match_target))) value)))

(defvar hydra_extract_json_opt (lambda (fname) (lambda (m) ((hydra_lib_maps_lookup fname) m))))

(defvar hydra_extract_json_opt_array (lambda (fname) (lambda (m) (((hydra_lib_maybes_maybe (list :right (list :nothing))) (lambda (a) ((hydra_lib_eithers_map (lambda (x) (list :just x))) (hydra_extract_json_expect_array a)))) ((hydra_extract_json_opt fname) m)))))

(defvar hydra_extract_json_opt_string (lambda (fname) (lambda (m) (((hydra_lib_maybes_maybe (list :right (list :nothing))) (lambda (s) ((hydra_lib_eithers_map (lambda (x) (list :just x))) (hydra_extract_json_expect_string s)))) ((hydra_extract_json_opt fname) m)))))

(defvar hydra_extract_json_require (lambda (fname) (lambda (m) (((hydra_lib_maybes_maybe (list :left (hydra_lib_strings_cat (list "required attribute " (hydra_extract_json_show_value fname) " not found")))) (lambda (value) (list :right value))) ((hydra_lib_maps_lookup fname) m)))))

(defvar hydra_extract_json_require_array (lambda (fname) (lambda (m) ((hydra_lib_eithers_bind ((hydra_extract_json_require fname) m)) hydra_extract_json_expect_array))))

(defvar hydra_extract_json_require_number (lambda (fname) (lambda (m) ((hydra_lib_eithers_bind ((hydra_extract_json_require fname) m)) hydra_extract_json_expect_number))))

(defvar hydra_extract_json_require_string (lambda (fname) (lambda (m) ((hydra_lib_eithers_bind ((hydra_extract_json_require fname) m)) hydra_extract_json_expect_string))))

(provide 'hydra.extract.json)
