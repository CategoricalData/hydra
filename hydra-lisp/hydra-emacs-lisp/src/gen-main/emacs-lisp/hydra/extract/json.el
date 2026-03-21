(require 'cl-lib)

(require 'hydra.json.model)

(require 'hydra.lib.eithers)

(require 'hydra.lib.maps)

(require 'hydra.lib.maybes)

(require 'hydra.lib.strings)

(defvar hydra_extract_json_show_value (lambda (value) "TODO: implement showValue"))

(defvar hydra_extract_json_expect_array (lambda (value) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :array) (funcall (lambda (els) (list :right els)) match_value)) (t (list :left (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 "expected ") "JSON array")) (funcall (hydra_lib_strings_cat2 " but found ") (hydra_extract_json_show_value value))))))) (cadr match_target))) value)))

(defvar hydra_extract_json_expect_number (lambda (value) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :number) (funcall (lambda (d) (list :right d)) match_value)) (t (list :left (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 "expected ") "JSON number")) (funcall (hydra_lib_strings_cat2 " but found ") (hydra_extract_json_show_value value))))))) (cadr match_target))) value)))

(defvar hydra_extract_json_expect_object (lambda (value) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :object) (funcall (lambda (m) (list :right m)) match_value)) (t (list :left (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 "expected ") "JSON object")) (funcall (hydra_lib_strings_cat2 " but found ") (hydra_extract_json_show_value value))))))) (cadr match_target))) value)))

(defvar hydra_extract_json_expect_string (lambda (value) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :string) (funcall (lambda (s) (list :right s)) match_value)) (t (list :left (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 "expected ") "JSON string")) (funcall (hydra_lib_strings_cat2 " but found ") (hydra_extract_json_show_value value))))))) (cadr match_target))) value)))

(defvar hydra_extract_json_opt (lambda (fname) (lambda (m) (funcall (hydra_lib_maps_lookup fname) m))))

(defvar hydra_extract_json_opt_array (lambda (fname) (lambda (m) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :right (list :nothing)))) (lambda (a) (funcall (hydra_lib_eithers_map (lambda (x) (list :just x))) (hydra_extract_json_expect_array a)))) (funcall (hydra_extract_json_opt fname) m)))))

(defvar hydra_extract_json_opt_string (lambda (fname) (lambda (m) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :right (list :nothing)))) (lambda (s) (funcall (hydra_lib_eithers_map (lambda (x) (list :just x))) (hydra_extract_json_expect_string s)))) (funcall (hydra_extract_json_opt fname) m)))))

(defvar hydra_extract_json_require (lambda (fname) (lambda (m) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :left (hydra_lib_strings_cat (list "required attribute " (hydra_extract_json_show_value fname) " not found"))))) (lambda (value) (list :right value))) (funcall (hydra_lib_maps_lookup fname) m)))))

(defvar hydra_extract_json_require_array (lambda (fname) (lambda (m) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_json_require fname) m)) hydra_extract_json_expect_array))))

(defvar hydra_extract_json_require_number (lambda (fname) (lambda (m) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_json_require fname) m)) hydra_extract_json_expect_number))))

(defvar hydra_extract_json_require_string (lambda (fname) (lambda (m) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_json_require fname) m)) hydra_extract_json_expect_string))))

(provide 'hydra.extract.json)
