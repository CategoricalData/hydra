(defpackage :hydra.extract.json
(:use :cl :hydra.json.model :hydra.lib.eithers :hydra.lib.maps :hydra.lib.maybes :hydra.lib.strings)
(:export :hydra_extract_json_show_value :hydra_extract_json_expect_array :hydra_extract_json_expect_number :hydra_extract_json_expect_object :hydra_extract_json_expect_string :hydra_extract_json_opt :hydra_extract_json_opt_array :hydra_extract_json_opt_string :hydra_extract_json_require :hydra_extract_json_require_array :hydra_extract_json_require_number :hydra_extract_json_require_string))

(in-package :hydra.extract.json)

(cl:defvar hydra_extract_json_show_value (cl:lambda (value) "TODO: implement showValue"))

(cl:defvar hydra_extract_json_expect_array (cl:lambda (value) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :array) ((cl:lambda (els) (list :right els)) match_value)) (t (list :left ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "expected ") "JSON array")) ((hydra_lib_strings_cat2 " but found ") (hydra_extract_json_show_value value))))))) (cadr match_target))) value)))

(cl:defvar hydra_extract_json_expect_number (cl:lambda (value) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :number) ((cl:lambda (d) (list :right d)) match_value)) (t (list :left ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "expected ") "JSON number")) ((hydra_lib_strings_cat2 " but found ") (hydra_extract_json_show_value value))))))) (cadr match_target))) value)))

(cl:defvar hydra_extract_json_expect_object (cl:lambda (value) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :object) ((cl:lambda (m) (list :right m)) match_value)) (t (list :left ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "expected ") "JSON object")) ((hydra_lib_strings_cat2 " but found ") (hydra_extract_json_show_value value))))))) (cadr match_target))) value)))

(cl:defvar hydra_extract_json_expect_string (cl:lambda (value) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :string) ((cl:lambda (s) (list :right s)) match_value)) (t (list :left ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "expected ") "JSON string")) ((hydra_lib_strings_cat2 " but found ") (hydra_extract_json_show_value value))))))) (cadr match_target))) value)))

(cl:defvar hydra_extract_json_opt (cl:lambda (fname) (cl:lambda (m) ((hydra_lib_maps_lookup fname) m))))

(cl:defvar hydra_extract_json_opt_array (cl:lambda (fname) (cl:lambda (m) (((hydra_lib_maybes_maybe (list :right (list :nothing))) (cl:lambda (a) ((hydra_lib_eithers_map (cl:lambda (x) (list :just x))) (hydra_extract_json_expect_array a)))) ((hydra_extract_json_opt fname) m)))))

(cl:defvar hydra_extract_json_opt_string (cl:lambda (fname) (cl:lambda (m) (((hydra_lib_maybes_maybe (list :right (list :nothing))) (cl:lambda (s) ((hydra_lib_eithers_map (cl:lambda (x) (list :just x))) (hydra_extract_json_expect_string s)))) ((hydra_extract_json_opt fname) m)))))

(cl:defvar hydra_extract_json_require (cl:lambda (fname) (cl:lambda (m) (((hydra_lib_maybes_maybe (list :left (hydra_lib_strings_cat (cl:list "required attribute " (hydra_extract_json_show_value fname) " not found")))) (cl:lambda (value) (list :right value))) ((hydra_lib_maps_lookup fname) m)))))

(cl:defvar hydra_extract_json_require_array (cl:lambda (fname) (cl:lambda (m) ((hydra_lib_eithers_bind ((hydra_extract_json_require fname) m)) hydra_extract_json_expect_array))))

(cl:defvar hydra_extract_json_require_number (cl:lambda (fname) (cl:lambda (m) ((hydra_lib_eithers_bind ((hydra_extract_json_require fname) m)) hydra_extract_json_expect_number))))

(cl:defvar hydra_extract_json_require_string (cl:lambda (fname) (cl:lambda (m) ((hydra_lib_eithers_bind ((hydra_extract_json_require fname) m)) hydra_extract_json_expect_string))))
