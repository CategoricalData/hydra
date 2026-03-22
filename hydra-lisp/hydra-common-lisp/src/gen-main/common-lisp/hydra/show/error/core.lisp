(defpackage :hydra.show.error.core
(:use :cl :hydra.core :hydra.error.core :hydra.lib.strings :hydra.show.core :hydra.show.meta)
(:export :hydra_show_error_core_duplicate_binding_error :hydra_show_error_core_duplicate_field_error :hydra_show_error_core_invalid_term_error :hydra_show_error_core_undefined_field_error :hydra_show_error_core_undefined_term_error :hydra_show_error_core_undefined_type_error :hydra_show_error_core_unexpected_term_variant_error :hydra_show_error_core_unexpected_type_variant_error))

(in-package :hydra.show.error.core)

(cl:defvar hydra_show_error_core_duplicate_binding_error (cl:lambda (e) (hydra_lib_strings_cat (cl:list "duplicate binding: " ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_core_duplicate_binding_error-name v)) e))))))

(cl:defvar hydra_show_error_core_duplicate_field_error (cl:lambda (e) (hydra_lib_strings_cat (cl:list "duplicate field: " ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_core_duplicate_field_error-name v)) e))))))

(cl:defvar hydra_show_error_core_invalid_term_error (cl:lambda (e) ((hydra_lib_strings_cat2 "invalid term: ") ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :duplicate_binding) (hydra_show_error_core_duplicate_binding_error match_value)) ((equal (car match_target) :duplicate_field) (hydra_show_error_core_duplicate_field_error match_value)))) (cadr match_target))) e))))

(cl:defvar hydra_show_error_core_undefined_field_error (cl:lambda (e) (let ((fname ((cl:lambda (v) (hydra_error_core_undefined_field_error-field_name v)) e))) (let ((tname ((cl:lambda (v) (hydra_error_core_undefined_field_error-type_name v)) e))) (hydra_lib_strings_cat (cl:list "no such field \"" ((cl:lambda (v) v) fname) "\" in type \"" ((cl:lambda (v) v) tname) "\""))))))

(cl:defvar hydra_show_error_core_undefined_term_error (cl:lambda (e) ((hydra_lib_strings_cat2 "undefined term: ") ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_core_undefined_term_error-name v)) e)))))

(cl:defvar hydra_show_error_core_undefined_type_error (cl:lambda (e) ((hydra_lib_strings_cat2 "undefined type: ") ((cl:lambda (v) v) ((cl:lambda (v) (hydra_error_core_undefined_type_error-name v)) e)))))

(cl:defvar hydra_show_error_core_unexpected_term_variant_error (cl:lambda (e) (let ((expected ((cl:lambda (v) (hydra_error_core_unexpected_term_variant_error-expected_variant v)) e))) (let ((actual ((cl:lambda (v) (hydra_error_core_unexpected_term_variant_error-actual_term v)) e))) (hydra_lib_strings_cat (cl:list "expected " (hydra_show_meta_term_variant expected) " term but found " (hydra_show_core_term actual)))))))

(cl:defvar hydra_show_error_core_unexpected_type_variant_error (cl:lambda (e) (let ((expected ((cl:lambda (v) (hydra_error_core_unexpected_type_variant_error-expected_variant v)) e))) (let ((actual ((cl:lambda (v) (hydra_error_core_unexpected_type_variant_error-actual_type v)) e))) (hydra_lib_strings_cat (cl:list "expected " (hydra_show_meta_type_variant expected) " type but found " (hydra_show_core_type actual)))))))
