(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.error.core)

(require 'hydra.lib.strings)

(require 'hydra.show.core)

(require 'hydra.show.meta)

(defvar hydra_show_error_core_duplicate_binding_error (lambda (e) (hydra_lib_strings_cat (list "duplicate binding: " (funcall (lambda (v) v) (funcall (lambda (v) (hydra_error_core_duplicate_binding_error-name v)) e))))))

(defvar hydra_show_error_core_duplicate_field_error (lambda (e) (hydra_lib_strings_cat (list "duplicate field: " (funcall (lambda (v) v) (funcall (lambda (v) (hydra_error_core_duplicate_field_error-name v)) e))))))

(defvar hydra_show_error_core_invalid_term_error (lambda (e) (funcall (hydra_lib_strings_cat2 "invalid term: ") (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :duplicate_binding) (hydra_show_error_core_duplicate_binding_error match_value)) ((equal (car match_target) :duplicate_field) (hydra_show_error_core_duplicate_field_error match_value)))) (cadr match_target))) e))))

(defvar hydra_show_error_core_undefined_field_error (lambda (e) (let ((fname (funcall (lambda (v) (hydra_error_core_undefined_field_error-field_name v)) e))) (let ((tname (funcall (lambda (v) (hydra_error_core_undefined_field_error-type_name v)) e))) (hydra_lib_strings_cat (list "no such field \"" (funcall (lambda (v) v) fname) "\" in type \"" (funcall (lambda (v) v) tname) "\""))))))

(defvar hydra_show_error_core_undefined_term_error (lambda (e) (funcall (hydra_lib_strings_cat2 "undefined term: ") (funcall (lambda (v) v) (funcall (lambda (v) (hydra_error_core_undefined_term_error-name v)) e)))))

(defvar hydra_show_error_core_undefined_type_error (lambda (e) (funcall (hydra_lib_strings_cat2 "undefined type: ") (funcall (lambda (v) v) (funcall (lambda (v) (hydra_error_core_undefined_type_error-name v)) e)))))

(defvar hydra_show_error_core_unexpected_term_variant_error (lambda (e) (let ((expected (funcall (lambda (v) (hydra_error_core_unexpected_term_variant_error-expected_variant v)) e))) (let ((actual (funcall (lambda (v) (hydra_error_core_unexpected_term_variant_error-actual_term v)) e))) (hydra_lib_strings_cat (list "expected " (hydra_show_meta_term_variant expected) " term but found " (hydra_show_core_term actual)))))))

(defvar hydra_show_error_core_unexpected_type_variant_error (lambda (e) (let ((expected (funcall (lambda (v) (hydra_error_core_unexpected_type_variant_error-expected_variant v)) e))) (let ((actual (funcall (lambda (v) (hydra_error_core_unexpected_type_variant_error-actual_type v)) e))) (hydra_lib_strings_cat (list "expected " (hydra_show_meta_type_variant expected) " type but found " (hydra_show_core_type actual)))))))

(provide 'hydra.show.error.core)
