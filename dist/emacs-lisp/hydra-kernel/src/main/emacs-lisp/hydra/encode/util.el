(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.util)

(defvar hydra_encode_util_case_convention (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :camel) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.util.CaseConvention" (make-hydra_core_field "camel" (funcall (lambda (_) (list :unit nil)) y))))) match_value)) ((equal (car match_target) :pascal) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.util.CaseConvention" (make-hydra_core_field "pascal" (funcall (lambda (_) (list :unit nil)) y))))) match_value)) ((equal (car match_target) :lower_snake) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.util.CaseConvention" (make-hydra_core_field "lowerSnake" (funcall (lambda (_) (list :unit nil)) y))))) match_value)) ((equal (car match_target) :upper_snake) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.util.CaseConvention" (make-hydra_core_field "upperSnake" (funcall (lambda (_) (list :unit nil)) y))))) match_value)))) (cadr match_target))))

(defvar hydra_encode_util_comparison (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :less_than) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.util.Comparison" (make-hydra_core_field "lessThan" (funcall (lambda (_) (list :unit nil)) y))))) match_value)) ((equal (car match_target) :equal_to) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.util.Comparison" (make-hydra_core_field "equalTo" (funcall (lambda (_) (list :unit nil)) y))))) match_value)) ((equal (car match_target) :greater_than) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.util.Comparison" (make-hydra_core_field "greaterThan" (funcall (lambda (_) (list :unit nil)) y))))) match_value)))) (cadr match_target))))

(defvar hydra_encode_util_precision (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :arbitrary) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.util.Precision" (make-hydra_core_field "arbitrary" (funcall (lambda (_) (list :unit nil)) y))))) match_value)) ((equal (car match_target) :bits) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.util.Precision" (make-hydra_core_field "bits" (funcall (lambda (x) (list :literal (list :integer (list :int32 x)))) y))))) match_value)))) (cadr match_target))))

(provide 'hydra.encode.util)
