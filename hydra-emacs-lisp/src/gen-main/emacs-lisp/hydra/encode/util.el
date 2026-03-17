(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.util)

(defvar hydra_encode_util_case_convention (lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :camel) ((lambda (y) (list :union (make-hydra_core_injection "hydra.util.CaseConvention" (make-hydra_core_field "camel" ((lambda (_) (list :unit nil)) y))))) match_value)) ((equal (car match_target) :pascal) ((lambda (y) (list :union (make-hydra_core_injection "hydra.util.CaseConvention" (make-hydra_core_field "pascal" ((lambda (_) (list :unit nil)) y))))) match_value)) ((equal (car match_target) :lower_snake) ((lambda (y) (list :union (make-hydra_core_injection "hydra.util.CaseConvention" (make-hydra_core_field "lowerSnake" ((lambda (_) (list :unit nil)) y))))) match_value)) ((equal (car match_target) :upper_snake) ((lambda (y) (list :union (make-hydra_core_injection "hydra.util.CaseConvention" (make-hydra_core_field "upperSnake" ((lambda (_) (list :unit nil)) y))))) match_value)))) (cadr match_target))))

(defvar hydra_encode_util_comparison (lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :less_than) ((lambda (y) (list :union (make-hydra_core_injection "hydra.util.Comparison" (make-hydra_core_field "lessThan" ((lambda (_) (list :unit nil)) y))))) match_value)) ((equal (car match_target) :equal_to) ((lambda (y) (list :union (make-hydra_core_injection "hydra.util.Comparison" (make-hydra_core_field "equalTo" ((lambda (_) (list :unit nil)) y))))) match_value)) ((equal (car match_target) :greater_than) ((lambda (y) (list :union (make-hydra_core_injection "hydra.util.Comparison" (make-hydra_core_field "greaterThan" ((lambda (_) (list :unit nil)) y))))) match_value)))) (cadr match_target))))

(defvar hydra_encode_util_precision (lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :arbitrary) ((lambda (y) (list :union (make-hydra_core_injection "hydra.util.Precision" (make-hydra_core_field "arbitrary" ((lambda (_) (list :unit nil)) y))))) match_value)) ((equal (car match_target) :bits) ((lambda (y) (list :union (make-hydra_core_injection "hydra.util.Precision" (make-hydra_core_field "bits" ((lambda (x) (list :literal (list :integer (list :int32 x)))) y))))) match_value)))) (cadr match_target))))

(provide 'hydra.encode.util)
