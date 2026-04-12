(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.errors)

(require 'hydra.extract.core)

(require 'hydra.lib.eithers)

(require 'hydra.lib.equality)

(require 'hydra.lib.logic)

(require 'hydra.util)

(defvar hydra_extract_util_comparison (lambda (cx) (lambda (graph) (lambda (term) (funcall (hydra_lib_eithers_bind (funcall (funcall (hydra_extract_core_unit_variant "hydra.util.Comparison") graph) term)) (lambda (fname) (if (funcall (hydra_lib_equality_equal (funcall (lambda (v) v) fname)) "equalTo") (list :right (list :equal_to nil)) (if (funcall (hydra_lib_equality_equal (funcall (lambda (v) v) fname)) "lessThan") (list :right (list :less_than nil)) (if (funcall (hydra_lib_equality_equal (funcall (lambda (v) v) fname)) "greaterThan") (list :right (list :greater_than nil)) (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "comparison" (funcall (lambda (v) v) fname))))))))))))))

(provide 'hydra.extract.util)
