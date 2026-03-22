(require 'cl-lib)

(require 'hydra.context)

(require 'hydra.core)

(require 'hydra.errors)

(require 'hydra.extract.core)

(require 'hydra.lib.eithers)

(require 'hydra.lib.equality)

(require 'hydra.lib.logic)

(require 'hydra.lib.strings)

(require 'hydra.util)

(defvar hydra_extract_util_comparison (lambda (cx) (lambda (graph) (lambda (term) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_core_unit_variant cx) "hydra.util.Comparison") graph) term)) (lambda (fname) (if (funcall (hydra_lib_equality_equal (funcall (lambda (v) v) fname)) "equalTo") (list :right (list :equal_to nil)) (if (funcall (hydra_lib_equality_equal (funcall (lambda (v) v) fname)) "lessThan") (list :right (list :less_than nil)) (if (funcall (hydra_lib_equality_equal (funcall (lambda (v) v) fname)) "greaterThan") (list :right (list :greater_than nil)) (list :left (make-hydra_context_in_context (list :other (funcall (hydra_lib_strings_cat2 "expected comparison but found ") (funcall (lambda (v) v) fname))) cx)))))))))))

(provide 'hydra.extract.util)
