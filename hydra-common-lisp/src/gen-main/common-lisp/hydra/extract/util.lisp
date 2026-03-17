(defpackage :hydra.extract.util
(:use :cl :hydra.context :hydra.core :hydra.error :hydra.extract.core :hydra.lib.eithers :hydra.lib.equality :hydra.lib.logic :hydra.lib.strings :hydra.util)
(:export :hydra_extract_util_comparison))

(in-package :hydra.extract.util)

(cl:defvar hydra_extract_util_comparison (cl:lambda (cx) (cl:lambda (graph) (cl:lambda (term) ((hydra_lib_eithers_bind ((((hydra_extract_core_unit_variant cx) "hydra.util.Comparison") graph) term)) (cl:lambda (fname) (if ((hydra_lib_equality_equal ((cl:lambda (v) v) fname)) "equalTo") (list :right (list :equal_to cl:nil)) (if ((hydra_lib_equality_equal ((cl:lambda (v) v) fname)) "lessThan") (list :right (list :less_than cl:nil)) (if ((hydra_lib_equality_equal ((cl:lambda (v) v) fname)) "greaterThan") (list :right (list :greater_than cl:nil)) (list :left (make-hydra_context_in_context ((hydra_lib_strings_cat2 "expected comparison but found ") ((cl:lambda (v) v) fname)) cx)))))))))))
