(define-library (hydra extract util)
(export hydra_extract_util_comparison)
(import (scheme base) (hydra context) (hydra core) (hydra errors) (hydra extract core) (hydra lib eithers) (hydra lib equality) (hydra lib logic) (hydra lib strings) (hydra util))
(begin
(define hydra_extract_util_comparison (lambda (cx) (lambda (graph) (lambda (term) ((hydra_lib_eithers_bind ((((hydra_extract_core_unit_variant cx) "hydra.util.Comparison") graph) term)) (lambda (fname) (if ((hydra_lib_equality_equal ((lambda (v) v) fname)) "equalTo") (list 'right (list 'equal_to '())) (if ((hydra_lib_equality_equal ((lambda (v) v) fname)) "lessThan") (list 'right (list 'less_than '())) (if ((hydra_lib_equality_equal ((lambda (v) v) fname)) "greaterThan") (list 'right (list 'greater_than '())) (list 'left (make-hydra_context_in_context (list 'other ((hydra_lib_strings_cat2 "expected comparison but found ") ((lambda (v) v) fname))) cx)))))))))))))
