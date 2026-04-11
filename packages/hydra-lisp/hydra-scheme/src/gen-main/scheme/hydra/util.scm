(define-library (hydra util)
(export hydra_util_case_convention-variants hydra_util_comparison-variants hydra_util_precision-variants)
(import (scheme base))
(begin
(define hydra_util_case_convention-variants (list 'camel 'pascal 'lower_snake 'upper_snake))
(define hydra_util_comparison-variants (list 'less_than 'equal_to 'greater_than))
(define hydra_util_precision-variants (list 'arbitrary 'bits))))
