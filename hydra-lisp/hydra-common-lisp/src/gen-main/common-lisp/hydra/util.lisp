(defpackage :hydra.util
(:use :cl)
(:export :hydra_util_case_convention-variants :hydra_util_comparison-variants :hydra_util_precision-variants))

(in-package :hydra.util)

(cl:defvar hydra_util_case_convention-variants (cl:list :camel :pascal :lower_snake :upper_snake))

(cl:defvar hydra_util_comparison-variants (cl:list :less_than :equal_to :greater_than))

(cl:defvar hydra_util_precision-variants (cl:list :arbitrary :bits))
