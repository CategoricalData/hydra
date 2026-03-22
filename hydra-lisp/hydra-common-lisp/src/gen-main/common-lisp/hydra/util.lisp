(defpackage :hydra.util
(:use :cl :hydra.context :hydra.errors)
(:export :make-hydra_util_adapter :hydra_util_adapter? :hydra_util_adapter-is_lossy :hydra_util_adapter-source :hydra_util_adapter-target :hydra_util_adapter-coder :make-hydra_util_bicoder :hydra_util_bicoder? :hydra_util_bicoder-encode :hydra_util_bicoder-decode :hydra_util_case_convention-variants :make-hydra_util_coder :hydra_util_coder? :hydra_util_coder-encode :hydra_util_coder-decode :hydra_util_comparison-variants :hydra_util_precision-variants))

(in-package :hydra.util)

(cl:defstruct hydra_util_adapter is_lossy source target coder)

(cl:defstruct hydra_util_bicoder encode decode)

(cl:defvar hydra_util_case_convention-variants (cl:list :camel :pascal :lower_snake :upper_snake))

(cl:defstruct hydra_util_coder encode decode)

(cl:defvar hydra_util_comparison-variants (cl:list :less_than :equal_to :greater_than))

(cl:defvar hydra_util_precision-variants (cl:list :arbitrary :bits))
