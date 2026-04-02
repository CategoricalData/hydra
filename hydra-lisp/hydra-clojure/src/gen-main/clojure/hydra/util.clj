(ns hydra.util)

(declare hydra_util_case_convention-variants hydra_util_comparison-variants hydra_util_precision-variants)

(def hydra_util_case_convention-variants (list :camel :pascal :lower_snake :upper_snake))

(def hydra_util_comparison-variants (list :less_than :equal_to :greater_than))

(def hydra_util_precision-variants (list :arbitrary :bits))
