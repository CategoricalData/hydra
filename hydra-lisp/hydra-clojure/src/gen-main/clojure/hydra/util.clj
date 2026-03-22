(ns hydra.util
  (:require [hydra.context :refer :all] [hydra.errors :refer :all]
))

(declare hydra_util_case_convention-variants hydra_util_comparison-variants hydra_util_precision-variants)

(defrecord hydra_util_adapter [is_lossy source target coder])
(defn make-hydra_util_adapter [is_lossy source target coder] (->hydra_util_adapter is_lossy source target coder))

(defrecord hydra_util_bicoder [encode decode])
(defn make-hydra_util_bicoder [encode decode] (->hydra_util_bicoder encode decode))

(def hydra_util_case_convention-variants (list :camel :pascal :lower_snake :upper_snake))

(defrecord hydra_util_coder [encode decode])
(defn make-hydra_util_coder [encode decode] (->hydra_util_coder encode decode))

(def hydra_util_comparison-variants (list :less_than :equal_to :greater_than))

(def hydra_util_precision-variants (list :arbitrary :bits))
