(ns hydra.compute
  (:require [hydra.context :refer :all] [hydra.error :refer :all]
))

(defrecord hydra_compute_adapter [is_lossy source target coder])
(defn make-hydra_compute_adapter [is_lossy source target coder] (->hydra_compute_adapter is_lossy source target coder))

(defrecord hydra_compute_bicoder [encode decode])
(defn make-hydra_compute_bicoder [encode decode] (->hydra_compute_bicoder encode decode))

(defrecord hydra_compute_coder [encode decode])
(defn make-hydra_compute_coder [encode decode] (->hydra_compute_coder encode decode))
