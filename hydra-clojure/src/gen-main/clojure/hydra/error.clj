(ns hydra.error
  (:require [hydra.core :refer :all]
))

(declare hydra_error_error-variants)

(defrecord hydra_error_decoding_error [value])
(defn make-hydra_error_decoding_error [value] (->hydra_error_decoding_error value))

(def hydra_error_error-variants (list :decoding :other :unification))

(defrecord hydra_error_other_error [value])
(defn make-hydra_error_other_error [value] (->hydra_error_other_error value))

(defrecord hydra_error_unification_error [left_type right_type message])
(defn make-hydra_error_unification_error [left_type right_type message] (->hydra_error_unification_error left_type right_type message))
