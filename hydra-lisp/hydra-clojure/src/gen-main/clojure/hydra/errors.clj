(ns hydra.errors
  (:require [hydra.core :refer :all] [hydra.error.checking :refer :all] [hydra.error.core :refer :all]
))

(declare hydra_errors_error-variants)

(defrecord hydra_errors_decoding_error [value])
(defn make-hydra_errors_decoding_error [value] (->hydra_errors_decoding_error value))

(def hydra_errors_error-variants (list :checking :decoding :duplicate_binding :duplicate_field :other :undefined_field :undefined_term_variable :untyped_term_variable :unexpected_term_variant :unexpected_type_variant :unification))

(defrecord hydra_errors_other_error [value])
(defn make-hydra_errors_other_error [value] (->hydra_errors_other_error value))

(defrecord hydra_errors_unification_error [left_type right_type message])
(defn make-hydra_errors_unification_error [left_type right_type message] (->hydra_errors_unification_error left_type right_type message))
