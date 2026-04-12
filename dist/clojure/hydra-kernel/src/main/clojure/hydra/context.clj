(ns hydra.context
  (:require [hydra.core :refer :all]
))

(defrecord hydra_context_context [trace messages other])
(defn make-hydra_context_context [trace messages other] (->hydra_context_context trace messages other))

(defrecord hydra_context_in_context [object context])
(defn make-hydra_context_in_context [object context] (->hydra_context_in_context object context))
