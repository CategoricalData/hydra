(ns hydra.phantoms
  (:require [hydra.core :refer :all]
))

(defrecord hydra_phantoms_t_binding [name term])
(defn make-hydra_phantoms_t_binding [name term] (->hydra_phantoms_t_binding name term))

(defrecord hydra_phantoms_t_term [value])
(defn make-hydra_phantoms_t_term [value] (->hydra_phantoms_t_term value))

(defrecord hydra_phantoms_t_term_definition [name term])
(defn make-hydra_phantoms_t_term_definition [name term] (->hydra_phantoms_t_term_definition name term))
