(ns hydra.encode.phantoms
  (:require [hydra.core :refer :all] [hydra.encode.core :refer :all] [hydra.phantoms :refer :all]
))

(declare hydra_encode_phantoms_t_term hydra_encode_phantoms_t_binding hydra_encode_phantoms_t_term_definition)

(def hydra_encode_phantoms_t_term (fn [a] (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.phantoms.TTerm" (hydra_encode_core_term ((fn [v] v) x)))))))

(def hydra_encode_phantoms_t_binding (fn [a] (fn [x] (list :record (->hydra_core_record "hydra.phantoms.TBinding" (list (->hydra_core_field "name" (hydra_encode_core_name ((fn [v] (:name v)) x))) (->hydra_core_field "term" ((hydra_encode_phantoms_t_term a) ((fn [v] (:term v)) x)))))))))

(def hydra_encode_phantoms_t_term_definition (fn [a] (fn [x] (list :record (->hydra_core_record "hydra.phantoms.TTermDefinition" (list (->hydra_core_field "name" (hydra_encode_core_name ((fn [v] (:name v)) x))) (->hydra_core_field "term" ((hydra_encode_phantoms_t_term a) ((fn [v] (:term v)) x)))))))))
