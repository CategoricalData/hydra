(ns hydra.encode.context
  (:require [hydra.context :refer :all] [hydra.core :refer :all] [hydra.encode.core :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.maps :refer :all]
))

(declare hydra_encode_context_context hydra_encode_context_in_context)

(def hydra_encode_context_context (fn [x] (list :record (->hydra_core_record "hydra.context.Context" (list (->hydra_core_field "trace" ((fn [xs] (list :list ((hydra_lib_lists_map (fn [x2] (list :literal (list :string x2)))) xs))) ((fn [v] (:trace v)) x))) (->hydra_core_field "messages" ((fn [xs] (list :list ((hydra_lib_lists_map (fn [x2] (list :literal (list :string x2)))) xs))) ((fn [v] (:messages v)) x))) (->hydra_core_field "other" ((fn [m] (list :map (((hydra_lib_maps_bimap hydra_encode_core_name) hydra_encode_core_term) m))) ((fn [v] (:other v)) x))))))))

(def hydra_encode_context_in_context (fn [e] (fn [x] (list :record (->hydra_core_record "hydra.context.InContext" (list (->hydra_core_field "object" (e ((fn [v] (:object v)) x))) (->hydra_core_field "context" (hydra_encode_context_context ((fn [v] (:context v)) x)))))))))
