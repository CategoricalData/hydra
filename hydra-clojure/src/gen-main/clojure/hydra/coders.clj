(ns hydra.coders
  (:require [hydra.core :refer :all] [hydra.graph :refer :all] [hydra.util :refer :all] [hydra.variants :refer :all]
))

(declare hydra_coders_coder_direction-variants hydra_coders_traversal_order-variants)

(defrecord hydra_coders_adapter_context [graph language adapters])
(defn make-hydra_coders_adapter_context [graph language adapters] (->hydra_coders_adapter_context graph language adapters))

(def hydra_coders_coder_direction-variants (list :encode :decode))

(defrecord hydra_coders_language [name constraints])
(defn make-hydra_coders_language [name constraints] (->hydra_coders_language name constraints))

(defrecord hydra_coders_language_constraints [elimination_variants literal_variants float_types function_variants integer_types term_variants type_variants types])
(defn make-hydra_coders_language_constraints [elimination_variants literal_variants float_types function_variants integer_types term_variants type_variants types] (->hydra_coders_language_constraints elimination_variants literal_variants float_types function_variants integer_types term_variants type_variants types))

(defrecord hydra_coders_language_name [value])
(defn make-hydra_coders_language_name [value] (->hydra_coders_language_name value))

(def hydra_coders_traversal_order-variants (list :pre :post))
