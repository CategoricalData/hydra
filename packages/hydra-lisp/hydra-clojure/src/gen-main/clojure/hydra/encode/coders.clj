(ns hydra.encode.coders
  (:require [hydra.coders :refer :all] [hydra.core :refer :all]
))

(declare hydra_encode_coders_coder_direction hydra_encode_coders_language_name hydra_encode_coders_traversal_order)

(def hydra_encode_coders_coder_direction (fn [match_target] ((fn [match_value] (cond (= (first match_target) :encode) ((fn [y] (list :union (->hydra_core_injection "hydra.coders.CoderDirection" (->hydra_core_field "encode" ((fn [_] (list :unit nil)) y))))) match_value) (= (first match_target) :decode) ((fn [y] (list :union (->hydra_core_injection "hydra.coders.CoderDirection" (->hydra_core_field "decode" ((fn [_] (list :unit nil)) y))))) match_value))) (second match_target))))

(def hydra_encode_coders_language_name (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.coders.LanguageName" ((fn [x2] (list :literal (list :string x2))) ((fn [v] v) x))))))

(def hydra_encode_coders_traversal_order (fn [match_target] ((fn [match_value] (cond (= (first match_target) :pre) ((fn [y] (list :union (->hydra_core_injection "hydra.coders.TraversalOrder" (->hydra_core_field "pre" ((fn [_] (list :unit nil)) y))))) match_value) (= (first match_target) :post) ((fn [y] (list :union (->hydra_core_injection "hydra.coders.TraversalOrder" (->hydra_core_field "post" ((fn [_] (list :unit nil)) y))))) match_value))) (second match_target))))
