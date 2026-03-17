(ns hydra.monads
  (:require [hydra.context :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.maps :refer :all] [hydra.lib.maybes :refer :all]
))

(declare hydra_monads_empty_context hydra_monads_maybe_to_list)

(def hydra_monads_empty_context (->hydra_context_context (list) (list) hydra_lib_maps_empty))

(def hydra_monads_maybe_to_list (fn [mx] (((hydra_lib_maybes_maybe (list)) hydra_lib_lists_pure) mx)))
