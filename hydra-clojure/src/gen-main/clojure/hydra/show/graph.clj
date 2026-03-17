(ns hydra.show.graph
  (:require [hydra.lib.lists :refer :all] [hydra.lib.strings :refer :all] [hydra.show.core :refer :all]
))

(declare hydra_show_graph_graph)

(def hydra_show_graph_graph (fn [elements] (let [element_strs ((hydra_lib_lists_map hydra_show_core_binding) elements)] (hydra_lib_strings_cat (list "{" ((hydra_lib_strings_intercalate ", ") element_strs) "}")))))
