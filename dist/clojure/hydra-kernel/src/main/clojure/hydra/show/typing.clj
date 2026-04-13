(ns hydra.show.typing
  (:require [hydra.core :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.maps :refer :all] [hydra.lib.pairs :refer :all] [hydra.lib.strings :refer :all] [hydra.show.core :refer :all] [hydra.typing :refer :all]
))

(declare hydra_show_typing_type_constraint hydra_show_typing_type_subst)

(def hydra_show_typing_type_constraint (fn [tc] (let [ltyp ((fn [v] (:left v)) tc) rtyp ((fn [v] (:right v)) tc)] (hydra_lib_strings_cat (list (hydra_show_core_type ltyp) "≡" (hydra_show_core_type rtyp))))))

(def hydra_show_typing_type_subst (fn [ts] (let [subst ((fn [v] v) ts) pairs (hydra_lib_maps_to_list subst) show_pair (fn [pair] (let [name ((fn [v] v) (hydra_lib_pairs_first pair)) typ (hydra_lib_pairs_second pair)] (hydra_lib_strings_cat (list name "↦" (hydra_show_core_type typ))))) pair_strs ((hydra_lib_lists_map show_pair) pairs)] (hydra_lib_strings_cat (list "{" ((hydra_lib_strings_intercalate ",") pair_strs) "}")))))
