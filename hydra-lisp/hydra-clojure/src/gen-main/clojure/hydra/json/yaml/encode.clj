(ns hydra.json.yaml.encode
  (:require [hydra.ext.org.yaml.model :refer :all] [hydra.json.encode :refer :all] [hydra.json.model :refer :all] [hydra.lib.eithers :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.maps :refer :all] [hydra.lib.pairs :refer :all]
))

(declare hydra_json_yaml_encode_json_to_yaml hydra_json_yaml_encode_to_yaml)

(def hydra_json_yaml_encode_json_to_yaml (fn [value] ((fn [match_target] ((fn [match_value] (cond (= (first match_target) :array) ((fn [arr] (list :sequence ((hydra_lib_lists_map (fn [v] (hydra_json_yaml_encode_json_to_yaml v))) arr))) match_value) (= (first match_target) :boolean) ((fn [b] (list :scalar (list :bool b))) match_value) (= (first match_target) :null) ((fn [_] (list :scalar (list :null nil))) match_value) (= (first match_target) :number) ((fn [n] (list :scalar (list :float n))) match_value) (= (first match_target) :object) ((fn [obj] (list :mapping (hydra_lib_maps_from_list ((hydra_lib_lists_map (fn [kv] (list (list :scalar (list :str (hydra_lib_pairs_first kv))) (hydra_json_yaml_encode_json_to_yaml (hydra_lib_pairs_second kv))))) (hydra_lib_maps_to_list obj))))) match_value) (= (first match_target) :string) ((fn [s] (list :scalar (list :str s))) match_value))) (second match_target))) value)))

(def hydra_json_yaml_encode_to_yaml (fn [term] ((hydra_lib_eithers_map (fn [v] (hydra_json_yaml_encode_json_to_yaml v))) (hydra_json_encode_to_json term))))
