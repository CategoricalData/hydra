(defpackage :hydra.json.yaml.encode
(:use :cl :hydra.json.encode :hydra.json.model :hydra.lib.eithers :hydra.lib.lists :hydra.lib.maps :hydra.lib.pairs :hydra.yaml.model)
(:export :hydra_json_yaml_encode_json_to_yaml :hydra_json_yaml_encode_to_yaml))

(in-package :hydra.json.yaml.encode)

(cl:defvar hydra_json_yaml_encode_json_to_yaml (cl:lambda (value) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :array) ((cl:lambda (arr) (list :sequence ((hydra_lib_lists_map (cl:lambda (v) (hydra_json_yaml_encode_json_to_yaml v))) arr))) match_value)) ((equal (car match_target) :boolean) ((cl:lambda (b) (list :scalar (list :bool b))) match_value)) ((equal (car match_target) :null) ((cl:lambda (_) (list :scalar (list :null cl:nil))) match_value)) ((equal (car match_target) :number) ((cl:lambda (n) (list :scalar (list :decimal n))) match_value)) ((equal (car match_target) :object) ((cl:lambda (obj) (list :mapping (hydra_lib_maps_from_list ((hydra_lib_lists_map (cl:lambda (kv) (cl:list (list :scalar (list :str (hydra_lib_pairs_first kv))) (hydra_json_yaml_encode_json_to_yaml (hydra_lib_pairs_second kv))))) (hydra_lib_maps_to_list obj))))) match_value)) ((equal (car match_target) :string) ((cl:lambda (s) (list :scalar (list :str s))) match_value)))) (cadr match_target))) value)))

(cl:defvar hydra_json_yaml_encode_to_yaml (cl:lambda (types) (cl:lambda (tname) (cl:lambda (typ) (cl:lambda (term) ((hydra_lib_eithers_map (cl:lambda (v) (hydra_json_yaml_encode_json_to_yaml v))) ((((hydra_json_encode_to_json types) tname) typ) term)))))))
