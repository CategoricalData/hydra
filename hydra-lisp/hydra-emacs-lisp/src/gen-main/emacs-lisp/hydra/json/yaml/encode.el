(require 'cl-lib)

(require 'hydra.ext.org.yaml.model)

(require 'hydra.json.encode)

(require 'hydra.json.model)

(require 'hydra.lib.eithers)

(require 'hydra.lib.lists)

(require 'hydra.lib.maps)

(require 'hydra.lib.pairs)

(defvar hydra_json_yaml_encode_json_to_yaml (lambda (value) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :array) ((lambda (arr) (list :sequence ((hydra_lib_lists_map (lambda (v) (hydra_json_yaml_encode_json_to_yaml v))) arr))) match_value)) ((equal (car match_target) :boolean) ((lambda (b) (list :scalar (list :bool b))) match_value)) ((equal (car match_target) :null) ((lambda (_) (list :scalar (list :null nil))) match_value)) ((equal (car match_target) :number) ((lambda (n) (list :scalar (list :float n))) match_value)) ((equal (car match_target) :object) ((lambda (obj) (list :mapping (hydra_lib_maps_from_list ((hydra_lib_lists_map (lambda (kv) (list (list :scalar (list :str (hydra_lib_pairs_first kv))) (hydra_json_yaml_encode_json_to_yaml (hydra_lib_pairs_second kv))))) (hydra_lib_maps_to_list obj))))) match_value)) ((equal (car match_target) :string) ((lambda (s) (list :scalar (list :str s))) match_value)))) (cadr match_target))) value)))

(defvar hydra_json_yaml_encode_to_yaml (lambda (term) ((hydra_lib_eithers_map (lambda (v) (hydra_json_yaml_encode_json_to_yaml v))) (hydra_json_encode_to_json term))))

(provide 'hydra.json.yaml.encode)
