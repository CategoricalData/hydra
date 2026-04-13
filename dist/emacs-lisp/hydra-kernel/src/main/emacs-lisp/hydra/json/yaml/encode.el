(require 'cl-lib)

(require 'hydra.json.encode)

(require 'hydra.json.model)

(require 'hydra.lib.eithers)

(require 'hydra.lib.lists)

(require 'hydra.lib.maps)

(require 'hydra.lib.pairs)

(require 'hydra.yaml.model)

(defvar hydra_json_yaml_encode_json_to_yaml (lambda (value) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :array) (funcall (lambda (arr) (list :sequence (funcall (hydra_lib_lists_map (lambda (v) (hydra_json_yaml_encode_json_to_yaml v))) arr))) match_value)) ((equal (car match_target) :boolean) (funcall (lambda (b) (list :scalar (list :bool b))) match_value)) ((equal (car match_target) :null) (funcall (lambda (_) (list :scalar (list :null nil))) match_value)) ((equal (car match_target) :number) (funcall (lambda (n) (list :scalar (list :float n))) match_value)) ((equal (car match_target) :object) (funcall (lambda (obj) (list :mapping (hydra_lib_maps_from_list (funcall (hydra_lib_lists_map (lambda (kv) (list (list :scalar (list :str (hydra_lib_pairs_first kv))) (hydra_json_yaml_encode_json_to_yaml (hydra_lib_pairs_second kv))))) (hydra_lib_maps_to_list obj))))) match_value)) ((equal (car match_target) :string) (funcall (lambda (s) (list :scalar (list :str s))) match_value)))) (cadr match_target))) value)))

(defvar hydra_json_yaml_encode_to_yaml (lambda (types) (lambda (tname) (lambda (typ) (lambda (term) (funcall (hydra_lib_eithers_map (lambda (v) (hydra_json_yaml_encode_json_to_yaml v))) (funcall (funcall (funcall (hydra_json_encode_to_json types) tname) typ) term)))))))

(provide 'hydra.json.yaml.encode)
