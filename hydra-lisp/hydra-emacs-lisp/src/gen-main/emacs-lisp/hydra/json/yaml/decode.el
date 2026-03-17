(require 'cl-lib)

(require 'hydra.ext.org.yaml.model)

(require 'hydra.json.decode)

(require 'hydra.json.model)

(require 'hydra.lib.eithers)

(require 'hydra.lib.literals)

(require 'hydra.lib.maps)

(require 'hydra.lib.pairs)

(defvar hydra_json_yaml_decode_yaml_to_json (lambda (node) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :mapping) ((lambda (m) (let ((convert_entry (lambda (kv) (let ((key_node (hydra_lib_pairs_first kv))) (let ((val_node (hydra_lib_pairs_second kv))) (let ((key_result ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :scalar) ((lambda (s) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :str) ((lambda (str) (list :right str)) match_value)) (t (list :left "non-string YAML mapping key")))) (cadr match_target))) s)) match_value)) (t (list :left "non-scalar YAML mapping key")))) (cadr match_target))) key_node))) (((hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (key) (let ((val_result (hydra_json_yaml_decode_yaml_to_json val_node))) ((hydra_lib_eithers_map (lambda (v) (list key v))) val_result)))) key_result))))))) (let ((entries ((hydra_lib_eithers_map_list convert_entry) (hydra_lib_maps_to_list m)))) ((hydra_lib_eithers_map (lambda (es) (list :object (hydra_lib_maps_from_list es)))) entries)))) match_value)) ((equal (car match_target) :scalar) ((lambda (s) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :bool) ((lambda (b) (list :right (list :boolean b))) match_value)) ((equal (car match_target) :float) ((lambda (f) (list :right (list :number f))) match_value)) ((equal (car match_target) :int) ((lambda (i) (list :right (list :number (hydra_lib_literals_bigint_to_bigfloat i)))) match_value)) ((equal (car match_target) :null) ((lambda (_) (list :right (list :null nil))) match_value)) ((equal (car match_target) :str) ((lambda (str) (list :right (list :string str))) match_value)))) (cadr match_target))) s)) match_value)) ((equal (car match_target) :sequence) ((lambda (nodes) (let ((results ((hydra_lib_eithers_map_list (lambda (n) (hydra_json_yaml_decode_yaml_to_json n))) nodes))) ((hydra_lib_eithers_map (lambda (vs) (list :array vs))) results))) match_value)))) (cadr match_target))) node)))

(defvar hydra_json_yaml_decode_from_yaml (lambda (types) (lambda (tname) (lambda (typ) (lambda (node) (let ((json_result (hydra_json_yaml_decode_yaml_to_json node))) (((hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (json) ((((hydra_json_decode_from_json types) tname) typ) json))) json_result)))))))

(provide 'hydra.json.yaml.decode)
