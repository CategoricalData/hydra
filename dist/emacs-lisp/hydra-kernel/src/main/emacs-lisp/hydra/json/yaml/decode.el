(require 'cl-lib)

(require 'hydra.json.decode)

(require 'hydra.json.model)

(require 'hydra.lib.eithers)

(require 'hydra.lib.literals)

(require 'hydra.lib.maps)

(require 'hydra.lib.pairs)

(require 'hydra.yaml.model)

(defvar hydra_json_yaml_decode_yaml_to_json (lambda (node) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :mapping) (funcall (lambda (m) (let ((convert_entry (lambda (kv) (let ((key_node (hydra_lib_pairs_first kv))) (let ((val_node (hydra_lib_pairs_second kv))) (let ((key_result (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :scalar) (funcall (lambda (s) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :str) (funcall (lambda (str) (list :right str)) match_value)) (t (list :left "non-string YAML mapping key")))) (cadr match_target))) s)) match_value)) (t (list :left "non-scalar YAML mapping key")))) (cadr match_target))) key_node))) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (key) (let ((val_result (hydra_json_yaml_decode_yaml_to_json val_node))) (funcall (hydra_lib_eithers_map (lambda (v) (list key v))) val_result)))) key_result))))))) (let ((entries (funcall (hydra_lib_eithers_map_list convert_entry) (hydra_lib_maps_to_list m)))) (funcall (hydra_lib_eithers_map (lambda (es) (list :object (hydra_lib_maps_from_list es)))) entries)))) match_value)) ((equal (car match_target) :scalar) (funcall (lambda (s) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :bool) (funcall (lambda (b) (list :right (list :boolean b))) match_value)) ((equal (car match_target) :decimal) (funcall (lambda (d) (list :right (list :number d))) match_value)) ((equal (car match_target) :float) (funcall (lambda (f) (list :right (list :number (hydra_lib_literals_float64_to_decimal (hydra_lib_literals_bigfloat_to_float64 f))))) match_value)) ((equal (car match_target) :int) (funcall (lambda (i) (list :right (list :number (hydra_lib_literals_bigint_to_decimal i)))) match_value)) ((equal (car match_target) :null) (funcall (lambda (_) (list :right (list :null nil))) match_value)) ((equal (car match_target) :str) (funcall (lambda (str) (list :right (list :string str))) match_value)))) (cadr match_target))) s)) match_value)) ((equal (car match_target) :sequence) (funcall (lambda (nodes) (let ((results (funcall (hydra_lib_eithers_map_list (lambda (n) (hydra_json_yaml_decode_yaml_to_json n))) nodes))) (funcall (hydra_lib_eithers_map (lambda (vs) (list :array vs))) results))) match_value)))) (cadr match_target))) node)))

(defvar hydra_json_yaml_decode_from_yaml (lambda (types) (lambda (tname) (lambda (typ) (lambda (node) (let ((json_result (hydra_json_yaml_decode_yaml_to_json node))) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (json) (funcall (funcall (funcall (hydra_json_decode_from_json types) tname) typ) json))) json_result)))))))

(provide 'hydra.json.yaml.decode)
