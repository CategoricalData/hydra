(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.json.model)

(require 'hydra.lib.lists)

(require 'hydra.lib.maps)

(defvar hydra_encode_json_model_value (lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :array) ((lambda (y) (list :union (make-hydra_core_injection "hydra.json.model.Value" (make-hydra_core_field "array" ((lambda (xs) (list :list ((hydra_lib_lists_map hydra_encode_json_model_value) xs))) y))))) match_value)) ((equal (car match_target) :boolean) ((lambda (y) (list :union (make-hydra_core_injection "hydra.json.model.Value" (make-hydra_core_field "boolean" ((lambda (x) (list :literal (list :boolean x))) y))))) match_value)) ((equal (car match_target) :null) ((lambda (y) (list :union (make-hydra_core_injection "hydra.json.model.Value" (make-hydra_core_field "null" ((lambda (_) (list :unit nil)) y))))) match_value)) ((equal (car match_target) :number) ((lambda (y) (list :union (make-hydra_core_injection "hydra.json.model.Value" (make-hydra_core_field "number" ((lambda (x) (list :literal (list :float (list :bigfloat x)))) y))))) match_value)) ((equal (car match_target) :object) ((lambda (y) (list :union (make-hydra_core_injection "hydra.json.model.Value" (make-hydra_core_field "object" ((lambda (m) (list :map (((hydra_lib_maps_bimap (lambda (x) (list :literal (list :string x)))) hydra_encode_json_model_value) m))) y))))) match_value)) ((equal (car match_target) :string) ((lambda (y) (list :union (make-hydra_core_injection "hydra.json.model.Value" (make-hydra_core_field "string" ((lambda (x) (list :literal (list :string x))) y))))) match_value)))) (cadr match_target))))

(provide 'hydra.encode.json.model)
