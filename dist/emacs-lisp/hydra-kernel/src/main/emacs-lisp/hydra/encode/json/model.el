(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.json.model)

(require 'hydra.lib.lists)

(require 'hydra.lib.maps)

(defvar hydra_encode_json_model_value (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :array) (funcall (lambda (y) (list :inject (make-hydra_core_injection "hydra.json.model.Value" (make-hydra_core_field "array" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map hydra_encode_json_model_value) xs))) y))))) match_value)) ((equal (car match_target) :boolean) (funcall (lambda (y) (list :inject (make-hydra_core_injection "hydra.json.model.Value" (make-hydra_core_field "boolean" (funcall (lambda (x) (list :literal (list :boolean x))) y))))) match_value)) ((equal (car match_target) :null) (funcall (lambda (y) (list :inject (make-hydra_core_injection "hydra.json.model.Value" (make-hydra_core_field "null" (funcall (lambda (_) (list :unit nil)) y))))) match_value)) ((equal (car match_target) :number) (funcall (lambda (y) (list :inject (make-hydra_core_injection "hydra.json.model.Value" (make-hydra_core_field "number" (funcall (lambda (x) (list :literal (list :decimal x))) y))))) match_value)) ((equal (car match_target) :object) (funcall (lambda (y) (list :inject (make-hydra_core_injection "hydra.json.model.Value" (make-hydra_core_field "object" (funcall (lambda (m) (list :map (funcall (funcall (hydra_lib_maps_bimap (lambda (x) (list :literal (list :string x)))) hydra_encode_json_model_value) m))) y))))) match_value)) ((equal (car match_target) :string) (funcall (lambda (y) (list :inject (make-hydra_core_injection "hydra.json.model.Value" (make-hydra_core_field "string" (funcall (lambda (x) (list :literal (list :string x))) y))))) match_value)))) (cadr match_target))))

(provide 'hydra.encode.json.model)
