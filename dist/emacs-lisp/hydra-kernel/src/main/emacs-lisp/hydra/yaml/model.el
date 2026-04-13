(require 'cl-lib)

(defvar hydra_yaml_model_node-variants (list :mapping :scalar :sequence))

(defvar hydra_yaml_model_scalar-variants (list :bool :float :int :null :str))

(provide 'hydra.yaml.model)
