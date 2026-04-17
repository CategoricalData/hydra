(ns hydra.yaml.model)

(declare hydra_yaml_model_node-variants hydra_yaml_model_scalar-variants)

(def hydra_yaml_model_node-variants (list :mapping :scalar :sequence))

(def hydra_yaml_model_scalar-variants (list :bool :decimal :float :int :null :str))
