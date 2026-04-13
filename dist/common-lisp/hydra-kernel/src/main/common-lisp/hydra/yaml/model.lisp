(defpackage :hydra.yaml.model
(:use :cl)
(:export :hydra_yaml_model_node-variants :hydra_yaml_model_scalar-variants))

(in-package :hydra.yaml.model)

(cl:defvar hydra_yaml_model_node-variants (cl:list :mapping :scalar :sequence))

(cl:defvar hydra_yaml_model_scalar-variants (cl:list :bool :float :int :null :str))
