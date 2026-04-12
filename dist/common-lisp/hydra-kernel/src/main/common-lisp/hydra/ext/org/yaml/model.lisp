(defpackage :hydra.ext.org.yaml.model
(:use :cl)
(:export :hydra_ext_org_yaml_model_node-variants :hydra_ext_org_yaml_model_scalar-variants))

(in-package :hydra.ext.org.yaml.model)

(cl:defvar hydra_ext_org_yaml_model_node-variants (cl:list :mapping :scalar :sequence))

(cl:defvar hydra_ext_org_yaml_model_scalar-variants (cl:list :bool :float :int :null :str))
