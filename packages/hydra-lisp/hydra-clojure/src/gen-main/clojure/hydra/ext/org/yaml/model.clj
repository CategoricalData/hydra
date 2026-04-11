(ns hydra.ext.org.yaml.model)

(declare hydra_ext_org_yaml_model_node-variants hydra_ext_org_yaml_model_scalar-variants)

(def hydra_ext_org_yaml_model_node-variants (list :mapping :scalar :sequence))

(def hydra_ext_org_yaml_model_scalar-variants (list :bool :float :int :null :str))
