(ns hydra.dsl.yaml.model
  (:require [hydra.core :refer :all] [hydra.phantoms :refer :all]
))

(declare hydra_dsl_yaml_model_node_mapping hydra_dsl_yaml_model_node_scalar hydra_dsl_yaml_model_node_sequence hydra_dsl_yaml_model_scalar_bool hydra_dsl_yaml_model_scalar_decimal hydra_dsl_yaml_model_scalar_float hydra_dsl_yaml_model_scalar_int hydra_dsl_yaml_model_scalar_null hydra_dsl_yaml_model_scalar_str)

(def hydra_dsl_yaml_model_node_mapping (fn [x] (list :inject (->hydra_core_injection "hydra.yaml.model.Node" (->hydra_core_field "mapping" ((fn [v] v) x))))))

(def hydra_dsl_yaml_model_node_scalar (fn [x] (list :inject (->hydra_core_injection "hydra.yaml.model.Node" (->hydra_core_field "scalar" ((fn [v] v) x))))))

(def hydra_dsl_yaml_model_node_sequence (fn [x] (list :inject (->hydra_core_injection "hydra.yaml.model.Node" (->hydra_core_field "sequence" ((fn [v] v) x))))))

(def hydra_dsl_yaml_model_scalar_bool (fn [x] (list :inject (->hydra_core_injection "hydra.yaml.model.Scalar" (->hydra_core_field "bool" ((fn [v] v) x))))))

(def hydra_dsl_yaml_model_scalar_decimal (fn [x] (list :inject (->hydra_core_injection "hydra.yaml.model.Scalar" (->hydra_core_field "decimal" ((fn [v] v) x))))))

(def hydra_dsl_yaml_model_scalar_float (fn [x] (list :inject (->hydra_core_injection "hydra.yaml.model.Scalar" (->hydra_core_field "float" ((fn [v] v) x))))))

(def hydra_dsl_yaml_model_scalar_int (fn [x] (list :inject (->hydra_core_injection "hydra.yaml.model.Scalar" (->hydra_core_field "int" ((fn [v] v) x))))))

(def hydra_dsl_yaml_model_scalar_null (list :inject (->hydra_core_injection "hydra.yaml.model.Scalar" (->hydra_core_field "null" (list :unit nil)))))

(def hydra_dsl_yaml_model_scalar_str (fn [x] (list :inject (->hydra_core_injection "hydra.yaml.model.Scalar" (->hydra_core_field "str" ((fn [v] v) x))))))
