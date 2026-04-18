(ns hydra.dsl.json.model
  (:require [hydra.core :refer :all] [hydra.phantoms :refer :all]
))

(declare hydra_dsl_json_model_value_array hydra_dsl_json_model_value_boolean hydra_dsl_json_model_value_null hydra_dsl_json_model_value_number hydra_dsl_json_model_value_object hydra_dsl_json_model_value_string)

(def hydra_dsl_json_model_value_array (fn [x] (list :inject (->hydra_core_injection "hydra.json.model.Value" (->hydra_core_field "array" ((fn [v] v) x))))))

(def hydra_dsl_json_model_value_boolean (fn [x] (list :inject (->hydra_core_injection "hydra.json.model.Value" (->hydra_core_field "boolean" ((fn [v] v) x))))))

(def hydra_dsl_json_model_value_null (list :inject (->hydra_core_injection "hydra.json.model.Value" (->hydra_core_field "null" (list :unit nil)))))

(def hydra_dsl_json_model_value_number (fn [x] (list :inject (->hydra_core_injection "hydra.json.model.Value" (->hydra_core_field "number" ((fn [v] v) x))))))

(def hydra_dsl_json_model_value_object (fn [x] (list :inject (->hydra_core_injection "hydra.json.model.Value" (->hydra_core_field "object" ((fn [v] v) x))))))

(def hydra_dsl_json_model_value_string (fn [x] (list :inject (->hydra_core_injection "hydra.json.model.Value" (->hydra_core_field "string" ((fn [v] v) x))))))
