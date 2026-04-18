(ns hydra.typeScript.model)

(declare hydra_type_script_model_primitive_type-variants hydra_type_script_model_type-variants)

(defrecord hydra_type_script_model_function_type [parameters range])
(defn make-hydra_type_script_model_function_type [parameters range] (->hydra_type_script_model_function_type parameters range))

(defrecord hydra_type_script_model_parameter [name type])
(defn make-hydra_type_script_model_parameter [name type] (->hydra_type_script_model_parameter name type))

(def hydra_type_script_model_primitive_type-variants (list :bigint :boolean :null :number :object :string :symbol :undefined))

(def hydra_type_script_model_type-variants (list :array :function :never :object_literal :primitive :tuple :unknown :void))
