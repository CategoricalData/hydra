(ns hydra.json.schema
  (:require [hydra.json.model :refer :all]
))

(declare hydra_json_schema_restriction-variants hydra_json_schema_type-variants hydra_json_schema_type_name-variants hydra_json_schema_string_restriction-variants hydra_json_schema_numeric_restriction-variants hydra_json_schema_array_restriction-variants hydra_json_schema_items-variants hydra_json_schema_additional_items-variants hydra_json_schema_object_restriction-variants hydra_json_schema_schema_or_array-variants hydra_json_schema_multiple_restriction-variants)

(defrecord hydra_json_schema_document [id definitions root])
(defn make-hydra_json_schema_document [id definitions root] (->hydra_json_schema_document id definitions root))

(defrecord hydra_json_schema_keyword [value])
(defn make-hydra_json_schema_keyword [value] (->hydra_json_schema_keyword value))

(defrecord hydra_json_schema_schema [value])
(defn make-hydra_json_schema_schema [value] (->hydra_json_schema_schema value))

(def hydra_json_schema_restriction-variants (list :type :string :number :array :object :multiple :reference :title :description))

(def hydra_json_schema_type-variants (list :single :multiple))

(def hydra_json_schema_type_name-variants (list :string :integer :number :boolean :null :array :object))

(def hydra_json_schema_string_restriction-variants (list :min_length :max_length :pattern))

(defrecord hydra_json_schema_regular_expression [value])
(defn make-hydra_json_schema_regular_expression [value] (->hydra_json_schema_regular_expression value))

(def hydra_json_schema_numeric_restriction-variants (list :minimum :maximum :multiple_of))

(defrecord hydra_json_schema_limit [value exclusive])
(defn make-hydra_json_schema_limit [value exclusive] (->hydra_json_schema_limit value exclusive))

(def hydra_json_schema_array_restriction-variants (list :items :additional_items :min_items :max_items :unique_items))

(def hydra_json_schema_items-variants (list :same_items :var_items))

(def hydra_json_schema_additional_items-variants (list :any :schema))

(def hydra_json_schema_object_restriction-variants (list :properties :additional_properties :required :min_properties :max_properties :dependencies :pattern_properties))

(def hydra_json_schema_schema_or_array-variants (list :schema :array))

(def hydra_json_schema_multiple_restriction-variants (list :all_of :any_of :one_of :not :enum))

(defrecord hydra_json_schema_schema_reference [value])
(defn make-hydra_json_schema_schema_reference [value] (->hydra_json_schema_schema_reference value))
