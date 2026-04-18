(ns hydra.pegasus.pdl
  (:require [hydra.json.model :refer :all]
))

(declare hydra_pegasus_pdl_named_schema_type-variants hydra_pegasus_pdl_primitive_type-variants hydra_pegasus_pdl_schema-variants)

(defrecord hydra_pegasus_pdl_annotations [doc deprecated])
(defn make-hydra_pegasus_pdl_annotations [doc deprecated] (->hydra_pegasus_pdl_annotations doc deprecated))

(defrecord hydra_pegasus_pdl_enum_field [name annotations])
(defn make-hydra_pegasus_pdl_enum_field [name annotations] (->hydra_pegasus_pdl_enum_field name annotations))

(defrecord hydra_pegasus_pdl_enum_field_name [value])
(defn make-hydra_pegasus_pdl_enum_field_name [value] (->hydra_pegasus_pdl_enum_field_name value))

(defrecord hydra_pegasus_pdl_enum_schema [fields])
(defn make-hydra_pegasus_pdl_enum_schema [fields] (->hydra_pegasus_pdl_enum_schema fields))

(defrecord hydra_pegasus_pdl_field_name [value])
(defn make-hydra_pegasus_pdl_field_name [value] (->hydra_pegasus_pdl_field_name value))

(defrecord hydra_pegasus_pdl_named_schema [qualified_name type annotations])
(defn make-hydra_pegasus_pdl_named_schema [qualified_name type annotations] (->hydra_pegasus_pdl_named_schema qualified_name type annotations))

(def hydra_pegasus_pdl_named_schema_type-variants (list :record :enum :typeref))

(defrecord hydra_pegasus_pdl_name [value])
(defn make-hydra_pegasus_pdl_name [value] (->hydra_pegasus_pdl_name value))

(defrecord hydra_pegasus_pdl_namespace [value])
(defn make-hydra_pegasus_pdl_namespace [value] (->hydra_pegasus_pdl_namespace value))

(defrecord hydra_pegasus_pdl_package [value])
(defn make-hydra_pegasus_pdl_package [value] (->hydra_pegasus_pdl_package value))

(def hydra_pegasus_pdl_primitive_type-variants (list :boolean :bytes :double :float :int :long :string))

(defrecord hydra_pegasus_pdl_property_key [value])
(defn make-hydra_pegasus_pdl_property_key [value] (->hydra_pegasus_pdl_property_key value))

(defrecord hydra_pegasus_pdl_property [key value])
(defn make-hydra_pegasus_pdl_property [key value] (->hydra_pegasus_pdl_property key value))

(defrecord hydra_pegasus_pdl_qualified_name [name namespace])
(defn make-hydra_pegasus_pdl_qualified_name [name namespace] (->hydra_pegasus_pdl_qualified_name name namespace))

(defrecord hydra_pegasus_pdl_record_field [name value optional default annotations])
(defn make-hydra_pegasus_pdl_record_field [name value optional default annotations] (->hydra_pegasus_pdl_record_field name value optional default annotations))

(defrecord hydra_pegasus_pdl_record_schema [fields includes])
(defn make-hydra_pegasus_pdl_record_schema [fields includes] (->hydra_pegasus_pdl_record_schema fields includes))

(def hydra_pegasus_pdl_schema-variants (list :array :fixed :inline :map :named :null :primitive :union))

(defrecord hydra_pegasus_pdl_schema_file [namespace package imports schemas])
(defn make-hydra_pegasus_pdl_schema_file [namespace package imports schemas] (->hydra_pegasus_pdl_schema_file namespace package imports schemas))

(defrecord hydra_pegasus_pdl_union_member [alias value annotations])
(defn make-hydra_pegasus_pdl_union_member [alias value annotations] (->hydra_pegasus_pdl_union_member alias value annotations))

(defrecord hydra_pegasus_pdl_union_schema [value])
(defn make-hydra_pegasus_pdl_union_schema [value] (->hydra_pegasus_pdl_union_schema value))
