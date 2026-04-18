(ns hydra.avro.schema
  (:require [hydra.json.model :refer :all]
))

(declare hydra_avro_schema_named_type-variants hydra_avro_schema_order-variants hydra_avro_schema_primitive-variants hydra_avro_schema_schema-variants)

(defrecord hydra_avro_schema_array [items])
(defn make-hydra_avro_schema_array [items] (->hydra_avro_schema_array items))

(defrecord hydra_avro_schema_enum [symbols default])
(defn make-hydra_avro_schema_enum [symbols default] (->hydra_avro_schema_enum symbols default))

(defrecord hydra_avro_schema_field [name doc type default order aliases annotations])
(defn make-hydra_avro_schema_field [name doc type default order aliases annotations] (->hydra_avro_schema_field name doc type default order aliases annotations))

(defrecord hydra_avro_schema_fixed [size])
(defn make-hydra_avro_schema_fixed [size] (->hydra_avro_schema_fixed size))

(defrecord hydra_avro_schema_map [values])
(defn make-hydra_avro_schema_map [values] (->hydra_avro_schema_map values))

(defrecord hydra_avro_schema_named [name namespace aliases doc type annotations])
(defn make-hydra_avro_schema_named [name namespace aliases doc type annotations] (->hydra_avro_schema_named name namespace aliases doc type annotations))

(def hydra_avro_schema_named_type-variants (list :enum :fixed :record))

(def hydra_avro_schema_order-variants (list :ascending :descending :ignore))

(def hydra_avro_schema_primitive-variants (list :null :boolean :int :long :float :double :bytes :string))

(defrecord hydra_avro_schema_record [fields])
(defn make-hydra_avro_schema_record [fields] (->hydra_avro_schema_record fields))

(def hydra_avro_schema_schema-variants (list :array :map :named :primitive :reference :union))

(defrecord hydra_avro_schema_union [value])
(defn make-hydra_avro_schema_union [value] (->hydra_avro_schema_union value))
