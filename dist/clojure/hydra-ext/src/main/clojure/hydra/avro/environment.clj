(ns hydra.avro.environment
  (:require [hydra.avro.schema :refer :all] [hydra.coders :refer :all] [hydra.core :refer :all] [hydra.json.model :refer :all]
))

(defrecord hydra_avro_environment_avro_qualified_name [namespace name])
(defn make-hydra_avro_environment_avro_qualified_name [namespace name] (->hydra_avro_environment_avro_qualified_name namespace name))

(defrecord hydra_avro_environment_avro_foreign_key [type_name constructor])
(defn make-hydra_avro_environment_avro_foreign_key [type_name constructor] (->hydra_avro_environment_avro_foreign_key type_name constructor))

(defrecord hydra_avro_environment_avro_primary_key [field_name constructor])
(defn make-hydra_avro_environment_avro_primary_key [field_name constructor] (->hydra_avro_environment_avro_primary_key field_name constructor))

(defrecord hydra_avro_environment_avro_environment [named_adapters namespace elements])
(defn make-hydra_avro_environment_avro_environment [named_adapters namespace elements] (->hydra_avro_environment_avro_environment named_adapters namespace elements))

(defrecord hydra_avro_environment_encode_environment [type_map emitted])
(defn make-hydra_avro_environment_encode_environment [type_map emitted] (->hydra_avro_environment_encode_environment type_map emitted))
