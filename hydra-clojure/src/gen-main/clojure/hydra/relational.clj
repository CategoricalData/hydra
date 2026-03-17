(ns hydra.relational)

(defrecord hydra_relational_column_name [value])
(defn make-hydra_relational_column_name [value] (->hydra_relational_column_name value))

(defrecord hydra_relational_column_schema [name domain])
(defn make-hydra_relational_column_schema [name domain] (->hydra_relational_column_schema name domain))

(defrecord hydra_relational_foreign_key [foreign_relation keys])
(defn make-hydra_relational_foreign_key [foreign_relation keys] (->hydra_relational_foreign_key foreign_relation keys))

(defrecord hydra_relational_primary_key [value])
(defn make-hydra_relational_primary_key [value] (->hydra_relational_primary_key value))

(defrecord hydra_relational_relation [value])
(defn make-hydra_relational_relation [value] (->hydra_relational_relation value))

(defrecord hydra_relational_relation_name [value])
(defn make-hydra_relational_relation_name [value] (->hydra_relational_relation_name value))

(defrecord hydra_relational_relation_schema [name columns primary_keys foreign_keys])
(defn make-hydra_relational_relation_schema [name columns primary_keys foreign_keys] (->hydra_relational_relation_schema name columns primary_keys foreign_keys))

(defrecord hydra_relational_relationship [value])
(defn make-hydra_relational_relationship [value] (->hydra_relational_relationship value))

(defrecord hydra_relational_row [value])
(defn make-hydra_relational_row [value] (->hydra_relational_row value))
