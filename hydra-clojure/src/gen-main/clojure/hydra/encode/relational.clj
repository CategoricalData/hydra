(ns hydra.encode.relational
  (:require [hydra.core :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.maps :refer :all] [hydra.lib.sets :refer :all] [hydra.relational :refer :all]
))

(declare hydra_encode_relational_column_name hydra_encode_relational_column_schema hydra_encode_relational_relation_name hydra_encode_relational_foreign_key hydra_encode_relational_primary_key hydra_encode_relational_row hydra_encode_relational_relation hydra_encode_relational_relation_schema hydra_encode_relational_relationship)

(def hydra_encode_relational_column_name (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.relational.ColumnName" ((fn [x] (list :literal (list :string x))) ((fn [v] v) x))))))

(def hydra_encode_relational_column_schema (fn [t_] (fn [x] (list :record (->hydra_core_record "hydra.relational.ColumnSchema" (list (->hydra_core_field "name" (hydra_encode_relational_column_name ((fn [v] (:name v)) x))) (->hydra_core_field "domain" (t_ ((fn [v] (:domain v)) x)))))))))

(def hydra_encode_relational_relation_name (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.relational.RelationName" ((fn [x] (list :literal (list :string x))) ((fn [v] v) x))))))

(def hydra_encode_relational_foreign_key (fn [x] (list :record (->hydra_core_record "hydra.relational.ForeignKey" (list (->hydra_core_field "foreignRelation" (hydra_encode_relational_relation_name ((fn [v] (:foreign_relation v)) x))) (->hydra_core_field "keys" ((fn [m] (list :map (((hydra_lib_maps_bimap hydra_encode_relational_column_name) hydra_encode_relational_column_name) m))) ((fn [v] (:keys v)) x))))))))

(def hydra_encode_relational_primary_key (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.relational.PrimaryKey" ((fn [xs] (list :list ((hydra_lib_lists_map hydra_encode_relational_column_name) xs))) ((fn [v] v) x))))))

(def hydra_encode_relational_row (fn [v] (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.relational.Row" ((fn [xs] (list :list ((hydra_lib_lists_map v) xs))) ((fn [v] v) x)))))))

(def hydra_encode_relational_relation (fn [v] (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.relational.Relation" ((fn [xs] (list :list ((hydra_lib_lists_map (hydra_encode_relational_row v)) xs))) ((fn [v] v) x)))))))

(def hydra_encode_relational_relation_schema (fn [t_] (fn [x] (list :record (->hydra_core_record "hydra.relational.RelationSchema" (list (->hydra_core_field "name" (hydra_encode_relational_relation_name ((fn [v] (:name v)) x))) (->hydra_core_field "columns" ((fn [xs] (list :list ((hydra_lib_lists_map (hydra_encode_relational_column_schema t_)) xs))) ((fn [v] (:columns v)) x))) (->hydra_core_field "primaryKeys" ((fn [xs] (list :list ((hydra_lib_lists_map hydra_encode_relational_primary_key) xs))) ((fn [v] (:primary_keys v)) x))) (->hydra_core_field "foreignKeys" ((fn [xs] (list :list ((hydra_lib_lists_map hydra_encode_relational_foreign_key) xs))) ((fn [v] (:foreign_keys v)) x)))))))))

(def hydra_encode_relational_relationship (fn [v] (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.relational.Relationship" ((fn [s] (list :set ((hydra_lib_sets_map (fn [m] (list :map (((hydra_lib_maps_bimap hydra_encode_relational_column_name) v) m)))) s))) ((fn [v] v) x)))))))
