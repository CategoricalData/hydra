(ns hydra.encode.tabular
  (:require [hydra.core :refer :all] [hydra.encode.core :refer :all] [hydra.encode.relational :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.maybes :refer :all] [hydra.tabular :refer :all]
))

(declare hydra_encode_tabular_column_type hydra_encode_tabular_data_row hydra_encode_tabular_header_row hydra_encode_tabular_table hydra_encode_tabular_table_type)

(def hydra_encode_tabular_column_type (fn [x] (list :record (->hydra_core_record "hydra.tabular.ColumnType" (list (->hydra_core_field "name" (hydra_encode_relational_column_name ((fn [v] (:name v)) x))) (->hydra_core_field "type" (hydra_encode_core_type ((fn [v] (:type v)) x))))))))

(def hydra_encode_tabular_data_row (fn [v] (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.tabular.DataRow" ((fn [xs] (list :list ((hydra_lib_lists_map (fn [opt] (list :maybe ((hydra_lib_maybes_map v) opt)))) xs))) ((fn [v] v) x)))))))

(def hydra_encode_tabular_header_row (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.tabular.HeaderRow" ((fn [xs] (list :list ((hydra_lib_lists_map (fn [x] (list :literal (list :string x)))) xs))) ((fn [v] v) x))))))

(def hydra_encode_tabular_table (fn [v] (fn [x] (list :record (->hydra_core_record "hydra.tabular.Table" (list (->hydra_core_field "header" ((fn [opt] (list :maybe ((hydra_lib_maybes_map hydra_encode_tabular_header_row) opt))) ((fn [v] (:header v)) x))) (->hydra_core_field "data" ((fn [xs] (list :list ((hydra_lib_lists_map (hydra_encode_tabular_data_row v)) xs))) ((fn [v] (:data v)) x)))))))))

(def hydra_encode_tabular_table_type (fn [x] (list :record (->hydra_core_record "hydra.tabular.TableType" (list (->hydra_core_field "name" (hydra_encode_relational_relation_name ((fn [v] (:name v)) x))) (->hydra_core_field "columns" ((fn [xs] (list :list ((hydra_lib_lists_map hydra_encode_tabular_column_type) xs))) ((fn [v] (:columns v)) x))))))))
