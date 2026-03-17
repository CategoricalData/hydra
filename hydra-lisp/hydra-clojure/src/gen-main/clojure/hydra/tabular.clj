(ns hydra.tabular
  (:require [hydra.core :refer :all] [hydra.relational :refer :all]
))

(defrecord hydra_tabular_column_type [name type])
(defn make-hydra_tabular_column_type [name type] (->hydra_tabular_column_type name type))

(defrecord hydra_tabular_data_row [value])
(defn make-hydra_tabular_data_row [value] (->hydra_tabular_data_row value))

(defrecord hydra_tabular_header_row [value])
(defn make-hydra_tabular_header_row [value] (->hydra_tabular_header_row value))

(defrecord hydra_tabular_table [header data])
(defn make-hydra_tabular_table [header data] (->hydra_tabular_table header data))

(defrecord hydra_tabular_table_type [name columns])
(defn make-hydra_tabular_table_type [name columns] (->hydra_tabular_table_type name columns))
