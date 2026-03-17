(defpackage :hydra.tabular
(:use :cl :hydra.core :hydra.relational)
(:export :make-hydra_tabular_column_type :hydra_tabular_column_type? :hydra_tabular_column_type-name :hydra_tabular_column_type-type :make-hydra_tabular_data_row :hydra_tabular_data_row? :hydra_tabular_data_row-value :make-hydra_tabular_header_row :hydra_tabular_header_row? :hydra_tabular_header_row-value :make-hydra_tabular_table :hydra_tabular_table? :hydra_tabular_table-header :hydra_tabular_table-data :make-hydra_tabular_table_type :hydra_tabular_table_type? :hydra_tabular_table_type-name :hydra_tabular_table_type-columns))

(in-package :hydra.tabular)

(cl:defstruct hydra_tabular_column_type name type)

(cl:defstruct hydra_tabular_data_row value)

(cl:defstruct hydra_tabular_header_row value)

(cl:defstruct hydra_tabular_table header data)

(cl:defstruct hydra_tabular_table_type name columns)
