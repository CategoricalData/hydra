(define-library (hydra tabular)
(export make-hydra_tabular_column_type hydra_tabular_column_type? hydra_tabular_column_type-name hydra_tabular_column_type-type make-hydra_tabular_data_row hydra_tabular_data_row? hydra_tabular_data_row-value make-hydra_tabular_header_row hydra_tabular_header_row? hydra_tabular_header_row-value make-hydra_tabular_table hydra_tabular_table? hydra_tabular_table-header hydra_tabular_table-data make-hydra_tabular_table_type hydra_tabular_table_type? hydra_tabular_table_type-name hydra_tabular_table_type-columns)
(import (scheme base) (hydra core) (hydra relational))
(begin
(define-record-type hydra_tabular_column_type (make-hydra_tabular_column_type name type) hydra_tabular_column_type? (name hydra_tabular_column_type-name) (type hydra_tabular_column_type-type))
(define-record-type hydra_tabular_data_row (make-hydra_tabular_data_row value) hydra_tabular_data_row? (value hydra_tabular_data_row-value))
(define-record-type hydra_tabular_header_row (make-hydra_tabular_header_row value) hydra_tabular_header_row? (value hydra_tabular_header_row-value))
(define-record-type hydra_tabular_table (make-hydra_tabular_table header data) hydra_tabular_table? (header hydra_tabular_table-header) (data hydra_tabular_table-data))
(define-record-type hydra_tabular_table_type (make-hydra_tabular_table_type name columns) hydra_tabular_table_type? (name hydra_tabular_table_type-name) (columns hydra_tabular_table_type-columns))))
