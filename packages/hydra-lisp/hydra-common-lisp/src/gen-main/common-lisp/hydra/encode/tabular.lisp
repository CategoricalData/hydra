(defpackage :hydra.encode.tabular
(:use :cl :hydra.core :hydra.encode.core :hydra.encode.relational :hydra.lib.lists :hydra.lib.maybes :hydra.tabular)
(:export :hydra_encode_tabular_column_type :hydra_encode_tabular_data_row :hydra_encode_tabular_header_row :hydra_encode_tabular_table :hydra_encode_tabular_table_type))

(in-package :hydra.encode.tabular)

(cl:defvar hydra_encode_tabular_column_type (cl:lambda (x) (list :record (make-hydra_core_record "hydra.tabular.ColumnType" (cl:list (make-hydra_core_field "name" (hydra_encode_relational_column_name ((cl:lambda (v) (hydra_tabular_column_type-name v)) x))) (make-hydra_core_field "type" (hydra_encode_core_type ((cl:lambda (v) (hydra_tabular_column_type-type v)) x))))))))

(cl:defvar hydra_encode_tabular_data_row (cl:lambda (v) (cl:lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.tabular.DataRow" ((cl:lambda (xs) (list :list ((hydra_lib_lists_map (cl:lambda (opt) (list :maybe ((hydra_lib_maybes_map v) opt)))) xs))) ((cl:lambda (v) v) x)))))))

(cl:defvar hydra_encode_tabular_header_row (cl:lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.tabular.HeaderRow" ((cl:lambda (xs) (list :list ((hydra_lib_lists_map (cl:lambda (x2) (list :literal (list :string x2)))) xs))) ((cl:lambda (v) v) x))))))

(cl:defvar hydra_encode_tabular_table (cl:lambda (v) (cl:lambda (x) (list :record (make-hydra_core_record "hydra.tabular.Table" (cl:list (make-hydra_core_field "header" ((cl:lambda (opt) (list :maybe ((hydra_lib_maybes_map hydra_encode_tabular_header_row) opt))) ((cl:lambda (v) (hydra_tabular_table-header v)) x))) (make-hydra_core_field "data" ((cl:lambda (xs) (list :list ((hydra_lib_lists_map (hydra_encode_tabular_data_row v)) xs))) ((cl:lambda (v) (hydra_tabular_table-data v)) x)))))))))

(cl:defvar hydra_encode_tabular_table_type (cl:lambda (x) (list :record (make-hydra_core_record "hydra.tabular.TableType" (cl:list (make-hydra_core_field "name" (hydra_encode_relational_relation_name ((cl:lambda (v) (hydra_tabular_table_type-name v)) x))) (make-hydra_core_field "columns" ((cl:lambda (xs) (list :list ((hydra_lib_lists_map hydra_encode_tabular_column_type) xs))) ((cl:lambda (v) (hydra_tabular_table_type-columns v)) x))))))))
