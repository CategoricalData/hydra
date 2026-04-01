(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.encode.core)

(require 'hydra.encode.relational)

(require 'hydra.lib.lists)

(require 'hydra.lib.maybes)

(require 'hydra.tabular)

(defvar hydra_encode_tabular_column_type (lambda (x) (list :record (make-hydra_core_record "hydra.tabular.ColumnType" (list (make-hydra_core_field "name" (hydra_encode_relational_column_name (funcall (lambda (v) (hydra_tabular_column_type-name v)) x))) (make-hydra_core_field "type" (hydra_encode_core_type (funcall (lambda (v) (hydra_tabular_column_type-type v)) x))))))))

(defvar hydra_encode_tabular_data_row (lambda (v) (lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.tabular.DataRow" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map (lambda (opt) (list :maybe (funcall (hydra_lib_maybes_map v) opt)))) xs))) (funcall (lambda (v) v) x)))))))

(defvar hydra_encode_tabular_header_row (lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.tabular.HeaderRow" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map (lambda (x2) (list :literal (list :string x2)))) xs))) (funcall (lambda (v) v) x))))))

(defvar hydra_encode_tabular_table (lambda (v) (lambda (x) (list :record (make-hydra_core_record "hydra.tabular.Table" (list (make-hydra_core_field "header" (funcall (lambda (opt) (list :maybe (funcall (hydra_lib_maybes_map hydra_encode_tabular_header_row) opt))) (funcall (lambda (v) (hydra_tabular_table-header v)) x))) (make-hydra_core_field "data" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map (hydra_encode_tabular_data_row v)) xs))) (funcall (lambda (v) (hydra_tabular_table-data v)) x)))))))))

(defvar hydra_encode_tabular_table_type (lambda (x) (list :record (make-hydra_core_record "hydra.tabular.TableType" (list (make-hydra_core_field "name" (hydra_encode_relational_relation_name (funcall (lambda (v) (hydra_tabular_table_type-name v)) x))) (make-hydra_core_field "columns" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map hydra_encode_tabular_column_type) xs))) (funcall (lambda (v) (hydra_tabular_table_type-columns v)) x))))))))

(provide 'hydra.encode.tabular)
