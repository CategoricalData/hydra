(ns hydra.dsl.tabular
  (:require [hydra.core :refer :all] [hydra.phantoms :refer :all]
))

(declare hydra_dsl_tabular_column_type hydra_dsl_tabular_column_type_name hydra_dsl_tabular_column_type_type hydra_dsl_tabular_column_type_with_name hydra_dsl_tabular_column_type_with_type hydra_dsl_tabular_data_row hydra_dsl_tabular_header_row hydra_dsl_tabular_table hydra_dsl_tabular_table_data hydra_dsl_tabular_table_header hydra_dsl_tabular_table_type hydra_dsl_tabular_table_type_columns hydra_dsl_tabular_table_type_name hydra_dsl_tabular_table_type_with_columns hydra_dsl_tabular_table_type_with_name hydra_dsl_tabular_table_with_data hydra_dsl_tabular_table_with_header hydra_dsl_tabular_un_data_row hydra_dsl_tabular_un_header_row)

(def hydra_dsl_tabular_column_type (fn [name] (fn [type] (list :record (->hydra_core_record "hydra.tabular.ColumnType" (list (->hydra_core_field "name" ((fn [v] v) name)) (->hydra_core_field "type" ((fn [v] v) type))))))))

(def hydra_dsl_tabular_column_type_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.tabular.ColumnType" "name")) ((fn [v] v) x)))))

(def hydra_dsl_tabular_column_type_type (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.tabular.ColumnType" "type")) ((fn [v] v) x)))))

(def hydra_dsl_tabular_column_type_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.tabular.ColumnType" (list (->hydra_core_field "name" ((fn [v] v) new_val)) (->hydra_core_field "type" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.tabular.ColumnType" "type")) ((fn [v] v) original))))))))))

(def hydra_dsl_tabular_column_type_with_type (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.tabular.ColumnType" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.tabular.ColumnType" "name")) ((fn [v] v) original)))) (->hydra_core_field "type" ((fn [v] v) new_val))))))))

(def hydra_dsl_tabular_data_row (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.tabular.DataRow" ((fn [v] v) x)))))

(def hydra_dsl_tabular_header_row (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.tabular.HeaderRow" ((fn [v] v) x)))))

(def hydra_dsl_tabular_table (fn [header] (fn [data] (list :record (->hydra_core_record "hydra.tabular.Table" (list (->hydra_core_field "header" ((fn [v] v) header)) (->hydra_core_field "data" ((fn [v] v) data))))))))

(def hydra_dsl_tabular_table_data (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.tabular.Table" "data")) ((fn [v] v) x)))))

(def hydra_dsl_tabular_table_header (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.tabular.Table" "header")) ((fn [v] v) x)))))

(def hydra_dsl_tabular_table_type (fn [name] (fn [columns] (list :record (->hydra_core_record "hydra.tabular.TableType" (list (->hydra_core_field "name" ((fn [v] v) name)) (->hydra_core_field "columns" ((fn [v] v) columns))))))))

(def hydra_dsl_tabular_table_type_columns (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.tabular.TableType" "columns")) ((fn [v] v) x)))))

(def hydra_dsl_tabular_table_type_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.tabular.TableType" "name")) ((fn [v] v) x)))))

(def hydra_dsl_tabular_table_type_with_columns (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.tabular.TableType" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.tabular.TableType" "name")) ((fn [v] v) original)))) (->hydra_core_field "columns" ((fn [v] v) new_val))))))))

(def hydra_dsl_tabular_table_type_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.tabular.TableType" (list (->hydra_core_field "name" ((fn [v] v) new_val)) (->hydra_core_field "columns" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.tabular.TableType" "columns")) ((fn [v] v) original))))))))))

(def hydra_dsl_tabular_table_with_data (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.tabular.Table" (list (->hydra_core_field "header" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.tabular.Table" "header")) ((fn [v] v) original)))) (->hydra_core_field "data" ((fn [v] v) new_val))))))))

(def hydra_dsl_tabular_table_with_header (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.tabular.Table" (list (->hydra_core_field "header" ((fn [v] v) new_val)) (->hydra_core_field "data" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.tabular.Table" "data")) ((fn [v] v) original))))))))))

(def hydra_dsl_tabular_un_data_row (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.tabular.DataRow") ((fn [v] v) x)))))

(def hydra_dsl_tabular_un_header_row (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.tabular.HeaderRow") ((fn [v] v) x)))))
