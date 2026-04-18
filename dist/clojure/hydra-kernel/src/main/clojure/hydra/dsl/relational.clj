(ns hydra.dsl.relational
  (:require [hydra.core :refer :all] [hydra.phantoms :refer :all]
))

(declare hydra_dsl_relational_column_name hydra_dsl_relational_column_schema hydra_dsl_relational_column_schema_domain hydra_dsl_relational_column_schema_name hydra_dsl_relational_column_schema_with_domain hydra_dsl_relational_column_schema_with_name hydra_dsl_relational_foreign_key hydra_dsl_relational_foreign_key_foreign_relation hydra_dsl_relational_foreign_key_keys hydra_dsl_relational_foreign_key_with_foreign_relation hydra_dsl_relational_foreign_key_with_keys hydra_dsl_relational_primary_key hydra_dsl_relational_relation hydra_dsl_relational_relation_name hydra_dsl_relational_relation_schema hydra_dsl_relational_relation_schema_columns hydra_dsl_relational_relation_schema_foreign_keys hydra_dsl_relational_relation_schema_name hydra_dsl_relational_relation_schema_primary_keys hydra_dsl_relational_relation_schema_with_columns hydra_dsl_relational_relation_schema_with_foreign_keys hydra_dsl_relational_relation_schema_with_name hydra_dsl_relational_relation_schema_with_primary_keys hydra_dsl_relational_relationship hydra_dsl_relational_row hydra_dsl_relational_un_column_name hydra_dsl_relational_un_primary_key hydra_dsl_relational_un_relation hydra_dsl_relational_un_relation_name hydra_dsl_relational_un_relationship hydra_dsl_relational_un_row)

(def hydra_dsl_relational_column_name (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.relational.ColumnName" ((fn [v] v) x)))))

(def hydra_dsl_relational_column_schema (fn [name] (fn [domain] (list :record (->hydra_core_record "hydra.relational.ColumnSchema" (list (->hydra_core_field "name" ((fn [v] v) name)) (->hydra_core_field "domain" ((fn [v] v) domain))))))))

(def hydra_dsl_relational_column_schema_domain (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.relational.ColumnSchema" "domain")) ((fn [v] v) x)))))

(def hydra_dsl_relational_column_schema_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.relational.ColumnSchema" "name")) ((fn [v] v) x)))))

(def hydra_dsl_relational_column_schema_with_domain (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.relational.ColumnSchema" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.relational.ColumnSchema" "name")) ((fn [v] v) original)))) (->hydra_core_field "domain" ((fn [v] v) new_val))))))))

(def hydra_dsl_relational_column_schema_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.relational.ColumnSchema" (list (->hydra_core_field "name" ((fn [v] v) new_val)) (->hydra_core_field "domain" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.relational.ColumnSchema" "domain")) ((fn [v] v) original))))))))))

(def hydra_dsl_relational_foreign_key (fn [foreign_relation] (fn [keys] (list :record (->hydra_core_record "hydra.relational.ForeignKey" (list (->hydra_core_field "foreignRelation" ((fn [v] v) foreign_relation)) (->hydra_core_field "keys" ((fn [v] v) keys))))))))

(def hydra_dsl_relational_foreign_key_foreign_relation (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.relational.ForeignKey" "foreignRelation")) ((fn [v] v) x)))))

(def hydra_dsl_relational_foreign_key_keys (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.relational.ForeignKey" "keys")) ((fn [v] v) x)))))

(def hydra_dsl_relational_foreign_key_with_foreign_relation (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.relational.ForeignKey" (list (->hydra_core_field "foreignRelation" ((fn [v] v) new_val)) (->hydra_core_field "keys" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.relational.ForeignKey" "keys")) ((fn [v] v) original))))))))))

(def hydra_dsl_relational_foreign_key_with_keys (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.relational.ForeignKey" (list (->hydra_core_field "foreignRelation" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.relational.ForeignKey" "foreignRelation")) ((fn [v] v) original)))) (->hydra_core_field "keys" ((fn [v] v) new_val))))))))

(def hydra_dsl_relational_primary_key (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.relational.PrimaryKey" ((fn [v] v) x)))))

(def hydra_dsl_relational_relation (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.relational.Relation" ((fn [v] v) x)))))

(def hydra_dsl_relational_relation_name (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.relational.RelationName" ((fn [v] v) x)))))

(def hydra_dsl_relational_relation_schema (fn [name] (fn [columns] (fn [primary_keys] (fn [foreign_keys] (list :record (->hydra_core_record "hydra.relational.RelationSchema" (list (->hydra_core_field "name" ((fn [v] v) name)) (->hydra_core_field "columns" ((fn [v] v) columns)) (->hydra_core_field "primaryKeys" ((fn [v] v) primary_keys)) (->hydra_core_field "foreignKeys" ((fn [v] v) foreign_keys))))))))))

(def hydra_dsl_relational_relation_schema_columns (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.relational.RelationSchema" "columns")) ((fn [v] v) x)))))

(def hydra_dsl_relational_relation_schema_foreign_keys (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.relational.RelationSchema" "foreignKeys")) ((fn [v] v) x)))))

(def hydra_dsl_relational_relation_schema_name (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.relational.RelationSchema" "name")) ((fn [v] v) x)))))

(def hydra_dsl_relational_relation_schema_primary_keys (fn [x] (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.relational.RelationSchema" "primaryKeys")) ((fn [v] v) x)))))

(def hydra_dsl_relational_relation_schema_with_columns (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.relational.RelationSchema" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.relational.RelationSchema" "name")) ((fn [v] v) original)))) (->hydra_core_field "columns" ((fn [v] v) new_val)) (->hydra_core_field "primaryKeys" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.relational.RelationSchema" "primaryKeys")) ((fn [v] v) original)))) (->hydra_core_field "foreignKeys" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.relational.RelationSchema" "foreignKeys")) ((fn [v] v) original))))))))))

(def hydra_dsl_relational_relation_schema_with_foreign_keys (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.relational.RelationSchema" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.relational.RelationSchema" "name")) ((fn [v] v) original)))) (->hydra_core_field "columns" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.relational.RelationSchema" "columns")) ((fn [v] v) original)))) (->hydra_core_field "primaryKeys" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.relational.RelationSchema" "primaryKeys")) ((fn [v] v) original)))) (->hydra_core_field "foreignKeys" ((fn [v] v) new_val))))))))

(def hydra_dsl_relational_relation_schema_with_name (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.relational.RelationSchema" (list (->hydra_core_field "name" ((fn [v] v) new_val)) (->hydra_core_field "columns" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.relational.RelationSchema" "columns")) ((fn [v] v) original)))) (->hydra_core_field "primaryKeys" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.relational.RelationSchema" "primaryKeys")) ((fn [v] v) original)))) (->hydra_core_field "foreignKeys" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.relational.RelationSchema" "foreignKeys")) ((fn [v] v) original))))))))))

(def hydra_dsl_relational_relation_schema_with_primary_keys (fn [original] (fn [new_val] (list :record (->hydra_core_record "hydra.relational.RelationSchema" (list (->hydra_core_field "name" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.relational.RelationSchema" "name")) ((fn [v] v) original)))) (->hydra_core_field "columns" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.relational.RelationSchema" "columns")) ((fn [v] v) original)))) (->hydra_core_field "primaryKeys" ((fn [v] v) new_val)) (->hydra_core_field "foreignKeys" (list :application (->hydra_core_application (list :project (->hydra_core_projection "hydra.relational.RelationSchema" "foreignKeys")) ((fn [v] v) original))))))))))

(def hydra_dsl_relational_relationship (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.relational.Relationship" ((fn [v] v) x)))))

(def hydra_dsl_relational_row (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.relational.Row" ((fn [v] v) x)))))

(def hydra_dsl_relational_un_column_name (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.relational.ColumnName") ((fn [v] v) x)))))

(def hydra_dsl_relational_un_primary_key (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.relational.PrimaryKey") ((fn [v] v) x)))))

(def hydra_dsl_relational_un_relation (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.relational.Relation") ((fn [v] v) x)))))

(def hydra_dsl_relational_un_relation_name (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.relational.RelationName") ((fn [v] v) x)))))

(def hydra_dsl_relational_un_relationship (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.relational.Relationship") ((fn [v] v) x)))))

(def hydra_dsl_relational_un_row (fn [x] (list :application (->hydra_core_application (list :unwrap "hydra.relational.Row") ((fn [v] v) x)))))
