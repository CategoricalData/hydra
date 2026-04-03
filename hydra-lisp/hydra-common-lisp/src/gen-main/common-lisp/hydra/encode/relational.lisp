(defpackage :hydra.encode.relational
(:use :cl :hydra.core :hydra.lib.lists :hydra.lib.maps :hydra.lib.sets :hydra.relational)
(:export :hydra_encode_relational_column_name :hydra_encode_relational_column_schema :hydra_encode_relational_relation_name :hydra_encode_relational_foreign_key :hydra_encode_relational_primary_key :hydra_encode_relational_row :hydra_encode_relational_relation :hydra_encode_relational_relation_schema :hydra_encode_relational_relationship))

(in-package :hydra.encode.relational)

(cl:defvar hydra_encode_relational_column_name (cl:lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.relational.ColumnName" ((cl:lambda (x2) (list :literal (list :string x2))) ((cl:lambda (v) v) x))))))

(cl:defvar hydra_encode_relational_column_schema (cl:lambda (t_) (cl:lambda (x) (list :record (make-hydra_core_record "hydra.relational.ColumnSchema" (cl:list (make-hydra_core_field "name" (hydra_encode_relational_column_name ((cl:lambda (v) (hydra_relational_column_schema-name v)) x))) (make-hydra_core_field "domain" (t_ ((cl:lambda (v) (hydra_relational_column_schema-domain v)) x)))))))))

(cl:defvar hydra_encode_relational_relation_name (cl:lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.relational.RelationName" ((cl:lambda (x2) (list :literal (list :string x2))) ((cl:lambda (v) v) x))))))

(cl:defvar hydra_encode_relational_foreign_key (cl:lambda (x) (list :record (make-hydra_core_record "hydra.relational.ForeignKey" (cl:list (make-hydra_core_field "foreignRelation" (hydra_encode_relational_relation_name ((cl:lambda (v) (hydra_relational_foreign_key-foreign_relation v)) x))) (make-hydra_core_field "keys" ((cl:lambda (m) (list :map (((hydra_lib_maps_bimap hydra_encode_relational_column_name) hydra_encode_relational_column_name) m))) ((cl:lambda (v) (hydra_relational_foreign_key-keys v)) x))))))))

(cl:defvar hydra_encode_relational_primary_key (cl:lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.relational.PrimaryKey" ((cl:lambda (xs) (list :list ((hydra_lib_lists_map hydra_encode_relational_column_name) xs))) ((cl:lambda (v) v) x))))))

(cl:defvar hydra_encode_relational_row (cl:lambda (v) (cl:lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.relational.Row" ((cl:lambda (xs) (list :list ((hydra_lib_lists_map v) xs))) ((cl:lambda (v) v) x)))))))

(cl:defvar hydra_encode_relational_relation (cl:lambda (v) (cl:lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.relational.Relation" ((cl:lambda (xs) (list :list ((hydra_lib_lists_map (hydra_encode_relational_row v)) xs))) ((cl:lambda (v) v) x)))))))

(cl:defvar hydra_encode_relational_relation_schema (cl:lambda (t_) (cl:lambda (x) (list :record (make-hydra_core_record "hydra.relational.RelationSchema" (cl:list (make-hydra_core_field "name" (hydra_encode_relational_relation_name ((cl:lambda (v) (hydra_relational_relation_schema-name v)) x))) (make-hydra_core_field "columns" ((cl:lambda (xs) (list :list ((hydra_lib_lists_map (hydra_encode_relational_column_schema t_)) xs))) ((cl:lambda (v) (hydra_relational_relation_schema-columns v)) x))) (make-hydra_core_field "primaryKeys" ((cl:lambda (xs) (list :list ((hydra_lib_lists_map hydra_encode_relational_primary_key) xs))) ((cl:lambda (v) (hydra_relational_relation_schema-primary_keys v)) x))) (make-hydra_core_field "foreignKeys" ((cl:lambda (xs) (list :list ((hydra_lib_lists_map hydra_encode_relational_foreign_key) xs))) ((cl:lambda (v) (hydra_relational_relation_schema-foreign_keys v)) x)))))))))

(cl:defvar hydra_encode_relational_relationship (cl:lambda (v) (cl:lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.relational.Relationship" ((cl:lambda (s) (list :set ((hydra_lib_sets_map (cl:lambda (m) (list :map (((hydra_lib_maps_bimap hydra_encode_relational_column_name) v) m)))) s))) ((cl:lambda (v) v) x)))))))
