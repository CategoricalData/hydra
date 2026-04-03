(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.lib.lists)

(require 'hydra.lib.maps)

(require 'hydra.lib.sets)

(require 'hydra.relational)

(defvar hydra_encode_relational_column_name (lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.relational.ColumnName" (funcall (lambda (x2) (list :literal (list :string x2))) (funcall (lambda (v) v) x))))))

(defvar hydra_encode_relational_column_schema (lambda (t_) (lambda (x) (list :record (make-hydra_core_record "hydra.relational.ColumnSchema" (list (make-hydra_core_field "name" (hydra_encode_relational_column_name (funcall (lambda (v) (hydra_relational_column_schema-name v)) x))) (make-hydra_core_field "domain" (t_ (funcall (lambda (v) (hydra_relational_column_schema-domain v)) x)))))))))

(defvar hydra_encode_relational_relation_name (lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.relational.RelationName" (funcall (lambda (x2) (list :literal (list :string x2))) (funcall (lambda (v) v) x))))))

(defvar hydra_encode_relational_foreign_key (lambda (x) (list :record (make-hydra_core_record "hydra.relational.ForeignKey" (list (make-hydra_core_field "foreignRelation" (hydra_encode_relational_relation_name (funcall (lambda (v) (hydra_relational_foreign_key-foreign_relation v)) x))) (make-hydra_core_field "keys" (funcall (lambda (m) (list :map (funcall (funcall (hydra_lib_maps_bimap hydra_encode_relational_column_name) hydra_encode_relational_column_name) m))) (funcall (lambda (v) (hydra_relational_foreign_key-keys v)) x))))))))

(defvar hydra_encode_relational_primary_key (lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.relational.PrimaryKey" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map hydra_encode_relational_column_name) xs))) (funcall (lambda (v) v) x))))))

(defvar hydra_encode_relational_row (lambda (v) (lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.relational.Row" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map v) xs))) (funcall (lambda (v) v) x)))))))

(defvar hydra_encode_relational_relation (lambda (v) (lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.relational.Relation" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map (hydra_encode_relational_row v)) xs))) (funcall (lambda (v) v) x)))))))

(defvar hydra_encode_relational_relation_schema (lambda (t_) (lambda (x) (list :record (make-hydra_core_record "hydra.relational.RelationSchema" (list (make-hydra_core_field "name" (hydra_encode_relational_relation_name (funcall (lambda (v) (hydra_relational_relation_schema-name v)) x))) (make-hydra_core_field "columns" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map (hydra_encode_relational_column_schema t_)) xs))) (funcall (lambda (v) (hydra_relational_relation_schema-columns v)) x))) (make-hydra_core_field "primaryKeys" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map hydra_encode_relational_primary_key) xs))) (funcall (lambda (v) (hydra_relational_relation_schema-primary_keys v)) x))) (make-hydra_core_field "foreignKeys" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map hydra_encode_relational_foreign_key) xs))) (funcall (lambda (v) (hydra_relational_relation_schema-foreign_keys v)) x)))))))))

(defvar hydra_encode_relational_relationship (lambda (v) (lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.relational.Relationship" (funcall (lambda (s) (list :set (funcall (hydra_lib_sets_map (lambda (m) (list :map (funcall (funcall (hydra_lib_maps_bimap hydra_encode_relational_column_name) v) m)))) s))) (funcall (lambda (v) v) x)))))))

(provide 'hydra.encode.relational)
