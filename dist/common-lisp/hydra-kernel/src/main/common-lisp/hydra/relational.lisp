(defpackage :hydra.relational
(:use :cl)
(:export :make-hydra_relational_column_name :hydra_relational_column_name? :hydra_relational_column_name-value :make-hydra_relational_column_schema :hydra_relational_column_schema? :hydra_relational_column_schema-name :hydra_relational_column_schema-domain :make-hydra_relational_foreign_key :hydra_relational_foreign_key? :hydra_relational_foreign_key-foreign_relation :hydra_relational_foreign_key-keys :make-hydra_relational_primary_key :hydra_relational_primary_key? :hydra_relational_primary_key-value :make-hydra_relational_relation :hydra_relational_relation? :hydra_relational_relation-value :make-hydra_relational_relation_name :hydra_relational_relation_name? :hydra_relational_relation_name-value :make-hydra_relational_relation_schema :hydra_relational_relation_schema? :hydra_relational_relation_schema-name :hydra_relational_relation_schema-columns :hydra_relational_relation_schema-primary_keys :hydra_relational_relation_schema-foreign_keys :make-hydra_relational_relationship :hydra_relational_relationship? :hydra_relational_relationship-value :make-hydra_relational_row :hydra_relational_row? :hydra_relational_row-value))

(in-package :hydra.relational)

(cl:defstruct hydra_relational_column_name value)

(cl:defstruct hydra_relational_column_schema name domain)

(cl:defstruct hydra_relational_foreign_key foreign_relation keys)

(cl:defstruct hydra_relational_primary_key value)

(cl:defstruct hydra_relational_relation value)

(cl:defstruct hydra_relational_relation_name value)

(cl:defstruct hydra_relational_relation_schema name columns primary_keys foreign_keys)

(cl:defstruct hydra_relational_relationship value)

(cl:defstruct hydra_relational_row value)
