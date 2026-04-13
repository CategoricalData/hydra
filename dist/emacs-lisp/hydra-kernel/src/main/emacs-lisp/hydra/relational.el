(require 'cl-lib)

(cl-defstruct hydra_relational_column_name value)

(cl-defstruct hydra_relational_column_schema name domain)

(cl-defstruct hydra_relational_foreign_key foreign_relation keys)

(cl-defstruct hydra_relational_primary_key value)

(cl-defstruct hydra_relational_relation value)

(cl-defstruct hydra_relational_relation_name value)

(cl-defstruct hydra_relational_relation_schema name columns primary_keys foreign_keys)

(cl-defstruct hydra_relational_relationship value)

(cl-defstruct hydra_relational_row value)

(provide 'hydra.relational)
