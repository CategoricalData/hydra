(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.relational)

(cl-defstruct hydra_tabular_column_type name type)

(cl-defstruct hydra_tabular_data_row value)

(cl-defstruct hydra_tabular_header_row value)

(cl-defstruct hydra_tabular_table header data)

(cl-defstruct hydra_tabular_table_type name columns)

(provide 'hydra.tabular)
