(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.graph)

(defvar hydra_module_definition-variants (list :term :type))

(cl-defstruct hydra_module_file_extension value)

(cl-defstruct hydra_module_library namespace prefix primitives)

(cl-defstruct hydra_module_module namespace elements term_dependencies type_dependencies description)

(cl-defstruct hydra_module_namespace value)

(cl-defstruct hydra_module_namespaces focus mapping)

(cl-defstruct hydra_module_qualified_name namespace local)

(cl-defstruct hydra_module_term_definition name term type)

(cl-defstruct hydra_module_type_definition name type)

(provide 'hydra.module)
