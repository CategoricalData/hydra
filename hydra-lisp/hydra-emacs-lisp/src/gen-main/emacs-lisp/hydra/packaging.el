(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.graph)

(defvar hydra_packaging_definition-variants (list :term :type))

(cl-defstruct hydra_packaging_file_extension value)

(cl-defstruct hydra_packaging_library namespace prefix primitives)

(cl-defstruct hydra_packaging_module namespace definitions term_dependencies type_dependencies description)

(cl-defstruct hydra_packaging_namespace value)

(cl-defstruct hydra_packaging_namespaces focus mapping)

(cl-defstruct hydra_packaging_package name modules dependencies description)

(cl-defstruct hydra_packaging_package_name value)

(cl-defstruct hydra_packaging_qualified_name namespace local)

(cl-defstruct hydra_packaging_term_definition name term type)

(cl-defstruct hydra_packaging_type_definition name type)

(provide 'hydra.packaging)
