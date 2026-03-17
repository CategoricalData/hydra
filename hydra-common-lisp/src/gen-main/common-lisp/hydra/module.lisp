(defpackage :hydra.module
(:use :cl :hydra.core :hydra.graph)
(:export :hydra_module_definition-variants :make-hydra_module_file_extension :hydra_module_file_extension? :hydra_module_file_extension-value :make-hydra_module_library :hydra_module_library? :hydra_module_library-namespace :hydra_module_library-prefix :hydra_module_library-primitives :make-hydra_module_module :hydra_module_module? :hydra_module_module-namespace :hydra_module_module-elements :hydra_module_module-term_dependencies :hydra_module_module-type_dependencies :hydra_module_module-description :make-hydra_module_namespace :hydra_module_namespace? :hydra_module_namespace-value :make-hydra_module_namespaces :hydra_module_namespaces? :hydra_module_namespaces-focus :hydra_module_namespaces-mapping :make-hydra_module_qualified_name :hydra_module_qualified_name? :hydra_module_qualified_name-namespace :hydra_module_qualified_name-local :make-hydra_module_term_definition :hydra_module_term_definition? :hydra_module_term_definition-name :hydra_module_term_definition-term :hydra_module_term_definition-type :make-hydra_module_type_definition :hydra_module_type_definition? :hydra_module_type_definition-name :hydra_module_type_definition-type))

(in-package :hydra.module)

(cl:defvar hydra_module_definition-variants (cl:list :term :type))

(cl:defstruct hydra_module_file_extension value)

(cl:defstruct hydra_module_library namespace prefix primitives)

(cl:defstruct hydra_module_module namespace elements term_dependencies type_dependencies description)

(cl:defstruct hydra_module_namespace value)

(cl:defstruct hydra_module_namespaces focus mapping)

(cl:defstruct hydra_module_qualified_name namespace local)

(cl:defstruct hydra_module_term_definition name term type)

(cl:defstruct hydra_module_type_definition name type)
