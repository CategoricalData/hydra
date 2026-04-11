(defpackage :hydra.packaging
(:use :cl :hydra.core :hydra.graph)
(:export :hydra_packaging_definition-variants :make-hydra_packaging_file_extension :hydra_packaging_file_extension? :hydra_packaging_file_extension-value :make-hydra_packaging_library :hydra_packaging_library? :hydra_packaging_library-namespace :hydra_packaging_library-prefix :hydra_packaging_library-primitives :make-hydra_packaging_module :hydra_packaging_module? :hydra_packaging_module-namespace :hydra_packaging_module-definitions :hydra_packaging_module-term_dependencies :hydra_packaging_module-type_dependencies :hydra_packaging_module-description :make-hydra_packaging_namespace :hydra_packaging_namespace? :hydra_packaging_namespace-value :make-hydra_packaging_namespaces :hydra_packaging_namespaces? :hydra_packaging_namespaces-focus :hydra_packaging_namespaces-mapping :make-hydra_packaging_package :hydra_packaging_package? :hydra_packaging_package-name :hydra_packaging_package-modules :hydra_packaging_package-dependencies :hydra_packaging_package-description :make-hydra_packaging_package_name :hydra_packaging_package_name? :hydra_packaging_package_name-value :make-hydra_packaging_qualified_name :hydra_packaging_qualified_name? :hydra_packaging_qualified_name-namespace :hydra_packaging_qualified_name-local :make-hydra_packaging_term_definition :hydra_packaging_term_definition? :hydra_packaging_term_definition-name :hydra_packaging_term_definition-term :hydra_packaging_term_definition-type :make-hydra_packaging_type_definition :hydra_packaging_type_definition? :hydra_packaging_type_definition-name :hydra_packaging_type_definition-type))

(in-package :hydra.packaging)

(cl:defvar hydra_packaging_definition-variants (cl:list :term :type))

(cl:defstruct hydra_packaging_file_extension value)

(cl:defstruct hydra_packaging_library namespace prefix primitives)

(cl:defstruct hydra_packaging_module namespace definitions term_dependencies type_dependencies description)

(cl:defstruct hydra_packaging_namespace value)

(cl:defstruct hydra_packaging_namespaces focus mapping)

(cl:defstruct hydra_packaging_package name modules dependencies description)

(cl:defstruct hydra_packaging_package_name value)

(cl:defstruct hydra_packaging_qualified_name namespace local)

(cl:defstruct hydra_packaging_term_definition name term type)

(cl:defstruct hydra_packaging_type_definition name type)
