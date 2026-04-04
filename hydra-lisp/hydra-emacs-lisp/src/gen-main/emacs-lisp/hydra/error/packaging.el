(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.packaging)

(cl-defstruct hydra_error_packaging_conflicting_module_namespace_error first second)

(cl-defstruct hydra_error_packaging_conflicting_variant_name_error namespace type_name variant_name conflicting_name)

(cl-defstruct hydra_error_packaging_definition_not_in_module_namespace_error namespace name)

(cl-defstruct hydra_error_packaging_duplicate_definition_name_error namespace name)

(cl-defstruct hydra_error_packaging_duplicate_module_namespace_error namespace)

(defvar hydra_error_packaging_invalid_module_error-variants (list :conflicting_variant_name :definition_not_in_module_namespace :duplicate_definition_name))

(defvar hydra_error_packaging_invalid_package_error-variants (list :conflicting_module_namespace :duplicate_module_namespace :invalid_module))

(provide 'hydra.error.packaging)
