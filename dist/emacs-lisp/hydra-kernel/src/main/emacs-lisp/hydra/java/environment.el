(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.graph)

(require 'hydra.java.syntax)

(require 'hydra.packaging)

(defvar hydra_java_environment_java_symbol_class-variants (list :constant :nullary_function :hoisted_lambda :unary_function :local_variable))

(cl-defstruct hydra_java_environment_java_features supports_diamond_operator)

(cl-defstruct hydra_java_environment_aliases current_namespace packages branch_vars recursive_vars in_scope_type_params polymorphic_locals in_scope_java_vars var_renames lambda_vars type_var_subst trusted_type_vars method_codomain thunked_vars)

(cl-defstruct hydra_java_environment_java_environment aliases graph)

(provide 'hydra.java.environment)
