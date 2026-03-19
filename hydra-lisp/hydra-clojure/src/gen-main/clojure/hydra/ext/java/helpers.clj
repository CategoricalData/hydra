(ns hydra.ext.java.helpers
  (:require [hydra.core :refer :all] [hydra.ext.java.syntax :refer :all] [hydra.graph :refer :all] [hydra.module :refer :all]
))

(declare hydra_ext_java_helpers_java_symbol_class-variants)

(def hydra_ext_java_helpers_java_symbol_class-variants (list :constant :nullary_function :hoisted_lambda :unary_function :local_variable))

(defrecord hydra_ext_java_helpers_java_features [supports_diamond_operator])
(defn make-hydra_ext_java_helpers_java_features [supports_diamond_operator] (->hydra_ext_java_helpers_java_features supports_diamond_operator))

(defrecord hydra_ext_java_helpers_aliases [current_namespace packages branch_vars recursive_vars in_scope_type_params polymorphic_locals in_scope_java_vars var_renames lambda_vars type_var_subst trusted_type_vars method_codomain thunked_vars])
(defn make-hydra_ext_java_helpers_aliases [current_namespace packages branch_vars recursive_vars in_scope_type_params polymorphic_locals in_scope_java_vars var_renames lambda_vars type_var_subst trusted_type_vars method_codomain thunked_vars] (->hydra_ext_java_helpers_aliases current_namespace packages branch_vars recursive_vars in_scope_type_params polymorphic_locals in_scope_java_vars var_renames lambda_vars type_var_subst trusted_type_vars method_codomain thunked_vars))

(defrecord hydra_ext_java_helpers_java_environment [aliases graph])
(defn make-hydra_ext_java_helpers_java_environment [aliases graph] (->hydra_ext_java_helpers_java_environment aliases graph))
