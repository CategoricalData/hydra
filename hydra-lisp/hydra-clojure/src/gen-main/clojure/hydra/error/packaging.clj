(ns hydra.error.packaging
  (:require [hydra.core :refer :all] [hydra.module :refer :all]
))

(declare hydra_error_packaging_invalid_module_error-variants hydra_error_packaging_invalid_package_error-variants)

(defrecord hydra_error_packaging_conflicting_module_namespace_error [first second])
(defn make-hydra_error_packaging_conflicting_module_namespace_error [first second] (->hydra_error_packaging_conflicting_module_namespace_error first second))

(defrecord hydra_error_packaging_conflicting_variant_name_error [namespace type_name variant_name conflicting_name])
(defn make-hydra_error_packaging_conflicting_variant_name_error [namespace type_name variant_name conflicting_name] (->hydra_error_packaging_conflicting_variant_name_error namespace type_name variant_name conflicting_name))

(defrecord hydra_error_packaging_definition_not_in_module_namespace_error [namespace name])
(defn make-hydra_error_packaging_definition_not_in_module_namespace_error [namespace name] (->hydra_error_packaging_definition_not_in_module_namespace_error namespace name))

(defrecord hydra_error_packaging_duplicate_definition_name_error [namespace name])
(defn make-hydra_error_packaging_duplicate_definition_name_error [namespace name] (->hydra_error_packaging_duplicate_definition_name_error namespace name))

(defrecord hydra_error_packaging_duplicate_module_namespace_error [namespace])
(defn make-hydra_error_packaging_duplicate_module_namespace_error [namespace] (->hydra_error_packaging_duplicate_module_namespace_error namespace))

(def hydra_error_packaging_invalid_module_error-variants (list :conflicting_variant_name :definition_not_in_module_namespace :duplicate_definition_name))

(def hydra_error_packaging_invalid_package_error-variants (list :conflicting_module_namespace :duplicate_module_namespace :invalid_module))
