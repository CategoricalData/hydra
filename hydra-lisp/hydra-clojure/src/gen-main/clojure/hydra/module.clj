(ns hydra.module
  (:require [hydra.core :refer :all] [hydra.graph :refer :all]
))

(declare hydra_module_definition-variants)

(def hydra_module_definition-variants (list :term :type))

(defrecord hydra_module_file_extension [value])
(defn make-hydra_module_file_extension [value] (->hydra_module_file_extension value))

(defrecord hydra_module_library [namespace prefix primitives])
(defn make-hydra_module_library [namespace prefix primitives] (->hydra_module_library namespace prefix primitives))

(defrecord hydra_module_module [namespace elements term_dependencies type_dependencies description])
(defn make-hydra_module_module [namespace elements term_dependencies type_dependencies description] (->hydra_module_module namespace elements term_dependencies type_dependencies description))

(defrecord hydra_module_namespace [value])
(defn make-hydra_module_namespace [value] (->hydra_module_namespace value))

(defrecord hydra_module_namespaces [focus mapping])
(defn make-hydra_module_namespaces [focus mapping] (->hydra_module_namespaces focus mapping))

(defrecord hydra_module_qualified_name [namespace local])
(defn make-hydra_module_qualified_name [namespace local] (->hydra_module_qualified_name namespace local))

(defrecord hydra_module_term_definition [name term type])
(defn make-hydra_module_term_definition [name term type] (->hydra_module_term_definition name term type))

(defrecord hydra_module_type_definition [name type])
(defn make-hydra_module_type_definition [name type] (->hydra_module_type_definition name type))
