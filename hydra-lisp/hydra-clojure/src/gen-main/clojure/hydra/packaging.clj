(ns hydra.packaging
  (:require [hydra.core :refer :all] [hydra.graph :refer :all]
))

(declare hydra_packaging_definition-variants)

(def hydra_packaging_definition-variants (list :term :type))

(defrecord hydra_packaging_file_extension [value])
(defn make-hydra_packaging_file_extension [value] (->hydra_packaging_file_extension value))

(defrecord hydra_packaging_library [namespace prefix primitives])
(defn make-hydra_packaging_library [namespace prefix primitives] (->hydra_packaging_library namespace prefix primitives))

(defrecord hydra_packaging_module [namespace definitions term_dependencies type_dependencies description])
(defn make-hydra_packaging_module [namespace definitions term_dependencies type_dependencies description] (->hydra_packaging_module namespace definitions term_dependencies type_dependencies description))

(defrecord hydra_packaging_namespace [value])
(defn make-hydra_packaging_namespace [value] (->hydra_packaging_namespace value))

(defrecord hydra_packaging_namespaces [focus mapping])
(defn make-hydra_packaging_namespaces [focus mapping] (->hydra_packaging_namespaces focus mapping))

(defrecord hydra_packaging_package [name modules dependencies description])
(defn make-hydra_packaging_package [name modules dependencies description] (->hydra_packaging_package name modules dependencies description))

(defrecord hydra_packaging_package_name [value])
(defn make-hydra_packaging_package_name [value] (->hydra_packaging_package_name value))

(defrecord hydra_packaging_qualified_name [namespace local])
(defn make-hydra_packaging_qualified_name [namespace local] (->hydra_packaging_qualified_name namespace local))

(defrecord hydra_packaging_term_definition [name term type])
(defn make-hydra_packaging_term_definition [name term type] (->hydra_packaging_term_definition name term type))

(defrecord hydra_packaging_type_definition [name type])
(defn make-hydra_packaging_type_definition [name type] (->hydra_packaging_type_definition name type))
