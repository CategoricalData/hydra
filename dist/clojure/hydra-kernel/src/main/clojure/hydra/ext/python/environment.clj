(ns hydra.ext.python.environment
  (:require [hydra.core :refer :all] [hydra.ext.python.syntax :refer :all] [hydra.graph :refer :all] [hydra.packaging :refer :all]
))

(declare hydra_ext_python_environment_python_version-variants)

(def hydra_ext_python_environment_python_version-variants (list :python310 :python312))

(defrecord hydra_ext_python_environment_python_environment [namespaces bound_type_variables graph nullary_bindings version skip_casts inline_variables])
(defn make-hydra_ext_python_environment_python_environment [namespaces bound_type_variables graph nullary_bindings version skip_casts inline_variables] (->hydra_ext_python_environment_python_environment namespaces bound_type_variables graph nullary_bindings version skip_casts inline_variables))

(defrecord hydra_ext_python_environment_python_module_metadata [namespaces type_variables uses_annotated uses_callable uses_cast uses_lru_cache uses_type_alias uses_dataclass uses_decimal uses_either uses_enum uses_frozen_dict uses_frozen_list uses_generic uses_just uses_left uses_maybe uses_name uses_node uses_nothing uses_right uses_type_var])
(defn make-hydra_ext_python_environment_python_module_metadata [namespaces type_variables uses_annotated uses_callable uses_cast uses_lru_cache uses_type_alias uses_dataclass uses_decimal uses_either uses_enum uses_frozen_dict uses_frozen_list uses_generic uses_just uses_left uses_maybe uses_name uses_node uses_nothing uses_right uses_type_var] (->hydra_ext_python_environment_python_module_metadata namespaces type_variables uses_annotated uses_callable uses_cast uses_lru_cache uses_type_alias uses_dataclass uses_decimal uses_either uses_enum uses_frozen_dict uses_frozen_list uses_generic uses_just uses_left uses_maybe uses_name uses_node uses_nothing uses_right uses_type_var))

(defrecord hydra_ext_python_environment_py_graph [graph metadata])
(defn make-hydra_ext_python_environment_py_graph [graph metadata] (->hydra_ext_python_environment_py_graph graph metadata))
