(ns hydra.cpp.environment
  (:require [hydra.core :refer :all] [hydra.packaging :refer :all]
))

(defrecord hydra_cpp_environment_cpp_environment [namespaces bound_type_variables])
(defn make-hydra_cpp_environment_cpp_environment [namespaces bound_type_variables] (->hydra_cpp_environment_cpp_environment namespaces bound_type_variables))
