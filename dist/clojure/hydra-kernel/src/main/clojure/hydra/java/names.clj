(ns hydra.java.names
  (:require [hydra.java.syntax :refer :all] [hydra.lib.lists :refer :all]
))

(declare hydra_java_names_accept_method_name hydra_java_names_apply_method_name hydra_java_names_compare_to_method_name hydra_java_names_equals_method_name hydra_java_names_get_method_name hydra_java_names_hash_code_method_name hydra_java_names_java_package_name hydra_java_names_hydra_core_package_name hydra_java_names_hydra_util_package_name hydra_java_names_instance_name hydra_java_names_java_lang_package_name hydra_java_names_java_util_function_package_name hydra_java_names_java_util_package_name hydra_java_names_other_instance_name hydra_java_names_otherwise_method_name hydra_java_names_partial_visitor_name hydra_java_names_set_method_name hydra_java_names_value_field_name hydra_java_names_visit_method_name hydra_java_names_visitor_name hydra_java_names_visitor_return_parameter)

(def hydra_java_names_accept_method_name "accept")

(def hydra_java_names_apply_method_name "apply")

(def hydra_java_names_compare_to_method_name "compareTo")

(def hydra_java_names_equals_method_name "equals")

(def hydra_java_names_get_method_name "get")

(def hydra_java_names_hash_code_method_name "hashCode")

(def hydra_java_names_java_package_name (fn [parts] ((hydra_lib_lists_map (fn [p] p)) parts)))

(def hydra_java_names_hydra_core_package_name (list :just (hydra_java_names_java_package_name (list "hydra" "core"))))

(def hydra_java_names_hydra_util_package_name (list :just (hydra_java_names_java_package_name (list "hydra" "util"))))

(def hydra_java_names_instance_name "instance")

(def hydra_java_names_java_lang_package_name (list :just (hydra_java_names_java_package_name (list "java" "lang"))))

(def hydra_java_names_java_util_function_package_name (list :just (hydra_java_names_java_package_name (list "java" "util" "function"))))

(def hydra_java_names_java_util_package_name (list :just (hydra_java_names_java_package_name (list "java" "util"))))

(def hydra_java_names_other_instance_name "other")

(def hydra_java_names_otherwise_method_name "otherwise")

(def hydra_java_names_partial_visitor_name "PartialVisitor")

(def hydra_java_names_set_method_name "set")

(def hydra_java_names_value_field_name "value")

(def hydra_java_names_visit_method_name "visit")

(def hydra_java_names_visitor_name "Visitor")

(def hydra_java_names_visitor_return_parameter "R")
