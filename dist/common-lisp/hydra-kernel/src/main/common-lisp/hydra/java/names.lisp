(defpackage :hydra.java.names
(:use :cl :hydra.java.syntax :hydra.lib.lists)
(:export :hydra_java_names_accept_method_name :hydra_java_names_apply_method_name :hydra_java_names_compare_to_method_name :hydra_java_names_equals_method_name :hydra_java_names_get_method_name :hydra_java_names_hash_code_method_name :hydra_java_names_java_package_name :hydra_java_names_hydra_core_package_name :hydra_java_names_hydra_util_package_name :hydra_java_names_instance_name :hydra_java_names_java_lang_package_name :hydra_java_names_java_util_function_package_name :hydra_java_names_java_util_package_name :hydra_java_names_other_instance_name :hydra_java_names_otherwise_method_name :hydra_java_names_partial_visitor_name :hydra_java_names_set_method_name :hydra_java_names_value_field_name :hydra_java_names_visit_method_name :hydra_java_names_visitor_name :hydra_java_names_visitor_return_parameter))

(in-package :hydra.java.names)

(cl:defvar hydra_java_names_accept_method_name "accept")

(cl:defvar hydra_java_names_apply_method_name "apply")

(cl:defvar hydra_java_names_compare_to_method_name "compareTo")

(cl:defvar hydra_java_names_equals_method_name "equals")

(cl:defvar hydra_java_names_get_method_name "get")

(cl:defvar hydra_java_names_hash_code_method_name "hashCode")

(cl:defvar hydra_java_names_java_package_name (cl:lambda (parts) ((hydra_lib_lists_map (cl:lambda (p) p)) parts)))

(cl:defvar hydra_java_names_hydra_core_package_name (list :just (hydra_java_names_java_package_name (cl:list "hydra" "core"))))

(cl:defvar hydra_java_names_hydra_util_package_name (list :just (hydra_java_names_java_package_name (cl:list "hydra" "util"))))

(cl:defvar hydra_java_names_instance_name "instance")

(cl:defvar hydra_java_names_java_lang_package_name (list :just (hydra_java_names_java_package_name (cl:list "java" "lang"))))

(cl:defvar hydra_java_names_java_util_function_package_name (list :just (hydra_java_names_java_package_name (cl:list "java" "util" "function"))))

(cl:defvar hydra_java_names_java_util_package_name (list :just (hydra_java_names_java_package_name (cl:list "java" "util"))))

(cl:defvar hydra_java_names_other_instance_name "other")

(cl:defvar hydra_java_names_otherwise_method_name "otherwise")

(cl:defvar hydra_java_names_partial_visitor_name "PartialVisitor")

(cl:defvar hydra_java_names_set_method_name "set")

(cl:defvar hydra_java_names_value_field_name "value")

(cl:defvar hydra_java_names_visit_method_name "visit")

(cl:defvar hydra_java_names_visitor_name "Visitor")

(cl:defvar hydra_java_names_visitor_return_parameter "R")
