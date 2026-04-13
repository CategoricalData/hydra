(require 'cl-lib)

(require 'hydra.ext.java.syntax)

(require 'hydra.lib.lists)

(defvar hydra_ext_java_names_accept_method_name "accept")

(defvar hydra_ext_java_names_apply_method_name "apply")

(defvar hydra_ext_java_names_compare_to_method_name "compareTo")

(defvar hydra_ext_java_names_equals_method_name "equals")

(defvar hydra_ext_java_names_get_method_name "get")

(defvar hydra_ext_java_names_hash_code_method_name "hashCode")

(defvar hydra_ext_java_names_java_package_name (lambda (parts) (funcall (hydra_lib_lists_map (lambda (p) p)) parts)))

(defvar hydra_ext_java_names_hydra_core_package_name (list :just (hydra_ext_java_names_java_package_name (list "hydra" "core"))))

(defvar hydra_ext_java_names_hydra_util_package_name (list :just (hydra_ext_java_names_java_package_name (list "hydra" "util"))))

(defvar hydra_ext_java_names_instance_name "instance")

(defvar hydra_ext_java_names_java_lang_package_name (list :just (hydra_ext_java_names_java_package_name (list "java" "lang"))))

(defvar hydra_ext_java_names_java_util_function_package_name (list :just (hydra_ext_java_names_java_package_name (list "java" "util" "function"))))

(defvar hydra_ext_java_names_java_util_package_name (list :just (hydra_ext_java_names_java_package_name (list "java" "util"))))

(defvar hydra_ext_java_names_other_instance_name "other")

(defvar hydra_ext_java_names_otherwise_method_name "otherwise")

(defvar hydra_ext_java_names_partial_visitor_name "PartialVisitor")

(defvar hydra_ext_java_names_set_method_name "set")

(defvar hydra_ext_java_names_value_field_name "value")

(defvar hydra_ext_java_names_visit_method_name "visit")

(defvar hydra_ext_java_names_visitor_name "Visitor")

(defvar hydra_ext_java_names_visitor_return_parameter "R")

(provide 'hydra.ext.java.names)
