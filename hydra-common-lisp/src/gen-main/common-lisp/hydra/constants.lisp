(defpackage :hydra.constants
(:use :cl :hydra.core)
(:export :hydra_constants_debug_inference :hydra_constants_ignored_variable :hydra_constants_key_classes :hydra_constants_key_debug_id :hydra_constants_key_deprecated :hydra_constants_key_description :hydra_constants_key_exclude :hydra_constants_key_first_class_type :hydra_constants_key_fresh_type_variable_count :hydra_constants_key_max_length :hydra_constants_key_min_length :hydra_constants_key_preserve_field_name :hydra_constants_key_type :hydra_constants_max_int32 :hydra_constants_max_trace_depth :hydra_constants_placeholder_name :hydra_constants_warning_auto_generated_file))

(in-package :hydra.constants)

(cl:defvar hydra_constants_debug_inference cl:t)

(cl:defvar hydra_constants_ignored_variable "_")

(cl:defvar hydra_constants_key_classes "classes")

(cl:defvar hydra_constants_key_debug_id "debugId")

(cl:defvar hydra_constants_key_deprecated "deprecated")

(cl:defvar hydra_constants_key_description "description")

(cl:defvar hydra_constants_key_exclude "exclude")

(cl:defvar hydra_constants_key_first_class_type "firstClassType")

(cl:defvar hydra_constants_key_fresh_type_variable_count "freshTypeVariableCount")

(cl:defvar hydra_constants_key_max_length "maxLength")

(cl:defvar hydra_constants_key_min_length "minLength")

(cl:defvar hydra_constants_key_preserve_field_name "preserveFieldName")

(cl:defvar hydra_constants_key_type "type")

(cl:defvar hydra_constants_max_int32 2147483647)

(cl:defvar hydra_constants_max_trace_depth 5000)

(cl:defvar hydra_constants_placeholder_name "Placeholder")

(cl:defvar hydra_constants_warning_auto_generated_file "Note: this is an automatically generated file. Do not edit.")
