(module
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.constants.debug_inference" (func $hydra.constants.debug_inference) )
  (export "hydra.constants.ignored_variable" (func $hydra.constants.ignored_variable) )
  (export "hydra.constants.key_classes" (func $hydra.constants.key_classes) )
  (export "hydra.constants.key_debug_id" (func $hydra.constants.key_debug_id) )
  (export "hydra.constants.key_deprecated" (func $hydra.constants.key_deprecated) )
  (export "hydra.constants.key_description" (func $hydra.constants.key_description) )
  (export "hydra.constants.key_exclude" (func $hydra.constants.key_exclude) )
  (export "hydra.constants.key_first_class_type" (func $hydra.constants.key_first_class_type) )
  (export "hydra.constants.key_fresh_type_variable_count" (func $hydra.constants.key_fresh_type_variable_count) )
  (export "hydra.constants.key_max_length" (func $hydra.constants.key_max_length) )
  (export "hydra.constants.key_min_length" (func $hydra.constants.key_min_length) )
  (export "hydra.constants.key_preserve_field_name" (func $hydra.constants.key_preserve_field_name) )
  (export "hydra.constants.key_type" (func $hydra.constants.key_type) )
  (export "hydra.constants.max_int32" (func $hydra.constants.max_int32) )
  (export "hydra.constants.max_trace_depth" (func $hydra.constants.max_trace_depth) )
  (export "hydra.constants.warning_auto_generated_file" (func $hydra.constants.warning_auto_generated_file) )
  (func $hydra.constants.debug_inference (result i32)
  i32.const 1
)
  (func $hydra.constants.ignored_variable (result i32)
  i32.const 0 ;; string: "_"
)
  (func $hydra.constants.key_classes (result i32)
  i32.const 0 ;; string: "classes"
)
  (func $hydra.constants.key_debug_id (result i32)
  i32.const 0 ;; string: "debugId"
)
  (func $hydra.constants.key_deprecated (result i32)
  i32.const 0 ;; string: "deprecated"
)
  (func $hydra.constants.key_description (result i32)
  i32.const 0 ;; string: "description"
)
  (func $hydra.constants.key_exclude (result i32)
  i32.const 0 ;; string: "exclude"
)
  (func $hydra.constants.key_first_class_type (result i32)
  i32.const 0 ;; string: "firstClassType"
)
  (func $hydra.constants.key_fresh_type_variable_count (result i32)
  i32.const 0 ;; string: "freshTypeVariableCount"
)
  (func $hydra.constants.key_max_length (result i32)
  i32.const 0 ;; string: "maxLength"
)
  (func $hydra.constants.key_min_length (result i32)
  i32.const 0 ;; string: "minLength"
)
  (func $hydra.constants.key_preserve_field_name (result i32)
  i32.const 0 ;; string: "preserveFieldName"
)
  (func $hydra.constants.key_type (result i32)
  i32.const 0 ;; string: "type"
)
  (func $hydra.constants.max_int32 (result i32)
  i32.const 2147483647
)
  (func $hydra.constants.max_trace_depth (result i32)
  i32.const 5000
)
  (func $hydra.constants.warning_auto_generated_file (result i32)
  i32.const 0 ;; string: "Note: this is an automatically generated file. Do not edit."
)
)
