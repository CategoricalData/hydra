(module
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\3b\00\00\00\4e\6f\74\65\3a\20\74\68\69\73\20\69\73\20\61\6e\20\61\75\74\6f\6d\61\74\69\63\61\6c\6c\79\20\67\65\6e\65\72\61\74\65\64\20\66\69\6c\65\2e\20\44\6f\20\6e\6f\74\20\65\64\69\74\2e\01\00\00\00\5f\07\00\00\00\63\6c\61\73\73\65\73\07\00\00\00\64\65\62\75\67\49\64\0a\00\00\00\64\65\70\72\65\63\61\74\65\64\0b\00\00\00\64\65\73\63\72\69\70\74\69\6f\6e\07\00\00\00\65\78\63\6c\75\64\65\0e\00\00\00\66\69\72\73\74\43\6c\61\73\73\54\79\70\65\16\00\00\00\66\72\65\73\68\54\79\70\65\56\61\72\69\61\62\6c\65\43\6f\75\6e\74\09\00\00\00\6d\61\78\4c\65\6e\67\74\68\09\00\00\00\6d\69\6e\4c\65\6e\67\74\68\11\00\00\00\70\72\65\73\65\72\76\65\46\69\65\6c\64\4e\61\6d\65\04\00\00\00\74\79\70\65")
  (global $__bump_ptr (mut i32) i32.const 1264 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
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
  i32.const 1087
)
  (func $hydra.constants.key_classes (result i32)
  i32.const 1092
)
  (func $hydra.constants.key_debug_id (result i32)
  i32.const 1103
)
  (func $hydra.constants.key_deprecated (result i32)
  i32.const 1114
)
  (func $hydra.constants.key_description (result i32)
  i32.const 1128
)
  (func $hydra.constants.key_exclude (result i32)
  i32.const 1143
)
  (func $hydra.constants.key_first_class_type (result i32)
  i32.const 1154
)
  (func $hydra.constants.key_fresh_type_variable_count (result i32)
  i32.const 1172
)
  (func $hydra.constants.key_max_length (result i32)
  i32.const 1198
)
  (func $hydra.constants.key_min_length (result i32)
  i32.const 1211
)
  (func $hydra.constants.key_preserve_field_name (result i32)
  i32.const 1224
)
  (func $hydra.constants.key_type (result i32)
  i32.const 1245
)
  (func $hydra.constants.max_int32 (result i32)
  i32.const 2147483647
)
  (func $hydra.constants.max_trace_depth (result i32)
  i32.const 5000
)
  (func $hydra.constants.warning_auto_generated_file (result i32)
  i32.const 1024
)
)
