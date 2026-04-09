(module
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.reflect" "hydra.reflect.elimination_variants" (func $hydra.reflect.elimination_variants (param i32) (result i32) ) )
  (import "hydra.reflect" "hydra.reflect.float_types" (func $hydra.reflect.float_types (param i32) (result i32) ) )
  (import "hydra.reflect" "hydra.reflect.function_variants" (func $hydra.reflect.function_variants (param i32) (result i32) ) )
  (import "hydra.reflect" "hydra.reflect.integer_types" (func $hydra.reflect.integer_types (param i32) (result i32) ) )
  (import "hydra.reflect" "hydra.reflect.literal_variants" (func $hydra.reflect.literal_variants (param i32) (result i32) ) )
  (import "hydra.reflect" "hydra.reflect.term_variants" (func $hydra.reflect.term_variants (param i32) (result i32) ) )
  (import "hydra.reflect" "hydra.reflect.type_variants" (func $hydra.reflect.type_variants (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.languages.hydra_language" (func $hydra.languages.hydra_language) )
  (func $hydra.languages.hydra_language (result i32)
  (local $elimination_variants i32)
  (local $float_types i32)
  (local $function_variants i32)
  (local $integer_types i32)
  (local $literal_variants i32)
  (local $t i32)
  (local $term_variants i32)
  (local $type_variants i32)
  (local $types i32)
  call $hydra.reflect.elimination_variants
  call $hydra.lib.sets.from_list
  local.set $elimination_variants
  call $hydra.reflect.literal_variants
  call $hydra.lib.sets.from_list
  local.set $literal_variants
  call $hydra.reflect.float_types
  call $hydra.lib.sets.from_list
  local.set $float_types
  call $hydra.reflect.function_variants
  call $hydra.lib.sets.from_list
  local.set $function_variants
  call $hydra.reflect.integer_types
  call $hydra.lib.sets.from_list
  local.set $integer_types
  call $hydra.reflect.term_variants
  call $hydra.lib.sets.from_list
  local.set $term_variants
  call $hydra.reflect.type_variants
  call $hydra.lib.sets.from_list
  local.set $type_variants
  (block $end_type (result i32)
  local.get $t
  br_table  $end_type
)
  local.set $types
  i32.const 0 ;; string: "hydra.core"
  local.get $elimination_variants
  local.get $literal_variants
  local.get $float_types
  local.get $function_variants
  local.get $integer_types
  local.get $term_variants
  local.get $type_variants
  local.get $types
)
)
