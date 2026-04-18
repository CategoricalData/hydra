(module
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\0a\00\00\00\68\79\64\72\61\2e\63\6f\72\65")
  (global $__bump_ptr (mut i32) i32.const 1040 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.languages.hydra_language" (func $hydra.languages.hydra_language) )
  (func $hydra.languages.hydra_language (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $elimination_variants i32)
  (local $float_types i32)
  (local $function_variants i32)
  (local $integer_types i32)
  (local $literal_variants i32)
  (local $t i32)
  (local $term_variants i32)
  (local $type_variants i32)
  (local $types i32)
  i32.const 0
  call $hydra.lib.sets.from_list
  local.set $elimination_variants
  i32.const 0
  call $hydra.lib.sets.from_list
  local.set $literal_variants
  i32.const 0
  call $hydra.lib.sets.from_list
  local.set $float_types
  i32.const 0
  call $hydra.lib.sets.from_list
  local.set $function_variants
  i32.const 0
  call $hydra.lib.sets.from_list
  local.set $integer_types
  i32.const 0
  call $hydra.lib.sets.from_list
  local.set $term_variants
  i32.const 0
  call $hydra.lib.sets.from_list
  local.set $type_variants
  local.get $t
  drop
  i32.const 0
  local.set $types
  i32.const 1024
  local.get $elimination_variants
  local.get $literal_variants
  local.get $float_types
  local.get $function_variants
  local.get $integer_types
  local.get $term_variants
  local.get $type_variants
  local.get $types
  i32.const 32
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=28
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=24
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=20
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=16
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=12
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
)
)
