(module
  (import "hydra.decode.core" "hydra.decode.core.term" (func $hydra.decode.core.term (param i32) (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.require_field" (func $hydra.extract.core.require_field (param i32) (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.strip_with_decoding_error" (func $hydra.extract.core.strip_with_decoding_error (param i32) (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.to_field_map" (func $hydra.extract.core.to_field_map (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\0f\00\00\00\65\78\70\65\63\74\65\64\20\72\65\63\6f\72\64\15\00\00\00\65\78\70\65\63\74\65\64\20\77\72\61\70\70\65\64\20\74\79\70\65\04\00\00\00\6e\61\6d\65\04\00\00\00\74\65\72\6d")
  (global $__bump_ptr (mut i32) i32.const 1088 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.decode.phantoms.t_binding" (func $hydra.decode.phantoms.t_binding) )
  (export "hydra.decode.phantoms.t_term" (func $hydra.decode.phantoms.t_term) )
  (export "hydra.decode.phantoms.t_term_definition" (func $hydra.decode.phantoms.t_term_definition) )
  (func $hydra.decode.phantoms.t_binding (param $a i32) (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $field_term i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
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
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1068
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1076
  local.get $a
  i32.const 0
  i32.const 0
  call $hydra.decode.phantoms.t_term
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_name
  local.get $field_term
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.phantoms.t_term (param $a i32) (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (local $err i32)
  (local $stripped i32)
  (local $v i32)
  (local $wrapped_term i32)
  i32.const 0
  local.get $err
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
  (block $end_term (result i32)
  (block $wrap
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $wrap $wrap
)
  local.get $v
  drop
  local.get $b
  local.get $cx
  local.get $wrapped_term
  i32.load offset=4
  call $hydra.decode.core.term
  call $hydra.lib.eithers.map
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.phantoms.t_term_definition (param $a i32) (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $field_term i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
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
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1068
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1076
  local.get $a
  i32.const 0
  i32.const 0
  call $hydra.decode.phantoms.t_term
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_name
  local.get $field_term
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
)
