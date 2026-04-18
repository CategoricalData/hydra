(module
  (import "hydra.extract.core" "hydra.extract.core.decode_list" (func $hydra.extract.core.decode_list (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.decode_map" (func $hydra.extract.core.decode_map (param i32) (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.require_field" (func $hydra.extract.core.require_field (param i32) (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.strip_with_decoding_error" (func $hydra.extract.core.strip_with_decoding_error (param i32) (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.to_field_map" (func $hydra.extract.core.to_field_map (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (param i32) (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\07\00\00\00\63\6f\6e\74\65\78\74\10\00\00\00\65\78\70\65\63\74\65\64\20\6c\69\74\65\72\61\6c\0f\00\00\00\65\78\70\65\63\74\65\64\20\72\65\63\6f\72\64\17\00\00\00\65\78\70\65\63\74\65\64\20\73\74\72\69\6e\67\20\6c\69\74\65\72\61\6c\08\00\00\00\6d\65\73\73\61\67\65\73\06\00\00\00\6f\62\6a\65\63\74\05\00\00\00\6f\74\68\65\72\05\00\00\00\74\72\61\63\65")
  (global $__bump_ptr (mut i32) i32.const 1152 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.decode.context.context" (func $hydra.decode.context.context) )
  (export "hydra.decode.context.in_context" (func $hydra.decode.context.in_context) )
  (func $hydra.decode.context.context (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $cx2 i32)
  (local $err i32)
  (local $field_map i32)
  (local $field_messages i32)
  (local $field_other i32)
  (local $field_trace i32)
  (local $raw2 i32)
  (local $record i32)
  (local $s i32)
  (local $stripped i32)
  (local $stripped2 i32)
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
  i32.const 1132
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
  (block $literal
  local.get $stripped2
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $literal $literal
)
  local.get $v
  drop
  (block $end_literal (result i32)
  (block $string
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $string $string
)
  local.get $v
  drop
  i32.const 1
  local.get $s
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
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
  i32.const 0
  i32.const 0
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1101
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
  (block $literal
  local.get $stripped2
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $literal $literal
)
  local.get $v
  drop
  (block $end_literal (result i32)
  (block $string
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $string $string
)
  local.get $v
  drop
  i32.const 1
  local.get $s
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
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
  i32.const 0
  i32.const 0
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1123
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  call $hydra.extract.core.decode_map
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_trace
  local.get $field_messages
  local.get $field_other
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.context.in_context (param $e i32) (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_context i32)
  (local $field_map i32)
  (local $field_object i32)
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
  i32.const 1113
  local.get $e
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1024
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_object
  local.get $field_context
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
