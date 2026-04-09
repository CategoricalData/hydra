(module
  (import "hydra.decode.core" "hydra.decode.core.name" (func $hydra.decode.core.name (param i32) (result i32) ) )
  (import "hydra.decode.core" "hydra.decode.core.term" (func $hydra.decode.core.term (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.require_field" (func $hydra.extract.core.require_field (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.to_field_map" (func $hydra.extract.core.to_field_map (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.strip_and_dereference_term_either" (func $hydra.lexical.strip_and_dereference_term_either (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.decode.phantoms.t_binding" (func $hydra.decode.phantoms.t_binding) )
  (export "hydra.decode.phantoms.t_term" (func $hydra.decode.phantoms.t_term) )
  (export "hydra.decode.phantoms.t_term_definition" (func $hydra.decode.phantoms.t_term_definition) )
  (func $hydra.decode.phantoms.t_binding (param $a i32) (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $field_term i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "name"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "term"
  local.get $a
  call $hydra.decode.phantoms.t_term
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_name
  local.get $field_term
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.phantoms.t_term (param $a i32) (param $cx i32) (param $raw i32) (result i32)
  (local $b i32)
  (local $err i32)
  (local $stripped i32)
  (local $wrapped_term i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $wrap
  local.get $stripped
  br_table $wrap $wrap
)
  local.get $b
  local.get $cx
  local.get $wrapped_term
  ;; project field: body
  call $hydra.decode.core.term
  call $hydra.lib.eithers.map
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.phantoms.t_term_definition (param $a i32) (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $field_term i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "name"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "term"
  local.get $a
  call $hydra.decode.phantoms.t_term
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_name
  local.get $field_term
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
)
