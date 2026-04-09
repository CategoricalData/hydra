(module
  (import "hydra.extract.core" "hydra.extract.core.decode_unit" (func $hydra.extract.core.decode_unit (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.strip_and_dereference_term_either" (func $hydra.lexical.strip_and_dereference_term_either (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.decode.classes.type_class" (func $hydra.decode.classes.type_class) )
  (func $hydra.decode.classes.type_class (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $f i32)
  (local $field i32)
  (local $fname i32)
  (local $fterm i32)
  (local $inj i32)
  (local $input i32)
  (local $stripped i32)
  (local $t i32)
  (local $variant_map i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $union
  local.get $stripped
  br_table $union $union
)
  local.get $inj
  ;; project field: field
  local.set $field
  local.get $field
  ;; project field: name
  local.set $fname
  local.get $field
  ;; project field: term
  local.set $fterm
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "equality"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "ordering"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  call $hydra.lib.maps.from_list
  local.set $variant_map
  i32.const 0
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "no such field "
  nop
  i32.const 0 ;; string: " in union"
  call $hydra.lib.strings.cat
  local.get $fterm
  local.get $f
  local.get $fname
  local.get $variant_map
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
)
