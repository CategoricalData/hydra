(module
  (import "hydra.extract.core" "hydra.extract.core.decode_list" (func $hydra.extract.core.decode_list (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.decode_map" (func $hydra.extract.core.decode_map (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.decode_set" (func $hydra.extract.core.decode_set (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.require_field" (func $hydra.extract.core.require_field (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.to_field_map" (func $hydra.extract.core.to_field_map (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.strip_and_dereference_term_either" (func $hydra.lexical.strip_and_dereference_term_either (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.decode.topology.graph" (func $hydra.decode.topology.graph) )
  (export "hydra.decode.topology.tarjan_state" (func $hydra.decode.topology.tarjan_state) )
  (export "hydra.decode.topology.vertex" (func $hydra.decode.topology.vertex) )
  (func $hydra.decode.topology.graph (result i32)
  call $hydra.decode.topology.vertex
  call $hydra.decode.topology.vertex
  call $hydra.extract.core.decode_list
  call $hydra.extract.core.decode_map
)
  (func $hydra.decode.topology.tarjan_state (param $cx i32) (param $raw i32) (result i32)
  (local $cx2 i32)
  (local $err i32)
  (local $field_counter i32)
  (local $field_indices i32)
  (local $field_low_links i32)
  (local $field_map i32)
  (local $field_on_stack i32)
  (local $field_sccs i32)
  (local $field_stack i32)
  (local $i i32)
  (local $raw2 i32)
  (local $record i32)
  (local $stripped i32)
  (local $stripped2 i32)
  (local $v i32)
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
  i32.const 0 ;; string: "counter"
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  br_table $literal $literal
)
  (block $end_literal (result i32)
  (block $integer
  local.get $v
  br_table $integer $integer
)
  (block $end_integer_value (result i32)
  (block $int32
  local.get $v
  br_table $int32 $int32
)
  i32.const 1
  local.get $i
  br $end_integer_value
)
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "indices"
  call $hydra.decode.topology.vertex
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  br_table $literal $literal
)
  (block $end_literal (result i32)
  (block $integer
  local.get $v
  br_table $integer $integer
)
  (block $end_integer_value (result i32)
  (block $int32
  local.get $v
  br_table $int32 $int32
)
  i32.const 1
  local.get $i
  br $end_integer_value
)
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
  call $hydra.extract.core.decode_map
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "lowLinks"
  call $hydra.decode.topology.vertex
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  br_table $literal $literal
)
  (block $end_literal (result i32)
  (block $integer
  local.get $v
  br_table $integer $integer
)
  (block $end_integer_value (result i32)
  (block $int32
  local.get $v
  br_table $int32 $int32
)
  i32.const 1
  local.get $i
  br $end_integer_value
)
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
  call $hydra.extract.core.decode_map
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "stack"
  call $hydra.decode.topology.vertex
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "onStack"
  call $hydra.decode.topology.vertex
  call $hydra.extract.core.decode_set
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "sccs"
  call $hydra.decode.topology.vertex
  call $hydra.extract.core.decode_list
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_counter
  local.get $field_indices
  local.get $field_low_links
  local.get $field_stack
  local.get $field_on_stack
  local.get $field_sccs
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.topology.vertex (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $i i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $literal
  local.get $stripped
  br_table $literal $literal
)
  (block $end_literal (result i32)
  (block $integer
  local.get $v
  br_table $integer $integer
)
  (block $end_integer_value (result i32)
  (block $int32
  local.get $v
  br_table $int32 $int32
)
  i32.const 1
  local.get $i
  br $end_integer_value
)
  br $end_literal
)
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
)
