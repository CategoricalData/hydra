(module
  (import "hydra.extract.core" "hydra.extract.core.unit_variant" (func $hydra.extract.core.unit_variant (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.extract.util.comparison" (func $hydra.extract.util.comparison) )
  (func $hydra.extract.util.comparison (param $cx i32) (param $graph i32) (param $term i32) (result i32)
  local.get $cx
  i32.const 0 ;; string: "hydra.util.Comparison"
  local.get $graph
  local.get $term
  call $hydra.extract.core.unit_variant
  nop
  i32.const 0 ;; string: "equalTo"
  call $hydra.lib.equality.equal
  i32.const 1
  i32.const 0
  nop
  i32.const 0 ;; string: "lessThan"
  call $hydra.lib.equality.equal
  i32.const 1
  i32.const 0
  nop
  i32.const 0 ;; string: "greaterThan"
  call $hydra.lib.equality.equal
  i32.const 1
  i32.const 0
  i32.const 0
  i32.const 0 ;; string: "expected comparison but found "
  nop
  call $hydra.lib.strings.cat2
  local.get $cx
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  call $hydra.lib.eithers.bind
)
)
