(module
  (import "hydra.encode.core" "hydra.encode.core.name" (func $hydra.encode.core.name (param i32) (result i32) ) )
  (import "hydra.encode.core" "hydra.encode.core.term" (func $hydra.encode.core.term (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.bimap" (func $hydra.lib.maps.bimap (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.encode.context.context" (func $hydra.encode.context.context) )
  (export "hydra.encode.context.in_context" (func $hydra.encode.context.in_context) )
  (func $hydra.encode.context.context (param $x i32) (result i32)
  (local $m i32)
  (local $x2 i32)
  (local $xs i32)
  i32.const 0 ;; string: "hydra.context.Context"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "trace"
  local.get $x2
  local.get $xs
  call $hydra.lib.lists.map
  i32.const 0 ;; string: "messages"
  local.get $x2
  local.get $xs
  call $hydra.lib.lists.map
  i32.const 0 ;; string: "other"
  call $hydra.encode.core.name
  call $hydra.encode.core.term
  local.get $m
  call $hydra.lib.maps.bimap
)
  (func $hydra.encode.context.in_context (param $e i32) (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.context.InContext"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "object"
  local.get $x
  ;; project field: object
  local.get $e
  i32.const 0 ;; string: "context"
  local.get $x
  ;; project field: context
  call $hydra.encode.context.context
)
)
