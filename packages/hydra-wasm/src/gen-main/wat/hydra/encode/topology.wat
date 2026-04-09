(module
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.bimap" (func $hydra.lib.maps.bimap (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.map" (func $hydra.lib.sets.map (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.encode.topology.graph" (func $hydra.encode.topology.graph) )
  (export "hydra.encode.topology.tarjan_state" (func $hydra.encode.topology.tarjan_state) )
  (export "hydra.encode.topology.vertex" (func $hydra.encode.topology.vertex) )
  (func $hydra.encode.topology.graph (param $m i32) (result i32)
  (local $xs i32)
  call $hydra.encode.topology.vertex
  call $hydra.encode.topology.vertex
  local.get $xs
  call $hydra.lib.lists.map
  local.get $m
  call $hydra.lib.maps.bimap
)
  (func $hydra.encode.topology.tarjan_state (param $x i32) (result i32)
  (local $m i32)
  (local $s i32)
  (local $x2 i32)
  (local $xs i32)
  (local $xs2 i32)
  i32.const 0 ;; string: "hydra.topology.TarjanState"
  i32.const 6
  ;; list elements follow
  i32.const 0 ;; string: "counter"
  local.get $x2
  i32.const 0 ;; string: "indices"
  call $hydra.encode.topology.vertex
  local.get $x2
  local.get $m
  call $hydra.lib.maps.bimap
  i32.const 0 ;; string: "lowLinks"
  call $hydra.encode.topology.vertex
  local.get $x2
  local.get $m
  call $hydra.lib.maps.bimap
  i32.const 0 ;; string: "stack"
  call $hydra.encode.topology.vertex
  local.get $xs
  call $hydra.lib.lists.map
  i32.const 0 ;; string: "onStack"
  call $hydra.encode.topology.vertex
  local.get $s
  call $hydra.lib.sets.map
  i32.const 0 ;; string: "sccs"
  call $hydra.encode.topology.vertex
  local.get $xs2
  call $hydra.lib.lists.map
  local.get $xs
  call $hydra.lib.lists.map
)
  (func $hydra.encode.topology.vertex (param $x i32) (result i32)
  local.get $x
)
)
