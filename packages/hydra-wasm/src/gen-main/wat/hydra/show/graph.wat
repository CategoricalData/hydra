(module
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.intercalate" (func $hydra.lib.strings.intercalate (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.binding" (func $hydra.show.core.binding (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.show.graph.graph" (func $hydra.show.graph.graph) )
  (func $hydra.show.graph.graph (param $elements i32) (result i32)
  (local $element_strs i32)
  call $hydra.show.core.binding
  local.get $elements
  call $hydra.lib.lists.map
  local.set $element_strs
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "{"
  i32.const 0 ;; string: ", "
  local.get $element_strs
  call $hydra.lib.strings.intercalate
  i32.const 0 ;; string: "}"
  call $hydra.lib.strings.cat
)
)
