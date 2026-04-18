(module
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.intercalate" (func $hydra.lib.strings.intercalate (param i32) (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\02\00\00\00\2c\20\01\00\00\00\7b\01\00\00\00\7d")
  (global $__bump_ptr (mut i32) i32.const 1040 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.show.graph.graph" (func $hydra.show.graph.graph) )
  (func $hydra.show.graph.graph (param $elements i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $element_strs i32)
  i32.const 0
  local.get $elements
  call $hydra.lib.lists.map
  local.set $element_strs
  i32.const 1030
  i32.const 1024
  local.get $element_strs
  call $hydra.lib.strings.intercalate
  i32.const 1035
  i32.const 16
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 3
  i32.store
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
  local.get $__rec_ptr
  call $hydra.lib.strings.cat
)
)
