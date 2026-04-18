(module
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.intercalate" (func $hydra.lib.strings.intercalate (param i32) (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.type" (func $hydra.show.core.type (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\01\00\00\00\2c\01\00\00\00\7b\01\00\00\00\7d\01\00\00\00\a6\01\00\00\00\61")
  (global $__bump_ptr (mut i32) i32.const 1056 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.show.typing.type_constraint" (func $hydra.show.typing.type_constraint) )
  (export "hydra.show.typing.type_subst" (func $hydra.show.typing.type_subst) )
  (func $hydra.show.typing.type_constraint (param $tc i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $ltyp i32)
  (local $rtyp i32)
  local.get $tc
  i32.load
  local.set $ltyp
  local.get $tc
  i32.load offset=4
  local.set $rtyp
  local.get $ltyp
  call $hydra.show.core.type
  i32.const 1044
  local.get $rtyp
  call $hydra.show.core.type
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
  (func $hydra.show.typing.type_subst (param $ts i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $name i32)
  (local $pair i32)
  (local $pair_strs i32)
  (local $pairs i32)
  (local $show_pair i32)
  (local $subst i32)
  (local $typ i32)
  local.get $ts
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $subst
  local.get $subst
  call $hydra.lib.maps.to_list
  local.set $pairs
  local.get $pair
  call $hydra.lib.pairs.first
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $name
  local.get $pair
  call $hydra.lib.pairs.second
  local.set $typ
  local.get $name
  i32.const 1039
  local.get $typ
  call $hydra.show.core.type
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
  local.set $show_pair
  local.get $show_pair
  local.get $pairs
  call $hydra.lib.lists.map
  local.set $pair_strs
  i32.const 1029
  i32.const 1024
  local.get $pair_strs
  call $hydra.lib.strings.intercalate
  i32.const 1034
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
