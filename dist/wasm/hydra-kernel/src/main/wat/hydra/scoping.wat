(module
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.reverse" (func $hydra.lib.lists.reverse (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.delete" (func $hydra.lib.maps.delete (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.insert" (func $hydra.lib.maps.insert (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.union" (func $hydra.lib.maps.union (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cat" (func $hydra.lib.maybes.cat (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.delete" (func $hydra.lib.sets.delete (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.insert" (func $hydra.lib.sets.insert (param i32) (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "")
  (global $__bump_ptr (mut i32) i32.const 1024 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.scoping.extend_graph_for_lambda" (func $hydra.scoping.extend_graph_for_lambda) )
  (export "hydra.scoping.extend_graph_for_let" (func $hydra.scoping.extend_graph_for_let) )
  (export "hydra.scoping.extend_graph_for_type_lambda" (func $hydra.scoping.extend_graph_for_type_lambda) )
  (export "hydra.scoping.extend_graph_with_bindings" (func $hydra.scoping.extend_graph_with_bindings) )
  (export "hydra.scoping.f_type_to_type_scheme" (func $hydra.scoping.f_type_to_type_scheme) )
  (export "hydra.scoping.type_scheme_to_f_type" (func $hydra.scoping.type_scheme_to_f_type) )
  (func $hydra.scoping.extend_graph_for_lambda (param $g i32) (param $lam i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $dom i32)
  (local $var i32)
  local.get $lam
  i32.load
  local.set $var
  local.get $g
  i32.load
  local.get $g
  i32.load offset=4
  local.get $var
  local.get $dom
  call $hydra.scoping.f_type_to_type_scheme
  local.get $g
  i32.load offset=4
  call $hydra.lib.maps.insert
  local.get $lam
  i32.load offset=4
  call $hydra.lib.maybes.maybe
  local.get $g
  i32.load offset=8
  local.get $var
  local.get $g
  i32.load offset=12
  call $hydra.lib.sets.insert
  local.get $var
  local.get $g
  i32.load offset=16
  call $hydra.lib.maps.delete
  local.get $g
  i32.load offset=20
  local.get $g
  i32.load offset=24
  local.get $g
  i32.load offset=28
  i32.const 32
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=28
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=24
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=20
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=16
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
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
)
  (func $hydra.scoping.extend_graph_for_let (param $for_binding i32) (param $g i32) (param $letrec i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (local $bindings i32)
  (local $g2 i32)
  (local $g_acc i32)
  (local $m i32)
  (local $new_meta i32)
  (local $s i32)
  (local $t i32)
  (local $ts i32)
  local.get $letrec
  i32.load
  local.set $bindings
  local.get $bindings
  local.get $g
  call $hydra.scoping.extend_graph_with_bindings
  local.set $g2
  local.get $b
  i32.load
  local.get $b
  i32.load offset=4
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
  local.get $bindings
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.get $g
  i32.load
  call $hydra.lib.maps.union
  local.get $b
  i32.load
  local.get $ts
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
  local.get $b
  i32.load offset=8
  call $hydra.lib.maybes.map
  local.get $bindings
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  call $hydra.lib.maps.from_list
  local.get $g
  i32.load offset=4
  call $hydra.lib.maps.union
  local.get $g
  i32.load offset=8
  local.get $b
  i32.load
  local.get $s
  call $hydra.lib.sets.delete
  local.get $g
  i32.load offset=12
  local.get $bindings
  call $hydra.lib.lists.foldl
  local.get $g_acc
  i32.load offset=16
  local.set $m
  local.get $b
  i32.load
  local.get $m
  call $hydra.lib.maps.delete
  local.get $b
  i32.load
  local.get $t
  local.get $m
  call $hydra.lib.maps.insert
  local.get $g_acc
  drop
  local.get $b
  drop
  local.get $for_binding
  drop
  i32.const 0
  call $hydra.lib.maybes.maybe
  local.set $new_meta
  local.get $g_acc
  i32.load
  local.get $g_acc
  i32.load offset=4
  local.get $g_acc
  i32.load offset=8
  local.get $g_acc
  i32.load offset=12
  local.get $new_meta
  local.get $g_acc
  i32.load offset=20
  local.get $g_acc
  i32.load offset=24
  local.get $g_acc
  i32.load offset=28
  i32.const 32
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=28
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=24
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=20
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=16
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
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.get $g2
  local.get $bindings
  call $hydra.lib.lists.foldl
  i32.load offset=16
  local.get $g
  i32.load offset=20
  local.get $g
  i32.load offset=24
  local.get $g
  i32.load offset=28
  i32.const 32
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=28
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=24
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=20
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=16
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
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
)
  (func $hydra.scoping.extend_graph_for_type_lambda (param $g i32) (param $tlam i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $name i32)
  local.get $tlam
  i32.load
  local.set $name
  local.get $g
  i32.load
  local.get $g
  i32.load offset=4
  local.get $g
  i32.load offset=8
  local.get $g
  i32.load offset=12
  local.get $g
  i32.load offset=16
  local.get $g
  i32.load offset=20
  local.get $g
  i32.load offset=24
  local.get $name
  local.get $g
  i32.load offset=28
  call $hydra.lib.sets.insert
  i32.const 32
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=28
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=24
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=20
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=16
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
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
)
  (func $hydra.scoping.extend_graph_with_bindings (param $bindings i32) (param $g i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (local $new_terms i32)
  (local $new_types i32)
  (local $ts i32)
  local.get $b
  i32.load
  local.get $b
  i32.load offset=4
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
  local.get $bindings
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $new_terms
  local.get $b
  i32.load
  local.get $ts
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
  local.get $b
  i32.load offset=8
  call $hydra.lib.maybes.map
  local.get $bindings
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  call $hydra.lib.maps.from_list
  local.set $new_types
  local.get $new_terms
  local.get $g
  i32.load
  call $hydra.lib.maps.union
  local.get $new_types
  local.get $g
  i32.load offset=4
  call $hydra.lib.maps.union
  local.get $g
  i32.load offset=8
  local.get $g
  i32.load offset=12
  local.get $g
  i32.load offset=16
  local.get $g
  i32.load offset=20
  local.get $g
  i32.load offset=24
  local.get $g
  i32.load offset=28
  i32.const 32
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=28
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=24
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=20
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=16
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
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
)
  (func $hydra.scoping.f_type_to_type_scheme (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $at i32)
  (local $ft i32)
  (local $gather_forall i32)
  (local $strip_annotations i32)
  (local $t i32)
  (local $typ2 i32)
  (local $v i32)
  (local $vars i32)
  (block $end_type (result i32)
  (block $annotated
  local.get $t
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $annotated
)
  local.get $v
  drop
  local.get $at
  i32.load
  drop
  local.get $strip_annotations
  drop
  i32.const 0
  br $end_type
)
  local.set $strip_annotations
  (block $end_type (result i32)
  (block $forall
  local.get $typ2
  drop
  local.get $strip_annotations
  drop
  i32.const 0
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $forall $forall
)
  local.get $v
  drop
  local.get $ft
  i32.load
  local.get $vars
  call $hydra.lib.lists.cons
  drop
  local.get $ft
  i32.load offset=4
  drop
  local.get $gather_forall
  drop
  i32.const 0
  br $end_type
)
  local.set $gather_forall
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  drop
  local.get $typ
  drop
  local.get $gather_forall
  drop
  i32.const 0
)
  (func $hydra.scoping.type_scheme_to_f_type (param $ts i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $body i32)
  (local $t i32)
  (local $v i32)
  (local $vars i32)
  local.get $ts
  i32.load
  local.set $vars
  local.get $ts
  i32.load offset=4
  local.set $body
  i32.const 3
  local.get $v
  local.get $t
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
  local.get $body
  local.get $vars
  call $hydra.lib.lists.reverse
  call $hydra.lib.lists.foldl
)
)
