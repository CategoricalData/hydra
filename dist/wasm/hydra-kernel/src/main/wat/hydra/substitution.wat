(module
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.delete" (func $hydra.lib.maps.delete (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.filter_with_key" (func $hydra.lib.maps.filter_with_key (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.insert" (func $hydra.lib.maps.insert (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.map" (func $hydra.lib.maps.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.null" (func $hydra.lib.maps.null (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.singleton" (func $hydra.lib.maps.singleton (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.union" (func $hydra.lib.maps.union (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.is_nothing" (func $hydra.lib.maybes.is_nothing (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.member" (func $hydra.lib.sets.member (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.union" (func $hydra.lib.sets.union (param i32) (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_term" (func $hydra.rewriting.rewrite_term (param i32) (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_type" (func $hydra.rewriting.rewrite_type (param i32) (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.free_variables_in_type" (func $hydra.variables.free_variables_in_type (param i32) (result i32) ) )
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
  (export "hydra.substitution.compose_type_subst" (func $hydra.substitution.compose_type_subst) )
  (export "hydra.substitution.compose_type_subst_list" (func $hydra.substitution.compose_type_subst_list) )
  (export "hydra.substitution.compose_type_subst_non_empty" (func $hydra.substitution.compose_type_subst_non_empty) )
  (export "hydra.substitution.id_type_subst" (func $hydra.substitution.id_type_subst) )
  (export "hydra.substitution.singleton_type_subst" (func $hydra.substitution.singleton_type_subst) )
  (export "hydra.substitution.subst_in_class_constraints" (func $hydra.substitution.subst_in_class_constraints) )
  (export "hydra.substitution.subst_in_context" (func $hydra.substitution.subst_in_context) )
  (export "hydra.substitution.subst_in_type" (func $hydra.substitution.subst_in_type) )
  (export "hydra.substitution.subst_in_type_non_empty" (func $hydra.substitution.subst_in_type_non_empty) )
  (export "hydra.substitution.subst_in_type_scheme" (func $hydra.substitution.subst_in_type_scheme) )
  (export "hydra.substitution.subst_types_in_term" (func $hydra.substitution.subst_types_in_term) )
  (export "hydra.substitution.substitute_in_binding" (func $hydra.substitution.substitute_in_binding) )
  (export "hydra.substitution.substitute_in_constraint" (func $hydra.substitution.substitute_in_constraint) )
  (export "hydra.substitution.substitute_in_constraints" (func $hydra.substitution.substitute_in_constraints) )
  (export "hydra.substitution.substitute_in_term" (func $hydra.substitution.substitute_in_term) )
  (func $hydra.substitution.compose_type_subst (param $s1 i32) (param $s2 i32) (result i32)
  local.get $s1
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.maps.null
  local.get $s2
  local.get $s2
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.maps.null
  local.get $s1
  local.get $s1
  local.get $s2
  call $hydra.substitution.compose_type_subst_non_empty
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
)
  (func $hydra.substitution.compose_type_subst_list (result i32)
  i32.const 0
  i32.const 0
  i32.const 0
  call $hydra.lib.lists.foldl
)
  (func $hydra.substitution.compose_type_subst_non_empty (param $s1 i32) (param $s2 i32) (result i32)
  (local $is_extra i32)
  (local $k i32)
  (local $with_extra i32)
  local.get $k
  local.get $s1
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.is_nothing
  local.set $is_extra
  local.get $is_extra
  local.get $s2
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.maps.filter_with_key
  local.set $with_extra
  local.get $with_extra
  local.get $s2
  i32.const 0
  call $hydra.substitution.subst_in_type
  local.get $s1
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.maps.map
  call $hydra.lib.maps.union
)
  (func $hydra.substitution.id_type_subst (result i32)
  i32.const 0
)
  (func $hydra.substitution.singleton_type_subst (param $v i32) (param $t i32) (result i32)
  local.get $v
  local.get $t
  call $hydra.lib.maps.singleton
)
  (func $hydra.substitution.subst_in_class_constraints (param $subst i32) (param $constraints i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $acc i32)
  (local $acc2 i32)
  (local $existing i32)
  (local $free_var i32)
  (local $free_vars i32)
  (local $insert_or_merge i32)
  (local $merged i32)
  (local $metadata i32)
  (local $pair i32)
  (local $subst_map i32)
  (local $target_type i32)
  (local $var_name i32)
  local.get $subst
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $subst_map
  local.get $var_name
  local.get $metadata
  local.get $acc
  call $hydra.lib.maps.insert
  local.get $existing
  i32.load
  local.get $metadata
  i32.load
  call $hydra.lib.sets.union
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.set $merged
  local.get $var_name
  local.get $merged
  local.get $acc
  call $hydra.lib.maps.insert
  local.get $var_name
  local.get $acc
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  local.set $insert_or_merge
  local.get $pair
  call $hydra.lib.pairs.first
  local.set $var_name
  local.get $pair
  call $hydra.lib.pairs.second
  local.set $metadata
  local.get $var_name
  drop
  local.get $metadata
  drop
  local.get $acc
  drop
  local.get $insert_or_merge
  drop
  i32.const 0
  local.get $target_type
  call $hydra.variables.free_variables_in_type
  call $hydra.lib.sets.to_list
  local.set $free_vars
  local.get $free_var
  drop
  local.get $metadata
  drop
  local.get $acc2
  drop
  local.get $insert_or_merge
  drop
  i32.const 0
  local.get $acc
  local.get $free_vars
  call $hydra.lib.lists.foldl
  local.get $var_name
  local.get $subst_map
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  i32.const 0
  local.get $constraints
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.foldl
)
  (func $hydra.substitution.subst_in_context (param $subst i32) (param $cx i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $cx2 i32)
  (local $new_bound_types i32)
  (local $new_class_constraints i32)
  local.get $subst
  i32.const 0
  call $hydra.substitution.subst_in_type_scheme
  local.get $cx
  i32.load offset=4
  call $hydra.lib.maps.map
  local.set $new_bound_types
  local.get $subst
  local.get $cx
  i32.load offset=8
  call $hydra.substitution.subst_in_class_constraints
  local.set $new_class_constraints
  local.get $cx
  i32.load
  local.get $new_bound_types
  local.get $cx
  i32.load offset=8
  local.get $cx
  i32.load offset=12
  local.get $cx
  i32.load offset=16
  local.get $cx
  i32.load offset=20
  local.get $cx
  i32.load offset=24
  local.get $cx
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
  local.set $cx2
  local.get $cx2
  i32.load
  local.get $cx2
  i32.load offset=4
  local.get $new_class_constraints
  local.get $cx2
  i32.load offset=12
  local.get $cx2
  i32.load offset=16
  local.get $cx2
  i32.load offset=20
  local.get $cx2
  i32.load offset=24
  local.get $cx2
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
  (func $hydra.substitution.subst_in_type (param $subst i32) (param $typ0 i32) (result i32)
  local.get $subst
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.maps.null
  local.get $typ0
  local.get $subst
  local.get $typ0
  call $hydra.substitution.subst_in_type_non_empty
  call $hydra.lib.logic.if_else
)
  (func $hydra.substitution.subst_in_type_non_empty (param $subst i32) (param $typ0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $lt i32)
  (local $recurse i32)
  (local $remove_var i32)
  (local $rewrite i32)
  (local $styp i32)
  (local $typ i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $variable
  (block $forall
  local.get $typ
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $forall $variable $variable
)
  local.get $v
  drop
  local.get $typ
  drop
  local.get $recurse
  drop
  i32.const 0
  i32.const 3
  local.get $lt
  i32.load
  local.get $lt
  i32.load
  drop
  local.get $remove_var
  drop
  i32.const 0
  local.get $lt
  i32.load offset=4
  call $hydra.substitution.subst_in_type
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
  local.get $lt
  i32.load
  local.get $subst
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  br $end_type
)
  local.get $v
  drop
  local.get $typ
  local.get $styp
  local.get $v
  local.get $subst
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  br $end_type
)
  local.set $rewrite
  local.get $v
  local.get $subst
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.maps.delete
  local.set $remove_var
  local.get $rewrite
  local.get $typ0
  call $hydra.rewriting.rewrite_type
)
  (func $hydra.substitution.subst_in_type_scheme (param $subst i32) (param $ts i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  local.get $ts
  i32.load
  local.get $subst
  local.get $ts
  i32.load offset=4
  call $hydra.substitution.subst_in_type
  local.get $subst
  i32.const 0
  call $hydra.substitution.subst_in_class_constraints
  local.get $ts
  i32.load offset=8
  call $hydra.lib.maybes.map
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
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
  (func $hydra.substitution.subst_types_in_term (param $subst i32) (param $term0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (local $dflt i32)
  (local $for_lambda i32)
  (local $for_let i32)
  (local $for_type_application i32)
  (local $for_type_lambda i32)
  (local $l i32)
  (local $param i32)
  (local $recurse i32)
  (local $rewrite i32)
  (local $rewrite_binding i32)
  (local $subst2 i32)
  (local $ta i32)
  (local $term i32)
  (local $tl i32)
  (local $tt i32)
  (local $v i32)
  local.get $term
  drop
  local.get $recurse
  drop
  i32.const 0
  local.set $dflt
  i32.const 5
  local.get $l
  i32.load
  local.get $subst
  i32.const 0
  call $hydra.substitution.subst_in_type
  local.get $l
  i32.load offset=4
  call $hydra.lib.maybes.map
  local.get $subst
  local.get $l
  i32.load offset=8
  call $hydra.substitution.subst_types_in_term
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
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
  local.set $for_lambda
  local.get $b
  i32.load
  local.get $subst
  local.get $b
  i32.load offset=4
  call $hydra.substitution.subst_types_in_term
  local.get $subst
  i32.const 0
  call $hydra.substitution.subst_in_type_scheme
  local.get $b
  i32.load offset=8
  call $hydra.lib.maybes.map
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
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
  local.set $rewrite_binding
  i32.const 6
  local.get $rewrite_binding
  local.get $l
  i32.load
  call $hydra.lib.lists.map
  local.get $subst
  local.get $l
  i32.load offset=4
  call $hydra.substitution.subst_types_in_term
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
  local.set $for_let
  i32.const 15
  local.get $subst
  local.get $tt
  i32.load
  call $hydra.substitution.subst_types_in_term
  local.get $subst
  local.get $tt
  i32.load offset=4
  call $hydra.substitution.subst_in_type
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
  local.set $for_type_application
  local.get $ta
  i32.load
  local.set $param
  local.get $param
  local.get $subst
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.maps.delete
  local.set $subst2
  i32.const 16
  local.get $param
  local.get $subst2
  local.get $ta
  i32.load offset=4
  call $hydra.substitution.subst_types_in_term
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
  local.set $for_type_lambda
  (block $end_term (result i32)
  (block $type_lambda
  (block $type_application
  (block $let
  (block $lambda
  local.get $term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $lambda $let $type_application $type_lambda $type_lambda
)
  local.get $v
  drop
  local.get $l
  drop
  local.get $for_lambda
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $l
  drop
  local.get $for_let
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $ta
  drop
  local.get $for_type_application
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $tl
  drop
  local.get $for_type_lambda
  drop
  i32.const 0
  br $end_term
)
  local.set $rewrite
  local.get $rewrite
  local.get $term0
  call $hydra.rewriting.rewrite_term
)
  (func $hydra.substitution.substitute_in_binding (param $subst i32) (param $b i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  local.get $b
  i32.load
  local.get $subst
  local.get $b
  i32.load offset=4
  call $hydra.substitution.substitute_in_term
  local.get $b
  i32.load offset=8
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
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
  (func $hydra.substitution.substitute_in_constraint (param $subst i32) (param $c i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  local.get $subst
  local.get $c
  i32.load
  call $hydra.substitution.subst_in_type
  local.get $subst
  local.get $c
  i32.load offset=4
  call $hydra.substitution.subst_in_type
  local.get $c
  i32.load offset=8
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
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
  (func $hydra.substitution.substitute_in_constraints (param $subst i32) (param $cs i32) (result i32)
  local.get $subst
  i32.const 0
  call $hydra.substitution.substitute_in_constraint
  local.get $cs
  call $hydra.lib.lists.map
)
  (func $hydra.substitution.substitute_in_term (param $subst i32) (param $term0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (local $bindings i32)
  (local $k i32)
  (local $l i32)
  (local $lt i32)
  (local $name i32)
  (local $names i32)
  (local $recurse i32)
  (local $rewrite i32)
  (local $rewrite_binding i32)
  (local $s i32)
  (local $sterm i32)
  (local $subst2 i32)
  (local $term i32)
  (local $v i32)
  (local $with_lambda i32)
  (local $with_let i32)
  local.get $subst
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $s
  local.get $l
  i32.load
  local.set $v
  local.get $v
  local.get $s
  call $hydra.lib.maps.delete
  local.set $subst2
  i32.const 5
  local.get $v
  local.get $l
  i32.load offset=4
  local.get $subst2
  local.get $l
  i32.load offset=8
  call $hydra.substitution.substitute_in_term
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
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
  local.set $with_lambda
  local.get $lt
  i32.load
  local.set $bindings
  i32.const 0
  local.get $bindings
  call $hydra.lib.lists.map
  call $hydra.lib.sets.from_list
  local.set $names
  local.get $k
  local.get $names
  call $hydra.lib.sets.member
  call $hydra.lib.logic.not
  local.get $s
  call $hydra.lib.maps.filter_with_key
  local.set $subst2
  local.get $b
  i32.load
  local.get $subst2
  local.get $b
  i32.load offset=4
  call $hydra.substitution.substitute_in_term
  local.get $b
  i32.load offset=8
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
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
  local.set $rewrite_binding
  i32.const 6
  local.get $rewrite_binding
  local.get $bindings
  call $hydra.lib.lists.map
  local.get $subst2
  local.get $lt
  i32.load offset=4
  call $hydra.substitution.substitute_in_term
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
  local.set $with_let
  (block $end_term (result i32)
  (block $variable
  (block $let
  (block $lambda
  local.get $term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $lambda $let $variable $variable
)
  local.get $v
  drop
  local.get $l
  drop
  local.get $with_lambda
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $l
  drop
  local.get $with_let
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $term
  drop
  local.get $recurse
  drop
  i32.const 0
  local.get $sterm
  local.get $name
  local.get $s
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.set $rewrite
  local.get $rewrite
  local.get $term0
  call $hydra.rewriting.rewrite_term
)
)
