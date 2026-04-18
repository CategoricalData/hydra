(module
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.nub" (func $hydra.lib.lists.nub (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.reverse" (func $hydra.lib.lists.reverse (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.uncons" (func $hydra.lib.lists.uncons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.zip" (func $hydra.lib.lists.zip (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int32" (func $hydra.lib.literals.show_int32 (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.insert" (func $hydra.lib.maps.insert (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.member" (func $hydra.lib.maps.member (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.union" (func $hydra.lib.maps.union (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.add" (func $hydra.lib.math.add (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.sub" (func $hydra.lib.math.sub (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.from_maybe" (func $hydra.lib.maybes.from_maybe (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.delete" (func $hydra.lib.sets.delete (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.difference" (func $hydra.lib.sets.difference (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.insert" (func $hydra.lib.sets.insert (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.member" (func $hydra.lib.sets.member (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.singleton" (func $hydra.lib.sets.singleton (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.union" (func $hydra.lib.sets.union (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.fold_over_type" (func $hydra.rewriting.fold_over_type (param i32) (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_term" (func $hydra.rewriting.rewrite_term (param i32) (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_term_with_context" (func $hydra.rewriting.rewrite_term_with_context (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_type" (func $hydra.rewriting.rewrite_type (param i32) (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.subterms" (func $hydra.rewriting.subterms (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.subtypes" (func $hydra.rewriting.subtypes (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\01\00\00\00\74")
  (global $__bump_ptr (mut i32) i32.const 1040 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.variables.free_type_variables_in_term" (func $hydra.variables.free_type_variables_in_term) )
  (export "hydra.variables.free_variables_in_term" (func $hydra.variables.free_variables_in_term) )
  (export "hydra.variables.free_variables_in_type" (func $hydra.variables.free_variables_in_type) )
  (export "hydra.variables.free_variables_in_type_ordered" (func $hydra.variables.free_variables_in_type_ordered) )
  (export "hydra.variables.free_variables_in_type_scheme" (func $hydra.variables.free_variables_in_type_scheme) )
  (export "hydra.variables.free_variables_in_type_scheme_simple" (func $hydra.variables.free_variables_in_type_scheme_simple) )
  (export "hydra.variables.free_variables_in_type_simple" (func $hydra.variables.free_variables_in_type_simple) )
  (export "hydra.variables.is_free_variable_in_term" (func $hydra.variables.is_free_variable_in_term) )
  (export "hydra.variables.normalize_type_variables_in_term" (func $hydra.variables.normalize_type_variables_in_term) )
  (export "hydra.variables.replace_free_term_variable" (func $hydra.variables.replace_free_term_variable) )
  (export "hydra.variables.replace_free_type_variable" (func $hydra.variables.replace_free_type_variable) )
  (export "hydra.variables.substitute_type_variables" (func $hydra.variables.substitute_type_variables) )
  (export "hydra.variables.substitute_type_variables_in_term" (func $hydra.variables.substitute_type_variables_in_term) )
  (export "hydra.variables.substitute_variable" (func $hydra.variables.substitute_variable) )
  (export "hydra.variables.substitute_variables" (func $hydra.variables.substitute_variables) )
  (export "hydra.variables.unshadow_variables" (func $hydra.variables.unshadow_variables) )
  (func $hydra.variables.free_type_variables_in_term (param $term0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $all_of i32)
  (local $b i32)
  (local $dflt i32)
  (local $domt i32)
  (local $for_binding i32)
  (local $get_all i32)
  (local $l i32)
  (local $new_vars i32)
  (local $recurse i32)
  (local $sets i32)
  (local $term i32)
  (local $tl i32)
  (local $try_type i32)
  (local $ts i32)
  (local $tt i32)
  (local $tvars i32)
  (local $typ i32)
  (local $v i32)
  (local $vars i32)
  i32.const 0
  i32.const 0
  local.get $sets
  call $hydra.lib.lists.foldl
  local.set $all_of
  local.get $typ
  call $hydra.variables.free_variables_in_type
  local.get $tvars
  call $hydra.lib.sets.difference
  local.set $try_type
  local.get $vars
  drop
  local.get $get_all
  drop
  i32.const 0
  local.set $recurse
  local.get $recurse
  local.get $term
  call $hydra.rewriting.subterms
  call $hydra.lib.lists.map
  drop
  local.get $all_of
  drop
  i32.const 0
  local.set $dflt
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
  i32.const 0
  local.get $vars
  drop
  local.get $try_type
  drop
  i32.const 0
  local.get $l
  i32.load offset=4
  call $hydra.lib.maybes.maybe
  local.set $domt
  local.get $domt
  local.get $l
  i32.load offset=8
  drop
  local.get $recurse
  drop
  i32.const 0
  call $hydra.lib.sets.union
  br $end_term
)
  local.get $v
  drop
  local.get $vars
  local.get $vars
  local.get $ts
  i32.load
  call $hydra.lib.sets.from_list
  call $hydra.lib.sets.union
  local.get $b
  i32.load offset=8
  call $hydra.lib.maybes.maybe
  local.set $new_vars
  local.get $new_vars
  drop
  local.get $b
  i32.load offset=4
  drop
  local.get $get_all
  drop
  i32.const 0
  i32.const 0
  local.get $new_vars
  drop
  local.get $ts
  i32.load offset=4
  drop
  local.get $try_type
  drop
  i32.const 0
  local.get $b
  i32.load offset=8
  call $hydra.lib.maybes.maybe
  call $hydra.lib.sets.union
  local.set $for_binding
  local.get $for_binding
  local.get $l
  i32.load
  call $hydra.lib.lists.map
  drop
  local.get $all_of
  drop
  i32.const 0
  local.get $l
  i32.load offset=4
  drop
  local.get $recurse
  drop
  i32.const 0
  call $hydra.lib.sets.union
  br $end_term
)
  local.get $v
  drop
  local.get $vars
  drop
  local.get $tt
  i32.load offset=4
  drop
  local.get $try_type
  drop
  i32.const 0
  local.get $tt
  i32.load
  drop
  local.get $recurse
  drop
  i32.const 0
  call $hydra.lib.sets.union
  br $end_term
)
  local.get $v
  drop
  local.get $vars
  drop
  i32.const 14
  local.get $tl
  i32.load
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
  drop
  local.get $try_type
  drop
  i32.const 0
  local.get $tl
  i32.load offset=4
  drop
  local.get $recurse
  drop
  i32.const 0
  call $hydra.lib.sets.union
  br $end_term
)
  local.set $get_all
  i32.const 0
  drop
  local.get $term0
  drop
  local.get $get_all
  drop
  i32.const 0
)
  (func $hydra.variables.free_variables_in_term (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $dflt_vars i32)
  (local $l i32)
  (local $s i32)
  (local $t i32)
  (local $v i32)
  local.get $s
  local.get $t
  call $hydra.variables.free_variables_in_term
  call $hydra.lib.sets.union
  i32.const 0
  local.get $term
  call $hydra.rewriting.subterms
  call $hydra.lib.lists.foldl
  local.set $dflt_vars
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
  i32.load
  local.get $l
  i32.load offset=8
  call $hydra.variables.free_variables_in_term
  call $hydra.lib.sets.delete
  br $end_term
)
  local.get $v
  drop
  i32.const 0
  drop
  local.get $dflt_vars
  drop
  i32.const 0
  i32.const 0
  local.get $l
  i32.load
  call $hydra.lib.lists.map
  call $hydra.lib.sets.from_list
  call $hydra.lib.sets.difference
  br $end_term
)
  local.get $v
  drop
  local.get $v
  call $hydra.lib.sets.singleton
  br $end_term
)
)
  (func $hydra.variables.free_variables_in_type (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $dflt_vars i32)
  (local $lt i32)
  (local $s i32)
  (local $t i32)
  (local $v i32)
  local.get $s
  local.get $t
  call $hydra.variables.free_variables_in_type
  call $hydra.lib.sets.union
  i32.const 0
  local.get $typ
  call $hydra.rewriting.subtypes
  call $hydra.lib.lists.foldl
  local.set $dflt_vars
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
  local.get $lt
  i32.load
  local.get $lt
  i32.load offset=4
  call $hydra.variables.free_variables_in_type
  call $hydra.lib.sets.delete
  br $end_type
)
  local.get $v
  drop
  local.get $v
  call $hydra.lib.sets.singleton
  br $end_type
)
)
  (func $hydra.variables.free_variables_in_type_ordered (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $bound_vars i32)
  (local $collect_vars i32)
  (local $ft i32)
  (local $t i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $forall
  (block $variable
  local.get $t
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $variable $forall $forall
)
  local.get $v
  drop
  local.get $v
  local.get $bound_vars
  call $hydra.lib.sets.member
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $v
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.lib.logic.if_else
  br $end_type
)
  local.get $v
  drop
  local.get $ft
  i32.load
  local.get $bound_vars
  call $hydra.lib.sets.insert
  drop
  local.get $ft
  i32.load offset=4
  drop
  local.get $collect_vars
  drop
  i32.const 0
  br $end_type
)
  local.set $collect_vars
  i32.const 0
  drop
  local.get $typ
  drop
  local.get $collect_vars
  drop
  i32.const 0
  call $hydra.lib.lists.nub
)
  (func $hydra.variables.free_variables_in_type_scheme (param $ts i32) (result i32)
  (local $t i32)
  (local $vars i32)
  local.get $ts
  i32.load
  local.set $vars
  local.get $ts
  i32.load offset=4
  local.set $t
  local.get $t
  call $hydra.variables.free_variables_in_type
  local.get $vars
  call $hydra.lib.sets.from_list
  call $hydra.lib.sets.difference
)
  (func $hydra.variables.free_variables_in_type_scheme_simple (param $ts i32) (result i32)
  (local $t i32)
  (local $vars i32)
  local.get $ts
  i32.load
  local.set $vars
  local.get $ts
  i32.load offset=4
  local.set $t
  local.get $t
  call $hydra.variables.free_variables_in_type_simple
  local.get $vars
  call $hydra.lib.sets.from_list
  call $hydra.lib.sets.difference
)
  (func $hydra.variables.free_variables_in_type_simple (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $helper i32)
  (local $typ2 i32)
  (local $types i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $variable
  local.get $typ2
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $variable $variable
)
  local.get $v
  drop
  local.get $v
  local.get $types
  call $hydra.lib.sets.insert
  br $end_type
)
  local.set $helper
  i32.const 0
  i32.const 0
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
  local.get $helper
  i32.const 0
  local.get $typ
  call $hydra.rewriting.fold_over_type
)
  (func $hydra.variables.is_free_variable_in_term (param $v i32) (param $term i32) (result i32)
  local.get $v
  local.get $term
  call $hydra.variables.free_variables_in_term
  call $hydra.lib.sets.member
  call $hydra.lib.logic.not
)
  (func $hydra.variables.normalize_type_variables_in_term (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $acc i32)
  (local $acc2 i32)
  (local $b i32)
  (local $b1 i32)
  (local $bindings0 i32)
  (local $bindings1 i32)
  (local $body0 i32)
  (local $bound_vars i32)
  (local $bs i32)
  (local $constraint_map i32)
  (local $domain i32)
  (local $gen i32)
  (local $i i32)
  (local $k i32)
  (local $l i32)
  (local $lt i32)
  (local $meta i32)
  (local $new_bound i32)
  (local $new_constraints i32)
  (local $new_name i32)
  (local $new_subst i32)
  (local $new_val i32)
  (local $new_vars i32)
  (local $next i32)
  (local $no_type i32)
  (local $old_constraints i32)
  (local $old_name i32)
  (local $p i32)
  (local $rem i32)
  (local $rename_constraint_keys i32)
  (local $replace_name i32)
  (local $rewrite i32)
  (local $rewrite_with_subst i32)
  (local $sb i32)
  (local $state i32)
  (local $step i32)
  (local $subst i32)
  (local $subst_type i32)
  (local $ta i32)
  (local $term0 i32)
  (local $term2 i32)
  (local $ti i32)
  (local $tl i32)
  (local $ts i32)
  (local $tt i32)
  (local $typ i32)
  (local $typ2 i32)
  (local $uc i32)
  (local $v i32)
  (local $vars i32)
  (local $with_type i32)
  local.get $v
  local.get $v
  local.get $subst
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.from_maybe
  local.set $replace_name
  (block $end_type (result i32)
  (block $variable
  local.get $typ2
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $variable $variable
)
  local.get $v
  drop
  i32.const 14
  local.get $subst
  drop
  local.get $v
  drop
  local.get $replace_name
  drop
  i32.const 0
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
  br $end_type
)
  local.set $rewrite
  local.get $rewrite
  local.get $typ
  call $hydra.rewriting.rewrite_type
  local.set $subst_type
  local.get $state
  call $hydra.lib.pairs.first
  local.set $sb
  local.get $state
  call $hydra.lib.pairs.second
  local.set $next
  local.get $sb
  call $hydra.lib.pairs.first
  local.set $subst
  local.get $sb
  call $hydra.lib.pairs.second
  local.set $bound_vars
  (block $end_term (result i32)
  (block $type_lambda
  (block $type_application
  (block $let
  (block $lambda
  local.get $term2
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
  i32.load offset=4
  local.set $domain
  i32.const 5
  local.get $l
  i32.load
  local.get $subst
  drop
  local.get $subst_type
  drop
  i32.const 0
  local.get $domain
  call $hydra.lib.maybes.map
  local.get $subst
  local.get $bound_vars
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
  local.get $next
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
  drop
  local.get $l
  i32.load offset=8
  drop
  local.get $rewrite_with_subst
  drop
  i32.const 0
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
  br $end_term
)
  local.get $v
  drop
  local.get $lt
  i32.load
  local.set $bindings0
  local.get $lt
  i32.load offset=4
  local.set $body0
  local.get $acc
  call $hydra.lib.lists.reverse
  local.get $uc
  call $hydra.lib.pairs.first
  local.set $b
  local.get $uc
  call $hydra.lib.pairs.second
  local.set $tl
  local.get $subst
  local.get $bound_vars
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
  local.get $next
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
  drop
  local.get $b
  i32.load offset=4
  drop
  local.get $rewrite_with_subst
  drop
  i32.const 0
  local.set $new_val
  local.get $b
  i32.load
  local.get $new_val
  i32.const 0
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
  local.set $b1
  local.get $b1
  local.get $acc
  call $hydra.lib.lists.cons
  drop
  local.get $tl
  drop
  local.get $step
  drop
  i32.const 0
  local.set $no_type
  local.get $ts
  i32.load
  local.set $vars
  local.get $ts
  i32.load offset=4
  local.set $typ
  local.get $vars
  call $hydra.lib.lists.length
  local.set $k
  i32.const 1024
  local.get $next
  local.get $i
  call $hydra.lib.math.add
  call $hydra.lib.literals.show_int32
  call $hydra.lib.strings.cat2
  local.set $ti
  local.get $rem
  i32.const 0
  call $hydra.lib.equality.equal
  local.get $acc2
  call $hydra.lib.lists.reverse
  local.get $i
  i32.const 1
  call $hydra.lib.math.add
  drop
  local.get $rem
  i32.const 1
  call $hydra.lib.math.sub
  drop
  local.get $ti
  local.get $acc2
  call $hydra.lib.lists.cons
  drop
  local.get $gen
  drop
  i32.const 0
  call $hydra.lib.logic.if_else
  local.set $gen
  i32.const 0
  drop
  local.get $k
  drop
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  drop
  local.get $gen
  drop
  i32.const 0
  local.set $new_vars
  local.get $vars
  local.get $new_vars
  call $hydra.lib.lists.zip
  call $hydra.lib.maps.from_list
  local.get $subst
  call $hydra.lib.maps.union
  local.set $new_subst
  local.get $bound_vars
  local.get $new_vars
  call $hydra.lib.sets.from_list
  call $hydra.lib.sets.union
  local.set $new_bound
  local.get $new_subst
  local.get $new_bound
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
  local.get $next
  local.get $k
  call $hydra.lib.math.add
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
  drop
  local.get $b
  i32.load offset=4
  drop
  local.get $rewrite_with_subst
  drop
  i32.const 0
  local.set $new_val
  local.get $p
  call $hydra.lib.pairs.first
  local.set $old_name
  local.get $p
  call $hydra.lib.pairs.second
  local.set $meta
  local.get $old_name
  local.get $old_name
  local.get $new_subst
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.from_maybe
  local.set $new_name
  local.get $new_name
  local.get $meta
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
  local.get $constraint_map
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $rename_constraint_keys
  local.get $ts
  i32.load offset=8
  local.set $old_constraints
  local.get $rename_constraint_keys
  local.get $old_constraints
  call $hydra.lib.maybes.map
  local.set $new_constraints
  local.get $b
  i32.load
  local.get $new_val
  local.get $new_vars
  local.get $new_subst
  drop
  local.get $typ
  drop
  local.get $subst_type
  drop
  i32.const 0
  local.get $new_constraints
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
  local.set $b1
  local.get $b1
  local.get $acc
  call $hydra.lib.lists.cons
  drop
  local.get $tl
  drop
  local.get $step
  drop
  i32.const 0
  local.set $with_type
  local.get $no_type
  local.get $ts
  drop
  local.get $with_type
  drop
  i32.const 0
  local.get $b
  i32.load offset=8
  call $hydra.lib.maybes.maybe
  local.get $bs
  call $hydra.lib.lists.uncons
  call $hydra.lib.maybes.maybe
  local.set $step
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  drop
  local.get $bindings0
  drop
  local.get $step
  drop
  i32.const 0
  local.set $bindings1
  i32.const 6
  local.get $bindings1
  local.get $subst
  local.get $bound_vars
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
  local.get $next
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
  drop
  local.get $body0
  drop
  local.get $rewrite_with_subst
  drop
  i32.const 0
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
  br $end_term
)
  local.get $v
  drop
  i32.const 15
  local.get $subst
  local.get $bound_vars
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
  local.get $next
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
  drop
  local.get $tt
  i32.load
  drop
  local.get $rewrite_with_subst
  drop
  i32.const 0
  local.get $subst
  drop
  local.get $tt
  i32.load offset=4
  drop
  local.get $subst_type
  drop
  i32.const 0
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
  br $end_term
)
  local.get $v
  drop
  i32.const 16
  local.get $subst
  drop
  local.get $ta
  i32.load
  drop
  local.get $replace_name
  drop
  i32.const 0
  local.get $subst
  local.get $bound_vars
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
  local.get $next
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
  drop
  local.get $ta
  i32.load offset=4
  drop
  local.get $rewrite_with_subst
  drop
  i32.const 0
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
  br $end_term
)
  local.set $rewrite
  local.get $rewrite
  local.get $term0
  call $hydra.rewriting.rewrite_term
  local.set $rewrite_with_subst
  i32.const 0
  i32.const 0
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
  i32.const 0
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
  drop
  local.get $term
  drop
  local.get $rewrite_with_subst
  drop
  i32.const 0
)
  (func $hydra.variables.replace_free_term_variable (param $vold i32) (param $tnew i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $l i32)
  (local $recurse i32)
  (local $rewrite i32)
  (local $t i32)
  (local $v i32)
  (block $end_term (result i32)
  (block $variable
  (block $lambda
  local.get $t
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $lambda $variable $variable
)
  local.get $v
  drop
  local.get $l
  i32.load
  local.set $v
  local.get $v
  local.get $vold
  call $hydra.lib.equality.equal
  local.get $t
  local.get $t
  drop
  local.get $recurse
  drop
  i32.const 0
  call $hydra.lib.logic.if_else
  br $end_term
)
  local.get $v
  drop
  local.get $v
  local.get $vold
  call $hydra.lib.equality.equal
  local.get $tnew
  i32.const 19
  local.get $v
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
  call $hydra.lib.logic.if_else
  br $end_term
)
  local.set $rewrite
  local.get $rewrite
  local.get $term
  call $hydra.rewriting.rewrite_term
)
  (func $hydra.variables.replace_free_type_variable (param $v i32) (param $rep i32) (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $ft i32)
  (local $map_expr i32)
  (local $recurse i32)
  (local $t i32)
  (local $v' i32)
  (block $end_type (result i32)
  (block $variable
  (block $forall
  local.get $t
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
  local.get $v
  local.get $ft
  i32.load
  call $hydra.lib.equality.equal
  local.get $t
  i32.const 3
  local.get $ft
  i32.load
  local.get $ft
  i32.load offset=4
  drop
  local.get $recurse
  drop
  i32.const 0
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
  call $hydra.lib.logic.if_else
  br $end_type
)
  local.get $v
  drop
  local.get $v
  local.get $v'
  call $hydra.lib.equality.equal
  local.get $rep
  local.get $t
  call $hydra.lib.logic.if_else
  br $end_type
)
  local.set $map_expr
  local.get $map_expr
  local.get $typ
  call $hydra.rewriting.rewrite_type
)
  (func $hydra.variables.substitute_type_variables (param $subst i32) (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $n i32)
  (local $replace i32)
  (local $typ2 i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $variable
  local.get $typ2
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $variable $variable
)
  local.get $v
  drop
  i32.const 14
  local.get $n
  local.get $n
  local.get $subst
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.from_maybe
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
  br $end_type
)
  local.set $replace
  local.get $replace
  local.get $typ
  call $hydra.rewriting.rewrite_type
)
  (func $hydra.variables.substitute_type_variables_in_term (param $subst i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $at i32)
  (local $b i32)
  (local $l i32)
  (local $lt i32)
  (local $map_binding i32)
  (local $mt i32)
  (local $mts i32)
  (local $recurse i32)
  (local $replace i32)
  (local $st i32)
  (local $st_opt i32)
  (local $st_scheme i32)
  (local $st_scheme_opt i32)
  (local $t i32)
  (local $tl i32)
  (local $ts i32)
  (local $tt i32)
  (local $v i32)
  local.get $subst
  i32.const 0
  call $hydra.variables.substitute_type_variables
  local.set $st
  local.get $st
  local.get $mt
  call $hydra.lib.maybes.map
  local.set $st_opt
  local.get $ts
  i32.load
  local.get $ts
  i32.load offset=4
  drop
  local.get $st
  drop
  i32.const 0
  local.get $ts
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
  local.set $st_scheme
  local.get $st_scheme
  local.get $mts
  call $hydra.lib.maybes.map
  local.set $st_scheme_opt
  (block $end_term (result i32)
  (block $annotated
  (block $type_lambda
  (block $type_application
  (block $let
  (block $lambda
  local.get $t
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $lambda $let $type_application $type_lambda $annotated $annotated
)
  local.get $v
  drop
  i32.const 5
  local.get $l
  i32.load
  local.get $l
  i32.load offset=4
  drop
  local.get $st_opt
  drop
  i32.const 0
  local.get $l
  i32.load offset=8
  drop
  local.get $recurse
  drop
  i32.const 0
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
  br $end_term
)
  local.get $v
  drop
  local.get $b
  i32.load
  local.get $b
  i32.load offset=4
  drop
  local.get $recurse
  drop
  i32.const 0
  local.get $b
  i32.load offset=8
  drop
  local.get $st_scheme_opt
  drop
  i32.const 0
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
  local.set $map_binding
  i32.const 6
  local.get $map_binding
  local.get $lt
  i32.load
  call $hydra.lib.lists.map
  local.get $lt
  i32.load offset=4
  drop
  local.get $recurse
  drop
  i32.const 0
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
  br $end_term
)
  local.get $v
  drop
  i32.const 15
  local.get $tt
  i32.load
  drop
  local.get $recurse
  drop
  i32.const 0
  local.get $tt
  i32.load offset=4
  drop
  local.get $st
  drop
  i32.const 0
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
  br $end_term
)
  local.get $v
  drop
  i32.const 16
  local.get $tl
  i32.load
  local.get $tl
  i32.load
  local.get $subst
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.from_maybe
  local.get $tl
  i32.load offset=4
  drop
  local.get $recurse
  drop
  i32.const 0
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
  br $end_term
)
  local.get $v
  drop
  i32.const 0
  local.get $at
  i32.load
  drop
  local.get $recurse
  drop
  i32.const 0
  local.get $at
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
  br $end_term
)
  local.set $replace
  local.get $replace
  local.get $term
  call $hydra.rewriting.rewrite_term
)
  (func $hydra.variables.substitute_variable (param $from i32) (param $to i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $l i32)
  (local $recurse i32)
  (local $replace i32)
  (local $term2 i32)
  (local $v i32)
  (local $x i32)
  (block $end_term (result i32)
  (block $lambda
  (block $variable
  local.get $term2
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $variable $lambda $lambda
)
  local.get $v
  drop
  i32.const 19
  local.get $x
  local.get $from
  call $hydra.lib.equality.equal
  local.get $to
  local.get $x
  call $hydra.lib.logic.if_else
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
  br $end_term
)
  local.get $v
  drop
  local.get $l
  i32.load
  local.get $from
  call $hydra.lib.equality.equal
  local.get $term2
  local.get $term2
  drop
  local.get $recurse
  drop
  i32.const 0
  call $hydra.lib.logic.if_else
  br $end_term
)
  local.set $replace
  local.get $replace
  local.get $term
  call $hydra.rewriting.rewrite_term
)
  (func $hydra.variables.substitute_variables (param $subst i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $l i32)
  (local $n i32)
  (local $recurse i32)
  (local $replace i32)
  (local $term2 i32)
  (local $v i32)
  (block $end_term (result i32)
  (block $lambda
  (block $variable
  local.get $term2
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $variable $lambda $lambda
)
  local.get $v
  drop
  i32.const 19
  local.get $n
  local.get $n
  local.get $subst
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.from_maybe
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
  br $end_term
)
  local.get $v
  drop
  local.get $term2
  drop
  local.get $recurse
  drop
  i32.const 0
  local.get $term2
  local.get $l
  i32.load
  local.get $subst
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.set $replace
  local.get $replace
  local.get $term
  call $hydra.rewriting.rewrite_term
)
  (func $hydra.variables.unshadow_variables (param $term0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $acc i32)
  (local $b i32)
  (local $base i32)
  (local $bname i32)
  (local $body i32)
  (local $candidate i32)
  (local $domain i32)
  (local $f i32)
  (local $fresh_name i32)
  (local $i i32)
  (local $l i32)
  (local $lt i32)
  (local $m i32)
  (local $m2 i32)
  (local $recurse i32)
  (local $renamed i32)
  (local $term i32)
  (local $v i32)
  (local $v2 i32)
  local.get $base
  drop
  i32.const 0
  drop
  i32.const 0
  local.get $i
  call $hydra.lib.literals.show_int32
  call $hydra.lib.strings.cat2
  local.set $candidate
  local.get $candidate
  local.get $m
  call $hydra.lib.maps.member
  local.get $base
  drop
  local.get $i
  i32.const 1
  call $hydra.lib.math.add
  drop
  local.get $m
  drop
  local.get $fresh_name
  drop
  i32.const 0
  local.get $candidate
  call $hydra.lib.logic.if_else
  local.set $fresh_name
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
  i32.load
  local.set $v
  local.get $l
  i32.load offset=4
  local.set $domain
  local.get $l
  i32.load offset=8
  local.set $body
  local.get $v
  local.get $m
  call $hydra.lib.maps.member
  local.get $v
  drop
  i32.const 2
  drop
  local.get $m
  drop
  local.get $fresh_name
  drop
  i32.const 0
  local.set $v2
  local.get $v
  local.get $v2
  local.get $v2
  local.get $v2
  local.get $m
  call $hydra.lib.maps.insert
  call $hydra.lib.maps.insert
  local.set $m2
  i32.const 5
  local.get $v2
  local.get $domain
  local.get $recurse
  drop
  local.get $m2
  drop
  local.get $body
  drop
  local.get $f
  drop
  i32.const 0
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
  i32.const 5
  local.get $v
  local.get $domain
  local.get $recurse
  drop
  local.get $v
  local.get $v
  local.get $m
  call $hydra.lib.maps.insert
  drop
  local.get $body
  drop
  local.get $f
  drop
  i32.const 0
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
  call $hydra.lib.logic.if_else
  br $end_term
)
  local.get $v
  drop
  local.get $b
  i32.load
  local.set $bname
  local.get $bname
  local.get $acc
  call $hydra.lib.maps.member
  local.get $acc
  local.get $bname
  local.get $bname
  local.get $acc
  call $hydra.lib.maps.insert
  call $hydra.lib.logic.if_else
  local.get $m
  local.get $lt
  i32.load
  call $hydra.lib.lists.foldl
  local.set $m2
  local.get $m2
  drop
  local.get $term
  drop
  local.get $recurse
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 19
  local.get $v
  local.get $renamed
  local.get $v
  local.get $m
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
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
  br $end_term
)
  local.set $f
  local.get $f
  i32.const 0
  local.get $term0
  call $hydra.rewriting.rewrite_term_with_context
)
)
