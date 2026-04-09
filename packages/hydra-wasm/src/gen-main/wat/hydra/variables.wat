(module
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.head" (func $hydra.lib.lists.head (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.nub" (func $hydra.lib.lists.nub (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.reverse" (func $hydra.lib.lists.reverse (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.tail" (func $hydra.lib.lists.tail (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.zip" (func $hydra.lib.lists.zip (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int32" (func $hydra.lib.literals.show_int32 (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.empty" (func $hydra.lib.maps.empty (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.insert" (func $hydra.lib.maps.insert (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.member" (func $hydra.lib.maps.member (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.union" (func $hydra.lib.maps.union (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.add" (func $hydra.lib.math.add (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.sub" (func $hydra.lib.math.sub (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.from_maybe" (func $hydra.lib.maybes.from_maybe (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.delete" (func $hydra.lib.sets.delete (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.difference" (func $hydra.lib.sets.difference (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.empty" (func $hydra.lib.sets.empty (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.insert" (func $hydra.lib.sets.insert (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.member" (func $hydra.lib.sets.member (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.singleton" (func $hydra.lib.sets.singleton (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.union" (func $hydra.lib.sets.union (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.fold_over_type" (func $hydra.rewriting.fold_over_type (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_term" (func $hydra.rewriting.rewrite_term (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_term_with_context" (func $hydra.rewriting.rewrite_term_with_context (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_type" (func $hydra.rewriting.rewrite_type (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.subterms" (func $hydra.rewriting.subterms (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.subtypes" (func $hydra.rewriting.subtypes (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
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
  (local $all_of i32)
  (local $b i32)
  (local $dflt i32)
  (local $domt i32)
  (local $f i32)
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
  (local $vars i32)
  call $hydra.lib.sets.union
  call $hydra.lib.sets.empty
  local.get $sets
  call $hydra.lib.lists.foldl
  local.set $all_of
  local.get $typ
  call $hydra.variables.free_variables_in_type
  local.get $tvars
  call $hydra.lib.sets.difference
  local.set $try_type
  local.get $vars
  local.get $get_all
  local.set $recurse
  local.get $recurse
  local.get $term
  call $hydra.rewriting.subterms
  call $hydra.lib.lists.map
  local.get $all_of
  local.set $dflt
  (block $end_term (result i32)
  (block $type_lambda
  (block $type_application
  (block $let
  (block $function
  local.get $term
  br_table $function $let $type_application $type_lambda $type_lambda
)
  (block $end_function (result i32)
  (block $lambda
  (block $elimination
  local.get $f
  br_table $elimination $lambda $lambda
)
  local.get $dflt
  br $end_function
)
  call $hydra.lib.sets.empty
  local.get $vars
  local.get $try_type
  local.get $l
  ;; project field: domain
  call $hydra.lib.maybes.maybe
  local.set $domt
  local.get $domt
  local.get $l
  ;; project field: body
  local.get $recurse
  call $hydra.lib.sets.union
  br $end_function
)
  br $end_term
)
  local.get $vars
  local.get $vars
  local.get $ts
  ;; project field: variables
  call $hydra.lib.sets.from_list
  call $hydra.lib.sets.union
  local.get $b
  ;; project field: type
  call $hydra.lib.maybes.maybe
  local.set $new_vars
  local.get $new_vars
  local.get $b
  ;; project field: term
  local.get $get_all
  call $hydra.lib.sets.empty
  local.get $new_vars
  local.get $ts
  ;; project field: type
  local.get $try_type
  local.get $b
  ;; project field: type
  call $hydra.lib.maybes.maybe
  call $hydra.lib.sets.union
  local.set $for_binding
  local.get $for_binding
  local.get $l
  ;; project field: bindings
  call $hydra.lib.lists.map
  local.get $all_of
  local.get $l
  ;; project field: body
  local.get $recurse
  call $hydra.lib.sets.union
  br $end_term
)
  local.get $vars
  local.get $tt
  ;; project field: type
  local.get $try_type
  local.get $tt
  ;; project field: body
  local.get $recurse
  call $hydra.lib.sets.union
  br $end_term
)
  local.get $vars
  local.get $tl
  ;; project field: parameter
  local.get $try_type
  local.get $tl
  ;; project field: body
  local.get $recurse
  call $hydra.lib.sets.union
  br $end_term
)
  local.set $get_all
  call $hydra.lib.sets.empty
  local.get $term0
  local.get $get_all
)
  (func $hydra.variables.free_variables_in_term (param $term i32) (result i32)
  (local $dflt_vars i32)
  (local $l i32)
  (local $s i32)
  (local $t i32)
  (local $v i32)
  local.get $s
  local.get $t
  call $hydra.variables.free_variables_in_term
  call $hydra.lib.sets.union
  call $hydra.lib.sets.empty
  local.get $term
  call $hydra.rewriting.subterms
  call $hydra.lib.lists.foldl
  local.set $dflt_vars
  (block $end_term (result i32)
  (block $variable
  (block $let
  (block $function
  local.get $term
  br_table $function $let $variable $variable
)
  (block $end_function (result i32)
  (block $lambda
  local.get $v
  br_table $lambda $lambda
)
  local.get $l
  ;; project field: parameter
  local.get $l
  ;; project field: body
  call $hydra.variables.free_variables_in_term
  call $hydra.lib.sets.delete
  br $end_function
)
  br $end_term
)
  local.get $dflt_vars
  ;; project field: name
  local.get $l
  ;; project field: bindings
  call $hydra.lib.lists.map
  call $hydra.lib.sets.from_list
  call $hydra.lib.sets.difference
  br $end_term
)
  local.get $v
  call $hydra.lib.sets.singleton
  br $end_term
)
)
  (func $hydra.variables.free_variables_in_type (param $typ i32) (result i32)
  (local $dflt_vars i32)
  (local $lt i32)
  (local $s i32)
  (local $t i32)
  (local $v i32)
  local.get $s
  local.get $t
  call $hydra.variables.free_variables_in_type
  call $hydra.lib.sets.union
  call $hydra.lib.sets.empty
  local.get $typ
  call $hydra.rewriting.subtypes
  call $hydra.lib.lists.foldl
  local.set $dflt_vars
  (block $end_type (result i32)
  (block $variable
  (block $forall
  local.get $typ
  br_table $forall $variable $variable
)
  local.get $lt
  ;; project field: parameter
  local.get $lt
  ;; project field: body
  call $hydra.variables.free_variables_in_type
  call $hydra.lib.sets.delete
  br $end_type
)
  local.get $v
  call $hydra.lib.sets.singleton
  br $end_type
)
)
  (func $hydra.variables.free_variables_in_type_ordered (param $typ i32) (result i32)
  (local $bound_vars i32)
  (local $collect_vars i32)
  (local $ft i32)
  (local $t i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $forall
  (block $variable
  local.get $t
  br_table $variable $forall $forall
)
  local.get $v
  local.get $bound_vars
  call $hydra.lib.sets.member
  i32.const 0
  ;; list elements follow
  i32.const 1
  ;; list elements follow
  local.get $v
  call $hydra.lib.logic.if_else
  br $end_type
)
  local.get $ft
  ;; project field: parameter
  local.get $bound_vars
  call $hydra.lib.sets.insert
  local.get $ft
  ;; project field: body
  local.get $collect_vars
  br $end_type
)
  local.set $collect_vars
  call $hydra.lib.sets.empty
  local.get $typ
  local.get $collect_vars
  call $hydra.lib.lists.nub
)
  (func $hydra.variables.free_variables_in_type_scheme (param $ts i32) (result i32)
  (local $t i32)
  (local $vars i32)
  local.get $ts
  ;; project field: variables
  local.set $vars
  local.get $ts
  ;; project field: type
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
  ;; project field: variables
  local.set $vars
  local.get $ts
  ;; project field: type
  local.set $t
  local.get $t
  call $hydra.variables.free_variables_in_type_simple
  local.get $vars
  call $hydra.lib.sets.from_list
  call $hydra.lib.sets.difference
)
  (func $hydra.variables.free_variables_in_type_simple (param $typ i32) (result i32)
  (local $helper i32)
  (local $typ2 i32)
  (local $types i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $variable
  local.get $typ2
  br_table $variable $variable
)
  local.get $v
  local.get $types
  call $hydra.lib.sets.insert
  br $end_type
)
  local.set $helper
  i32.const 0
  local.get $helper
  call $hydra.lib.sets.empty
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
  (local $recurse i32)
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
  br_table $variable $variable
)
  local.get $subst
  local.get $v
  local.get $replace_name
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
  (block $function
  local.get $term2
  br_table $function $let $type_application $type_lambda $type_lambda
)
  (block $end_function (result i32)
  (block $lambda
  (block $elimination
  local.get $v
  br_table $elimination $lambda $lambda
)
  local.get $term2
  local.get $recurse
  br $end_function
)
  local.get $l
  ;; project field: domain
  local.set $domain
  local.get $l
  ;; project field: parameter
  local.get $subst
  local.get $subst_type
  local.get $domain
  call $hydra.lib.maybes.map
  local.get $subst
  local.get $bound_vars
  local.get $next
  local.get $l
  ;; project field: body
  local.get $rewrite_with_subst
  br $end_function
)
  br $end_term
)
  local.get $lt
  ;; project field: bindings
  local.set $bindings0
  local.get $lt
  ;; project field: body
  local.set $body0
  local.get $bs
  call $hydra.lib.lists.null
  local.get $acc
  call $hydra.lib.lists.reverse
  local.get $bs
  call $hydra.lib.lists.head
  local.set $b
  local.get $bs
  call $hydra.lib.lists.tail
  local.set $tl
  local.get $subst
  local.get $bound_vars
  local.get $next
  local.get $b
  ;; project field: term
  local.get $rewrite_with_subst
  local.set $new_val
  local.get $b
  ;; project field: name
  local.get $new_val
  i32.const 0
  local.set $b1
  local.get $b1
  local.get $acc
  call $hydra.lib.lists.cons
  local.get $tl
  local.get $step
  local.set $no_type
  local.get $ts
  ;; project field: variables
  local.set $vars
  local.get $ts
  ;; project field: type
  local.set $typ
  local.get $vars
  call $hydra.lib.lists.length
  local.set $k
  i32.const 0 ;; string: "t"
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
  local.get $rem
  i32.const 1
  call $hydra.lib.math.sub
  local.get $ti
  local.get $acc2
  call $hydra.lib.lists.cons
  local.get $gen
  call $hydra.lib.logic.if_else
  local.set $gen
  i32.const 0
  local.get $k
  i32.const 0
  ;; list elements follow
  local.get $gen
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
  local.get $next
  local.get $k
  call $hydra.lib.math.add
  local.get $b
  ;; project field: term
  local.get $rewrite_with_subst
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
  local.get $constraint_map
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $rename_constraint_keys
  local.get $ts
  ;; project field: constraints
  local.set $old_constraints
  local.get $rename_constraint_keys
  local.get $old_constraints
  call $hydra.lib.maybes.map
  local.set $new_constraints
  local.get $b
  ;; project field: name
  local.get $new_val
  local.get $new_vars
  local.get $new_subst
  local.get $typ
  local.get $subst_type
  local.get $new_constraints
  local.set $b1
  local.get $b1
  local.get $acc
  call $hydra.lib.lists.cons
  local.get $tl
  local.get $step
  local.set $with_type
  local.get $no_type
  local.get $ts
  local.get $with_type
  local.get $b
  ;; project field: type
  call $hydra.lib.maybes.maybe
  call $hydra.lib.logic.if_else
  local.set $step
  i32.const 0
  ;; list elements follow
  local.get $bindings0
  local.get $step
  local.set $bindings1
  local.get $bindings1
  local.get $subst
  local.get $bound_vars
  local.get $next
  local.get $body0
  local.get $rewrite_with_subst
  br $end_term
)
  local.get $subst
  local.get $bound_vars
  local.get $next
  local.get $tt
  ;; project field: body
  local.get $rewrite_with_subst
  local.get $subst
  local.get $tt
  ;; project field: type
  local.get $subst_type
  br $end_term
)
  local.get $subst
  local.get $ta
  ;; project field: parameter
  local.get $replace_name
  local.get $subst
  local.get $bound_vars
  local.get $next
  local.get $ta
  ;; project field: body
  local.get $rewrite_with_subst
  br $end_term
)
  local.set $rewrite
  local.get $rewrite
  local.get $term0
  call $hydra.rewriting.rewrite_term
  local.set $rewrite_with_subst
  call $hydra.lib.maps.empty
  call $hydra.lib.sets.empty
  i32.const 0
  local.get $term
  local.get $rewrite_with_subst
)
  (func $hydra.variables.replace_free_term_variable (param $vold i32) (param $tnew i32) (param $term i32) (result i32)
  (local $f i32)
  (local $l i32)
  (local $recurse i32)
  (local $rewrite i32)
  (local $t i32)
  (local $v i32)
  (block $end_term (result i32)
  (block $variable
  (block $function
  local.get $t
  br_table $function $variable $variable
)
  (block $end_function (result i32)
  (block $lambda
  local.get $f
  br_table $lambda $lambda
)
  local.get $l
  ;; project field: parameter
  local.set $v
  local.get $v
  local.get $vold
  call $hydra.lib.equality.equal
  local.get $t
  local.get $t
  local.get $recurse
  call $hydra.lib.logic.if_else
  br $end_function
)
  br $end_term
)
  local.get $v
  local.get $vold
  call $hydra.lib.equality.equal
  local.get $tnew
  local.get $v
  call $hydra.lib.logic.if_else
  br $end_term
)
  local.set $rewrite
  local.get $rewrite
  local.get $term
  call $hydra.rewriting.rewrite_term
)
  (func $hydra.variables.replace_free_type_variable (param $v i32) (param $rep i32) (param $typ i32) (result i32)
  (local $ft i32)
  (local $map_expr i32)
  (local $recurse i32)
  (local $t i32)
  (local $v' i32)
  (block $end_type (result i32)
  (block $variable
  (block $forall
  local.get $t
  br_table $forall $variable $variable
)
  local.get $v
  local.get $ft
  ;; project field: parameter
  call $hydra.lib.equality.equal
  local.get $t
  local.get $ft
  ;; project field: parameter
  local.get $ft
  ;; project field: body
  local.get $recurse
  call $hydra.lib.logic.if_else
  br $end_type
)
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
  (local $n i32)
  (local $replace i32)
  (local $typ2 i32)
  (block $end_type (result i32)
  (block $variable
  local.get $typ2
  br_table $variable $variable
)
  local.get $n
  local.get $n
  local.get $subst
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.from_maybe
  br $end_type
)
  local.set $replace
  local.get $replace
  local.get $typ
  call $hydra.rewriting.rewrite_type
)
  (func $hydra.variables.substitute_type_variables_in_term (param $subst i32) (param $term i32) (result i32)
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
  call $hydra.variables.substitute_type_variables
  local.set $st
  local.get $st
  local.get $mt
  call $hydra.lib.maybes.map
  local.set $st_opt
  local.get $ts
  ;; project field: variables
  local.get $ts
  ;; project field: type
  local.get $st
  local.get $ts
  ;; project field: constraints
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
  (block $function
  local.get $t
  br_table $function $let $type_application $type_lambda $annotated $annotated
)
  (block $end_function (result i32)
  (block $lambda
  local.get $v
  br_table $lambda $lambda
)
  local.get $l
  ;; project field: parameter
  local.get $l
  ;; project field: domain
  local.get $st_opt
  local.get $l
  ;; project field: body
  local.get $recurse
  br $end_function
)
  br $end_term
)
  local.get $b
  ;; project field: name
  local.get $b
  ;; project field: term
  local.get $recurse
  local.get $b
  ;; project field: type
  local.get $st_scheme_opt
  local.set $map_binding
  local.get $map_binding
  local.get $lt
  ;; project field: bindings
  call $hydra.lib.lists.map
  local.get $lt
  ;; project field: body
  local.get $recurse
  br $end_term
)
  local.get $tt
  ;; project field: body
  local.get $recurse
  local.get $tt
  ;; project field: type
  local.get $st
  br $end_term
)
  local.get $tl
  ;; project field: parameter
  local.get $tl
  ;; project field: parameter
  local.get $subst
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.from_maybe
  local.get $tl
  ;; project field: body
  local.get $recurse
  br $end_term
)
  local.get $at
  ;; project field: body
  local.get $recurse
  local.get $at
  ;; project field: annotation
  br $end_term
)
  local.set $replace
  local.get $replace
  local.get $term
  call $hydra.rewriting.rewrite_term
)
  (func $hydra.variables.substitute_variable (param $from i32) (param $to i32) (param $term i32) (result i32)
  (local $l i32)
  (local $recurse i32)
  (local $replace i32)
  (local $term2 i32)
  (local $v i32)
  (local $x i32)
  (block $end_term (result i32)
  (block $function
  (block $variable
  local.get $term2
  br_table $variable $function $function
)
  local.get $x
  local.get $from
  call $hydra.lib.equality.equal
  local.get $to
  local.get $x
  call $hydra.lib.logic.if_else
  br $end_term
)
  (block $end_function (result i32)
  (block $lambda
  local.get $v
  br_table $lambda $lambda
)
  local.get $l
  ;; project field: parameter
  local.get $from
  call $hydra.lib.equality.equal
  local.get $term2
  local.get $term2
  local.get $recurse
  call $hydra.lib.logic.if_else
  br $end_function
)
  br $end_term
)
  local.set $replace
  local.get $replace
  local.get $term
  call $hydra.rewriting.rewrite_term
)
  (func $hydra.variables.substitute_variables (param $subst i32) (param $term i32) (result i32)
  (local $l i32)
  (local $n i32)
  (local $recurse i32)
  (local $replace i32)
  (local $term2 i32)
  (local $v i32)
  (block $end_term (result i32)
  (block $function
  (block $variable
  local.get $term2
  br_table $variable $function $function
)
  local.get $n
  local.get $n
  local.get $subst
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.from_maybe
  br $end_term
)
  (block $end_function (result i32)
  (block $lambda
  local.get $v
  br_table $lambda $lambda
)
  local.get $term2
  local.get $recurse
  local.get $term2
  local.get $l
  ;; project field: parameter
  local.get $subst
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  br $end_function
)
  br $end_term
)
  local.set $replace
  local.get $replace
  local.get $term
  call $hydra.rewriting.rewrite_term
)
  (func $hydra.variables.unshadow_variables (param $term0 i32) (result i32)
  (local $acc i32)
  (local $b i32)
  (local $base i32)
  (local $bname i32)
  (local $body i32)
  (local $candidate i32)
  (local $domain i32)
  (local $f i32)
  (local $fn i32)
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
  nop
  local.get $i
  call $hydra.lib.literals.show_int32
  call $hydra.lib.strings.cat2
  local.set $candidate
  local.get $candidate
  local.get $m
  call $hydra.lib.maps.member
  local.get $base
  local.get $i
  i32.const 1
  call $hydra.lib.math.add
  local.get $m
  local.get $fresh_name
  local.get $candidate
  call $hydra.lib.logic.if_else
  local.set $fresh_name
  (block $end_term (result i32)
  (block $variable
  (block $let
  (block $function
  local.get $term
  br_table $function $let $variable $variable
)
  (block $end_function (result i32)
  (block $lambda
  local.get $fn
  br_table $lambda $lambda
)
  local.get $l
  ;; project field: parameter
  local.set $v
  local.get $l
  ;; project field: domain
  local.set $domain
  local.get $l
  ;; project field: body
  local.set $body
  local.get $v
  local.get $m
  call $hydra.lib.maps.member
  local.get $v
  i32.const 2
  local.get $m
  local.get $fresh_name
  local.set $v2
  local.get $v
  local.get $v2
  local.get $v2
  local.get $v2
  local.get $m
  call $hydra.lib.maps.insert
  call $hydra.lib.maps.insert
  local.set $m2
  local.get $v2
  local.get $domain
  local.get $recurse
  local.get $m2
  local.get $body
  local.get $f
  local.get $v
  local.get $domain
  local.get $recurse
  local.get $v
  local.get $v
  local.get $m
  call $hydra.lib.maps.insert
  local.get $body
  local.get $f
  call $hydra.lib.logic.if_else
  br $end_function
)
  br $end_term
)
  local.get $b
  ;; project field: name
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
  ;; project field: bindings
  call $hydra.lib.lists.foldl
  local.set $m2
  local.get $m2
  local.get $term
  local.get $recurse
  br $end_term
)
  local.get $v
  local.get $renamed
  local.get $v
  local.get $m
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.set $f
  local.get $f
  call $hydra.lib.maps.empty
  local.get $term0
  call $hydra.rewriting.rewrite_term_with_context
)
)
