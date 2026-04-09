(module
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.delete" (func $hydra.lib.maps.delete (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.empty" (func $hydra.lib.maps.empty (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.filter_with_key" (func $hydra.lib.maps.filter_with_key (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.insert" (func $hydra.lib.maps.insert (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.map" (func $hydra.lib.maps.map (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.null" (func $hydra.lib.maps.null (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.singleton" (func $hydra.lib.maps.singleton (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.union" (func $hydra.lib.maps.union (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.is_nothing" (func $hydra.lib.maybes.is_nothing (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.member" (func $hydra.lib.sets.member (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.union" (func $hydra.lib.sets.union (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_term" (func $hydra.rewriting.rewrite_term (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_type" (func $hydra.rewriting.rewrite_type (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.free_variables_in_type" (func $hydra.variables.free_variables_in_type (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
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
  nop
  call $hydra.lib.maps.null
  local.get $s2
  nop
  call $hydra.lib.maps.null
  local.get $s1
  local.get $s1
  local.get $s2
  call $hydra.substitution.compose_type_subst_non_empty
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
)
  (func $hydra.substitution.compose_type_subst_list (result i32)
  call $hydra.substitution.compose_type_subst
  call $hydra.substitution.id_type_subst
  call $hydra.lib.lists.foldl
)
  (func $hydra.substitution.compose_type_subst_non_empty (param $s1 i32) (param $s2 i32) (result i32)
  (local $is_extra i32)
  (local $k i32)
  (local $with_extra i32)
  local.get $k
  nop
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.is_nothing
  local.set $is_extra
  local.get $is_extra
  nop
  call $hydra.lib.maps.filter_with_key
  local.set $with_extra
  local.get $with_extra
  local.get $s2
  call $hydra.substitution.subst_in_type
  nop
  call $hydra.lib.maps.map
  call $hydra.lib.maps.union
)
  (func $hydra.substitution.id_type_subst (result i32)
  call $hydra.lib.maps.empty
)
  (func $hydra.substitution.singleton_type_subst (param $v i32) (param $t i32) (result i32)
  local.get $v
  local.get $t
  call $hydra.lib.maps.singleton
)
  (func $hydra.substitution.subst_in_class_constraints (param $subst i32) (param $constraints i32) (result i32)
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
  nop
  local.set $subst_map
  local.get $var_name
  local.get $metadata
  local.get $acc
  call $hydra.lib.maps.insert
  local.get $existing
  ;; project field: classes
  local.get $metadata
  ;; project field: classes
  call $hydra.lib.sets.union
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
  local.get $metadata
  local.get $acc
  local.get $insert_or_merge
  local.get $target_type
  call $hydra.variables.free_variables_in_type
  call $hydra.lib.sets.to_list
  local.set $free_vars
  local.get $free_var
  local.get $metadata
  local.get $acc2
  local.get $insert_or_merge
  local.get $acc
  local.get $free_vars
  call $hydra.lib.lists.foldl
  local.get $var_name
  local.get $subst_map
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  call $hydra.lib.maps.empty
  local.get $constraints
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.foldl
)
  (func $hydra.substitution.subst_in_context (param $subst i32) (param $cx i32) (result i32)
  (local $cx2 i32)
  (local $new_bound_types i32)
  (local $new_class_constraints i32)
  local.get $subst
  call $hydra.substitution.subst_in_type_scheme
  local.get $cx
  ;; project field: bound_types
  call $hydra.lib.maps.map
  local.set $new_bound_types
  local.get $subst
  local.get $cx
  ;; project field: class_constraints
  call $hydra.substitution.subst_in_class_constraints
  local.set $new_class_constraints
  local.get $cx
  ;; project field: bound_terms
  local.get $new_bound_types
  local.get $cx
  ;; project field: class_constraints
  local.get $cx
  ;; project field: lambda_variables
  local.get $cx
  ;; project field: metadata
  local.get $cx
  ;; project field: primitives
  local.get $cx
  ;; project field: schema_types
  local.get $cx
  ;; project field: type_variables
  local.set $cx2
  local.get $cx2
  ;; project field: bound_terms
  local.get $cx2
  ;; project field: bound_types
  local.get $new_class_constraints
  local.get $cx2
  ;; project field: lambda_variables
  local.get $cx2
  ;; project field: metadata
  local.get $cx2
  ;; project field: primitives
  local.get $cx2
  ;; project field: schema_types
  local.get $cx2
  ;; project field: type_variables
)
  (func $hydra.substitution.subst_in_type (param $subst i32) (param $typ0 i32) (result i32)
  nop
  call $hydra.lib.maps.null
  local.get $typ0
  local.get $subst
  local.get $typ0
  call $hydra.substitution.subst_in_type_non_empty
  call $hydra.lib.logic.if_else
)
  (func $hydra.substitution.subst_in_type_non_empty (param $subst i32) (param $typ0 i32) (result i32)
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
  br_table $forall $variable $variable
)
  local.get $typ
  local.get $recurse
  local.get $lt
  ;; project field: parameter
  local.get $lt
  ;; project field: parameter
  local.get $remove_var
  local.get $lt
  ;; project field: body
  call $hydra.substitution.subst_in_type
  local.get $lt
  ;; project field: parameter
  nop
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  br $end_type
)
  local.get $typ
  local.get $styp
  local.get $v
  nop
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  br $end_type
)
  local.set $rewrite
  local.get $v
  nop
  call $hydra.lib.maps.delete
  local.set $remove_var
  local.get $rewrite
  local.get $typ0
  call $hydra.rewriting.rewrite_type
)
  (func $hydra.substitution.subst_in_type_scheme (param $subst i32) (param $ts i32) (result i32)
  local.get $ts
  ;; project field: variables
  local.get $subst
  local.get $ts
  ;; project field: type
  call $hydra.substitution.subst_in_type
  local.get $subst
  call $hydra.substitution.subst_in_class_constraints
  local.get $ts
  ;; project field: constraints
  call $hydra.lib.maybes.map
)
  (func $hydra.substitution.subst_types_in_term (param $subst i32) (param $term0 i32) (result i32)
  (local $b i32)
  (local $dflt i32)
  (local $f i32)
  (local $for_function i32)
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
  local.get $term
  local.get $recurse
  local.set $dflt
  (block $end_function (result i32)
  (block $lambda
  (block $elimination
  local.get $f
  br_table $elimination $lambda $lambda
)
  local.get $dflt
  br $end_function
)
  local.get $l
  local.get $for_lambda
  br $end_function
)
  local.set $for_function
  local.get $l
  ;; project field: parameter
  local.get $subst
  call $hydra.substitution.subst_in_type
  local.get $l
  ;; project field: domain
  call $hydra.lib.maybes.map
  local.get $subst
  local.get $l
  ;; project field: body
  call $hydra.substitution.subst_types_in_term
  local.set $for_lambda
  local.get $b
  ;; project field: name
  local.get $subst
  local.get $b
  ;; project field: term
  call $hydra.substitution.subst_types_in_term
  local.get $subst
  call $hydra.substitution.subst_in_type_scheme
  local.get $b
  ;; project field: type
  call $hydra.lib.maybes.map
  local.set $rewrite_binding
  local.get $rewrite_binding
  local.get $l
  ;; project field: bindings
  call $hydra.lib.lists.map
  local.get $subst
  local.get $l
  ;; project field: body
  call $hydra.substitution.subst_types_in_term
  local.set $for_let
  local.get $subst
  local.get $tt
  ;; project field: body
  call $hydra.substitution.subst_types_in_term
  local.get $subst
  local.get $tt
  ;; project field: type
  call $hydra.substitution.subst_in_type
  local.set $for_type_application
  local.get $ta
  ;; project field: parameter
  local.set $param
  local.get $param
  nop
  call $hydra.lib.maps.delete
  local.set $subst2
  local.get $param
  local.get $subst2
  local.get $ta
  ;; project field: body
  call $hydra.substitution.subst_types_in_term
  local.set $for_type_lambda
  (block $end_term (result i32)
  (block $type_lambda
  (block $type_application
  (block $let
  (block $function
  local.get $term
  br_table $function $let $type_application $type_lambda $type_lambda
)
  local.get $f
  local.get $for_function
  br $end_term
)
  local.get $l
  local.get $for_let
  br $end_term
)
  local.get $ta
  local.get $for_type_application
  br $end_term
)
  local.get $tl
  local.get $for_type_lambda
  br $end_term
)
  local.set $rewrite
  local.get $rewrite
  local.get $term0
  call $hydra.rewriting.rewrite_term
)
  (func $hydra.substitution.substitute_in_binding (param $subst i32) (param $b i32) (result i32)
  local.get $b
  ;; project field: name
  local.get $subst
  local.get $b
  ;; project field: term
  call $hydra.substitution.substitute_in_term
  local.get $b
  ;; project field: type
)
  (func $hydra.substitution.substitute_in_constraint (param $subst i32) (param $c i32) (result i32)
  local.get $subst
  local.get $c
  ;; project field: left
  call $hydra.substitution.subst_in_type
  local.get $subst
  local.get $c
  ;; project field: right
  call $hydra.substitution.subst_in_type
  local.get $c
  ;; project field: comment
)
  (func $hydra.substitution.substitute_in_constraints (param $subst i32) (param $cs i32) (result i32)
  local.get $subst
  call $hydra.substitution.substitute_in_constraint
  local.get $cs
  call $hydra.lib.lists.map
)
  (func $hydra.substitution.substitute_in_term (param $subst i32) (param $term0 i32) (result i32)
  (local $b i32)
  (local $bindings i32)
  (local $fun i32)
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
  nop
  local.set $s
  local.get $l
  ;; project field: parameter
  local.set $v
  local.get $v
  local.get $s
  call $hydra.lib.maps.delete
  local.set $subst2
  local.get $v
  local.get $l
  ;; project field: domain
  local.get $subst2
  local.get $l
  ;; project field: body
  call $hydra.substitution.substitute_in_term
  local.set $with_lambda
  local.get $lt
  ;; project field: bindings
  local.set $bindings
  ;; project field: name
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
  ;; project field: name
  local.get $subst2
  local.get $b
  ;; project field: term
  call $hydra.substitution.substitute_in_term
  local.get $b
  ;; project field: type
  local.set $rewrite_binding
  local.get $rewrite_binding
  local.get $bindings
  call $hydra.lib.lists.map
  local.get $subst2
  local.get $lt
  ;; project field: body
  call $hydra.substitution.substitute_in_term
  local.set $with_let
  (block $end_term (result i32)
  (block $variable
  (block $let
  (block $function
  local.get $term
  br_table $function $let $variable $variable
)
  (block $end_function (result i32)
  (block $lambda
  local.get $fun
  br_table $lambda $lambda
)
  local.get $l
  local.get $with_lambda
  br $end_function
)
  br $end_term
)
  local.get $l
  local.get $with_let
  br $end_term
)
  local.get $term
  local.get $recurse
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
