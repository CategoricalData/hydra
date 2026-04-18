(module
  (import "hydra.lexical" "hydra.lexical.require_binding" (func $hydra.lexical.require_binding (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.gt" (func $hydra.lib.equality.gt (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat" (func $hydra.lib.lists.concat (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.filter" (func $hydra.lib.lists.filter (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.nub" (func $hydra.lib.lists.nub (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.pure" (func $hydra.lib.lists.pure (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.zip_with" (func $hydra.lib.lists.zip_with (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int32" (func $hydra.lib.literals.show_int32 (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.insert" (func $hydra.lib.maps.insert (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.keys" (func $hydra.lib.maps.keys (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.add" (func $hydra.lib.math.add (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cat" (func $hydra.lib.maybes.cat (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.from_maybe" (func $hydra.lib.maybes.from_maybe (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.insert" (func $hydra.lib.sets.insert (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.intersection" (func $hydra.lib.sets.intersection (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.member" (func $hydra.lib.sets.member (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.union" (func $hydra.lib.sets.union (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.local_name_of" (func $hydra.names.local_name_of (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.fold_over_term" (func $hydra.rewriting.fold_over_term (param i32) (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.fold_over_type" (func $hydra.rewriting.fold_over_type (param i32) (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_term" (func $hydra.rewriting.rewrite_term (param i32) (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_type" (func $hydra.rewriting.rewrite_type (param i32) (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_type_m" (func $hydra.rewriting.rewrite_type_m (param i32) (param i32) (result i32) ) )
  (import "hydra.sorting" "hydra.sorting.find_reachable_nodes" (func $hydra.sorting.find_reachable_nodes (param i32) (param i32) (result i32) ) )
  (import "hydra.sorting" "hydra.sorting.topological_sort" (func $hydra.sorting.topological_sort (param i32) (result i32) ) )
  (import "hydra.sorting" "hydra.sorting.topological_sort_components" (func $hydra.sorting.topological_sort_components (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_term" (func $hydra.strip.deannotate_term (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.free_variables_in_term" (func $hydra.variables.free_variables_in_term (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.free_variables_in_type" (func $hydra.variables.free_variables_in_type (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.substitute_variable" (func $hydra.variables.substitute_variable (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.substitute_variables" (func $hydra.variables.substitute_variables (param i32) (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\0b\00\00\00\49\6d\70\6f\73\73\69\62\6c\65\21\18\00\00\00\4e\6f\20\73\75\63\68\20\74\79\70\65\20\69\6e\20\73\63\68\65\6d\61\3a\20\0a\00\00\00\5b\5b\5b\72\6f\6f\74\5d\5d\5d\01\00\00\00\5f")
  (global $__bump_ptr (mut i32) i32.const 1088 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.dependencies.definitions_with_dependencies" (func $hydra.dependencies.definitions_with_dependencies) )
  (export "hydra.dependencies.flatten_let_terms" (func $hydra.dependencies.flatten_let_terms) )
  (export "hydra.dependencies.inline_type" (func $hydra.dependencies.inline_type) )
  (export "hydra.dependencies.is_lambda" (func $hydra.dependencies.is_lambda) )
  (export "hydra.dependencies.lift_lambda_above_let" (func $hydra.dependencies.lift_lambda_above_let) )
  (export "hydra.dependencies.prune_let" (func $hydra.dependencies.prune_let) )
  (export "hydra.dependencies.replace_typedefs" (func $hydra.dependencies.replace_typedefs) )
  (export "hydra.dependencies.simplify_term" (func $hydra.dependencies.simplify_term) )
  (export "hydra.dependencies.term_dependency_names" (func $hydra.dependencies.term_dependency_names) )
  (export "hydra.dependencies.to_short_names" (func $hydra.dependencies.to_short_names) )
  (export "hydra.dependencies.topological_sort_binding_map" (func $hydra.dependencies.topological_sort_binding_map) )
  (export "hydra.dependencies.topological_sort_bindings" (func $hydra.dependencies.topological_sort_bindings) )
  (export "hydra.dependencies.topological_sort_type_definitions" (func $hydra.dependencies.topological_sort_type_definitions) )
  (export "hydra.dependencies.type_dependency_names" (func $hydra.dependencies.type_dependency_names) )
  (export "hydra.dependencies.type_names_in_type" (func $hydra.dependencies.type_names_in_type) )
  (func $hydra.dependencies.definitions_with_dependencies (param $cx i32) (param $graph i32) (param $original i32) (result i32)
  (local $all_dep_names i32)
  (local $dep_names i32)
  (local $el i32)
  (local $name i32)
  i32.const 1
  i32.const 0
  i32.const 0
  local.get $el
  i32.load offset=4
  call $hydra.dependencies.term_dependency_names
  call $hydra.lib.sets.to_list
  local.set $dep_names
  i32.const 0
  local.get $original
  call $hydra.lib.lists.map
  local.get $dep_names
  local.get $original
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  call $hydra.lib.lists.concat2
  call $hydra.lib.lists.nub
  local.set $all_dep_names
  local.get $graph
  local.get $name
  call $hydra.lexical.require_binding
  local.get $all_dep_names
  call $hydra.lib.eithers.map_list
)
  (func $hydra.dependencies.flatten_let_terms (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $ann i32)
  (local $arg_ i32)
  (local $at i32)
  (local $b i32)
  (local $binding i32)
  (local $bindings i32)
  (local $bindings1 i32)
  (local $body i32)
  (local $body1 i32)
  (local $deps i32)
  (local $flatten i32)
  (local $flatten_body_let i32)
  (local $flattened_bindings i32)
  (local $for_result i32)
  (local $hr i32)
  (local $inner_binding i32)
  (local $inner_bindings i32)
  (local $inner_body i32)
  (local $inner_let i32)
  (local $inner_lt i32)
  (local $key0 i32)
  (local $lt i32)
  (local $merged i32)
  (local $n i32)
  (local $new_binding i32)
  (local $new_bindings i32)
  (local $new_body i32)
  (local $prefix i32)
  (local $qualify i32)
  (local $recurse i32)
  (local $recursive i32)
  (local $replace_vars i32)
  (local $rewrite_binding i32)
  (local $rewritten i32)
  (local $subst i32)
  (local $t i32)
  (local $term2 i32)
  (local $to_subst_pair i32)
  (local $v i32)
  (local $val0 i32)
  (local $val1 i32)
  (local $val2 i32)
  local.get $binding
  i32.load
  local.set $key0
  local.get $binding
  i32.load offset=4
  local.set $val0
  local.get $binding
  i32.load offset=8
  local.set $t
  (block $end_term (result i32)
  (block $let
  (block $annotated
  local.get $val0
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $let $let
)
  local.get $v
  drop
  local.get $at
  i32.load
  local.set $val1
  local.get $at
  i32.load offset=4
  local.set $ann
  local.get $key0
  local.get $val1
  local.get $t
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
  drop
  local.get $rewrite_binding
  drop
  i32.const 0
  local.set $recursive
  local.get $recursive
  call $hydra.lib.pairs.first
  local.set $inner_binding
  local.get $recursive
  call $hydra.lib.pairs.second
  local.set $deps
  local.get $inner_binding
  i32.load offset=4
  local.set $val2
  local.get $key0
  i32.const 0
  local.get $val2
  local.get $ann
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
  local.get $t
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
  local.get $deps
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
  local.get $inner_let
  i32.load
  local.set $bindings1
  local.get $inner_let
  i32.load offset=4
  local.set $body1
  local.get $key0
  drop
  i32.const 0
  drop
  i32.const 0
  i32.const 1081
  call $hydra.lib.strings.cat2
  local.set $prefix
  local.get $prefix
  local.get $n
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.cat2
  local.set $qualify
  local.get $b
  i32.load
  local.get $b
  i32.load
  drop
  local.get $qualify
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
  local.set $to_subst_pair
  local.get $to_subst_pair
  local.get $bindings1
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $subst
  local.get $subst
  i32.const 0
  call $hydra.variables.substitute_variables
  local.set $replace_vars
  local.get $body1
  drop
  local.get $replace_vars
  drop
  i32.const 0
  local.set $new_body
  local.get $b
  i32.load
  drop
  local.get $qualify
  drop
  i32.const 0
  local.get $b
  i32.load offset=4
  drop
  local.get $replace_vars
  drop
  i32.const 0
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
  local.set $new_binding
  local.get $key0
  local.get $new_body
  local.get $t
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
  local.get $new_binding
  local.get $bindings1
  call $hydra.lib.lists.map
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
  local.set $rewrite_binding
  (block $end_term (result i32)
  (block $let
  local.get $body
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $let $let
)
  local.get $v
  drop
  local.get $inner_lt
  i32.load
  local.set $inner_bindings
  local.get $inner_lt
  i32.load offset=4
  local.set $inner_body
  local.get $bindings
  local.get $inner_bindings
  call $hydra.lib.lists.concat2
  drop
  local.get $inner_body
  drop
  local.get $flatten_body_let
  drop
  i32.const 0
  br $end_term
)
  local.set $flatten_body_let
  local.get $term2
  drop
  local.get $recurse
  drop
  i32.const 0
  local.set $rewritten
  (block $end_term (result i32)
  (block $let
  local.get $rewritten
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $let $let
)
  local.get $v
  drop
  local.get $lt
  i32.load
  local.set $bindings
  local.get $lt
  i32.load offset=4
  local.set $body
  local.get $hr
  call $hydra.lib.pairs.second
  local.get $hr
  call $hydra.lib.pairs.first
  call $hydra.lib.lists.pure
  call $hydra.lib.lists.concat2
  local.set $for_result
  local.get $arg_
  drop
  local.get $rewrite_binding
  drop
  i32.const 0
  drop
  local.get $for_result
  drop
  i32.const 0
  local.get $bindings
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  local.set $flattened_bindings
  local.get $flattened_bindings
  drop
  local.get $body
  drop
  local.get $flatten_body_let
  drop
  i32.const 0
  local.set $merged
  local.get $merged
  call $hydra.lib.pairs.first
  local.set $new_bindings
  local.get $merged
  call $hydra.lib.pairs.second
  local.set $new_body
  i32.const 6
  local.get $new_bindings
  local.get $new_body
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
  local.set $flatten
  local.get $flatten
  local.get $term
  call $hydra.rewriting.rewrite_term
)
  (func $hydra.dependencies.inline_type (param $schema i32) (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $after_recurse i32)
  (local $f i32)
  (local $recurse i32)
  (local $tr i32)
  (local $typ2 i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $variable
  local.get $tr
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
  i32.const 0
  i32.const 6
  i32.const 1039
  local.get $v
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.cat2
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
  local.get $schema
  i32.const 0
  call $hydra.dependencies.inline_type
  local.get $v
  local.get $schema
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  br $end_type
)
  local.set $after_recurse
  local.get $typ2
  drop
  local.get $recurse
  drop
  i32.const 0
  local.get $tr
  drop
  local.get $after_recurse
  drop
  i32.const 0
  call $hydra.lib.eithers.bind
  local.set $f
  local.get $f
  local.get $typ
  call $hydra.rewriting.rewrite_type_m
)
  (func $hydra.dependencies.is_lambda (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $lt i32)
  (local $v i32)
  (block $end_term (result i32)
  (block $let
  (block $lambda
  local.get $term
  call $hydra.strip.deannotate_term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $lambda $let $let
)
  local.get $v
  drop
  i32.const 1
  br $end_term
)
  local.get $v
  drop
  local.get $lt
  i32.load offset=4
  call $hydra.dependencies.is_lambda
  br $end_term
)
)
  (func $hydra.dependencies.lift_lambda_above_let (param $term0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $at i32)
  (local $b i32)
  (local $bs i32)
  (local $cons i32)
  (local $dig_for_lambdas i32)
  (local $l i32)
  (local $original i32)
  (local $recurse i32)
  (local $rewrite i32)
  (local $rewrite_binding i32)
  (local $rewrite_bindings i32)
  (local $t i32)
  (local $term i32)
  (local $term2 i32)
  (local $v i32)
  local.get $b
  i32.load
  local.get $recurse
  drop
  local.get $b
  i32.load offset=4
  drop
  local.get $rewrite
  drop
  i32.const 0
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
  local.get $rewrite_binding
  local.get $bs
  call $hydra.lib.lists.map
  local.set $rewrite_bindings
  (block $end_term (result i32)
  (block $let
  (block $lambda
  (block $annotated
  local.get $term2
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $lambda $let $let
)
  local.get $v
  drop
  local.get $original
  drop
  i32.const 0
  local.get $t
  drop
  local.get $cons
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
  drop
  local.get $at
  i32.load
  drop
  local.get $dig_for_lambdas
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 5
  local.get $l
  i32.load
  local.get $l
  i32.load offset=4
  local.get $l
  i32.load offset=8
  drop
  local.get $cons
  drop
  i32.const 0
  drop
  local.get $t
  drop
  local.get $cons
  drop
  i32.const 0
  drop
  local.get $l
  i32.load offset=8
  drop
  local.get $dig_for_lambdas
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
  local.get $original
  drop
  i32.const 6
  local.get $l
  i32.load
  drop
  local.get $rewrite_bindings
  drop
  i32.const 0
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
  drop
  local.get $cons
  drop
  i32.const 0
  drop
  local.get $l
  i32.load offset=4
  drop
  local.get $dig_for_lambdas
  drop
  i32.const 0
  br $end_term
)
  local.set $dig_for_lambdas
  (block $end_term (result i32)
  (block $let
  local.get $term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $let $let
)
  local.get $v
  drop
  local.get $term
  drop
  i32.const 6
  local.get $l
  i32.load
  drop
  local.get $rewrite_bindings
  drop
  i32.const 0
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
  drop
  local.get $l
  i32.load offset=4
  drop
  local.get $dig_for_lambdas
  drop
  i32.const 0
  br $end_term
)
  local.set $rewrite
  local.get $rewrite
  local.get $term0
  call $hydra.rewriting.rewrite_term
)
  (func $hydra.dependencies.prune_let (param $l i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $adj i32)
  (local $b i32)
  (local $binding_map i32)
  (local $n i32)
  (local $pruned_bindings i32)
  (local $reachable i32)
  (local $root_name i32)
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
  local.get $l
  i32.load
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $binding_map
  i32.const 1067
  local.set $root_name
  local.get $binding_map
  call $hydra.lib.maps.keys
  call $hydra.lib.sets.from_list
  local.get $n
  local.get $root_name
  call $hydra.lib.equality.equal
  local.get $l
  i32.load offset=4
  i32.const 17
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
  local.get $n
  local.get $binding_map
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.from_maybe
  call $hydra.lib.logic.if_else
  call $hydra.variables.free_variables_in_term
  call $hydra.lib.sets.intersection
  local.set $adj
  local.get $adj
  local.get $root_name
  call $hydra.sorting.find_reachable_nodes
  local.set $reachable
  local.get $b
  i32.load
  local.get $reachable
  call $hydra.lib.sets.member
  local.get $l
  i32.load
  call $hydra.lib.lists.filter
  local.set $pruned_bindings
  local.get $pruned_bindings
  local.get $l
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
)
  (func $hydra.dependencies.replace_typedefs (param $types i32) (param $typ0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $at i32)
  (local $for_mono i32)
  (local $for_type_scheme i32)
  (local $recurse i32)
  (local $rewrite i32)
  (local $t i32)
  (local $ts i32)
  (local $typ i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $wrap
  (block $variable
  (block $union
  (block $record
  (block $annotated
  local.get $typ
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $record $union $variable $wrap $wrap
)
  local.get $v
  drop
  i32.const 0
  local.get $recurse
  drop
  local.get $at
  i32.load
  drop
  local.get $rewrite
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
  br $end_type
)
  local.get $v
  drop
  local.get $typ
  br $end_type
)
  local.get $v
  drop
  local.get $typ
  br $end_type
)
  local.get $v
  drop
  (block $end_type (result i32)
  (block $wrap
  (block $union
  (block $record
  local.get $t
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $union $wrap $wrap
)
  local.get $v
  drop
  local.get $typ
  br $end_type
)
  local.get $v
  drop
  local.get $typ
  br $end_type
)
  local.get $v
  drop
  local.get $typ
  br $end_type
)
  local.set $for_mono
  local.get $ts
  i32.load offset=4
  local.set $t
  local.get $ts
  i32.load
  call $hydra.lib.lists.null
  local.get $t
  drop
  local.get $for_mono
  drop
  i32.const 0
  local.get $typ
  call $hydra.lib.logic.if_else
  local.set $for_type_scheme
  local.get $typ
  local.get $ts
  drop
  local.get $for_type_scheme
  drop
  i32.const 0
  local.get $v
  local.get $types
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  br $end_type
)
  local.get $v
  drop
  local.get $typ
  br $end_type
)
  local.set $rewrite
  local.get $rewrite
  local.get $typ0
  call $hydra.rewriting.rewrite_type
)
  (func $hydra.dependencies.simplify_term (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $app i32)
  (local $body i32)
  (local $for_lhs i32)
  (local $for_rhs i32)
  (local $for_term i32)
  (local $l i32)
  (local $lhs i32)
  (local $recurse i32)
  (local $rhs i32)
  (local $simplify i32)
  (local $stripped i32)
  (local $term2 i32)
  (local $v i32)
  (local $var i32)
  (block $end_term (result i32)
  (block $variable
  local.get $rhs
  call $hydra.strip.deannotate_term
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
  local.get $var
  local.get $v
  local.get $body
  call $hydra.variables.substitute_variable
  call $hydra.dependencies.simplify_term
  br $end_term
)
  local.set $for_rhs
  (block $end_term (result i32)
  (block $lambda
  local.get $lhs
  call $hydra.strip.deannotate_term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $lambda $lambda
)
  local.get $v
  drop
  local.get $l
  i32.load
  local.set $var
  local.get $l
  i32.load offset=8
  local.set $body
  local.get $var
  local.get $body
  call $hydra.variables.free_variables_in_term
  call $hydra.lib.sets.member
  local.get $rhs
  drop
  local.get $var
  drop
  local.get $body
  drop
  local.get $for_rhs
  drop
  i32.const 0
  local.get $body
  call $hydra.dependencies.simplify_term
  call $hydra.lib.logic.if_else
  br $end_term
)
  local.set $for_lhs
  (block $end_term (result i32)
  (block $application
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $application $application
)
  local.get $v
  drop
  local.get $app
  i32.load
  local.set $lhs
  local.get $app
  i32.load offset=4
  local.set $rhs
  local.get $lhs
  drop
  local.get $rhs
  drop
  local.get $for_lhs
  drop
  i32.const 0
  br $end_term
)
  local.set $for_term
  local.get $term2
  call $hydra.strip.deannotate_term
  local.set $stripped
  local.get $stripped
  drop
  local.get $for_term
  drop
  i32.const 0
  drop
  local.get $recurse
  drop
  i32.const 0
  local.set $simplify
  local.get $simplify
  local.get $term
  call $hydra.rewriting.rewrite_term
)
  (func $hydra.dependencies.term_dependency_names (param $binds i32) (param $with_prims i32) (param $with_noms i32) (param $term0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $add_names i32)
  (local $case_stmt i32)
  (local $injection i32)
  (local $name i32)
  (local $names i32)
  (local $nominal i32)
  (local $prim i32)
  (local $proj i32)
  (local $record i32)
  (local $term i32)
  (local $v i32)
  (local $var i32)
  (local $wrapped_term i32)
  local.get $with_noms
  local.get $name
  local.get $names
  call $hydra.lib.sets.insert
  local.get $names
  call $hydra.lib.logic.if_else
  local.set $nominal
  local.get $with_prims
  local.get $name
  local.get $names
  call $hydra.lib.sets.insert
  local.get $names
  call $hydra.lib.logic.if_else
  local.set $prim
  local.get $binds
  local.get $name
  local.get $names
  call $hydra.lib.sets.insert
  local.get $names
  call $hydra.lib.logic.if_else
  local.set $var
  (block $end_term (result i32)
  (block $wrap
  (block $variable
  (block $inject
  (block $record
  (block $unwrap
  (block $project
  (block $cases
  local.get $term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $cases $project $unwrap $record $inject $variable $wrap $wrap
)
  local.get $v
  drop
  local.get $case_stmt
  i32.load
  drop
  local.get $nominal
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $proj
  i32.load
  drop
  local.get $nominal
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $name
  drop
  local.get $nominal
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $record
  i32.load
  drop
  local.get $nominal
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $injection
  i32.load
  drop
  local.get $nominal
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $name
  drop
  local.get $var
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $wrapped_term
  i32.load
  drop
  local.get $nominal
  drop
  i32.const 0
  br $end_term
)
  local.set $add_names
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
  local.get $add_names
  i32.const 0
  local.get $term0
  call $hydra.rewriting.fold_over_term
)
  (func $hydra.dependencies.to_short_names (param $original i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $acc i32)
  (local $add_name i32)
  (local $group i32)
  (local $group_names_by_local i32)
  (local $groups i32)
  (local $i i32)
  (local $local i32)
  (local $local_names i32)
  (local $name i32)
  (local $names i32)
  (local $range_from i32)
  (local $rename i32)
  (local $rename_group i32)
  (local $start i32)
  local.get $name
  call $hydra.names.local_name_of
  local.set $local
  i32.const 0
  local.get $local
  local.get $acc
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.from_maybe
  local.set $group
  local.get $local
  local.get $name
  local.get $group
  call $hydra.lib.sets.insert
  local.get $acc
  call $hydra.lib.maps.insert
  local.set $add_name
  local.get $add_name
  i32.const 0
  local.get $names
  call $hydra.lib.lists.foldl
  local.set $group_names_by_local
  local.get $original
  drop
  local.get $group_names_by_local
  drop
  i32.const 0
  local.set $groups
  local.get $local_names
  call $hydra.lib.pairs.first
  local.set $local
  local.get $local_names
  call $hydra.lib.pairs.second
  local.set $names
  local.get $start
  local.get $start
  i32.const 1
  call $hydra.lib.math.add
  drop
  local.get $range_from
  drop
  i32.const 0
  call $hydra.lib.lists.cons
  local.set $range_from
  local.get $name
  local.get $i
  i32.const 1
  call $hydra.lib.equality.gt
  local.get $local
  local.get $i
  call $hydra.lib.literals.show_int32
  call $hydra.lib.strings.cat2
  local.get $local
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
  local.set $rename
  local.get $rename
  local.get $names
  call $hydra.lib.sets.to_list
  i32.const 1
  drop
  local.get $range_from
  drop
  i32.const 0
  call $hydra.lib.lists.zip_with
  local.set $rename_group
  local.get $rename_group
  local.get $groups
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  call $hydra.lib.maps.from_list
)
  (func $hydra.dependencies.topological_sort_binding_map (param $binding_map i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $at i32)
  (local $bindings i32)
  (local $deps_of i32)
  (local $has_type_annotation i32)
  (local $keys i32)
  (local $name i32)
  (local $name_and_term i32)
  (local $term i32)
  (local $to_pair i32)
  (local $v i32)
  local.get $binding_map
  call $hydra.lib.maps.to_list
  local.set $bindings
  i32.const 0
  local.get $bindings
  call $hydra.lib.lists.map
  call $hydra.lib.sets.from_list
  local.set $keys
  (block $end_term (result i32)
  (block $annotated
  local.get $term
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
  local.get $has_type_annotation
  drop
  i32.const 0
  br $end_term
)
  local.set $has_type_annotation
  local.get $name_and_term
  call $hydra.lib.pairs.first
  local.set $name
  local.get $name_and_term
  call $hydra.lib.pairs.second
  local.set $term
  local.get $name
  local.get $term
  drop
  local.get $has_type_annotation
  drop
  i32.const 0
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $keys
  local.get $term
  call $hydra.variables.free_variables_in_term
  call $hydra.lib.sets.intersection
  call $hydra.lib.sets.to_list
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
  local.set $deps_of
  local.get $name
  i32.const 8
  i32.const 5
  i32.const 1024
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
  local.get $name
  local.get $binding_map
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
  local.set $to_pair
  local.get $to_pair
  i32.const 0
  call $hydra.lib.lists.map
  local.get $deps_of
  local.get $bindings
  call $hydra.lib.lists.map
  call $hydra.sorting.topological_sort_components
  call $hydra.lib.lists.map
)
  (func $hydra.dependencies.topological_sort_bindings (param $els i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $adjlist i32)
  (local $e i32)
  local.get $e
  i32.load
  i32.const 0
  i32.const 1
  i32.const 1
  local.get $e
  i32.load offset=4
  call $hydra.dependencies.term_dependency_names
  call $hydra.lib.sets.to_list
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
  local.set $adjlist
  local.get $adjlist
  local.get $els
  call $hydra.lib.lists.map
  call $hydra.sorting.topological_sort
)
  (func $hydra.dependencies.topological_sort_type_definitions (param $defs i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $d i32)
  (local $def i32)
  (local $n i32)
  (local $name_to_def i32)
  (local $names i32)
  (local $sorted i32)
  (local $to_pair i32)
  local.get $def
  i32.load
  i32.const 0
  local.get $def
  i32.load offset=4
  i32.load offset=4
  call $hydra.dependencies.type_dependency_names
  call $hydra.lib.sets.to_list
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
  local.set $to_pair
  local.get $d
  i32.load
  local.get $d
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
  local.get $defs
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $name_to_def
  local.get $to_pair
  local.get $defs
  call $hydra.lib.lists.map
  call $hydra.sorting.topological_sort_components
  local.set $sorted
  local.get $n
  local.get $name_to_def
  call $hydra.lib.maps.lookup
  local.get $names
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  local.get $sorted
  call $hydra.lib.lists.map
)
  (func $hydra.dependencies.type_dependency_names (param $with_schema i32) (param $typ i32) (result i32)
  local.get $with_schema
  local.get $typ
  call $hydra.variables.free_variables_in_type
  local.get $typ
  call $hydra.dependencies.type_names_in_type
  call $hydra.lib.sets.union
  local.get $typ
  call $hydra.variables.free_variables_in_type
  call $hydra.lib.logic.if_else
)
  (func $hydra.dependencies.type_names_in_type (param $typ0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $add_names i32)
  (local $names i32)
  local.get $names
  local.set $add_names
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
  local.get $add_names
  i32.const 0
  local.get $typ0
  call $hydra.rewriting.fold_over_type
)
)
