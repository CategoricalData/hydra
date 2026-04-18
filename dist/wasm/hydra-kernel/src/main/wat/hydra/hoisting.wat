(module
  (import "hydra.environment" "hydra.environment.term_as_bindings" (func $hydra.environment.term_as_bindings (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.choose_unique_name" (func $hydra.lexical.choose_unique_name (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.gte" (func $hydra.lib.equality.gte (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat" (func $hydra.lib.lists.concat (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.filter" (func $hydra.lib.lists.filter (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.maybe_head" (func $hydra.lib.lists.maybe_head (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.nub" (func $hydra.lib.lists.nub (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.partition" (func $hydra.lib.lists.partition (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.reverse" (func $hydra.lib.lists.reverse (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.uncons" (func $hydra.lib.lists.uncons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.zip" (func $hydra.lib.lists.zip (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.and" (func $hydra.lib.logic.and (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.or" (func $hydra.lib.logic.or (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.keys" (func $hydra.lib.maps.keys (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.map" (func $hydra.lib.maps.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.add" (func $hydra.lib.math.add (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cat" (func $hydra.lib.maybes.cat (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.difference" (func $hydra.lib.sets.difference (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.insert" (func $hydra.lib.sets.insert (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.intersection" (func $hydra.lib.sets.intersection (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.member" (func $hydra.lib.sets.member (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.null" (func $hydra.lib.sets.null (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.singleton" (func $hydra.lib.sets.singleton (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.union" (func $hydra.lib.sets.union (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.unions" (func $hydra.lib.sets.unions (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.intercalate" (func $hydra.lib.strings.intercalate (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.split_on" (func $hydra.lib.strings.split_on (param i32) (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_and_fold_term_with_graph" (func $hydra.rewriting.rewrite_and_fold_term_with_graph (param i32) (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_and_fold_term_with_graph_and_path" (func $hydra.rewriting.rewrite_and_fold_term_with_graph_and_path (param i32) (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.subterms" (func $hydra.rewriting.subterms (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.extend_graph_for_let" (func $hydra.scoping.extend_graph_for_let (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.sorting" "hydra.sorting.propagate_tags" (func $hydra.sorting.propagate_tags (param i32) (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_and_detype_term" (func $hydra.strip.deannotate_and_detype_term (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_type_parameters" (func $hydra.strip.deannotate_type_parameters (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.strip_type_lambdas" (func $hydra.strip.strip_type_lambdas (param i32) (result i32) ) )
  (import "hydra.substitution" "hydra.substitution.substitute_in_binding" (func $hydra.substitution.substitute_in_binding (param i32) (param i32) (result i32) ) )
  (import "hydra.substitution" "hydra.substitution.substitute_in_term" (func $hydra.substitution.substitute_in_term (param i32) (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.free_variables_in_term" (func $hydra.variables.free_variables_in_term (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.free_variables_in_type" (func $hydra.variables.free_variables_in_type (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\01\00\00\00\2e\01\00\00\00\5f\05\00\00\00\5f\62\6f\64\79\07\00\00\00\5f\68\6f\69\73\74\5f\04\00\00\00\62\6f\64\79")
  (global $__bump_ptr (mut i32) i32.const 1072 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.hoisting.augment_bindings_with_new_free_vars" (func $hydra.hoisting.augment_bindings_with_new_free_vars) )
  (export "hydra.hoisting.binding_is_polymorphic" (func $hydra.hoisting.binding_is_polymorphic) )
  (export "hydra.hoisting.binding_uses_context_type_vars" (func $hydra.hoisting.binding_uses_context_type_vars) )
  (export "hydra.hoisting.count_var_occurrences" (func $hydra.hoisting.count_var_occurrences) )
  (export "hydra.hoisting.hoist_all_let_bindings" (func $hydra.hoisting.hoist_all_let_bindings) )
  (export "hydra.hoisting.hoist_case_statements" (func $hydra.hoisting.hoist_case_statements) )
  (export "hydra.hoisting.hoist_case_statements_in_graph" (func $hydra.hoisting.hoist_case_statements_in_graph) )
  (export "hydra.hoisting.hoist_let_bindings_with_context" (func $hydra.hoisting.hoist_let_bindings_with_context) )
  (export "hydra.hoisting.hoist_let_bindings_with_predicate" (func $hydra.hoisting.hoist_let_bindings_with_predicate) )
  (export "hydra.hoisting.hoist_polymorphic_let_bindings" (func $hydra.hoisting.hoist_polymorphic_let_bindings) )
  (export "hydra.hoisting.hoist_subterms" (func $hydra.hoisting.hoist_subterms) )
  (export "hydra.hoisting.is_application_function" (func $hydra.hoisting.is_application_function) )
  (export "hydra.hoisting.is_lambda_body" (func $hydra.hoisting.is_lambda_body) )
  (export "hydra.hoisting.is_union_elimination" (func $hydra.hoisting.is_union_elimination) )
  (export "hydra.hoisting.is_union_elimination_application" (func $hydra.hoisting.is_union_elimination_application) )
  (export "hydra.hoisting.normalize_path_for_hoisting" (func $hydra.hoisting.normalize_path_for_hoisting) )
  (export "hydra.hoisting.should_hoist_all" (func $hydra.hoisting.should_hoist_all) )
  (export "hydra.hoisting.should_hoist_case_statement" (func $hydra.hoisting.should_hoist_case_statement) )
  (export "hydra.hoisting.should_hoist_polymorphic" (func $hydra.hoisting.should_hoist_polymorphic) )
  (export "hydra.hoisting.update_hoist_state" (func $hydra.hoisting.update_hoist_state) )
  (func $hydra.hoisting.augment_bindings_with_new_free_vars (param $cx i32) (param $bound_vars i32) (param $bindings i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $acc i32)
  (local $augment i32)
  (local $b i32)
  (local $free_vars i32)
  (local $results i32)
  (local $t i32)
  (local $term i32)
  (local $tl i32)
  (local $ts i32)
  (local $types i32)
  (local $v i32)
  (local $var_type_pairs i32)
  (local $var_types i32)
  (local $vars i32)
  (local $wrap_after_type_lambdas i32)
  i32.const 0
  local.get $cx
  i32.load offset=4
  call $hydra.lib.maps.map
  local.set $types
  (block $end_term (result i32)
  (block $type_lambda
  local.get $term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $type_lambda $type_lambda
)
  local.get $v
  drop
  i32.const 16
  local.get $tl
  i32.load
  local.get $vars
  drop
  local.get $tl
  i32.load offset=4
  drop
  local.get $wrap_after_type_lambdas
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
  local.set $wrap_after_type_lambdas
  local.get $bound_vars
  local.get $b
  i32.load offset=4
  call $hydra.variables.free_variables_in_term
  call $hydra.lib.sets.intersection
  call $hydra.lib.sets.to_list
  local.set $free_vars
  local.get $v
  local.get $v
  local.get $types
  call $hydra.lib.maps.lookup
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
  local.get $free_vars
  call $hydra.lib.lists.map
  local.set $var_type_pairs
  i32.const 0
  local.get $var_type_pairs
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  local.set $var_types
  local.get $free_vars
  call $hydra.lib.lists.null
  local.get $var_types
  call $hydra.lib.lists.length
  local.get $var_type_pairs
  call $hydra.lib.lists.length
  call $hydra.lib.equality.equal
  call $hydra.lib.logic.not
  call $hydra.lib.logic.or
  local.get $b
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
  local.get $b
  i32.load
  local.get $var_type_pairs
  drop
  local.get $b
  i32.load offset=4
  drop
  local.get $wrap_after_type_lambdas
  drop
  i32.const 0
  local.get $ts
  i32.load
  i32.const 4
  local.get $t
  local.get $acc
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
  local.get $ts
  i32.load offset=4
  local.get $var_types
  call $hydra.lib.lists.reverse
  call $hydra.lib.lists.foldl
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
  local.get $b
  i32.load
  i32.const 1
  local.get $t
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
  i32.const 19
  local.get $b
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
  local.get $free_vars
  call $hydra.lib.lists.foldl
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
  local.set $augment
  local.get $augment
  local.get $bindings
  call $hydra.lib.lists.map
  local.set $results
  i32.const 0
  local.get $results
  call $hydra.lib.lists.map
  i32.const 0
  local.get $results
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  call $hydra.lib.maps.from_list
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
  (func $hydra.hoisting.binding_is_polymorphic (param $binding i32) (result i32)
  (local $ts i32)
  i32.const 0
  local.get $ts
  i32.load
  call $hydra.lib.lists.null
  call $hydra.lib.logic.not
  local.get $binding
  i32.load offset=8
  call $hydra.lib.maybes.maybe
)
  (func $hydra.hoisting.binding_uses_context_type_vars (param $cx i32) (param $binding i32) (result i32)
  (local $context_type_vars i32)
  (local $free_in_type i32)
  (local $ts i32)
  i32.const 0
  local.get $ts
  i32.load offset=4
  call $hydra.variables.free_variables_in_type
  local.set $free_in_type
  local.get $cx
  i32.load offset=28
  local.set $context_type_vars
  local.get $free_in_type
  local.get $context_type_vars
  call $hydra.lib.sets.intersection
  call $hydra.lib.sets.null
  call $hydra.lib.logic.not
  local.get $binding
  i32.load offset=8
  call $hydra.lib.maybes.maybe
)
  (func $hydra.hoisting.count_var_occurrences (param $name i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $acc i32)
  (local $child_count i32)
  (local $t i32)
  (local $v i32)
  local.get $acc
  local.get $name
  local.get $t
  call $hydra.hoisting.count_var_occurrences
  call $hydra.lib.math.add
  i32.const 0
  local.get $term
  call $hydra.rewriting.subterms
  call $hydra.lib.lists.foldl
  local.set $child_count
  (block $end_term (result i32)
  (block $variable
  local.get $term
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
  local.get $name
  call $hydra.lib.equality.equal
  i32.const 1
  local.get $child_count
  call $hydra.lib.math.add
  local.get $child_count
  call $hydra.lib.logic.if_else
  br $end_term
)
)
  (func $hydra.hoisting.hoist_all_let_bindings (param $let0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $empty_cx i32)
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
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
  local.set $empty_cx
  i32.const 1
  i32.const 0
  local.get $empty_cx
  local.get $let0
  call $hydra.hoisting.hoist_let_bindings_with_predicate
)
  (func $hydra.hoisting.hoist_case_statements (result i32)
  i32.const 0
  i32.const 0
  i32.const 0
  call $hydra.hoisting.hoist_subterms
)
  (func $hydra.hoisting.hoist_case_statements_in_graph (param $bindings i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $empty_tx i32)
  (local $term0 i32)
  (local $term1 i32)
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
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
  local.set $empty_tx
  i32.const 6
  local.get $bindings
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
  local.set $term0
  local.get $empty_tx
  local.get $term0
  call $hydra.hoisting.hoist_case_statements
  local.set $term1
  local.get $term1
  call $hydra.environment.term_as_bindings
)
  (func $hydra.hoisting.hoist_let_bindings_with_context (param $is_parent_binding i32) (param $cx i32) (param $let0 i32) (result i32)
  local.get $is_parent_binding
  i32.const 0
  local.get $cx
  local.get $let0
  call $hydra.hoisting.hoist_let_bindings_with_predicate
)
  (func $hydra.hoisting.hoist_let_bindings_with_predicate (param $is_parent_binding i32) (param $should_hoist_binding i32) (param $cx0 i32) (param $let0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $a i32)
  (local $already_used_names i32)
  (local $augment_result i32)
  (local $augment_subst i32)
  (local $b i32)
  (local $binding_and_replacement_pairs i32)
  (local $binding_dependencies i32)
  (local $binding_edges i32)
  (local $binding_immediate_captured_vars i32)
  (local $binding_with_captured_vars i32)
  (local $bindings_and_names i32)
  (local $bindings_so_far i32)
  (local $bindings_so_far_augmented i32)
  (local $bindings_so_far_final i32)
  (local $bindings_so_far_subst i32)
  (local $bindings_with_captured_vars i32)
  (local $body i32)
  (local $body_final i32)
  (local $body_only_subst i32)
  (local $body_subst i32)
  (local $body_with_cache i32)
  (local $bound_term_variables i32)
  (local $cache_bindings i32)
  (local $captured_term_var_type_pairs i32)
  (local $captured_term_var_types i32)
  (local $captured_term_vars i32)
  (local $captured_type_vars i32)
  (local $captured_vars_map i32)
  (local $cx i32)
  (local $cx1 i32)
  (local $dom i32)
  (local $empty_bindings_and_names i32)
  (local $final_term i32)
  (local $final_used_names i32)
  (local $for_active_binding i32)
  (local $for_binding i32)
  (local $free_in_binding_type i32)
  (local $free_in_captured_var_types i32)
  (local $free_variables_in_each_binding i32)
  (local $full_subst i32)
  (local $global_binding_name i32)
  (local $hoist_binding_map i32)
  (local $hoist_name_replacement_pairs i32)
  (local $hoist_one i32)
  (local $hoist_pairs i32)
  (local $hoist_pairs_and_names i32)
  (local $hoist_us i32)
  (local $hoisted_binding_names i32)
  (local $hoisted_bindings i32)
  (local $hoisted_bindings_final i32)
  (local $hoisted_bindings_subst i32)
  (local $init i32)
  (local $is_cacheable i32)
  (local $is_poly i32)
  (local $keep_us i32)
  (local $keep_us_final i32)
  (local $keep_us_subst i32)
  (local $l i32)
  (local $multi_ref i32)
  (local $multi_ref_pairs i32)
  (local $name i32)
  (local $new_binding_and_replacement i32)
  (local $new_bindings_and_names i32)
  (local $new_pairs i32)
  (local $new_term i32)
  (local $new_type_scheme i32)
  (local $new_used_names i32)
  (local $orig_type i32)
  (local $p i32)
  (local $pair i32)
  (local $partition_pair i32)
  (local $poly_let_variables i32)
  (local $prefix i32)
  (local $previously_finished_bindings i32)
  (local $recurse i32)
  (local $replacement i32)
  (local $replacements i32)
  (local $result i32)
  (local $result_bindings i32)
  (local $result_pair i32)
  (local $result_term i32)
  (local $rewrite i32)
  (local $single_ref_pairs i32)
  (local $stripped_term i32)
  (local $t i32)
  (local $term i32)
  (local $term_with_lambdas i32)
  (local $term_with_type_lambdas i32)
  (local $ts i32)
  (local $typ i32)
  (local $types i32)
  (local $v i32)
  (local $vars i32)
  (local $with_type_apps i32)
  local.get $pair
  call $hydra.lib.pairs.first
  local.set $binding_and_replacement_pairs
  local.get $pair
  call $hydra.lib.pairs.second
  local.set $already_used_names
  local.get $binding_with_captured_vars
  call $hydra.lib.pairs.first
  local.set $b
  local.get $binding_with_captured_vars
  call $hydra.lib.pairs.second
  local.set $captured_term_vars
  i32.const 0
  local.get $cx
  i32.load offset=4
  call $hydra.lib.maps.map
  local.set $types
  local.get $v
  local.get $v
  local.get $types
  call $hydra.lib.maps.lookup
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
  local.get $captured_term_vars
  call $hydra.lib.lists.map
  local.set $captured_term_var_type_pairs
  local.get $typ
  call $hydra.strip.deannotate_type_parameters
  i32.const 0
  local.get $captured_term_var_type_pairs
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  call $hydra.lib.lists.map
  local.set $captured_term_var_types
  i32.const 0
  local.get $ts
  i32.load offset=4
  call $hydra.variables.free_variables_in_type
  local.get $b
  i32.load offset=8
  call $hydra.lib.maybes.maybe
  local.set $free_in_binding_type
  local.get $t
  call $hydra.variables.free_variables_in_type
  local.get $captured_term_var_types
  call $hydra.lib.lists.map
  call $hydra.lib.sets.unions
  local.set $free_in_captured_var_types
  local.get $cx
  i32.load offset=28
  local.get $free_in_binding_type
  local.get $free_in_captured_var_types
  call $hydra.lib.sets.union
  call $hydra.lib.sets.intersection
  call $hydra.lib.sets.to_list
  local.set $captured_type_vars
  local.get $already_used_names
  local.get $prefix
  local.get $b
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.cat2
  call $hydra.lexical.choose_unique_name
  local.set $global_binding_name
  local.get $global_binding_name
  local.get $already_used_names
  call $hydra.lib.sets.insert
  local.set $new_used_names
  local.get $captured_term_var_types
  call $hydra.lib.lists.length
  local.get $captured_term_var_type_pairs
  call $hydra.lib.lists.length
  call $hydra.lib.equality.equal
  local.get $captured_type_vars
  local.get $ts
  i32.load
  call $hydra.lib.lists.concat2
  call $hydra.lib.lists.nub
  i32.const 4
  local.get $a
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
  local.get $ts
  i32.load offset=4
  local.get $captured_term_var_types
  call $hydra.lib.lists.reverse
  call $hydra.lib.lists.foldl
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
  local.get $b
  i32.load offset=8
  call $hydra.lib.maybes.map
  i32.const 0
  call $hydra.lib.logic.if_else
  local.set $new_type_scheme
  local.get $b
  i32.load offset=4
  call $hydra.strip.strip_type_lambdas
  local.set $stripped_term
  i32.const 5
  local.get $p
  call $hydra.lib.pairs.first
  local.get $dom
  call $hydra.strip.deannotate_type_parameters
  local.get $p
  call $hydra.lib.pairs.second
  call $hydra.lib.maybes.map
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
  local.get $stripped_term
  local.get $captured_term_var_type_pairs
  call $hydra.lib.lists.reverse
  call $hydra.lib.lists.foldl
  local.set $term_with_lambdas
  i32.const 16
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
  local.get $term_with_lambdas
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  i32.const 0
  local.get $new_type_scheme
  call $hydra.lib.maybes.maybe
  call $hydra.lib.lists.reverse
  call $hydra.lib.lists.foldl
  local.set $term_with_type_lambdas
  i32.const 15
  local.get $t
  i32.const 14
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
  i32.const 19
  local.get $global_binding_name
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
  local.get $captured_type_vars
  call $hydra.lib.lists.foldl
  local.set $with_type_apps
  i32.const 1
  local.get $t
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
  local.get $with_type_apps
  local.get $captured_term_vars
  call $hydra.lib.lists.foldl
  local.set $replacement
  local.get $global_binding_name
  local.get $term_with_type_lambdas
  local.get $new_type_scheme
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
  local.get $replacement
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
  local.set $new_binding_and_replacement
  local.get $new_binding_and_replacement
  local.get $binding_and_replacement_pairs
  call $hydra.lib.lists.cons
  local.set $new_pairs
  local.get $new_pairs
  local.get $new_used_names
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
  local.set $hoist_one
  local.get $bindings_and_names
  call $hydra.lib.pairs.first
  local.set $previously_finished_bindings
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $bindings_and_names
  call $hydra.lib.pairs.second
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
  local.set $empty_bindings_and_names
  local.get $empty_bindings_and_names
  drop
  local.get $term
  drop
  local.get $recurse
  drop
  i32.const 0
  local.set $result
  local.get $result
  call $hydra.lib.pairs.first
  local.set $new_bindings_and_names
  local.get $new_bindings_and_names
  call $hydra.lib.pairs.first
  local.set $bindings_so_far
  local.get $new_bindings_and_names
  call $hydra.lib.pairs.second
  local.set $already_used_names
  local.get $result
  call $hydra.lib.pairs.second
  local.set $new_term
  (block $end_term (result i32)
  (block $let
  local.get $new_term
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
  local.get $l
  i32.load offset=4
  local.set $body
  local.get $cx
  drop
  local.get $should_hoist_binding
  drop
  i32.const 0
  local.get $l
  i32.load
  call $hydra.lib.lists.partition
  local.set $partition_pair
  local.get $partition_pair
  call $hydra.lib.pairs.first
  local.set $hoist_us
  local.get $partition_pair
  call $hydra.lib.pairs.second
  local.set $keep_us
  i32.const 0
  local.get $hoist_us
  call $hydra.lib.lists.map
  local.set $hoisted_binding_names
  i32.const 0
  i32.const 0
  i32.const 0
  local.get $v
  local.get $cx
  i32.load offset=4
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.map
  call $hydra.lib.maybes.maybe
  local.get $cx
  i32.load
  call $hydra.lib.maps.keys
  call $hydra.lib.sets.from_list
  local.get $cx
  i32.load offset=12
  call $hydra.lib.sets.difference
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.filter
  call $hydra.lib.sets.from_list
  local.set $poly_let_variables
  local.get $cx
  i32.load offset=12
  local.get $cx
  i32.load
  call $hydra.lib.maps.keys
  call $hydra.lib.sets.from_list
  local.get $cx
  i32.load offset=12
  call $hydra.lib.sets.difference
  call $hydra.lib.sets.union
  local.set $bound_term_variables
  local.get $bound_term_variables
  local.get $b
  i32.load offset=4
  call $hydra.variables.free_variables_in_term
  call $hydra.lib.sets.intersection
  call $hydra.lib.sets.to_list
  local.get $hoist_us
  call $hydra.lib.lists.map
  local.set $free_variables_in_each_binding
  local.get $v
  local.get $hoisted_binding_names
  call $hydra.lib.sets.from_list
  call $hydra.lib.sets.member
  local.get $vars
  call $hydra.lib.lists.partition
  local.get $free_variables_in_each_binding
  call $hydra.lib.lists.map
  local.set $binding_dependencies
  local.get $hoisted_binding_names
  i32.const 0
  local.get $binding_dependencies
  call $hydra.lib.lists.map
  call $hydra.lib.lists.zip
  local.set $binding_edges
  local.get $hoisted_binding_names
  i32.const 0
  local.get $binding_dependencies
  call $hydra.lib.lists.map
  call $hydra.lib.lists.zip
  local.set $binding_immediate_captured_vars
  local.get $binding_edges
  local.get $binding_immediate_captured_vars
  call $hydra.sorting.propagate_tags
  call $hydra.lib.maps.from_list
  local.set $captured_vars_map
  local.get $b
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $vars
  local.get $poly_let_variables
  call $hydra.lib.sets.difference
  call $hydra.lib.sets.to_list
  local.get $b
  i32.load
  local.get $captured_vars_map
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
  local.get $hoist_us
  call $hydra.lib.lists.map
  local.set $bindings_with_captured_vars
  local.get $prefix
  drop
  local.get $cx
  drop
  local.get $hoist_one
  drop
  i32.const 0
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $already_used_names
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
  local.get $bindings_with_captured_vars
  call $hydra.lib.lists.foldl
  local.set $hoist_pairs_and_names
  local.get $hoist_pairs_and_names
  call $hydra.lib.pairs.first
  call $hydra.lib.lists.reverse
  local.set $hoist_pairs
  i32.const 0
  local.get $hoist_pairs
  call $hydra.lib.lists.map
  local.set $hoisted_bindings
  i32.const 0
  local.get $hoist_pairs
  call $hydra.lib.lists.map
  local.set $replacements
  local.get $hoist_pairs_and_names
  call $hydra.lib.pairs.second
  local.set $final_used_names
  i32.const 0
  local.get $hoist_us
  call $hydra.lib.lists.map
  local.get $replacements
  call $hydra.lib.lists.zip
  local.set $hoist_name_replacement_pairs
  local.get $b
  i32.load
  local.get $b
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
  local.get $hoist_us
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $hoist_binding_map
  local.get $name
  local.get $body
  call $hydra.hoisting.count_var_occurrences
  i32.const 2
  call $hydra.lib.equality.gte
  local.set $multi_ref
  i32.const 0
  local.get $b
  call $hydra.hoisting.binding_is_polymorphic
  local.get $name
  local.get $hoist_binding_map
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  local.set $is_poly
  local.get $multi_ref
  local.get $is_poly
  call $hydra.lib.logic.not
  call $hydra.lib.logic.and
  local.set $is_cacheable
  local.get $p
  call $hydra.lib.pairs.first
  drop
  local.get $is_cacheable
  drop
  i32.const 0
  call $hydra.lib.logic.not
  local.get $hoist_name_replacement_pairs
  call $hydra.lib.lists.filter
  local.set $single_ref_pairs
  local.get $p
  call $hydra.lib.pairs.first
  drop
  local.get $is_cacheable
  drop
  i32.const 0
  local.get $hoist_name_replacement_pairs
  call $hydra.lib.lists.filter
  local.set $multi_ref_pairs
  local.get $hoist_name_replacement_pairs
  call $hydra.lib.maps.from_list
  local.set $full_subst
  local.get $single_ref_pairs
  call $hydra.lib.maps.from_list
  local.set $body_only_subst
  local.get $body_only_subst
  local.get $body
  call $hydra.substitution.substitute_in_term
  local.set $body_subst
  i32.const 0
  local.get $b
  i32.load offset=8
  local.get $p
  call $hydra.lib.pairs.first
  local.get $hoist_binding_map
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  local.set $orig_type
  local.get $p
  call $hydra.lib.pairs.first
  local.get $p
  call $hydra.lib.pairs.second
  local.get $orig_type
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
  local.get $multi_ref_pairs
  call $hydra.lib.lists.map
  local.set $cache_bindings
  local.get $cache_bindings
  call $hydra.lib.lists.null
  local.get $body_subst
  i32.const 6
  local.get $cache_bindings
  local.get $body_subst
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
  local.set $body_with_cache
  local.get $full_subst
  i32.const 0
  call $hydra.substitution.substitute_in_binding
  local.get $keep_us
  call $hydra.lib.lists.map
  local.set $keep_us_subst
  local.get $full_subst
  i32.const 0
  call $hydra.substitution.substitute_in_binding
  local.get $hoisted_bindings
  call $hydra.lib.lists.map
  local.set $hoisted_bindings_subst
  local.get $full_subst
  i32.const 0
  call $hydra.substitution.substitute_in_binding
  local.get $bindings_so_far
  call $hydra.lib.lists.map
  local.set $bindings_so_far_subst
  local.get $cx
  local.get $bound_term_variables
  local.get $poly_let_variables
  call $hydra.lib.sets.difference
  local.get $bindings_so_far_subst
  call $hydra.hoisting.augment_bindings_with_new_free_vars
  local.set $augment_result
  local.get $augment_result
  call $hydra.lib.pairs.first
  local.set $bindings_so_far_augmented
  local.get $augment_result
  call $hydra.lib.pairs.second
  local.set $augment_subst
  local.get $augment_subst
  i32.const 0
  call $hydra.substitution.substitute_in_binding
  local.get $hoisted_bindings_subst
  call $hydra.lib.lists.map
  local.set $hoisted_bindings_final
  local.get $augment_subst
  i32.const 0
  call $hydra.substitution.substitute_in_binding
  local.get $bindings_so_far_augmented
  call $hydra.lib.lists.map
  local.set $bindings_so_far_final
  local.get $augment_subst
  local.get $body_with_cache
  call $hydra.substitution.substitute_in_term
  local.set $body_final
  local.get $augment_subst
  i32.const 0
  call $hydra.substitution.substitute_in_binding
  local.get $keep_us_subst
  call $hydra.lib.lists.map
  local.set $keep_us_final
  local.get $keep_us_final
  call $hydra.lib.lists.null
  local.get $body_final
  i32.const 6
  local.get $keep_us_final
  local.get $body_final
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
  local.set $final_term
  local.get $previously_finished_bindings
  local.get $hoisted_bindings_final
  local.get $bindings_so_far_final
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
  call $hydra.lib.lists.concat
  local.get $final_used_names
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
  local.get $final_term
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
  i32.const 0
  local.get $cx0
  local.get $let0
  call $hydra.scoping.extend_graph_for_let
  local.set $cx1
  local.get $b
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  i32.const 1029
  call $hydra.lib.strings.cat2
  local.set $prefix
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $b
  i32.load
  call $hydra.lib.sets.singleton
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
  local.set $init
  local.get $prefix
  drop
  local.get $rewrite
  drop
  i32.const 0
  local.get $cx1
  local.get $init
  local.get $b
  i32.load offset=4
  call $hydra.rewriting.rewrite_and_fold_term_with_graph
  local.set $result_pair
  local.get $result_pair
  call $hydra.lib.pairs.first
  call $hydra.lib.pairs.first
  local.set $result_bindings
  local.get $result_pair
  call $hydra.lib.pairs.second
  local.set $result_term
  local.get $b
  i32.load
  local.get $result_term
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
  local.get $result_bindings
  call $hydra.lib.lists.cons
  local.set $for_active_binding
  local.get $b
  drop
  local.get $is_parent_binding
  drop
  i32.const 0
  local.get $b
  drop
  local.get $for_active_binding
  drop
  i32.const 0
  local.get $b
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
  local.set $for_binding
  local.get $for_binding
  local.get $let0
  i32.load
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  local.get $let0
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
  (func $hydra.hoisting.hoist_polymorphic_let_bindings (param $is_parent_binding i32) (param $let0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $empty_cx i32)
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
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
  local.set $empty_cx
  local.get $is_parent_binding
  i32.const 0
  local.get $empty_cx
  local.get $let0
  call $hydra.hoisting.hoist_let_bindings_with_predicate
)
  (func $hydra.hoisting.hoist_subterms (param $should_hoist i32) (param $cx0 i32) (param $term0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $acc i32)
  (local $b i32)
  (local $baseline_lambda_vars i32)
  (local $binding i32)
  (local $binding_path_prefix i32)
  (local $bindings i32)
  (local $body i32)
  (local $body_path_prefix i32)
  (local $body_prefix i32)
  (local $body_result i32)
  (local $collect_and_replace i32)
  (local $collected_bindings i32)
  (local $counter i32)
  (local $current_counter i32)
  (local $cx i32)
  (local $final_acc i32)
  (local $final_counter i32)
  (local $first_binding_name i32)
  (local $local_let i32)
  (local $lt i32)
  (local $lt2 i32)
  (local $name_prefix i32)
  (local $new_binding i32)
  (local $new_bindings i32)
  (local $new_bindings_reversed i32)
  (local $new_body i32)
  (local $new_counter i32)
  (local $new_value i32)
  (local $path i32)
  (local $process_binding i32)
  (local $process_immediate_subterm i32)
  (local $process_let_term i32)
  (local $recurse i32)
  (local $recursed i32)
  (local $recursed_term i32)
  (local $result i32)
  (local $rewrite i32)
  (local $subterm i32)
  (local $term i32)
  (local $transformed_subterm i32)
  (local $v i32)
  local.get $cx
  i32.load offset=12
  local.set $baseline_lambda_vars
  local.get $acc
  call $hydra.lib.pairs.first
  local.set $current_counter
  local.get $acc
  call $hydra.lib.pairs.second
  local.set $collected_bindings
  (block $end_term (result i32)
  (block $type_lambda
  (block $let
  local.get $term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $let $type_lambda $type_lambda
)
  local.get $v
  drop
  local.get $acc
  local.get $term
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
  local.get $acc
  local.get $term
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
  local.set $collect_and_replace
  local.get $collect_and_replace
  local.get $cx
  local.get $counter
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
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
  local.get $subterm
  call $hydra.rewriting.rewrite_and_fold_term_with_graph_and_path
  local.set $result
  local.get $result
  call $hydra.lib.pairs.first
  local.set $final_acc
  local.get $result
  call $hydra.lib.pairs.second
  local.set $transformed_subterm
  local.get $final_acc
  call $hydra.lib.pairs.first
  local.set $final_counter
  local.get $final_acc
  call $hydra.lib.pairs.second
  local.set $bindings
  local.get $bindings
  call $hydra.lib.lists.null
  local.get $final_counter
  local.get $transformed_subterm
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
  i32.const 6
  local.get $bindings
  call $hydra.lib.lists.reverse
  local.get $transformed_subterm
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
  local.set $local_let
  local.get $final_counter
  local.get $local_let
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
  local.set $process_immediate_subterm
  local.get $lt
  i32.load
  local.set $bindings
  local.get $lt
  i32.load offset=4
  local.set $body
  i32.const 1029
  i32.const 1024
  local.get $binding
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.split_on
  call $hydra.lib.strings.intercalate
  local.set $name_prefix
  local.get $path
  i32.const 7
  local.get $binding
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
  call $hydra.lib.lists.concat2
  local.set $binding_path_prefix
  local.get $cx
  drop
  i32.const 1
  drop
  local.get $name_prefix
  drop
  local.get $binding_path_prefix
  drop
  local.get $binding
  i32.load offset=4
  drop
  local.get $process_immediate_subterm
  drop
  i32.const 0
  local.set $result
  local.get $result
  call $hydra.lib.pairs.second
  local.set $new_value
  local.get $binding
  i32.load
  local.get $new_value
  local.get $binding
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
  local.get $new_binding
  local.get $acc
  call $hydra.lib.lists.cons
  local.set $process_binding
  local.get $process_binding
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $bindings
  call $hydra.lib.lists.foldl
  local.set $new_bindings_reversed
  local.get $new_bindings_reversed
  call $hydra.lib.lists.reverse
  local.set $new_bindings
  local.get $path
  i32.const 6
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
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.lib.lists.concat2
  local.set $body_path_prefix
  i32.const 1054
  i32.const 1029
  i32.const 1024
  local.get $b
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.split_on
  call $hydra.lib.strings.intercalate
  local.get $bindings
  call $hydra.lib.lists.maybe_head
  call $hydra.lib.maybes.maybe
  local.set $first_binding_name
  local.get $first_binding_name
  i32.const 1034
  call $hydra.lib.strings.cat2
  local.set $body_prefix
  local.get $cx
  drop
  i32.const 1
  drop
  local.get $body_prefix
  drop
  local.get $body_path_prefix
  drop
  local.get $body
  drop
  local.get $process_immediate_subterm
  drop
  i32.const 0
  local.set $body_result
  local.get $body_result
  call $hydra.lib.pairs.second
  local.set $new_body
  local.get $counter
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
  local.set $process_let_term
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
  local.get $counter
  drop
  local.get $term
  drop
  local.get $recurse
  drop
  i32.const 0
  local.set $recursed
  local.get $recursed
  call $hydra.lib.pairs.first
  local.set $new_counter
  local.get $recursed
  call $hydra.lib.pairs.second
  local.set $recursed_term
  (block $end_term (result i32)
  (block $let
  local.get $recursed_term
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
  local.get $cx
  drop
  local.get $new_counter
  drop
  local.get $path
  drop
  local.get $lt2
  drop
  local.get $process_let_term
  drop
  i32.const 0
  br $end_term
)
  br $end_term
)
  local.set $rewrite
  local.get $rewrite
  local.get $cx0
  i32.const 1
  local.get $term0
  call $hydra.rewriting.rewrite_and_fold_term_with_graph_and_path
  call $hydra.lib.pairs.second
)
  (func $hydra.hoisting.is_application_function (param $acc i32) (result i32)
  (local $__rec_ptr i32)
  (local $v i32)
  (block $end_subterm_step (result i32)
  (block $application_function
  local.get $acc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $application_function $application_function
)
  local.get $v
  drop
  i32.const 1
  br $end_subterm_step
)
)
  (func $hydra.hoisting.is_lambda_body (param $acc i32) (result i32)
  (local $__rec_ptr i32)
  (local $v i32)
  (block $end_subterm_step (result i32)
  (block $lambda_body
  local.get $acc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $lambda_body $lambda_body
)
  local.get $v
  drop
  i32.const 1
  br $end_subterm_step
)
)
  (func $hydra.hoisting.is_union_elimination (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $v i32)
  (block $end_term (result i32)
  (block $cases
  local.get $term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $cases $cases
)
  local.get $v
  drop
  i32.const 1
  br $end_term
)
)
  (func $hydra.hoisting.is_union_elimination_application (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $app i32)
  (local $v i32)
  (block $end_term (result i32)
  (block $application
  local.get $term
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
  call $hydra.strip.deannotate_and_detype_term
  call $hydra.hoisting.is_union_elimination
  br $end_term
)
)
  (func $hydra.hoisting.normalize_path_for_hoisting (param $path i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $after_first i32)
  (local $first i32)
  (local $go i32)
  (local $remaining i32)
  (local $rest i32)
  (local $second i32)
  (local $uc1 i32)
  (local $uc2 i32)
  local.get $remaining
  local.get $uc1
  call $hydra.lib.pairs.first
  local.set $first
  local.get $uc1
  call $hydra.lib.pairs.second
  local.set $after_first
  local.get $remaining
  local.get $uc2
  call $hydra.lib.pairs.first
  local.set $second
  local.get $uc2
  call $hydra.lib.pairs.second
  local.set $rest
  local.get $first
  call $hydra.hoisting.is_application_function
  local.get $second
  call $hydra.hoisting.is_lambda_body
  call $hydra.lib.logic.and
  i32.const 6
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
  local.get $rest
  drop
  local.get $go
  drop
  i32.const 0
  call $hydra.lib.lists.cons
  local.get $first
  local.get $after_first
  drop
  local.get $go
  drop
  i32.const 0
  call $hydra.lib.lists.cons
  call $hydra.lib.logic.if_else
  local.get $after_first
  call $hydra.lib.lists.uncons
  call $hydra.lib.maybes.maybe
  local.get $remaining
  call $hydra.lib.lists.uncons
  call $hydra.lib.maybes.maybe
  local.set $go
  local.get $path
  drop
  local.get $go
  drop
  i32.const 0
)
  (func $hydra.hoisting.should_hoist_all (param $_ i32) (param $_2 i32) (result i32)
  i32.const 1
)
  (func $hydra.hoisting.should_hoist_case_statement (param $path_and_term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $acc i32)
  (local $final_state i32)
  (local $path i32)
  (local $st i32)
  (local $term i32)
  local.get $path_and_term
  call $hydra.lib.pairs.first
  local.set $path
  local.get $path_and_term
  call $hydra.lib.pairs.second
  local.set $term
  local.get $term
  call $hydra.hoisting.is_union_elimination
  local.get $term
  call $hydra.hoisting.is_union_elimination_application
  call $hydra.lib.logic.or
  call $hydra.lib.logic.not
  i32.const 0
  local.get $acc
  local.get $st
  call $hydra.hoisting.update_hoist_state
  i32.const 1
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
  local.get $path
  call $hydra.lib.lists.foldl
  local.set $final_state
  local.get $final_state
  call $hydra.lib.pairs.first
  call $hydra.lib.logic.not
  call $hydra.lib.logic.if_else
)
  (func $hydra.hoisting.should_hoist_polymorphic (param $cx i32) (param $binding i32) (result i32)
  local.get $binding
  call $hydra.hoisting.binding_is_polymorphic
  local.get $cx
  local.get $binding
  call $hydra.hoisting.binding_uses_context_type_vars
  call $hydra.lib.logic.or
)
  (func $hydra.hoisting.update_hoist_state (param $accessor i32) (param $state i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $at_top i32)
  (local $used_app i32)
  (local $v i32)
  local.get $state
  call $hydra.lib.pairs.first
  local.set $at_top
  local.get $state
  call $hydra.lib.pairs.second
  local.set $used_app
  local.get $at_top
  call $hydra.lib.logic.not
  i32.const 0
  local.get $used_app
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
  (block $end_subterm_step (result i32)
  (block $application_argument
  (block $application_function
  (block $union_cases_default
  (block $union_cases_branch
  (block $lambda_body
  (block $let_binding
  (block $let_body
  (block $annotated_body
  local.get $accessor
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated_body $let_body $let_binding $lambda_body $union_cases_branch $union_cases_default $application_function $application_argument $application_argument
)
  local.get $v
  drop
  i32.const 1
  local.get $used_app
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
  br $end_subterm_step
)
  local.get $v
  drop
  i32.const 1
  local.get $used_app
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
  br $end_subterm_step
)
  local.get $v
  drop
  i32.const 1
  local.get $used_app
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
  br $end_subterm_step
)
  local.get $v
  drop
  local.get $used_app
  i32.const 0
  i32.const 1
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
  i32.const 1
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
  call $hydra.lib.logic.if_else
  br $end_subterm_step
)
  local.get $v
  drop
  local.get $used_app
  i32.const 0
  i32.const 1
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
  i32.const 1
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
  call $hydra.lib.logic.if_else
  br $end_subterm_step
)
  local.get $v
  drop
  local.get $used_app
  i32.const 0
  i32.const 1
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
  i32.const 1
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
  call $hydra.lib.logic.if_else
  br $end_subterm_step
)
  local.get $v
  drop
  local.get $used_app
  i32.const 0
  i32.const 1
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
  i32.const 1
  i32.const 1
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
  br $end_subterm_step
)
  local.get $v
  drop
  i32.const 0
  local.get $used_app
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
  br $end_subterm_step
)
  call $hydra.lib.logic.if_else
)
)
