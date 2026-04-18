(module
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.reverse" (func $hydra.lib.lists.reverse (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.zip" (func $hydra.lib.lists.zip (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.insert" (func $hydra.lib.maps.insert (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.insert" (func $hydra.lib.sets.insert (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.compact_name" (func $hydra.names.compact_name (param i32) (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.unique_label" (func $hydra.names.unique_label (param i32) (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\01\00\00\00\2e\04\00\00\00\2e\6b\65\79\06\00\00\00\2e\76\61\6c\75\65\01\00\00\00\3d\03\00\00\00\61\72\67\04\00\00\00\62\6f\64\79\07\00\00\00\64\65\66\61\75\6c\74\03\00\00\00\66\75\6e\02\00\00\00\69\6e\04\00\00\00\6a\75\73\74")
  (global $__bump_ptr (mut i32) i32.const 1104 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.show.paths.subterm_step" (func $hydra.show.paths.subterm_step) )
  (export "hydra.show.paths.term_to_subterm_graph" (func $hydra.show.paths.term_to_subterm_graph) )
  (func $hydra.show.paths.subterm_step (param $step i32) (result i32)
  (local $__rec_ptr i32)
  (local $i i32)
  (local $idx i32)
  (local $idx_suff i32)
  (local $name i32)
  (local $s i32)
  (local $suffix i32)
  (local $v i32)
  i32.const 0
  local.set $idx
  local.get $s
  local.get $suffix
  call $hydra.lib.strings.cat2
  local.get $i
  drop
  local.get $idx
  drop
  i32.const 0
  call $hydra.lib.maybes.map
  local.set $idx_suff
  (block $end_subterm_step (result i32)
  (block $wrapped_term
  (block $injection_term
  (block $type_application_term
  (block $type_lambda_body
  (block $sum_term
  (block $set_element
  (block $record_field
  (block $product_term
  (block $maybe_term
  (block $map_value
  (block $map_key
  (block $list_element
  (block $let_binding
  (block $let_body
  (block $union_cases_branch
  (block $union_cases_default
  (block $lambda_body
  (block $application_argument
  (block $application_function
  (block $annotated_body
  local.get $step
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated_body $application_function $application_argument $lambda_body $union_cases_default $union_cases_branch $let_body $let_binding $list_element $map_key $map_value $maybe_term $product_term $record_field $set_element $sum_term $type_lambda_body $type_application_term $injection_term $wrapped_term $wrapped_term
)
  local.get $v
  drop
  i32.const 0
  br $end_subterm_step
)
  local.get $v
  drop
  i32.const 1078
  br $end_subterm_step
)
  local.get $v
  drop
  i32.const 1052
  br $end_subterm_step
)
  local.get $v
  drop
  i32.const 1059
  br $end_subterm_step
)
  local.get $v
  drop
  i32.const 1067
  br $end_subterm_step
)
  local.get $v
  drop
  i32.const 1024
  local.get $name
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.cat2
  br $end_subterm_step
)
  local.get $v
  drop
  i32.const 1085
  br $end_subterm_step
)
  local.get $v
  drop
  local.get $name
  drop
  i32.const 0
  drop
  i32.const 0
  i32.const 1047
  call $hydra.lib.strings.cat2
  br $end_subterm_step
)
  local.get $v
  drop
  local.get $i
  drop
  local.get $idx
  drop
  i32.const 0
  br $end_subterm_step
)
  local.get $v
  drop
  i32.const 1029
  drop
  local.get $i
  drop
  local.get $idx_suff
  drop
  i32.const 0
  br $end_subterm_step
)
  local.get $v
  drop
  i32.const 1037
  drop
  local.get $i
  drop
  local.get $idx_suff
  drop
  i32.const 0
  br $end_subterm_step
)
  local.get $v
  drop
  i32.const 1091
  br $end_subterm_step
)
  local.get $v
  drop
  local.get $i
  drop
  local.get $idx
  drop
  i32.const 0
  br $end_subterm_step
)
  local.get $v
  drop
  i32.const 1024
  local.get $name
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.cat2
  br $end_subterm_step
)
  local.get $v
  drop
  local.get $i
  drop
  local.get $idx
  drop
  i32.const 0
  br $end_subterm_step
)
  local.get $v
  drop
  i32.const 0
  br $end_subterm_step
)
  local.get $v
  drop
  i32.const 0
  br $end_subterm_step
)
  local.get $v
  drop
  i32.const 0
  br $end_subterm_step
)
  local.get $v
  drop
  i32.const 0
  br $end_subterm_step
)
  local.get $v
  drop
  i32.const 0
  br $end_subterm_step
)
)
  (func $hydra.show.paths.term_to_subterm_graph (param $namespaces i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $add_binding_name i32)
  (local $add_binding_term i32)
  (local $binding i32)
  (local $binding_names i32)
  (local $bindings i32)
  (local $current_ids i32)
  (local $current_nodes i32)
  (local $current_nodes_visited i32)
  (local $current_state i32)
  (local $current_term i32)
  (local $current_visited i32)
  (local $dont_care_step i32)
  (local $edge i32)
  (local $edges i32)
  (local $env i32)
  (local $final_edges i32)
  (local $final_nodes i32)
  (local $final_nodes_edges i32)
  (local $helper i32)
  (local $ids i32)
  (local $ids1 i32)
  (local $initial_state i32)
  (local $let_expr i32)
  (local $mroot i32)
  (local $name i32)
  (local $new_edges i32)
  (local $new_ids i32)
  (local $new_nodes i32)
  (local $new_visited i32)
  (local $next_path i32)
  (local $node i32)
  (local $node_binding i32)
  (local $node_binding_pairs i32)
  (local $nodes i32)
  (local $nodes1 i32)
  (local $nodes_edges i32)
  (local $nodes_visited_ids i32)
  (local $nodes_visited_ids1 i32)
  (local $path i32)
  (local $raw_label i32)
  (local $result i32)
  (local $root i32)
  (local $state i32)
  (local $state_after_bindings i32)
  (local $step i32)
  (local $step_term i32)
  (local $term1 i32)
  (local $unique_label i32)
  (local $v i32)
  (local $visited i32)
  (local $visited1 i32)
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
  local.set $dont_care_step
  local.get $step_term
  call $hydra.lib.pairs.first
  local.set $step
  local.get $step_term
  call $hydra.lib.pairs.second
  local.set $current_term
  local.get $state
  call $hydra.lib.pairs.first
  local.set $nodes_edges
  local.get $state
  call $hydra.lib.pairs.second
  local.set $visited
  local.get $nodes_edges
  call $hydra.lib.pairs.first
  local.set $nodes
  local.get $nodes_edges
  call $hydra.lib.pairs.second
  local.set $edges
  local.get $step
  local.get $path
  call $hydra.lib.lists.cons
  local.set $next_path
  (block $end_term (result i32)
  (block $variable
  (block $let
  local.get $current_term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $let $variable $variable
)
  local.get $v
  drop
  local.get $let_expr
  i32.load
  local.set $bindings
  local.get $let_expr
  i32.load offset=4
  local.set $env
  i32.const 0
  local.get $bindings
  call $hydra.lib.lists.map
  local.set $binding_names
  local.get $nodes_visited_ids
  call $hydra.lib.pairs.first
  local.set $current_nodes_visited
  local.get $nodes_visited_ids
  call $hydra.lib.pairs.second
  local.set $current_ids
  local.get $current_nodes_visited
  call $hydra.lib.pairs.first
  local.set $current_nodes
  local.get $current_nodes_visited
  call $hydra.lib.pairs.second
  local.set $current_visited
  local.get $namespaces
  local.get $name
  call $hydra.names.compact_name
  local.set $raw_label
  local.get $current_visited
  local.get $raw_label
  call $hydra.names.unique_label
  local.set $unique_label
  local.get $name
  local.get $raw_label
  local.get $unique_label
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
  local.set $node
  local.get $unique_label
  local.get $current_visited
  call $hydra.lib.sets.insert
  local.set $new_visited
  local.get $node
  local.get $current_nodes
  call $hydra.lib.lists.cons
  local.set $new_nodes
  local.get $name
  local.get $node
  local.get $current_ids
  call $hydra.lib.maps.insert
  local.set $new_ids
  local.get $new_nodes
  local.get $new_visited
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
  local.get $new_ids
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
  local.set $add_binding_name
  local.get $add_binding_name
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $visited
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
  local.get $ids
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
  local.get $binding_names
  call $hydra.lib.lists.foldl
  local.set $nodes_visited_ids1
  local.get $nodes_visited_ids1
  call $hydra.lib.pairs.first
  call $hydra.lib.pairs.first
  local.set $nodes1
  local.get $nodes_visited_ids1
  call $hydra.lib.pairs.first
  call $hydra.lib.pairs.second
  local.set $visited1
  local.get $nodes_visited_ids1
  call $hydra.lib.pairs.second
  local.set $ids1
  local.get $node_binding
  call $hydra.lib.pairs.first
  local.set $root
  local.get $node_binding
  call $hydra.lib.pairs.second
  local.set $binding
  local.get $binding
  i32.load offset=4
  local.set $term1
  local.get $ids1
  drop
  local.get $root
  drop
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  drop
  local.get $current_state
  drop
  local.get $dont_care_step
  local.get $term1
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
  local.get $helper
  drop
  i32.const 0
  local.set $add_binding_term
  local.get $nodes1
  local.get $bindings
  call $hydra.lib.lists.zip
  local.set $node_binding_pairs
  local.get $add_binding_term
  local.get $nodes1
  local.get $nodes
  call $hydra.lib.lists.concat2
  local.get $edges
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
  local.get $visited1
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
  local.get $node_binding_pairs
  call $hydra.lib.lists.foldl
  local.set $state_after_bindings
  local.get $ids1
  drop
  local.get $mroot
  drop
  local.get $next_path
  drop
  local.get $state_after_bindings
  drop
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
  local.get $env
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
  local.get $helper
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $state
  local.get $state
  local.get $root
  local.get $next_path
  call $hydra.lib.lists.reverse
  local.get $node
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
  local.set $edge
  local.get $edge
  local.get $edges
  call $hydra.lib.lists.cons
  local.set $new_edges
  local.get $nodes
  local.get $new_edges
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
  local.get $visited
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
  local.get $ids
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  local.get $mroot
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.set $helper
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
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
  local.set $initial_state
  i32.const 0
  drop
  i32.const 0
  drop
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  drop
  local.get $initial_state
  drop
  local.get $dont_care_step
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
  drop
  local.get $helper
  drop
  i32.const 0
  local.set $result
  local.get $result
  call $hydra.lib.pairs.first
  local.set $final_nodes_edges
  local.get $final_nodes_edges
  call $hydra.lib.pairs.first
  local.set $final_nodes
  local.get $final_nodes_edges
  call $hydra.lib.pairs.second
  local.set $final_edges
  local.get $final_nodes
  local.get $final_edges
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
)
