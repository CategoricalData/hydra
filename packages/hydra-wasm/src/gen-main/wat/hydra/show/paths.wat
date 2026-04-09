(module
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.reverse" (func $hydra.lib.lists.reverse (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.zip" (func $hydra.lib.lists.zip (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.empty" (func $hydra.lib.maps.empty (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.insert" (func $hydra.lib.maps.insert (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.empty" (func $hydra.lib.sets.empty (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.insert" (func $hydra.lib.sets.insert (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.compact_name" (func $hydra.names.compact_name (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.unique_label" (func $hydra.names.unique_label (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.show.paths.subterm_step" (func $hydra.show.paths.subterm_step) )
  (export "hydra.show.paths.term_to_subterm_graph" (func $hydra.show.paths.term_to_subterm_graph) )
  (func $hydra.show.paths.subterm_step (param $step i32) (result i32)
  (local $i i32)
  (local $idx i32)
  (local $idx_suff i32)
  (local $s i32)
  (local $suffix i32)
  i32.const 0
  local.set $idx
  local.get $s
  local.get $suffix
  call $hydra.lib.strings.cat2
  local.get $i
  local.get $idx
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
  br_table $annotated_body $application_function $application_argument $lambda_body $union_cases_default $union_cases_branch $let_body $let_binding $list_element $map_key $map_value $maybe_term $product_term $record_field $set_element $sum_term $type_lambda_body $type_application_term $injection_term $wrapped_term $wrapped_term
)
  i32.const 0
  br $end_subterm_step
)
  i32.const 0 ;; string: "fun"
  br $end_subterm_step
)
  i32.const 0 ;; string: "arg"
  br $end_subterm_step
)
  i32.const 0 ;; string: "body"
  br $end_subterm_step
)
  i32.const 0 ;; string: "default"
  br $end_subterm_step
)
  i32.const 0 ;; string: "."
  nop
  call $hydra.lib.strings.cat2
  br $end_subterm_step
)
  i32.const 0 ;; string: "in"
  br $end_subterm_step
)
  nop
  i32.const 0 ;; string: "="
  call $hydra.lib.strings.cat2
  br $end_subterm_step
)
  local.get $i
  local.get $idx
  br $end_subterm_step
)
  i32.const 0 ;; string: ".key"
  local.get $i
  local.get $idx_suff
  br $end_subterm_step
)
  i32.const 0 ;; string: ".value"
  local.get $i
  local.get $idx_suff
  br $end_subterm_step
)
  i32.const 0 ;; string: "just"
  br $end_subterm_step
)
  local.get $i
  local.get $idx
  br $end_subterm_step
)
  i32.const 0 ;; string: "."
  nop
  call $hydra.lib.strings.cat2
  br $end_subterm_step
)
  local.get $i
  local.get $idx
  br $end_subterm_step
)
  i32.const 0
  br $end_subterm_step
)
  i32.const 0
  br $end_subterm_step
)
  i32.const 0
  br $end_subterm_step
)
  i32.const 0
  br $end_subterm_step
)
  i32.const 0
  br $end_subterm_step
)
)
  (func $hydra.show.paths.term_to_subterm_graph (param $namespaces i32) (param $term i32) (result i32)
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
  (local $visited i32)
  (local $visited1 i32)
  i32.const 0
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
  br_table $let $variable $variable
)
  local.get $let_expr
  ;; project field: bindings
  local.set $bindings
  local.get $let_expr
  ;; project field: body
  local.set $env
  ;; project field: name
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
  local.get $new_ids
  local.set $add_binding_name
  local.get $add_binding_name
  i32.const 0
  ;; list elements follow
  local.get $visited
  local.get $ids
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
  ;; project field: term
  local.set $term1
  local.get $ids1
  local.get $root
  i32.const 0
  ;; list elements follow
  local.get $current_state
  local.get $dont_care_step
  local.get $term1
  local.get $helper
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
  local.get $visited1
  local.get $node_binding_pairs
  call $hydra.lib.lists.foldl
  local.set $state_after_bindings
  local.get $ids1
  local.get $mroot
  local.get $next_path
  local.get $state_after_bindings
  i32.const 0
  local.get $env
  local.get $helper
  br $end_term
)
  local.get $state
  local.get $state
  local.get $root
  local.get $next_path
  call $hydra.lib.lists.reverse
  local.get $node
  local.set $edge
  local.get $edge
  local.get $edges
  call $hydra.lib.lists.cons
  local.set $new_edges
  local.get $nodes
  local.get $new_edges
  local.get $visited
  local.get $name
  local.get $ids
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  local.get $mroot
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.set $helper
  i32.const 0
  ;; list elements follow
  i32.const 0
  ;; list elements follow
  call $hydra.lib.sets.empty
  local.set $initial_state
  call $hydra.lib.maps.empty
  i32.const 0
  i32.const 0
  ;; list elements follow
  local.get $initial_state
  local.get $dont_care_step
  local.get $term
  local.get $helper
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
)
)
