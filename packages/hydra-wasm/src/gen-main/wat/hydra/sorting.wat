(module
  (import "hydra.constants" "hydra.constants.max_int32" (func $hydra.constants.max_int32 (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.identity" (func $hydra.lib.equality.identity (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.min" (func $hydra.lib.equality.min (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat" (func $hydra.lib.lists.concat (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.filter" (func $hydra.lib.lists.filter (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.head" (func $hydra.lib.lists.head (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.reverse" (func $hydra.lib.lists.reverse (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.sort" (func $hydra.lib.lists.sort (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.sort_on" (func $hydra.lib.lists.sort_on (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.tail" (func $hydra.lib.lists.tail (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.zip" (func $hydra.lib.lists.zip (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.empty" (func $hydra.lib.maps.empty (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.find_with_default" (func $hydra.lib.maps.find_with_default (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.insert" (func $hydra.lib.maps.insert (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.keys" (func $hydra.lib.maps.keys (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.map" (func $hydra.lib.maps.map (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.member" (func $hydra.lib.maps.member (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.add" (func $hydra.lib.math.add (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.range" (func $hydra.lib.math.range (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cat" (func $hydra.lib.maybes.cat (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.from_just" (func $hydra.lib.maybes.from_just (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map_maybe" (func $hydra.lib.maybes.map_maybe (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.delete" (func $hydra.lib.sets.delete (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.difference" (func $hydra.lib.sets.difference (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.empty" (func $hydra.lib.sets.empty (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.insert" (func $hydra.lib.sets.insert (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.member" (func $hydra.lib.sets.member (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.null" (func $hydra.lib.sets.null (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.singleton" (func $hydra.lib.sets.singleton (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.unions" (func $hydra.lib.sets.unions (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.sorting.adjacency_list_to_map" (func $hydra.sorting.adjacency_list_to_map) )
  (export "hydra.sorting.adjacency_lists_to_graph" (func $hydra.sorting.adjacency_lists_to_graph) )
  (export "hydra.sorting.create_ordering_isomorphism" (func $hydra.sorting.create_ordering_isomorphism) )
  (export "hydra.sorting.find_reachable_nodes" (func $hydra.sorting.find_reachable_nodes) )
  (export "hydra.sorting.initial_state" (func $hydra.sorting.initial_state) )
  (export "hydra.sorting.pop_stack_until" (func $hydra.sorting.pop_stack_until) )
  (export "hydra.sorting.propagate_tags" (func $hydra.sorting.propagate_tags) )
  (export "hydra.sorting.strong_connect" (func $hydra.sorting.strong_connect) )
  (export "hydra.sorting.strongly_connected_components" (func $hydra.sorting.strongly_connected_components) )
  (export "hydra.sorting.topological_sort" (func $hydra.sorting.topological_sort) )
  (export "hydra.sorting.topological_sort_components" (func $hydra.sorting.topological_sort_components) )
  (export "hydra.sorting.topological_sort_nodes" (func $hydra.sorting.topological_sort_nodes) )
  (func $hydra.sorting.adjacency_list_to_map (param $pairs i32) (result i32)
  (local $existing i32)
  (local $k i32)
  (local $mp i32)
  (local $p i32)
  (local $vs i32)
  local.get $p
  call $hydra.lib.pairs.first
  local.set $k
  local.get $p
  call $hydra.lib.pairs.second
  local.set $vs
  i32.const 0
  ;; list elements follow
  call $hydra.lib.equality.identity
  local.get $k
  local.get $mp
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  local.set $existing
  local.get $k
  local.get $existing
  local.get $vs
  call $hydra.lib.lists.concat2
  local.get $mp
  call $hydra.lib.maps.insert
  call $hydra.lib.maps.empty
  local.get $pairs
  call $hydra.lib.lists.foldl
)
  (func $hydra.sorting.adjacency_lists_to_graph (param $edges0 i32) (result i32)
  (local $graph i32)
  (local $indexed_edges i32)
  (local $k i32)
  (local $k_neighbors i32)
  (local $key_to_vertex i32)
  (local $neighbors i32)
  (local $sorted_edges i32)
  (local $v i32)
  (local $vertex_map i32)
  (local $vertex_to_key i32)
  (local $vk_neighbors i32)
  call $hydra.lib.pairs.first
  local.get $edges0
  call $hydra.lib.lists.sort_on
  local.set $sorted_edges
  i32.const 0
  local.get $sorted_edges
  call $hydra.lib.lists.length
  call $hydra.lib.math.range
  local.get $sorted_edges
  call $hydra.lib.lists.zip
  local.set $indexed_edges
  local.get $vk_neighbors
  call $hydra.lib.pairs.first
  local.set $v
  local.get $vk_neighbors
  call $hydra.lib.pairs.second
  local.set $k_neighbors
  local.get $k_neighbors
  call $hydra.lib.pairs.first
  local.set $k
  local.get $k
  local.get $v
  local.get $indexed_edges
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $key_to_vertex
  local.get $vk_neighbors
  call $hydra.lib.pairs.first
  local.set $v
  local.get $vk_neighbors
  call $hydra.lib.pairs.second
  local.set $k_neighbors
  local.get $k_neighbors
  call $hydra.lib.pairs.first
  local.set $k
  local.get $v
  local.get $k
  local.get $indexed_edges
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $vertex_map
  local.get $vk_neighbors
  call $hydra.lib.pairs.first
  local.set $v
  local.get $vk_neighbors
  call $hydra.lib.pairs.second
  local.set $k_neighbors
  local.get $k_neighbors
  call $hydra.lib.pairs.second
  local.set $neighbors
  local.get $v
  local.get $k
  local.get $key_to_vertex
  call $hydra.lib.maps.lookup
  local.get $neighbors
  call $hydra.lib.maybes.map_maybe
  local.get $indexed_edges
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $graph
  local.get $v
  local.get $vertex_map
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.from_just
  local.set $vertex_to_key
  local.get $graph
  local.get $vertex_to_key
)
  (func $hydra.sorting.create_ordering_isomorphism (param $source_ord i32) (param $target_ord i32) (result i32)
  (local $els i32)
  (local $mp i32)
  (local $n i32)
  (local $source_to_target_mapping i32)
  (local $target_to_source_mapping i32)
  local.get $source_ord
  local.get $els
  call $hydra.lib.lists.zip
  call $hydra.lib.maps.from_list
  local.set $mp
  local.get $n
  local.get $mp
  call $hydra.lib.maps.lookup
  local.get $target_ord
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  local.set $source_to_target_mapping
  local.get $target_ord
  local.get $els
  call $hydra.lib.lists.zip
  call $hydra.lib.maps.from_list
  local.set $mp
  local.get $n
  local.get $mp
  call $hydra.lib.maps.lookup
  local.get $source_ord
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  local.set $target_to_source_mapping
  local.get $source_to_target_mapping
  local.get $target_to_source_mapping
)
  (func $hydra.sorting.find_reachable_nodes (param $adj i32) (param $root i32) (result i32)
  (local $n i32)
  (local $node i32)
  (local $to_visit i32)
  (local $v i32)
  (local $visit i32)
  (local $visited i32)
  local.get $node
  local.get $adj
  local.get $visited
  call $hydra.lib.sets.difference
  local.set $to_visit
  local.get $to_visit
  call $hydra.lib.sets.null
  local.get $visited
  local.get $n
  local.get $v
  call $hydra.lib.sets.insert
  local.get $n
  local.get $visit
  local.get $visited
  local.get $to_visit
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.foldl
  call $hydra.lib.logic.if_else
  local.set $visit
  local.get $root
  call $hydra.lib.sets.singleton
  local.get $root
  local.get $visit
)
  (func $hydra.sorting.initial_state (result i32)
  i32.const 0
  call $hydra.lib.maps.empty
  call $hydra.lib.maps.empty
  i32.const 0
  ;; list elements follow
  call $hydra.lib.sets.empty
  i32.const 0
  ;; list elements follow
)
  (func $hydra.sorting.pop_stack_until (param $v i32) (param $st0 i32) (result i32)
  (local $acc i32)
  (local $acc' i32)
  (local $go i32)
  (local $new_st i32)
  (local $new_st2 i32)
  (local $st i32)
  (local $x i32)
  (local $xs i32)
  local.get $st
  ;; project field: stack
  call $hydra.lib.lists.head
  local.set $x
  local.get $st
  ;; project field: stack
  call $hydra.lib.lists.tail
  local.set $xs
  local.get $st
  ;; project field: counter
  local.get $st
  ;; project field: indices
  local.get $st
  ;; project field: low_links
  local.get $xs
  local.get $st
  ;; project field: on_stack
  local.get $st
  ;; project field: sccs
  local.set $new_st
  local.get $new_st
  ;; project field: counter
  local.get $new_st
  ;; project field: indices
  local.get $new_st
  ;; project field: low_links
  local.get $new_st
  ;; project field: stack
  local.get $x
  local.get $st
  ;; project field: on_stack
  call $hydra.lib.sets.delete
  local.get $new_st
  ;; project field: sccs
  local.set $new_st2
  local.get $x
  local.get $acc
  call $hydra.lib.lists.cons
  local.set $acc'
  local.get $x
  local.get $v
  call $hydra.lib.equality.equal
  local.get $acc'
  call $hydra.lib.lists.reverse
  local.get $new_st2
  local.get $acc'
  local.get $new_st2
  local.get $go
  call $hydra.lib.logic.if_else
  local.set $go
  i32.const 0
  ;; list elements follow
  local.get $st0
  local.get $go
)
  (func $hydra.sorting.propagate_tags (param $edges i32) (param $node_tags i32) (result i32)
  (local $adj_map i32)
  (local $all_nodes i32)
  (local $get_tags_for_node i32)
  (local $n i32)
  (local $node i32)
  (local $reachable i32)
  (local $tag_map i32)
  local.get $edges
  call $hydra.sorting.adjacency_list_to_map
  local.set $adj_map
  call $hydra.lib.sets.from_list
  local.get $node_tags
  call $hydra.sorting.adjacency_list_to_map
  call $hydra.lib.maps.map
  local.set $tag_map
  call $hydra.lib.pairs.first
  local.get $edges
  call $hydra.lib.lists.map
  call $hydra.lib.pairs.first
  local.get $node_tags
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat2
  call $hydra.lib.sets.from_list
  call $hydra.lib.sets.to_list
  local.set $all_nodes
  i32.const 0
  ;; list elements follow
  call $hydra.lib.equality.identity
  local.get $n
  local.get $adj_map
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  call $hydra.lib.sets.from_list
  local.get $node
  call $hydra.sorting.find_reachable_nodes
  local.set $reachable
  call $hydra.lib.sets.empty
  call $hydra.lib.equality.identity
  local.get $n
  local.get $tag_map
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  local.get $reachable
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.sets.unions
  local.set $get_tags_for_node
  local.get $n
  local.get $n
  local.get $get_tags_for_node
  local.get $all_nodes
  call $hydra.lib.lists.map
)
  (func $hydra.sorting.strong_connect (param $graph i32) (param $v i32) (param $st i32) (result i32)
  (local $comp i32)
  (local $comp_result i32)
  (local $i i32)
  (local $idx_v i32)
  (local $idx_w i32)
  (local $low_link i32)
  (local $low_v i32)
  (local $low_v1 i32)
  (local $low_v2 i32)
  (local $low_w i32)
  (local $neighbors i32)
  (local $new_st i32)
  (local $process_neighbor i32)
  (local $s i32)
  (local $st_ i32)
  (local $st_after i32)
  (local $st_after_neighbors i32)
  (local $st_popped i32)
  (local $w i32)
  local.get $st
  ;; project field: counter
  local.set $i
  local.get $i
  i32.const 1
  call $hydra.lib.math.add
  local.get $v
  local.get $i
  local.get $st
  ;; project field: indices
  call $hydra.lib.maps.insert
  local.get $v
  local.get $i
  local.get $st
  ;; project field: low_links
  call $hydra.lib.maps.insert
  local.get $v
  local.get $st
  ;; project field: stack
  call $hydra.lib.lists.cons
  local.get $v
  local.get $st
  ;; project field: on_stack
  call $hydra.lib.sets.insert
  local.get $st
  ;; project field: sccs
  local.set $new_st
  i32.const 0
  ;; list elements follow
  local.get $v
  local.get $graph
  call $hydra.lib.maps.find_with_default
  local.set $neighbors
  call $hydra.constants.max_int32
  local.get $v
  local.get $s
  ;; project field: low_links
  call $hydra.lib.maps.find_with_default
  local.set $low_v1
  call $hydra.constants.max_int32
  local.get $w
  local.get $s
  ;; project field: indices
  call $hydra.lib.maps.find_with_default
  local.set $idx_w
  local.get $s
  ;; project field: counter
  local.get $s
  ;; project field: indices
  local.get $v
  local.get $low_v1
  local.get $idx_w
  call $hydra.lib.equality.min
  local.get $s
  ;; project field: low_links
  call $hydra.lib.maps.insert
  local.get $s
  ;; project field: stack
  local.get $s
  ;; project field: on_stack
  local.get $s
  ;; project field: sccs
  local.set $low_link
  local.get $w
  local.get $st_
  ;; project field: indices
  call $hydra.lib.maps.member
  call $hydra.lib.logic.not
  local.get $graph
  local.get $w
  local.get $st_
  call $hydra.sorting.strong_connect
  local.set $st_after
  call $hydra.constants.max_int32
  local.get $v
  local.get $st_after
  ;; project field: low_links
  call $hydra.lib.maps.find_with_default
  local.set $low_v2
  call $hydra.constants.max_int32
  local.get $w
  local.get $st_after
  ;; project field: low_links
  call $hydra.lib.maps.find_with_default
  local.set $low_w
  local.get $st_after
  ;; project field: counter
  local.get $st_after
  ;; project field: indices
  local.get $v
  local.get $low_v2
  local.get $low_w
  call $hydra.lib.equality.min
  local.get $st_after
  ;; project field: low_links
  call $hydra.lib.maps.insert
  local.get $st_after
  ;; project field: stack
  local.get $st_after
  ;; project field: on_stack
  local.get $st_after
  ;; project field: sccs
  local.get $w
  local.get $st_
  ;; project field: on_stack
  call $hydra.lib.sets.member
  local.get $st_
  local.get $low_link
  local.get $st_
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  local.set $process_neighbor
  local.get $process_neighbor
  local.get $new_st
  local.get $neighbors
  call $hydra.lib.lists.foldl
  local.set $st_after_neighbors
  call $hydra.constants.max_int32
  local.get $v
  local.get $st_after_neighbors
  ;; project field: low_links
  call $hydra.lib.maps.find_with_default
  local.set $low_v
  call $hydra.constants.max_int32
  local.get $v
  local.get $st_after_neighbors
  ;; project field: indices
  call $hydra.lib.maps.find_with_default
  local.set $idx_v
  local.get $low_v
  local.get $idx_v
  call $hydra.lib.equality.equal
  local.get $v
  local.get $st_after_neighbors
  call $hydra.sorting.pop_stack_until
  local.set $comp_result
  local.get $comp_result
  call $hydra.lib.pairs.first
  local.set $comp
  local.get $comp_result
  call $hydra.lib.pairs.second
  local.set $st_popped
  local.get $st_popped
  ;; project field: counter
  local.get $st_popped
  ;; project field: indices
  local.get $st_popped
  ;; project field: low_links
  local.get $st_popped
  ;; project field: stack
  local.get $st_popped
  ;; project field: on_stack
  local.get $comp
  local.get $st_popped
  ;; project field: sccs
  call $hydra.lib.lists.cons
  local.get $st_after_neighbors
  call $hydra.lib.logic.if_else
)
  (func $hydra.sorting.strongly_connected_components (param $graph i32) (result i32)
  (local $final_state i32)
  (local $st i32)
  (local $v i32)
  (local $verts i32)
  local.get $graph
  call $hydra.lib.maps.keys
  local.set $verts
  local.get $v
  local.get $st
  ;; project field: indices
  call $hydra.lib.maps.member
  local.get $st
  local.get $graph
  local.get $v
  local.get $st
  call $hydra.sorting.strong_connect
  call $hydra.lib.logic.if_else
  call $hydra.sorting.initial_state
  local.get $verts
  call $hydra.lib.lists.foldl
  local.set $final_state
  call $hydra.lib.lists.sort
  local.get $final_state
  ;; project field: sccs
  call $hydra.lib.lists.map
  call $hydra.lib.lists.reverse
)
  (func $hydra.sorting.topological_sort (param $pairs i32) (result i32)
  (local $is_cycle i32)
  (local $scc i32)
  (local $sccs i32)
  (local $with_cycles i32)
  local.get $pairs
  call $hydra.sorting.topological_sort_components
  local.set $sccs
  local.get $scc
  call $hydra.lib.lists.tail
  call $hydra.lib.lists.null
  call $hydra.lib.logic.not
  local.set $is_cycle
  local.get $is_cycle
  local.get $sccs
  call $hydra.lib.lists.filter
  local.set $with_cycles
  local.get $with_cycles
  call $hydra.lib.lists.null
  i32.const 1
  local.get $sccs
  call $hydra.lib.lists.concat
  i32.const 0
  local.get $with_cycles
  call $hydra.lib.logic.if_else
)
  (func $hydra.sorting.topological_sort_components (param $pairs i32) (result i32)
  (local $comp i32)
  (local $g i32)
  (local $graph_result i32)
  local.get $pairs
  call $hydra.sorting.adjacency_lists_to_graph
  local.set $graph_result
  local.get $graph_result
  call $hydra.lib.pairs.first
  local.set $g
  local.get $graph_result
  call $hydra.lib.pairs.second
  local.get $comp
  call $hydra.lib.lists.map
  local.get $g
  call $hydra.sorting.strongly_connected_components
  call $hydra.lib.lists.map
)
  (func $hydra.sorting.topological_sort_nodes (param $get_key i32) (param $get_adj i32) (param $nodes i32) (result i32)
  (local $c i32)
  (local $comps i32)
  (local $k i32)
  (local $n i32)
  (local $nodes_by_key i32)
  (local $pairs i32)
  local.get $n
  local.get $get_key
  local.get $n
  local.get $nodes
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $nodes_by_key
  local.get $n
  local.get $get_key
  local.get $n
  local.get $get_adj
  local.get $nodes
  call $hydra.lib.lists.map
  local.set $pairs
  local.get $pairs
  call $hydra.sorting.topological_sort_components
  local.set $comps
  local.get $k
  local.get $nodes_by_key
  call $hydra.lib.maps.lookup
  local.get $c
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  local.get $comps
  call $hydra.lib.lists.map
)
)
