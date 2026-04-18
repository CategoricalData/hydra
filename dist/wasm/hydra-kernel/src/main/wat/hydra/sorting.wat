(module
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.gt" (func $hydra.lib.equality.gt (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.min" (func $hydra.lib.equality.min (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat" (func $hydra.lib.lists.concat (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.filter" (func $hydra.lib.lists.filter (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.reverse" (func $hydra.lib.lists.reverse (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.sort_on" (func $hydra.lib.lists.sort_on (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.uncons" (func $hydra.lib.lists.uncons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.zip" (func $hydra.lib.lists.zip (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.find_with_default" (func $hydra.lib.maps.find_with_default (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.insert" (func $hydra.lib.maps.insert (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.keys" (func $hydra.lib.maps.keys (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.map" (func $hydra.lib.maps.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.member" (func $hydra.lib.maps.member (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.add" (func $hydra.lib.math.add (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.range" (func $hydra.lib.math.range (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cat" (func $hydra.lib.maybes.cat (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map_maybe" (func $hydra.lib.maybes.map_maybe (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.delete" (func $hydra.lib.sets.delete (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.difference" (func $hydra.lib.sets.difference (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.insert" (func $hydra.lib.sets.insert (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.member" (func $hydra.lib.sets.member (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.null" (func $hydra.lib.sets.null (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.singleton" (func $hydra.lib.sets.singleton (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.unions" (func $hydra.lib.sets.unions (param i32) (result i32) ) )
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
  (local $__rec_ptr i32)
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
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  i32.const 0
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
  i32.const 0
  local.get $pairs
  call $hydra.lib.lists.foldl
)
  (func $hydra.sorting.adjacency_lists_to_graph (param $edges0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
  i32.const 0
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
  local.get $indexed_edges
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $graph
  local.get $v
  local.get $vertex_map
  call $hydra.lib.maps.lookup
  local.set $vertex_to_key
  local.get $graph
  local.get $vertex_to_key
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
  (func $hydra.sorting.create_ordering_isomorphism (param $source_ord i32) (param $target_ord i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
  (func $hydra.sorting.find_reachable_nodes (param $adj i32) (param $root i32) (result i32)
  (local $n i32)
  (local $node i32)
  (local $to_visit i32)
  (local $v i32)
  (local $visit i32)
  (local $visited i32)
  local.get $node
  drop
  local.get $adj
  drop
  i32.const 0
  local.get $visited
  call $hydra.lib.sets.difference
  local.set $to_visit
  local.get $to_visit
  call $hydra.lib.sets.null
  local.get $visited
  local.get $n
  local.get $v
  call $hydra.lib.sets.insert
  drop
  local.get $n
  drop
  local.get $visit
  drop
  i32.const 0
  local.get $visited
  local.get $to_visit
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.foldl
  call $hydra.lib.logic.if_else
  local.set $visit
  local.get $root
  call $hydra.lib.sets.singleton
  drop
  local.get $root
  drop
  local.get $visit
  drop
  i32.const 0
)
  (func $hydra.sorting.initial_state (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  i32.const 0
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  i32.const 24
  call $__alloc
  local.set $__rec_ptr
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
  (func $hydra.sorting.pop_stack_until (param $v i32) (param $st0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $acc i32)
  (local $acc' i32)
  (local $go i32)
  (local $new_st i32)
  (local $new_st2 i32)
  (local $st i32)
  (local $uc i32)
  (local $x i32)
  (local $xs i32)
  local.get $acc
  call $hydra.lib.lists.reverse
  local.get $st
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
  local.get $uc
  call $hydra.lib.pairs.first
  local.set $x
  local.get $uc
  call $hydra.lib.pairs.second
  local.set $xs
  local.get $st
  i32.load
  local.get $st
  i32.load offset=4
  local.get $st
  i32.load offset=8
  local.get $xs
  local.get $st
  i32.load offset=16
  local.get $st
  i32.load offset=20
  i32.const 24
  call $__alloc
  local.set $__rec_ptr
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
  local.set $new_st
  local.get $new_st
  i32.load
  local.get $new_st
  i32.load offset=4
  local.get $new_st
  i32.load offset=8
  local.get $new_st
  i32.load offset=12
  local.get $x
  local.get $st
  i32.load offset=16
  call $hydra.lib.sets.delete
  local.get $new_st
  i32.load offset=20
  i32.const 24
  call $__alloc
  local.set $__rec_ptr
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
  local.get $acc'
  drop
  local.get $new_st2
  drop
  local.get $go
  drop
  i32.const 0
  call $hydra.lib.logic.if_else
  local.get $st
  i32.load offset=12
  call $hydra.lib.lists.uncons
  call $hydra.lib.maybes.maybe
  local.set $go
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  drop
  local.get $st0
  drop
  local.get $go
  drop
  i32.const 0
)
  (func $hydra.sorting.propagate_tags (param $edges i32) (param $node_tags i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
  i32.const 0
  local.get $node_tags
  call $hydra.sorting.adjacency_list_to_map
  call $hydra.lib.maps.map
  local.set $tag_map
  i32.const 0
  local.get $edges
  call $hydra.lib.lists.map
  i32.const 0
  local.get $node_tags
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat2
  call $hydra.lib.sets.from_list
  call $hydra.lib.sets.to_list
  local.set $all_nodes
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  i32.const 0
  local.get $n
  local.get $adj_map
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  call $hydra.lib.sets.from_list
  local.get $node
  call $hydra.sorting.find_reachable_nodes
  local.set $reachable
  i32.const 0
  i32.const 0
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
  drop
  local.get $get_tags_for_node
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
  local.get $all_nodes
  call $hydra.lib.lists.map
)
  (func $hydra.sorting.strong_connect (param $graph i32) (param $v i32) (param $st i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
  i32.load
  local.set $i
  local.get $i
  i32.const 1
  call $hydra.lib.math.add
  local.get $v
  local.get $i
  local.get $st
  i32.load offset=4
  call $hydra.lib.maps.insert
  local.get $v
  local.get $i
  local.get $st
  i32.load offset=8
  call $hydra.lib.maps.insert
  local.get $v
  local.get $st
  i32.load offset=12
  call $hydra.lib.lists.cons
  local.get $v
  local.get $st
  i32.load offset=16
  call $hydra.lib.sets.insert
  local.get $st
  i32.load offset=20
  i32.const 24
  call $__alloc
  local.set $__rec_ptr
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
  local.set $new_st
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $v
  local.get $graph
  call $hydra.lib.maps.find_with_default
  local.set $neighbors
  i32.const 0
  local.get $v
  local.get $s
  i32.load offset=8
  call $hydra.lib.maps.find_with_default
  local.set $low_v1
  i32.const 0
  local.get $w
  local.get $s
  i32.load offset=4
  call $hydra.lib.maps.find_with_default
  local.set $idx_w
  local.get $s
  i32.load
  local.get $s
  i32.load offset=4
  local.get $v
  local.get $low_v1
  local.get $idx_w
  call $hydra.lib.equality.min
  local.get $s
  i32.load offset=8
  call $hydra.lib.maps.insert
  local.get $s
  i32.load offset=12
  local.get $s
  i32.load offset=16
  local.get $s
  i32.load offset=20
  i32.const 24
  call $__alloc
  local.set $__rec_ptr
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
  local.set $low_link
  local.get $w
  local.get $st_
  i32.load offset=4
  call $hydra.lib.maps.member
  call $hydra.lib.logic.not
  local.get $graph
  local.get $w
  local.get $st_
  call $hydra.sorting.strong_connect
  local.set $st_after
  i32.const 0
  local.get $v
  local.get $st_after
  i32.load offset=8
  call $hydra.lib.maps.find_with_default
  local.set $low_v2
  i32.const 0
  local.get $w
  local.get $st_after
  i32.load offset=8
  call $hydra.lib.maps.find_with_default
  local.set $low_w
  local.get $st_after
  i32.load
  local.get $st_after
  i32.load offset=4
  local.get $v
  local.get $low_v2
  local.get $low_w
  call $hydra.lib.equality.min
  local.get $st_after
  i32.load offset=8
  call $hydra.lib.maps.insert
  local.get $st_after
  i32.load offset=12
  local.get $st_after
  i32.load offset=16
  local.get $st_after
  i32.load offset=20
  i32.const 24
  call $__alloc
  local.set $__rec_ptr
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
  local.get $w
  local.get $st_
  i32.load offset=16
  call $hydra.lib.sets.member
  local.get $st_
  drop
  local.get $low_link
  drop
  i32.const 0
  local.get $st_
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  local.set $process_neighbor
  local.get $process_neighbor
  local.get $new_st
  local.get $neighbors
  call $hydra.lib.lists.foldl
  local.set $st_after_neighbors
  i32.const 0
  local.get $v
  local.get $st_after_neighbors
  i32.load offset=8
  call $hydra.lib.maps.find_with_default
  local.set $low_v
  i32.const 0
  local.get $v
  local.get $st_after_neighbors
  i32.load offset=4
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
  i32.load
  local.get $st_popped
  i32.load offset=4
  local.get $st_popped
  i32.load offset=8
  local.get $st_popped
  i32.load offset=12
  local.get $st_popped
  i32.load offset=16
  local.get $comp
  local.get $st_popped
  i32.load offset=20
  call $hydra.lib.lists.cons
  i32.const 24
  call $__alloc
  local.set $__rec_ptr
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
  i32.load offset=4
  call $hydra.lib.maps.member
  local.get $st
  local.get $graph
  local.get $v
  local.get $st
  call $hydra.sorting.strong_connect
  call $hydra.lib.logic.if_else
  i32.const 0
  local.get $verts
  call $hydra.lib.lists.foldl
  local.set $final_state
  i32.const 0
  local.get $final_state
  i32.load offset=20
  call $hydra.lib.lists.map
  call $hydra.lib.lists.reverse
)
  (func $hydra.sorting.topological_sort (param $pairs i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $is_cycle i32)
  (local $scc i32)
  (local $sccs i32)
  (local $with_cycles i32)
  local.get $pairs
  call $hydra.sorting.topological_sort_components
  local.set $sccs
  local.get $scc
  call $hydra.lib.lists.length
  i32.const 1
  call $hydra.lib.equality.gt
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
  local.get $with_cycles
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
  call $hydra.lib.maybes.map_maybe
  local.get $g
  call $hydra.sorting.strongly_connected_components
  call $hydra.lib.lists.map
)
  (func $hydra.sorting.topological_sort_nodes (param $get_key i32) (param $get_adj i32) (param $nodes i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $c i32)
  (local $comps i32)
  (local $k i32)
  (local $n i32)
  (local $nodes_by_key i32)
  (local $pairs i32)
  local.get $n
  drop
  local.get $get_key
  drop
  i32.const 0
  local.get $n
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
  local.get $nodes
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $nodes_by_key
  local.get $n
  drop
  local.get $get_key
  drop
  i32.const 0
  local.get $n
  drop
  local.get $get_adj
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
