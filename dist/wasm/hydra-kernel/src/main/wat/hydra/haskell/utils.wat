(module
  (import "hydra.analysis" "hydra.analysis.module_dependency_namespaces" (func $hydra.analysis.module_dependency_namespaces (param i32) (param i32) (param i32) (param i32) (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.formatting" "hydra.formatting.capitalize" (func $hydra.formatting.capitalize (param i32) (result i32) ) )
  (import "hydra.formatting" "hydra.formatting.decapitalize" (func $hydra.formatting.decapitalize (param i32) (result i32) ) )
  (import "hydra.formatting" "hydra.formatting.sanitize_with_underscores" (func $hydra.formatting.sanitize_with_underscores (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.gt" (func $hydra.lib.equality.gt (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.lt" (func $hydra.lib.equality.lt (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.drop" (func $hydra.lib.lists.drop (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.maybe_last" (func $hydra.lib.lists.maybe_last (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.replicate" (func $hydra.lib.lists.replicate (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.reverse" (func $hydra.lib.lists.reverse (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.uncons" (func $hydra.lib.lists.uncons (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.and" (func $hydra.lib.logic.and (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.or" (func $hydra.lib.logic.or (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.insert" (func $hydra.lib.maps.insert (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.add" (func $hydra.lib.math.add (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.sub" (func $hydra.lib.math.sub (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cases" (func $hydra.lib.maybes.cases (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.from_maybe" (func $hydra.lib.maybes.from_maybe (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.member" (func $hydra.lib.sets.member (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.split_on" (func $hydra.lib.strings.split_on (param i32) (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.local_name_of" (func $hydra.names.local_name_of (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.qualify_name" (func $hydra.names.qualify_name (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.unqualify_name" (func $hydra.names.unqualify_name (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_type" (func $hydra.strip.deannotate_type (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\00\00\00\00\01\00\00\00\2e\01\00\00\00\5f\02\00\00\00\75\6e")
  (global $__bump_ptr (mut i32) i32.const 1056 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.haskell.utils.application_pattern" (func $hydra.haskell.utils.application_pattern) )
  (export "hydra.haskell.utils.element_reference" (func $hydra.haskell.utils.element_reference) )
  (export "hydra.haskell.utils.hsapp" (func $hydra.haskell.utils.hsapp) )
  (export "hydra.haskell.utils.hslambda" (func $hydra.haskell.utils.hslambda) )
  (export "hydra.haskell.utils.hslit" (func $hydra.haskell.utils.hslit) )
  (export "hydra.haskell.utils.hsvar" (func $hydra.haskell.utils.hsvar) )
  (export "hydra.haskell.utils.namespaces_for_module" (func $hydra.haskell.utils.namespaces_for_module) )
  (export "hydra.haskell.utils.newtype_accessor_name" (func $hydra.haskell.utils.newtype_accessor_name) )
  (export "hydra.haskell.utils.raw_name" (func $hydra.haskell.utils.raw_name) )
  (export "hydra.haskell.utils.record_field_reference" (func $hydra.haskell.utils.record_field_reference) )
  (export "hydra.haskell.utils.sanitize_haskell_name" (func $hydra.haskell.utils.sanitize_haskell_name) )
  (export "hydra.haskell.utils.simple_name" (func $hydra.haskell.utils.simple_name) )
  (export "hydra.haskell.utils.simple_value_binding" (func $hydra.haskell.utils.simple_value_binding) )
  (export "hydra.haskell.utils.to_type_application" (func $hydra.haskell.utils.to_type_application) )
  (export "hydra.haskell.utils.type_name_for_record" (func $hydra.haskell.utils.type_name_for_record) )
  (export "hydra.haskell.utils.union_field_reference" (func $hydra.haskell.utils.union_field_reference) )
  (export "hydra.haskell.utils.unpack_forall_type" (func $hydra.haskell.utils.unpack_forall_type) )
  (func $hydra.haskell.utils.application_pattern (param $name i32) (param $args i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 0
  local.get $name
  local.get $args
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
)
  (func $hydra.haskell.utils.element_reference (param $namespaces i32) (param $name i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $alias_str i32)
  (local $esc_local i32)
  (local $gmod i32)
  (local $gname i32)
  (local $local i32)
  (local $mn i32)
  (local $mns i32)
  (local $namespace_pair i32)
  (local $namespaces_map i32)
  (local $ns i32)
  (local $qname i32)
  local.get $namespaces
  i32.load
  local.set $namespace_pair
  local.get $namespace_pair
  call $hydra.lib.pairs.first
  local.set $gname
  local.get $namespace_pair
  call $hydra.lib.pairs.second
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $gmod
  local.get $namespaces
  i32.load offset=4
  local.set $namespaces_map
  local.get $name
  call $hydra.names.qualify_name
  local.set $qname
  local.get $qname
  i32.load offset=4
  local.set $local
  local.get $local
  call $hydra.haskell.utils.sanitize_haskell_name
  local.set $esc_local
  local.get $qname
  i32.load
  local.set $mns
  local.get $qname
  i32.load
  local.get $local
  call $hydra.haskell.utils.simple_name
  local.get $ns
  local.get $namespaces_map
  call $hydra.lib.maps.lookup
  local.get $local
  call $hydra.haskell.utils.simple_name
  local.get $mn
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $alias_str
  local.get $ns
  local.get $gname
  call $hydra.lib.equality.equal
  local.get $esc_local
  call $hydra.haskell.utils.simple_name
  local.get $alias_str
  i32.const 1028
  local.get $local
  call $hydra.haskell.utils.sanitize_haskell_name
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
  call $hydra.lib.strings.cat
  call $hydra.haskell.utils.raw_name
  call $hydra.lib.logic.if_else
  call $hydra.lib.maybes.cases
  call $hydra.lib.maybes.cases
)
  (func $hydra.haskell.utils.hsapp (param $l i32) (param $r i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 0
  local.get $l
  local.get $r
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
)
  (func $hydra.haskell.utils.hslambda (param $name i32) (param $rhs i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 7
  i32.const 4
  local.get $name
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
  local.get $rhs
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
)
  (func $hydra.haskell.utils.hslit (param $lit i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 6
  local.get $lit
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
  (func $hydra.haskell.utils.hsvar (param $s i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 17
  local.get $s
  call $hydra.haskell.utils.raw_name
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
  (func $hydra.haskell.utils.namespaces_for_module (param $mod i32) (param $cx i32) (param $g i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $a i32)
  (local $alias_counts i32)
  (local $alias_entries i32)
  (local $alias_from_suffix i32)
  (local $alias_min_segs i32)
  (local $alias_min_segs_count i32)
  (local $alias_str i32)
  (local $b i32)
  (local $can_grow i32)
  (local $capitalized_suffix i32)
  (local $count i32)
  (local $drop_count i32)
  (local $e i32)
  (local $existing i32)
  (local $final_state i32)
  (local $focus_pair i32)
  (local $grow_step i32)
  (local $initial_state i32)
  (local $k i32)
  (local $m i32)
  (local $max_segs i32)
  (local $min_segs i32)
  (local $min_segs_count i32)
  (local $n i32)
  (local $namespace i32)
  (local $new_n i32)
  (local $nm i32)
  (local $ns i32)
  (local $nss i32)
  (local $nss_as_list i32)
  (local $prev i32)
  (local $result_map i32)
  (local $seg_count i32)
  (local $segments_of i32)
  (local $segs i32)
  (local $segs_for i32)
  (local $segs_map i32)
  (local $state i32)
  (local $suffix i32)
  (local $taken_for i32)
  (local $to_module_name i32)
  local.get $cx
  local.get $g
  i32.const 1
  i32.const 1
  i32.const 1
  i32.const 1
  local.get $mod
  call $hydra.analysis.module_dependency_namespaces
  local.get $mod
  i32.load
  local.set $ns
  i32.const 1028
  local.get $namespace
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.split_on
  local.set $segments_of
  local.get $segs
  call $hydra.lib.lists.length
  local.get $n
  call $hydra.lib.math.sub
  local.set $drop_count
  local.get $drop_count
  local.get $segs
  call $hydra.lib.lists.drop
  local.set $suffix
  i32.const 0
  local.get $suffix
  call $hydra.lib.lists.map
  local.set $capitalized_suffix
  local.get $capitalized_suffix
  call $hydra.lib.strings.cat
  local.set $alias_from_suffix
  local.get $namespace
  drop
  local.get $segments_of
  drop
  i32.const 0
  drop
  i32.const 1
  drop
  local.get $alias_from_suffix
  drop
  i32.const 0
  local.set $to_module_name
  local.get $ns
  local.get $ns
  drop
  local.get $to_module_name
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
  local.set $focus_pair
  local.get $nss
  call $hydra.lib.sets.to_list
  local.set $nss_as_list
  local.get $nm
  local.get $nm
  drop
  local.get $segments_of
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
  local.get $nss_as_list
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $segs_map
  local.get $a
  local.get $b
  call $hydra.lib.equality.gt
  local.get $a
  local.get $b
  call $hydra.lib.logic.if_else
  i32.const 1
  local.get $nm
  drop
  local.get $segments_of
  drop
  i32.const 0
  call $hydra.lib.lists.length
  local.get $nss_as_list
  call $hydra.lib.lists.map
  call $hydra.lib.lists.foldl
  local.set $max_segs
  local.get $nm
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
  local.get $nss_as_list
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $initial_state
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $nm
  local.get $segs_map
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.from_maybe
  local.set $segs_for
  i32.const 1
  local.get $nm
  local.get $state
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.from_maybe
  local.set $taken_for
  local.get $nm
  drop
  local.get $segs_for
  drop
  i32.const 0
  local.set $segs
  local.get $state
  drop
  local.get $nm
  drop
  local.get $taken_for
  drop
  i32.const 0
  local.set $n
  local.get $segs
  call $hydra.lib.lists.length
  local.set $seg_count
  local.get $segs
  drop
  local.get $n
  drop
  local.get $alias_from_suffix
  drop
  i32.const 0
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $alias_str
  local.get $nm
  local.get $n
  local.get $seg_count
  local.get $alias_str
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
  local.get $nss_as_list
  call $hydra.lib.lists.map
  local.set $alias_entries
  local.get $e
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  local.set $k
  local.get $k
  i32.const 1
  i32.const 0
  local.get $k
  local.get $m
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.from_maybe
  call $hydra.lib.math.add
  local.get $m
  call $hydra.lib.maps.insert
  i32.const 0
  local.get $alias_entries
  call $hydra.lib.lists.foldl
  local.set $alias_counts
  local.get $e
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.set $seg_count
  local.get $e
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  local.set $k
  local.get $k
  local.get $m
  call $hydra.lib.maps.lookup
  local.set $existing
  local.get $k
  local.get $existing
  local.get $seg_count
  local.get $seg_count
  local.get $prev
  call $hydra.lib.equality.lt
  local.get $seg_count
  local.get $prev
  call $hydra.lib.logic.if_else
  call $hydra.lib.maybes.cases
  local.get $m
  call $hydra.lib.maps.insert
  i32.const 0
  local.get $alias_entries
  call $hydra.lib.lists.foldl
  local.set $alias_min_segs
  local.get $e
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.set $seg_count
  local.get $e
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  local.set $k
  local.get $seg_count
  local.get $k
  local.get $alias_min_segs
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.from_maybe
  local.set $min_segs
  local.get $seg_count
  local.get $min_segs
  call $hydra.lib.equality.equal
  local.get $k
  i32.const 1
  i32.const 0
  local.get $k
  local.get $m
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.from_maybe
  call $hydra.lib.math.add
  local.get $m
  call $hydra.lib.maps.insert
  local.get $m
  call $hydra.lib.logic.if_else
  i32.const 0
  local.get $alias_entries
  call $hydra.lib.lists.foldl
  local.set $alias_min_segs_count
  local.get $e
  call $hydra.lib.pairs.first
  local.set $nm
  local.get $e
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.set $n
  local.get $e
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.set $seg_count
  local.get $e
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  local.set $alias_str
  i32.const 0
  local.get $alias_str
  local.get $alias_counts
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.from_maybe
  local.set $count
  local.get $seg_count
  local.get $alias_str
  local.get $alias_min_segs
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.from_maybe
  local.set $min_segs
  i32.const 0
  local.get $alias_str
  local.get $alias_min_segs_count
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.from_maybe
  local.set $min_segs_count
  local.get $count
  i32.const 1
  call $hydra.lib.equality.gt
  local.get $seg_count
  local.get $n
  call $hydra.lib.equality.gt
  local.get $seg_count
  local.get $min_segs
  call $hydra.lib.equality.gt
  local.get $min_segs_count
  i32.const 1
  call $hydra.lib.equality.gt
  call $hydra.lib.logic.or
  call $hydra.lib.logic.and
  call $hydra.lib.logic.and
  local.set $can_grow
  local.get $can_grow
  local.get $n
  i32.const 1
  call $hydra.lib.math.add
  local.get $n
  call $hydra.lib.logic.if_else
  local.set $new_n
  local.get $nm
  local.get $new_n
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
  local.get $alias_entries
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $grow_step
  local.get $grow_step
  local.get $initial_state
  local.get $max_segs
  i32.const 0
  call $hydra.lib.lists.replicate
  call $hydra.lib.lists.foldl
  local.set $final_state
  local.get $nm
  local.get $nm
  drop
  local.get $segs_for
  drop
  i32.const 0
  drop
  local.get $final_state
  drop
  local.get $nm
  drop
  local.get $taken_for
  drop
  i32.const 0
  drop
  local.get $alias_from_suffix
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
  local.get $nss_as_list
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $result_map
  i32.const 1
  local.get $focus_pair
  local.get $result_map
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
  call $hydra.lib.eithers.bind
)
  (func $hydra.haskell.utils.newtype_accessor_name (param $name i32) (result i32)
  i32.const 1038
  local.get $name
  call $hydra.names.local_name_of
  call $hydra.lib.strings.cat2
)
  (func $hydra.haskell.utils.raw_name (param $n i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
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
  (func $hydra.haskell.utils.record_field_reference (param $namespaces i32) (param $sname i32) (param $fname i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $capitalized i32)
  (local $decapitalized i32)
  (local $fname_str i32)
  (local $nm i32)
  (local $ns i32)
  (local $qname i32)
  (local $qual_name i32)
  (local $type_name_str i32)
  (local $unqual_name i32)
  local.get $fname
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $fname_str
  local.get $sname
  call $hydra.names.qualify_name
  local.set $qname
  local.get $qname
  i32.load
  local.set $ns
  local.get $sname
  call $hydra.haskell.utils.type_name_for_record
  local.set $type_name_str
  local.get $type_name_str
  call $hydra.formatting.decapitalize
  local.set $decapitalized
  local.get $fname_str
  call $hydra.formatting.capitalize
  local.set $capitalized
  local.get $decapitalized
  local.get $capitalized
  call $hydra.lib.strings.cat2
  local.set $nm
  local.get $ns
  local.get $nm
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
  local.set $qual_name
  local.get $qual_name
  call $hydra.names.unqualify_name
  local.set $unqual_name
  local.get $namespaces
  local.get $unqual_name
  call $hydra.haskell.utils.element_reference
)
  (func $hydra.haskell.utils.sanitize_haskell_name (result i32)
  i32.const 0
  i32.const 0
  call $hydra.formatting.sanitize_with_underscores
)
  (func $hydra.haskell.utils.simple_name (param $arg_ i32) (result i32)
  local.get $arg_
  call $hydra.haskell.utils.sanitize_haskell_name
  call $hydra.haskell.utils.raw_name
)
  (func $hydra.haskell.utils.simple_value_binding (param $hname i32) (param $rhs i32) (param $bindings i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $pat i32)
  (local $right_hand_side i32)
  i32.const 0
  local.get $hname
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
  local.set $pat
  local.get $rhs
  local.set $right_hand_side
  i32.const 0
  local.get $pat
  local.get $right_hand_side
  local.get $bindings
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
)
  (func $hydra.haskell.utils.to_type_application (param $types i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $app i32)
  (local $dummy_type i32)
  (local $l i32)
  (local $p i32)
  i32.const 7
  i32.const 1
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
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
  local.set $dummy_type
  local.get $dummy_type
  local.get $p
  call $hydra.lib.pairs.second
  call $hydra.lib.lists.null
  local.get $p
  call $hydra.lib.pairs.first
  i32.const 0
  local.get $p
  call $hydra.lib.pairs.second
  drop
  local.get $app
  drop
  i32.const 0
  local.get $p
  call $hydra.lib.pairs.first
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
  local.get $l
  call $hydra.lib.lists.uncons
  call $hydra.lib.maybes.map
  call $hydra.lib.maybes.from_maybe
  local.set $app
  local.get $types
  call $hydra.lib.lists.reverse
  drop
  local.get $app
  drop
  i32.const 0
)
  (func $hydra.haskell.utils.type_name_for_record (param $sname i32) (result i32)
  (local $parts i32)
  (local $sname_str i32)
  local.get $sname
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $sname_str
  i32.const 1028
  local.get $sname_str
  call $hydra.lib.strings.split_on
  local.set $parts
  local.get $sname_str
  local.get $parts
  call $hydra.lib.lists.maybe_last
  call $hydra.lib.maybes.from_maybe
)
  (func $hydra.haskell.utils.union_field_reference (param $bound_names i32) (param $namespaces i32) (param $sname i32) (param $fname i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $capitalized_field_name i32)
  (local $capitalized_type_name i32)
  (local $deconflict i32)
  (local $fname_str i32)
  (local $name i32)
  (local $nm i32)
  (local $ns i32)
  (local $qname i32)
  (local $qual_name i32)
  (local $tname i32)
  (local $type_name_str i32)
  (local $unqual_name i32)
  local.get $fname
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $fname_str
  local.get $sname
  call $hydra.names.qualify_name
  local.set $qname
  local.get $qname
  i32.load
  local.set $ns
  local.get $sname
  call $hydra.haskell.utils.type_name_for_record
  local.set $type_name_str
  local.get $type_name_str
  call $hydra.formatting.capitalize
  local.set $capitalized_type_name
  local.get $fname_str
  call $hydra.formatting.capitalize
  local.set $capitalized_field_name
  local.get $ns
  local.get $name
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
  call $hydra.names.unqualify_name
  local.set $tname
  local.get $tname
  local.get $bound_names
  call $hydra.lib.sets.member
  local.get $name
  i32.const 1033
  call $hydra.lib.strings.cat2
  drop
  local.get $deconflict
  drop
  i32.const 0
  local.get $name
  call $hydra.lib.logic.if_else
  local.set $deconflict
  local.get $capitalized_type_name
  local.get $capitalized_field_name
  call $hydra.lib.strings.cat2
  drop
  local.get $deconflict
  drop
  i32.const 0
  local.set $nm
  local.get $ns
  local.get $nm
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
  local.set $qual_name
  local.get $qual_name
  call $hydra.names.unqualify_name
  local.set $unqual_name
  local.get $namespaces
  local.get $unqual_name
  call $hydra.haskell.utils.element_reference
)
  (func $hydra.haskell.utils.unpack_forall_type (param $t i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $fat i32)
  (local $final_type i32)
  (local $recursive_result i32)
  (local $tbody i32)
  (local $v i32)
  (local $vars i32)
  (block $end_type (result i32)
  (block $forall
  local.get $t
  call $hydra.strip.deannotate_type
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $forall $forall
)
  local.get $v
  drop
  local.get $fat
  i32.load
  local.set $v
  local.get $fat
  i32.load offset=4
  local.set $tbody
  local.get $tbody
  call $hydra.haskell.utils.unpack_forall_type
  local.set $recursive_result
  local.get $recursive_result
  call $hydra.lib.pairs.first
  local.set $vars
  local.get $recursive_result
  call $hydra.lib.pairs.second
  local.set $final_type
  local.get $v
  local.get $vars
  call $hydra.lib.lists.cons
  local.get $final_type
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
)
)
