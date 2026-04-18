(module
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.gt" (func $hydra.lib.equality.gt (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.take" (func $hydra.lib.lists.take (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int32" (func $hydra.lib.literals.show_int32 (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.and" (func $hydra.lib.logic.and (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.filter" (func $hydra.lib.maps.filter (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.filter_with_key" (func $hydra.lib.maps.filter_with_key (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.keys" (func $hydra.lib.maps.keys (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.member" (func $hydra.lib.maps.member (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.union" (func $hydra.lib.maps.union (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.add" (func $hydra.lib.math.add (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.bind" (func $hydra.lib.maybes.bind (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cat" (func $hydra.lib.maybes.cat (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.is_nothing" (func $hydra.lib.maybes.is_nothing (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map_maybe" (func $hydra.lib.maybes.map_maybe (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.member" (func $hydra.lib.sets.member (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.intercalate" (func $hydra.lib.strings.intercalate (param i32) (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.term" (func $hydra.show.core.term (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_and_detype_term" (func $hydra.strip.deannotate_and_detype_term (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_term" (func $hydra.strip.deannotate_term (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_type" (func $hydra.strip.deannotate_type (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\02\00\00\00\2c\20\17\00\00\00\2e\20\41\76\61\69\6c\61\62\6c\65\20\65\6c\65\6d\65\6e\74\73\3a\20\7b\03\00\00\00\2e\2e\2e\13\00\00\00\69\6e\6a\65\63\74\69\6f\6e\20\66\6f\72\20\74\79\70\65\20\11\00\00\00\6e\6f\20\73\75\63\68\20\65\6c\65\6d\65\6e\74\3a\20\06\00\00\00\72\65\63\6f\72\64\01\00\00\00\7d")
  (global $__bump_ptr (mut i32) i32.const 1136 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.lexical.build_graph" (func $hydra.lexical.build_graph) )
  (export "hydra.lexical.choose_unique_name" (func $hydra.lexical.choose_unique_name) )
  (export "hydra.lexical.dereference_schema_type" (func $hydra.lexical.dereference_schema_type) )
  (export "hydra.lexical.dereference_variable" (func $hydra.lexical.dereference_variable) )
  (export "hydra.lexical.elements_to_graph" (func $hydra.lexical.elements_to_graph) )
  (export "hydra.lexical.empty_context" (func $hydra.lexical.empty_context) )
  (export "hydra.lexical.empty_graph" (func $hydra.lexical.empty_graph) )
  (export "hydra.lexical.fields_of" (func $hydra.lexical.fields_of) )
  (export "hydra.lexical.get_field" (func $hydra.lexical.get_field) )
  (export "hydra.lexical.graph_to_bindings" (func $hydra.lexical.graph_to_bindings) )
  (export "hydra.lexical.graph_with_primitives" (func $hydra.lexical.graph_with_primitives) )
  (export "hydra.lexical.lookup_binding" (func $hydra.lexical.lookup_binding) )
  (export "hydra.lexical.lookup_primitive" (func $hydra.lexical.lookup_primitive) )
  (export "hydra.lexical.lookup_term" (func $hydra.lexical.lookup_term) )
  (export "hydra.lexical.match_enum" (func $hydra.lexical.match_enum) )
  (export "hydra.lexical.match_record" (func $hydra.lexical.match_record) )
  (export "hydra.lexical.match_union" (func $hydra.lexical.match_union) )
  (export "hydra.lexical.match_unit_field" (func $hydra.lexical.match_unit_field) )
  (export "hydra.lexical.require_binding" (func $hydra.lexical.require_binding) )
  (export "hydra.lexical.require_primitive" (func $hydra.lexical.require_primitive) )
  (export "hydra.lexical.require_primitive_type" (func $hydra.lexical.require_primitive_type) )
  (export "hydra.lexical.require_term" (func $hydra.lexical.require_term) )
  (export "hydra.lexical.resolve_term" (func $hydra.lexical.resolve_term) )
  (export "hydra.lexical.strip_and_dereference_term" (func $hydra.lexical.strip_and_dereference_term) )
  (export "hydra.lexical.strip_and_dereference_term_either" (func $hydra.lexical.strip_and_dereference_term_either) )
  (func $hydra.lexical.build_graph (param $elements i32) (param $environment i32) (param $primitives i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (local $element_terms i32)
  (local $element_types i32)
  (local $filtered_terms i32)
  (local $filtered_types i32)
  (local $k i32)
  (local $kv i32)
  (local $let_terms i32)
  (local $merged_terms i32)
  (local $mt i32)
  (local $t i32)
  (local $ts i32)
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
  local.get $elements
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $element_terms
  local.get $kv
  call $hydra.lib.pairs.first
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
  local.get $kv
  call $hydra.lib.pairs.second
  call $hydra.lib.maybes.map
  local.get $environment
  call $hydra.lib.maps.to_list
  call $hydra.lib.maybes.map_maybe
  call $hydra.lib.maps.from_list
  local.set $let_terms
  local.get $element_terms
  local.get $let_terms
  call $hydra.lib.maps.union
  local.set $merged_terms
  local.get $k
  local.get $primitives
  call $hydra.lib.maps.member
  call $hydra.lib.logic.not
  local.get $merged_terms
  call $hydra.lib.maps.filter_with_key
  local.set $filtered_terms
  local.get $b
  i32.load
  local.get $ts
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
  i32.load offset=8
  call $hydra.lib.maybes.map
  local.get $elements
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  call $hydra.lib.maps.from_list
  local.set $element_types
  local.get $k
  local.get $primitives
  call $hydra.lib.maps.member
  call $hydra.lib.logic.not
  local.get $element_types
  call $hydra.lib.maps.filter_with_key
  local.set $filtered_types
  local.get $filtered_terms
  local.get $filtered_types
  i32.const 0
  local.get $mt
  call $hydra.lib.maybes.is_nothing
  local.get $environment
  call $hydra.lib.maps.filter
  call $hydra.lib.maps.keys
  call $hydra.lib.sets.from_list
  i32.const 0
  local.get $primitives
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
)
  (func $hydra.lexical.choose_unique_name (param $reserved i32) (param $name i32) (result i32)
  (local $candidate i32)
  (local $index i32)
  (local $try_name i32)
  local.get $index
  i32.const 1
  call $hydra.lib.equality.equal
  local.get $name
  local.get $name
  drop
  i32.const 0
  drop
  i32.const 0
  local.get $index
  call $hydra.lib.literals.show_int32
  call $hydra.lib.strings.cat2
  call $hydra.lib.logic.if_else
  local.set $candidate
  local.get $candidate
  local.get $reserved
  call $hydra.lib.sets.member
  local.get $index
  i32.const 1
  call $hydra.lib.math.add
  drop
  local.get $try_name
  drop
  i32.const 0
  local.get $candidate
  call $hydra.lib.logic.if_else
  local.set $try_name
  i32.const 1
  drop
  local.get $try_name
  drop
  i32.const 0
)
  (func $hydra.lexical.dereference_schema_type (param $name i32) (param $types i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $at i32)
  (local $for_type i32)
  (local $ft i32)
  (local $t i32)
  (local $ts i32)
  (local $ts2 i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $variable
  (block $forall
  (block $annotated
  local.get $t
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $forall $variable $variable
)
  local.get $v
  drop
  local.get $at
  i32.load
  drop
  local.get $for_type
  drop
  i32.const 0
  br $end_type
)
  local.get $v
  drop
  local.get $ft
  i32.load
  local.get $ts
  i32.load
  call $hydra.lib.lists.cons
  local.get $ts
  i32.load offset=4
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
  local.get $ft
  i32.load offset=4
  drop
  local.get $for_type
  drop
  i32.const 0
  call $hydra.lib.maybes.map
  br $end_type
)
  local.get $v
  drop
  local.get $v
  local.get $types
  call $hydra.lexical.dereference_schema_type
  br $end_type
)
  local.set $for_type
  local.get $name
  local.get $types
  call $hydra.lib.maps.lookup
  local.get $ts
  i32.load
  local.get $ts2
  i32.load
  call $hydra.lib.lists.concat2
  local.get $ts2
  i32.load offset=4
  local.get $ts2
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
  local.get $ts
  i32.load offset=4
  drop
  local.get $for_type
  drop
  i32.const 0
  call $hydra.lib.maybes.map
  call $hydra.lib.maybes.bind
)
  (func $hydra.lexical.dereference_variable (param $graph i32) (param $name i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $right_ i32)
  i32.const 0
  i32.const 7
  i32.const 0
  local.get $name
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
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
  local.get $right_
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
  local.get $graph
  local.get $name
  call $hydra.lexical.lookup_binding
  call $hydra.lib.maybes.maybe
)
  (func $hydra.lexical.elements_to_graph (param $parent i32) (param $schema_types i32) (param $elements i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $g i32)
  (local $prims i32)
  local.get $parent
  i32.load offset=20
  local.set $prims
  local.get $elements
  i32.const 0
  local.get $prims
  call $hydra.lexical.build_graph
  local.set $g
  local.get $g
  i32.load
  local.get $g
  i32.load offset=4
  local.get $g
  i32.load offset=8
  local.get $g
  i32.load offset=12
  local.get $g
  i32.load offset=16
  local.get $g
  i32.load offset=20
  local.get $schema_types
  local.get $g
  i32.load offset=28
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
)
  (func $hydra.lexical.empty_context (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
)
  (func $hydra.lexical.empty_graph (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
)
  (func $hydra.lexical.fields_of (param $t i32) (result i32)
  (local $__rec_ptr i32)
  (local $forall_type i32)
  (local $rt i32)
  (local $stripped i32)
  (local $v i32)
  local.get $t
  call $hydra.strip.deannotate_type
  local.set $stripped
  (block $end_type (result i32)
  (block $union
  (block $record
  (block $forall
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $forall $record $union $union
)
  local.get $v
  drop
  local.get $forall_type
  i32.load offset=4
  call $hydra.lexical.fields_of
  br $end_type
)
  local.get $v
  drop
  local.get $rt
  br $end_type
)
  local.get $v
  drop
  local.get $rt
  br $end_type
)
)
  (func $hydra.lexical.get_field (param $m i32) (param $fname i32) (param $decode i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 0
  i32.const 7
  i32.const 2
  local.get $fname
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
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
  local.get $decode
  local.get $fname
  local.get $m
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
)
  (func $hydra.lexical.graph_to_bindings (param $g i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $name i32)
  (local $p i32)
  (local $term i32)
  local.get $p
  call $hydra.lib.pairs.first
  local.set $name
  local.get $p
  call $hydra.lib.pairs.second
  local.set $term
  local.get $name
  local.get $term
  local.get $name
  local.get $g
  i32.load offset=4
  call $hydra.lib.maps.lookup
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
  local.get $g
  i32.load
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.map
)
  (func $hydra.lexical.graph_with_primitives (param $built_in i32) (param $user_provided i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $p i32)
  (local $prims i32)
  (local $ps i32)
  (local $to_map i32)
  local.get $p
  i32.load
  local.get $p
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
  local.get $ps
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $to_map
  local.get $user_provided
  drop
  local.get $to_map
  drop
  i32.const 0
  local.get $built_in
  drop
  local.get $to_map
  drop
  i32.const 0
  call $hydra.lib.maps.union
  local.set $prims
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  i32.const 0
  local.get $prims
  call $hydra.lexical.build_graph
)
  (func $hydra.lexical.lookup_binding (param $graph i32) (param $name i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $term i32)
  local.get $name
  local.get $term
  local.get $name
  local.get $graph
  i32.load offset=4
  call $hydra.lib.maps.lookup
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
  local.get $name
  local.get $graph
  i32.load
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.map
)
  (func $hydra.lexical.lookup_primitive (param $graph i32) (param $name i32) (result i32)
  local.get $name
  local.get $graph
  i32.load offset=20
  call $hydra.lib.maps.lookup
)
  (func $hydra.lexical.lookup_term (param $graph i32) (param $name i32) (result i32)
  local.get $name
  local.get $graph
  i32.load
  call $hydra.lib.maps.lookup
)
  (func $hydra.lexical.match_enum (param $graph i32) (param $tname i32) (param $pairs i32) (result i32)
  (local $pair i32)
  local.get $graph
  local.get $tname
  local.get $pair
  call $hydra.lib.pairs.first
  local.get $pair
  call $hydra.lib.pairs.second
  call $hydra.lexical.match_unit_field
  local.get $pairs
  call $hydra.lib.lists.map
  i32.const 0
  call $hydra.lexical.match_union
)
  (func $hydra.lexical.match_record (param $graph i32) (param $decode i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $field i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  local.get $term
  call $hydra.strip.deannotate_and_detype_term
  local.set $stripped
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $field
  i32.load
  local.get $field
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
  local.get $record
  i32.load offset=4
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  drop
  local.get $decode
  drop
  i32.const 0
  br $end_term
)
)
  (func $hydra.lexical.match_union (param $graph i32) (param $tname i32) (param $pairs i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $el i32)
  (local $exp i32)
  (local $f i32)
  (local $fname i32)
  (local $injection i32)
  (local $mapping i32)
  (local $name i32)
  (local $stripped i32)
  (local $v i32)
  (local $val i32)
  local.get $term
  call $hydra.strip.deannotate_and_detype_term
  local.set $stripped
  local.get $pairs
  call $hydra.lib.maps.from_list
  local.set $mapping
  (block $end_term (result i32)
  (block $inject
  (block $variable
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $variable $inject $inject
)
  local.get $v
  drop
  local.get $graph
  local.get $name
  call $hydra.lexical.require_binding
  local.get $graph
  local.get $tname
  local.get $pairs
  local.get $el
  i32.load offset=4
  call $hydra.lexical.match_union
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $v
  drop
  local.get $injection
  i32.load offset=4
  i32.load
  local.set $fname
  local.get $injection
  i32.load offset=4
  i32.load offset=4
  local.set $val
  i32.const 0
  i32.const 7
  i32.const 2
  local.get $fname
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
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
  local.get $val
  drop
  local.get $f
  drop
  i32.const 0
  local.get $fname
  local.get $mapping
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  local.set $exp
  local.get $injection
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  local.get $tname
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.equality.equal
  local.get $exp
  i32.const 0
  i32.const 7
  i32.const 4
  i32.const 1064
  local.get $tname
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.cat2
  local.get $term
  call $hydra.show.core.term
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
)
  (func $hydra.lexical.match_unit_field (param $fname i32) (param $x i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  local.get $fname
  i32.const 1
  local.get $x
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
  (func $hydra.lexical.require_binding (param $graph i32) (param $name i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $ellipsis i32)
  (local $err_msg i32)
  (local $show_all i32)
  (local $strings i32)
  (local $x i32)
  i32.const 0
  local.set $show_all
  local.get $strings
  call $hydra.lib.lists.length
  i32.const 3
  call $hydra.lib.equality.gt
  local.get $show_all
  call $hydra.lib.logic.not
  call $hydra.lib.logic.and
  i32.const 3
  local.get $strings
  call $hydra.lib.lists.take
  i32.const 1057
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
  local.get $strings
  call $hydra.lib.logic.if_else
  local.set $ellipsis
  i32.const 1087
  local.get $name
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.cat2
  i32.const 1030
  call $hydra.lib.strings.cat2
  i32.const 1024
  i32.const 0
  local.get $graph
  i32.load
  call $hydra.lib.maps.keys
  call $hydra.lib.lists.map
  drop
  local.get $ellipsis
  drop
  i32.const 0
  call $hydra.lib.strings.intercalate
  call $hydra.lib.strings.cat2
  i32.const 1118
  call $hydra.lib.strings.cat2
  local.set $err_msg
  i32.const 0
  i32.const 7
  i32.const 3
  local.get $err_msg
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
  i32.const 1
  local.get $x
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
  local.get $graph
  local.get $name
  call $hydra.lexical.lookup_binding
  call $hydra.lib.maybes.maybe
)
  (func $hydra.lexical.require_primitive (param $graph i32) (param $name i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $x i32)
  i32.const 0
  i32.const 7
  i32.const 1
  local.get $name
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
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
  local.get $x
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
  local.get $graph
  local.get $name
  call $hydra.lexical.lookup_primitive
  call $hydra.lib.maybes.maybe
)
  (func $hydra.lexical.require_primitive_type (param $tx i32) (param $name i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $_p i32)
  (local $mts i32)
  (local $ts i32)
  local.get $_p
  i32.load offset=4
  local.get $name
  local.get $tx
  i32.load offset=20
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.map
  local.set $mts
  i32.const 0
  i32.const 7
  i32.const 1
  local.get $name
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
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
  local.get $ts
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
  local.get $mts
  call $hydra.lib.maybes.maybe
)
  (func $hydra.lexical.require_term (param $graph i32) (param $name i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $x i32)
  i32.const 0
  i32.const 7
  i32.const 0
  local.get $name
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
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
  local.get $x
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
  local.get $graph
  local.get $name
  call $hydra.lexical.resolve_term
  call $hydra.lib.maybes.maybe
)
  (func $hydra.lexical.resolve_term (param $graph i32) (param $name i32) (result i32)
  (local $__rec_ptr i32)
  (local $name' i32)
  (local $recurse i32)
  (local $stripped i32)
  (local $term i32)
  (local $v i32)
  local.get $term
  call $hydra.strip.deannotate_term
  local.set $stripped
  (block $end_term (result i32)
  (block $variable
  local.get $stripped
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
  local.get $graph
  local.get $name'
  call $hydra.lexical.resolve_term
  br $end_term
)
  local.set $recurse
  i32.const 0
  local.get $recurse
  local.get $graph
  local.get $name
  call $hydra.lexical.lookup_term
  call $hydra.lib.maybes.maybe
)
  (func $hydra.lexical.strip_and_dereference_term (param $graph i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $stripped i32)
  (local $t i32)
  (local $v i32)
  local.get $term
  call $hydra.strip.deannotate_and_detype_term
  local.set $stripped
  (block $end_term (result i32)
  (block $variable
  local.get $stripped
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
  local.get $graph
  local.get $v
  call $hydra.lexical.require_term
  local.get $graph
  local.get $t
  call $hydra.lexical.strip_and_dereference_term
  call $hydra.lib.eithers.bind
  br $end_term
)
)
  (func $hydra.lexical.strip_and_dereference_term_either (param $graph i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $binding i32)
  (local $left_ i32)
  (local $stripped i32)
  (local $v i32)
  local.get $term
  call $hydra.strip.deannotate_and_detype_term
  local.set $stripped
  (block $end_term (result i32)
  (block $variable
  local.get $stripped
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
  local.get $left_
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
  local.get $graph
  local.get $binding
  i32.load offset=4
  call $hydra.lexical.strip_and_dereference_term_either
  local.get $graph
  local.get $v
  call $hydra.lexical.dereference_variable
  call $hydra.lib.eithers.either
  br $end_term
)
)
)
