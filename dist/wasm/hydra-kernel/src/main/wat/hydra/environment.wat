(module
  (import "hydra.decode.core" "hydra.decode.core.type" (func $hydra.decode.core.type (param i32) (param i32) (result i32) ) )
  (import "hydra.decode.core" "hydra.decode.core.type_scheme" (func $hydra.decode.core.type_scheme (param i32) (param i32) (result i32) ) )
  (import "hydra.encode.core" "hydra.encode.core.type" (func $hydra.encode.core.type (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.graph_to_bindings" (func $hydra.lexical.graph_to_bindings (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bimap" (func $hydra.lib.eithers.bimap (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat" (func $hydra.lib.lists.concat (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.filter" (func $hydra.lib.lists.filter (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cat" (func $hydra.lib.maybes.cat (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.extend_graph_for_lambda" (func $hydra.scoping.extend_graph_for_lambda (param i32) (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.extend_graph_for_let" (func $hydra.scoping.extend_graph_for_let (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.extend_graph_for_type_lambda" (func $hydra.scoping.extend_graph_for_type_lambda (param i32) (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.f_type_to_type_scheme" (func $hydra.scoping.f_type_to_type_scheme (param i32) (result i32) ) )
  (import "hydra.sorting" "hydra.sorting.topological_sort_nodes" (func $hydra.sorting.topological_sort_nodes (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_term" (func $hydra.strip.deannotate_term (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_type" (func $hydra.strip.deannotate_type (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.free_variables_in_term" (func $hydra.variables.free_variables_in_term (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\0f\00\00\00\68\79\64\72\61\2e\63\6f\72\65\2e\4e\61\6d\65\0f\00\00\00\68\79\64\72\61\2e\63\6f\72\65\2e\54\79\70\65\15\00\00\00\68\79\64\72\61\2e\63\6f\72\65\2e\54\79\70\65\53\63\68\65\6d\65\0d\00\00\00\74\79\70\65\64\20\62\69\6e\64\69\6e\67\0f\00\00\00\75\6e\74\79\70\65\64\20\62\69\6e\64\69\6e\67")
  (global $__bump_ptr (mut i32) i32.const 1136 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.environment.definition_as_type_application_term" (func $hydra.environment.definition_as_type_application_term) )
  (export "hydra.environment.graph_as_let" (func $hydra.environment.graph_as_let) )
  (export "hydra.environment.graph_as_term" (func $hydra.environment.graph_as_term) )
  (export "hydra.environment.graph_as_types" (func $hydra.environment.graph_as_types) )
  (export "hydra.environment.partition_definitions" (func $hydra.environment.partition_definitions) )
  (export "hydra.environment.reorder_defs" (func $hydra.environment.reorder_defs) )
  (export "hydra.environment.schema_graph_to_typing_environment" (func $hydra.environment.schema_graph_to_typing_environment) )
  (export "hydra.environment.term_as_bindings" (func $hydra.environment.term_as_bindings) )
  (export "hydra.environment.types_to_definitions" (func $hydra.environment.types_to_definitions) )
  (export "hydra.environment.with_lambda_context" (func $hydra.environment.with_lambda_context) )
  (export "hydra.environment.with_let_context" (func $hydra.environment.with_let_context) )
  (export "hydra.environment.with_type_lambda_context" (func $hydra.environment.with_type_lambda_context) )
  (func $hydra.environment.definition_as_type_application_term (param $el i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $ts i32)
  i32.const 0
  i32.const 4
  i32.const 6
  i32.const 1087
  i32.const 1104
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
  i32.const 1
  local.get $el
  i32.load offset=4
  local.get $ts
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
  local.get $el
  i32.load offset=8
  call $hydra.lib.maybes.maybe
)
  (func $hydra.environment.graph_as_let (param $bindings i32) (param $body i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  local.get $bindings
  local.get $body
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
  (func $hydra.environment.graph_as_term (param $bindings i32) (param $body i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 6
  local.get $bindings
  local.get $body
  call $hydra.environment.graph_as_let
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
  (func $hydra.environment.graph_as_types (param $graph i32) (param $els i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $el i32)
  (local $to_pair i32)
  (local $typ i32)
  local.get $el
  i32.load
  local.get $typ
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
  local.get $el
  i32.load offset=4
  call $hydra.decode.core.type
  call $hydra.lib.eithers.map
  local.set $to_pair
  i32.const 0
  local.get $to_pair
  local.get $els
  call $hydra.lib.eithers.map_list
  call $hydra.lib.eithers.map
)
  (func $hydra.environment.partition_definitions (param $defs i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $def i32)
  (local $get_term i32)
  (local $get_type i32)
  (local $td i32)
  (local $v i32)
  (block $end_definition (result i32)
  (block $term
  (block $type
  local.get $def
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $type $term $term
)
  local.get $v
  drop
  local.get $td
  br $end_definition
)
  local.get $v
  drop
  i32.const 0
  br $end_definition
)
  local.set $get_type
  (block $end_definition (result i32)
  (block $term
  (block $type
  local.get $def
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $type $term $term
)
  local.get $v
  drop
  i32.const 0
  br $end_definition
)
  local.get $v
  drop
  local.get $td
  br $end_definition
)
  local.set $get_term
  local.get $get_type
  local.get $defs
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  local.get $get_term
  local.get $defs
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
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
  (func $hydra.environment.reorder_defs (param $defs i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $d i32)
  (local $name_first i32)
  (local $name_rest i32)
  (local $partitioned i32)
  (local $sorted_term_defs i32)
  (local $td i32)
  (local $term_defs_wrapped i32)
  (local $type_defs i32)
  (local $type_defs_raw i32)
  (local $v i32)
  local.get $defs
  call $hydra.environment.partition_definitions
  local.set $partitioned
  local.get $partitioned
  call $hydra.lib.pairs.first
  local.set $type_defs_raw
  local.get $td
  i32.load
  i32.const 1024
  call $hydra.lib.equality.equal
  local.get $type_defs_raw
  call $hydra.lib.lists.filter
  local.set $name_first
  local.get $td
  i32.load
  i32.const 1024
  call $hydra.lib.equality.equal
  call $hydra.lib.logic.not
  local.get $type_defs_raw
  call $hydra.lib.lists.filter
  local.set $name_rest
  i32.const 1
  local.get $td
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
  local.get $name_first
  call $hydra.lib.lists.map
  i32.const 1
  local.get $td
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
  local.get $name_rest
  call $hydra.lib.lists.map
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 2
  i32.store
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
  local.set $type_defs
  i32.const 0
  local.get $td
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
  local.get $partitioned
  call $hydra.lib.pairs.second
  call $hydra.lib.lists.map
  local.set $term_defs_wrapped
  (block $end_definition (result i32)
  (block $term
  local.get $d
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $term $term
)
  local.get $v
  drop
  local.get $td
  i32.load
  br $end_definition
)
  (block $end_definition (result i32)
  (block $term
  local.get $d
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $term $term
)
  local.get $v
  drop
  local.get $td
  i32.load offset=4
  call $hydra.variables.free_variables_in_term
  call $hydra.lib.sets.to_list
  br $end_definition
)
  local.get $term_defs_wrapped
  call $hydra.sorting.topological_sort_nodes
  call $hydra.lib.lists.concat
  local.set $sorted_term_defs
  local.get $type_defs
  local.get $sorted_term_defs
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 2
  i32.store
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
)
  (func $hydra.environment.schema_graph_to_typing_environment (param $g i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $_a i32)
  (local $_e i32)
  (local $decode_type i32)
  (local $decode_type_scheme i32)
  (local $decoded i32)
  (local $el i32)
  (local $for_term i32)
  (local $ft i32)
  (local $i i32)
  (local $mpairs i32)
  (local $mts i32)
  (local $r i32)
  (local $term i32)
  (local $to_pair i32)
  (local $to_type_scheme i32)
  (local $ts i32)
  (local $typ i32)
  (local $v i32)
  (local $vars i32)
  (block $end_type (result i32)
  (block $forall
  local.get $typ
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
  local.get $ft
  i32.load
  local.get $vars
  call $hydra.lib.lists.cons
  drop
  local.get $ft
  i32.load offset=4
  drop
  local.get $to_type_scheme
  drop
  i32.const 0
  br $end_type
)
  local.set $to_type_scheme
  i32.const 1
  local.get $_e
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
  local.get $_a
  local.get $g
  local.get $term
  call $hydra.decode.core.type
  call $hydra.lib.eithers.bimap
  local.set $decode_type
  i32.const 1
  local.get $_e
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
  local.get $_a
  local.get $g
  local.get $term
  call $hydra.decode.core.type_scheme
  call $hydra.lib.eithers.bimap
  local.set $decode_type_scheme
  (block $end_term (result i32)
  (block $inject
  (block $record
  local.get $term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $inject $inject
)
  local.get $v
  drop
  local.get $r
  i32.load
  i32.const 1062
  call $hydra.lib.equality.equal
  i32.const 0
  local.get $el
  i32.load offset=4
  drop
  local.get $decode_type_scheme
  drop
  i32.const 0
  call $hydra.lib.eithers.map
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
  br $end_term
)
  local.get $v
  drop
  local.get $i
  i32.load
  i32.const 1043
  call $hydra.lib.equality.equal
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  drop
  local.get $decoded
  drop
  local.get $to_type_scheme
  drop
  i32.const 0
  local.get $el
  i32.load offset=4
  drop
  local.get $decode_type
  drop
  i32.const 0
  call $hydra.lib.eithers.map
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
  br $end_term
)
  local.set $for_term
  local.get $typ
  call $hydra.scoping.f_type_to_type_scheme
  local.get $el
  i32.load offset=4
  drop
  local.get $decode_type
  drop
  i32.const 0
  call $hydra.lib.eithers.map
  local.get $ts
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  i32.const 14
  i32.const 1062
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
  call $hydra.lib.equality.equal
  i32.const 0
  local.get $el
  i32.load offset=4
  drop
  local.get $decode_type_scheme
  drop
  i32.const 0
  call $hydra.lib.eithers.map
  local.get $ts
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  i32.const 14
  i32.const 1043
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
  call $hydra.lib.equality.equal
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  drop
  local.get $decoded
  drop
  local.get $to_type_scheme
  drop
  i32.const 0
  local.get $el
  i32.load offset=4
  drop
  local.get $decode_type
  drop
  i32.const 0
  call $hydra.lib.eithers.map
  local.get $el
  i32.load offset=4
  call $hydra.strip.deannotate_term
  drop
  local.get $for_term
  drop
  i32.const 0
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  local.get $el
  i32.load offset=8
  call $hydra.lib.maybes.maybe
  i32.const 1
  local.get $el
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
  local.get $mts
  call $hydra.lib.maybes.map
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
  local.set $to_pair
  local.get $mpairs
  call $hydra.lib.maybes.cat
  call $hydra.lib.maps.from_list
  local.get $to_pair
  local.get $g
  call $hydra.lexical.graph_to_bindings
  call $hydra.lib.eithers.map_list
  call $hydra.lib.eithers.map
)
  (func $hydra.environment.term_as_bindings (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $lt i32)
  (local $v i32)
  (block $end_term (result i32)
  (block $let
  local.get $term
  call $hydra.strip.deannotate_term
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
  br $end_term
)
)
  (func $hydra.environment.types_to_definitions (param $type_map i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $name i32)
  (local $pair i32)
  (local $to_element i32)
  local.get $pair
  call $hydra.lib.pairs.first
  local.set $name
  local.get $name
  local.get $pair
  call $hydra.lib.pairs.second
  call $hydra.encode.core.type
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
  local.set $to_element
  local.get $to_element
  local.get $type_map
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.map
)
  (func $hydra.environment.with_lambda_context (param $get_context i32) (param $set_context i32) (param $env i32) (param $lam i32) (param $body i32) (result i32)
  (local $new_context i32)
  local.get $env
  drop
  local.get $get_context
  drop
  i32.const 0
  local.get $lam
  call $hydra.scoping.extend_graph_for_lambda
  local.set $new_context
  local.get $new_context
  drop
  local.get $env
  drop
  local.get $set_context
  drop
  i32.const 0
  drop
  local.get $body
  drop
  i32.const 0
)
  (func $hydra.environment.with_let_context (param $get_context i32) (param $set_context i32) (param $for_binding i32) (param $env i32) (param $letrec i32) (param $body i32) (result i32)
  (local $new_context i32)
  local.get $for_binding
  local.get $env
  drop
  local.get $get_context
  drop
  i32.const 0
  local.get $letrec
  call $hydra.scoping.extend_graph_for_let
  local.set $new_context
  local.get $new_context
  drop
  local.get $env
  drop
  local.get $set_context
  drop
  i32.const 0
  drop
  local.get $body
  drop
  i32.const 0
)
  (func $hydra.environment.with_type_lambda_context (param $get_context i32) (param $set_context i32) (param $env i32) (param $tlam i32) (param $body i32) (result i32)
  (local $new_context i32)
  local.get $env
  drop
  local.get $get_context
  drop
  i32.const 0
  local.get $tlam
  call $hydra.scoping.extend_graph_for_type_lambda
  local.set $new_context
  local.get $new_context
  drop
  local.get $env
  drop
  local.get $set_context
  drop
  i32.const 0
  drop
  local.get $body
  drop
  i32.const 0
)
)
