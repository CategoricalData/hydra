(module
  (import "hydra.decode.core" "hydra.decode.core.type" (func $hydra.decode.core.type (param i32) (result i32) ) )
  (import "hydra.decode.core" "hydra.decode.core.type_scheme" (func $hydra.decode.core.type_scheme (param i32) (result i32) ) )
  (import "hydra.encode.core" "hydra.encode.core.type" (func $hydra.encode.core.type (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.graph_to_bindings" (func $hydra.lexical.graph_to_bindings (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bimap" (func $hydra.lib.eithers.bimap (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat" (func $hydra.lib.lists.concat (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.filter" (func $hydra.lib.lists.filter (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cat" (func $hydra.lib.maybes.cat (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.pure" (func $hydra.lib.maybes.pure (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.extend_graph_for_lambda" (func $hydra.scoping.extend_graph_for_lambda (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.extend_graph_for_let" (func $hydra.scoping.extend_graph_for_let (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.extend_graph_for_type_lambda" (func $hydra.scoping.extend_graph_for_type_lambda (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.f_type_to_type_scheme" (func $hydra.scoping.f_type_to_type_scheme (param i32) (result i32) ) )
  (import "hydra.sorting" "hydra.sorting.topological_sort_nodes" (func $hydra.sorting.topological_sort_nodes (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_term" (func $hydra.strip.deannotate_term (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_type" (func $hydra.strip.deannotate_type (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.free_variables_in_term" (func $hydra.variables.free_variables_in_term (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
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
  (func $hydra.environment.definition_as_type_application_term (param $cx i32) (param $el i32) (result i32)
  (local $ts i32)
  i32.const 0
  i32.const 0 ;; string: "missing element type"
  local.get $cx
  i32.const 1
  local.get $el
  ;; project field: term
  local.get $ts
  ;; project field: type
  local.get $el
  ;; project field: type
  call $hydra.lib.maybes.maybe
)
  (func $hydra.environment.graph_as_let (param $bindings i32) (param $body i32) (result i32)
  local.get $bindings
  local.get $body
)
  (func $hydra.environment.graph_as_term (param $bindings i32) (param $body i32) (result i32)
  local.get $bindings
  local.get $body
  call $hydra.environment.graph_as_let
)
  (func $hydra.environment.graph_as_types (param $cx i32) (param $graph i32) (param $els i32) (result i32)
  (local $_wc_a i32)
  (local $_wc_e i32)
  (local $el i32)
  (local $to_pair i32)
  (local $typ i32)
  local.get $el
  ;; project field: name
  local.get $typ
  local.get $_wc_e
  local.get $cx
  local.get $_wc_a
  local.get $graph
  local.get $el
  ;; project field: term
  call $hydra.decode.core.type
  call $hydra.lib.eithers.bimap
  call $hydra.lib.eithers.map
  local.set $to_pair
  call $hydra.lib.maps.from_list
  local.get $to_pair
  local.get $els
  call $hydra.lib.eithers.map_list
  call $hydra.lib.eithers.map
)
  (func $hydra.environment.partition_definitions (param $defs i32) (result i32)
  (local $def i32)
  (local $get_term i32)
  (local $get_type i32)
  (local $td i32)
  (block $end_definition (result i32)
  (block $term
  (block $type
  local.get $def
  br_table $type $term $term
)
  local.get $td
  br $end_definition
)
  i32.const 0
  br $end_definition
)
  local.set $get_type
  (block $end_definition (result i32)
  (block $term
  (block $type
  local.get $def
  br_table $type $term $term
)
  i32.const 0
  br $end_definition
)
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
)
  (func $hydra.environment.reorder_defs (param $defs i32) (result i32)
  (local $d i32)
  (local $name_first i32)
  (local $name_rest i32)
  (local $partitioned i32)
  (local $sorted_term_defs i32)
  (local $td i32)
  (local $term_defs_wrapped i32)
  (local $type_defs i32)
  (local $type_defs_raw i32)
  local.get $defs
  call $hydra.environment.partition_definitions
  local.set $partitioned
  local.get $partitioned
  call $hydra.lib.pairs.first
  local.set $type_defs_raw
  local.get $td
  ;; project field: name
  i32.const 0 ;; string: "hydra.core.Name"
  call $hydra.lib.equality.equal
  local.get $type_defs_raw
  call $hydra.lib.lists.filter
  local.set $name_first
  local.get $td
  ;; project field: name
  i32.const 0 ;; string: "hydra.core.Name"
  call $hydra.lib.equality.equal
  call $hydra.lib.logic.not
  local.get $type_defs_raw
  call $hydra.lib.lists.filter
  local.set $name_rest
  i32.const 2
  ;; list elements follow
  local.get $td
  local.get $name_first
  call $hydra.lib.lists.map
  local.get $td
  local.get $name_rest
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  local.set $type_defs
  local.get $td
  local.get $partitioned
  call $hydra.lib.pairs.second
  call $hydra.lib.lists.map
  local.set $term_defs_wrapped
  (block $end_definition (result i32)
  (block $term
  local.get $d
  br_table $term $term
)
  local.get $td
  ;; project field: name
  br $end_definition
)
  (block $end_definition (result i32)
  (block $term
  local.get $d
  br_table $term $term
)
  local.get $td
  ;; project field: term
  call $hydra.variables.free_variables_in_term
  call $hydra.lib.sets.to_list
  br $end_definition
)
  local.get $term_defs_wrapped
  call $hydra.sorting.topological_sort_nodes
  call $hydra.lib.lists.concat
  local.set $sorted_term_defs
  i32.const 2
  ;; list elements follow
  local.get $type_defs
  local.get $sorted_term_defs
  call $hydra.lib.lists.concat
)
  (func $hydra.environment.schema_graph_to_typing_environment (param $cx i32) (param $g i32) (result i32)
  (local $_a i32)
  (local $_wc_a i32)
  (local $_wc_e i32)
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
  (local $vars i32)
  (block $end_type (result i32)
  (block $forall
  local.get $typ
  call $hydra.strip.deannotate_type
  br_table $forall $forall
)
  local.get $ft
  ;; project field: parameter
  local.get $vars
  call $hydra.lib.lists.cons
  local.get $ft
  ;; project field: body
  local.get $to_type_scheme
  br $end_type
)
  local.set $to_type_scheme
  local.get $_wc_e
  local.get $cx
  local.get $_wc_a
  nop
  local.get $_a
  local.get $g
  local.get $term
  call $hydra.decode.core.type
  call $hydra.lib.eithers.bimap
  call $hydra.lib.eithers.bimap
  local.set $decode_type
  local.get $_wc_e
  local.get $cx
  local.get $_wc_a
  nop
  local.get $_a
  local.get $g
  local.get $term
  call $hydra.decode.core.type_scheme
  call $hydra.lib.eithers.bimap
  call $hydra.lib.eithers.bimap
  local.set $decode_type_scheme
  (block $end_term (result i32)
  (block $union
  (block $record
  local.get $term
  br_table $record $union $union
)
  local.get $r
  ;; project field: type_name
  i32.const 0 ;; string: "hydra.core.TypeScheme"
  call $hydra.lib.equality.equal
  call $hydra.lib.maybes.pure
  local.get $el
  ;; project field: term
  local.get $decode_type_scheme
  call $hydra.lib.eithers.map
  i32.const 1
  i32.const 0
  call $hydra.lib.logic.if_else
  br $end_term
)
  local.get $i
  ;; project field: type_name
  i32.const 0 ;; string: "hydra.core.Type"
  call $hydra.lib.equality.equal
  i32.const 0
  ;; list elements follow
  local.get $decoded
  local.get $to_type_scheme
  local.get $el
  ;; project field: term
  local.get $decode_type
  call $hydra.lib.eithers.map
  i32.const 1
  i32.const 0
  call $hydra.lib.logic.if_else
  br $end_term
)
  local.set $for_term
  local.get $typ
  call $hydra.scoping.f_type_to_type_scheme
  local.get $el
  ;; project field: term
  local.get $decode_type
  call $hydra.lib.eithers.map
  local.get $ts
  i32.const 0
  ;; list elements follow
  i32.const 0 ;; string: "hydra.core.TypeScheme"
  i32.const 0
  call $hydra.lib.equality.equal
  call $hydra.lib.maybes.pure
  local.get $el
  ;; project field: term
  local.get $decode_type_scheme
  call $hydra.lib.eithers.map
  local.get $ts
  i32.const 0
  ;; list elements follow
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 0
  call $hydra.lib.equality.equal
  i32.const 0
  ;; list elements follow
  local.get $decoded
  local.get $to_type_scheme
  local.get $el
  ;; project field: term
  local.get $decode_type
  call $hydra.lib.eithers.map
  local.get $el
  ;; project field: term
  call $hydra.strip.deannotate_term
  local.get $for_term
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  local.get $el
  ;; project field: type
  call $hydra.lib.maybes.maybe
  i32.const 1
  local.get $el
  ;; project field: name
  local.get $ts
  local.get $mts
  call $hydra.lib.maybes.map
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
  (local $lt i32)
  (block $end_term (result i32)
  (block $let
  local.get $term
  call $hydra.strip.deannotate_term
  br_table $let $let
)
  local.get $lt
  ;; project field: bindings
  br $end_term
)
)
  (func $hydra.environment.types_to_definitions (param $type_map i32) (result i32)
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
  local.set $to_element
  local.get $to_element
  local.get $type_map
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.map
)
  (func $hydra.environment.with_lambda_context (param $get_context i32) (param $set_context i32) (param $env i32) (param $lam i32) (param $body i32) (result i32)
  (local $new_context i32)
  local.get $env
  local.get $get_context
  local.get $lam
  call $hydra.scoping.extend_graph_for_lambda
  local.set $new_context
  local.get $new_context
  local.get $env
  local.get $set_context
  local.get $body
)
  (func $hydra.environment.with_let_context (param $get_context i32) (param $set_context i32) (param $for_binding i32) (param $env i32) (param $letrec i32) (param $body i32) (result i32)
  (local $new_context i32)
  local.get $for_binding
  local.get $env
  local.get $get_context
  local.get $letrec
  call $hydra.scoping.extend_graph_for_let
  local.set $new_context
  local.get $new_context
  local.get $env
  local.get $set_context
  local.get $body
)
  (func $hydra.environment.with_type_lambda_context (param $get_context i32) (param $set_context i32) (param $env i32) (param $tlam i32) (param $body i32) (result i32)
  (local $new_context i32)
  local.get $env
  local.get $get_context
  local.get $tlam
  call $hydra.scoping.extend_graph_for_type_lambda
  local.set $new_context
  local.get $new_context
  local.get $env
  local.get $set_context
  local.get $body
)
)
