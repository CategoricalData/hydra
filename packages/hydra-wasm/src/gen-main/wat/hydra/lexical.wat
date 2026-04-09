(module
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.gt" (func $hydra.lib.equality.gt (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.take" (func $hydra.lib.lists.take (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int32" (func $hydra.lib.literals.show_int32 (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.and" (func $hydra.lib.logic.and (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.empty" (func $hydra.lib.maps.empty (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.filter" (func $hydra.lib.maps.filter (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.filter_with_key" (func $hydra.lib.maps.filter_with_key (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.keys" (func $hydra.lib.maps.keys (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.map" (func $hydra.lib.maps.map (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.member" (func $hydra.lib.maps.member (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.union" (func $hydra.lib.maps.union (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.add" (func $hydra.lib.math.add (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.bind" (func $hydra.lib.maybes.bind (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cat" (func $hydra.lib.maybes.cat (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.from_just" (func $hydra.lib.maybes.from_just (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.is_just" (func $hydra.lib.maybes.is_just (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.is_nothing" (func $hydra.lib.maybes.is_nothing (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.empty" (func $hydra.lib.sets.empty (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.member" (func $hydra.lib.sets.member (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.intercalate" (func $hydra.lib.strings.intercalate (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.term" (func $hydra.show.core.term (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_and_detype_term" (func $hydra.strip.deannotate_and_detype_term (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_term" (func $hydra.strip.deannotate_term (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_type" (func $hydra.strip.deannotate_type (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
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
  (local $b i32)
  (local $element_terms i32)
  (local $element_types i32)
  (local $filtered_terms i32)
  (local $filtered_types i32)
  (local $k i32)
  (local $let_terms i32)
  (local $merged_terms i32)
  (local $mt i32)
  (local $ts i32)
  local.get $b
  ;; project field: name
  local.get $b
  ;; project field: term
  local.get $elements
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $element_terms
  local.get $mt
  call $hydra.lib.maybes.from_just
  local.get $mt
  call $hydra.lib.maybes.is_just
  local.get $environment
  call $hydra.lib.maps.filter
  call $hydra.lib.maps.map
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
  ;; project field: name
  local.get $ts
  local.get $b
  ;; project field: type
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
  call $hydra.lib.maps.empty
  local.get $mt
  call $hydra.lib.maybes.is_nothing
  local.get $environment
  call $hydra.lib.maps.filter
  call $hydra.lib.maps.keys
  call $hydra.lib.sets.from_list
  call $hydra.lib.maps.empty
  local.get $primitives
  call $hydra.lib.maps.empty
  call $hydra.lib.sets.empty
)
  (func $hydra.lexical.choose_unique_name (param $reserved i32) (param $name i32) (result i32)
  (local $candidate i32)
  (local $index i32)
  (local $try_name i32)
  local.get $index
  i32.const 1
  call $hydra.lib.equality.equal
  local.get $name
  nop
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
  local.get $try_name
  local.get $candidate
  call $hydra.lib.logic.if_else
  local.set $try_name
  i32.const 1
  local.get $try_name
)
  (func $hydra.lexical.dereference_schema_type (param $name i32) (param $types i32) (result i32)
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
  br_table $annotated $forall $variable $variable
)
  local.get $at
  ;; project field: body
  local.get $for_type
  br $end_type
)
  local.get $ft
  ;; project field: parameter
  local.get $ts
  ;; project field: variables
  call $hydra.lib.lists.cons
  local.get $ts
  ;; project field: type
  local.get $ts
  ;; project field: constraints
  local.get $ft
  ;; project field: body
  local.get $for_type
  call $hydra.lib.maybes.map
  br $end_type
)
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
  ;; project field: variables
  local.get $ts2
  ;; project field: variables
  call $hydra.lib.lists.concat2
  local.get $ts2
  ;; project field: type
  local.get $ts2
  ;; project field: constraints
  local.get $ts
  ;; project field: type
  local.get $for_type
  call $hydra.lib.maybes.map
  call $hydra.lib.maybes.bind
)
  (func $hydra.lexical.dereference_variable (param $graph i32) (param $name i32) (result i32)
  (local $right_ i32)
  i32.const 0
  i32.const 0 ;; string: "no such element: "
  nop
  call $hydra.lib.strings.cat2
  i32.const 1
  local.get $right_
  local.get $graph
  local.get $name
  call $hydra.lexical.lookup_binding
  call $hydra.lib.maybes.maybe
)
  (func $hydra.lexical.elements_to_graph (param $parent i32) (param $schema_types i32) (param $elements i32) (result i32)
  (local $g i32)
  (local $prims i32)
  local.get $parent
  ;; project field: primitives
  local.set $prims
  local.get $elements
  call $hydra.lib.maps.empty
  local.get $prims
  call $hydra.lexical.build_graph
  local.set $g
  local.get $g
  ;; project field: bound_terms
  local.get $g
  ;; project field: bound_types
  local.get $g
  ;; project field: class_constraints
  local.get $g
  ;; project field: lambda_variables
  local.get $g
  ;; project field: metadata
  local.get $g
  ;; project field: primitives
  local.get $schema_types
  local.get $g
  ;; project field: type_variables
)
  (func $hydra.lexical.empty_context (result i32)
  i32.const 0
  ;; list elements follow
  i32.const 0
  ;; list elements follow
  call $hydra.lib.maps.empty
)
  (func $hydra.lexical.empty_graph (result i32)
  call $hydra.lib.maps.empty
  call $hydra.lib.maps.empty
  call $hydra.lib.maps.empty
  call $hydra.lib.sets.empty
  call $hydra.lib.maps.empty
  call $hydra.lib.maps.empty
  call $hydra.lib.maps.empty
  call $hydra.lib.sets.empty
)
  (func $hydra.lexical.fields_of (param $t i32) (result i32)
  (local $forall_type i32)
  (local $rt i32)
  (local $stripped i32)
  local.get $t
  call $hydra.strip.deannotate_type
  local.set $stripped
  (block $end_type (result i32)
  (block $union
  (block $record
  (block $forall
  local.get $stripped
  br_table $forall $record $union $union
)
  local.get $forall_type
  ;; project field: body
  call $hydra.lexical.fields_of
  br $end_type
)
  local.get $rt
  br $end_type
)
  local.get $rt
  br $end_type
)
)
  (func $hydra.lexical.get_field (param $cx i32) (param $m i32) (param $fname i32) (param $decode i32) (result i32)
  i32.const 0
  i32.const 0 ;; string: "expected field "
  nop
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: " not found"
  call $hydra.lib.strings.cat2
  local.get $cx
  local.get $decode
  local.get $fname
  local.get $m
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
)
  (func $hydra.lexical.graph_to_bindings (param $g i32) (result i32)
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
  ;; project field: bound_types
  call $hydra.lib.maps.lookup
  local.get $g
  ;; project field: bound_terms
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.map
)
  (func $hydra.lexical.graph_with_primitives (param $built_in i32) (param $user_provided i32) (result i32)
  (local $p i32)
  (local $prims i32)
  (local $ps i32)
  (local $to_map i32)
  local.get $p
  ;; project field: name
  local.get $p
  local.get $ps
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $to_map
  local.get $user_provided
  local.get $to_map
  local.get $built_in
  local.get $to_map
  call $hydra.lib.maps.union
  local.set $prims
  i32.const 0
  ;; list elements follow
  call $hydra.lib.maps.empty
  local.get $prims
  call $hydra.lexical.build_graph
)
  (func $hydra.lexical.lookup_binding (param $graph i32) (param $name i32) (result i32)
  (local $term i32)
  local.get $name
  local.get $term
  local.get $name
  local.get $graph
  ;; project field: bound_types
  call $hydra.lib.maps.lookup
  local.get $name
  local.get $graph
  ;; project field: bound_terms
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.map
)
  (func $hydra.lexical.lookup_primitive (param $graph i32) (param $name i32) (result i32)
  local.get $name
  local.get $graph
  ;; project field: primitives
  call $hydra.lib.maps.lookup
)
  (func $hydra.lexical.lookup_term (param $graph i32) (param $name i32) (result i32)
  local.get $name
  local.get $graph
  ;; project field: bound_terms
  call $hydra.lib.maps.lookup
)
  (func $hydra.lexical.match_enum (param $cx i32) (param $graph i32) (param $tname i32) (param $pairs i32) (result i32)
  (local $pair i32)
  local.get $cx
  local.get $graph
  local.get $tname
  local.get $pair
  call $hydra.lib.pairs.first
  local.get $pair
  call $hydra.lib.pairs.second
  call $hydra.lexical.match_unit_field
  local.get $pairs
  call $hydra.lib.lists.map
  call $hydra.lexical.match_union
)
  (func $hydra.lexical.match_record (param $cx i32) (param $graph i32) (param $decode i32) (param $term i32) (result i32)
  (local $field i32)
  (local $record i32)
  (local $stripped i32)
  local.get $term
  call $hydra.strip.deannotate_and_detype_term
  local.set $stripped
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $field
  ;; project field: name
  local.get $field
  ;; project field: term
  local.get $record
  ;; project field: fields
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.get $decode
  br $end_term
)
)
  (func $hydra.lexical.match_union (param $cx i32) (param $graph i32) (param $tname i32) (param $pairs i32) (param $term i32) (result i32)
  (local $el i32)
  (local $exp i32)
  (local $f i32)
  (local $fname i32)
  (local $injection i32)
  (local $mapping i32)
  (local $name i32)
  (local $stripped i32)
  (local $val i32)
  local.get $term
  call $hydra.strip.deannotate_and_detype_term
  local.set $stripped
  local.get $pairs
  call $hydra.lib.maps.from_list
  local.set $mapping
  (block $end_term (result i32)
  (block $union
  (block $variable
  local.get $stripped
  br_table $variable $union $union
)
  local.get $cx
  local.get $graph
  local.get $name
  call $hydra.lexical.require_binding
  local.get $cx
  local.get $graph
  local.get $tname
  local.get $pairs
  local.get $el
  ;; project field: term
  call $hydra.lexical.match_union
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $injection
  ;; project field: field
  ;; project field: name
  local.set $fname
  local.get $injection
  ;; project field: field
  ;; project field: term
  local.set $val
  i32.const 0
  i32.const 0 ;; string: "no matching case for field ""
  nop
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: "" in union type "
  call $hydra.lib.strings.cat2
  nop
  call $hydra.lib.strings.cat2
  local.get $cx
  local.get $val
  local.get $f
  local.get $fname
  local.get $mapping
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  local.set $exp
  nop
  nop
  call $hydra.lib.equality.equal
  local.get $exp
  i32.const 0
  i32.const 0 ;; string: "expected injection for type "
  nop
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: ", got "
  call $hydra.lib.strings.cat2
  local.get $term
  call $hydra.show.core.term
  call $hydra.lib.strings.cat2
  local.get $cx
  call $hydra.lib.logic.if_else
  br $end_term
)
)
  (func $hydra.lexical.match_unit_field (param $fname i32) (param $x i32) (result i32)
  local.get $fname
  i32.const 1
  local.get $x
)
  (func $hydra.lexical.require_binding (param $cx i32) (param $graph i32) (param $name i32) (result i32)
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
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "..."
  call $hydra.lib.lists.concat2
  local.get $strings
  call $hydra.lib.logic.if_else
  local.set $ellipsis
  i32.const 0 ;; string: "no such element: "
  nop
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: ". Available elements: {"
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: ", "
  nop
  local.get $graph
  ;; project field: bound_terms
  call $hydra.lib.maps.keys
  call $hydra.lib.lists.map
  local.get $ellipsis
  call $hydra.lib.strings.intercalate
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: "}"
  call $hydra.lib.strings.cat2
  local.set $err_msg
  i32.const 0
  local.get $err_msg
  local.get $cx
  i32.const 1
  local.get $x
  local.get $graph
  local.get $name
  call $hydra.lexical.lookup_binding
  call $hydra.lib.maybes.maybe
)
  (func $hydra.lexical.require_primitive (param $cx i32) (param $graph i32) (param $name i32) (result i32)
  (local $x i32)
  i32.const 0
  i32.const 0 ;; string: "no such primitive function: "
  nop
  call $hydra.lib.strings.cat2
  local.get $cx
  i32.const 1
  local.get $x
  local.get $graph
  local.get $name
  call $hydra.lexical.lookup_primitive
  call $hydra.lib.maybes.maybe
)
  (func $hydra.lexical.require_primitive_type (param $cx i32) (param $tx i32) (param $name i32) (result i32)
  (local $_p i32)
  (local $mts i32)
  (local $ts i32)
  local.get $_p
  ;; project field: type
  local.get $name
  local.get $tx
  ;; project field: primitives
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.map
  local.set $mts
  i32.const 0
  i32.const 0 ;; string: "no such primitive function: "
  nop
  call $hydra.lib.strings.cat2
  local.get $cx
  i32.const 1
  local.get $ts
  local.get $mts
  call $hydra.lib.maybes.maybe
)
  (func $hydra.lexical.require_term (param $cx i32) (param $graph i32) (param $name i32) (result i32)
  (local $x i32)
  i32.const 0
  i32.const 0 ;; string: "no such element: "
  nop
  call $hydra.lib.strings.cat2
  local.get $cx
  i32.const 1
  local.get $x
  local.get $graph
  local.get $name
  call $hydra.lexical.resolve_term
  call $hydra.lib.maybes.maybe
)
  (func $hydra.lexical.resolve_term (param $graph i32) (param $name i32) (result i32)
  (local $name' i32)
  (local $recurse i32)
  (local $stripped i32)
  (local $term i32)
  local.get $term
  call $hydra.strip.deannotate_term
  local.set $stripped
  (block $end_term (result i32)
  (block $variable
  local.get $stripped
  br_table $variable $variable
)
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
  (func $hydra.lexical.strip_and_dereference_term (param $cx i32) (param $graph i32) (param $term i32) (result i32)
  (local $stripped i32)
  (local $t i32)
  (local $v i32)
  local.get $term
  call $hydra.strip.deannotate_and_detype_term
  local.set $stripped
  (block $end_term (result i32)
  (block $variable
  local.get $stripped
  br_table $variable $variable
)
  local.get $cx
  local.get $graph
  local.get $v
  call $hydra.lexical.require_term
  local.get $cx
  local.get $graph
  local.get $t
  call $hydra.lexical.strip_and_dereference_term
  call $hydra.lib.eithers.bind
  br $end_term
)
)
  (func $hydra.lexical.strip_and_dereference_term_either (param $graph i32) (param $term i32) (result i32)
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
  br_table $variable $variable
)
  i32.const 0
  local.get $left_
  local.get $graph
  local.get $binding
  ;; project field: term
  call $hydra.lexical.strip_and_dereference_term_either
  local.get $graph
  local.get $v
  call $hydra.lexical.dereference_variable
  call $hydra.lib.eithers.either
  br $end_term
)
)
)
