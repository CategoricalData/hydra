(module
  (import "hydra.dependencies" "hydra.dependencies.lift_lambda_above_let" (func $hydra.dependencies.lift_lambda_above_let (param i32) (result i32) ) )
  (import "hydra.environment" "hydra.environment.graph_as_types" (func $hydra.environment.graph_as_types (param i32) (result i32) ) )
  (import "hydra.environment" "hydra.environment.term_as_bindings" (func $hydra.environment.term_as_bindings (param i32) (result i32) ) )
  (import "hydra.environment" "hydra.environment.types_to_definitions" (func $hydra.environment.types_to_definitions (param i32) (result i32) ) )
  (import "hydra.hoisting" "hydra.hoisting.hoist_case_statements_in_graph" (func $hydra.hoisting.hoist_case_statements_in_graph (param i32) (result i32) ) )
  (import "hydra.hoisting" "hydra.hoisting.hoist_polymorphic_let_bindings" (func $hydra.hoisting.hoist_polymorphic_let_bindings (param i32) (result i32) ) )
  (import "hydra.inference" "hydra.inference.infer_graph_types" (func $hydra.inference.infer_graph_types (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.build_graph" (func $hydra.lexical.build_graph (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.graph_to_bindings" (func $hydra.lexical.graph_to_bindings (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bimap" (func $hydra.lib.eithers.bimap (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.identity" (func $hydra.lib.equality.identity (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat" (func $hydra.lib.lists.concat (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.filter" (func $hydra.lib.lists.filter (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.head" (func $hydra.lib.lists.head (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.pure" (func $hydra.lib.lists.pure (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.tail" (func $hydra.lib.lists.tail (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigfloat_to_float64" (func $hydra.lib.literals.bigfloat_to_float64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_int32" (func $hydra.lib.literals.bigint_to_int32 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_int64" (func $hydra.lib.literals.bigint_to_int64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_int8" (func $hydra.lib.literals.bigint_to_int8 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.binary_to_string" (func $hydra.lib.literals.binary_to_string (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.uint32_to_bigint" (func $hydra.lib.literals.uint32_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.uint64_to_bigint" (func $hydra.lib.literals.uint64_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.uint8_to_bigint" (func $hydra.lib.literals.uint8_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.empty" (func $hydra.lib.maps.empty (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.insert" (func $hydra.lib.maps.insert (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.map" (func $hydra.lib.maps.map (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.null" (func $hydra.lib.maps.null (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cat" (func $hydra.lib.maybes.cat (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.from_just" (func $hydra.lib.maybes.from_just (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.is_just" (func $hydra.lib.maybes.is_just (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.empty" (func $hydra.lib.sets.empty (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.member" (func $hydra.lib.sets.member (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.intercalate" (func $hydra.lib.strings.intercalate (param i32) (result i32) ) )
  (import "hydra.literals" "hydra.literals.bigfloat_to_float_value" (func $hydra.literals.bigfloat_to_float_value (param i32) (result i32) ) )
  (import "hydra.literals" "hydra.literals.bigint_to_integer_value" (func $hydra.literals.bigint_to_integer_value (param i32) (result i32) ) )
  (import "hydra.literals" "hydra.literals.float_value_to_bigfloat" (func $hydra.literals.float_value_to_bigfloat (param i32) (result i32) ) )
  (import "hydra.literals" "hydra.literals.integer_value_to_bigint" (func $hydra.literals.integer_value_to_bigint (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.namespace_of" (func $hydra.names.namespace_of (param i32) (result i32) ) )
  (import "hydra.reduction" "hydra.reduction.eta_expand_term" (func $hydra.reduction.eta_expand_term (param i32) (result i32) ) )
  (import "hydra.reflect" "hydra.reflect.literal_type" (func $hydra.reflect.literal_type (param i32) (result i32) ) )
  (import "hydra.reflect" "hydra.reflect.literal_type_variant" (func $hydra.reflect.literal_type_variant (param i32) (result i32) ) )
  (import "hydra.reflect" "hydra.reflect.literal_types" (func $hydra.reflect.literal_types (param i32) (result i32) ) )
  (import "hydra.reflect" "hydra.reflect.term_variant" (func $hydra.reflect.term_variant (param i32) (result i32) ) )
  (import "hydra.reflect" "hydra.reflect.type_variant" (func $hydra.reflect.type_variant (param i32) (result i32) ) )
  (import "hydra.resolution" "hydra.resolution.require_union_type" (func $hydra.resolution.require_union_type (param i32) (result i32) ) )
  (import "hydra.resolution" "hydra.resolution.type_to_type_scheme" (func $hydra.resolution.type_to_type_scheme (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_term_m" (func $hydra.rewriting.rewrite_term_m (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_type_m" (func $hydra.rewriting.rewrite_type_m (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.type_scheme_to_f_type" (func $hydra.scoping.type_scheme_to_f_type (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.literal" (func $hydra.show.core.literal (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.type" (func $hydra.show.core.type (param i32) (result i32) ) )
  (import "hydra.show.errors" "hydra.show.errors.error" (func $hydra.show.errors.error (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_type" (func $hydra.strip.deannotate_type (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.strip_type_lambdas" (func $hydra.strip.strip_type_lambdas (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.unshadow_variables" (func $hydra.variables.unshadow_variables (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.adapt.adapt_data_graph" (func $hydra.adapt.adapt_data_graph) )
  (export "hydra.adapt.adapt_float_type" (func $hydra.adapt.adapt_float_type) )
  (export "hydra.adapt.adapt_graph_schema" (func $hydra.adapt.adapt_graph_schema) )
  (export "hydra.adapt.adapt_integer_type" (func $hydra.adapt.adapt_integer_type) )
  (export "hydra.adapt.adapt_lambda_domains" (func $hydra.adapt.adapt_lambda_domains) )
  (export "hydra.adapt.adapt_literal" (func $hydra.adapt.adapt_literal) )
  (export "hydra.adapt.adapt_literal_type" (func $hydra.adapt.adapt_literal_type) )
  (export "hydra.adapt.adapt_literal_types_map" (func $hydra.adapt.adapt_literal_types_map) )
  (export "hydra.adapt.adapt_literal_value" (func $hydra.adapt.adapt_literal_value) )
  (export "hydra.adapt.adapt_nested_types" (func $hydra.adapt.adapt_nested_types) )
  (export "hydra.adapt.adapt_primitive" (func $hydra.adapt.adapt_primitive) )
  (export "hydra.adapt.adapt_term" (func $hydra.adapt.adapt_term) )
  (export "hydra.adapt.adapt_term_for_language" (func $hydra.adapt.adapt_term_for_language) )
  (export "hydra.adapt.adapt_type" (func $hydra.adapt.adapt_type) )
  (export "hydra.adapt.adapt_type_for_language" (func $hydra.adapt.adapt_type_for_language) )
  (export "hydra.adapt.adapt_type_scheme" (func $hydra.adapt.adapt_type_scheme) )
  (export "hydra.adapt.compose_coders" (func $hydra.adapt.compose_coders) )
  (export "hydra.adapt.data_graph_to_definitions" (func $hydra.adapt.data_graph_to_definitions) )
  (export "hydra.adapt.literal_type_supported" (func $hydra.adapt.literal_type_supported) )
  (export "hydra.adapt.prepare_float_type" (func $hydra.adapt.prepare_float_type) )
  (export "hydra.adapt.prepare_integer_type" (func $hydra.adapt.prepare_integer_type) )
  (export "hydra.adapt.prepare_literal_type" (func $hydra.adapt.prepare_literal_type) )
  (export "hydra.adapt.prepare_same" (func $hydra.adapt.prepare_same) )
  (export "hydra.adapt.prepare_type" (func $hydra.adapt.prepare_type) )
  (export "hydra.adapt.push_type_apps_inward" (func $hydra.adapt.push_type_apps_inward) )
  (export "hydra.adapt.schema_graph_to_definitions" (func $hydra.adapt.schema_graph_to_definitions) )
  (export "hydra.adapt.simple_language_adapter" (func $hydra.adapt.simple_language_adapter) )
  (export "hydra.adapt.term_alternatives" (func $hydra.adapt.term_alternatives) )
  (export "hydra.adapt.type_alternatives" (func $hydra.adapt.type_alternatives) )
  (func $hydra.adapt.adapt_data_graph (param $constraints i32) (param $do_expand i32) (param $els0 i32) (param $cx i32) (param $graph0 i32) (result i32)
  (local $adapt_binding i32)
  (local $adapted i32)
  (local $adapted_graph i32)
  (local $adapted_graph_raw i32)
  (local $adapted_schema_types i32)
  (local $adapted_terms i32)
  (local $adapted_type i32)
  (local $el i32)
  (local $els1 i32)
  (local $els1_raw i32)
  (local $g i32)
  (local $kv i32)
  (local $litmap i32)
  (local $new_term i32)
  (local $prim1 i32)
  (local $prim_pairs i32)
  (local $prims0 i32)
  (local $prims1 i32)
  (local $process_binding i32)
  (local $schema_bindings i32)
  (local $schema_result i32)
  (local $schema_types0 i32)
  (local $t i32)
  (local $t1 i32)
  (local $t2 i32)
  (local $term i32)
  (local $tmap0 i32)
  (local $tmap1 i32)
  (local $transform_binding i32)
  (local $transform_term i32)
  (local $transformed i32)
  (local $ts i32)
  (local $ts1 i32)
  (local $tx i32)
  (local $wrapped i32)
  (local $x i32)
  local.get $g
  local.set $tx
  local.get $term
  call $hydra.adapt.push_type_apps_inward
  call $hydra.variables.unshadow_variables
  local.set $t1
  local.get $do_expand
  local.get $tx
  local.get $t1
  call $hydra.reduction.eta_expand_term
  call $hydra.adapt.push_type_apps_inward
  local.get $t1
  call $hydra.lib.logic.if_else
  call $hydra.variables.unshadow_variables
  local.set $t2
  local.get $t2
  call $hydra.dependencies.lift_lambda_above_let
  local.set $transform_term
  local.get $el
  ;; project field: name
  local.get $g
  local.get $el
  ;; project field: term
  local.get $transform_term
  local.get $el
  ;; project field: type
  local.set $transform_binding
  local.get $constraints
  call $hydra.adapt.adapt_literal_types_map
  local.set $litmap
  local.get $graph0
  ;; project field: primitives
  local.set $prims0
  local.get $graph0
  ;; project field: schema_types
  local.set $schema_types0
  local.get $ts
  call $hydra.scoping.type_scheme_to_f_type
  local.get $schema_types0
  call $hydra.lib.maps.map
  call $hydra.environment.types_to_definitions
  local.set $schema_bindings
  local.get $schema_types0
  call $hydra.lib.maps.null
  i32.const 1
  call $hydra.lib.maps.empty
  nop
  local.get $x
  local.get $cx
  local.get $graph0
  local.get $schema_bindings
  call $hydra.environment.graph_as_types
  call $hydra.lib.eithers.bimap
  local.get $constraints
  local.get $litmap
  local.get $tmap0
  call $hydra.adapt.adapt_graph_schema
  i32.const 1
  local.get $t
  call $hydra.resolution.type_to_type_scheme
  local.get $tmap1
  call $hydra.lib.maps.map
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.logic.if_else
  local.get $schema_result
  local.set $adapted_schema_types
  local.get $graph0
  local.get $el
  local.get $transform_binding
  local.set $transformed
  local.get $transformed
  call $hydra.lib.lists.pure
  i32.const 0
  local.set $wrapped
  local.get $constraints
  local.get $litmap
  local.get $cx
  local.get $graph0
  local.get $wrapped
  call $hydra.adapt.adapt_term
  local.get $constraints
  local.get $litmap
  call $hydra.adapt.adapt_lambda_domains
  local.get $adapted
  call $hydra.rewriting.rewrite_term_m
  call $hydra.lib.eithers.bind
  local.set $adapt_binding
  local.get $adapt_binding
  local.get $els0
  call $hydra.lib.eithers.map_list
  call $hydra.environment.term_as_bindings
  local.get $adapted_terms
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  local.set $els1_raw
  local.get $constraints
  local.get $litmap
  call $hydra.adapt.adapt_nested_types
  local.get $el
  ;; project field: term
  call $hydra.rewriting.rewrite_term_m
  i32.const 1
  i32.const 0
  local.get $constraints
  local.get $litmap
  local.get $ts
  call $hydra.adapt.adapt_type_scheme
  i32.const 1
  local.get $ts1
  call $hydra.lib.eithers.bind
  local.get $el
  ;; project field: type
  call $hydra.lib.maybes.maybe
  i32.const 1
  local.get $el
  ;; project field: name
  local.get $new_term
  local.get $adapted_type
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  local.set $process_binding
  local.get $process_binding
  local.get $els1_raw
  call $hydra.lib.eithers.map_list
  local.get $constraints
  local.get $litmap
  local.get $kv
  call $hydra.lib.pairs.second
  call $hydra.adapt.adapt_primitive
  i32.const 1
  local.get $kv
  call $hydra.lib.pairs.first
  local.get $prim1
  call $hydra.lib.eithers.bind
  local.get $prims0
  call $hydra.lib.maps.to_list
  call $hydra.lib.eithers.map_list
  local.get $prim_pairs
  call $hydra.lib.maps.from_list
  local.set $prims1
  local.get $els1
  call $hydra.lib.maps.empty
  local.get $prims1
  call $hydra.lexical.build_graph
  local.set $adapted_graph_raw
  local.get $adapted_graph_raw
  ;; project field: bound_terms
  local.get $adapted_graph_raw
  ;; project field: bound_types
  local.get $adapted_graph_raw
  ;; project field: class_constraints
  local.get $adapted_graph_raw
  ;; project field: lambda_variables
  local.get $adapted_graph_raw
  ;; project field: metadata
  local.get $adapted_graph_raw
  ;; project field: primitives
  local.get $adapted_schema_types
  local.get $adapted_graph_raw
  ;; project field: type_variables
  local.set $adapted_graph
  i32.const 1
  local.get $adapted_graph
  local.get $els1
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.adapt.adapt_float_type (param $constraints i32) (param $ft i32) (result i32)
  (local $alt i32)
  (local $for_unsupported i32)
  (local $ft2 i32)
  (local $supported i32)
  local.get $ft
  local.get $constraints
  ;; project field: float_types
  call $hydra.lib.sets.member
  local.set $supported
  local.get $constraints
  call $hydra.adapt.adapt_float_type
  local.set $alt
  (block $end_float_type (result i32)
  (block $float64
  (block $float32
  (block $bigfloat
  local.get $ft2
  br_table $bigfloat $float32 $float64 $float64
)
  i32.const 0
  local.get $alt
  br $end_float_type
)
  i32.const 0
  local.get $alt
  br $end_float_type
)
  i32.const 0
  local.get $alt
  br $end_float_type
)
  local.set $for_unsupported
  local.get $supported
  local.get $ft
  local.get $ft
  local.get $for_unsupported
  call $hydra.lib.logic.if_else
)
  (func $hydra.adapt.adapt_graph_schema (param $constraints i32) (param $litmap i32) (param $types0 i32) (result i32)
  (local $map_pair i32)
  (local $name i32)
  (local $pair i32)
  (local $pairs i32)
  (local $typ i32)
  (local $typ1 i32)
  local.get $pair
  call $hydra.lib.pairs.first
  local.set $name
  local.get $pair
  call $hydra.lib.pairs.second
  local.set $typ
  local.get $constraints
  local.get $litmap
  local.get $typ
  call $hydra.adapt.adapt_type
  i32.const 1
  local.get $name
  local.get $typ1
  call $hydra.lib.eithers.bind
  local.set $map_pair
  local.get $map_pair
  local.get $types0
  call $hydra.lib.maps.to_list
  call $hydra.lib.eithers.map_list
  i32.const 1
  local.get $pairs
  call $hydra.lib.maps.from_list
  call $hydra.lib.eithers.bind
)
  (func $hydra.adapt.adapt_integer_type (param $constraints i32) (param $it i32) (result i32)
  (local $alt i32)
  (local $for_unsupported i32)
  (local $it2 i32)
  (local $supported i32)
  local.get $it
  local.get $constraints
  ;; project field: integer_types
  call $hydra.lib.sets.member
  local.set $supported
  local.get $constraints
  call $hydra.adapt.adapt_integer_type
  local.set $alt
  (block $end_integer_type (result i32)
  (block $uint64
  (block $uint32
  (block $uint16
  (block $uint8
  (block $int64
  (block $int32
  (block $int16
  (block $int8
  (block $bigint
  local.get $it2
  br_table $bigint $int8 $int16 $int32 $int64 $uint8 $uint16 $uint32 $uint64 $uint64
)
  i32.const 0
  br $end_integer_type
)
  i32.const 0
  local.get $alt
  br $end_integer_type
)
  i32.const 0
  local.get $alt
  br $end_integer_type
)
  i32.const 0
  local.get $alt
  br $end_integer_type
)
  i32.const 0
  local.get $alt
  br $end_integer_type
)
  i32.const 0
  local.get $alt
  br $end_integer_type
)
  i32.const 0
  local.get $alt
  br $end_integer_type
)
  i32.const 0
  local.get $alt
  br $end_integer_type
)
  i32.const 0
  local.get $alt
  br $end_integer_type
)
  local.set $for_unsupported
  local.get $supported
  local.get $it
  local.get $it
  local.get $for_unsupported
  call $hydra.lib.logic.if_else
)
  (func $hydra.adapt.adapt_lambda_domains (param $constraints i32) (param $litmap i32) (param $recurse i32) (param $term i32) (result i32)
  (local $adapted_domain i32)
  (local $dom i32)
  (local $dom1 i32)
  (local $f i32)
  (local $l i32)
  (local $rewritten i32)
  local.get $term
  local.get $recurse
  (block $end_term (result i32)
  (block $function
  local.get $rewritten
  br_table $function $function
)
  (block $end_function (result i32)
  (block $lambda
  local.get $f
  br_table $lambda $lambda
)
  i32.const 1
  i32.const 0
  local.get $constraints
  local.get $litmap
  local.get $dom
  call $hydra.adapt.adapt_type
  i32.const 1
  local.get $dom1
  call $hydra.lib.eithers.bind
  local.get $l
  ;; project field: domain
  call $hydra.lib.maybes.maybe
  i32.const 1
  local.get $l
  ;; project field: parameter
  local.get $adapted_domain
  local.get $l
  ;; project field: body
  call $hydra.lib.eithers.bind
  br $end_function
)
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.adapt.adapt_literal (param $lt i32) (param $l i32) (result i32)
  (local $b i32)
  (local $f i32)
  (local $ft i32)
  (local $i i32)
  (local $it i32)
  (block $end_literal (result i32)
  (block $integer
  (block $float
  (block $boolean
  (block $binary
  local.get $l
  br_table $binary $boolean $float $integer $integer
)
  (block $end_literal_type (result i32)
  (block $string
  local.get $lt
  br_table $string $string
)
  local.get $b
  call $hydra.lib.literals.binary_to_string
  br $end_literal_type
)
  br $end_literal
)
  (block $end_literal_type (result i32)
  (block $integer
  local.get $lt
  br_table $integer $integer
)
  local.get $it
  local.get $b
  i32.const 0 ;; string: "1:bigint"
  i32.const 0 ;; string: "0:bigint"
  call $hydra.lib.logic.if_else
  call $hydra.literals.bigint_to_integer_value
  br $end_literal_type
)
  br $end_literal
)
  (block $end_literal_type (result i32)
  (block $float
  local.get $lt
  br_table $float $float
)
  local.get $ft
  local.get $f
  call $hydra.literals.float_value_to_bigfloat
  call $hydra.literals.bigfloat_to_float_value
  br $end_literal_type
)
  br $end_literal
)
  (block $end_literal_type (result i32)
  (block $integer
  local.get $lt
  br_table $integer $integer
)
  local.get $it
  local.get $i
  call $hydra.literals.integer_value_to_bigint
  call $hydra.literals.bigint_to_integer_value
  br $end_literal_type
)
  br $end_literal
)
)
  (func $hydra.adapt.adapt_literal_type (param $constraints i32) (param $lt i32) (result i32)
  (local $for_unsupported i32)
  (local $ft i32)
  (local $it i32)
  (local $lt2 i32)
  (local $x i32)
  (block $end_literal_type (result i32)
  (block $integer
  (block $float
  (block $boolean
  (block $binary
  local.get $lt2
  br_table $binary $boolean $float $integer $integer
)
  i32.const 0
  br $end_literal_type
)
  local.get $x
  local.get $constraints
  i32.const 0
  call $hydra.adapt.adapt_integer_type
  call $hydra.lib.maybes.map
  br $end_literal_type
)
  local.get $x
  local.get $constraints
  local.get $ft
  call $hydra.adapt.adapt_float_type
  call $hydra.lib.maybes.map
  br $end_literal_type
)
  local.get $x
  local.get $constraints
  local.get $it
  call $hydra.adapt.adapt_integer_type
  call $hydra.lib.maybes.map
  br $end_literal_type
)
  local.set $for_unsupported
  local.get $constraints
  local.get $lt
  call $hydra.adapt.literal_type_supported
  i32.const 0
  local.get $lt
  local.get $for_unsupported
  call $hydra.lib.logic.if_else
)
  (func $hydra.adapt.adapt_literal_types_map (param $constraints i32) (result i32)
  (local $lt i32)
  (local $lt2 i32)
  (local $try_type i32)
  i32.const 0
  local.get $lt
  local.get $lt2
  local.get $constraints
  local.get $lt
  call $hydra.adapt.adapt_literal_type
  call $hydra.lib.maybes.maybe
  local.set $try_type
  local.get $try_type
  call $hydra.reflect.literal_types
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  call $hydra.lib.maps.from_list
)
  (func $hydra.adapt.adapt_literal_value (param $litmap i32) (param $lt i32) (param $l i32) (result i32)
  (local $lt2 i32)
  local.get $l
  call $hydra.show.core.literal
  local.get $lt2
  local.get $l
  call $hydra.adapt.adapt_literal
  local.get $lt
  local.get $litmap
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
)
  (func $hydra.adapt.adapt_nested_types (param $constraints i32) (param $litmap i32) (param $recurse i32) (param $term i32) (result i32)
  (local $adapt_b i32)
  (local $adapted_b_type i32)
  (local $adapted_bindings i32)
  (local $b i32)
  (local $lt i32)
  (local $rewritten i32)
  (local $ts i32)
  (local $ts1 i32)
  local.get $term
  local.get $recurse
  (block $end_term (result i32)
  (block $let
  local.get $rewritten
  br_table $let $let
)
  i32.const 1
  i32.const 0
  local.get $constraints
  local.get $litmap
  local.get $ts
  call $hydra.adapt.adapt_type_scheme
  i32.const 1
  local.get $ts1
  call $hydra.lib.eithers.bind
  local.get $b
  ;; project field: type
  call $hydra.lib.maybes.maybe
  i32.const 1
  local.get $b
  ;; project field: name
  local.get $b
  ;; project field: term
  local.get $adapted_b_type
  call $hydra.lib.eithers.bind
  local.set $adapt_b
  local.get $adapt_b
  local.get $lt
  ;; project field: bindings
  call $hydra.lib.eithers.map_list
  i32.const 1
  local.get $adapted_bindings
  local.get $lt
  ;; project field: body
  call $hydra.lib.eithers.bind
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.adapt.adapt_primitive (param $constraints i32) (param $litmap i32) (param $prim0 i32) (result i32)
  (local $ts0 i32)
  (local $ts1 i32)
  local.get $prim0
  ;; project field: type
  local.set $ts0
  local.get $constraints
  local.get $litmap
  local.get $ts0
  call $hydra.adapt.adapt_type_scheme
  i32.const 1
  local.get $prim0
  ;; project field: name
  local.get $ts1
  local.get $prim0
  ;; project field: implementation
  call $hydra.lib.eithers.bind
)
  (func $hydra.adapt.adapt_term (param $constraints i32) (param $litmap i32) (param $cx i32) (param $graph i32) (param $term0 i32) (result i32)
  (local $alts i32)
  (local $alts0 i32)
  (local $atyp i32)
  (local $for_non_null i32)
  (local $for_supported i32)
  (local $for_unsupported i32)
  (local $l i32)
  (local $lt i32)
  (local $mterm i32)
  (local $recurse i32)
  (local $rewrite i32)
  (local $supported_variant i32)
  (local $t i32)
  (local $ta i32)
  (local $term i32)
  (local $term02 i32)
  (local $term1 i32)
  (local $try_alts i32)
  (local $try_term i32)
  (block $end_term (result i32)
  (block $literal
  local.get $term
  br_table $literal $literal
)
  local.get $l
  call $hydra.reflect.literal_type
  local.set $lt
  i32.const 1
  local.get $constraints
  local.get $lt
  call $hydra.adapt.literal_type_supported
  local.get $term
  local.get $litmap
  local.get $lt
  local.get $l
  call $hydra.adapt.adapt_literal_value
  call $hydra.lib.logic.if_else
  br $end_term
)
  local.set $for_supported
  local.get $alts
  call $hydra.lib.lists.head
  local.get $try_term
  local.get $alts
  call $hydra.lib.lists.tail
  local.get $try_alts
  i32.const 1
  local.get $t
  local.get $mterm
  call $hydra.lib.maybes.maybe
  call $hydra.lib.eithers.bind
  local.set $for_non_null
  local.get $alts
  call $hydra.lib.lists.null
  i32.const 1
  i32.const 0
  local.get $alts
  local.get $for_non_null
  call $hydra.lib.logic.if_else
  local.set $try_alts
  local.get $cx
  local.get $graph
  local.get $term
  call $hydra.adapt.term_alternatives
  local.get $alts0
  local.get $try_alts
  call $hydra.lib.eithers.bind
  local.set $for_unsupported
  local.get $term
  call $hydra.reflect.term_variant
  local.get $constraints
  ;; project field: term_variants
  call $hydra.lib.sets.member
  local.set $supported_variant
  local.get $supported_variant
  local.get $term
  local.get $for_supported
  local.get $term
  local.get $for_unsupported
  call $hydra.lib.logic.if_else
  local.set $try_term
  local.get $term02
  local.get $recurse
  (block $end_term (result i32)
  (block $type_lambda
  (block $type_application
  local.get $term1
  br_table $type_application $type_lambda $type_lambda
)
  local.get $constraints
  local.get $litmap
  local.get $ta
  ;; project field: type
  call $hydra.adapt.adapt_type
  i32.const 1
  local.get $ta
  ;; project field: body
  local.get $atyp
  call $hydra.lib.eithers.bind
  br $end_term
)
  i32.const 1
  local.get $term1
  br $end_term
)
  call $hydra.lib.eithers.bind
  local.set $rewrite
  local.get $rewrite
  local.get $term0
  call $hydra.rewriting.rewrite_term_m
)
  (func $hydra.adapt.adapt_term_for_language (param $lang i32) (param $cx i32) (param $g i32) (param $term i32) (result i32)
  (local $constraints i32)
  (local $litmap i32)
  local.get $lang
  ;; project field: constraints
  local.set $constraints
  local.get $constraints
  call $hydra.adapt.adapt_literal_types_map
  local.set $litmap
  local.get $constraints
  local.get $litmap
  local.get $cx
  local.get $g
  local.get $term
  call $hydra.adapt.adapt_term
)
  (func $hydra.adapt.adapt_type (param $constraints i32) (param $litmap i32) (param $type0 i32) (result i32)
  (local $alts i32)
  (local $alts0 i32)
  (local $for_supported i32)
  (local $for_unsupported i32)
  (local $lt i32)
  (local $lt2 i32)
  (local $recurse i32)
  (local $rewrite i32)
  (local $supported_variant i32)
  (local $t i32)
  (local $try_alts i32)
  (local $try_type i32)
  (local $typ i32)
  (local $type1 i32)
  (local $type2 i32)
  (block $end_type (result i32)
  (block $literal
  local.get $typ
  br_table $literal $literal
)
  local.get $constraints
  local.get $lt
  call $hydra.adapt.literal_type_supported
  local.get $typ
  i32.const 0
  local.get $lt2
  local.get $lt
  local.get $litmap
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  call $hydra.lib.logic.if_else
  br $end_type
)
  local.set $for_supported
  local.get $alts
  call $hydra.lib.lists.null
  i32.const 0
  local.get $alts
  call $hydra.lib.lists.tail
  local.get $try_alts
  local.get $t
  local.get $alts
  call $hydra.lib.lists.head
  local.get $try_type
  call $hydra.lib.maybes.maybe
  call $hydra.lib.logic.if_else
  local.set $try_alts
  local.get $typ
  call $hydra.adapt.type_alternatives
  local.set $alts0
  local.get $alts0
  local.get $try_alts
  local.set $for_unsupported
  local.get $typ
  call $hydra.reflect.type_variant
  local.get $constraints
  ;; project field: type_variants
  call $hydra.lib.sets.member
  local.set $supported_variant
  local.get $supported_variant
  local.get $typ
  local.get $for_supported
  local.get $typ
  local.get $for_unsupported
  call $hydra.lib.logic.if_else
  local.set $try_type
  local.get $typ
  local.get $recurse
  i32.const 0
  i32.const 0 ;; string: "no alternatives for type: "
  local.get $typ
  call $hydra.show.core.type
  call $hydra.lib.strings.cat2
  i32.const 1
  local.get $type2
  local.get $type1
  local.get $try_type
  call $hydra.lib.maybes.maybe
  call $hydra.lib.eithers.bind
  local.set $rewrite
  local.get $rewrite
  local.get $type0
  call $hydra.rewriting.rewrite_type_m
)
  (func $hydra.adapt.adapt_type_for_language (param $lang i32) (param $typ i32) (result i32)
  (local $constraints i32)
  (local $litmap i32)
  local.get $lang
  ;; project field: constraints
  local.set $constraints
  local.get $constraints
  call $hydra.adapt.adapt_literal_types_map
  local.set $litmap
  local.get $constraints
  local.get $litmap
  local.get $typ
  call $hydra.adapt.adapt_type
)
  (func $hydra.adapt.adapt_type_scheme (param $constraints i32) (param $litmap i32) (param $ts0 i32) (result i32)
  (local $t0 i32)
  (local $t1 i32)
  (local $vars0 i32)
  local.get $ts0
  ;; project field: variables
  local.set $vars0
  local.get $ts0
  ;; project field: type
  local.set $t0
  local.get $constraints
  local.get $litmap
  local.get $t0
  call $hydra.adapt.adapt_type
  i32.const 1
  local.get $vars0
  local.get $t1
  local.get $ts0
  ;; project field: constraints
  call $hydra.lib.eithers.bind
)
  (func $hydra.adapt.compose_coders (param $c1 i32) (param $c2 i32) (result i32)
  (local $a i32)
  (local $b1 i32)
  (local $b2 i32)
  (local $c i32)
  (local $cx i32)
  local.get $c1
  local.get $cx
  local.get $a
  ;; project field: encode
  local.get $c2
  local.get $cx
  local.get $b1
  ;; project field: encode
  call $hydra.lib.eithers.bind
  local.get $c2
  local.get $cx
  local.get $c
  ;; project field: decode
  local.get $c1
  local.get $cx
  local.get $b2
  ;; project field: decode
  call $hydra.lib.eithers.bind
)
  (func $hydra.adapt.data_graph_to_definitions (param $constraints i32) (param $do_infer i32) (param $do_expand i32) (param $do_hoist_case_statements i32) (param $do_hoist_polymorphic_let_bindings i32) (param $original_bindings i32) (param $graph0 i32) (param $namespaces i32) (param $cx i32) (result i32)
  (local $acc i32)
  (local $adapt_result i32)
  (local $adapted i32)
  (local $adapted_bindings i32)
  (local $b i32)
  (local $bindings i32)
  (local $bins0 i32)
  (local $bins1 i32)
  (local $bins2 i32)
  (local $bins3 i32)
  (local $bins4 i32)
  (local $bins5 i32)
  (local $check_bindings_typed i32)
  (local $debug_label i32)
  (local $defs_grouped i32)
  (local $el i32)
  (local $elements_by_namespace i32)
  (local $els_for_ns i32)
  (local $existing i32)
  (local $g i32)
  (local $hoist_cases i32)
  (local $hoist_poly i32)
  (local $hoisted i32)
  (local $ic i32)
  (local $is_parent_binding i32)
  (local $let_after i32)
  (local $let_before i32)
  (local $namespaces_set i32)
  (local $normalize_bindings i32)
  (local $ns i32)
  (local $rebuild_graph i32)
  (local $result i32)
  (local $selected_elements i32)
  (local $stripped i32)
  (local $term0 i32)
  (local $term1 i32)
  (local $to_def i32)
  (local $ts i32)
  (local $unshadowed0 i32)
  (local $untyped_bindings i32)
  (local $x i32)
  local.get $namespaces
  call $hydra.lib.sets.from_list
  local.set $namespaces_set
  i32.const 0
  local.get $ns
  local.get $namespaces_set
  call $hydra.lib.sets.member
  local.get $b
  ;; project field: name
  call $hydra.names.namespace_of
  call $hydra.lib.maybes.maybe
  local.set $is_parent_binding
  local.get $b
  ;; project field: name
  local.get $b
  ;; project field: term
  call $hydra.strip.strip_type_lambdas
  local.get $b
  ;; project field: type
  local.get $bindings
  call $hydra.lib.lists.map
  local.set $stripped
  local.get $stripped
  i32.const 0
  local.set $term0
  local.get $term0
  call $hydra.variables.unshadow_variables
  call $hydra.environment.term_as_bindings
  local.set $unshadowed0
  local.get $unshadowed0
  call $hydra.hoisting.hoist_case_statements_in_graph
  local.set $hoisted
  local.get $hoisted
  i32.const 0
  local.set $term1
  local.get $term1
  call $hydra.variables.unshadow_variables
  call $hydra.environment.term_as_bindings
  local.set $hoist_cases
  local.get $bindings
  i32.const 0
  local.set $let_before
  local.get $is_parent_binding
  local.get $let_before
  call $hydra.hoisting.hoist_polymorphic_let_bindings
  local.set $let_after
  local.get $let_after
  ;; project field: bindings
  local.set $hoist_poly
  nop
  local.get $b
  ;; project field: type
  call $hydra.lib.maybes.is_just
  call $hydra.lib.logic.not
  local.get $bindings
  call $hydra.lib.lists.filter
  call $hydra.lib.lists.map
  local.set $untyped_bindings
  local.get $untyped_bindings
  call $hydra.lib.lists.null
  i32.const 1
  local.get $bindings
  i32.const 0
  i32.const 4
  ;; list elements follow
  i32.const 0 ;; string: "Found untyped bindings ("
  local.get $debug_label
  i32.const 0 ;; string: "): "
  i32.const 0 ;; string: ", "
  local.get $untyped_bindings
  call $hydra.lib.strings.intercalate
  call $hydra.lib.strings.cat
  call $hydra.lib.logic.if_else
  local.set $check_bindings_typed
  local.get $b
  ;; project field: name
  local.get $b
  ;; project field: term
  call $hydra.adapt.push_type_apps_inward
  local.get $b
  ;; project field: type
  local.get $bindings
  call $hydra.lib.lists.map
  local.set $normalize_bindings
  local.get $bindings
  call $hydra.lib.maps.empty
  local.get $graph0
  ;; project field: primitives
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
  local.get $graph0
  ;; project field: schema_types
  local.get $g
  ;; project field: type_variables
  local.set $rebuild_graph
  local.get $original_bindings
  local.set $bins0
  local.get $do_hoist_case_statements
  local.get $bins0
  local.get $hoist_cases
  local.get $bins0
  call $hydra.lib.logic.if_else
  local.set $bins1
  local.get $do_infer
  local.get $result
  call $hydra.lib.pairs.first
  call $hydra.lib.pairs.second
  local.get $ic
  ;; project field: object
  call $hydra.show.errors.error
  local.get $x
  local.get $cx
  local.get $bins1
  local.get $bins1
  local.get $rebuild_graph
  call $hydra.inference.infer_graph_types
  call $hydra.lib.eithers.bimap
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "after case hoisting"
  local.get $bins1
  local.get $check_bindings_typed
  call $hydra.lib.logic.if_else
  local.get $do_hoist_polymorphic_let_bindings
  i32.const 0 ;; string: "after let hoisting"
  local.get $bins2
  local.get $hoist_poly
  local.get $check_bindings_typed
  i32.const 1
  local.get $bins2
  call $hydra.lib.logic.if_else
  local.get $constraints
  local.get $do_expand
  local.get $bins3
  local.get $cx
  local.get $bins3
  local.get $rebuild_graph
  call $hydra.adapt.adapt_data_graph
  local.get $adapt_result
  call $hydra.lib.pairs.first
  local.set $adapted
  local.get $adapt_result
  call $hydra.lib.pairs.second
  local.set $adapted_bindings
  i32.const 0 ;; string: "after adaptation"
  local.get $adapted_bindings
  local.get $check_bindings_typed
  local.get $bins4
  local.get $normalize_bindings
  local.set $bins5
  local.get $el
  ;; project field: name
  local.get $el
  ;; project field: term
  local.get $ts
  local.get $el
  ;; project field: type
  call $hydra.lib.maybes.map
  local.set $to_def
  i32.const 0
  local.get $ns
  local.get $namespaces_set
  call $hydra.lib.sets.member
  local.get $el
  ;; project field: name
  call $hydra.names.namespace_of
  call $hydra.lib.maybes.maybe
  local.get $bins5
  call $hydra.lib.lists.filter
  local.set $selected_elements
  local.get $acc
  i32.const 0
  ;; list elements follow
  call $hydra.lib.equality.identity
  local.get $ns
  local.get $acc
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  local.set $existing
  local.get $ns
  local.get $existing
  i32.const 1
  ;; list elements follow
  local.get $el
  call $hydra.lib.lists.concat2
  local.get $acc
  call $hydra.lib.maps.insert
  local.get $el
  ;; project field: name
  call $hydra.names.namespace_of
  call $hydra.lib.maybes.maybe
  call $hydra.lib.maps.empty
  local.get $selected_elements
  call $hydra.lib.lists.foldl
  local.set $elements_by_namespace
  i32.const 0
  ;; list elements follow
  call $hydra.lib.equality.identity
  local.get $ns
  local.get $elements_by_namespace
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  local.set $els_for_ns
  local.get $to_def
  local.get $els_for_ns
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  local.get $namespaces
  call $hydra.lib.lists.map
  local.set $defs_grouped
  local.get $bins5
  call $hydra.lib.maps.empty
  local.get $adapted
  ;; project field: primitives
  call $hydra.lexical.build_graph
  local.set $g
  i32.const 1
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
  local.get $adapted
  ;; project field: schema_types
  local.get $g
  ;; project field: type_variables
  local.get $defs_grouped
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.adapt.literal_type_supported (param $constraints i32) (param $lt i32) (result i32)
  (local $for_type i32)
  (local $ft i32)
  (local $it i32)
  (local $lt2 i32)
  (block $end_literal_type (result i32)
  (block $integer
  (block $float
  local.get $lt2
  br_table $float $integer $integer
)
  local.get $ft
  local.get $constraints
  ;; project field: float_types
  call $hydra.lib.sets.member
  br $end_literal_type
)
  local.get $it
  local.get $constraints
  ;; project field: integer_types
  call $hydra.lib.sets.member
  br $end_literal_type
)
  local.set $for_type
  local.get $lt
  call $hydra.reflect.literal_type_variant
  local.get $constraints
  ;; project field: literal_variants
  call $hydra.lib.sets.member
  local.get $lt
  local.get $for_type
  i32.const 0
  call $hydra.lib.logic.if_else
)
  (func $hydra.adapt.prepare_float_type (param $ft i32) (result i32)
  (local $d i32)
  (local $v i32)
  (block $end_float_type (result i32)
  (block $bigfloat
  local.get $ft
  br_table $bigfloat $bigfloat
)
  i32.const 0
  (block $end_float_value (result i32)
  (block $bigfloat
  local.get $v
  br_table $bigfloat $bigfloat
)
  local.get $d
  call $hydra.lib.literals.bigfloat_to_float64
  br $end_float_value
)
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "replace arbitrary-precision floating-point numbers with 64-bit floating-point numbers (doubles)"
  call $hydra.lib.sets.from_list
  br $end_float_type
)
)
  (func $hydra.adapt.prepare_integer_type (param $it i32) (result i32)
  (local $i i32)
  (local $v i32)
  (block $end_integer_type (result i32)
  (block $uint64
  (block $uint32
  (block $uint8
  (block $bigint
  local.get $it
  br_table $bigint $uint8 $uint32 $uint64 $uint64
)
  i32.const 0
  (block $end_integer_value (result i32)
  (block $bigint
  local.get $v
  br_table $bigint $bigint
)
  local.get $i
  call $hydra.lib.literals.bigint_to_int64
  br $end_integer_value
)
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "replace arbitrary-precision integers with 64-bit integers"
  call $hydra.lib.sets.from_list
  br $end_integer_type
)
  i32.const 0
  (block $end_integer_value (result i32)
  (block $uint8
  local.get $v
  br_table $uint8 $uint8
)
  local.get $i
  call $hydra.lib.literals.uint8_to_bigint
  call $hydra.lib.literals.bigint_to_int8
  br $end_integer_value
)
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "replace unsigned 8-bit integers with signed 8-bit integers"
  call $hydra.lib.sets.from_list
  br $end_integer_type
)
  i32.const 0
  (block $end_integer_value (result i32)
  (block $uint32
  local.get $v
  br_table $uint32 $uint32
)
  local.get $i
  call $hydra.lib.literals.uint32_to_bigint
  call $hydra.lib.literals.bigint_to_int32
  br $end_integer_value
)
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "replace unsigned 32-bit integers with signed 32-bit integers"
  call $hydra.lib.sets.from_list
  br $end_integer_type
)
  i32.const 0
  (block $end_integer_value (result i32)
  (block $uint64
  local.get $v
  br_table $uint64 $uint64
)
  local.get $i
  call $hydra.lib.literals.uint64_to_bigint
  call $hydra.lib.literals.bigint_to_int64
  br $end_integer_value
)
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "replace unsigned 64-bit integers with signed 64-bit integers"
  call $hydra.lib.sets.from_list
  br $end_integer_type
)
)
  (func $hydra.adapt.prepare_literal_type (param $at i32) (result i32)
  (local $b i32)
  (local $ft i32)
  (local $fv i32)
  (local $it i32)
  (local $iv i32)
  (local $msgs i32)
  (local $rep i32)
  (local $result i32)
  (local $rtyp i32)
  (local $v i32)
  (block $end_literal_type (result i32)
  (block $integer
  (block $float
  (block $binary
  local.get $at
  br_table $binary $float $integer $integer
)
  i32.const 0
  (block $end_literal (result i32)
  (block $binary
  local.get $v
  br_table $binary $binary
)
  local.get $b
  call $hydra.lib.literals.binary_to_string
  br $end_literal
)
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "replace binary strings with character strings"
  call $hydra.lib.sets.from_list
  br $end_literal_type
)
  local.get $ft
  call $hydra.adapt.prepare_float_type
  local.set $result
  local.get $result
  call $hydra.lib.pairs.first
  local.set $rtyp
  local.get $result
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.set $rep
  local.get $result
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  local.set $msgs
  local.get $rtyp
  (block $end_literal (result i32)
  (block $float
  local.get $v
  br_table $float $float
)
  local.get $fv
  local.get $rep
  br $end_literal
)
  local.get $msgs
  br $end_literal_type
)
  local.get $it
  call $hydra.adapt.prepare_integer_type
  local.set $result
  local.get $result
  call $hydra.lib.pairs.first
  local.set $rtyp
  local.get $result
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.set $rep
  local.get $result
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  local.set $msgs
  local.get $rtyp
  (block $end_literal (result i32)
  (block $integer
  local.get $v
  br_table $integer $integer
)
  local.get $iv
  local.get $rep
  br $end_literal
)
  local.get $msgs
  br $end_literal_type
)
)
  (func $hydra.adapt.prepare_same (param $x i32) (result i32)
  (local $y i32)
  local.get $x
  local.get $y
  call $hydra.lib.sets.empty
)
  (func $hydra.adapt.prepare_type (param $cx i32) (param $typ i32) (result i32)
  (local $at i32)
  (local $av i32)
  (local $msgs i32)
  (local $rep i32)
  (local $result i32)
  (local $rtyp i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $literal
  local.get $typ
  call $hydra.strip.deannotate_type
  br_table $literal $literal
)
  local.get $at
  call $hydra.adapt.prepare_literal_type
  local.set $result
  local.get $result
  call $hydra.lib.pairs.first
  local.set $rtyp
  local.get $result
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.set $rep
  local.get $result
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  local.set $msgs
  local.get $rtyp
  (block $end_term (result i32)
  (block $literal
  local.get $v
  br_table $literal $literal
)
  local.get $av
  local.get $rep
  br $end_term
)
  local.get $msgs
  br $end_type
)
)
  (func $hydra.adapt.push_type_apps_inward (param $term i32) (result i32)
  (local $a i32)
  (local $at i32)
  (local $b i32)
  (local $body i32)
  (local $body1 i32)
  (local $cs i32)
  (local $e i32)
  (local $elm i32)
  (local $els i32)
  (local $f i32)
  (local $fld i32)
  (local $for_elimination i32)
  (local $for_field i32)
  (local $for_function i32)
  (local $for_let i32)
  (local $for_map i32)
  (local $for_pair i32)
  (local $fun i32)
  (local $go i32)
  (local $i i32)
  (local $l i32)
  (local $lt i32)
  (local $m i32)
  (local $map_binding i32)
  (local $name i32)
  (local $p i32)
  (local $push i32)
  (local $r i32)
  (local $s i32)
  (local $t i32)
  (local $ta i32)
  (local $tt i32)
  (local $typ i32)
  (local $v i32)
  (local $wt i32)
  (block $end_term (result i32)
  (block $let
  (block $function
  (block $application
  local.get $body
  br_table $application $function $let $let
)
  local.get $a
  ;; project field: function
  local.get $typ
  local.get $a
  ;; project field: argument
  local.get $go
  br $end_term
)
  (block $end_function (result i32)
  (block $lambda
  local.get $f
  br_table $lambda $lambda
)
  local.get $l
  ;; project field: parameter
  local.get $l
  ;; project field: domain
  local.get $l
  ;; project field: body
  local.get $typ
  local.get $go
  br $end_function
)
  br $end_term
)
  local.get $lt
  ;; project field: bindings
  local.get $lt
  ;; project field: body
  local.get $typ
  local.get $go
  br $end_term
)
  local.set $push
  local.get $fld
  ;; project field: name
  local.get $fld
  ;; project field: term
  local.get $go
  local.set $for_field
  (block $end_elimination (result i32)
  (block $wrap
  (block $union
  (block $record
  local.get $elm
  br_table $record $union $wrap $wrap
)
  local.get $p
  br $end_elimination
)
  local.get $cs
  ;; project field: type_name
  local.get $go
  local.get $cs
  ;; project field: default
  call $hydra.lib.maybes.map
  local.get $for_field
  local.get $cs
  ;; project field: cases
  call $hydra.lib.lists.map
  br $end_elimination
)
  local.get $name
  br $end_elimination
)
  local.set $for_elimination
  (block $end_function (result i32)
  (block $lambda
  (block $elimination
  local.get $fun
  br_table $elimination $lambda $lambda
)
  local.get $elm
  local.get $for_elimination
  br $end_function
)
  local.get $l
  ;; project field: parameter
  local.get $l
  ;; project field: domain
  local.get $l
  ;; project field: body
  local.get $go
  br $end_function
)
  local.set $for_function
  local.get $b
  ;; project field: name
  local.get $b
  ;; project field: term
  local.get $go
  local.get $b
  ;; project field: type
  local.set $map_binding
  local.get $map_binding
  local.get $lt
  ;; project field: bindings
  call $hydra.lib.lists.map
  local.get $lt
  ;; project field: body
  local.get $go
  local.set $for_let
  local.get $p
  call $hydra.lib.pairs.first
  local.get $go
  local.get $p
  call $hydra.lib.pairs.second
  local.get $go
  local.set $for_pair
  local.get $for_pair
  local.get $m
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $for_map
  (block $end_term (result i32)
  (block $wrap
  (block $variable
  (block $unit
  (block $union
  (block $type_lambda
  (block $type_application
  (block $set
  (block $record
  (block $pair
  (block $maybe
  (block $map
  (block $literal
  (block $list
  (block $let
  (block $function
  (block $either
  (block $application
  (block $annotated
  local.get $t
  br_table $annotated $application $either $function $let $list $literal $map $maybe $pair $record $set $type_application $type_lambda $union $unit $variable $wrap $wrap
)
  local.get $at
  ;; project field: body
  local.get $go
  local.get $at
  ;; project field: annotation
  br $end_term
)
  local.get $a
  ;; project field: function
  local.get $go
  local.get $a
  ;; project field: argument
  local.get $go
  br $end_term
)
  i32.const 0
  local.get $l
  local.get $go
  i32.const 1
  local.get $r
  local.get $go
  local.get $e
  call $hydra.lib.eithers.either
  br $end_term
)
  local.get $fun
  local.get $for_function
  br $end_term
)
  local.get $lt
  local.get $for_let
  br $end_term
)
  local.get $go
  local.get $els
  call $hydra.lib.lists.map
  br $end_term
)
  local.get $v
  br $end_term
)
  local.get $m
  local.get $for_map
  br $end_term
)
  local.get $go
  local.get $m
  call $hydra.lib.maybes.map
  br $end_term
)
  local.get $p
  call $hydra.lib.pairs.first
  local.get $go
  local.get $p
  call $hydra.lib.pairs.second
  local.get $go
  br $end_term
)
  local.get $r
  ;; project field: type_name
  local.get $for_field
  local.get $r
  ;; project field: fields
  call $hydra.lib.lists.map
  br $end_term
)
  local.get $go
  local.get $s
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.sets.from_list
  br $end_term
)
  local.get $tt
  ;; project field: body
  local.get $go
  local.set $body1
  local.get $body1
  local.get $tt
  ;; project field: type
  local.get $push
  br $end_term
)
  local.get $ta
  ;; project field: parameter
  local.get $ta
  ;; project field: body
  local.get $go
  br $end_term
)
  local.get $i
  ;; project field: type_name
  local.get $i
  ;; project field: field
  local.get $for_field
  br $end_term
)
  i32.const 0
  br $end_term
)
  local.get $v
  br $end_term
)
  local.get $wt
  ;; project field: type_name
  local.get $wt
  ;; project field: body
  local.get $go
  br $end_term
)
  local.set $go
  local.get $term
  local.get $go
)
  (func $hydra.adapt.schema_graph_to_definitions (param $constraints i32) (param $graph i32) (param $name_lists i32) (param $cx i32) (result i32)
  (local $litmap i32)
  (local $n i32)
  (local $names i32)
  (local $pair i32)
  (local $tmap0 i32)
  (local $tmap1 i32)
  (local $to_def i32)
  (local $x i32)
  local.get $constraints
  call $hydra.adapt.adapt_literal_types_map
  local.set $litmap
  nop
  local.get $x
  local.get $cx
  local.get $graph
  local.get $graph
  call $hydra.lexical.graph_to_bindings
  call $hydra.environment.graph_as_types
  call $hydra.lib.eithers.bimap
  local.get $constraints
  local.get $litmap
  local.get $tmap0
  call $hydra.adapt.adapt_graph_schema
  local.get $pair
  call $hydra.lib.pairs.first
  i32.const 0
  ;; list elements follow
  local.get $pair
  call $hydra.lib.pairs.second
  i32.const 0
  local.set $to_def
  i32.const 1
  local.get $tmap1
  local.get $to_def
  local.get $n
  local.get $n
  local.get $tmap1
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.from_just
  local.get $names
  call $hydra.lib.lists.map
  call $hydra.lib.lists.map
  local.get $name_lists
  call $hydra.lib.lists.map
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.adapt.simple_language_adapter (param $lang i32) (param $cx i32) (param $g i32) (param $typ i32) (result i32)
  (local $_s i32)
  (local $_x i32)
  (local $adapted_type i32)
  (local $constraints i32)
  (local $cx2 i32)
  (local $litmap i32)
  (local $term i32)
  local.get $lang
  ;; project field: constraints
  local.set $constraints
  local.get $constraints
  call $hydra.adapt.adapt_literal_types_map
  local.set $litmap
  local.get $constraints
  local.get $litmap
  local.get $typ
  call $hydra.adapt.adapt_type
  i32.const 1
  i32.const 0
  local.get $typ
  local.get $adapted_type
  local.get $_s
  local.get $cx2
  local.get $_x
  local.get $constraints
  local.get $litmap
  local.get $cx2
  local.get $g
  local.get $term
  call $hydra.adapt.adapt_term
  call $hydra.lib.eithers.bimap
  i32.const 1
  local.get $term
  call $hydra.lib.eithers.bind
)
  (func $hydra.adapt.term_alternatives (param $cx i32) (param $graph i32) (param $term i32) (result i32)
  (local $abs i32)
  (local $at i32)
  (local $field i32)
  (local $fname i32)
  (local $for_field_type i32)
  (local $ft i32)
  (local $fterm i32)
  (local $ftname i32)
  (local $ic i32)
  (local $inj i32)
  (local $ot i32)
  (local $rt i32)
  (local $ta i32)
  (local $term2 i32)
  (local $tname i32)
  (local $wt i32)
  (local $x i32)
  (block $end_term (result i32)
  (block $wrap
  (block $unit
  (block $union
  (block $type_application
  (block $type_lambda
  (block $maybe
  (block $annotated
  local.get $term
  br_table $annotated $maybe $type_lambda $type_application $union $unit $wrap $wrap
)
  local.get $at
  ;; project field: body
  local.set $term2
  i32.const 1
  i32.const 1
  ;; list elements follow
  local.get $term2
  br $end_term
)
  i32.const 1
  i32.const 1
  ;; list elements follow
  i32.const 0
  ;; list elements follow
  i32.const 1
  ;; list elements follow
  local.get $term2
  local.get $ot
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.get $abs
  ;; project field: body
  local.set $term2
  i32.const 1
  i32.const 1
  ;; list elements follow
  local.get $term2
  br $end_term
)
  local.get $ta
  ;; project field: body
  local.set $term2
  i32.const 1
  i32.const 1
  ;; list elements follow
  local.get $term2
  br $end_term
)
  local.get $inj
  ;; project field: type_name
  local.set $tname
  local.get $inj
  ;; project field: field
  local.set $field
  local.get $field
  ;; project field: name
  local.set $fname
  local.get $field
  ;; project field: term
  local.set $fterm
  local.get $ft
  ;; project field: name
  local.set $ftname
  local.get $fname
  local.get $ftname
  local.get $fname
  call $hydra.lib.equality.equal
  local.get $fterm
  i32.const 0
  call $hydra.lib.logic.if_else
  local.set $for_field_type
  local.get $ic
  ;; project field: object
  call $hydra.show.errors.error
  local.get $x
  local.get $cx
  local.get $graph
  local.get $tname
  call $hydra.resolution.require_union_type
  call $hydra.lib.eithers.bimap
  i32.const 1
  i32.const 1
  ;; list elements follow
  local.get $tname
  local.get $for_field_type
  local.get $rt
  call $hydra.lib.lists.map
  call $hydra.lib.eithers.bind
  br $end_term
)
  i32.const 1
  i32.const 1
  ;; list elements follow
  i32.const 1
  br $end_term
)
  local.get $wt
  ;; project field: body
  local.set $term2
  i32.const 1
  i32.const 1
  ;; list elements follow
  local.get $term2
  br $end_term
)
)
  (func $hydra.adapt.type_alternatives (param $type i32) (result i32)
  (local $at i32)
  (local $f i32)
  (local $opt_fields i32)
  (local $ot i32)
  (local $rt i32)
  (local $to_opt_field i32)
  (local $type2 i32)
  (block $end_type (result i32)
  (block $void
  (block $unit
  (block $union
  (block $maybe
  (block $annotated
  local.get $type
  br_table $annotated $maybe $union $unit $void $void
)
  local.get $at
  ;; project field: body
  local.set $type2
  i32.const 1
  ;; list elements follow
  local.get $type2
  br $end_type
)
  i32.const 1
  ;; list elements follow
  local.get $ot
  br $end_type
)
  local.get $f
  ;; project field: name
  local.get $f
  ;; project field: type
  local.set $to_opt_field
  local.get $to_opt_field
  local.get $rt
  call $hydra.lib.lists.map
  local.set $opt_fields
  i32.const 1
  ;; list elements follow
  local.get $opt_fields
  br $end_type
)
  i32.const 1
  ;; list elements follow
  i32.const 0
  br $end_type
)
  i32.const 1
  ;; list elements follow
  i32.const 0
  br $end_type
)
)
)
