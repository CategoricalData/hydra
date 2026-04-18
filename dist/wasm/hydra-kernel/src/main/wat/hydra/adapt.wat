(module
  (import "hydra.dependencies" "hydra.dependencies.lift_lambda_above_let" (func $hydra.dependencies.lift_lambda_above_let (param i32) (result i32) ) )
  (import "hydra.environment" "hydra.environment.graph_as_types" (func $hydra.environment.graph_as_types (param i32) (param i32) (result i32) ) )
  (import "hydra.environment" "hydra.environment.term_as_bindings" (func $hydra.environment.term_as_bindings (param i32) (result i32) ) )
  (import "hydra.environment" "hydra.environment.types_to_definitions" (func $hydra.environment.types_to_definitions (param i32) (result i32) ) )
  (import "hydra.hoisting" "hydra.hoisting.hoist_case_statements_in_graph" (func $hydra.hoisting.hoist_case_statements_in_graph (param i32) (result i32) ) )
  (import "hydra.hoisting" "hydra.hoisting.hoist_polymorphic_let_bindings" (func $hydra.hoisting.hoist_polymorphic_let_bindings (param i32) (param i32) (result i32) ) )
  (import "hydra.inference" "hydra.inference.infer_graph_types" (func $hydra.inference.infer_graph_types (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.build_graph" (func $hydra.lexical.build_graph (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.graph_to_bindings" (func $hydra.lexical.graph_to_bindings (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bimap" (func $hydra.lib.eithers.bimap (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat" (func $hydra.lib.lists.concat (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.filter" (func $hydra.lib.lists.filter (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.pure" (func $hydra.lib.lists.pure (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.uncons" (func $hydra.lib.lists.uncons (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigfloat_to_float64" (func $hydra.lib.literals.bigfloat_to_float64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_int32" (func $hydra.lib.literals.bigint_to_int32 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_int64" (func $hydra.lib.literals.bigint_to_int64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_int8" (func $hydra.lib.literals.bigint_to_int8 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.binary_to_string" (func $hydra.lib.literals.binary_to_string (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.decimal_to_float64" (func $hydra.lib.literals.decimal_to_float64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_decimal" (func $hydra.lib.literals.show_decimal (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.uint32_to_bigint" (func $hydra.lib.literals.uint32_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.uint64_to_bigint" (func $hydra.lib.literals.uint64_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.uint8_to_bigint" (func $hydra.lib.literals.uint8_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.insert" (func $hydra.lib.maps.insert (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.map" (func $hydra.lib.maps.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.null" (func $hydra.lib.maps.null (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.bind" (func $hydra.lib.maybes.bind (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cat" (func $hydra.lib.maybes.cat (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.is_just" (func $hydra.lib.maybes.is_just (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map_maybe" (func $hydra.lib.maybes.map_maybe (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.member" (func $hydra.lib.sets.member (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.intercalate" (func $hydra.lib.strings.intercalate (param i32) (param i32) (result i32) ) )
  (import "hydra.literals" "hydra.literals.bigfloat_to_float_value" (func $hydra.literals.bigfloat_to_float_value (param i32) (param i32) (result i32) ) )
  (import "hydra.literals" "hydra.literals.bigint_to_integer_value" (func $hydra.literals.bigint_to_integer_value (param i32) (param i32) (result i32) ) )
  (import "hydra.literals" "hydra.literals.float_value_to_bigfloat" (func $hydra.literals.float_value_to_bigfloat (param i32) (result i32) ) )
  (import "hydra.literals" "hydra.literals.integer_value_to_bigint" (func $hydra.literals.integer_value_to_bigint (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.namespace_of" (func $hydra.names.namespace_of (param i32) (result i32) ) )
  (import "hydra.reduction" "hydra.reduction.eta_expand_term" (func $hydra.reduction.eta_expand_term (param i32) (param i32) (result i32) ) )
  (import "hydra.reflect" "hydra.reflect.literal_type" (func $hydra.reflect.literal_type (param i32) (result i32) ) )
  (import "hydra.reflect" "hydra.reflect.literal_type_variant" (func $hydra.reflect.literal_type_variant (param i32) (result i32) ) )
  (import "hydra.reflect" "hydra.reflect.term_variant" (func $hydra.reflect.term_variant (param i32) (result i32) ) )
  (import "hydra.reflect" "hydra.reflect.type_variant" (func $hydra.reflect.type_variant (param i32) (result i32) ) )
  (import "hydra.resolution" "hydra.resolution.require_union_type" (func $hydra.resolution.require_union_type (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.resolution" "hydra.resolution.type_to_type_scheme" (func $hydra.resolution.type_to_type_scheme (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_term_m" (func $hydra.rewriting.rewrite_term_m (param i32) (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_type_m" (func $hydra.rewriting.rewrite_type_m (param i32) (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.type_scheme_to_f_type" (func $hydra.scoping.type_scheme_to_f_type (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.literal" (func $hydra.show.core.literal (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.type" (func $hydra.show.core.type (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_type" (func $hydra.strip.deannotate_type (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.strip_type_lambdas" (func $hydra.strip.strip_type_lambdas (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.unshadow_variables" (func $hydra.variables.unshadow_variables (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\03\00\00\00\29\3a\20\02\00\00\00\2c\20\08\00\00\00\30\3a\62\69\67\69\6e\74\08\00\00\00\31\3a\62\69\67\69\6e\74\18\00\00\00\46\6f\75\6e\64\20\75\6e\74\79\70\65\64\20\62\69\6e\64\69\6e\67\73\20\28\10\00\00\00\61\66\74\65\72\20\61\64\61\70\74\61\74\69\6f\6e\13\00\00\00\61\66\74\65\72\20\63\61\73\65\20\68\6f\69\73\74\69\6e\67\12\00\00\00\61\66\74\65\72\20\6c\65\74\20\68\6f\69\73\74\69\6e\67\1a\00\00\00\6e\6f\20\61\6c\74\65\72\6e\61\74\69\76\65\73\20\66\6f\72\20\74\65\72\6d\3a\20\1a\00\00\00\6e\6f\20\61\6c\74\65\72\6e\61\74\69\76\65\73\20\66\6f\72\20\74\79\70\65\3a\20\58\00\00\00\72\65\70\6c\61\63\65\20\61\72\62\69\74\72\61\72\79\2d\70\72\65\63\69\73\69\6f\6e\20\64\65\63\69\6d\61\6c\20\6e\75\6d\62\65\72\73\20\77\69\74\68\20\36\34\2d\62\69\74\20\66\6c\6f\61\74\69\6e\67\2d\70\6f\69\6e\74\20\6e\75\6d\62\65\72\73\20\28\64\6f\75\62\6c\65\73\29\5f\00\00\00\72\65\70\6c\61\63\65\20\61\72\62\69\74\72\61\72\79\2d\70\72\65\63\69\73\69\6f\6e\20\66\6c\6f\61\74\69\6e\67\2d\70\6f\69\6e\74\20\6e\75\6d\62\65\72\73\20\77\69\74\68\20\36\34\2d\62\69\74\20\66\6c\6f\61\74\69\6e\67\2d\70\6f\69\6e\74\20\6e\75\6d\62\65\72\73\20\28\64\6f\75\62\6c\65\73\29\39\00\00\00\72\65\70\6c\61\63\65\20\61\72\62\69\74\72\61\72\79\2d\70\72\65\63\69\73\69\6f\6e\20\69\6e\74\65\67\65\72\73\20\77\69\74\68\20\36\34\2d\62\69\74\20\69\6e\74\65\67\65\72\73\2d\00\00\00\72\65\70\6c\61\63\65\20\62\69\6e\61\72\79\20\73\74\72\69\6e\67\73\20\77\69\74\68\20\63\68\61\72\61\63\74\65\72\20\73\74\72\69\6e\67\73\3c\00\00\00\72\65\70\6c\61\63\65\20\75\6e\73\69\67\6e\65\64\20\33\32\2d\62\69\74\20\69\6e\74\65\67\65\72\73\20\77\69\74\68\20\73\69\67\6e\65\64\20\33\32\2d\62\69\74\20\69\6e\74\65\67\65\72\73\3c\00\00\00\72\65\70\6c\61\63\65\20\75\6e\73\69\67\6e\65\64\20\36\34\2d\62\69\74\20\69\6e\74\65\67\65\72\73\20\77\69\74\68\20\73\69\67\6e\65\64\20\36\34\2d\62\69\74\20\69\6e\74\65\67\65\72\73\3a\00\00\00\72\65\70\6c\61\63\65\20\75\6e\73\69\67\6e\65\64\20\38\2d\62\69\74\20\69\6e\74\65\67\65\72\73\20\77\69\74\68\20\73\69\67\6e\65\64\20\38\2d\62\69\74\20\69\6e\74\65\67\65\72\73")
  (global $__bump_ptr (mut i32) i32.const 1712 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
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
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $adapt_binding i32)
  (local $adapted i32)
  (local $adapted_graph i32)
  (local $adapted_graph_raw i32)
  (local $adapted_schema_types i32)
  (local $adapted_terms i32)
  (local $adapted_type i32)
  (local $e i32)
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
  i32.load
  local.get $g
  drop
  local.get $el
  i32.load offset=4
  drop
  local.get $transform_term
  drop
  i32.const 0
  local.get $el
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
  local.set $transform_binding
  local.get $constraints
  call $hydra.adapt.adapt_literal_types_map
  local.set $litmap
  local.get $graph0
  i32.load offset=20
  local.set $prims0
  local.get $graph0
  i32.load offset=24
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
  i32.const 1
  local.get $e
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
  local.get $x
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.logic.if_else
  local.get $schema_result
  local.set $adapted_schema_types
  local.get $graph0
  drop
  local.get $el
  drop
  local.get $transform_binding
  drop
  i32.const 0
  local.set $transformed
  i32.const 6
  local.get $transformed
  call $hydra.lib.lists.pure
  i32.const 17
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
  local.set $wrapped
  local.get $constraints
  local.get $litmap
  local.get $cx
  local.get $graph0
  local.get $wrapped
  call $hydra.adapt.adapt_term
  local.get $constraints
  local.get $litmap
  i32.const 0
  i32.const 0
  call $hydra.adapt.adapt_lambda_domains
  local.get $adapted
  call $hydra.rewriting.rewrite_term_m
  call $hydra.lib.eithers.bind
  local.set $adapt_binding
  local.get $adapt_binding
  local.get $els0
  call $hydra.lib.eithers.map_list
  i32.const 0
  local.get $adapted_terms
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  local.set $els1_raw
  local.get $constraints
  local.get $litmap
  i32.const 0
  i32.const 0
  call $hydra.adapt.adapt_nested_types
  local.get $el
  i32.load offset=4
  call $hydra.rewriting.rewrite_term_m
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
  local.get $constraints
  local.get $litmap
  local.get $ts
  call $hydra.adapt.adapt_type_scheme
  i32.const 1
  local.get $ts1
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
  local.get $el
  i32.load offset=8
  call $hydra.lib.maybes.maybe
  i32.const 1
  local.get $el
  i32.load
  local.get $new_term
  local.get $adapted_type
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
  local.get $prims0
  call $hydra.lib.maps.to_list
  call $hydra.lib.eithers.map_list
  local.get $prim_pairs
  call $hydra.lib.maps.from_list
  local.set $prims1
  local.get $els1
  i32.const 0
  local.get $prims1
  call $hydra.lexical.build_graph
  local.set $adapted_graph_raw
  local.get $adapted_graph_raw
  i32.load
  local.get $adapted_graph_raw
  i32.load offset=4
  local.get $adapted_graph_raw
  i32.load offset=8
  local.get $adapted_graph_raw
  i32.load offset=12
  local.get $adapted_graph_raw
  i32.load offset=16
  local.get $adapted_graph_raw
  i32.load offset=20
  local.get $adapted_schema_types
  local.get $adapted_graph_raw
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
  local.set $adapted_graph
  i32.const 1
  local.get $adapted_graph
  local.get $els1
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.adapt.adapt_float_type (param $constraints i32) (param $ft i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $alt i32)
  (local $for_unsupported i32)
  (local $ft2 i32)
  (local $supported i32)
  (local $v i32)
  local.get $ft
  local.get $constraints
  i32.load offset=8
  call $hydra.lib.sets.member
  local.set $supported
  local.get $constraints
  i32.const 0
  call $hydra.adapt.adapt_float_type
  local.set $alt
  (block $end_float_type (result i32)
  (block $float64
  (block $float32
  (block $bigfloat
  local.get $ft2
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $bigfloat $float32 $float64 $float64
)
  local.get $v
  drop
  i32.const 2
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
  drop
  local.get $alt
  drop
  i32.const 0
  br $end_float_type
)
  local.get $v
  drop
  i32.const 2
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
  drop
  local.get $alt
  drop
  i32.const 0
  br $end_float_type
)
  local.get $v
  drop
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
  drop
  local.get $alt
  drop
  i32.const 0
  br $end_float_type
)
  local.set $for_unsupported
  local.get $supported
  local.get $ft
  local.get $ft
  drop
  local.get $for_unsupported
  drop
  i32.const 0
  call $hydra.lib.logic.if_else
)
  (func $hydra.adapt.adapt_graph_schema (param $constraints i32) (param $litmap i32) (param $types0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
  local.set $map_pair
  local.get $map_pair
  local.get $types0
  call $hydra.lib.maps.to_list
  call $hydra.lib.eithers.map_list
  i32.const 1
  local.get $pairs
  call $hydra.lib.maps.from_list
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
  (func $hydra.adapt.adapt_integer_type (param $constraints i32) (param $it i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $alt i32)
  (local $for_unsupported i32)
  (local $it2 i32)
  (local $supported i32)
  (local $v i32)
  local.get $it
  local.get $constraints
  i32.load offset=16
  call $hydra.lib.sets.member
  local.set $supported
  local.get $constraints
  i32.const 0
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
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $bigint $int8 $int16 $int32 $int64 $uint8 $uint16 $uint32 $uint64 $uint64
)
  local.get $v
  drop
  i32.const 0
  br $end_integer_type
)
  local.get $v
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
  drop
  local.get $alt
  drop
  i32.const 0
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 7
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
  drop
  local.get $alt
  drop
  i32.const 0
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 8
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
  drop
  local.get $alt
  drop
  i32.const 0
  br $end_integer_type
)
  local.get $v
  drop
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
  drop
  local.get $alt
  drop
  i32.const 0
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 2
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
  drop
  local.get $alt
  drop
  i32.const 0
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 3
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
  drop
  local.get $alt
  drop
  i32.const 0
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 4
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
  drop
  local.get $alt
  drop
  i32.const 0
  br $end_integer_type
)
  local.get $v
  drop
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
  drop
  local.get $alt
  drop
  i32.const 0
  br $end_integer_type
)
  local.set $for_unsupported
  local.get $supported
  local.get $it
  local.get $it
  drop
  local.get $for_unsupported
  drop
  i32.const 0
  call $hydra.lib.logic.if_else
)
  (func $hydra.adapt.adapt_lambda_domains (param $constraints i32) (param $litmap i32) (param $recurse i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $adapted_domain i32)
  (local $dom i32)
  (local $dom1 i32)
  (local $l i32)
  (local $rewritten i32)
  (local $v i32)
  local.get $term
  drop
  local.get $recurse
  drop
  i32.const 0
  (block $end_term (result i32)
  (block $lambda
  local.get $rewritten
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $lambda $lambda
)
  local.get $v
  drop
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
  local.get $constraints
  local.get $litmap
  local.get $dom
  call $hydra.adapt.adapt_type
  i32.const 1
  local.get $dom1
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
  local.get $l
  i32.load offset=4
  call $hydra.lib.maybes.maybe
  i32.const 1
  i32.const 5
  local.get $l
  i32.load
  local.get $adapted_domain
  local.get $l
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
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.adapt.adapt_literal (param $lt i32) (param $l i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (local $d i32)
  (local $f i32)
  (local $ft i32)
  (local $i i32)
  (local $it i32)
  (local $v i32)
  (block $end_literal (result i32)
  (block $integer
  (block $float
  (block $decimal
  (block $boolean
  (block $binary
  local.get $l
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $binary $boolean $decimal $float $integer $integer
)
  local.get $v
  drop
  (block $end_literal_type (result i32)
  (block $string
  local.get $lt
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $string $string
)
  local.get $v
  drop
  i32.const 5
  local.get $b
  call $hydra.lib.literals.binary_to_string
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
  br $end_literal_type
)
  br $end_literal
)
  local.get $v
  drop
  (block $end_literal_type (result i32)
  (block $integer
  local.get $lt
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $integer $integer
)
  local.get $v
  drop
  i32.const 4
  local.get $it
  local.get $b
  i32.const 1049
  i32.const 1037
  call $hydra.lib.logic.if_else
  call $hydra.literals.bigint_to_integer_value
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
  br $end_literal_type
)
  br $end_literal
)
  local.get $v
  drop
  (block $end_literal_type (result i32)
  (block $string
  (block $float
  local.get $lt
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $float $string $string
)
  local.get $v
  drop
  i32.const 3
  i32.const 2
  local.get $d
  call $hydra.lib.literals.decimal_to_float64
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
  br $end_literal_type
)
  local.get $v
  drop
  i32.const 5
  local.get $d
  call $hydra.lib.literals.show_decimal
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
  br $end_literal_type
)
  br $end_literal
)
  local.get $v
  drop
  (block $end_literal_type (result i32)
  (block $float
  local.get $lt
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $float $float
)
  local.get $v
  drop
  i32.const 3
  local.get $ft
  local.get $f
  call $hydra.literals.float_value_to_bigfloat
  call $hydra.literals.bigfloat_to_float_value
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
  br $end_literal_type
)
  br $end_literal
)
  local.get $v
  drop
  (block $end_literal_type (result i32)
  (block $integer
  local.get $lt
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $integer $integer
)
  local.get $v
  drop
  i32.const 4
  local.get $it
  local.get $i
  call $hydra.literals.integer_value_to_bigint
  call $hydra.literals.bigint_to_integer_value
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
  br $end_literal_type
)
  br $end_literal
)
)
  (func $hydra.adapt.adapt_literal_type (param $constraints i32) (param $lt i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $for_unsupported i32)
  (local $ft i32)
  (local $it i32)
  (local $lt2 i32)
  (local $v i32)
  (local $x i32)
  (block $end_literal_type (result i32)
  (block $integer
  (block $float
  (block $decimal
  (block $boolean
  (block $binary
  local.get $lt2
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $binary $boolean $decimal $float $integer $integer
)
  local.get $v
  drop
  i32.const 5
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
  br $end_literal_type
)
  local.get $v
  drop
  i32.const 4
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
  local.get $constraints
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
  call $hydra.adapt.adapt_integer_type
  call $hydra.lib.maybes.map
  br $end_literal_type
)
  local.get $v
  drop
  i32.const 3
  i32.const 2
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
  br $end_literal_type
)
  local.get $v
  drop
  i32.const 3
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
  local.get $constraints
  local.get $ft
  call $hydra.adapt.adapt_float_type
  call $hydra.lib.maybes.map
  br $end_literal_type
)
  local.get $v
  drop
  i32.const 4
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
  drop
  local.get $for_unsupported
  drop
  i32.const 0
  call $hydra.lib.logic.if_else
)
  (func $hydra.adapt.adapt_literal_types_map (param $constraints i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $lt i32)
  (local $lt2 i32)
  (local $try_type i32)
  i32.const 0
  local.get $lt
  local.get $lt2
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
  local.get $constraints
  local.get $lt
  call $hydra.adapt.adapt_literal_type
  call $hydra.lib.maybes.maybe
  local.set $try_type
  local.get $try_type
  i32.const 0
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  call $hydra.lib.maps.from_list
)
  (func $hydra.adapt.adapt_literal_value (param $litmap i32) (param $lt i32) (param $l i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $lt2 i32)
  i32.const 5
  local.get $l
  call $hydra.show.core.literal
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
  local.get $lt2
  local.get $l
  call $hydra.adapt.adapt_literal
  local.get $lt
  local.get $litmap
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
)
  (func $hydra.adapt.adapt_nested_types (param $constraints i32) (param $litmap i32) (param $recurse i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $adapt_b i32)
  (local $adapted_b_type i32)
  (local $adapted_bindings i32)
  (local $b i32)
  (local $lt i32)
  (local $rewritten i32)
  (local $ts i32)
  (local $ts1 i32)
  (local $v i32)
  local.get $term
  drop
  local.get $recurse
  drop
  i32.const 0
  (block $end_term (result i32)
  (block $let
  local.get $rewritten
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
  local.get $constraints
  local.get $litmap
  local.get $ts
  call $hydra.adapt.adapt_type_scheme
  i32.const 1
  local.get $ts1
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
  local.get $b
  i32.load offset=8
  call $hydra.lib.maybes.maybe
  i32.const 1
  local.get $b
  i32.load
  local.get $b
  i32.load offset=4
  local.get $adapted_b_type
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
  call $hydra.lib.eithers.bind
  local.set $adapt_b
  local.get $adapt_b
  local.get $lt
  i32.load
  call $hydra.lib.eithers.map_list
  i32.const 1
  i32.const 6
  local.get $adapted_bindings
  local.get $lt
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
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.adapt.adapt_primitive (param $constraints i32) (param $litmap i32) (param $prim0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $ts0 i32)
  (local $ts1 i32)
  local.get $prim0
  i32.load offset=4
  local.set $ts0
  local.get $constraints
  local.get $litmap
  local.get $ts0
  call $hydra.adapt.adapt_type_scheme
  i32.const 1
  local.get $prim0
  i32.load
  local.get $ts1
  local.get $prim0
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
  (func $hydra.adapt.adapt_term (param $constraints i32) (param $litmap i32) (param $cx i32) (param $graph i32) (param $term0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $alts i32)
  (local $alts0 i32)
  (local $atyp i32)
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
  (local $uc i32)
  (local $v i32)
  (block $end_term (result i32)
  (block $literal
  local.get $term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $literal $literal
)
  local.get $v
  drop
  local.get $l
  call $hydra.reflect.literal_type
  local.set $lt
  i32.const 1
  local.get $constraints
  local.get $lt
  call $hydra.adapt.literal_type_supported
  local.get $term
  i32.const 8
  local.get $litmap
  local.get $lt
  local.get $l
  call $hydra.adapt.adapt_literal_value
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
  br $end_term
)
  local.set $for_supported
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
  local.get $uc
  call $hydra.lib.pairs.first
  drop
  local.get $try_term
  drop
  i32.const 0
  local.get $uc
  call $hydra.lib.pairs.second
  drop
  local.get $try_alts
  drop
  i32.const 0
  i32.const 1
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
  local.get $mterm
  call $hydra.lib.maybes.maybe
  call $hydra.lib.eithers.bind
  local.get $alts
  call $hydra.lib.lists.uncons
  call $hydra.lib.maybes.maybe
  local.set $try_alts
  local.get $cx
  local.get $graph
  local.get $term
  call $hydra.adapt.term_alternatives
  local.get $alts0
  drop
  local.get $try_alts
  drop
  i32.const 0
  call $hydra.lib.eithers.bind
  local.set $for_unsupported
  local.get $term
  call $hydra.reflect.term_variant
  local.get $constraints
  i32.load offset=20
  call $hydra.lib.sets.member
  local.set $supported_variant
  local.get $supported_variant
  local.get $term
  drop
  local.get $for_supported
  drop
  i32.const 0
  local.get $term
  drop
  local.get $for_unsupported
  drop
  i32.const 0
  call $hydra.lib.logic.if_else
  local.set $try_term
  local.get $term02
  drop
  local.get $recurse
  drop
  i32.const 0
  (block $end_term (result i32)
  (block $type_lambda
  (block $type_application
  local.get $term1
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $type_application $type_lambda $type_lambda
)
  local.get $v
  drop
  local.get $constraints
  local.get $litmap
  local.get $ta
  i32.load offset=4
  call $hydra.adapt.adapt_type
  i32.const 1
  i32.const 15
  local.get $ta
  i32.load
  local.get $atyp
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
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $v
  drop
  i32.const 1
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
  i32.load offset=4
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
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
  (local $uc i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $literal
  local.get $typ
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $literal $literal
)
  local.get $v
  drop
  local.get $constraints
  local.get $lt
  call $hydra.adapt.literal_type_supported
  local.get $typ
  i32.const 6
  i32.const 5
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
  i32.const 6
  local.get $lt2
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
  local.get $lt
  local.get $litmap
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  call $hydra.lib.logic.if_else
  br $end_type
)
  local.set $for_supported
  local.get $alts
  call $hydra.lib.lists.uncons
  local.get $uc
  call $hydra.lib.pairs.second
  drop
  local.get $try_alts
  drop
  i32.const 0
  local.get $t
  local.get $uc
  call $hydra.lib.pairs.first
  drop
  local.get $try_type
  drop
  i32.const 0
  call $hydra.lib.maybes.maybe
  call $hydra.lib.maybes.bind
  local.set $try_alts
  local.get $typ
  call $hydra.adapt.type_alternatives
  local.set $alts0
  local.get $alts0
  drop
  local.get $try_alts
  drop
  i32.const 0
  local.set $for_unsupported
  local.get $typ
  call $hydra.reflect.type_variant
  local.get $constraints
  i32.load offset=24
  call $hydra.lib.sets.member
  local.set $supported_variant
  local.get $supported_variant
  local.get $typ
  drop
  local.get $for_supported
  drop
  i32.const 0
  local.get $typ
  drop
  local.get $for_unsupported
  drop
  i32.const 0
  call $hydra.lib.logic.if_else
  local.set $try_type
  local.get $typ
  drop
  local.get $recurse
  drop
  i32.const 0
  i32.const 0
  i32.const 6
  i32.const 1184
  local.get $typ
  call $hydra.show.core.type
  call $hydra.lib.strings.cat2
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
  local.get $type2
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
  local.get $type1
  drop
  local.get $try_type
  drop
  i32.const 0
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
  i32.load offset=4
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
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $t0 i32)
  (local $t1 i32)
  (local $vars0 i32)
  local.get $ts0
  i32.load
  local.set $vars0
  local.get $ts0
  i32.load offset=4
  local.set $t0
  local.get $constraints
  local.get $litmap
  local.get $t0
  call $hydra.adapt.adapt_type
  i32.const 1
  local.get $vars0
  local.get $t1
  local.get $ts0
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
  (func $hydra.adapt.compose_coders (param $c1 i32) (param $c2 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  local.get $c1
  i32.load
  local.get $c2
  i32.load
  call $hydra.lib.eithers.bind
  local.get $c2
  i32.load offset=4
  local.get $c1
  i32.load offset=4
  call $hydra.lib.eithers.bind
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
  (func $hydra.adapt.data_graph_to_definitions (param $constraints i32) (param $do_infer i32) (param $do_expand i32) (param $do_hoist_case_statements i32) (param $do_hoist_polymorphic_let_bindings i32) (param $original_bindings i32) (param $graph0 i32) (param $namespaces i32) (param $cx i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
  local.get $namespaces
  call $hydra.lib.sets.from_list
  local.set $namespaces_set
  i32.const 0
  local.get $ns
  local.get $namespaces_set
  call $hydra.lib.sets.member
  local.get $b
  i32.load
  call $hydra.names.namespace_of
  call $hydra.lib.maybes.maybe
  local.set $is_parent_binding
  local.get $b
  i32.load
  local.get $b
  i32.load offset=4
  call $hydra.strip.strip_type_lambdas
  local.get $b
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
  local.get $bindings
  call $hydra.lib.lists.map
  local.set $stripped
  i32.const 6
  local.get $stripped
  i32.const 17
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
  local.set $term0
  local.get $term0
  call $hydra.variables.unshadow_variables
  call $hydra.environment.term_as_bindings
  local.set $unshadowed0
  local.get $unshadowed0
  call $hydra.hoisting.hoist_case_statements_in_graph
  local.set $hoisted
  i32.const 6
  local.get $hoisted
  i32.const 17
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
  local.set $term1
  local.get $term1
  call $hydra.variables.unshadow_variables
  call $hydra.environment.term_as_bindings
  local.set $hoist_cases
  local.get $bindings
  i32.const 17
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
  local.set $let_before
  local.get $is_parent_binding
  local.get $let_before
  call $hydra.hoisting.hoist_polymorphic_let_bindings
  local.set $let_after
  local.get $let_after
  i32.load
  local.set $hoist_poly
  local.get $b
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  local.get $b
  i32.load offset=8
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
  i32.const 6
  i32.const 1061
  local.get $debug_label
  i32.const 1024
  i32.const 1031
  local.get $untyped_bindings
  call $hydra.lib.strings.intercalate
  i32.const 20
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 4
  i32.store
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
  local.get $__rec_ptr
  call $hydra.lib.strings.cat
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
  local.set $check_bindings_typed
  local.get $b
  i32.load
  local.get $b
  i32.load offset=4
  call $hydra.adapt.push_type_apps_inward
  local.get $b
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
  local.get $bindings
  call $hydra.lib.lists.map
  local.set $normalize_bindings
  local.get $bindings
  i32.const 0
  local.get $graph0
  i32.load offset=20
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
  local.get $graph0
  i32.load offset=24
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
  local.set $rebuild_graph
  local.get $original_bindings
  local.set $bins0
  local.get $do_hoist_case_statements
  local.get $bins0
  drop
  local.get $hoist_cases
  drop
  i32.const 0
  local.get $bins0
  call $hydra.lib.logic.if_else
  local.set $bins1
  local.get $do_infer
  local.get $result
  call $hydra.lib.pairs.first
  call $hydra.lib.pairs.second
  local.get $cx
  local.get $bins1
  local.get $bins1
  drop
  local.get $rebuild_graph
  drop
  i32.const 0
  call $hydra.inference.infer_graph_types
  call $hydra.lib.eithers.map
  i32.const 1109
  drop
  local.get $bins1
  drop
  local.get $check_bindings_typed
  drop
  i32.const 0
  call $hydra.lib.logic.if_else
  local.get $do_hoist_polymorphic_let_bindings
  i32.const 1132
  drop
  local.get $bins2
  drop
  local.get $hoist_poly
  drop
  i32.const 0
  drop
  local.get $check_bindings_typed
  drop
  i32.const 0
  i32.const 1
  local.get $bins2
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
  local.get $constraints
  local.get $do_expand
  local.get $bins3
  local.get $cx
  local.get $bins3
  drop
  local.get $rebuild_graph
  drop
  i32.const 0
  call $hydra.adapt.adapt_data_graph
  local.get $adapt_result
  call $hydra.lib.pairs.first
  local.set $adapted
  local.get $adapt_result
  call $hydra.lib.pairs.second
  local.set $adapted_bindings
  i32.const 1089
  drop
  local.get $adapted_bindings
  drop
  local.get $check_bindings_typed
  drop
  i32.const 0
  local.get $bins4
  drop
  local.get $normalize_bindings
  drop
  i32.const 0
  local.set $bins5
  local.get $el
  i32.load
  local.get $el
  i32.load offset=4
  local.get $ts
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
  local.get $el
  i32.load offset=8
  call $hydra.lib.maybes.map
  local.set $to_def
  i32.const 0
  local.get $ns
  local.get $namespaces_set
  call $hydra.lib.sets.member
  local.get $el
  i32.load
  call $hydra.names.namespace_of
  call $hydra.lib.maybes.maybe
  local.get $bins5
  call $hydra.lib.lists.filter
  local.set $selected_elements
  local.get $acc
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  i32.const 0
  local.get $ns
  local.get $acc
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  local.set $existing
  local.get $ns
  local.get $existing
  local.get $el
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
  local.get $acc
  call $hydra.lib.maps.insert
  local.get $el
  i32.load
  call $hydra.names.namespace_of
  call $hydra.lib.maybes.maybe
  i32.const 0
  local.get $selected_elements
  call $hydra.lib.lists.foldl
  local.set $elements_by_namespace
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  i32.const 0
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
  i32.const 0
  local.get $adapted
  i32.load offset=20
  call $hydra.lexical.build_graph
  local.set $g
  i32.const 1
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
  local.get $adapted
  i32.load offset=24
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
  local.get $defs_grouped
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.adapt.literal_type_supported (param $constraints i32) (param $lt i32) (result i32)
  (local $__rec_ptr i32)
  (local $for_type i32)
  (local $ft i32)
  (local $it i32)
  (local $lt2 i32)
  (local $v i32)
  (block $end_literal_type (result i32)
  (block $integer
  (block $float
  local.get $lt2
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $float $integer $integer
)
  local.get $v
  drop
  local.get $ft
  local.get $constraints
  i32.load offset=8
  call $hydra.lib.sets.member
  br $end_literal_type
)
  local.get $v
  drop
  local.get $it
  local.get $constraints
  i32.load offset=16
  call $hydra.lib.sets.member
  br $end_literal_type
)
  local.set $for_type
  local.get $lt
  call $hydra.reflect.literal_type_variant
  local.get $constraints
  i32.load offset=4
  call $hydra.lib.sets.member
  local.get $lt
  drop
  local.get $for_type
  drop
  i32.const 0
  i32.const 0
  call $hydra.lib.logic.if_else
)
  (func $hydra.adapt.prepare_float_type (param $ft i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $d i32)
  (local $v i32)
  (block $end_float_type (result i32)
  (block $bigfloat
  local.get $ft
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $bigfloat $bigfloat
)
  local.get $v
  drop
  i32.const 2
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
  (block $end_float_value (result i32)
  (block $bigfloat
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $bigfloat $bigfloat
)
  local.get $v
  drop
  i32.const 2
  local.get $d
  call $hydra.lib.literals.bigfloat_to_float64
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
  br $end_float_value
)
  i32.const 1306
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
  call $hydra.lib.sets.from_list
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
  br $end_float_type
)
)
  (func $hydra.adapt.prepare_integer_type (param $it i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $i i32)
  (local $v i32)
  (block $end_integer_type (result i32)
  (block $uint64
  (block $uint32
  (block $uint8
  (block $bigint
  local.get $it
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $bigint $uint8 $uint32 $uint64 $uint64
)
  local.get $v
  drop
  i32.const 4
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
  (block $end_integer_value (result i32)
  (block $bigint
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $bigint $bigint
)
  local.get $v
  drop
  i32.const 4
  local.get $i
  call $hydra.lib.literals.bigint_to_int64
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
  br $end_integer_value
)
  i32.const 1405
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
  call $hydra.lib.sets.from_list
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
  br $end_integer_type
)
  local.get $v
  drop
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
  (block $end_integer_value (result i32)
  (block $uint8
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $uint8 $uint8
)
  local.get $v
  drop
  i32.const 1
  local.get $i
  call $hydra.lib.literals.uint8_to_bigint
  call $hydra.lib.literals.bigint_to_int8
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
  br $end_integer_value
)
  i32.const 1643
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
  call $hydra.lib.sets.from_list
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
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 3
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
  (block $end_integer_value (result i32)
  (block $uint32
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $uint32 $uint32
)
  local.get $v
  drop
  i32.const 3
  local.get $i
  call $hydra.lib.literals.uint32_to_bigint
  call $hydra.lib.literals.bigint_to_int32
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
  br $end_integer_value
)
  i32.const 1515
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
  call $hydra.lib.sets.from_list
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
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 4
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
  (block $end_integer_value (result i32)
  (block $uint64
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $uint64 $uint64
)
  local.get $v
  drop
  i32.const 4
  local.get $i
  call $hydra.lib.literals.uint64_to_bigint
  call $hydra.lib.literals.bigint_to_int64
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
  br $end_integer_value
)
  i32.const 1579
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
  call $hydra.lib.sets.from_list
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
  br $end_integer_type
)
)
  (func $hydra.adapt.prepare_literal_type (param $at i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (local $d i32)
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
  (block $decimal
  (block $binary
  local.get $at
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $binary $decimal $float $integer $integer
)
  local.get $v
  drop
  i32.const 5
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
  (block $end_literal (result i32)
  (block $binary
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $binary $binary
)
  local.get $v
  drop
  i32.const 5
  local.get $b
  call $hydra.lib.literals.binary_to_string
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
  br $end_literal
)
  i32.const 1466
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
  call $hydra.lib.sets.from_list
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
  br $end_literal_type
)
  local.get $v
  drop
  i32.const 3
  i32.const 2
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
  (block $end_literal (result i32)
  (block $decimal
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $decimal $decimal
)
  local.get $v
  drop
  i32.const 3
  i32.const 2
  local.get $d
  call $hydra.lib.literals.decimal_to_float64
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
  br $end_literal
)
  i32.const 1214
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
  call $hydra.lib.sets.from_list
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
  br $end_literal_type
)
  local.get $v
  drop
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
  i32.const 3
  local.get $rtyp
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
  (block $end_literal (result i32)
  (block $float
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $float $float
)
  local.get $v
  drop
  i32.const 3
  local.get $fv
  drop
  local.get $rep
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
  br $end_literal
)
  local.get $msgs
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
  br $end_literal_type
)
  local.get $v
  drop
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
  i32.const 4
  local.get $rtyp
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
  (block $end_literal (result i32)
  (block $integer
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $integer $integer
)
  local.get $v
  drop
  i32.const 4
  local.get $iv
  drop
  local.get $rep
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
  br $end_literal
)
  local.get $msgs
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
  br $end_literal_type
)
)
  (func $hydra.adapt.prepare_same (param $x i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $y i32)
  local.get $x
  local.get $y
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
  (func $hydra.adapt.prepare_type (param $cx i32) (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $literal $literal
)
  local.get $v
  drop
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
  i32.const 6
  local.get $rtyp
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
  (block $end_term (result i32)
  (block $literal
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $literal $literal
)
  local.get $v
  drop
  i32.const 8
  local.get $av
  drop
  local.get $rep
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
  br $end_term
)
  local.get $msgs
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
  br $end_type
)
)
  (func $hydra.adapt.push_type_apps_inward (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $a i32)
  (local $at i32)
  (local $b i32)
  (local $body i32)
  (local $body1 i32)
  (local $cs i32)
  (local $e i32)
  (local $els i32)
  (local $fld i32)
  (local $for_field i32)
  (local $for_let i32)
  (local $for_map i32)
  (local $for_pair i32)
  (local $go i32)
  (local $i i32)
  (local $l i32)
  (local $lt i32)
  (local $m i32)
  (local $map_binding i32)
  (local $n i32)
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
  (block $lambda
  (block $application
  local.get $body
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $application $lambda $let $let
)
  local.get $v
  drop
  i32.const 1
  i32.const 15
  local.get $a
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
  local.get $a
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
  drop
  local.get $go
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 5
  local.get $l
  i32.load
  local.get $l
  i32.load offset=4
  i32.const 15
  local.get $l
  i32.load offset=8
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
  drop
  local.get $go
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 6
  local.get $lt
  i32.load
  i32.const 15
  local.get $lt
  i32.load offset=4
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
  drop
  local.get $go
  drop
  i32.const 0
  br $end_term
)
  local.set $push
  local.get $fld
  i32.load
  local.get $fld
  i32.load offset=4
  drop
  local.get $go
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
  local.set $for_field
  local.get $b
  i32.load
  local.get $b
  i32.load offset=4
  drop
  local.get $go
  drop
  i32.const 0
  local.get $b
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
  local.set $map_binding
  local.get $map_binding
  local.get $lt
  i32.load
  call $hydra.lib.lists.map
  local.get $lt
  i32.load offset=4
  drop
  local.get $go
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
  local.set $for_let
  local.get $p
  call $hydra.lib.pairs.first
  drop
  local.get $go
  drop
  i32.const 0
  local.get $p
  call $hydra.lib.pairs.second
  drop
  local.get $go
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
  (block $unwrap
  (block $unit
  (block $inject
  (block $type_lambda
  (block $type_application
  (block $set
  (block $record
  (block $project
  (block $pair
  (block $maybe
  (block $map
  (block $literal
  (block $list
  (block $let
  (block $lambda
  (block $either
  (block $cases
  (block $application
  (block $annotated
  local.get $t
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $application $cases $either $lambda $let $list $literal $map $maybe $pair $project $record $set $type_application $type_lambda $inject $unit $unwrap $variable $wrap $wrap
)
  local.get $v
  drop
  i32.const 0
  local.get $at
  i32.load
  drop
  local.get $go
  drop
  i32.const 0
  local.get $at
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
  br $end_term
)
  local.get $v
  drop
  i32.const 1
  local.get $a
  i32.load
  drop
  local.get $go
  drop
  i32.const 0
  local.get $a
  i32.load offset=4
  drop
  local.get $go
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
  br $end_term
)
  local.get $v
  drop
  i32.const 2
  local.get $cs
  i32.load
  local.get $go
  local.get $cs
  i32.load offset=4
  call $hydra.lib.maybes.map
  local.get $for_field
  local.get $cs
  i32.load offset=8
  call $hydra.lib.lists.map
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
  br $end_term
)
  local.get $v
  drop
  i32.const 3
  i32.const 0
  local.get $l
  drop
  local.get $go
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
  i32.const 1
  local.get $r
  drop
  local.get $go
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
  local.get $e
  call $hydra.lib.eithers.either
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
  br $end_term
)
  local.get $v
  drop
  i32.const 5
  local.get $l
  i32.load
  local.get $l
  i32.load offset=4
  local.get $l
  i32.load offset=8
  drop
  local.get $go
  drop
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
  br $end_term
)
  local.get $v
  drop
  i32.const 6
  local.get $lt
  drop
  local.get $for_let
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
  br $end_term
)
  local.get $v
  drop
  i32.const 7
  local.get $go
  local.get $els
  call $hydra.lib.lists.map
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
  br $end_term
)
  local.get $v
  drop
  i32.const 8
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
  br $end_term
)
  local.get $v
  drop
  i32.const 9
  local.get $m
  drop
  local.get $for_map
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
  br $end_term
)
  local.get $v
  drop
  i32.const 10
  local.get $go
  local.get $m
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
  br $end_term
)
  local.get $v
  drop
  i32.const 11
  local.get $p
  call $hydra.lib.pairs.first
  drop
  local.get $go
  drop
  i32.const 0
  local.get $p
  call $hydra.lib.pairs.second
  drop
  local.get $go
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
  br $end_term
)
  local.get $v
  drop
  i32.const 12
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
  br $end_term
)
  local.get $v
  drop
  i32.const 13
  local.get $r
  i32.load
  local.get $for_field
  local.get $r
  i32.load offset=4
  call $hydra.lib.lists.map
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
  br $end_term
)
  local.get $v
  drop
  i32.const 14
  local.get $go
  local.get $s
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.sets.from_list
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
  br $end_term
)
  local.get $v
  drop
  local.get $tt
  i32.load
  drop
  local.get $go
  drop
  i32.const 0
  local.set $body1
  local.get $body1
  drop
  local.get $tt
  i32.load offset=4
  drop
  local.get $push
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 16
  local.get $ta
  i32.load
  local.get $ta
  i32.load offset=4
  drop
  local.get $go
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
  br $end_term
)
  local.get $v
  drop
  i32.const 4
  local.get $i
  i32.load
  local.get $i
  i32.load offset=4
  drop
  local.get $for_field
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
  br $end_term
)
  local.get $v
  drop
  i32.const 17
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
  br $end_term
)
  local.get $v
  drop
  i32.const 18
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
  br $end_term
)
  local.get $v
  drop
  i32.const 19
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
  br $end_term
)
  local.get $v
  drop
  i32.const 20
  local.get $wt
  i32.load
  local.get $wt
  i32.load offset=4
  drop
  local.get $go
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
  br $end_term
)
  local.set $go
  local.get $term
  drop
  local.get $go
  drop
  i32.const 0
)
  (func $hydra.adapt.schema_graph_to_definitions (param $constraints i32) (param $graph i32) (param $name_lists i32) (param $cx i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $e i32)
  (local $litmap i32)
  (local $n i32)
  (local $names i32)
  (local $pair i32)
  (local $t i32)
  (local $tmap0 i32)
  (local $tmap1 i32)
  (local $to_def i32)
  (local $x i32)
  local.get $constraints
  call $hydra.adapt.adapt_literal_types_map
  local.set $litmap
  i32.const 1
  local.get $e
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
  local.get $x
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
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $pair
  call $hydra.lib.pairs.second
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
  local.set $to_def
  i32.const 1
  local.get $tmap1
  local.get $to_def
  local.get $n
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
  local.get $n
  local.get $tmap1
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.map
  local.get $names
  call $hydra.lib.maybes.map_maybe
  call $hydra.lib.lists.map
  local.get $name_lists
  call $hydra.lib.lists.map
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
  call $hydra.lib.eithers.bind
)
  (func $hydra.adapt.simple_language_adapter (param $lang i32) (param $cx i32) (param $g i32) (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $adapted_type i32)
  (local $constraints i32)
  (local $cx2 i32)
  (local $litmap i32)
  (local $term i32)
  local.get $lang
  i32.load offset=4
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
  local.get $constraints
  local.get $litmap
  local.get $cx2
  local.get $g
  local.get $term
  call $hydra.adapt.adapt_term
  i32.const 1
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
  i32.const 16
  call $__alloc
  local.set $__rec_ptr
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
  (func $hydra.adapt.term_alternatives (param $cx i32) (param $graph i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $abs i32)
  (local $at i32)
  (local $field i32)
  (local $fname i32)
  (local $for_field_type i32)
  (local $ft i32)
  (local $fterm i32)
  (local $ftname i32)
  (local $inj i32)
  (local $ot i32)
  (local $rt i32)
  (local $ta i32)
  (local $term2 i32)
  (local $tname i32)
  (local $v i32)
  (local $wt i32)
  (block $end_term (result i32)
  (block $wrap
  (block $unit
  (block $inject
  (block $type_application
  (block $type_lambda
  (block $maybe
  (block $annotated
  local.get $term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $maybe $type_lambda $type_application $inject $unit $wrap $wrap
)
  local.get $v
  drop
  local.get $at
  i32.load
  local.set $term2
  i32.const 1
  local.get $term2
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
  br $end_term
)
  local.get $v
  drop
  i32.const 1
  i32.const 7
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $term2
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
  local.get $ot
  call $hydra.lib.maybes.maybe
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
  br $end_term
)
  local.get $v
  drop
  local.get $abs
  i32.load offset=4
  local.set $term2
  i32.const 1
  local.get $term2
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
  br $end_term
)
  local.get $v
  drop
  local.get $ta
  i32.load
  local.set $term2
  i32.const 1
  local.get $term2
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
  br $end_term
)
  local.get $v
  drop
  local.get $inj
  i32.load
  local.set $tname
  local.get $inj
  i32.load offset=4
  local.set $field
  local.get $field
  i32.load
  local.set $fname
  local.get $field
  i32.load offset=4
  local.set $fterm
  local.get $ft
  i32.load
  local.set $ftname
  local.get $fname
  i32.const 10
  local.get $ftname
  local.get $fname
  call $hydra.lib.equality.equal
  local.get $fterm
  i32.const 0
  call $hydra.lib.logic.if_else
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
  local.set $for_field_type
  local.get $cx
  local.get $graph
  local.get $tname
  call $hydra.resolution.require_union_type
  i32.const 1
  i32.const 13
  local.get $tname
  local.get $for_field_type
  local.get $rt
  call $hydra.lib.lists.map
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
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
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
  br $end_term
)
  local.get $v
  drop
  i32.const 1
  i32.const 8
  i32.const 1
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
  br $end_term
)
  local.get $v
  drop
  local.get $wt
  i32.load offset=4
  local.set $term2
  i32.const 1
  local.get $term2
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
  br $end_term
)
)
  (func $hydra.adapt.type_alternatives (param $type i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $at i32)
  (local $f i32)
  (local $opt_fields i32)
  (local $ot i32)
  (local $rt i32)
  (local $to_opt_field i32)
  (local $type2 i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $void
  (block $unit
  (block $union
  (block $maybe
  (block $annotated
  local.get $type
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $maybe $union $unit $void $void
)
  local.get $v
  drop
  local.get $at
  i32.load
  local.set $type2
  local.get $type2
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
  br $end_type
)
  local.get $v
  drop
  i32.const 5
  local.get $ot
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
  br $end_type
)
  local.get $v
  drop
  local.get $f
  i32.load
  i32.const 8
  local.get $f
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
  local.set $to_opt_field
  local.get $to_opt_field
  local.get $rt
  call $hydra.lib.lists.map
  local.set $opt_fields
  i32.const 10
  local.get $opt_fields
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
  br $end_type
)
  local.get $v
  drop
  i32.const 6
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
  br $end_type
)
  local.get $v
  drop
  i32.const 13
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
  br $end_type
)
)
)
