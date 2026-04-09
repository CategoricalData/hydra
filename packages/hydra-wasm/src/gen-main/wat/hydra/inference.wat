(module
  (import "hydra.annotations" "hydra.annotations.debug_if" (func $hydra.annotations.debug_if (param i32) (result i32) ) )
  (import "hydra.checking" "hydra.checking.check_for_unbound_type_variables" (func $hydra.checking.check_for_unbound_type_variables (param i32) (result i32) ) )
  (import "hydra.checking" "hydra.checking.check_type_subst" (func $hydra.checking.check_type_subst (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.let" (func $hydra.extract.core.let (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.record_type" (func $hydra.extract.core.record_type (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.union_type" (func $hydra.extract.core.union_type (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.wrapped_type" (func $hydra.extract.core.wrapped_type (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.build_graph" (func $hydra.lexical.build_graph (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bimap" (func $hydra.lib.eithers.bimap (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_maybe" (func $hydra.lib.eithers.map_maybe (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.lte" (func $hydra.lib.equality.lte (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat" (func $hydra.lib.lists.concat (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.filter" (func $hydra.lib.lists.filter (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.head" (func $hydra.lib.lists.head (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.nub" (func $hydra.lib.lists.nub (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.reverse" (func $hydra.lib.lists.reverse (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.singleton" (func $hydra.lib.lists.singleton (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.tail" (func $hydra.lib.lists.tail (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.zip" (func $hydra.lib.lists.zip (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.zip_with" (func $hydra.lib.lists.zip_with (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int32" (func $hydra.lib.literals.show_int32 (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.and" (func $hydra.lib.logic.and (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.elems" (func $hydra.lib.maps.elems (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.empty" (func $hydra.lib.maps.empty (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.insert" (func $hydra.lib.maps.insert (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.keys" (func $hydra.lib.maps.keys (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.member" (func $hydra.lib.maps.member (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.null" (func $hydra.lib.maps.null (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.singleton" (func $hydra.lib.maps.singleton (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.union" (func $hydra.lib.maps.union (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.add" (func $hydra.lib.math.add (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.range" (func $hydra.lib.math.range (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.sub" (func $hydra.lib.math.sub (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cat" (func $hydra.lib.maybes.cat (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.from_maybe" (func $hydra.lib.maybes.from_maybe (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.to_list" (func $hydra.lib.maybes.to_list (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.difference" (func $hydra.lib.sets.difference (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.empty" (func $hydra.lib.sets.empty (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.member" (func $hydra.lib.sets.member (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.null" (func $hydra.lib.sets.null (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.singleton" (func $hydra.lib.sets.singleton (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.union" (func $hydra.lib.sets.union (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.unions" (func $hydra.lib.sets.unions (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.split_on" (func $hydra.lib.strings.split_on (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.fresh_name" (func $hydra.names.fresh_name (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.fresh_names" (func $hydra.names.fresh_names (param i32) (result i32) ) )
  (import "hydra.reflect" "hydra.reflect.literal_type" (func $hydra.reflect.literal_type (param i32) (result i32) ) )
  (import "hydra.resolution" "hydra.resolution.find_field_type" (func $hydra.resolution.find_field_type (param i32) (result i32) ) )
  (import "hydra.resolution" "hydra.resolution.instantiate_type_scheme" (func $hydra.resolution.instantiate_type_scheme (param i32) (result i32) ) )
  (import "hydra.resolution" "hydra.resolution.nominal_application" (func $hydra.resolution.nominal_application (param i32) (result i32) ) )
  (import "hydra.resolution" "hydra.resolution.require_schema_type" (func $hydra.resolution.require_schema_type (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_term" (func $hydra.rewriting.rewrite_term (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.term" (func $hydra.show.core.term (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.type" (func $hydra.show.core.type (param i32) (result i32) ) )
  (import "hydra.show.typing" "hydra.show.typing.type_subst" (func $hydra.show.typing.type_subst (param i32) (result i32) ) )
  (import "hydra.sorting" "hydra.sorting.topological_sort_components" (func $hydra.sorting.topological_sort_components (param i32) (result i32) ) )
  (import "hydra.substitution" "hydra.substitution.compose_type_subst" (func $hydra.substitution.compose_type_subst (param i32) (result i32) ) )
  (import "hydra.substitution" "hydra.substitution.compose_type_subst_list" (func $hydra.substitution.compose_type_subst_list (param i32) (result i32) ) )
  (import "hydra.substitution" "hydra.substitution.id_type_subst" (func $hydra.substitution.id_type_subst (param i32) (result i32) ) )
  (import "hydra.substitution" "hydra.substitution.subst_in_class_constraints" (func $hydra.substitution.subst_in_class_constraints (param i32) (result i32) ) )
  (import "hydra.substitution" "hydra.substitution.subst_in_context" (func $hydra.substitution.subst_in_context (param i32) (result i32) ) )
  (import "hydra.substitution" "hydra.substitution.subst_in_type" (func $hydra.substitution.subst_in_type (param i32) (result i32) ) )
  (import "hydra.substitution" "hydra.substitution.subst_in_type_scheme" (func $hydra.substitution.subst_in_type_scheme (param i32) (result i32) ) )
  (import "hydra.substitution" "hydra.substitution.subst_types_in_term" (func $hydra.substitution.subst_types_in_term (param i32) (result i32) ) )
  (import "hydra.substitution" "hydra.substitution.substitute_in_term" (func $hydra.substitution.substitute_in_term (param i32) (result i32) ) )
  (import "hydra.unification" "hydra.unification.unify_type_constraints" (func $hydra.unification.unify_type_constraints (param i32) (result i32) ) )
  (import "hydra.unification" "hydra.unification.unify_type_lists" (func $hydra.unification.unify_type_lists (param i32) (result i32) ) )
  (import "hydra.unification" "hydra.unification.unify_types" (func $hydra.unification.unify_types (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.free_type_variables_in_term" (func $hydra.variables.free_type_variables_in_term (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.free_variables_in_term" (func $hydra.variables.free_variables_in_term (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.free_variables_in_type" (func $hydra.variables.free_variables_in_type (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.free_variables_in_type_ordered" (func $hydra.variables.free_variables_in_type_ordered (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.free_variables_in_type_scheme_simple" (func $hydra.variables.free_variables_in_type_scheme_simple (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.normalize_type_variables_in_term" (func $hydra.variables.normalize_type_variables_in_term (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.inference.bind_constraints" (func $hydra.inference.bind_constraints) )
  (export "hydra.inference.bind_unbound_type_variables" (func $hydra.inference.bind_unbound_type_variables) )
  (export "hydra.inference.build_type_application_term" (func $hydra.inference.build_type_application_term) )
  (export "hydra.inference.extend_context" (func $hydra.inference.extend_context) )
  (export "hydra.inference.finalize_inferred_term" (func $hydra.inference.finalize_inferred_term) )
  (export "hydra.inference.for_inferred_term" (func $hydra.inference.for_inferred_term) )
  (export "hydra.inference.free_variables_in_context" (func $hydra.inference.free_variables_in_context) )
  (export "hydra.inference.fresh_variable_type" (func $hydra.inference.fresh_variable_type) )
  (export "hydra.inference.generalize" (func $hydra.inference.generalize) )
  (export "hydra.inference.infer_graph_types" (func $hydra.inference.infer_graph_types) )
  (export "hydra.inference.infer_in_graph_context" (func $hydra.inference.infer_in_graph_context) )
  (export "hydra.inference.infer_many" (func $hydra.inference.infer_many) )
  (export "hydra.inference.infer_type_of" (func $hydra.inference.infer_type_of) )
  (export "hydra.inference.infer_type_of_annotated_term" (func $hydra.inference.infer_type_of_annotated_term) )
  (export "hydra.inference.infer_type_of_application" (func $hydra.inference.infer_type_of_application) )
  (export "hydra.inference.infer_type_of_case_statement" (func $hydra.inference.infer_type_of_case_statement) )
  (export "hydra.inference.infer_type_of_collection" (func $hydra.inference.infer_type_of_collection) )
  (export "hydra.inference.infer_type_of_either" (func $hydra.inference.infer_type_of_either) )
  (export "hydra.inference.infer_type_of_elimination" (func $hydra.inference.infer_type_of_elimination) )
  (export "hydra.inference.infer_type_of_function" (func $hydra.inference.infer_type_of_function) )
  (export "hydra.inference.infer_type_of_injection" (func $hydra.inference.infer_type_of_injection) )
  (export "hydra.inference.infer_type_of_lambda" (func $hydra.inference.infer_type_of_lambda) )
  (export "hydra.inference.infer_type_of_let" (func $hydra.inference.infer_type_of_let) )
  (export "hydra.inference.infer_type_of_let_normalized" (func $hydra.inference.infer_type_of_let_normalized) )
  (export "hydra.inference.infer_type_of_list" (func $hydra.inference.infer_type_of_list) )
  (export "hydra.inference.infer_type_of_literal" (func $hydra.inference.infer_type_of_literal) )
  (export "hydra.inference.infer_type_of_map" (func $hydra.inference.infer_type_of_map) )
  (export "hydra.inference.infer_type_of_optional" (func $hydra.inference.infer_type_of_optional) )
  (export "hydra.inference.infer_type_of_pair" (func $hydra.inference.infer_type_of_pair) )
  (export "hydra.inference.infer_type_of_primitive" (func $hydra.inference.infer_type_of_primitive) )
  (export "hydra.inference.infer_type_of_projection" (func $hydra.inference.infer_type_of_projection) )
  (export "hydra.inference.infer_type_of_record" (func $hydra.inference.infer_type_of_record) )
  (export "hydra.inference.infer_type_of_set" (func $hydra.inference.infer_type_of_set) )
  (export "hydra.inference.infer_type_of_term" (func $hydra.inference.infer_type_of_term) )
  (export "hydra.inference.infer_type_of_type_application" (func $hydra.inference.infer_type_of_type_application) )
  (export "hydra.inference.infer_type_of_type_lambda" (func $hydra.inference.infer_type_of_type_lambda) )
  (export "hydra.inference.infer_type_of_unit" (func $hydra.inference.infer_type_of_unit) )
  (export "hydra.inference.infer_type_of_unwrap" (func $hydra.inference.infer_type_of_unwrap) )
  (export "hydra.inference.infer_type_of_variable" (func $hydra.inference.infer_type_of_variable) )
  (export "hydra.inference.infer_type_of_wrapped_term" (func $hydra.inference.infer_type_of_wrapped_term) )
  (export "hydra.inference.infer_types_of_temporary_bindings" (func $hydra.inference.infer_types_of_temporary_bindings) )
  (export "hydra.inference.is_unbound" (func $hydra.inference.is_unbound) )
  (export "hydra.inference.map_constraints" (func $hydra.inference.map_constraints) )
  (export "hydra.inference.merge_class_constraints" (func $hydra.inference.merge_class_constraints) )
  (export "hydra.inference.show_inference_result" (func $hydra.inference.show_inference_result) )
  (export "hydra.inference.yield" (func $hydra.inference.yield) )
  (export "hydra.inference.yield_checked" (func $hydra.inference.yield_checked) )
  (export "hydra.inference.yield_checked_with_constraints" (func $hydra.inference.yield_checked_with_constraints) )
  (export "hydra.inference.yield_debug" (func $hydra.inference.yield_debug) )
  (export "hydra.inference.yield_with_constraints" (func $hydra.inference.yield_with_constraints) )
  (func $hydra.inference.bind_constraints (param $flow_cx i32) (param $cx i32) (param $constraints i32) (result i32)
  (local $_a i32)
  (local $_ic i32)
  (local $s i32)
  local.get $_ic
  ;; project field: object
  ;; project field: message
  local.get $_ic
  ;; project field: context
  local.get $_a
  local.get $flow_cx
  local.get $cx
  ;; project field: schema_types
  local.get $constraints
  call $hydra.unification.unify_type_constraints
  call $hydra.lib.eithers.bimap
  local.get $flow_cx
  local.get $cx
  local.get $s
  call $hydra.checking.check_type_subst
  i32.const 1
  local.get $s
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.inference.bind_unbound_type_variables (param $cx i32) (param $term0 i32) (result i32)
  (local $b i32)
  (local $bname i32)
  (local $bterm i32)
  (local $bterm2 i32)
  (local $bvars i32)
  (local $for_binding i32)
  (local $l i32)
  (local $rewrite i32)
  (local $svars i32)
  (local $t i32)
  (local $term i32)
  (local $ts i32)
  (local $ts2 i32)
  (local $unbound i32)
  (local $unbound_in_term i32)
  (local $unbound_in_type i32)
  (local $v i32)
  local.get $cx
  ;; project field: schema_types
  call $hydra.lib.maps.keys
  call $hydra.lib.sets.from_list
  local.set $svars
  (block $end_term (result i32)
  (block $let
  local.get $term
  br_table $let $let
)
  local.get $b
  ;; project field: name
  local.set $bname
  local.get $b
  ;; project field: term
  local.set $bterm
  local.get $bname
  local.get $cx
  local.get $bterm
  call $hydra.inference.bind_unbound_type_variables
  i32.const 0
  local.get $ts
  ;; project field: variables
  call $hydra.lib.sets.from_list
  local.set $bvars
  local.get $ts
  ;; project field: type
  call $hydra.variables.free_variables_in_type
  local.set $unbound_in_type
  local.get $bterm
  call $hydra.variables.free_type_variables_in_term
  local.set $unbound_in_term
  local.get $unbound_in_type
  local.get $unbound_in_term
  call $hydra.lib.sets.union
  local.get $svars
  local.get $bvars
  call $hydra.lib.sets.union
  call $hydra.lib.sets.difference
  call $hydra.lib.sets.to_list
  local.set $unbound
  local.get $ts
  ;; project field: variables
  local.get $unbound
  call $hydra.lib.lists.concat2
  local.get $ts
  ;; project field: type
  local.get $ts
  ;; project field: constraints
  local.set $ts2
  local.get $v
  local.get $t
  local.get $bterm
  local.get $unbound
  call $hydra.lib.lists.foldl
  local.set $bterm2
  local.get $bname
  local.get $bterm2
  local.get $ts2
  local.get $b
  ;; project field: type
  call $hydra.lib.maybes.maybe
  local.set $for_binding
  local.get $for_binding
  local.get $l
  ;; project field: bindings
  call $hydra.lib.lists.map
  local.get $cx
  local.get $l
  ;; project field: body
  call $hydra.inference.bind_unbound_type_variables
  br $end_term
)
  local.set $rewrite
  local.get $rewrite
  local.get $term0
  call $hydra.rewriting.rewrite_term
)
  (func $hydra.inference.build_type_application_term (param $tvars i32) (param $body i32) (result i32)
  (local $t i32)
  (local $v i32)
  local.get $t
  local.get $v
  local.get $body
  local.get $tvars
  call $hydra.lib.lists.foldl
)
  (func $hydra.inference.extend_context (param $pairs i32) (param $cx i32) (result i32)
  local.get $cx
  ;; project field: bound_terms
  local.get $pairs
  call $hydra.lib.maps.from_list
  local.get $cx
  ;; project field: bound_types
  call $hydra.lib.maps.union
  local.get $cx
  ;; project field: class_constraints
  local.get $cx
  ;; project field: lambda_variables
  local.get $cx
  ;; project field: metadata
  local.get $cx
  ;; project field: primitives
  local.get $cx
  ;; project field: schema_types
  local.get $cx
  ;; project field: type_variables
)
  (func $hydra.inference.finalize_inferred_term (param $flow_cx i32) (param $cx i32) (param $term i32) (result i32)
  (local $term2 i32)
  local.get $cx
  local.get $term
  call $hydra.inference.bind_unbound_type_variables
  local.set $term2
  local.get $flow_cx
  local.get $cx
  local.get $term2
  call $hydra.checking.check_for_unbound_type_variables
  i32.const 1
  local.get $term2
  call $hydra.variables.normalize_type_variables_in_term
  call $hydra.lib.eithers.bind
)
  (func $hydra.inference.for_inferred_term (param $fcx i32) (param $cx i32) (param $term i32) (param $desc i32) (param $f i32) (result i32)
  (local $rp i32)
  local.get $fcx
  local.get $cx
  local.get $term
  local.get $desc
  call $hydra.inference.infer_type_of_term
  i32.const 1
  local.get $rp
  local.get $f
  local.get $rp
  ;; project field: context
  call $hydra.lib.eithers.bind
)
  (func $hydra.inference.free_variables_in_context (param $cx i32) (result i32)
  call $hydra.lib.sets.union
  call $hydra.lib.sets.empty
  call $hydra.variables.free_variables_in_type_scheme_simple
  local.get $cx
  ;; project field: bound_types
  call $hydra.lib.maps.elems
  call $hydra.lib.lists.map
  call $hydra.lib.lists.foldl
)
  (func $hydra.inference.fresh_variable_type (param $cx i32) (result i32)
  (local $cx2 i32)
  (local $name i32)
  (local $result i32)
  local.get $cx
  call $hydra.names.fresh_name
  local.set $result
  local.get $result
  call $hydra.lib.pairs.first
  local.set $name
  local.get $result
  call $hydra.lib.pairs.second
  local.set $cx2
  local.get $name
  local.get $cx2
)
  (func $hydra.inference.generalize (param $cx i32) (param $typ i32) (result i32)
  (local $all_constraints i32)
  (local $constraints_maybe i32)
  (local $is_type_var_name i32)
  (local $meta i32)
  (local $parts i32)
  (local $relevant_constraints i32)
  (local $v i32)
  (local $vars i32)
  i32.const 0 ;; string: "."
  nop
  call $hydra.lib.strings.split_on
  local.set $parts
  local.get $parts
  call $hydra.lib.lists.length
  i32.const 1
  call $hydra.lib.equality.lte
  local.set $is_type_var_name
  local.get $cx
  local.get $v
  call $hydra.inference.is_unbound
  local.get $v
  local.get $is_type_var_name
  call $hydra.lib.logic.and
  local.get $typ
  call $hydra.variables.free_variables_in_type_ordered
  call $hydra.lib.lists.filter
  call $hydra.lib.lists.nub
  local.set $vars
  local.get $cx
  ;; project field: class_constraints
  local.set $all_constraints
  local.get $v
  local.get $meta
  local.get $v
  local.get $all_constraints
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.map
  local.get $vars
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  call $hydra.lib.maps.from_list
  local.set $relevant_constraints
  local.get $relevant_constraints
  call $hydra.lib.maps.null
  i32.const 0
  local.get $relevant_constraints
  call $hydra.lib.logic.if_else
  local.set $constraints_maybe
  local.get $vars
  local.get $typ
  local.get $constraints_maybe
)
  (func $hydra.inference.infer_graph_types (param $fcx0 i32) (param $bindings0 i32) (param $g0 i32) (result i32)
  (local $bindings i32)
  (local $fcx i32)
  (local $fcx2 i32)
  (local $finalized i32)
  (local $from_let_term i32)
  (local $g i32)
  (local $l i32)
  (local $let0 i32)
  (local $prims i32)
  (local $raw_g i32)
  (local $result i32)
  (local $schema_types i32)
  (local $term i32)
  i32.const 0 ;; string: "graph inference"
  local.get $fcx0
  ;; project field: trace
  call $hydra.lib.lists.cons
  local.get $fcx0
  ;; project field: messages
  local.get $fcx0
  ;; project field: other
  local.set $fcx
  local.get $bindings0
  i32.const 0
  local.set $let0
  local.get $l
  ;; project field: bindings
  local.set $bindings
  local.get $g0
  ;; project field: primitives
  local.set $prims
  local.get $g0
  ;; project field: schema_types
  local.set $schema_types
  local.get $bindings
  call $hydra.lib.maps.empty
  local.get $prims
  call $hydra.lexical.build_graph
  local.set $raw_g
  local.get $raw_g
  ;; project field: bound_terms
  local.get $raw_g
  ;; project field: bound_types
  local.get $raw_g
  ;; project field: class_constraints
  local.get $raw_g
  ;; project field: lambda_variables
  local.get $raw_g
  ;; project field: metadata
  local.get $raw_g
  ;; project field: primitives
  local.get $schema_types
  local.get $raw_g
  ;; project field: type_variables
  local.set $g
  local.get $g
  local.get $bindings
  local.set $from_let_term
  local.get $fcx
  local.get $g0
  local.get $let0
  i32.const 0 ;; string: "graph term"
  call $hydra.inference.infer_type_of_term
  local.get $result
  ;; project field: context
  local.set $fcx2
  local.get $result
  ;; project field: term
  local.set $term
  local.get $fcx2
  local.get $g0
  local.get $term
  call $hydra.inference.finalize_inferred_term
  (block $end_term (result i32)
  (block $variable
  (block $let
  local.get $finalized
  br_table $let $variable $variable
)
  i32.const 1
  local.get $l
  local.get $from_let_term
  local.get $fcx2
  br $end_term
)
  i32.const 0
  i32.const 0 ;; string: "Expected inferred graph as let term"
  local.get $fcx2
  br $end_term
)
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.inference.infer_in_graph_context (param $fcx i32) (param $cx i32) (param $term i32) (result i32)
  local.get $fcx
  local.get $cx
  local.get $term
  i32.const 0 ;; string: "single term"
  call $hydra.inference.infer_type_of_term
)
  (func $hydra.inference.infer_many (param $fcx i32) (param $cx i32) (param $pairs i32) (result i32)
  (local $c1 i32)
  (local $c1_subst i32)
  (local $c2 i32)
  (local $desc i32)
  (local $dflt i32)
  (local $e i32)
  (local $e1 i32)
  (local $e2 i32)
  (local $fcx2 i32)
  (local $fcx3 i32)
  (local $merged_constraints i32)
  (local $result1 i32)
  (local $result2 i32)
  (local $rp2 i32)
  (local $s1 i32)
  (local $s2 i32)
  (local $t1 i32)
  (local $t2 i32)
  (local $tl i32)
  local.get $pairs
  call $hydra.lib.lists.null
  i32.const 1
  i32.const 0
  ;; list elements follow
  i32.const 0
  ;; list elements follow
  call $hydra.substitution.id_type_subst
  call $hydra.lib.maps.empty
  local.get $fcx
  local.get $pairs
  call $hydra.lib.lists.head
  call $hydra.lib.pairs.first
  local.set $e
  local.get $pairs
  call $hydra.lib.lists.head
  call $hydra.lib.pairs.second
  local.set $desc
  local.get $pairs
  call $hydra.lib.lists.tail
  local.set $tl
  local.get $fcx
  local.get $cx
  local.get $e
  local.get $desc
  call $hydra.inference.infer_type_of_term
  local.get $result1
  ;; project field: context
  local.set $fcx2
  local.get $result1
  ;; project field: term
  local.set $e1
  local.get $result1
  ;; project field: type
  local.set $t1
  local.get $result1
  ;; project field: subst
  local.set $s1
  local.get $result1
  ;; project field: class_constraints
  local.set $c1
  local.get $fcx2
  local.get $s1
  local.get $cx
  call $hydra.substitution.subst_in_context
  local.get $tl
  call $hydra.inference.infer_many
  local.get $rp2
  call $hydra.lib.pairs.first
  local.set $result2
  local.get $rp2
  call $hydra.lib.pairs.second
  local.set $fcx3
  local.get $result2
  call $hydra.lib.pairs.first
  local.set $e2
  local.get $result2
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.set $t2
  local.get $result2
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.set $s2
  local.get $result2
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  local.set $c2
  local.get $s2
  local.get $c1
  call $hydra.substitution.subst_in_class_constraints
  local.set $c1_subst
  local.get $c1_subst
  local.get $c2
  call $hydra.inference.merge_class_constraints
  local.set $merged_constraints
  i32.const 1
  local.get $s2
  local.get $e1
  call $hydra.substitution.subst_types_in_term
  local.get $e2
  call $hydra.lib.lists.cons
  local.get $s2
  local.get $t1
  call $hydra.substitution.subst_in_type
  local.get $t2
  call $hydra.lib.lists.cons
  local.get $s1
  local.get $s2
  call $hydra.substitution.compose_type_subst
  local.get $merged_constraints
  local.get $fcx3
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  local.set $dflt
  local.get $dflt
  call $hydra.lib.logic.if_else
)
  (func $hydra.inference.infer_type_of (param $fcx i32) (param $cx i32) (param $term i32) (result i32)
  (local $binding i32)
  (local $bindings i32)
  (local $fcx2 i32)
  (local $finalized i32)
  (local $let_result i32)
  (local $let_term i32)
  (local $mts i32)
  (local $result i32)
  (local $term1 i32)
  (local $ts i32)
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "ignoredVariableName"
  local.get $term
  i32.const 0
  i32.const 0 ;; string: "ignoredBody"
  local.set $let_term
  local.get $fcx
  local.get $cx
  local.get $let_term
  i32.const 0 ;; string: "infer type of term"
  call $hydra.inference.infer_type_of_term
  local.get $result
  ;; project field: context
  local.set $fcx2
  local.get $fcx2
  local.get $cx
  local.get $result
  ;; project field: term
  call $hydra.inference.finalize_inferred_term
  local.get $fcx2
  local.get $cx
  local.get $finalized
  call $hydra.extract.core.let
  local.get $let_result
  ;; project field: bindings
  local.set $bindings
  i32.const 1
  local.get $bindings
  call $hydra.lib.lists.length
  call $hydra.lib.equality.equal
  local.get $bindings
  call $hydra.lib.lists.head
  local.set $binding
  local.get $binding
  ;; project field: term
  local.set $term1
  local.get $binding
  ;; project field: type
  local.set $mts
  i32.const 0
  i32.const 0 ;; string: "Expected a type scheme"
  local.get $fcx2
  i32.const 1
  local.get $term1
  local.get $ts
  local.get $fcx2
  local.get $mts
  call $hydra.lib.maybes.maybe
  i32.const 0
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "Expected a single binding with a type scheme, but got: "
  local.get $bindings
  call $hydra.lib.lists.length
  call $hydra.lib.literals.show_int32
  i32.const 0 ;; string: " bindings"
  call $hydra.lib.strings.cat
  local.get $fcx2
  call $hydra.lib.logic.if_else
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.inference.infer_type_of_annotated_term (param $fcx i32) (param $cx i32) (param $at i32) (result i32)
  (local $ann i32)
  (local $fcx2 i32)
  (local $iconstraints i32)
  (local $isubst i32)
  (local $iterm i32)
  (local $itype i32)
  (local $result i32)
  (local $term i32)
  local.get $at
  ;; project field: body
  local.set $term
  local.get $at
  ;; project field: annotation
  local.set $ann
  local.get $fcx
  local.get $cx
  local.get $term
  i32.const 0 ;; string: "annotated term"
  call $hydra.inference.infer_type_of_term
  local.get $result
  ;; project field: context
  local.set $fcx2
  local.get $result
  ;; project field: term
  local.set $iterm
  local.get $result
  ;; project field: type
  local.set $itype
  local.get $result
  ;; project field: subst
  local.set $isubst
  local.get $result
  ;; project field: class_constraints
  local.set $iconstraints
  i32.const 1
  local.get $iterm
  local.get $ann
  local.get $itype
  local.get $isubst
  local.get $iconstraints
  local.get $fcx2
  call $hydra.lib.eithers.bind
)
  (func $hydra.inference.infer_type_of_application (param $fcx0 i32) (param $cx i32) (param $app i32) (result i32)
  (local $_a i32)
  (local $_ic i32)
  (local $a i32)
  (local $b i32)
  (local $c0 i32)
  (local $c0_subst i32)
  (local $c1 i32)
  (local $c1_subst i32)
  (local $e0 i32)
  (local $e1 i32)
  (local $fcx i32)
  (local $fcx2 i32)
  (local $fcx3 i32)
  (local $fcx4 i32)
  (local $lhs_result i32)
  (local $r_constraints i32)
  (local $r_expr i32)
  (local $r_subst i32)
  (local $r_type i32)
  (local $rhs_result i32)
  (local $s0 i32)
  (local $s1 i32)
  (local $s2 i32)
  (local $t0 i32)
  (local $t1 i32)
  (local $v i32)
  (local $v_result i32)
  i32.const 0 ;; string: "application"
  local.get $fcx0
  ;; project field: trace
  call $hydra.lib.lists.cons
  local.get $fcx0
  ;; project field: messages
  local.get $fcx0
  ;; project field: other
  local.set $fcx
  local.get $app
  ;; project field: function
  local.set $e0
  local.get $app
  ;; project field: argument
  local.set $e1
  local.get $fcx
  local.get $cx
  local.get $e0
  i32.const 0 ;; string: "lhs"
  call $hydra.inference.infer_type_of_term
  local.get $lhs_result
  ;; project field: context
  local.set $fcx2
  local.get $lhs_result
  ;; project field: term
  local.set $a
  local.get $lhs_result
  ;; project field: type
  local.set $t0
  local.get $lhs_result
  ;; project field: subst
  local.set $s0
  local.get $lhs_result
  ;; project field: class_constraints
  local.set $c0
  local.get $fcx2
  local.get $s0
  local.get $cx
  call $hydra.substitution.subst_in_context
  local.get $e1
  i32.const 0 ;; string: "rhs"
  call $hydra.inference.infer_type_of_term
  local.get $rhs_result
  ;; project field: context
  local.set $fcx3
  local.get $rhs_result
  ;; project field: term
  local.set $b
  local.get $rhs_result
  ;; project field: type
  local.set $t1
  local.get $rhs_result
  ;; project field: subst
  local.set $s1
  local.get $rhs_result
  ;; project field: class_constraints
  local.set $c1
  local.get $fcx3
  call $hydra.names.fresh_name
  local.set $v_result
  local.get $v_result
  call $hydra.lib.pairs.first
  local.set $v
  local.get $v_result
  call $hydra.lib.pairs.second
  local.set $fcx4
  local.get $_ic
  ;; project field: object
  ;; project field: message
  local.get $_ic
  ;; project field: context
  local.get $_a
  local.get $fcx4
  local.get $cx
  ;; project field: schema_types
  local.get $s1
  local.get $t0
  call $hydra.substitution.subst_in_type
  local.get $t1
  local.get $v
  i32.const 0 ;; string: "application lhs"
  call $hydra.unification.unify_types
  call $hydra.lib.eithers.bimap
  local.get $fcx4
  local.get $cx
  local.get $s2
  call $hydra.checking.check_type_subst
  local.get $s1
  local.get $s2
  call $hydra.substitution.compose_type_subst
  local.get $a
  call $hydra.substitution.subst_types_in_term
  local.get $s2
  local.get $b
  call $hydra.substitution.subst_types_in_term
  local.set $r_expr
  local.get $s2
  local.get $v
  call $hydra.substitution.subst_in_type
  local.set $r_type
  i32.const 3
  ;; list elements follow
  local.get $s0
  local.get $s1
  local.get $s2
  call $hydra.substitution.compose_type_subst_list
  local.set $r_subst
  local.get $s2
  local.get $s1
  local.get $c0
  call $hydra.substitution.subst_in_class_constraints
  call $hydra.substitution.subst_in_class_constraints
  local.set $c0_subst
  local.get $s2
  local.get $c1
  call $hydra.substitution.subst_in_class_constraints
  local.set $c1_subst
  local.get $c0_subst
  local.get $c1_subst
  call $hydra.inference.merge_class_constraints
  local.set $r_constraints
  i32.const 1
  local.get $r_expr
  local.get $r_type
  local.get $r_subst
  local.get $r_constraints
  local.get $fcx4
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.inference.infer_type_of_case_statement (param $fcx i32) (param $cx i32) (param $case_stmt i32) (result i32)
  (local $all_elem_constraints i32)
  (local $case_constraints i32)
  (local $case_elem_constraints i32)
  (local $case_map i32)
  (local $case_results i32)
  (local $case_rp i32)
  (local $cases i32)
  (local $cod i32)
  (local $codv i32)
  (local $codv_result i32)
  (local $dflt i32)
  (local $dflt_class_constraints i32)
  (local $dflt_constraints i32)
  (local $dflt_result i32)
  (local $dflt_rp i32)
  (local $f i32)
  (local $fcx2 i32)
  (local $fcx3 i32)
  (local $fcx4 i32)
  (local $fcx5 i32)
  (local $fname i32)
  (local $fnames i32)
  (local $ft i32)
  (local $ftype i32)
  (local $isubst i32)
  (local $iterms i32)
  (local $itype i32)
  (local $itypes i32)
  (local $mc_result i32)
  (local $n i32)
  (local $r i32)
  (local $schema_type i32)
  (local $sfields i32)
  (local $st_rp i32)
  (local $stype i32)
  (local $subst i32)
  (local $svars i32)
  (local $t i32)
  (local $tname i32)
  (local $x i32)
  local.get $case_stmt
  ;; project field: type_name
  local.set $tname
  local.get $case_stmt
  ;; project field: default
  local.set $dflt
  local.get $case_stmt
  ;; project field: cases
  local.set $cases
  ;; project field: name
  local.get $cases
  call $hydra.lib.lists.map
  local.set $fnames
  local.get $fcx
  local.get $cx
  ;; project field: schema_types
  local.get $tname
  call $hydra.resolution.require_schema_type
  local.get $st_rp
  call $hydra.lib.pairs.first
  local.set $schema_type
  local.get $st_rp
  call $hydra.lib.pairs.second
  local.set $fcx2
  local.get $schema_type
  ;; project field: variables
  local.set $svars
  local.get $schema_type
  ;; project field: type
  local.set $stype
  local.get $fcx2
  local.get $tname
  local.get $stype
  call $hydra.extract.core.union_type
  local.get $fcx2
  local.get $cx
  local.get $t
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "case "
  nop
  i32.const 0 ;; string: ".<default>"
  call $hydra.lib.strings.cat
  call $hydra.inference.infer_type_of_term
  local.get $dflt
  call $hydra.lib.eithers.map_maybe
  local.get $dflt_rp
  local.set $dflt_result
  local.get $fcx2
  ;; project field: context
  local.get $dflt_rp
  call $hydra.lib.maybes.map
  call $hydra.lib.maybes.from_maybe
  local.set $fcx3
  local.get $fcx3
  local.get $cx
  local.get $f
  ;; project field: term
  i32.const 4
  ;; list elements follow
  i32.const 0 ;; string: "case "
  nop
  i32.const 0 ;; string: "."
  nop
  call $hydra.lib.strings.cat
  local.get $cases
  call $hydra.lib.lists.map
  call $hydra.inference.infer_many
  local.get $case_rp
  call $hydra.lib.pairs.first
  local.set $case_results
  local.get $case_rp
  call $hydra.lib.pairs.second
  local.set $fcx4
  local.get $case_results
  call $hydra.lib.pairs.first
  local.set $iterms
  local.get $case_results
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.set $itypes
  local.get $case_results
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.set $isubst
  local.get $case_results
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  local.set $case_elem_constraints
  local.get $fcx4
  call $hydra.names.fresh_name
  local.set $codv_result
  local.get $codv_result
  call $hydra.lib.pairs.first
  local.set $codv
  local.get $codv_result
  call $hydra.lib.pairs.second
  local.set $fcx5
  local.get $codv
  local.set $cod
  local.get $ft
  ;; project field: name
  local.get $ft
  ;; project field: type
  local.get $sfields
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $case_map
  local.get $cod
  local.get $isubst
  local.get $r
  ;; project field: type
  call $hydra.substitution.subst_in_type
  i32.const 0 ;; string: "match default"
  local.get $dflt_result
  call $hydra.lib.maybes.map
  call $hydra.lib.maybes.to_list
  local.set $dflt_constraints
  local.get $itype
  local.get $ftype
  local.get $cod
  i32.const 0 ;; string: "case type"
  local.get $fname
  local.get $case_map
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.map
  local.get $fnames
  local.get $itypes
  call $hydra.lib.lists.zip_with
  call $hydra.lib.maybes.cat
  local.set $case_constraints
  call $hydra.lib.maps.empty
  ;; project field: class_constraints
  local.get $dflt_result
  call $hydra.lib.maybes.map
  call $hydra.lib.maybes.from_maybe
  local.set $dflt_class_constraints
  local.get $case_elem_constraints
  local.get $dflt_class_constraints
  call $hydra.inference.merge_class_constraints
  local.set $all_elem_constraints
  local.get $fcx5
  local.get $cx
  local.get $fcx5
  local.get $svars
  local.get $tname
  ;; project field: term
  local.get $dflt_result
  call $hydra.lib.maybes.map
  local.get $n
  local.get $t
  local.get $fnames
  local.get $iterms
  call $hydra.lib.lists.zip_with
  call $hydra.inference.build_type_application_term
  local.get $tname
  local.get $x
  local.get $svars
  call $hydra.lib.lists.map
  call $hydra.resolution.nominal_application
  local.get $cod
  i32.const 2
  ;; list elements follow
  ;; project field: subst
  local.get $dflt_result
  call $hydra.lib.maybes.map
  call $hydra.lib.maybes.to_list
  i32.const 2
  ;; list elements follow
  local.get $isubst
  local.get $subst
  call $hydra.lib.lists.concat
  call $hydra.substitution.compose_type_subst_list
  local.get $subst
  local.get $all_elem_constraints
  call $hydra.substitution.subst_in_class_constraints
  call $hydra.inference.yield_with_constraints
  i32.const 2
  ;; list elements follow
  local.get $dflt_constraints
  local.get $case_constraints
  call $hydra.lib.lists.concat
  call $hydra.inference.map_constraints
  i32.const 1
  local.get $mc_result
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.inference.infer_type_of_collection (param $fcx i32) (param $cx i32) (param $typ_cons i32) (param $trm_cons i32) (param $desc i32) (param $class_names i32) (param $els i32) (result i32)
  (local $all_constraints i32)
  (local $class_constraints i32)
  (local $constraints i32)
  (local $elem_constraints i32)
  (local $fcx2 i32)
  (local $fcx3 i32)
  (local $i i32)
  (local $isubst i32)
  (local $iterm i32)
  (local $itype i32)
  (local $mc_result i32)
  (local $results i32)
  (local $results_rp i32)
  (local $subst1 i32)
  (local $subst2 i32)
  (local $t i32)
  (local $terms i32)
  (local $types i32)
  (local $var i32)
  (local $var_result i32)
  local.get $fcx
  call $hydra.names.fresh_name
  local.set $var_result
  local.get $var_result
  call $hydra.lib.pairs.first
  local.set $var
  local.get $var_result
  call $hydra.lib.pairs.second
  local.set $fcx2
  local.get $class_names
  call $hydra.lib.sets.null
  call $hydra.lib.maps.empty
  local.get $var
  local.get $class_names
  call $hydra.lib.maps.singleton
  call $hydra.lib.logic.if_else
  local.set $class_constraints
  local.get $els
  call $hydra.lib.lists.null
  i32.const 1
  local.get $fcx2
  i32.const 1
  ;; list elements follow
  local.get $var
  i32.const 0
  ;; list elements follow
  local.get $trm_cons
  call $hydra.inference.build_type_application_term
  local.get $var
  local.get $typ_cons
  call $hydra.substitution.id_type_subst
  local.get $class_constraints
  call $hydra.inference.yield_with_constraints
  local.get $fcx2
  local.get $cx
  local.get $els
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "#"
  local.get $i
  call $hydra.lib.literals.show_int32
  call $hydra.lib.strings.cat
  i32.const 1
  local.get $els
  call $hydra.lib.lists.length
  i32.const 1
  call $hydra.lib.math.add
  call $hydra.lib.math.range
  call $hydra.lib.lists.map
  call $hydra.lib.lists.zip
  call $hydra.inference.infer_many
  local.get $results_rp
  call $hydra.lib.pairs.first
  local.set $results
  local.get $results_rp
  call $hydra.lib.pairs.second
  local.set $fcx3
  local.get $results
  call $hydra.lib.pairs.first
  local.set $terms
  local.get $results
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.set $types
  local.get $results
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.set $subst1
  local.get $results
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  local.set $elem_constraints
  local.get $var
  local.get $t
  local.get $desc
  local.get $types
  call $hydra.lib.lists.map
  local.set $constraints
  local.get $class_constraints
  local.get $elem_constraints
  call $hydra.inference.merge_class_constraints
  local.set $all_constraints
  local.get $fcx3
  local.get $cx
  local.get $terms
  local.get $trm_cons
  local.set $iterm
  local.get $var
  local.get $typ_cons
  local.set $itype
  local.get $subst1
  local.get $subst2
  call $hydra.substitution.compose_type_subst
  local.set $isubst
  local.get $fcx3
  local.get $iterm
  local.get $itype
  local.get $isubst
  local.get $subst2
  local.get $all_constraints
  call $hydra.substitution.subst_in_class_constraints
  call $hydra.inference.yield_with_constraints
  local.get $constraints
  call $hydra.inference.map_constraints
  i32.const 1
  local.get $mc_result
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.logic.if_else
)
  (func $hydra.inference.infer_type_of_either (param $fcx i32) (param $cx i32) (param $e i32) (result i32)
  (local $either_term i32)
  (local $either_type i32)
  (local $fcx2 i32)
  (local $fcx3 i32)
  (local $fv_result i32)
  (local $iterm i32)
  (local $l i32)
  (local $left_type i32)
  (local $r i32)
  (local $r1 i32)
  (local $right_type i32)
  (local $subst i32)
  (local $term_with_both_types i32)
  (local $term_with_left_type i32)
  local.get $fcx
  local.get $cx
  local.get $l
  i32.const 0 ;; string: "either left value"
  call $hydra.inference.infer_type_of_term
  local.get $r1
  ;; project field: context
  local.set $fcx2
  local.get $r1
  ;; project field: term
  local.set $iterm
  local.get $r1
  ;; project field: type
  local.set $left_type
  local.get $r1
  ;; project field: subst
  local.set $subst
  local.get $fcx2
  call $hydra.inference.fresh_variable_type
  local.set $fv_result
  local.get $fv_result
  call $hydra.lib.pairs.first
  local.set $right_type
  local.get $fv_result
  call $hydra.lib.pairs.second
  local.set $fcx3
  i32.const 0
  local.get $iterm
  local.set $either_term
  local.get $either_term
  local.get $left_type
  local.set $term_with_left_type
  local.get $term_with_left_type
  local.get $right_type
  local.set $term_with_both_types
  local.get $left_type
  local.get $right_type
  local.set $either_type
  i32.const 1
  local.get $fcx3
  local.get $term_with_both_types
  local.get $either_type
  local.get $subst
  call $hydra.inference.yield_checked
  call $hydra.lib.eithers.bind
  local.get $fcx
  local.get $cx
  local.get $r
  i32.const 0 ;; string: "either right value"
  call $hydra.inference.infer_type_of_term
  local.get $r1
  ;; project field: context
  local.set $fcx2
  local.get $r1
  ;; project field: term
  local.set $iterm
  local.get $r1
  ;; project field: type
  local.set $right_type
  local.get $r1
  ;; project field: subst
  local.set $subst
  local.get $fcx2
  call $hydra.inference.fresh_variable_type
  local.set $fv_result
  local.get $fv_result
  call $hydra.lib.pairs.first
  local.set $left_type
  local.get $fv_result
  call $hydra.lib.pairs.second
  local.set $fcx3
  i32.const 1
  local.get $iterm
  local.set $either_term
  local.get $either_term
  local.get $left_type
  local.set $term_with_left_type
  local.get $term_with_left_type
  local.get $right_type
  local.set $term_with_both_types
  local.get $left_type
  local.get $right_type
  local.set $either_type
  i32.const 1
  local.get $fcx3
  local.get $term_with_both_types
  local.get $either_type
  local.get $subst
  call $hydra.inference.yield_checked
  call $hydra.lib.eithers.bind
  local.get $e
  call $hydra.lib.eithers.either
)
  (func $hydra.inference.infer_type_of_elimination (param $fcx i32) (param $cx i32) (param $elm i32) (result i32)
  (local $c i32)
  (local $p i32)
  (local $tname i32)
  (block $end_elimination (result i32)
  (block $wrap
  (block $union
  (block $record
  local.get $elm
  br_table $record $union $wrap $wrap
)
  local.get $fcx
  local.get $cx
  local.get $p
  call $hydra.inference.infer_type_of_projection
  br $end_elimination
)
  local.get $fcx
  local.get $cx
  local.get $c
  call $hydra.inference.infer_type_of_case_statement
  br $end_elimination
)
  local.get $fcx
  local.get $cx
  local.get $tname
  call $hydra.inference.infer_type_of_unwrap
  br $end_elimination
)
)
  (func $hydra.inference.infer_type_of_function (param $fcx i32) (param $cx i32) (param $f i32) (result i32)
  (local $elm i32)
  (local $l i32)
  (block $end_function (result i32)
  (block $lambda
  (block $elimination
  local.get $f
  br_table $elimination $lambda $lambda
)
  local.get $fcx
  local.get $cx
  local.get $elm
  call $hydra.inference.infer_type_of_elimination
  br $end_function
)
  local.get $fcx
  local.get $cx
  local.get $l
  call $hydra.inference.infer_type_of_lambda
  br $end_function
)
)
  (func $hydra.inference.infer_type_of_injection (param $fcx i32) (param $cx i32) (param $injection i32) (result i32)
  (local $fcx2 i32)
  (local $fcx3 i32)
  (local $field i32)
  (local $fname i32)
  (local $ftyp i32)
  (local $isubst i32)
  (local $iterm i32)
  (local $ityp i32)
  (local $mc_result i32)
  (local $result i32)
  (local $schema_type i32)
  (local $sfields i32)
  (local $st_rp i32)
  (local $stype i32)
  (local $subst i32)
  (local $svars i32)
  (local $term i32)
  (local $tname i32)
  (local $x i32)
  local.get $injection
  ;; project field: type_name
  local.set $tname
  local.get $injection
  ;; project field: field
  local.set $field
  local.get $field
  ;; project field: name
  local.set $fname
  local.get $field
  ;; project field: term
  local.set $term
  local.get $fcx
  local.get $cx
  local.get $term
  i32.const 0 ;; string: "injected term"
  call $hydra.inference.infer_type_of_term
  local.get $result
  ;; project field: context
  local.set $fcx2
  local.get $fcx2
  local.get $cx
  ;; project field: schema_types
  local.get $tname
  call $hydra.resolution.require_schema_type
  local.get $st_rp
  call $hydra.lib.pairs.first
  local.set $schema_type
  local.get $st_rp
  call $hydra.lib.pairs.second
  local.set $fcx3
  local.get $schema_type
  ;; project field: variables
  local.set $svars
  local.get $schema_type
  ;; project field: type
  local.set $stype
  local.get $result
  ;; project field: term
  local.set $iterm
  local.get $result
  ;; project field: type
  local.set $ityp
  local.get $result
  ;; project field: subst
  local.set $isubst
  local.get $fcx3
  local.get $tname
  local.get $stype
  call $hydra.extract.core.union_type
  local.get $fcx3
  local.get $fname
  local.get $sfields
  call $hydra.resolution.find_field_type
  local.get $fcx3
  local.get $cx
  local.get $fcx3
  local.get $svars
  local.get $tname
  local.get $fname
  local.get $iterm
  call $hydra.inference.build_type_application_term
  local.get $tname
  local.get $x
  local.get $svars
  call $hydra.lib.lists.map
  call $hydra.resolution.nominal_application
  local.get $isubst
  local.get $subst
  call $hydra.substitution.compose_type_subst
  call $hydra.inference.yield
  i32.const 1
  ;; list elements follow
  local.get $ftyp
  local.get $ityp
  i32.const 0 ;; string: "schema type of injected field"
  call $hydra.inference.map_constraints
  i32.const 1
  local.get $mc_result
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.inference.infer_type_of_lambda (param $fcx i32) (param $cx i32) (param $lambda i32) (result i32)
  (local $body i32)
  (local $cx2 i32)
  (local $cx3 i32)
  (local $dom i32)
  (local $fcx2 i32)
  (local $fcx3 i32)
  (local $icod i32)
  (local $iconstraints i32)
  (local $isubst i32)
  (local $iterm i32)
  (local $rdom i32)
  (local $result i32)
  (local $rterm i32)
  (local $rtype i32)
  (local $var i32)
  (local $vars i32)
  (local $vdom i32)
  (local $vdom_result i32)
  local.get $lambda
  ;; project field: parameter
  local.set $var
  local.get $lambda
  ;; project field: body
  local.set $body
  local.get $fcx
  call $hydra.names.fresh_name
  local.set $vdom_result
  local.get $vdom_result
  call $hydra.lib.pairs.first
  local.set $vdom
  local.get $vdom_result
  call $hydra.lib.pairs.second
  local.set $fcx2
  local.get $vdom
  local.set $dom
  i32.const 1
  ;; list elements follow
  local.get $var
  i32.const 0
  ;; list elements follow
  local.get $dom
  i32.const 0
  local.get $cx
  call $hydra.inference.extend_context
  local.set $cx2
  local.get $fcx2
  local.get $cx2
  local.get $body
  i32.const 0 ;; string: "lambda body"
  call $hydra.inference.infer_type_of_term
  local.get $result
  ;; project field: context
  local.set $fcx3
  local.get $result
  ;; project field: term
  local.set $iterm
  local.get $result
  ;; project field: type
  local.set $icod
  local.get $result
  ;; project field: subst
  local.set $isubst
  local.get $isubst
  local.get $dom
  call $hydra.substitution.subst_in_type
  local.set $rdom
  local.get $var
  local.get $rdom
  local.get $iterm
  local.set $rterm
  local.get $rdom
  local.get $icod
  local.set $rtype
  i32.const 3
  ;; list elements follow
  local.get $rdom
  call $hydra.variables.free_variables_in_type
  local.get $icod
  call $hydra.variables.free_variables_in_type
  local.get $isubst
  local.get $cx2
  call $hydra.substitution.subst_in_context
  call $hydra.inference.free_variables_in_context
  call $hydra.lib.sets.unions
  local.set $vars
  local.get $isubst
  local.get $cx
  call $hydra.substitution.subst_in_context
  local.set $cx3
  local.get $isubst
  local.get $result
  ;; project field: class_constraints
  call $hydra.substitution.subst_in_class_constraints
  local.set $iconstraints
  i32.const 1
  local.get $rterm
  local.get $rtype
  local.get $isubst
  local.get $iconstraints
  local.get $fcx3
  call $hydra.lib.eithers.bind
)
  (func $hydra.inference.infer_type_of_let (param $fcx0 i32) (param $cx i32) (param $let0 i32) (result i32)
  (local $adj_list i32)
  (local $b i32)
  (local $binding i32)
  (local $binding_list i32)
  (local $binding_map i32)
  (local $binding_map2 i32)
  (local $bindings0 i32)
  (local $bins i32)
  (local $body0 i32)
  (local $bs i32)
  (local $create_let i32)
  (local $e i32)
  (local $fcx i32)
  (local $fcx_r i32)
  (local $group i32)
  (local $groups i32)
  (local $helper i32)
  (local $iconstraints i32)
  (local $iresult i32)
  (local $isubst i32)
  (local $iterm i32)
  (local $itype i32)
  (local $l i32)
  (local $let_body i32)
  (local $level i32)
  (local $n i32)
  (local $name i32)
  (local $name_set i32)
  (local $names i32)
  (local $nonzero i32)
  (local $res i32)
  (local $restore_let i32)
  (local $result i32)
  (local $rewrite_result i32)
  (local $rewritten_let i32)
  (local $term i32)
  (local $term2 i32)
  (local $to_pair i32)
  i32.const 0 ;; string: "let"
  local.get $fcx0
  ;; project field: trace
  call $hydra.lib.lists.cons
  local.get $fcx0
  ;; project field: messages
  local.get $fcx0
  ;; project field: other
  local.set $fcx
  local.get $let0
  ;; project field: bindings
  local.set $bindings0
  local.get $let0
  ;; project field: body
  local.set $body0
  ;; project field: name
  local.get $bindings0
  call $hydra.lib.lists.map
  local.set $names
  local.get $names
  call $hydra.lib.sets.from_list
  local.set $name_set
  local.get $binding
  ;; project field: name
  local.set $name
  local.get $binding
  ;; project field: term
  local.set $term
  local.get $name
  local.get $n
  local.get $name_set
  call $hydra.lib.sets.member
  local.get $term
  call $hydra.variables.free_variables_in_term
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.filter
  local.set $to_pair
  local.get $to_pair
  local.get $bindings0
  call $hydra.lib.lists.map
  local.set $adj_list
  local.get $adj_list
  call $hydra.sorting.topological_sort_components
  local.set $groups
  local.get $names
  local.get $bindings0
  call $hydra.lib.lists.zip
  call $hydra.lib.maps.from_list
  local.set $binding_map
  local.get $n
  local.get $binding_map
  call $hydra.lib.maps.lookup
  local.get $group
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  local.get $e
  local.set $create_let
  local.get $create_let
  local.get $body0
  local.get $groups
  call $hydra.lib.lists.reverse
  call $hydra.lib.lists.foldl
  local.set $rewritten_let
  (block $end_term (result i32)
  (block $let
  local.get $term2
  br_table $let $let
)
  local.get $l
  ;; project field: bindings
  local.set $bs
  local.get $l
  ;; project field: body
  local.set $let_body
  local.get $level
  i32.const 1
  call $hydra.lib.math.sub
  i32.const 2
  ;; list elements follow
  local.get $bs
  local.get $bins
  call $hydra.lib.lists.concat
  local.get $let_body
  local.get $helper
  br $end_term
)
  local.set $nonzero
  local.get $level
  i32.const 0
  call $hydra.lib.equality.equal
  local.get $bins
  local.get $term
  local.get $term
  local.get $nonzero
  call $hydra.lib.logic.if_else
  local.set $helper
  local.get $groups
  call $hydra.lib.lists.length
  i32.const 0
  ;; list elements follow
  local.get $iterm
  local.get $helper
  local.set $result
  local.get $result
  call $hydra.lib.pairs.first
  local.set $binding_list
  local.get $result
  call $hydra.lib.pairs.second
  local.set $e
  local.get $b
  ;; project field: name
  local.get $b
  local.get $binding_list
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $binding_map2
  local.get $n
  local.get $binding_map2
  call $hydra.lib.maps.lookup
  local.get $names
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  local.get $e
  local.set $restore_let
  local.get $iresult
  ;; project field: context
  local.set $fcx_r
  local.get $iresult
  ;; project field: term
  local.set $iterm
  local.get $iresult
  ;; project field: type
  local.set $itype
  local.get $iresult
  ;; project field: subst
  local.set $isubst
  local.get $iresult
  ;; project field: class_constraints
  local.set $iconstraints
  local.get $iterm
  local.get $restore_let
  local.get $itype
  local.get $isubst
  local.get $iconstraints
  local.get $fcx_r
  local.set $rewrite_result
  (block $end_term (result i32)
  (block $let
  local.get $rewritten_let
  br_table $let $let
)
  local.get $fcx
  local.get $cx
  local.get $l
  call $hydra.inference.infer_type_of_let_normalized
  br $end_term
)
  local.set $res
  local.get $rewrite_result
  local.get $res
  call $hydra.lib.eithers.map
)
  (func $hydra.inference.infer_type_of_let_normalized (param $fcx0 i32) (param $cx0 i32) (param $let_term i32) (result i32)
  (local $_a i32)
  (local $_ic i32)
  (local $acc i32)
  (local $all_constraints i32)
  (local $all_inferred_constraints i32)
  (local $b i32)
  (local $binding_constraints_subst i32)
  (local $binding_pair i32)
  (local $bins0 i32)
  (local $bins1 i32)
  (local $bnames i32)
  (local $body0 i32)
  (local $body1 i32)
  (local $body_constraints i32)
  (local $body_result i32)
  (local $bterms1 i32)
  (local $bterms1_subst i32)
  (local $bvars i32)
  (local $bvars_result i32)
  (local $c i32)
  (local $composed_subst i32)
  (local $constraints_with_s2 i32)
  (local $create_binding i32)
  (local $cx1 i32)
  (local $fcx i32)
  (local $fcx2 i32)
  (local $fcx3 i32)
  (local $fcx4 i32)
  (local $final_ts i32)
  (local $g2 i32)
  (local $g2base i32)
  (local $inferred_constraints i32)
  (local $inferred_result i32)
  (local $ir_rp i32)
  (local $merged_constraints i32)
  (local $name i32)
  (local $name_ts_pair i32)
  (local $original_binding_constraints i32)
  (local $original_constraints_subst i32)
  (local $pair i32)
  (local $s1 i32)
  (local $s2 i32)
  (local $sbody i32)
  (local $st1 i32)
  (local $subst_and_constraints i32)
  (local $t i32)
  (local $tbins0 i32)
  (local $tbins1 i32)
  (local $tbody i32)
  (local $term i32)
  (local $ts i32)
  (local $tsbins1 i32)
  (local $type_lambda_term i32)
  (local $v i32)
  (local $x i32)
  i32.const 0 ;; string: "let-normalized"
  local.get $fcx0
  ;; project field: trace
  call $hydra.lib.lists.cons
  local.get $fcx0
  ;; project field: messages
  local.get $fcx0
  ;; project field: other
  local.set $fcx
  local.get $let_term
  ;; project field: bindings
  local.set $bins0
  local.get $let_term
  ;; project field: body
  local.set $body0
  ;; project field: name
  local.get $bins0
  call $hydra.lib.lists.map
  local.set $bnames
  local.get $bins0
  call $hydra.lib.lists.length
  local.get $fcx
  call $hydra.names.fresh_names
  local.set $bvars_result
  local.get $bvars_result
  call $hydra.lib.pairs.first
  local.set $bvars
  local.get $bvars_result
  call $hydra.lib.pairs.second
  local.set $fcx2
  local.get $x
  local.get $bvars
  call $hydra.lib.lists.map
  local.set $tbins0
  local.get $bnames
  i32.const 0
  ;; list elements follow
  local.get $t
  i32.const 0
  local.get $tbins0
  call $hydra.lib.lists.map
  call $hydra.lib.lists.zip
  local.get $cx0
  call $hydra.inference.extend_context
  local.set $cx1
  local.get $fcx2
  local.get $cx1
  local.get $bins0
  call $hydra.inference.infer_types_of_temporary_bindings
  local.get $ir_rp
  call $hydra.lib.pairs.first
  local.set $inferred_result
  local.get $ir_rp
  call $hydra.lib.pairs.second
  local.set $fcx3
  local.get $inferred_result
  call $hydra.lib.pairs.first
  local.set $bterms1
  local.get $inferred_result
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.set $tbins1
  local.get $inferred_result
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  local.set $subst_and_constraints
  local.get $subst_and_constraints
  call $hydra.lib.pairs.first
  local.set $s1
  local.get $subst_and_constraints
  call $hydra.lib.pairs.second
  local.set $inferred_constraints
  local.get $_ic
  ;; project field: object
  ;; project field: message
  local.get $_ic
  ;; project field: context
  local.get $_a
  local.get $fcx3
  local.get $cx0
  ;; project field: schema_types
  local.get $s1
  call $hydra.substitution.subst_in_type
  local.get $tbins0
  call $hydra.lib.lists.map
  local.get $tbins1
  i32.const 0 ;; string: "temporary type bindings"
  call $hydra.unification.unify_type_lists
  call $hydra.lib.eithers.bimap
  local.get $fcx3
  local.get $cx0
  local.get $s2
  call $hydra.checking.check_type_subst
  local.get $s1
  local.get $s2
  call $hydra.substitution.compose_type_subst
  local.get $cx0
  call $hydra.substitution.subst_in_context
  local.set $g2base
  local.get $s2
  local.get $inferred_constraints
  call $hydra.substitution.subst_in_class_constraints
  local.set $constraints_with_s2
  local.get $s1
  local.get $s2
  call $hydra.substitution.compose_type_subst
  local.set $composed_subst
  local.get $acc
  local.get $acc
  local.get $acc
  local.get $c
  call $hydra.inference.merge_class_constraints
  local.get $ts
  ;; project field: constraints
  call $hydra.lib.maybes.maybe
  local.get $b
  ;; project field: type
  call $hydra.lib.maybes.maybe
  call $hydra.lib.maps.empty
  local.get $bins0
  call $hydra.lib.lists.foldl
  local.set $original_binding_constraints
  local.get $composed_subst
  local.get $original_binding_constraints
  call $hydra.substitution.subst_in_class_constraints
  local.set $original_constraints_subst
  local.get $constraints_with_s2
  local.get $original_constraints_subst
  call $hydra.inference.merge_class_constraints
  local.set $all_inferred_constraints
  local.get $g2base
  ;; project field: class_constraints
  local.get $all_inferred_constraints
  call $hydra.inference.merge_class_constraints
  local.set $merged_constraints
  local.get $g2base
  ;; project field: bound_terms
  local.get $g2base
  ;; project field: bound_types
  local.get $merged_constraints
  local.get $g2base
  ;; project field: lambda_variables
  local.get $g2base
  ;; project field: metadata
  local.get $g2base
  ;; project field: primitives
  local.get $g2base
  ;; project field: schema_types
  local.get $g2base
  ;; project field: type_variables
  local.set $g2
  local.get $s2
  call $hydra.substitution.subst_types_in_term
  local.get $bterms1
  call $hydra.lib.lists.map
  local.set $bterms1_subst
  local.get $bnames
  local.get $g2
  local.get $s2
  local.get $t
  call $hydra.substitution.subst_in_type
  call $hydra.inference.generalize
  local.get $tbins1
  call $hydra.lib.lists.map
  call $hydra.lib.lists.zip
  local.set $tsbins1
  local.get $fcx3
  local.get $tsbins1
  local.get $g2
  call $hydra.inference.extend_context
  local.get $body0
  i32.const 0 ;; string: "let body"
  call $hydra.inference.infer_type_of_term
  local.get $body_result
  ;; project field: context
  local.set $fcx4
  local.get $body_result
  ;; project field: term
  local.set $body1
  local.get $body_result
  ;; project field: type
  local.set $tbody
  local.get $body_result
  ;; project field: subst
  local.set $sbody
  local.get $pair
  call $hydra.lib.pairs.first
  local.set $name
  local.get $pair
  call $hydra.lib.pairs.second
  local.set $ts
  local.get $name
  local.get $ts
  ;; project field: variables
  local.get $name
  call $hydra.inference.build_type_application_term
  local.get $tsbins1
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $st1
  local.get $binding_pair
  call $hydra.lib.pairs.first
  local.set $name_ts_pair
  local.get $binding_pair
  call $hydra.lib.pairs.second
  local.set $term
  local.get $name_ts_pair
  call $hydra.lib.pairs.first
  local.set $name
  local.get $name_ts_pair
  call $hydra.lib.pairs.second
  local.set $ts
  local.get $sbody
  local.get $ts
  call $hydra.substitution.subst_in_type_scheme
  local.set $final_ts
  local.get $v
  local.get $b
  local.get $st1
  local.get $term
  call $hydra.substitution.substitute_in_term
  local.get $final_ts
  ;; project field: variables
  call $hydra.lib.lists.reverse
  call $hydra.lib.lists.foldl
  local.set $type_lambda_term
  local.get $name
  local.get $sbody
  local.get $s2
  call $hydra.substitution.compose_type_subst
  local.get $type_lambda_term
  call $hydra.substitution.subst_types_in_term
  local.get $final_ts
  local.set $create_binding
  local.get $create_binding
  local.get $tsbins1
  local.get $bterms1_subst
  call $hydra.lib.lists.zip
  call $hydra.lib.lists.map
  local.set $bins1
  local.get $sbody
  local.get $body_result
  ;; project field: class_constraints
  call $hydra.substitution.subst_in_class_constraints
  local.set $body_constraints
  local.get $sbody
  local.get $constraints_with_s2
  call $hydra.substitution.subst_in_class_constraints
  local.set $binding_constraints_subst
  local.get $binding_constraints_subst
  local.get $body_constraints
  call $hydra.inference.merge_class_constraints
  local.set $all_constraints
  i32.const 1
  local.get $bins1
  local.get $body1
  local.get $tbody
  i32.const 3
  ;; list elements follow
  local.get $s1
  local.get $s2
  local.get $sbody
  call $hydra.substitution.compose_type_subst_list
  local.get $all_constraints
  local.get $fcx4
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.inference.infer_type_of_list (param $fcx i32) (param $cx i32) (result i32)
  (local $x i32)
  local.get $fcx
  local.get $cx
  local.get $x
  local.get $x
  i32.const 0 ;; string: "list element"
  call $hydra.lib.sets.empty
  call $hydra.inference.infer_type_of_collection
)
  (func $hydra.inference.infer_type_of_literal (param $fcx i32) (param $lit i32) (result i32)
  local.get $lit
  local.get $lit
  call $hydra.reflect.literal_type
  call $hydra.substitution.id_type_subst
  call $hydra.lib.maps.empty
  local.get $fcx
)
  (func $hydra.inference.infer_type_of_map (param $fcx i32) (param $cx i32) (param $m i32) (result i32)
  (local $all_map_constraints i32)
  (local $fcx2 i32)
  (local $fcx3 i32)
  (local $fcx4 i32)
  (local $fcx5 i32)
  (local $k i32)
  (local $k_elem_constraints i32)
  (local $k_results i32)
  (local $k_rp i32)
  (local $kcons i32)
  (local $key_constraints i32)
  (local $ksubst i32)
  (local $kterms i32)
  (local $ktypes i32)
  (local $kvar i32)
  (local $kvar_result i32)
  (local $mc_result i32)
  (local $subst i32)
  (local $t i32)
  (local $v i32)
  (local $v_elem_constraints i32)
  (local $v_results i32)
  (local $v_rp i32)
  (local $vcons i32)
  (local $vsubst i32)
  (local $vterms i32)
  (local $vtypes i32)
  (local $vvar i32)
  (local $vvar_result i32)
  local.get $fcx
  call $hydra.names.fresh_name
  local.set $kvar_result
  local.get $kvar_result
  call $hydra.lib.pairs.first
  local.set $kvar
  local.get $kvar_result
  call $hydra.lib.pairs.second
  local.set $fcx2
  local.get $fcx2
  call $hydra.names.fresh_name
  local.set $vvar_result
  local.get $vvar_result
  call $hydra.lib.pairs.first
  local.set $vvar
  local.get $vvar_result
  call $hydra.lib.pairs.second
  local.set $fcx3
  local.get $kvar
  i32.const 0 ;; string: "ordering"
  call $hydra.lib.sets.singleton
  call $hydra.lib.maps.singleton
  local.set $key_constraints
  local.get $m
  call $hydra.lib.maps.null
  i32.const 1
  local.get $fcx3
  i32.const 2
  ;; list elements follow
  local.get $kvar
  local.get $vvar
  call $hydra.lib.maps.empty
  call $hydra.inference.build_type_application_term
  local.get $kvar
  local.get $vvar
  call $hydra.substitution.id_type_subst
  local.get $key_constraints
  call $hydra.inference.yield_with_constraints
  local.get $fcx3
  local.get $cx
  local.get $k
  i32.const 0 ;; string: "map key"
  local.get $m
  call $hydra.lib.maps.keys
  call $hydra.lib.lists.map
  call $hydra.inference.infer_many
  local.get $k_rp
  call $hydra.lib.pairs.first
  local.set $k_results
  local.get $k_rp
  call $hydra.lib.pairs.second
  local.set $fcx4
  local.get $k_results
  call $hydra.lib.pairs.first
  local.set $kterms
  local.get $k_results
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.set $ktypes
  local.get $k_results
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.set $ksubst
  local.get $k_results
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  local.set $k_elem_constraints
  local.get $fcx4
  local.get $ksubst
  local.get $cx
  call $hydra.substitution.subst_in_context
  local.get $v
  i32.const 0 ;; string: "map value"
  local.get $m
  call $hydra.lib.maps.elems
  call $hydra.lib.lists.map
  call $hydra.inference.infer_many
  local.get $v_rp
  call $hydra.lib.pairs.first
  local.set $v_results
  local.get $v_rp
  call $hydra.lib.pairs.second
  local.set $fcx5
  local.get $v_results
  call $hydra.lib.pairs.first
  local.set $vterms
  local.get $v_results
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.set $vtypes
  local.get $v_results
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.set $vsubst
  local.get $v_results
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  local.set $v_elem_constraints
  local.get $kvar
  local.get $t
  i32.const 0 ;; string: "map key"
  local.get $ktypes
  call $hydra.lib.lists.map
  local.set $kcons
  local.get $vvar
  local.get $t
  i32.const 0 ;; string: "map value"
  local.get $vtypes
  call $hydra.lib.lists.map
  local.set $vcons
  local.get $key_constraints
  local.get $k_elem_constraints
  local.get $v_elem_constraints
  call $hydra.inference.merge_class_constraints
  call $hydra.inference.merge_class_constraints
  local.set $all_map_constraints
  local.get $fcx5
  local.get $cx
  local.get $fcx5
  local.get $kterms
  local.get $vterms
  call $hydra.lib.lists.zip
  call $hydra.lib.maps.from_list
  local.get $kvar
  local.get $vvar
  i32.const 3
  ;; list elements follow
  local.get $ksubst
  local.get $vsubst
  local.get $subst
  call $hydra.substitution.compose_type_subst_list
  local.get $subst
  local.get $all_map_constraints
  call $hydra.substitution.subst_in_class_constraints
  call $hydra.inference.yield_with_constraints
  i32.const 2
  ;; list elements follow
  local.get $kcons
  local.get $vcons
  call $hydra.lib.lists.concat
  call $hydra.inference.map_constraints
  i32.const 1
  local.get $mc_result
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.logic.if_else
)
  (func $hydra.inference.infer_type_of_optional (param $fcx i32) (param $cx i32) (param $m i32) (result i32)
  (local $terms i32)
  (local $trm_cons i32)
  (local $x i32)
  local.get $terms
  call $hydra.lib.lists.null
  i32.const 0
  local.get $terms
  call $hydra.lib.lists.head
  call $hydra.lib.logic.if_else
  local.set $trm_cons
  local.get $fcx
  local.get $cx
  local.get $x
  local.get $trm_cons
  i32.const 0 ;; string: "optional element"
  call $hydra.lib.sets.empty
  i32.const 0
  ;; list elements follow
  call $hydra.lib.lists.singleton
  local.get $m
  call $hydra.lib.maybes.maybe
  call $hydra.inference.infer_type_of_collection
)
  (func $hydra.inference.infer_type_of_pair (param $fcx i32) (param $cx i32) (param $p i32) (result i32)
  (local $fcx2 i32)
  (local $ifst i32)
  (local $isnd i32)
  (local $isubst i32)
  (local $iterms i32)
  (local $itypes i32)
  (local $pair_elem_constraints i32)
  (local $pair_term i32)
  (local $results i32)
  (local $rp i32)
  (local $term_with_types i32)
  (local $ty_fst i32)
  (local $ty_snd i32)
  local.get $fcx
  local.get $cx
  i32.const 2
  ;; list elements follow
  local.get $p
  call $hydra.lib.pairs.first
  i32.const 0 ;; string: "pair first element"
  local.get $p
  call $hydra.lib.pairs.second
  i32.const 0 ;; string: "pair second element"
  call $hydra.inference.infer_many
  local.get $rp
  call $hydra.lib.pairs.first
  local.set $results
  local.get $rp
  call $hydra.lib.pairs.second
  local.set $fcx2
  local.get $results
  call $hydra.lib.pairs.first
  local.set $iterms
  local.get $results
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.set $itypes
  local.get $results
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.set $isubst
  local.get $results
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  local.set $pair_elem_constraints
  local.get $iterms
  call $hydra.lib.lists.head
  local.set $ifst
  local.get $iterms
  call $hydra.lib.lists.tail
  call $hydra.lib.lists.head
  local.set $isnd
  local.get $itypes
  call $hydra.lib.lists.head
  local.set $ty_fst
  local.get $itypes
  call $hydra.lib.lists.tail
  call $hydra.lib.lists.head
  local.set $ty_snd
  local.get $ifst
  local.get $isnd
  local.set $pair_term
  local.get $pair_term
  local.get $ty_fst
  local.get $ty_snd
  local.set $term_with_types
  i32.const 1
  local.get $fcx2
  local.get $term_with_types
  local.get $ty_fst
  local.get $ty_snd
  local.get $isubst
  local.get $pair_elem_constraints
  call $hydra.inference.yield_with_constraints
  call $hydra.lib.eithers.bind
)
  (func $hydra.inference.infer_type_of_primitive (param $fcx i32) (param $cx i32) (param $name i32) (result i32)
  (local $constraints i32)
  (local $fcx2 i32)
  (local $scheme i32)
  (local $ts i32)
  (local $ts_result i32)
  i32.const 0
  i32.const 0 ;; string: "No such primitive: "
  nop
  call $hydra.lib.strings.cat2
  local.get $fcx
  local.get $fcx
  local.get $scheme
  call $hydra.resolution.instantiate_type_scheme
  local.set $ts_result
  local.get $ts_result
  call $hydra.lib.pairs.first
  local.set $ts
  local.get $ts_result
  call $hydra.lib.pairs.second
  local.set $fcx2
  call $hydra.lib.maps.empty
  local.get $ts
  ;; project field: constraints
  call $hydra.lib.maybes.from_maybe
  local.set $constraints
  i32.const 1
  local.get $fcx2
  local.get $ts
  ;; project field: variables
  local.get $name
  call $hydra.inference.build_type_application_term
  local.get $ts
  ;; project field: type
  call $hydra.substitution.id_type_subst
  local.get $constraints
  call $hydra.inference.yield_checked_with_constraints
  ;; project field: type
  local.get $name
  local.get $cx
  ;; project field: primitives
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.map
  call $hydra.lib.maybes.maybe
)
  (func $hydra.inference.infer_type_of_projection (param $fcx i32) (param $cx i32) (param $proj i32) (result i32)
  (local $fcx2 i32)
  (local $fname i32)
  (local $ftyp i32)
  (local $schema_type i32)
  (local $sfields i32)
  (local $st_rp i32)
  (local $stype i32)
  (local $svars i32)
  (local $tname i32)
  (local $x i32)
  local.get $proj
  ;; project field: type_name
  local.set $tname
  local.get $proj
  ;; project field: field
  local.set $fname
  local.get $fcx
  local.get $cx
  ;; project field: schema_types
  local.get $tname
  call $hydra.resolution.require_schema_type
  local.get $st_rp
  call $hydra.lib.pairs.first
  local.set $schema_type
  local.get $st_rp
  call $hydra.lib.pairs.second
  local.set $fcx2
  local.get $schema_type
  ;; project field: variables
  local.set $svars
  local.get $schema_type
  ;; project field: type
  local.set $stype
  local.get $fcx2
  local.get $tname
  local.get $stype
  call $hydra.extract.core.record_type
  local.get $fcx2
  local.get $fname
  local.get $sfields
  call $hydra.resolution.find_field_type
  i32.const 1
  local.get $fcx2
  local.get $svars
  local.get $tname
  local.get $fname
  call $hydra.inference.build_type_application_term
  local.get $tname
  local.get $x
  local.get $svars
  call $hydra.lib.lists.map
  call $hydra.resolution.nominal_application
  local.get $ftyp
  call $hydra.substitution.id_type_subst
  call $hydra.inference.yield
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.inference.infer_type_of_record (param $fcx i32) (param $cx i32) (param $record i32) (result i32)
  (local $f i32)
  (local $fcx2 i32)
  (local $fcx3 i32)
  (local $fields i32)
  (local $fnames i32)
  (local $isubst i32)
  (local $iterms i32)
  (local $ityp i32)
  (local $itypes i32)
  (local $mc_result i32)
  (local $n i32)
  (local $rec_elem_constraints i32)
  (local $results i32)
  (local $rp i32)
  (local $schema_type i32)
  (local $st_rp i32)
  (local $stype i32)
  (local $subst i32)
  (local $svars i32)
  (local $t i32)
  (local $tname i32)
  (local $x i32)
  local.get $record
  ;; project field: type_name
  local.set $tname
  local.get $record
  ;; project field: fields
  local.set $fields
  ;; project field: name
  local.get $fields
  call $hydra.lib.lists.map
  local.set $fnames
  local.get $fcx
  local.get $cx
  ;; project field: schema_types
  local.get $tname
  call $hydra.resolution.require_schema_type
  local.get $st_rp
  call $hydra.lib.pairs.first
  local.set $schema_type
  local.get $st_rp
  call $hydra.lib.pairs.second
  local.set $fcx2
  local.get $fcx2
  local.get $cx
  local.get $f
  ;; project field: term
  i32.const 0 ;; string: "field "
  nop
  call $hydra.lib.strings.cat2
  local.get $fields
  call $hydra.lib.lists.map
  call $hydra.inference.infer_many
  local.get $rp
  call $hydra.lib.pairs.first
  local.set $results
  local.get $rp
  call $hydra.lib.pairs.second
  local.set $fcx3
  local.get $schema_type
  ;; project field: variables
  local.set $svars
  local.get $schema_type
  ;; project field: type
  local.set $stype
  local.get $results
  call $hydra.lib.pairs.first
  local.set $iterms
  local.get $results
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.set $itypes
  local.get $results
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.set $isubst
  local.get $results
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  local.set $rec_elem_constraints
  local.get $n
  local.get $t
  local.get $fnames
  local.get $itypes
  call $hydra.lib.lists.zip_with
  local.set $ityp
  local.get $fcx3
  local.get $cx
  local.get $fcx3
  local.get $svars
  local.get $tname
  local.get $n
  local.get $t
  local.get $fnames
  local.get $iterms
  call $hydra.lib.lists.zip_with
  call $hydra.inference.build_type_application_term
  local.get $tname
  local.get $x
  local.get $svars
  call $hydra.lib.lists.map
  call $hydra.resolution.nominal_application
  local.get $isubst
  local.get $subst
  call $hydra.substitution.compose_type_subst
  local.get $subst
  local.get $rec_elem_constraints
  call $hydra.substitution.subst_in_class_constraints
  call $hydra.inference.yield_with_constraints
  i32.const 1
  ;; list elements follow
  local.get $stype
  local.get $ityp
  i32.const 0 ;; string: "schema type of record"
  call $hydra.inference.map_constraints
  i32.const 1
  local.get $mc_result
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.inference.infer_type_of_set (param $fcx i32) (param $cx i32) (param $s i32) (result i32)
  (local $terms i32)
  (local $x i32)
  local.get $fcx
  local.get $cx
  local.get $x
  local.get $terms
  call $hydra.lib.sets.from_list
  i32.const 0 ;; string: "set element"
  i32.const 0 ;; string: "ordering"
  call $hydra.lib.sets.singleton
  local.get $s
  call $hydra.lib.sets.to_list
  call $hydra.inference.infer_type_of_collection
)
  (func $hydra.inference.infer_type_of_term (param $fcx i32) (param $cx i32) (param $term i32) (param $desc i32) (result i32)
  (local $a i32)
  (local $e i32)
  (local $els i32)
  (local $f i32)
  (local $fcx2 i32)
  (local $i i32)
  (local $l i32)
  (local $m i32)
  (local $name i32)
  (local $p i32)
  (local $r i32)
  (local $s i32)
  (local $ta i32)
  (local $tt i32)
  (local $w i32)
  local.get $desc
  local.get $fcx
  ;; project field: trace
  call $hydra.lib.lists.cons
  local.get $fcx
  ;; project field: messages
  local.get $fcx
  ;; project field: other
  local.set $fcx2
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
  local.get $term
  br_table $annotated $application $either $function $let $list $literal $map $maybe $pair $record $set $type_application $type_lambda $union $unit $variable $wrap $wrap
)
  local.get $fcx2
  local.get $cx
  local.get $a
  call $hydra.inference.infer_type_of_annotated_term
  br $end_term
)
  local.get $fcx2
  local.get $cx
  local.get $a
  call $hydra.inference.infer_type_of_application
  br $end_term
)
  local.get $fcx2
  local.get $cx
  local.get $e
  call $hydra.inference.infer_type_of_either
  br $end_term
)
  local.get $fcx2
  local.get $cx
  local.get $f
  call $hydra.inference.infer_type_of_function
  br $end_term
)
  local.get $fcx2
  local.get $cx
  local.get $l
  call $hydra.inference.infer_type_of_let
  br $end_term
)
  local.get $fcx2
  local.get $cx
  local.get $els
  call $hydra.inference.infer_type_of_list
  br $end_term
)
  i32.const 1
  local.get $fcx2
  local.get $l
  call $hydra.inference.infer_type_of_literal
  br $end_term
)
  local.get $fcx2
  local.get $cx
  local.get $m
  call $hydra.inference.infer_type_of_map
  br $end_term
)
  local.get $fcx2
  local.get $cx
  local.get $m
  call $hydra.inference.infer_type_of_optional
  br $end_term
)
  local.get $fcx2
  local.get $cx
  local.get $p
  call $hydra.inference.infer_type_of_pair
  br $end_term
)
  local.get $fcx2
  local.get $cx
  local.get $r
  call $hydra.inference.infer_type_of_record
  br $end_term
)
  local.get $fcx2
  local.get $cx
  local.get $s
  call $hydra.inference.infer_type_of_set
  br $end_term
)
  local.get $fcx2
  local.get $cx
  local.get $tt
  call $hydra.inference.infer_type_of_type_application
  br $end_term
)
  local.get $fcx2
  local.get $cx
  local.get $ta
  call $hydra.inference.infer_type_of_type_lambda
  br $end_term
)
  local.get $fcx2
  local.get $cx
  local.get $i
  call $hydra.inference.infer_type_of_injection
  br $end_term
)
  i32.const 1
  local.get $fcx2
  call $hydra.inference.infer_type_of_unit
  br $end_term
)
  local.get $fcx2
  local.get $cx
  local.get $name
  call $hydra.inference.infer_type_of_variable
  br $end_term
)
  local.get $fcx2
  local.get $cx
  local.get $w
  call $hydra.inference.infer_type_of_wrapped_term
  br $end_term
)
)
  (func $hydra.inference.infer_type_of_type_application (param $fcx i32) (param $cx i32) (param $tt i32) (result i32)
  local.get $fcx
  local.get $cx
  local.get $tt
  ;; project field: body
  i32.const 0 ;; string: "type application term"
  call $hydra.inference.infer_type_of_term
)
  (func $hydra.inference.infer_type_of_type_lambda (param $fcx i32) (param $cx i32) (param $ta i32) (result i32)
  local.get $fcx
  local.get $cx
  local.get $ta
  ;; project field: body
  i32.const 0 ;; string: "type abstraction"
  call $hydra.inference.infer_type_of_term
)
  (func $hydra.inference.infer_type_of_unit (param $fcx i32) (result i32)
  i32.const 0
  i32.const 0
  call $hydra.substitution.id_type_subst
  call $hydra.lib.maps.empty
  local.get $fcx
)
  (func $hydra.inference.infer_type_of_unwrap (param $fcx i32) (param $cx i32) (param $tname i32) (result i32)
  (local $fcx2 i32)
  (local $schema_type i32)
  (local $st_rp i32)
  (local $stype i32)
  (local $svars i32)
  (local $wtyp i32)
  (local $x i32)
  local.get $fcx
  local.get $cx
  ;; project field: schema_types
  local.get $tname
  call $hydra.resolution.require_schema_type
  local.get $st_rp
  call $hydra.lib.pairs.first
  local.set $schema_type
  local.get $st_rp
  call $hydra.lib.pairs.second
  local.set $fcx2
  local.get $schema_type
  ;; project field: variables
  local.set $svars
  local.get $schema_type
  ;; project field: type
  local.set $stype
  local.get $fcx2
  local.get $tname
  local.get $stype
  call $hydra.extract.core.wrapped_type
  i32.const 1
  local.get $fcx2
  local.get $svars
  local.get $tname
  call $hydra.inference.build_type_application_term
  local.get $tname
  local.get $x
  local.get $svars
  call $hydra.lib.lists.map
  call $hydra.resolution.nominal_application
  local.get $wtyp
  call $hydra.substitution.id_type_subst
  call $hydra.inference.yield
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.inference.infer_type_of_variable (param $fcx i32) (param $cx i32) (param $name i32) (result i32)
  (local $constraints i32)
  (local $fcx2 i32)
  (local $scheme i32)
  (local $ts i32)
  (local $ts_result i32)
  i32.const 0
  i32.const 0 ;; string: "Variable not bound to type: "
  nop
  call $hydra.lib.strings.cat2
  local.get $fcx
  local.get $fcx
  local.get $scheme
  call $hydra.resolution.instantiate_type_scheme
  local.set $ts_result
  local.get $ts_result
  call $hydra.lib.pairs.first
  local.set $ts
  local.get $ts_result
  call $hydra.lib.pairs.second
  local.set $fcx2
  call $hydra.lib.maps.empty
  local.get $ts
  ;; project field: constraints
  call $hydra.lib.maybes.from_maybe
  local.set $constraints
  i32.const 1
  local.get $fcx2
  local.get $ts
  ;; project field: variables
  local.get $name
  call $hydra.inference.build_type_application_term
  local.get $ts
  ;; project field: type
  call $hydra.substitution.id_type_subst
  local.get $constraints
  call $hydra.inference.yield_checked_with_constraints
  ;; project field: type
  local.get $name
  local.get $cx
  ;; project field: primitives
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.map
  call $hydra.lib.maybes.maybe
  local.get $fcx
  local.get $scheme
  call $hydra.resolution.instantiate_type_scheme
  local.set $ts_result
  local.get $ts_result
  call $hydra.lib.pairs.first
  local.set $ts
  local.get $ts_result
  call $hydra.lib.pairs.second
  local.set $fcx2
  call $hydra.lib.maps.empty
  local.get $ts
  ;; project field: constraints
  call $hydra.lib.maybes.from_maybe
  local.set $constraints
  i32.const 1
  local.get $ts
  ;; project field: variables
  local.get $name
  call $hydra.inference.build_type_application_term
  local.get $ts
  ;; project field: type
  call $hydra.substitution.id_type_subst
  local.get $constraints
  local.get $fcx2
  local.get $name
  local.get $cx
  ;; project field: bound_types
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
)
  (func $hydra.inference.infer_type_of_wrapped_term (param $fcx i32) (param $cx i32) (param $wt i32) (result i32)
  (local $fcx2 i32)
  (local $fcx3 i32)
  (local $isubst i32)
  (local $iterm i32)
  (local $ityp i32)
  (local $itype i32)
  (local $mc_result i32)
  (local $result i32)
  (local $schema_type i32)
  (local $st_rp i32)
  (local $stype i32)
  (local $subst i32)
  (local $svars i32)
  (local $term i32)
  (local $tname i32)
  (local $x i32)
  local.get $wt
  ;; project field: type_name
  local.set $tname
  local.get $wt
  ;; project field: body
  local.set $term
  local.get $fcx
  local.get $cx
  ;; project field: schema_types
  local.get $tname
  call $hydra.resolution.require_schema_type
  local.get $st_rp
  call $hydra.lib.pairs.first
  local.set $schema_type
  local.get $st_rp
  call $hydra.lib.pairs.second
  local.set $fcx2
  local.get $fcx2
  local.get $cx
  local.get $term
  i32.const 0 ;; string: "wrapped term"
  call $hydra.inference.infer_type_of_term
  local.get $result
  ;; project field: context
  local.set $fcx3
  local.get $schema_type
  ;; project field: variables
  local.set $svars
  local.get $schema_type
  ;; project field: type
  local.set $stype
  local.get $result
  ;; project field: term
  local.set $iterm
  local.get $result
  ;; project field: type
  local.set $itype
  local.get $result
  ;; project field: subst
  local.set $isubst
  local.get $itype
  local.set $ityp
  local.get $fcx3
  local.get $cx
  local.get $fcx3
  local.get $svars
  local.get $tname
  local.get $iterm
  call $hydra.inference.build_type_application_term
  local.get $tname
  local.get $x
  local.get $svars
  call $hydra.lib.lists.map
  call $hydra.resolution.nominal_application
  local.get $isubst
  local.get $subst
  call $hydra.substitution.compose_type_subst
  call $hydra.inference.yield
  i32.const 1
  ;; list elements follow
  local.get $stype
  local.get $ityp
  i32.const 0 ;; string: "schema type of wrapper"
  call $hydra.inference.map_constraints
  i32.const 1
  local.get $mc_result
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.inference.infer_types_of_temporary_bindings (param $fcx i32) (param $cx i32) (param $bins i32) (result i32)
  (local $_a i32)
  (local $_ic i32)
  (local $binding i32)
  (local $c1 i32)
  (local $c1_inferred i32)
  (local $c1_subst i32)
  (local $c2 i32)
  (local $dflt i32)
  (local $fcx2 i32)
  (local $fcx3 i32)
  (local $fresh_constraints i32)
  (local $h i32)
  (local $instantiated_ts i32)
  (local $j i32)
  (local $k i32)
  (local $merged_constraints i32)
  (local $original_binding_constraints i32)
  (local $r i32)
  (local $r_prime i32)
  (local $rest_pair i32)
  (local $result1 i32)
  (local $result2 i32)
  (local $rp2 i32)
  (local $tl i32)
  (local $ts i32)
  (local $ts_result i32)
  (local $u i32)
  (local $u_prime i32)
  (local $unify_subst i32)
  (local $v i32)
  local.get $bins
  call $hydra.lib.lists.null
  i32.const 1
  i32.const 0
  ;; list elements follow
  i32.const 0
  ;; list elements follow
  call $hydra.substitution.id_type_subst
  call $hydra.lib.maps.empty
  local.get $fcx
  local.get $bins
  call $hydra.lib.lists.head
  local.set $binding
  local.get $binding
  ;; project field: name
  local.set $k
  local.get $binding
  ;; project field: term
  local.set $v
  local.get $bins
  call $hydra.lib.lists.tail
  local.set $tl
  local.get $fcx
  local.get $cx
  local.get $v
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "temporary let binding '"
  nop
  i32.const 0 ;; string: "'"
  call $hydra.lib.strings.cat
  call $hydra.inference.infer_type_of_term
  local.get $result1
  ;; project field: context
  local.set $fcx2
  local.get $result1
  ;; project field: term
  local.set $j
  local.get $result1
  ;; project field: type
  local.set $u_prime
  local.get $result1
  ;; project field: subst
  local.set $u
  local.get $result1
  ;; project field: class_constraints
  local.set $c1_inferred
  i32.const 1
  call $hydra.lib.maps.empty
  local.get $fcx2
  local.get $ts
  call $hydra.resolution.instantiate_type_scheme
  local.set $ts_result
  local.get $ts_result
  call $hydra.lib.pairs.first
  local.set $instantiated_ts
  call $hydra.lib.maps.empty
  local.get $instantiated_ts
  ;; project field: constraints
  call $hydra.lib.maybes.from_maybe
  local.set $fresh_constraints
  local.get $_ic
  ;; project field: object
  ;; project field: message
  local.get $_ic
  ;; project field: context
  local.get $_a
  local.get $fcx2
  local.get $cx
  ;; project field: schema_types
  local.get $instantiated_ts
  ;; project field: type
  local.get $u_prime
  i32.const 0 ;; string: "original binding type"
  call $hydra.unification.unify_types
  call $hydra.lib.eithers.bimap
  i32.const 1
  local.get $unify_subst
  local.get $fresh_constraints
  call $hydra.substitution.subst_in_class_constraints
  call $hydra.lib.eithers.bind
  local.get $binding
  ;; project field: type
  call $hydra.lib.maybes.maybe
  local.get $c1_inferred
  local.get $original_binding_constraints
  call $hydra.inference.merge_class_constraints
  local.set $c1
  local.get $fcx2
  local.get $u
  local.get $cx
  call $hydra.substitution.subst_in_context
  local.get $tl
  call $hydra.inference.infer_types_of_temporary_bindings
  local.get $rp2
  call $hydra.lib.pairs.first
  local.set $result2
  local.get $rp2
  call $hydra.lib.pairs.second
  local.set $fcx3
  local.get $result2
  call $hydra.lib.pairs.first
  local.set $h
  local.get $result2
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.set $r_prime
  local.get $result2
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  local.set $rest_pair
  local.get $rest_pair
  call $hydra.lib.pairs.first
  local.set $r
  local.get $rest_pair
  call $hydra.lib.pairs.second
  local.set $c2
  local.get $r
  local.get $c1
  call $hydra.substitution.subst_in_class_constraints
  local.set $c1_subst
  local.get $c1_subst
  local.get $c2
  call $hydra.inference.merge_class_constraints
  local.set $merged_constraints
  i32.const 1
  local.get $r
  local.get $j
  call $hydra.substitution.subst_types_in_term
  local.get $h
  call $hydra.lib.lists.cons
  local.get $r
  local.get $u_prime
  call $hydra.substitution.subst_in_type
  local.get $r_prime
  call $hydra.lib.lists.cons
  local.get $u
  local.get $r
  call $hydra.substitution.compose_type_subst
  local.get $merged_constraints
  local.get $fcx3
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  local.set $dflt
  local.get $dflt
  call $hydra.lib.logic.if_else
)
  (func $hydra.inference.is_unbound (param $cx i32) (param $v i32) (result i32)
  local.get $v
  local.get $cx
  call $hydra.inference.free_variables_in_context
  call $hydra.lib.sets.member
  call $hydra.lib.logic.not
  local.get $v
  local.get $cx
  ;; project field: schema_types
  call $hydra.lib.maps.member
  call $hydra.lib.logic.not
  call $hydra.lib.logic.and
)
  (func $hydra.inference.map_constraints (param $flow_cx i32) (param $cx i32) (param $f i32) (param $constraints i32) (result i32)
  (local $_a i32)
  (local $_ic i32)
  (local $s i32)
  local.get $_ic
  ;; project field: object
  ;; project field: message
  local.get $_ic
  ;; project field: context
  local.get $_a
  local.get $flow_cx
  local.get $cx
  ;; project field: schema_types
  local.get $constraints
  call $hydra.unification.unify_type_constraints
  call $hydra.lib.eithers.bimap
  local.get $flow_cx
  local.get $cx
  local.get $s
  call $hydra.checking.check_type_subst
  i32.const 1
  local.get $s
  local.get $f
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.inference.merge_class_constraints (param $m1 i32) (param $m2 i32) (result i32)
  (local $acc i32)
  (local $existing i32)
  (local $k i32)
  (local $merged i32)
  (local $pair i32)
  (local $v i32)
  local.get $pair
  call $hydra.lib.pairs.first
  local.set $k
  local.get $pair
  call $hydra.lib.pairs.second
  local.set $v
  local.get $k
  local.get $v
  local.get $acc
  call $hydra.lib.maps.insert
  local.get $existing
  ;; project field: classes
  local.get $v
  ;; project field: classes
  call $hydra.lib.sets.union
  local.set $merged
  local.get $k
  local.get $merged
  local.get $acc
  call $hydra.lib.maps.insert
  local.get $k
  local.get $acc
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  local.get $m1
  local.get $m2
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.foldl
)
  (func $hydra.inference.show_inference_result (param $result i32) (result i32)
  (local $subst i32)
  (local $term i32)
  (local $typ i32)
  local.get $result
  ;; project field: term
  local.set $term
  local.get $result
  ;; project field: type
  local.set $typ
  local.get $result
  ;; project field: subst
  local.set $subst
  i32.const 7
  ;; list elements follow
  i32.const 0 ;; string: "{term="
  local.get $term
  call $hydra.show.core.term
  i32.const 0 ;; string: ", type="
  local.get $typ
  call $hydra.show.core.type
  i32.const 0 ;; string: ", subst="
  local.get $subst
  call $hydra.show.typing.type_subst
  i32.const 0 ;; string: "}"
  call $hydra.lib.strings.cat
)
  (func $hydra.inference.yield (param $fcx i32) (param $term i32) (param $typ i32) (param $subst i32) (result i32)
  local.get $subst
  local.get $term
  call $hydra.substitution.subst_types_in_term
  local.get $subst
  local.get $typ
  call $hydra.substitution.subst_in_type
  local.get $subst
  call $hydra.lib.maps.empty
  local.get $fcx
)
  (func $hydra.inference.yield_checked (param $fcx i32) (param $term i32) (param $typ i32) (param $subst i32) (result i32)
  (local $iterm i32)
  (local $itype i32)
  local.get $subst
  local.get $term
  call $hydra.substitution.subst_types_in_term
  local.set $iterm
  local.get $subst
  local.get $typ
  call $hydra.substitution.subst_in_type
  local.set $itype
  local.get $iterm
  local.get $itype
  local.get $subst
  call $hydra.lib.maps.empty
  local.get $fcx
)
  (func $hydra.inference.yield_checked_with_constraints (param $fcx i32) (param $term i32) (param $typ i32) (param $subst i32) (param $constraints i32) (result i32)
  (local $iconstraints i32)
  (local $iterm i32)
  (local $itype i32)
  local.get $subst
  local.get $term
  call $hydra.substitution.subst_types_in_term
  local.set $iterm
  local.get $subst
  local.get $typ
  call $hydra.substitution.subst_in_type
  local.set $itype
  local.get $subst
  local.get $constraints
  call $hydra.substitution.subst_in_class_constraints
  local.set $iconstraints
  local.get $iterm
  local.get $itype
  local.get $subst
  local.get $iconstraints
  local.get $fcx
)
  (func $hydra.inference.yield_debug (param $fcx i32) (param $cx i32) (param $debug_id i32) (param $term i32) (param $typ i32) (param $subst i32) (result i32)
  (local $rterm i32)
  (local $rtyp i32)
  local.get $subst
  local.get $term
  call $hydra.substitution.subst_types_in_term
  local.set $rterm
  local.get $subst
  local.get $typ
  call $hydra.substitution.subst_in_type
  local.set $rtyp
  local.get $fcx
  local.get $debug_id
  i32.const 10
  ;; list elements follow
  i32.const 0 ;; string: "
	term: "
  local.get $term
  call $hydra.show.core.term
  i32.const 0 ;; string: "
	typ: "
  local.get $typ
  call $hydra.show.core.type
  i32.const 0 ;; string: "
	subst: "
  local.get $subst
  call $hydra.show.typing.type_subst
  i32.const 0 ;; string: "
	rterm: "
  local.get $rterm
  call $hydra.show.core.term
  i32.const 0 ;; string: "
	rtyp: "
  local.get $rtyp
  call $hydra.show.core.type
  call $hydra.lib.strings.cat
  call $hydra.annotations.debug_if
  i32.const 1
  local.get $rterm
  local.get $rtyp
  local.get $subst
  call $hydra.lib.maps.empty
  local.get $fcx
  call $hydra.lib.eithers.bind
)
  (func $hydra.inference.yield_with_constraints (param $fcx i32) (param $term i32) (param $typ i32) (param $subst i32) (param $constraints i32) (result i32)
  local.get $subst
  local.get $term
  call $hydra.substitution.subst_types_in_term
  local.get $subst
  local.get $typ
  call $hydra.substitution.subst_in_type
  local.get $subst
  local.get $constraints
  local.get $fcx
)
)
