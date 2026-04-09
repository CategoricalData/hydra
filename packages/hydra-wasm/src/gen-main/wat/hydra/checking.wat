(module
  (import "hydra.constants" "hydra.constants.debug_inference" (func $hydra.constants.debug_inference (param i32) (result i32) ) )
  (import "hydra.dependencies" "hydra.dependencies.replace_typedefs" (func $hydra.dependencies.replace_typedefs (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.function_type" (func $hydra.extract.core.function_type (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.record_type" (func $hydra.extract.core.record_type (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.union_type" (func $hydra.extract.core.union_type (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.wrapped_type" (func $hydra.extract.core.wrapped_type (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.dereference_schema_type" (func $hydra.lexical.dereference_schema_type (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_maybe" (func $hydra.lib.eithers.map_maybe (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.at" (func $hydra.lib.lists.at (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.filter" (func $hydra.lib.lists.filter (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.head" (func $hydra.lib.lists.head (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.pure" (func $hydra.lib.lists.pure (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.tail" (func $hydra.lib.lists.tail (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.zip" (func $hydra.lib.lists.zip (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.zip_with" (func $hydra.lib.lists.zip_with (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int32" (func $hydra.lib.literals.show_int32 (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.and" (func $hydra.lib.logic.and (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.or" (func $hydra.lib.logic.or (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.empty" (func $hydra.lib.maps.empty (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.insert" (func $hydra.lib.maps.insert (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.keys" (func $hydra.lib.maps.keys (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.map" (func $hydra.lib.maps.map (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.member" (func $hydra.lib.maps.member (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.null" (func $hydra.lib.maps.null (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.singleton" (func $hydra.lib.maps.singleton (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.size" (func $hydra.lib.maps.size (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.union" (func $hydra.lib.maps.union (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cat" (func $hydra.lib.maybes.cat (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.pure" (func $hydra.lib.maybes.pure (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.difference" (func $hydra.lib.sets.difference (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.empty" (func $hydra.lib.sets.empty (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.insert" (func $hydra.lib.sets.insert (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.intersection" (func $hydra.lib.sets.intersection (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.member" (func $hydra.lib.sets.member (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.null" (func $hydra.lib.sets.null (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.union" (func $hydra.lib.sets.union (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.fresh_name" (func $hydra.names.fresh_name (param i32) (result i32) ) )
  (import "hydra.reflect" "hydra.reflect.literal_type" (func $hydra.reflect.literal_type (param i32) (result i32) ) )
  (import "hydra.resolution" "hydra.resolution.find_field_type" (func $hydra.resolution.find_field_type (param i32) (result i32) ) )
  (import "hydra.resolution" "hydra.resolution.fully_strip_and_normalize_type" (func $hydra.resolution.fully_strip_and_normalize_type (param i32) (result i32) ) )
  (import "hydra.resolution" "hydra.resolution.instantiate_type" (func $hydra.resolution.instantiate_type (param i32) (result i32) ) )
  (import "hydra.resolution" "hydra.resolution.instantiate_type_scheme" (func $hydra.resolution.instantiate_type_scheme (param i32) (result i32) ) )
  (import "hydra.resolution" "hydra.resolution.nominal_application" (func $hydra.resolution.nominal_application (param i32) (result i32) ) )
  (import "hydra.resolution" "hydra.resolution.require_schema_type" (func $hydra.resolution.require_schema_type (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.fold_over_type" (func $hydra.rewriting.fold_over_type (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.subterms" (func $hydra.rewriting.subterms (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.f_type_to_type_scheme" (func $hydra.scoping.f_type_to_type_scheme (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.type_scheme_to_f_type" (func $hydra.scoping.type_scheme_to_f_type (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.type" (func $hydra.show.core.type (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_type" (func $hydra.strip.deannotate_type (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_type_recursive" (func $hydra.strip.deannotate_type_recursive (param i32) (result i32) ) )
  (import "hydra.substitution" "hydra.substitution.subst_in_type" (func $hydra.substitution.subst_in_type (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.free_variables_in_type" (func $hydra.variables.free_variables_in_type (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.free_variables_in_type_simple" (func $hydra.variables.free_variables_in_type_simple (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.substitute_type_variables" (func $hydra.variables.substitute_type_variables (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.checking.all_equal" (func $hydra.checking.all_equal) )
  (export "hydra.checking.apply_type_arguments_to_type" (func $hydra.checking.apply_type_arguments_to_type) )
  (export "hydra.checking.check_for_unbound_type_variables" (func $hydra.checking.check_for_unbound_type_variables) )
  (export "hydra.checking.check_nominal_application" (func $hydra.checking.check_nominal_application) )
  (export "hydra.checking.check_same_type" (func $hydra.checking.check_same_type) )
  (export "hydra.checking.check_type" (func $hydra.checking.check_type) )
  (export "hydra.checking.check_type_subst" (func $hydra.checking.check_type_subst) )
  (export "hydra.checking.check_type_variables" (func $hydra.checking.check_type_variables) )
  (export "hydra.checking.contains_in_scope_type_vars" (func $hydra.checking.contains_in_scope_type_vars) )
  (export "hydra.checking.normalize_type_free_vars" (func $hydra.checking.normalize_type_free_vars) )
  (export "hydra.checking.to_f_context" (func $hydra.checking.to_f_context) )
  (export "hydra.checking.type_lists_effectively_equal" (func $hydra.checking.type_lists_effectively_equal) )
  (export "hydra.checking.type_of" (func $hydra.checking.type_of) )
  (export "hydra.checking.type_of_annotated_term" (func $hydra.checking.type_of_annotated_term) )
  (export "hydra.checking.type_of_application" (func $hydra.checking.type_of_application) )
  (export "hydra.checking.type_of_case_statement" (func $hydra.checking.type_of_case_statement) )
  (export "hydra.checking.type_of_either" (func $hydra.checking.type_of_either) )
  (export "hydra.checking.type_of_injection" (func $hydra.checking.type_of_injection) )
  (export "hydra.checking.type_of_lambda" (func $hydra.checking.type_of_lambda) )
  (export "hydra.checking.type_of_let" (func $hydra.checking.type_of_let) )
  (export "hydra.checking.type_of_list" (func $hydra.checking.type_of_list) )
  (export "hydra.checking.type_of_literal" (func $hydra.checking.type_of_literal) )
  (export "hydra.checking.type_of_map" (func $hydra.checking.type_of_map) )
  (export "hydra.checking.type_of_maybe" (func $hydra.checking.type_of_maybe) )
  (export "hydra.checking.type_of_pair" (func $hydra.checking.type_of_pair) )
  (export "hydra.checking.type_of_primitive" (func $hydra.checking.type_of_primitive) )
  (export "hydra.checking.type_of_projection" (func $hydra.checking.type_of_projection) )
  (export "hydra.checking.type_of_record" (func $hydra.checking.type_of_record) )
  (export "hydra.checking.type_of_set" (func $hydra.checking.type_of_set) )
  (export "hydra.checking.type_of_term" (func $hydra.checking.type_of_term) )
  (export "hydra.checking.type_of_type_application" (func $hydra.checking.type_of_type_application) )
  (export "hydra.checking.type_of_type_lambda" (func $hydra.checking.type_of_type_lambda) )
  (export "hydra.checking.type_of_unit" (func $hydra.checking.type_of_unit) )
  (export "hydra.checking.type_of_unwrap" (func $hydra.checking.type_of_unwrap) )
  (export "hydra.checking.type_of_variable" (func $hydra.checking.type_of_variable) )
  (export "hydra.checking.type_of_wrapped_term" (func $hydra.checking.type_of_wrapped_term) )
  (export "hydra.checking.types_all_effectively_equal" (func $hydra.checking.types_all_effectively_equal) )
  (export "hydra.checking.types_effectively_equal" (func $hydra.checking.types_effectively_equal) )
  (func $hydra.checking.all_equal (param $els i32) (result i32)
  (local $b i32)
  (local $t i32)
  local.get $els
  call $hydra.lib.lists.null
  i32.const 1
  local.get $b
  local.get $t
  local.get $els
  call $hydra.lib.lists.head
  call $hydra.lib.equality.equal
  call $hydra.lib.logic.and
  i32.const 1
  local.get $els
  call $hydra.lib.lists.tail
  call $hydra.lib.lists.foldl
  call $hydra.lib.logic.if_else
)
  (func $hydra.checking.apply_type_arguments_to_type (param $cx i32) (param $tx i32) (param $type_args i32) (param $t i32) (result i32)
  (local $ft i32)
  (local $nonnull i32)
  (local $tbody i32)
  (local $v i32)
  local.get $type_args
  call $hydra.lib.lists.null
  i32.const 1
  local.get $t
  (block $end_type (result i32)
  (block $forall
  local.get $t
  br_table $forall $forall
)
  local.get $ft
  ;; project field: parameter
  local.set $v
  local.get $ft
  ;; project field: body
  local.set $tbody
  local.get $cx
  local.get $tx
  local.get $type_args
  call $hydra.lib.lists.tail
  local.get $v
  local.get $type_args
  call $hydra.lib.lists.head
  call $hydra.lib.maps.singleton
  local.get $tbody
  call $hydra.substitution.subst_in_type
  call $hydra.checking.apply_type_arguments_to_type
  br $end_type
)
  local.set $nonnull
  local.get $nonnull
  call $hydra.lib.logic.if_else
)
  (func $hydra.checking.check_for_unbound_type_variables (param $cx i32) (param $tx i32) (param $term0 i32) (result i32)
  (local $b i32)
  (local $badvars i32)
  (local $bterm i32)
  (local $check i32)
  (local $check_optional i32)
  (local $check_recursive i32)
  (local $dflt i32)
  (local $f i32)
  (local $for_binding i32)
  (local $freevars i32)
  (local $l i32)
  (local $lbinding i32)
  (local $m i32)
  (local $new_trace i32)
  (local $new_vars i32)
  (local $recurse i32)
  (local $svars i32)
  (local $term i32)
  (local $tl i32)
  (local $trace i32)
  (local $ts i32)
  (local $tt i32)
  (local $typ i32)
  (local $vars i32)
  local.get $tx
  ;; project field: schema_types
  call $hydra.lib.maps.keys
  call $hydra.lib.sets.from_list
  local.set $svars
  local.get $vars
  local.get $trace
  local.get $lbinding
  local.get $check_recursive
  local.set $recurse
  local.get $recurse
  local.get $term
  call $hydra.rewriting.subterms
  call $hydra.lib.eithers.map_list
  i32.const 1
  call $hydra.lib.eithers.bind
  local.set $dflt
  local.get $typ
  call $hydra.variables.free_variables_in_type
  local.set $freevars
  local.get $freevars
  local.get $vars
  call $hydra.lib.sets.difference
  local.get $svars
  call $hydra.lib.sets.difference
  local.set $badvars
  local.get $badvars
  call $hydra.lib.sets.null
  i32.const 1
  i32.const 0
  local.get $badvars
  local.get $typ
  local.get $cx
  call $hydra.lib.logic.if_else
  local.set $check
  local.get $check
  local.get $m
  call $hydra.lib.eithers.map_maybe
  i32.const 1
  call $hydra.lib.eithers.bind
  local.set $check_optional
  (block $end_term (result i32)
  (block $type_lambda
  (block $type_application
  (block $let
  (block $function
  local.get $term
  br_table $function $let $type_application $type_lambda $type_lambda
)
  (block $end_function (result i32)
  (block $lambda
  (block $elimination
  local.get $f
  br_table $elimination $lambda $lambda
)
  local.get $dflt
  br $end_function
)
  local.get $l
  ;; project field: domain
  local.get $check_optional
  local.get $l
  ;; project field: body
  local.get $recurse
  call $hydra.lib.eithers.bind
  br $end_function
)
  br $end_term
)
  local.get $b
  ;; project field: term
  local.set $bterm
  local.get $vars
  local.get $vars
  local.get $ts
  ;; project field: variables
  call $hydra.lib.sets.from_list
  call $hydra.lib.sets.union
  local.get $b
  ;; project field: type
  call $hydra.lib.maybes.maybe
  local.set $new_vars
  nop
  local.get $trace
  call $hydra.lib.lists.cons
  local.set $new_trace
  local.get $new_vars
  local.get $new_trace
  local.get $b
  local.get $bterm
  local.get $check_recursive
  local.set $for_binding
  local.get $for_binding
  local.get $l
  ;; project field: bindings
  call $hydra.lib.eithers.map_list
  local.get $l
  ;; project field: body
  local.get $recurse
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $tt
  ;; project field: type
  local.get $check
  local.get $tt
  ;; project field: body
  local.get $recurse
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $tl
  ;; project field: parameter
  local.get $check
  local.get $tl
  ;; project field: body
  local.get $recurse
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.set $check_recursive
  call $hydra.lib.sets.empty
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "top level"
  i32.const 0
  local.get $term0
  local.get $check_recursive
)
  (func $hydra.checking.check_nominal_application (param $cx i32) (param $tx i32) (param $tname i32) (param $type_args i32) (result i32)
  (local $argslen i32)
  (local $cx2 i32)
  (local $result i32)
  (local $schema_type i32)
  (local $vars i32)
  (local $varslen i32)
  local.get $cx
  local.get $tx
  ;; project field: schema_types
  local.get $tname
  call $hydra.resolution.require_schema_type
  local.get $result
  call $hydra.lib.pairs.first
  local.set $schema_type
  local.get $result
  call $hydra.lib.pairs.second
  local.set $cx2
  local.get $schema_type
  ;; project field: variables
  local.set $vars
  local.get $vars
  call $hydra.lib.lists.length
  local.set $varslen
  local.get $type_args
  call $hydra.lib.lists.length
  local.set $argslen
  local.get $varslen
  local.get $argslen
  call $hydra.lib.equality.equal
  i32.const 1
  local.get $cx2
  i32.const 0
  local.get $tname
  local.get $varslen
  local.get $argslen
  local.get $type_args
  local.get $cx2
  call $hydra.lib.logic.if_else
  call $hydra.lib.eithers.bind
)
  (func $hydra.checking.check_same_type (param $cx i32) (param $tx i32) (param $desc i32) (param $types i32) (result i32)
  local.get $tx
  local.get $types
  call $hydra.checking.types_all_effectively_equal
  i32.const 1
  local.get $types
  call $hydra.lib.lists.head
  i32.const 0
  local.get $types
  local.get $desc
  local.get $cx
  call $hydra.lib.logic.if_else
)
  (func $hydra.checking.check_type (param $cx i32) (param $tx i32) (param $term i32) (param $typ i32) (result i32)
  (local $_p i32)
  (local $t0 i32)
  (local $vars i32)
  local.get $tx
  ;; project field: type_variables
  local.set $vars
  call $hydra.constants.debug_inference
  local.get $_p
  call $hydra.lib.pairs.first
  local.get $cx
  local.get $tx
  i32.const 0
  ;; list elements follow
  local.get $term
  call $hydra.checking.type_of
  call $hydra.lib.eithers.map
  local.get $tx
  local.get $t0
  local.get $typ
  call $hydra.checking.types_effectively_equal
  i32.const 1
  i32.const 0
  local.get $typ
  local.get $t0
  local.get $cx
  call $hydra.lib.logic.if_else
  call $hydra.lib.eithers.bind
  i32.const 1
  call $hydra.lib.logic.if_else
)
  (func $hydra.checking.check_type_subst (param $cx i32) (param $tx i32) (param $subst i32) (result i32)
  (local $bad_pairs i32)
  (local $bad_vars i32)
  (local $is_nominal i32)
  (local $p i32)
  (local $print_pair i32)
  (local $s i32)
  (local $suspect_vars i32)
  (local $ts i32)
  (local $v i32)
  (local $vars i32)
  nop
  local.set $s
  local.get $s
  call $hydra.lib.maps.keys
  call $hydra.lib.sets.from_list
  local.set $vars
  local.get $vars
  local.get $tx
  ;; project field: schema_types
  call $hydra.lib.maps.keys
  call $hydra.lib.sets.from_list
  call $hydra.lib.sets.intersection
  local.set $suspect_vars
  (block $end_type (result i32)
  (block $wrap
  (block $union
  (block $record
  local.get $ts
  ;; project field: type
  call $hydra.strip.deannotate_type
  br_table $record $union $wrap $wrap
)
  i32.const 1
  br $end_type
)
  i32.const 1
  br $end_type
)
  i32.const 1
  br $end_type
)
  local.set $is_nominal
  i32.const 0
  local.get $is_nominal
  local.get $v
  local.get $tx
  ;; project field: schema_types
  call $hydra.lexical.dereference_schema_type
  call $hydra.lib.maybes.maybe
  local.get $suspect_vars
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.filter
  call $hydra.lib.sets.from_list
  local.set $bad_vars
  local.get $p
  call $hydra.lib.pairs.first
  local.get $bad_vars
  call $hydra.lib.sets.member
  local.get $s
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.filter
  local.set $bad_pairs
  nop
  i32.const 0 ;; string: " --> "
  call $hydra.lib.strings.cat2
  local.get $p
  call $hydra.lib.pairs.second
  call $hydra.show.core.type
  call $hydra.lib.strings.cat2
  local.set $print_pair
  local.get $bad_vars
  call $hydra.lib.sets.null
  i32.const 1
  local.get $subst
  i32.const 0
  local.get $subst
  local.get $cx
  call $hydra.lib.logic.if_else
)
  (func $hydra.checking.check_type_variables (param $_tx i32) (param $_typ i32) )
  (func $hydra.checking.contains_in_scope_type_vars (param $tx i32) (param $t i32) (result i32)
  (local $free_vars i32)
  (local $vars i32)
  local.get $tx
  ;; project field: type_variables
  local.set $vars
  local.get $t
  call $hydra.variables.free_variables_in_type_simple
  local.set $free_vars
  local.get $vars
  local.get $free_vars
  call $hydra.lib.sets.intersection
  call $hydra.lib.sets.null
  call $hydra.lib.logic.not
)
  (func $hydra.checking.normalize_type_free_vars (param $typ i32) (result i32)
  (local $acc i32)
  (local $collect_vars i32)
  (local $subst i32)
  (local $t i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $variable
  local.get $t
  br_table $variable $variable
)
  local.get $v
  local.get $acc
  call $hydra.lib.maps.member
  local.get $acc
  local.get $v
  i32.const 0 ;; string: "_tv"
  local.get $acc
  call $hydra.lib.maps.size
  call $hydra.lib.literals.show_int32
  call $hydra.lib.strings.cat2
  local.get $acc
  call $hydra.lib.maps.insert
  call $hydra.lib.logic.if_else
  br $end_type
)
  local.set $collect_vars
  i32.const 0
  local.get $collect_vars
  call $hydra.lib.maps.empty
  local.get $typ
  call $hydra.rewriting.fold_over_type
  local.set $subst
  local.get $subst
  local.get $typ
  call $hydra.variables.substitute_type_variables
)
  (func $hydra.checking.to_f_context (param $cx i32) (result i32)
  call $hydra.scoping.type_scheme_to_f_type
  local.get $cx
  ;; project field: bound_types
  call $hydra.lib.maps.map
)
  (func $hydra.checking.type_lists_effectively_equal (param $tx i32) (param $tlist1 i32) (param $tlist2 i32) (result i32)
  local.get $tlist1
  call $hydra.lib.lists.length
  local.get $tlist2
  call $hydra.lib.lists.length
  call $hydra.lib.equality.equal
  call $hydra.lib.logic.and
  i32.const 1
  local.get $tx
  call $hydra.checking.types_effectively_equal
  local.get $tlist1
  local.get $tlist2
  call $hydra.lib.lists.zip_with
  call $hydra.lib.lists.foldl
  i32.const 0
  call $hydra.lib.logic.if_else
)
  (func $hydra.checking.type_of (param $cx i32) (param $tx i32) (param $type_args i32) (param $term i32) (result i32)
  (local $cx1 i32)
  (local $elm i32)
  (local $f i32)
  (local $v i32)
  i32.const 0 ;; string: "typeOf"
  local.get $cx
  ;; project field: trace
  call $hydra.lib.lists.cons
  local.get $cx
  ;; project field: messages
  local.get $cx
  ;; project field: other
  local.set $cx1
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
  local.get $cx1
  local.get $tx
  local.get $type_args
  local.get $v
  call $hydra.checking.type_of_annotated_term
  br $end_term
)
  local.get $cx1
  local.get $tx
  local.get $type_args
  local.get $v
  call $hydra.checking.type_of_application
  br $end_term
)
  local.get $cx1
  local.get $tx
  local.get $type_args
  local.get $v
  call $hydra.checking.type_of_either
  br $end_term
)
  (block $end_function (result i32)
  (block $lambda
  (block $elimination
  local.get $f
  br_table $elimination $lambda $lambda
)
  (block $end_elimination (result i32)
  (block $wrap
  (block $union
  (block $record
  local.get $elm
  br_table $record $union $wrap $wrap
)
  local.get $cx1
  local.get $tx
  local.get $type_args
  local.get $v
  call $hydra.checking.type_of_projection
  br $end_elimination
)
  local.get $cx1
  local.get $tx
  local.get $type_args
  local.get $v
  call $hydra.checking.type_of_case_statement
  br $end_elimination
)
  local.get $cx1
  local.get $tx
  local.get $type_args
  local.get $v
  call $hydra.checking.type_of_unwrap
  br $end_elimination
)
  br $end_function
)
  local.get $cx1
  local.get $tx
  local.get $type_args
  local.get $v
  call $hydra.checking.type_of_lambda
  br $end_function
)
  br $end_term
)
  local.get $cx1
  local.get $tx
  local.get $type_args
  local.get $v
  call $hydra.checking.type_of_let
  br $end_term
)
  local.get $cx1
  local.get $tx
  local.get $type_args
  local.get $v
  call $hydra.checking.type_of_list
  br $end_term
)
  local.get $cx1
  local.get $tx
  local.get $type_args
  local.get $v
  call $hydra.checking.type_of_literal
  br $end_term
)
  local.get $cx1
  local.get $tx
  local.get $type_args
  local.get $v
  call $hydra.checking.type_of_map
  br $end_term
)
  local.get $cx1
  local.get $tx
  local.get $type_args
  local.get $v
  call $hydra.checking.type_of_maybe
  br $end_term
)
  local.get $cx1
  local.get $tx
  local.get $type_args
  local.get $v
  call $hydra.checking.type_of_pair
  br $end_term
)
  local.get $cx1
  local.get $tx
  local.get $type_args
  local.get $v
  call $hydra.checking.type_of_record
  br $end_term
)
  local.get $cx1
  local.get $tx
  local.get $type_args
  local.get $v
  call $hydra.checking.type_of_set
  br $end_term
)
  local.get $cx1
  local.get $tx
  local.get $type_args
  local.get $v
  call $hydra.checking.type_of_type_application
  br $end_term
)
  local.get $cx1
  local.get $tx
  local.get $type_args
  local.get $v
  call $hydra.checking.type_of_type_lambda
  br $end_term
)
  local.get $cx1
  local.get $tx
  local.get $type_args
  local.get $v
  call $hydra.checking.type_of_injection
  br $end_term
)
  local.get $cx1
  local.get $tx
  local.get $type_args
  call $hydra.checking.type_of_unit
  br $end_term
)
  local.get $cx1
  local.get $tx
  local.get $type_args
  local.get $v
  call $hydra.checking.type_of_variable
  br $end_term
)
  local.get $cx1
  local.get $tx
  local.get $type_args
  local.get $v
  call $hydra.checking.type_of_wrapped_term
  br $end_term
)
)
  (func $hydra.checking.type_of_annotated_term (param $cx i32) (param $tx i32) (param $type_args i32) (param $at i32) (result i32)
  local.get $cx
  local.get $tx
  local.get $type_args
  local.get $at
  ;; project field: body
  call $hydra.checking.type_of
)
  (func $hydra.checking.type_of_application (param $cx i32) (param $tx i32) (param $type_args i32) (param $app i32) (result i32)
  (local $applied i32)
  (local $arg i32)
  (local $cod i32)
  (local $cx0 i32)
  (local $cx1 i32)
  (local $cx2 i32)
  (local $cx3 i32)
  (local $cx4 i32)
  (local $dom i32)
  (local $fresh_n i32)
  (local $ft i32)
  (local $fun i32)
  (local $name_result i32)
  (local $result1 i32)
  (local $result2 i32)
  (local $result3 i32)
  (local $t i32)
  (local $targ i32)
  (local $tfun i32)
  (local $try_type i32)
  local.get $app
  ;; project field: function
  local.set $fun
  local.get $app
  ;; project field: argument
  local.set $arg
  (block $end_type (result i32)
  (block $variable
  (block $function
  (block $forall
  local.get $tfun
  br_table $forall $function $variable $variable
)
  local.get $cx0
  local.get $ft
  ;; project field: body
  local.get $targ
  local.get $try_type
  br $end_type
)
  local.get $ft
  ;; project field: domain
  local.set $dom
  local.get $ft
  ;; project field: codomain
  local.set $cod
  local.get $tx
  local.get $dom
  local.get $targ
  call $hydra.checking.types_effectively_equal
  i32.const 1
  local.get $cod
  local.get $cx0
  i32.const 0
  local.get $dom
  local.get $targ
  local.get $cx0
  call $hydra.lib.logic.if_else
  br $end_type
)
  local.get $cx0
  call $hydra.names.fresh_name
  local.set $name_result
  local.get $name_result
  call $hydra.lib.pairs.first
  local.set $fresh_n
  local.get $name_result
  call $hydra.lib.pairs.second
  local.set $cx1
  i32.const 1
  local.get $fresh_n
  local.get $cx1
  br $end_type
)
  local.set $try_type
  local.get $cx
  local.get $tx
  i32.const 0
  ;; list elements follow
  local.get $fun
  call $hydra.checking.type_of
  local.get $result1
  call $hydra.lib.pairs.first
  local.set $tfun
  local.get $result1
  call $hydra.lib.pairs.second
  local.set $cx2
  local.get $cx2
  local.get $tx
  i32.const 0
  ;; list elements follow
  local.get $arg
  call $hydra.checking.type_of
  local.get $result2
  call $hydra.lib.pairs.first
  local.set $targ
  local.get $result2
  call $hydra.lib.pairs.second
  local.set $cx3
  local.get $cx3
  local.get $tfun
  local.get $targ
  local.get $try_type
  local.get $result3
  call $hydra.lib.pairs.first
  local.set $t
  local.get $result3
  call $hydra.lib.pairs.second
  local.set $cx4
  local.get $cx4
  local.get $tx
  local.get $type_args
  local.get $t
  call $hydra.checking.apply_type_arguments_to_type
  i32.const 1
  local.get $applied
  local.get $cx4
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.checking.type_of_case_statement (param $cx i32) (param $tx i32) (param $type_args i32) (param $cs i32) (result i32)
  (local $acc i32)
  (local $acc_r i32)
  (local $cases i32)
  (local $cod i32)
  (local $cods i32)
  (local $cterms i32)
  (local $cx2 i32)
  (local $cx3 i32)
  (local $cx_a i32)
  (local $cx_b i32)
  (local $dflt i32)
  (local $dflt_result i32)
  (local $e i32)
  (local $fcods i32)
  (local $fcods_r i32)
  (local $fcods_result i32)
  (local $fold_r i32)
  (local $fold_result i32)
  (local $ft i32)
  (local $t i32)
  (local $t_result i32)
  (local $tcterms i32)
  (local $tdflt i32)
  (local $term i32)
  (local $tname i32)
  (local $types i32)
  local.get $cs
  ;; project field: type_name
  local.set $tname
  local.get $cs
  ;; project field: default
  local.set $dflt
  local.get $cs
  ;; project field: cases
  local.set $cases
  ;; project field: term
  local.get $cases
  call $hydra.lib.lists.map
  local.set $cterms
  local.get $cx
  local.get $tx
  i32.const 0
  ;; list elements follow
  local.get $e
  call $hydra.checking.type_of
  local.get $dflt
  call $hydra.lib.eithers.map_maybe
  call $hydra.lib.pairs.first
  local.get $dflt_result
  call $hydra.lib.maybes.map
  local.set $tdflt
  local.get $cx
  call $hydra.lib.pairs.second
  local.get $dflt_result
  call $hydra.lib.maybes.maybe
  local.set $cx2
  local.get $acc
  local.get $acc_r
  call $hydra.lib.pairs.first
  local.set $types
  local.get $acc_r
  call $hydra.lib.pairs.second
  local.set $cx_a
  local.get $cx_a
  local.get $tx
  i32.const 0
  ;; list elements follow
  local.get $term
  call $hydra.checking.type_of
  local.get $t_result
  call $hydra.lib.pairs.first
  local.set $t
  local.get $t_result
  call $hydra.lib.pairs.second
  local.set $cx_b
  i32.const 1
  local.get $types
  local.get $t
  call $hydra.lib.lists.pure
  call $hydra.lib.lists.concat2
  local.get $cx_b
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  i32.const 1
  i32.const 0
  ;; list elements follow
  local.get $cx2
  local.get $cterms
  call $hydra.lib.lists.foldl
  local.set $fold_result
  local.get $fold_result
  local.get $fold_r
  call $hydra.lib.pairs.first
  local.set $tcterms
  local.get $fold_r
  call $hydra.lib.pairs.second
  local.set $cx3
  local.get $acc
  local.get $acc_r
  call $hydra.lib.pairs.first
  local.set $cods
  local.get $cx3
  local.get $t
  call $hydra.extract.core.function_type
  i32.const 1
  local.get $cods
  local.get $ft
  ;; project field: codomain
  call $hydra.lib.lists.pure
  call $hydra.lib.lists.concat2
  local.get $cx3
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  i32.const 1
  i32.const 0
  ;; list elements follow
  local.get $cx3
  local.get $tcterms
  call $hydra.lib.lists.foldl
  local.set $fcods_result
  local.get $fcods_result
  local.get $fcods_r
  call $hydra.lib.pairs.first
  local.set $fcods
  local.get $tdflt
  call $hydra.lib.maybes.pure
  local.get $fcods
  call $hydra.lib.lists.map
  call $hydra.lib.lists.cons
  call $hydra.lib.maybes.cat
  local.set $cods
  local.get $cx3
  local.get $tx
  i32.const 0 ;; string: "case branches"
  local.get $cods
  call $hydra.checking.check_same_type
  i32.const 1
  local.get $tname
  local.get $type_args
  call $hydra.resolution.nominal_application
  local.get $cod
  local.get $cx3
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.checking.type_of_either (param $cx i32) (param $tx i32) (param $type_args i32) (param $et i32) (result i32)
  (local $cx2 i32)
  (local $left_term i32)
  (local $left_type i32)
  (local $n i32)
  (local $result i32)
  (local $right_term i32)
  (local $right_type i32)
  local.get $type_args
  call $hydra.lib.lists.length
  local.set $n
  local.get $n
  i32.const 2
  call $hydra.lib.equality.equal
  local.get $cx
  local.get $tx
  i32.const 0
  ;; list elements follow
  local.get $left_term
  call $hydra.checking.type_of
  local.get $result
  call $hydra.lib.pairs.first
  local.set $left_type
  local.get $result
  call $hydra.lib.pairs.second
  local.set $cx2
  i32.const 1
  local.get $left_type
  i32.const 1
  local.get $type_args
  call $hydra.lib.lists.at
  local.get $cx2
  call $hydra.lib.eithers.bind
  local.get $cx
  local.get $tx
  i32.const 0
  ;; list elements follow
  local.get $right_term
  call $hydra.checking.type_of
  local.get $result
  call $hydra.lib.pairs.first
  local.set $right_type
  local.get $result
  call $hydra.lib.pairs.second
  local.set $cx2
  i32.const 1
  i32.const 0
  local.get $type_args
  call $hydra.lib.lists.at
  local.get $right_type
  local.get $cx2
  call $hydra.lib.eithers.bind
  local.get $et
  call $hydra.lib.eithers.either
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 2
  local.get $n
  local.get $type_args
  local.get $cx
  call $hydra.lib.logic.if_else
)
  (func $hydra.checking.type_of_injection (param $cx i32) (param $tx i32) (param $type_args i32) (param $injection i32) (result i32)
  (local $cx2 i32)
  (local $field i32)
  (local $fname i32)
  (local $fterm i32)
  (local $sbody i32)
  (local $schema_result i32)
  (local $schema_type i32)
  (local $sfields i32)
  (local $svars i32)
  (local $tname i32)
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
  local.set $fterm
  local.get $cx
  local.get $tx
  ;; project field: schema_types
  local.get $tname
  call $hydra.resolution.require_schema_type
  local.get $schema_result
  call $hydra.lib.pairs.first
  local.set $schema_type
  local.get $schema_result
  call $hydra.lib.pairs.second
  local.set $cx2
  local.get $schema_type
  ;; project field: variables
  local.set $svars
  local.get $schema_type
  ;; project field: type
  local.set $sbody
  local.get $cx2
  local.get $tname
  local.get $sbody
  call $hydra.extract.core.union_type
  local.get $cx2
  local.get $fname
  local.get $sfields
  call $hydra.resolution.find_field_type
  i32.const 1
  local.get $tname
  local.get $type_args
  call $hydra.resolution.nominal_application
  local.get $cx2
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.checking.type_of_lambda (param $cx i32) (param $tx i32) (param $type_args i32) (param $l i32) (result i32)
  (local $applied i32)
  (local $body i32)
  (local $cod i32)
  (local $cod_result i32)
  (local $cx2 i32)
  (local $cx3 i32)
  (local $dom i32)
  (local $mdom i32)
  (local $tbody i32)
  (local $tbody_result i32)
  (local $types2 i32)
  (local $v i32)
  local.get $l
  ;; project field: parameter
  local.set $v
  local.get $l
  ;; project field: domain
  local.set $mdom
  local.get $l
  ;; project field: body
  local.set $body
  i32.const 0
  i32.const 0
  local.get $cx
  local.get $v
  local.get $dom
  call $hydra.scoping.f_type_to_type_scheme
  local.get $tx
  ;; project field: bound_types
  call $hydra.lib.maps.insert
  local.set $types2
  local.get $cx
  local.get $tx
  ;; project field: bound_terms
  local.get $types2
  local.get $tx
  ;; project field: class_constraints
  local.get $tx
  ;; project field: lambda_variables
  local.get $tx
  ;; project field: metadata
  local.get $tx
  ;; project field: primitives
  local.get $tx
  ;; project field: schema_types
  local.get $tx
  ;; project field: type_variables
  i32.const 0
  ;; list elements follow
  local.get $body
  call $hydra.checking.type_of
  local.get $cod_result
  call $hydra.lib.pairs.first
  local.set $cod
  local.get $cod_result
  call $hydra.lib.pairs.second
  local.set $cx2
  i32.const 1
  local.get $dom
  local.get $cod
  local.get $cx2
  call $hydra.lib.eithers.bind
  local.get $mdom
  call $hydra.lib.maybes.maybe
  local.get $tbody_result
  call $hydra.lib.pairs.first
  local.set $tbody
  local.get $tbody_result
  call $hydra.lib.pairs.second
  local.set $cx3
  local.get $cx3
  local.get $tx
  local.get $type_args
  local.get $tbody
  call $hydra.checking.apply_type_arguments_to_type
  i32.const 1
  local.get $applied
  local.get $cx3
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.checking.type_of_let (param $cx i32) (param $tx i32) (param $type_args i32) (param $let_term i32) (result i32)
  (local $acc i32)
  (local $acc_r i32)
  (local $applied i32)
  (local $b i32)
  (local $binding_type i32)
  (local $bnames i32)
  (local $body i32)
  (local $bs i32)
  (local $btype i32)
  (local $btypes i32)
  (local $btypes_r i32)
  (local $btypes_result i32)
  (local $cx2 i32)
  (local $t i32)
  (local $t_result i32)
  (local $ts i32)
  (local $tx2 i32)
  (local $types i32)
  local.get $let_term
  ;; project field: bindings
  local.set $bs
  local.get $let_term
  ;; project field: body
  local.set $body
  ;; project field: name
  local.get $bs
  call $hydra.lib.lists.map
  local.set $bnames
  i32.const 0
  local.get $b
  local.get $cx
  i32.const 1
  local.get $ts
  call $hydra.scoping.type_scheme_to_f_type
  local.get $b
  ;; project field: type
  call $hydra.lib.maybes.maybe
  local.set $binding_type
  local.get $acc
  local.get $acc_r
  call $hydra.lib.pairs.first
  local.set $types
  local.get $b
  local.get $binding_type
  i32.const 1
  local.get $types
  local.get $btype
  call $hydra.lib.lists.pure
  call $hydra.lib.lists.concat2
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  i32.const 1
  i32.const 0
  ;; list elements follow
  local.get $bs
  call $hydra.lib.lists.foldl
  local.set $btypes_result
  local.get $btypes_result
  local.get $btypes_r
  call $hydra.lib.pairs.first
  local.set $btypes
  local.get $tx
  ;; project field: bound_terms
  local.get $bnames
  call $hydra.scoping.f_type_to_type_scheme
  local.get $btypes
  call $hydra.lib.lists.map
  call $hydra.lib.lists.zip
  call $hydra.lib.maps.from_list
  local.get $tx
  ;; project field: bound_types
  call $hydra.lib.maps.union
  local.get $tx
  ;; project field: class_constraints
  local.get $tx
  ;; project field: lambda_variables
  local.get $tx
  ;; project field: metadata
  local.get $tx
  ;; project field: primitives
  local.get $tx
  ;; project field: schema_types
  local.get $tx
  ;; project field: type_variables
  local.set $tx2
  local.get $cx
  local.get $tx2
  i32.const 0
  ;; list elements follow
  local.get $body
  call $hydra.checking.type_of
  local.get $t_result
  call $hydra.lib.pairs.first
  local.set $t
  local.get $t_result
  call $hydra.lib.pairs.second
  local.set $cx2
  local.get $cx2
  local.get $tx
  local.get $type_args
  local.get $t
  call $hydra.checking.apply_type_arguments_to_type
  i32.const 1
  local.get $applied
  local.get $cx2
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.checking.type_of_list (param $cx i32) (param $tx i32) (param $type_args i32) (param $els i32) (result i32)
  (local $acc i32)
  (local $acc_r i32)
  (local $cx2 i32)
  (local $cx_a i32)
  (local $cx_b i32)
  (local $eltypes i32)
  (local $fold_r i32)
  (local $fold_result i32)
  (local $t i32)
  (local $t_result i32)
  (local $term i32)
  (local $types i32)
  (local $unified_type i32)
  local.get $els
  call $hydra.lib.lists.null
  local.get $type_args
  call $hydra.lib.lists.length
  i32.const 1
  call $hydra.lib.equality.equal
  i32.const 1
  local.get $type_args
  call $hydra.lib.lists.head
  local.get $cx
  i32.const 0
  i32.const 0
  i32.const 1
  local.get $type_args
  call $hydra.lib.lists.length
  local.get $type_args
  local.get $cx
  call $hydra.lib.logic.if_else
  local.get $acc
  local.get $acc_r
  call $hydra.lib.pairs.first
  local.set $types
  local.get $acc_r
  call $hydra.lib.pairs.second
  local.set $cx_a
  local.get $cx_a
  local.get $tx
  i32.const 0
  ;; list elements follow
  local.get $term
  call $hydra.checking.type_of
  local.get $t_result
  call $hydra.lib.pairs.first
  local.set $t
  local.get $t_result
  call $hydra.lib.pairs.second
  local.set $cx_b
  i32.const 1
  local.get $types
  local.get $t
  call $hydra.lib.lists.pure
  call $hydra.lib.lists.concat2
  local.get $cx_b
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  i32.const 1
  i32.const 0
  ;; list elements follow
  local.get $cx
  local.get $els
  call $hydra.lib.lists.foldl
  local.set $fold_result
  local.get $fold_result
  local.get $fold_r
  call $hydra.lib.pairs.first
  local.set $eltypes
  local.get $fold_r
  call $hydra.lib.pairs.second
  local.set $cx2
  local.get $cx2
  local.get $tx
  i32.const 0 ;; string: "list elements"
  local.get $eltypes
  call $hydra.checking.check_same_type
  i32.const 1
  local.get $unified_type
  local.get $cx2
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.logic.if_else
)
  (func $hydra.checking.type_of_literal (param $cx i32) (param $tx i32) (param $type_args i32) (param $lit i32) (result i32)
  (local $applied i32)
  (local $t i32)
  local.get $lit
  call $hydra.reflect.literal_type
  local.set $t
  local.get $cx
  local.get $tx
  local.get $type_args
  local.get $t
  call $hydra.checking.apply_type_arguments_to_type
  i32.const 1
  local.get $applied
  local.get $cx
  call $hydra.lib.eithers.bind
)
  (func $hydra.checking.type_of_map (param $cx i32) (param $tx i32) (param $type_args i32) (param $m i32) (result i32)
  (local $acc i32)
  (local $acc_r i32)
  (local $applied i32)
  (local $cx2 i32)
  (local $cx3 i32)
  (local $cx_a i32)
  (local $cx_b i32)
  (local $key_fold_r i32)
  (local $key_fold_result i32)
  (local $key_types i32)
  (local $kt i32)
  (local $p i32)
  (local $pairs i32)
  (local $t i32)
  (local $t_result i32)
  (local $types i32)
  (local $val_fold_r i32)
  (local $val_fold_result i32)
  (local $val_types i32)
  (local $vt i32)
  local.get $m
  call $hydra.lib.maps.null
  local.get $type_args
  call $hydra.lib.lists.length
  i32.const 2
  call $hydra.lib.equality.equal
  i32.const 1
  i32.const 0
  local.get $type_args
  call $hydra.lib.lists.at
  i32.const 1
  local.get $type_args
  call $hydra.lib.lists.at
  local.get $cx
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 2
  local.get $type_args
  call $hydra.lib.lists.length
  local.get $type_args
  local.get $cx
  call $hydra.lib.logic.if_else
  local.get $m
  call $hydra.lib.maps.to_list
  local.set $pairs
  local.get $acc
  local.get $acc_r
  call $hydra.lib.pairs.first
  local.set $types
  local.get $acc_r
  call $hydra.lib.pairs.second
  local.set $cx_a
  local.get $cx_a
  local.get $tx
  i32.const 0
  ;; list elements follow
  local.get $p
  call $hydra.lib.pairs.first
  call $hydra.checking.type_of
  local.get $t_result
  call $hydra.lib.pairs.first
  local.set $t
  local.get $t_result
  call $hydra.lib.pairs.second
  local.set $cx_b
  i32.const 1
  local.get $types
  local.get $t
  call $hydra.lib.lists.pure
  call $hydra.lib.lists.concat2
  local.get $cx_b
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  i32.const 1
  i32.const 0
  ;; list elements follow
  local.get $cx
  local.get $pairs
  call $hydra.lib.lists.foldl
  local.set $key_fold_result
  local.get $key_fold_result
  local.get $key_fold_r
  call $hydra.lib.pairs.first
  local.set $key_types
  local.get $key_fold_r
  call $hydra.lib.pairs.second
  local.set $cx2
  local.get $cx2
  local.get $tx
  i32.const 0 ;; string: "map keys"
  local.get $key_types
  call $hydra.checking.check_same_type
  local.get $acc
  local.get $acc_r
  call $hydra.lib.pairs.first
  local.set $types
  local.get $acc_r
  call $hydra.lib.pairs.second
  local.set $cx_a
  local.get $cx_a
  local.get $tx
  i32.const 0
  ;; list elements follow
  local.get $p
  call $hydra.lib.pairs.second
  call $hydra.checking.type_of
  local.get $t_result
  call $hydra.lib.pairs.first
  local.set $t
  local.get $t_result
  call $hydra.lib.pairs.second
  local.set $cx_b
  i32.const 1
  local.get $types
  local.get $t
  call $hydra.lib.lists.pure
  call $hydra.lib.lists.concat2
  local.get $cx_b
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  i32.const 1
  i32.const 0
  ;; list elements follow
  local.get $cx2
  local.get $pairs
  call $hydra.lib.lists.foldl
  local.set $val_fold_result
  local.get $val_fold_result
  local.get $val_fold_r
  call $hydra.lib.pairs.first
  local.set $val_types
  local.get $val_fold_r
  call $hydra.lib.pairs.second
  local.set $cx3
  local.get $cx3
  local.get $tx
  i32.const 0 ;; string: "map values"
  local.get $val_types
  call $hydra.checking.check_same_type
  local.get $cx3
  local.get $tx
  local.get $type_args
  local.get $kt
  local.get $vt
  call $hydra.checking.apply_type_arguments_to_type
  i32.const 1
  local.get $applied
  local.get $cx3
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.logic.if_else
)
  (func $hydra.checking.type_of_maybe (param $cx i32) (param $tx i32) (param $type_args i32) (param $mt i32) (result i32)
  (local $applied i32)
  (local $cx2 i32)
  (local $for_just i32)
  (local $for_nothing i32)
  (local $n i32)
  (local $t i32)
  (local $t_result i32)
  (local $term i32)
  (local $term_type i32)
  local.get $type_args
  call $hydra.lib.lists.length
  local.set $n
  local.get $n
  i32.const 1
  call $hydra.lib.equality.equal
  i32.const 1
  local.get $type_args
  call $hydra.lib.lists.head
  local.get $cx
  i32.const 0
  i32.const 0
  i32.const 1
  local.get $n
  local.get $type_args
  local.get $cx
  call $hydra.lib.logic.if_else
  local.set $for_nothing
  local.get $cx
  local.get $tx
  i32.const 0
  ;; list elements follow
  local.get $term
  call $hydra.checking.type_of
  local.get $t_result
  call $hydra.lib.pairs.first
  local.set $term_type
  local.get $t_result
  call $hydra.lib.pairs.second
  local.set $cx2
  local.get $term_type
  local.set $t
  local.get $cx2
  local.get $tx
  local.get $type_args
  local.get $t
  call $hydra.checking.apply_type_arguments_to_type
  i32.const 1
  local.get $applied
  local.get $cx2
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  local.set $for_just
  local.get $for_nothing
  local.get $for_just
  local.get $mt
  call $hydra.lib.maybes.maybe
)
  (func $hydra.checking.type_of_pair (param $cx i32) (param $tx i32) (param $type_args i32) (param $p i32) (result i32)
  (local $cx2 i32)
  (local $cx3 i32)
  (local $first_type i32)
  (local $n i32)
  (local $pair_fst i32)
  (local $pair_snd i32)
  (local $result1 i32)
  (local $result2 i32)
  (local $second_type i32)
  local.get $type_args
  call $hydra.lib.lists.length
  local.set $n
  local.get $n
  i32.const 2
  call $hydra.lib.equality.equal
  local.get $p
  call $hydra.lib.pairs.first
  local.set $pair_fst
  local.get $p
  call $hydra.lib.pairs.second
  local.set $pair_snd
  local.get $cx
  local.get $tx
  i32.const 0
  ;; list elements follow
  local.get $pair_fst
  call $hydra.checking.type_of
  local.get $result1
  call $hydra.lib.pairs.first
  local.set $first_type
  local.get $result1
  call $hydra.lib.pairs.second
  local.set $cx2
  local.get $cx2
  local.get $tx
  i32.const 0
  ;; list elements follow
  local.get $pair_snd
  call $hydra.checking.type_of
  local.get $result2
  call $hydra.lib.pairs.first
  local.set $second_type
  local.get $result2
  call $hydra.lib.pairs.second
  local.set $cx3
  i32.const 1
  local.get $first_type
  local.get $second_type
  local.get $cx3
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 2
  local.get $n
  local.get $type_args
  local.get $cx
  call $hydra.lib.logic.if_else
)
  (func $hydra.checking.type_of_primitive (param $cx i32) (param $tx i32) (param $type_args i32) (param $name i32) (result i32)
  (local $_p i32)
  (local $applied i32)
  (local $cx2 i32)
  (local $inst_result i32)
  (local $raw_ts i32)
  (local $t i32)
  (local $ts i32)
  (local $ts_raw i32)
  local.get $_p
  ;; project field: type
  local.get $name
  local.get $tx
  ;; project field: primitives
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.map
  local.set $raw_ts
  i32.const 0
  i32.const 0
  ;; list elements follow
  local.get $name
  local.get $cx
  local.get $cx
  local.get $ts_raw
  call $hydra.resolution.instantiate_type_scheme
  local.set $inst_result
  local.get $inst_result
  call $hydra.lib.pairs.first
  local.set $ts
  local.get $inst_result
  call $hydra.lib.pairs.second
  local.set $cx2
  local.get $ts
  call $hydra.scoping.type_scheme_to_f_type
  local.set $t
  local.get $cx2
  local.get $tx
  local.get $type_args
  local.get $t
  call $hydra.checking.apply_type_arguments_to_type
  i32.const 1
  local.get $applied
  local.get $cx2
  call $hydra.lib.eithers.bind
  local.get $raw_ts
  call $hydra.lib.maybes.maybe
)
  (func $hydra.checking.type_of_projection (param $cx i32) (param $tx i32) (param $type_args i32) (param $p i32) (result i32)
  (local $cx2 i32)
  (local $fname i32)
  (local $ftyp i32)
  (local $sbody i32)
  (local $schema_result i32)
  (local $schema_type i32)
  (local $sfields i32)
  (local $sftyp i32)
  (local $subst i32)
  (local $svars i32)
  (local $tname i32)
  local.get $p
  ;; project field: type_name
  local.set $tname
  local.get $p
  ;; project field: field
  local.set $fname
  local.get $cx
  local.get $tx
  ;; project field: schema_types
  local.get $tname
  call $hydra.resolution.require_schema_type
  local.get $schema_result
  call $hydra.lib.pairs.first
  local.set $schema_type
  local.get $schema_result
  call $hydra.lib.pairs.second
  local.set $cx2
  local.get $schema_type
  ;; project field: variables
  local.set $svars
  local.get $schema_type
  ;; project field: type
  local.set $sbody
  local.get $cx2
  local.get $tname
  local.get $sbody
  call $hydra.extract.core.record_type
  local.get $cx2
  local.get $fname
  local.get $sfields
  call $hydra.resolution.find_field_type
  local.get $svars
  local.get $type_args
  call $hydra.lib.lists.zip
  call $hydra.lib.maps.from_list
  local.set $subst
  local.get $subst
  local.get $ftyp
  call $hydra.substitution.subst_in_type
  local.set $sftyp
  i32.const 1
  local.get $tname
  local.get $type_args
  call $hydra.resolution.nominal_application
  local.get $sftyp
  local.get $cx2
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.checking.type_of_record (param $cx i32) (param $tx i32) (param $type_args i32) (param $record i32) (result i32)
  (local $acc i32)
  (local $acc_r i32)
  (local $cx2 i32)
  (local $cx_a i32)
  (local $cx_b i32)
  (local $fields i32)
  (local $fold_r i32)
  (local $fold_result i32)
  (local $t i32)
  (local $t_result i32)
  (local $term i32)
  (local $tname i32)
  (local $types i32)
  local.get $record
  ;; project field: type_name
  local.set $tname
  local.get $record
  ;; project field: fields
  local.set $fields
  local.get $acc
  local.get $acc_r
  call $hydra.lib.pairs.first
  local.set $types
  local.get $acc_r
  call $hydra.lib.pairs.second
  local.set $cx_a
  local.get $cx_a
  local.get $tx
  i32.const 0
  ;; list elements follow
  local.get $term
  call $hydra.checking.type_of
  local.get $t_result
  call $hydra.lib.pairs.first
  local.set $t
  local.get $t_result
  call $hydra.lib.pairs.second
  local.set $cx_b
  i32.const 1
  local.get $types
  local.get $t
  call $hydra.lib.lists.pure
  call $hydra.lib.lists.concat2
  local.get $cx_b
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  i32.const 1
  i32.const 0
  ;; list elements follow
  local.get $cx
  ;; project field: term
  local.get $fields
  call $hydra.lib.lists.map
  call $hydra.lib.lists.foldl
  local.set $fold_result
  local.get $fold_result
  local.get $fold_r
  call $hydra.lib.pairs.second
  local.set $cx2
  i32.const 1
  local.get $tname
  local.get $type_args
  call $hydra.resolution.nominal_application
  local.get $cx2
  call $hydra.lib.eithers.bind
)
  (func $hydra.checking.type_of_set (param $cx i32) (param $tx i32) (param $type_args i32) (param $els i32) (result i32)
  (local $acc i32)
  (local $acc_r i32)
  (local $cx2 i32)
  (local $cx_a i32)
  (local $cx_b i32)
  (local $eltypes i32)
  (local $fold_r i32)
  (local $fold_result i32)
  (local $t i32)
  (local $t_result i32)
  (local $term i32)
  (local $types i32)
  (local $unified_type i32)
  local.get $els
  call $hydra.lib.sets.null
  local.get $type_args
  call $hydra.lib.lists.length
  i32.const 1
  call $hydra.lib.equality.equal
  i32.const 1
  local.get $type_args
  call $hydra.lib.lists.head
  local.get $cx
  i32.const 0
  i32.const 0
  i32.const 1
  local.get $type_args
  call $hydra.lib.lists.length
  local.get $type_args
  local.get $cx
  call $hydra.lib.logic.if_else
  local.get $acc
  local.get $acc_r
  call $hydra.lib.pairs.first
  local.set $types
  local.get $acc_r
  call $hydra.lib.pairs.second
  local.set $cx_a
  local.get $cx_a
  local.get $tx
  i32.const 0
  ;; list elements follow
  local.get $term
  call $hydra.checking.type_of
  local.get $t_result
  call $hydra.lib.pairs.first
  local.set $t
  local.get $t_result
  call $hydra.lib.pairs.second
  local.set $cx_b
  i32.const 1
  local.get $types
  local.get $t
  call $hydra.lib.lists.pure
  call $hydra.lib.lists.concat2
  local.get $cx_b
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  i32.const 1
  i32.const 0
  ;; list elements follow
  local.get $cx
  local.get $els
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.foldl
  local.set $fold_result
  local.get $fold_result
  local.get $fold_r
  call $hydra.lib.pairs.first
  local.set $eltypes
  local.get $fold_r
  call $hydra.lib.pairs.second
  local.set $cx2
  local.get $cx2
  local.get $tx
  i32.const 0 ;; string: "set elements"
  local.get $eltypes
  call $hydra.checking.check_same_type
  i32.const 1
  local.get $unified_type
  local.get $cx2
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.logic.if_else
)
  (func $hydra.checking.type_of_term (param $cx i32) (param $g i32) (param $term i32) (result i32)
  call $hydra.lib.pairs.first
  local.get $cx
  local.get $g
  i32.const 0
  ;; list elements follow
  local.get $term
  call $hydra.checking.type_of
  call $hydra.lib.eithers.map
)
  (func $hydra.checking.type_of_type_application (param $cx i32) (param $tx i32) (param $type_args i32) (param $tyapp i32) (result i32)
  (local $body i32)
  (local $t i32)
  local.get $tyapp
  ;; project field: body
  local.set $body
  local.get $tyapp
  ;; project field: type
  local.set $t
  local.get $cx
  local.get $tx
  local.get $t
  local.get $type_args
  call $hydra.lib.lists.cons
  local.get $body
  call $hydra.checking.type_of
)
  (func $hydra.checking.type_of_type_lambda (param $cx i32) (param $tx i32) (param $type_args i32) (param $tl i32) (result i32)
  (local $applied i32)
  (local $body i32)
  (local $cx2 i32)
  (local $result1 i32)
  (local $t1 i32)
  (local $tx2 i32)
  (local $v i32)
  (local $vars i32)
  local.get $tl
  ;; project field: parameter
  local.set $v
  local.get $tl
  ;; project field: body
  local.set $body
  local.get $tx
  ;; project field: type_variables
  local.set $vars
  local.get $tx
  ;; project field: bound_terms
  local.get $tx
  ;; project field: bound_types
  local.get $tx
  ;; project field: class_constraints
  local.get $tx
  ;; project field: lambda_variables
  local.get $tx
  ;; project field: metadata
  local.get $tx
  ;; project field: primitives
  local.get $tx
  ;; project field: schema_types
  local.get $v
  local.get $vars
  call $hydra.lib.sets.insert
  local.set $tx2
  local.get $cx
  local.get $tx2
  i32.const 0
  ;; list elements follow
  local.get $body
  call $hydra.checking.type_of
  local.get $result1
  call $hydra.lib.pairs.first
  local.set $t1
  local.get $result1
  call $hydra.lib.pairs.second
  local.set $cx2
  local.get $cx2
  local.get $tx
  local.get $type_args
  local.get $v
  local.get $t1
  call $hydra.checking.apply_type_arguments_to_type
  i32.const 1
  local.get $applied
  local.get $cx2
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.checking.type_of_unit (param $cx i32) (param $tx i32) (param $type_args i32) (result i32)
  (local $applied i32)
  local.get $cx
  local.get $tx
  local.get $type_args
  i32.const 0
  call $hydra.checking.apply_type_arguments_to_type
  i32.const 1
  local.get $applied
  local.get $cx
  call $hydra.lib.eithers.bind
)
  (func $hydra.checking.type_of_unwrap (param $cx i32) (param $tx i32) (param $type_args i32) (param $tname i32) (result i32)
  (local $cx2 i32)
  (local $sbody i32)
  (local $schema_result i32)
  (local $schema_type i32)
  (local $subst i32)
  (local $svars i32)
  (local $swrapped i32)
  (local $wrapped i32)
  local.get $cx
  local.get $tx
  ;; project field: schema_types
  local.get $tname
  call $hydra.resolution.require_schema_type
  local.get $schema_result
  call $hydra.lib.pairs.first
  local.set $schema_type
  local.get $schema_result
  call $hydra.lib.pairs.second
  local.set $cx2
  local.get $schema_type
  ;; project field: variables
  local.set $svars
  local.get $schema_type
  ;; project field: type
  local.set $sbody
  local.get $cx2
  local.get $tname
  local.get $sbody
  call $hydra.extract.core.wrapped_type
  local.get $svars
  local.get $type_args
  call $hydra.lib.lists.zip
  call $hydra.lib.maps.from_list
  local.set $subst
  local.get $subst
  local.get $wrapped
  call $hydra.substitution.subst_in_type
  local.set $swrapped
  i32.const 1
  local.get $tname
  local.get $type_args
  call $hydra.resolution.nominal_application
  local.get $swrapped
  local.get $cx2
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.checking.type_of_variable (param $cx i32) (param $tx i32) (param $type_args i32) (param $name i32) (result i32)
  (local $_p i32)
  (local $applied i32)
  (local $cx2 i32)
  (local $for_scheme i32)
  (local $raw_type_scheme i32)
  (local $t i32)
  (local $t_result i32)
  (local $ts i32)
  local.get $name
  local.get $tx
  ;; project field: bound_types
  call $hydra.lib.maps.lookup
  local.set $raw_type_scheme
  local.get $type_args
  call $hydra.lib.lists.null
  local.get $cx
  local.get $ts
  call $hydra.scoping.type_scheme_to_f_type
  call $hydra.resolution.instantiate_type
  local.get $ts
  call $hydra.scoping.type_scheme_to_f_type
  local.get $cx
  call $hydra.lib.logic.if_else
  local.set $t_result
  local.get $t_result
  call $hydra.lib.pairs.first
  local.set $t
  local.get $t_result
  call $hydra.lib.pairs.second
  local.set $cx2
  local.get $cx2
  local.get $tx
  local.get $type_args
  local.get $t
  call $hydra.checking.apply_type_arguments_to_type
  i32.const 1
  local.get $applied
  local.get $cx2
  call $hydra.lib.eithers.bind
  local.set $for_scheme
  i32.const 0
  i32.const 0
  ;; list elements follow
  local.get $name
  local.get $cx
  local.get $for_scheme
  local.get $_p
  ;; project field: type
  local.get $name
  local.get $tx
  ;; project field: primitives
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.map
  call $hydra.lib.maybes.maybe
  local.get $for_scheme
  local.get $raw_type_scheme
  call $hydra.lib.maybes.maybe
)
  (func $hydra.checking.type_of_wrapped_term (param $cx i32) (param $tx i32) (param $type_args i32) (param $wt i32) (result i32)
  (local $body i32)
  (local $cx2 i32)
  (local $result i32)
  (local $tname i32)
  local.get $wt
  ;; project field: type_name
  local.set $tname
  local.get $wt
  ;; project field: body
  local.set $body
  local.get $cx
  local.get $tx
  i32.const 0
  ;; list elements follow
  local.get $body
  call $hydra.checking.type_of
  local.get $result
  call $hydra.lib.pairs.second
  local.set $cx2
  i32.const 1
  local.get $tname
  local.get $type_args
  call $hydra.resolution.nominal_application
  local.get $cx2
  call $hydra.lib.eithers.bind
)
  (func $hydra.checking.types_all_effectively_equal (param $tx i32) (param $tlist i32) (result i32)
  (local $acc i32)
  (local $all_vars i32)
  (local $any_contains_free_var i32)
  (local $contains_free_var i32)
  (local $schema_names i32)
  (local $t i32)
  (local $types i32)
  local.get $tx
  ;; project field: schema_types
  local.set $types
  local.get $t
  call $hydra.variables.free_variables_in_type_simple
  local.set $all_vars
  local.get $types
  call $hydra.lib.maps.keys
  call $hydra.lib.sets.from_list
  local.set $schema_names
  local.get $all_vars
  local.get $schema_names
  call $hydra.lib.sets.difference
  call $hydra.lib.sets.null
  call $hydra.lib.logic.not
  local.set $contains_free_var
  local.get $acc
  local.get $t
  local.get $contains_free_var
  call $hydra.lib.logic.or
  i32.const 0
  local.get $tlist
  call $hydra.lib.lists.foldl
  local.set $any_contains_free_var
  local.get $any_contains_free_var
  i32.const 1
  local.get $t
  call $hydra.checking.normalize_type_free_vars
  local.get $tlist
  call $hydra.lib.lists.map
  call $hydra.checking.all_equal
  i32.const 1
  local.get $types
  local.get $t
  call $hydra.dependencies.replace_typedefs
  call $hydra.strip.deannotate_type_recursive
  call $hydra.checking.normalize_type_free_vars
  local.get $tlist
  call $hydra.lib.lists.map
  call $hydra.checking.all_equal
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
)
  (func $hydra.checking.types_effectively_equal (param $tx i32) (param $t1 i32) (param $t2 i32) (result i32)
  local.get $tx
  local.get $t1
  call $hydra.checking.contains_in_scope_type_vars
  local.get $tx
  local.get $t2
  call $hydra.checking.contains_in_scope_type_vars
  local.get $tx
  i32.const 2
  ;; list elements follow
  local.get $t1
  call $hydra.resolution.fully_strip_and_normalize_type
  local.get $t2
  call $hydra.resolution.fully_strip_and_normalize_type
  call $hydra.checking.types_all_effectively_equal
  call $hydra.lib.logic.or
  call $hydra.lib.logic.or
)
)
