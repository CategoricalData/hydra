(module
  (import "hydra.dependencies" "hydra.dependencies.replace_typedefs" (func $hydra.dependencies.replace_typedefs (param i32) (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.function_type" (func $hydra.extract.core.function_type (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.record_type" (func $hydra.extract.core.record_type (param i32) (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.union_type" (func $hydra.extract.core.union_type (param i32) (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.wrapped_type" (func $hydra.extract.core.wrapped_type (param i32) (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.dereference_schema_type" (func $hydra.lexical.dereference_schema_type (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_maybe" (func $hydra.lib.eithers.map_maybe (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.filter" (func $hydra.lib.lists.filter (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.maybe_head" (func $hydra.lib.lists.maybe_head (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.pure" (func $hydra.lib.lists.pure (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.uncons" (func $hydra.lib.lists.uncons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.zip" (func $hydra.lib.lists.zip (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.zip_with" (func $hydra.lib.lists.zip_with (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int32" (func $hydra.lib.literals.show_int32 (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.and" (func $hydra.lib.logic.and (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.or" (func $hydra.lib.logic.or (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.insert" (func $hydra.lib.maps.insert (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.keys" (func $hydra.lib.maps.keys (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.map" (func $hydra.lib.maps.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.member" (func $hydra.lib.maps.member (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.null" (func $hydra.lib.maps.null (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.singleton" (func $hydra.lib.maps.singleton (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.size" (func $hydra.lib.maps.size (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.union" (func $hydra.lib.maps.union (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cat" (func $hydra.lib.maybes.cat (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.difference" (func $hydra.lib.sets.difference (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.insert" (func $hydra.lib.sets.insert (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.intersection" (func $hydra.lib.sets.intersection (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.member" (func $hydra.lib.sets.member (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.null" (func $hydra.lib.sets.null (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.union" (func $hydra.lib.sets.union (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.fresh_name" (func $hydra.names.fresh_name (param i32) (result i32) ) )
  (import "hydra.reflect" "hydra.reflect.literal_type" (func $hydra.reflect.literal_type (param i32) (result i32) ) )
  (import "hydra.resolution" "hydra.resolution.find_field_type" (func $hydra.resolution.find_field_type (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.resolution" "hydra.resolution.fully_strip_and_normalize_type" (func $hydra.resolution.fully_strip_and_normalize_type (param i32) (result i32) ) )
  (import "hydra.resolution" "hydra.resolution.instantiate_type" (func $hydra.resolution.instantiate_type (param i32) (param i32) (result i32) ) )
  (import "hydra.resolution" "hydra.resolution.instantiate_type_scheme" (func $hydra.resolution.instantiate_type_scheme (param i32) (param i32) (result i32) ) )
  (import "hydra.resolution" "hydra.resolution.nominal_application" (func $hydra.resolution.nominal_application (param i32) (param i32) (result i32) ) )
  (import "hydra.resolution" "hydra.resolution.require_schema_type" (func $hydra.resolution.require_schema_type (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.fold_over_type" (func $hydra.rewriting.fold_over_type (param i32) (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.subterms" (func $hydra.rewriting.subterms (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.f_type_to_type_scheme" (func $hydra.scoping.f_type_to_type_scheme (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.type_scheme_to_f_type" (func $hydra.scoping.type_scheme_to_f_type (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.type" (func $hydra.show.core.type (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_type" (func $hydra.strip.deannotate_type (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_type_recursive" (func $hydra.strip.deannotate_type_recursive (param i32) (result i32) ) )
  (import "hydra.substitution" "hydra.substitution.subst_in_type" (func $hydra.substitution.subst_in_type (param i32) (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.free_variables_in_type" (func $hydra.variables.free_variables_in_type (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.free_variables_in_type_simple" (func $hydra.variables.free_variables_in_type_simple (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.substitute_type_variables" (func $hydra.variables.substitute_type_variables (param i32) (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\05\00\00\00\20\2d\2d\3e\20\0c\00\00\00\20\74\79\70\65\20\61\72\67\73\3a\20\02\00\00\00\2c\20\15\00\00\00\2e\20\43\6f\6e\74\65\78\74\20\68\61\73\20\76\61\72\73\3a\20\7b\12\00\00\00\2e\20\54\72\79\69\6e\67\20\74\6f\20\61\70\70\6c\79\20\03\00\00\00\5f\74\76\0d\00\00\00\63\61\73\65\20\62\72\61\6e\63\68\65\73\0b\00\00\00\66\6f\72\61\6c\6c\20\74\79\70\65\0d\00\00\00\6c\69\73\74\20\65\6c\65\6d\65\6e\74\73\08\00\00\00\6d\61\70\20\6b\65\79\73\0a\00\00\00\6d\61\70\20\76\61\6c\75\65\73\0c\00\00\00\73\65\74\20\65\6c\65\6d\65\6e\74\73\09\00\00\00\74\6f\70\20\6c\65\76\65\6c\06\00\00\00\74\79\70\65\4f\66\01\00\00\00\7d")
  (global $__bump_ptr (mut i32) i32.const 1232 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
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
  (local $h i32)
  (local $t i32)
  (local $uc i32)
  (local $x i32)
  i32.const 1
  local.get $uc
  call $hydra.lib.pairs.first
  local.set $h
  local.get $uc
  call $hydra.lib.pairs.second
  local.set $t
  local.get $b
  local.get $x
  local.get $h
  call $hydra.lib.equality.equal
  call $hydra.lib.logic.and
  i32.const 1
  local.get $t
  call $hydra.lib.lists.foldl
  local.get $els
  call $hydra.lib.lists.uncons
  call $hydra.lib.maybes.maybe
)
  (func $hydra.checking.apply_type_arguments_to_type (param $cx i32) (param $tx i32) (param $type_args i32) (param $t i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $ah i32)
  (local $at i32)
  (local $ft i32)
  (local $tbody i32)
  (local $uc i32)
  (local $v i32)
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
  local.get $uc
  call $hydra.lib.pairs.first
  local.set $ah
  local.get $uc
  call $hydra.lib.pairs.second
  local.set $at
  (block $end_type (result i32)
  (block $forall
  local.get $t
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
  local.set $v
  local.get $ft
  i32.load offset=4
  local.set $tbody
  local.get $cx
  local.get $tx
  local.get $at
  local.get $v
  local.get $ah
  call $hydra.lib.maps.singleton
  local.get $tbody
  call $hydra.substitution.subst_in_type
  call $hydra.checking.apply_type_arguments_to_type
  br $end_type
)
  local.get $type_args
  call $hydra.lib.lists.uncons
  call $hydra.lib.maybes.maybe
)
  (func $hydra.checking.check_for_unbound_type_variables (param $cx i32) (param $tx i32) (param $term0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (local $badvars i32)
  (local $bterm i32)
  (local $check i32)
  (local $check_optional i32)
  (local $check_recursive i32)
  (local $dflt i32)
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
  (local $v i32)
  (local $vars i32)
  local.get $tx
  i32.load offset=24
  call $hydra.lib.maps.keys
  call $hydra.lib.sets.from_list
  local.set $svars
  local.get $vars
  drop
  local.get $trace
  drop
  local.get $lbinding
  drop
  local.get $check_recursive
  drop
  i32.const 0
  local.set $recurse
  local.get $recurse
  local.get $term
  call $hydra.rewriting.subterms
  call $hydra.lib.eithers.map_list
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  i32.const 0
  i32.const 6
  local.get $badvars
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
  call $hydra.lib.logic.if_else
  local.set $check
  local.get $check
  local.get $m
  call $hydra.lib.eithers.map_maybe
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
  call $hydra.lib.eithers.bind
  local.set $check_optional
  (block $end_term (result i32)
  (block $type_lambda
  (block $type_application
  (block $let
  (block $lambda
  local.get $term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $lambda $let $type_application $type_lambda $type_lambda
)
  local.get $v
  drop
  local.get $l
  i32.load offset=4
  drop
  local.get $check_optional
  drop
  i32.const 0
  local.get $l
  i32.load offset=8
  drop
  local.get $recurse
  drop
  i32.const 0
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $v
  drop
  local.get $b
  i32.load offset=4
  local.set $bterm
  local.get $vars
  local.get $vars
  local.get $ts
  i32.load
  call $hydra.lib.sets.from_list
  call $hydra.lib.sets.union
  local.get $b
  i32.load offset=8
  call $hydra.lib.maybes.maybe
  local.set $new_vars
  local.get $b
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  local.get $trace
  call $hydra.lib.lists.cons
  local.set $new_trace
  local.get $new_vars
  drop
  local.get $new_trace
  drop
  local.get $b
  drop
  local.get $bterm
  drop
  local.get $check_recursive
  drop
  i32.const 0
  local.set $for_binding
  local.get $for_binding
  local.get $l
  i32.load
  call $hydra.lib.eithers.map_list
  local.get $l
  i32.load offset=4
  drop
  local.get $recurse
  drop
  i32.const 0
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $v
  drop
  local.get $tt
  i32.load offset=4
  drop
  local.get $check
  drop
  i32.const 0
  local.get $tt
  i32.load
  drop
  local.get $recurse
  drop
  i32.const 0
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $v
  drop
  i32.const 14
  local.get $tl
  i32.load
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $check
  drop
  i32.const 0
  local.get $tl
  i32.load offset=4
  drop
  local.get $recurse
  drop
  i32.const 0
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.set $check_recursive
  i32.const 0
  drop
  i32.const 1200
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
  drop
  i32.const 0
  drop
  local.get $term0
  drop
  local.get $check_recursive
  drop
  i32.const 0
)
  (func $hydra.checking.check_nominal_application (param $cx i32) (param $tx i32) (param $tname i32) (param $type_args i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $argslen i32)
  (local $cx2 i32)
  (local $result i32)
  (local $schema_type i32)
  (local $vars i32)
  (local $varslen i32)
  local.get $cx
  local.get $tx
  i32.load offset=24
  local.get $tname
  call $hydra.resolution.require_schema_type
  local.get $result
  call $hydra.lib.pairs.first
  local.set $schema_type
  local.get $result
  call $hydra.lib.pairs.second
  local.set $cx2
  local.get $schema_type
  i32.load
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
  i32.const 0
  local.get $cx2
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  i32.const 0
  i32.const 0
  i32.const 4
  i32.const 14
  local.get $tname
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.get $varslen
  local.get $argslen
  local.get $type_args
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  call $hydra.lib.eithers.bind
)
  (func $hydra.checking.check_same_type (param $cx i32) (param $tx i32) (param $desc i32) (param $types i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $t i32)
  (local $unequal_err i32)
  i32.const 0
  i32.const 0
  i32.const 8
  local.get $types
  local.get $desc
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.set $unequal_err
  local.get $tx
  local.get $types
  call $hydra.checking.types_all_effectively_equal
  local.get $unequal_err
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
  local.get $types
  call $hydra.lib.lists.maybe_head
  call $hydra.lib.maybes.maybe
  local.get $unequal_err
  call $hydra.lib.logic.if_else
)
  (func $hydra.checking.check_type (param $cx i32) (param $tx i32) (param $term i32) (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $_p i32)
  (local $t0 i32)
  (local $vars i32)
  local.get $tx
  i32.load offset=28
  local.set $vars
  i32.const 0
  local.get $_p
  call $hydra.lib.pairs.first
  local.get $cx
  local.get $tx
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $term
  call $hydra.checking.type_of
  call $hydra.lib.eithers.map
  local.get $tx
  local.get $t0
  local.get $typ
  call $hydra.checking.types_effectively_equal
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
  i32.const 0
  i32.const 0
  i32.const 5
  local.get $typ
  local.get $t0
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  call $hydra.lib.eithers.bind
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
)
  (func $hydra.checking.check_type_subst (param $cx i32) (param $tx i32) (param $subst i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
  local.get $subst
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $s
  local.get $s
  call $hydra.lib.maps.keys
  call $hydra.lib.sets.from_list
  local.set $vars
  local.get $vars
  local.get $tx
  i32.load offset=24
  call $hydra.lib.maps.keys
  call $hydra.lib.sets.from_list
  call $hydra.lib.sets.intersection
  local.set $suspect_vars
  (block $end_type (result i32)
  (block $wrap
  (block $union
  (block $record
  local.get $ts
  i32.load offset=4
  call $hydra.strip.deannotate_type
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $union $wrap $wrap
)
  local.get $v
  drop
  i32.const 1
  br $end_type
)
  local.get $v
  drop
  i32.const 1
  br $end_type
)
  local.get $v
  drop
  i32.const 1
  br $end_type
)
  local.set $is_nominal
  i32.const 0
  local.get $is_nominal
  local.get $v
  local.get $tx
  i32.load offset=24
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
  local.get $p
  call $hydra.lib.pairs.first
  drop
  i32.const 0
  drop
  i32.const 0
  i32.const 1024
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  i32.const 0
  i32.const 0
  local.get $subst
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
  call $hydra.lib.logic.if_else
)
  (func $hydra.checking.check_type_variables (param $_tx i32) (param $_typ i32) (result i32)
  i32.const 0
)
  (func $hydra.checking.contains_in_scope_type_vars (param $tx i32) (param $t i32) (result i32)
  (local $free_vars i32)
  (local $vars i32)
  local.get $tx
  i32.load offset=28
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
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $acc i32)
  (local $collect_vars i32)
  (local $subst i32)
  (local $t i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $variable
  local.get $t
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
  local.get $v
  local.get $acc
  call $hydra.lib.maps.member
  local.get $acc
  local.get $v
  i32.const 1102
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
  local.get $collect_vars
  i32.const 0
  local.get $typ
  call $hydra.rewriting.fold_over_type
  local.set $subst
  local.get $subst
  local.get $typ
  call $hydra.variables.substitute_type_variables
)
  (func $hydra.checking.to_f_context (param $cx i32) (result i32)
  i32.const 0
  local.get $cx
  i32.load offset=4
  call $hydra.lib.maps.map
)
  (func $hydra.checking.type_lists_effectively_equal (param $tx i32) (param $tlist1 i32) (param $tlist2 i32) (result i32)
  local.get $tlist1
  call $hydra.lib.lists.length
  local.get $tlist2
  call $hydra.lib.lists.length
  call $hydra.lib.equality.equal
  i32.const 0
  i32.const 1
  local.get $tx
  i32.const 0
  i32.const 0
  call $hydra.checking.types_effectively_equal
  local.get $tlist1
  local.get $tlist2
  call $hydra.lib.lists.zip_with
  call $hydra.lib.lists.foldl
  i32.const 0
  call $hydra.lib.logic.if_else
)
  (func $hydra.checking.type_of (param $cx i32) (param $tx i32) (param $type_args i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $cx1 i32)
  (local $v i32)
  i32.const 1213
  local.get $cx
  i32.load
  call $hydra.lib.lists.cons
  local.get $cx
  i32.load offset=4
  local.get $cx
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
  local.set $cx1
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
  local.get $term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $application $cases $either $lambda $let $list $literal $map $maybe $pair $project $record $set $type_application $type_lambda $inject $unit $unwrap $variable $wrap $wrap
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
  call $hydra.checking.type_of_case_statement
  br $end_term
)
  local.get $cx1
  local.get $tx
  local.get $type_args
  local.get $v
  call $hydra.checking.type_of_either
  br $end_term
)
  local.get $cx1
  local.get $tx
  local.get $type_args
  local.get $v
  call $hydra.checking.type_of_lambda
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
  call $hydra.checking.type_of_projection
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
  local.get $v
  drop
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
  call $hydra.checking.type_of_unwrap
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
  i32.load
  call $hydra.checking.type_of
)
  (func $hydra.checking.type_of_application (param $cx i32) (param $tx i32) (param $type_args i32) (param $app i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
  (local $v i32)
  local.get $app
  i32.load
  local.set $fun
  local.get $app
  i32.load offset=4
  local.set $arg
  (block $end_type (result i32)
  (block $variable
  (block $function
  (block $forall
  local.get $tfun
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $forall $function $variable $variable
)
  local.get $v
  drop
  local.get $cx0
  drop
  local.get $ft
  i32.load offset=4
  drop
  local.get $targ
  drop
  local.get $try_type
  drop
  i32.const 0
  br $end_type
)
  local.get $v
  drop
  local.get $ft
  i32.load
  local.set $dom
  local.get $ft
  i32.load offset=4
  local.set $cod
  local.get $tx
  local.get $dom
  local.get $targ
  call $hydra.checking.types_effectively_equal
  i32.const 1
  local.get $cod
  local.get $cx0
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  i32.const 0
  i32.const 0
  i32.const 5
  local.get $dom
  local.get $targ
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  br $end_type
)
  local.get $v
  drop
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
  i32.const 14
  local.get $fresh_n
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.get $cx1
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.set $try_type
  local.get $cx
  local.get $tx
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
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
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $arg
  call $hydra.checking.type_of
  local.get $result2
  call $hydra.lib.pairs.first
  local.set $targ
  local.get $result2
  call $hydra.lib.pairs.second
  local.set $cx3
  local.get $cx3
  drop
  local.get $tfun
  drop
  local.get $targ
  drop
  local.get $try_type
  drop
  i32.const 0
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  (func $hydra.checking.type_of_case_statement (param $cx i32) (param $tx i32) (param $type_args i32) (param $cs i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
  i32.load
  local.set $tname
  local.get $cs
  i32.load offset=4
  local.set $dflt
  local.get $cs
  i32.load offset=8
  local.set $cases
  i32.const 0
  local.get $cases
  call $hydra.lib.lists.map
  local.set $cterms
  local.get $cx
  local.get $tx
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $e
  call $hydra.checking.type_of
  local.get $dflt
  call $hydra.lib.eithers.map_maybe
  i32.const 0
  local.get $dflt_result
  call $hydra.lib.maybes.map
  local.set $tdflt
  local.get $cx
  i32.const 0
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
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  i32.const 1
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $cx2
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $t
  call $hydra.extract.core.function_type
  i32.const 1
  local.get $cods
  local.get $ft
  i32.load offset=4
  call $hydra.lib.lists.pure
  call $hydra.lib.lists.concat2
  local.get $cx3
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  i32.const 1
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $cx3
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $tcterms
  call $hydra.lib.lists.foldl
  local.set $fcods_result
  local.get $fcods_result
  local.get $fcods_r
  call $hydra.lib.pairs.first
  local.set $fcods
  local.get $tdflt
  i32.const 0
  local.get $fcods
  call $hydra.lib.lists.map
  call $hydra.lib.lists.cons
  call $hydra.lib.maybes.cat
  local.set $cods
  local.get $cx3
  local.get $tx
  i32.const 1109
  local.get $cods
  call $hydra.checking.check_same_type
  i32.const 1
  i32.const 4
  local.get $tname
  local.get $type_args
  call $hydra.resolution.nominal_application
  local.get $cod
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx3
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  (func $hydra.checking.type_of_either (param $cx i32) (param $tx i32) (param $type_args i32) (param $et i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $arity_err i32)
  (local $cx2 i32)
  (local $left_term i32)
  (local $left_type i32)
  (local $n i32)
  (local $result i32)
  (local $right_term i32)
  (local $right_type i32)
  (local $ta0 i32)
  (local $ta1 i32)
  (local $uc0 i32)
  (local $uc1 i32)
  local.get $type_args
  call $hydra.lib.lists.length
  local.set $n
  i32.const 0
  i32.const 0
  i32.const 4
  i32.const 2
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
  i32.const 2
  local.get $n
  local.get $type_args
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.set $arity_err
  local.get $arity_err
  local.get $uc0
  call $hydra.lib.pairs.first
  local.set $ta0
  local.get $arity_err
  local.get $uc1
  call $hydra.lib.pairs.first
  local.set $ta1
  local.get $n
  i32.const 2
  call $hydra.lib.equality.equal
  local.get $cx
  local.get $tx
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $left_term
  call $hydra.checking.type_of
  local.get $result
  call $hydra.lib.pairs.first
  local.set $left_type
  local.get $result
  call $hydra.lib.pairs.second
  local.set $cx2
  i32.const 1
  i32.const 2
  local.get $left_type
  local.get $ta1
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx2
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $tx
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $right_term
  call $hydra.checking.type_of
  local.get $result
  call $hydra.lib.pairs.first
  local.set $right_type
  local.get $result
  call $hydra.lib.pairs.second
  local.set $cx2
  i32.const 1
  i32.const 2
  local.get $ta0
  local.get $right_type
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx2
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $et
  call $hydra.lib.eithers.either
  local.get $arity_err
  call $hydra.lib.logic.if_else
  local.get $uc0
  call $hydra.lib.pairs.second
  call $hydra.lib.lists.uncons
  call $hydra.lib.maybes.maybe
  local.get $type_args
  call $hydra.lib.lists.uncons
  call $hydra.lib.maybes.maybe
)
  (func $hydra.checking.type_of_injection (param $cx i32) (param $tx i32) (param $type_args i32) (param $injection i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
  i32.load
  local.set $tname
  local.get $injection
  i32.load offset=4
  local.set $field
  local.get $field
  i32.load
  local.set $fname
  local.get $field
  i32.load offset=4
  local.set $fterm
  local.get $cx
  local.get $tx
  i32.load offset=24
  local.get $tname
  call $hydra.resolution.require_schema_type
  local.get $schema_result
  call $hydra.lib.pairs.first
  local.set $schema_type
  local.get $schema_result
  call $hydra.lib.pairs.second
  local.set $cx2
  local.get $schema_type
  i32.load
  local.set $svars
  local.get $schema_type
  i32.load offset=4
  local.set $sbody
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
)
  (func $hydra.checking.type_of_lambda (param $cx i32) (param $tx i32) (param $type_args i32) (param $l i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
  i32.load
  local.set $v
  local.get $l
  i32.load offset=4
  local.set $mdom
  local.get $l
  i32.load offset=8
  local.set $body
  i32.const 0
  i32.const 0
  i32.const 10
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
  local.get $v
  local.get $dom
  call $hydra.scoping.f_type_to_type_scheme
  local.get $tx
  i32.load offset=4
  call $hydra.lib.maps.insert
  local.set $types2
  local.get $cx
  local.get $tx
  i32.load
  local.get $types2
  local.get $tx
  i32.load offset=8
  local.get $tx
  i32.load offset=12
  local.get $tx
  i32.load offset=16
  local.get $tx
  i32.load offset=20
  local.get $tx
  i32.load offset=24
  local.get $tx
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
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $body
  call $hydra.checking.type_of
  local.get $cod_result
  call $hydra.lib.pairs.first
  local.set $cod
  local.get $cod_result
  call $hydra.lib.pairs.second
  local.set $cx2
  i32.const 1
  i32.const 4
  local.get $dom
  local.get $cod
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx2
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  (func $hydra.checking.type_of_let (param $cx i32) (param $tx i32) (param $type_args i32) (param $let_term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
  i32.load
  local.set $bs
  local.get $let_term
  i32.load offset=4
  local.set $body
  i32.const 0
  local.get $bs
  call $hydra.lib.lists.map
  local.set $bnames
  i32.const 0
  i32.const 0
  i32.const 11
  local.get $b
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
  call $hydra.scoping.type_scheme_to_f_type
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  call $hydra.lib.maybes.maybe
  local.set $binding_type
  local.get $acc
  local.get $acc_r
  call $hydra.lib.pairs.first
  local.set $types
  local.get $b
  drop
  local.get $binding_type
  drop
  i32.const 0
  i32.const 1
  local.get $types
  local.get $btype
  call $hydra.lib.lists.pure
  call $hydra.lib.lists.concat2
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  i32.const 1
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
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
  local.get $bs
  call $hydra.lib.lists.foldl
  local.set $btypes_result
  local.get $btypes_result
  local.get $btypes_r
  call $hydra.lib.pairs.first
  local.set $btypes
  local.get $tx
  i32.load
  local.get $bnames
  i32.const 0
  local.get $btypes
  call $hydra.lib.lists.map
  call $hydra.lib.lists.zip
  call $hydra.lib.maps.from_list
  local.get $tx
  i32.load offset=4
  call $hydra.lib.maps.union
  local.get $tx
  i32.load offset=8
  local.get $tx
  i32.load offset=12
  local.get $tx
  i32.load offset=16
  local.get $tx
  i32.load offset=20
  local.get $tx
  i32.load offset=24
  local.get $tx
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
  local.set $tx2
  local.get $cx
  local.get $tx2
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
)
  (func $hydra.checking.type_of_list (param $cx i32) (param $tx i32) (param $type_args i32) (param $els i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $acc i32)
  (local $acc_r i32)
  (local $cx2 i32)
  (local $cx_a i32)
  (local $cx_b i32)
  (local $eltypes i32)
  (local $fold_r i32)
  (local $fold_result i32)
  (local $list_arity_err i32)
  (local $t i32)
  (local $t_result i32)
  (local $ta0 i32)
  (local $term i32)
  (local $types i32)
  (local $unified_type i32)
  i32.const 0
  i32.const 0
  i32.const 4
  i32.const 5
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
  local.get $type_args
  call $hydra.lib.lists.length
  local.get $type_args
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.set $list_arity_err
  local.get $els
  call $hydra.lib.lists.null
  local.get $type_args
  call $hydra.lib.lists.length
  i32.const 1
  call $hydra.lib.equality.equal
  local.get $list_arity_err
  i32.const 1
  i32.const 5
  local.get $ta0
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.get $cx
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $type_args
  call $hydra.lib.lists.maybe_head
  call $hydra.lib.maybes.maybe
  local.get $list_arity_err
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
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  i32.const 1
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $cx
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  i32.const 1141
  local.get $eltypes
  call $hydra.checking.check_same_type
  i32.const 1
  i32.const 5
  local.get $unified_type
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.get $cx2
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  call $hydra.lib.logic.if_else
)
  (func $hydra.checking.type_of_literal (param $cx i32) (param $tx i32) (param $type_args i32) (param $lit i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $applied i32)
  (local $t i32)
  i32.const 6
  local.get $lit
  call $hydra.reflect.literal_type
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.set $t
  local.get $cx
  local.get $tx
  local.get $type_args
  local.get $t
  call $hydra.checking.apply_type_arguments_to_type
  i32.const 1
  local.get $applied
  local.get $cx
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  (func $hydra.checking.type_of_map (param $cx i32) (param $tx i32) (param $type_args i32) (param $m i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
  (local $map_arity_err i32)
  (local $p i32)
  (local $pairs i32)
  (local $t i32)
  (local $t_result i32)
  (local $ta0 i32)
  (local $ta1 i32)
  (local $types i32)
  (local $uc0 i32)
  (local $uc1 i32)
  (local $val_fold_r i32)
  (local $val_fold_result i32)
  (local $val_types i32)
  (local $vt i32)
  i32.const 0
  i32.const 0
  i32.const 4
  i32.const 7
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
  i32.const 2
  local.get $type_args
  call $hydra.lib.lists.length
  local.get $type_args
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.set $map_arity_err
  local.get $m
  call $hydra.lib.maps.null
  local.get $type_args
  call $hydra.lib.lists.length
  i32.const 2
  call $hydra.lib.equality.equal
  local.get $map_arity_err
  local.get $uc0
  call $hydra.lib.pairs.first
  local.set $ta0
  local.get $map_arity_err
  local.get $uc1
  call $hydra.lib.pairs.first
  local.set $ta1
  i32.const 1
  i32.const 7
  local.get $ta0
  local.get $ta1
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $uc0
  call $hydra.lib.pairs.second
  call $hydra.lib.lists.uncons
  call $hydra.lib.maybes.maybe
  local.get $type_args
  call $hydra.lib.lists.uncons
  call $hydra.lib.maybes.maybe
  local.get $map_arity_err
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
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  i32.const 1
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $cx
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  i32.const 1158
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
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  i32.const 1
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $cx2
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  i32.const 1170
  local.get $val_types
  call $hydra.checking.check_same_type
  local.get $cx3
  local.get $tx
  local.get $type_args
  i32.const 7
  local.get $kt
  local.get $vt
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  call $hydra.checking.apply_type_arguments_to_type
  i32.const 1
  local.get $applied
  local.get $cx3
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.logic.if_else
)
  (func $hydra.checking.type_of_maybe (param $cx i32) (param $tx i32) (param $type_args i32) (param $mt i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $applied i32)
  (local $cx2 i32)
  (local $for_just i32)
  (local $for_nothing i32)
  (local $maybe_arity_err i32)
  (local $n i32)
  (local $t i32)
  (local $t_result i32)
  (local $ta0 i32)
  (local $term i32)
  (local $term_type i32)
  local.get $type_args
  call $hydra.lib.lists.length
  local.set $n
  i32.const 0
  i32.const 0
  i32.const 4
  i32.const 8
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
  local.get $n
  local.get $type_args
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.set $maybe_arity_err
  local.get $n
  i32.const 1
  call $hydra.lib.equality.equal
  local.get $maybe_arity_err
  i32.const 1
  i32.const 8
  local.get $ta0
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.get $cx
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $type_args
  call $hydra.lib.lists.maybe_head
  call $hydra.lib.maybes.maybe
  local.get $maybe_arity_err
  call $hydra.lib.logic.if_else
  local.set $for_nothing
  local.get $cx
  local.get $tx
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $term
  call $hydra.checking.type_of
  local.get $t_result
  call $hydra.lib.pairs.first
  local.set $term_type
  local.get $t_result
  call $hydra.lib.pairs.second
  local.set $cx2
  i32.const 8
  local.get $term_type
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.set $t
  local.get $cx2
  local.get $tx
  local.get $type_args
  local.get $t
  call $hydra.checking.apply_type_arguments_to_type
  i32.const 1
  local.get $applied
  local.get $cx2
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.set $for_just
  local.get $for_nothing
  local.get $for_just
  local.get $mt
  call $hydra.lib.maybes.maybe
)
  (func $hydra.checking.type_of_pair (param $cx i32) (param $tx i32) (param $type_args i32) (param $p i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
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
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $pair_snd
  call $hydra.checking.type_of
  local.get $result2
  call $hydra.lib.pairs.first
  local.set $second_type
  local.get $result2
  call $hydra.lib.pairs.second
  local.set $cx3
  i32.const 1
  i32.const 9
  local.get $first_type
  local.get $second_type
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx3
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  i32.const 0
  i32.const 0
  i32.const 4
  i32.const 9
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
  i32.const 2
  local.get $n
  local.get $type_args
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
)
  (func $hydra.checking.type_of_primitive (param $cx i32) (param $tx i32) (param $type_args i32) (param $name i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $_p i32)
  (local $applied i32)
  (local $cx2 i32)
  (local $inst_result i32)
  (local $raw_ts i32)
  (local $t i32)
  (local $ts i32)
  (local $ts_raw i32)
  local.get $_p
  i32.load offset=4
  local.get $name
  local.get $tx
  i32.load offset=20
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.map
  local.set $raw_ts
  i32.const 0
  i32.const 9
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $raw_ts
  call $hydra.lib.maybes.maybe
)
  (func $hydra.checking.type_of_projection (param $cx i32) (param $tx i32) (param $type_args i32) (param $p i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
  i32.load
  local.set $tname
  local.get $p
  i32.load offset=4
  local.set $fname
  local.get $cx
  local.get $tx
  i32.load offset=24
  local.get $tname
  call $hydra.resolution.require_schema_type
  local.get $schema_result
  call $hydra.lib.pairs.first
  local.set $schema_type
  local.get $schema_result
  call $hydra.lib.pairs.second
  local.set $cx2
  local.get $schema_type
  i32.load
  local.set $svars
  local.get $schema_type
  i32.load offset=4
  local.set $sbody
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
  i32.const 4
  local.get $tname
  local.get $type_args
  call $hydra.resolution.nominal_application
  local.get $sftyp
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx2
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
)
  (func $hydra.checking.type_of_record (param $cx i32) (param $tx i32) (param $type_args i32) (param $record i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
  i32.load
  local.set $tname
  local.get $record
  i32.load offset=4
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
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  i32.const 1
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $cx
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  i32.const 0
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  (func $hydra.checking.type_of_set (param $cx i32) (param $tx i32) (param $type_args i32) (param $els i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $acc i32)
  (local $acc_r i32)
  (local $cx2 i32)
  (local $cx_a i32)
  (local $cx_b i32)
  (local $eltypes i32)
  (local $fold_r i32)
  (local $fold_result i32)
  (local $set_arity_err i32)
  (local $t i32)
  (local $t_result i32)
  (local $ta0 i32)
  (local $term i32)
  (local $types i32)
  (local $unified_type i32)
  i32.const 0
  i32.const 0
  i32.const 4
  i32.const 11
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
  local.get $type_args
  call $hydra.lib.lists.length
  local.get $type_args
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.set $set_arity_err
  local.get $els
  call $hydra.lib.sets.null
  local.get $type_args
  call $hydra.lib.lists.length
  i32.const 1
  call $hydra.lib.equality.equal
  local.get $set_arity_err
  i32.const 1
  i32.const 11
  local.get $ta0
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.get $cx
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $type_args
  call $hydra.lib.lists.maybe_head
  call $hydra.lib.maybes.maybe
  local.get $set_arity_err
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
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  i32.const 1
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $cx
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  i32.const 1184
  local.get $eltypes
  call $hydra.checking.check_same_type
  i32.const 1
  i32.const 11
  local.get $unified_type
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.get $cx2
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  call $hydra.lib.logic.if_else
)
  (func $hydra.checking.type_of_term (param $cx i32) (param $g i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  i32.const 0
  local.get $cx
  local.get $g
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $term
  call $hydra.checking.type_of
  call $hydra.lib.eithers.map
)
  (func $hydra.checking.type_of_type_application (param $cx i32) (param $tx i32) (param $type_args i32) (param $tyapp i32) (result i32)
  (local $body i32)
  (local $t i32)
  local.get $tyapp
  i32.load
  local.set $body
  local.get $tyapp
  i32.load offset=4
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
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $applied i32)
  (local $body i32)
  (local $cx2 i32)
  (local $result1 i32)
  (local $t1 i32)
  (local $tx2 i32)
  (local $v i32)
  (local $vars i32)
  local.get $tl
  i32.load
  local.set $v
  local.get $tl
  i32.load offset=4
  local.set $body
  local.get $tx
  i32.load offset=28
  local.set $vars
  local.get $tx
  i32.load
  local.get $tx
  i32.load offset=4
  local.get $tx
  i32.load offset=8
  local.get $tx
  i32.load offset=12
  local.get $tx
  i32.load offset=16
  local.get $tx
  i32.load offset=20
  local.get $tx
  i32.load offset=24
  local.get $v
  local.get $vars
  call $hydra.lib.sets.insert
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
  local.set $tx2
  local.get $cx
  local.get $tx2
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
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
  i32.const 3
  local.get $v
  local.get $t1
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  call $hydra.checking.apply_type_arguments_to_type
  i32.const 1
  local.get $applied
  local.get $cx2
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  (func $hydra.checking.type_of_unit (param $cx i32) (param $tx i32) (param $type_args i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $applied i32)
  local.get $cx
  local.get $tx
  local.get $type_args
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
  call $hydra.checking.apply_type_arguments_to_type
  i32.const 1
  local.get $applied
  local.get $cx
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  (func $hydra.checking.type_of_unwrap (param $cx i32) (param $tx i32) (param $type_args i32) (param $tname i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
  i32.load offset=24
  local.get $tname
  call $hydra.resolution.require_schema_type
  local.get $schema_result
  call $hydra.lib.pairs.first
  local.set $schema_type
  local.get $schema_result
  call $hydra.lib.pairs.second
  local.set $cx2
  local.get $schema_type
  i32.load
  local.set $svars
  local.get $schema_type
  i32.load offset=4
  local.set $sbody
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
  i32.const 4
  local.get $tname
  local.get $type_args
  call $hydra.resolution.nominal_application
  local.get $swrapped
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx2
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  (func $hydra.checking.type_of_variable (param $cx i32) (param $tx i32) (param $type_args i32) (param $name i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
  i32.load offset=4
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.set $for_scheme
  i32.const 0
  i32.const 10
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
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
  local.get $for_scheme
  local.get $_p
  i32.load offset=4
  local.get $name
  local.get $tx
  i32.load offset=20
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.map
  call $hydra.lib.maybes.maybe
  local.get $for_scheme
  local.get $raw_type_scheme
  call $hydra.lib.maybes.maybe
)
  (func $hydra.checking.type_of_wrapped_term (param $cx i32) (param $tx i32) (param $type_args i32) (param $wt i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $body i32)
  (local $cx2 i32)
  (local $result i32)
  (local $tname i32)
  local.get $wt
  i32.load
  local.set $tname
  local.get $wt
  i32.load offset=4
  local.set $body
  local.get $cx
  local.get $tx
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  (func $hydra.checking.types_all_effectively_equal (param $tx i32) (param $tlist i32) (result i32)
  (local $acc i32)
  (local $all_vars i32)
  (local $any_contains_free_var i32)
  (local $contains_free_var i32)
  (local $schema_names i32)
  (local $t i32)
  (local $types i32)
  local.get $tx
  i32.load offset=24
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
  drop
  local.get $contains_free_var
  drop
  i32.const 0
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
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  local.get $tx
  local.get $t1
  call $hydra.checking.contains_in_scope_type_vars
  local.get $tx
  local.get $t2
  call $hydra.checking.contains_in_scope_type_vars
  local.get $tx
  local.get $t1
  call $hydra.resolution.fully_strip_and_normalize_type
  local.get $t2
  call $hydra.resolution.fully_strip_and_normalize_type
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
  call $hydra.checking.types_all_effectively_equal
  call $hydra.lib.logic.or
  call $hydra.lib.logic.or
)
)
