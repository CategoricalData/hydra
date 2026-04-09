(module
  (import "hydra.arity" "hydra.arity.type_arity" (func $hydra.arity.type_arity (param i32) (result i32) ) )
  (import "hydra.arity" "hydra.arity.type_scheme_arity" (func $hydra.arity.type_scheme_arity (param i32) (result i32) ) )
  (import "hydra.decode.core" "hydra.decode.core.type" (func $hydra.decode.core.type (param i32) (result i32) ) )
  (import "hydra.dependencies" "hydra.dependencies.type_dependency_names" (func $hydra.dependencies.type_dependency_names (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.require_binding" (func $hydra.lexical.require_binding (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bimap" (func $hydra.lib.eithers.bimap (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.gt" (func $hydra.lib.equality.gt (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.identity" (func $hydra.lib.equality.identity (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat" (func $hydra.lib.lists.concat (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.and" (func $hydra.lib.logic.and (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.or" (func $hydra.lib.logic.or (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.elems" (func $hydra.lib.maps.elems (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.empty" (func $hydra.lib.maps.empty (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.keys" (func $hydra.lib.maps.keys (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.union" (func $hydra.lib.maps.union (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cases" (func $hydra.lib.maybes.cases (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.is_just" (func $hydra.lib.maybes.is_just (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.difference" (func $hydra.lib.sets.difference (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.empty" (func $hydra.lib.sets.empty (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.member" (func $hydra.lib.sets.member (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.null" (func $hydra.lib.sets.null (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.singleton" (func $hydra.lib.sets.singleton (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.union" (func $hydra.lib.sets.union (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.split_on" (func $hydra.lib.strings.split_on (param i32) (result i32) ) )
  (import "hydra.reflect" "hydra.reflect.type_variant" (func $hydra.reflect.type_variant (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.fold_over_type" (func $hydra.rewriting.fold_over_type (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_term" (func $hydra.strip.deannotate_term (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_type" (func $hydra.strip.deannotate_type (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.predicates.is_complex_binding" (func $hydra.predicates.is_complex_binding) )
  (export "hydra.predicates.is_complex_term" (func $hydra.predicates.is_complex_term) )
  (export "hydra.predicates.is_complex_variable" (func $hydra.predicates.is_complex_variable) )
  (export "hydra.predicates.is_encoded_term" (func $hydra.predicates.is_encoded_term) )
  (export "hydra.predicates.is_encoded_type" (func $hydra.predicates.is_encoded_type) )
  (export "hydra.predicates.is_enum_row_type" (func $hydra.predicates.is_enum_row_type) )
  (export "hydra.predicates.is_enum_type" (func $hydra.predicates.is_enum_type) )
  (export "hydra.predicates.is_nominal_type" (func $hydra.predicates.is_nominal_type) )
  (export "hydra.predicates.is_serializable" (func $hydra.predicates.is_serializable) )
  (export "hydra.predicates.is_serializable_by_name" (func $hydra.predicates.is_serializable_by_name) )
  (export "hydra.predicates.is_serializable_type" (func $hydra.predicates.is_serializable_type) )
  (export "hydra.predicates.is_trivial_term" (func $hydra.predicates.is_trivial_term) )
  (export "hydra.predicates.is_type" (func $hydra.predicates.is_type) )
  (export "hydra.predicates.is_unit_term" (func $hydra.predicates.is_unit_term) )
  (export "hydra.predicates.is_unit_type" (func $hydra.predicates.is_unit_type) )
  (export "hydra.predicates.type_dependencies" (func $hydra.predicates.type_dependencies) )
  (func $hydra.predicates.is_complex_binding (param $tc i32) (param $b i32) (result i32)
  (local $is_complex i32)
  (local $is_non_nullary i32)
  (local $is_polymorphic i32)
  (local $mts i32)
  (local $term i32)
  (local $ts i32)
  local.get $b
  ;; project field: term
  local.set $term
  local.get $b
  ;; project field: type
  local.set $mts
  local.get $mts
  local.get $tc
  local.get $term
  call $hydra.predicates.is_complex_term
  local.get $ts
  ;; project field: variables
  call $hydra.lib.lists.null
  call $hydra.lib.logic.not
  local.set $is_polymorphic
  local.get $ts
  ;; project field: type
  call $hydra.arity.type_arity
  i32.const 0
  call $hydra.lib.equality.gt
  local.set $is_non_nullary
  local.get $tc
  local.get $term
  call $hydra.predicates.is_complex_term
  local.set $is_complex
  local.get $is_polymorphic
  local.get $is_non_nullary
  call $hydra.lib.logic.or
  local.get $is_complex
  call $hydra.lib.logic.or
  call $hydra.lib.maybes.cases
)
  (func $hydra.predicates.is_complex_term (param $tc i32) (param $t i32) (result i32)
  (local $name i32)
  (block $end_term (result i32)
  (block $variable
  (block $type_lambda
  (block $type_application
  (block $let
  local.get $t
  br_table $let $type_application $type_lambda $variable $variable
)
  i32.const 1
  br $end_term
)
  i32.const 1
  br $end_term
)
  i32.const 1
  br $end_term
)
  local.get $tc
  local.get $name
  call $hydra.predicates.is_complex_variable
  br $end_term
)
)
  (func $hydra.predicates.is_complex_variable (param $tc i32) (param $name i32) (result i32)
  (local $meta_lookup i32)
  (local $prim i32)
  (local $prim_lookup i32)
  (local $ts i32)
  (local $type_lookup i32)
  local.get $name
  local.get $tc
  ;; project field: metadata
  call $hydra.lib.maps.lookup
  local.set $meta_lookup
  local.get $meta_lookup
  call $hydra.lib.maybes.is_just
  i32.const 1
  local.get $name
  local.get $tc
  ;; project field: lambda_variables
  call $hydra.lib.sets.member
  i32.const 1
  local.get $name
  local.get $tc
  ;; project field: bound_types
  call $hydra.lib.maps.lookup
  local.set $type_lookup
  local.get $name
  local.get $tc
  ;; project field: primitives
  call $hydra.lib.maps.lookup
  local.set $prim_lookup
  i32.const 1
  local.get $prim
  ;; project field: type
  call $hydra.arity.type_scheme_arity
  i32.const 0
  call $hydra.lib.equality.gt
  local.get $prim_lookup
  call $hydra.lib.maybes.maybe
  local.get $ts
  call $hydra.arity.type_scheme_arity
  i32.const 0
  call $hydra.lib.equality.gt
  local.get $type_lookup
  call $hydra.lib.maybes.maybe
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
)
  (func $hydra.predicates.is_encoded_term (param $t i32) (result i32)
  (local $a i32)
  (block $end_term (result i32)
  (block $union
  (block $application
  local.get $t
  call $hydra.strip.deannotate_term
  br_table $application $union $union
)
  local.get $a
  ;; project field: function
  call $hydra.predicates.is_encoded_term
  br $end_term
)
  i32.const 0 ;; string: "hydra.core.Term"
  nop
  call $hydra.lib.equality.equal
  br $end_term
)
)
  (func $hydra.predicates.is_encoded_type (param $t i32) (result i32)
  (local $a i32)
  (block $end_term (result i32)
  (block $union
  (block $application
  local.get $t
  call $hydra.strip.deannotate_term
  br_table $application $union $union
)
  local.get $a
  ;; project field: function
  call $hydra.predicates.is_encoded_type
  br $end_term
)
  i32.const 0 ;; string: "hydra.core.Type"
  nop
  call $hydra.lib.equality.equal
  br $end_term
)
)
  (func $hydra.predicates.is_enum_row_type (param $rt i32) (result i32)
  (local $f i32)
  call $hydra.lib.logic.and
  i32.const 1
  local.get $f
  ;; project field: type
  call $hydra.strip.deannotate_type
  call $hydra.predicates.is_unit_type
  local.get $rt
  call $hydra.lib.lists.map
  call $hydra.lib.lists.foldl
)
  (func $hydra.predicates.is_enum_type (param $typ i32) (result i32)
  (local $rt i32)
  (block $end_type (result i32)
  (block $union
  local.get $typ
  call $hydra.strip.deannotate_type
  br_table $union $union
)
  local.get $rt
  call $hydra.predicates.is_enum_row_type
  br $end_type
)
)
  (func $hydra.predicates.is_nominal_type (param $typ i32) (result i32)
  (local $fa i32)
  (block $end_type (result i32)
  (block $forall
  (block $wrap
  (block $union
  (block $record
  local.get $typ
  call $hydra.strip.deannotate_type
  br_table $record $union $wrap $forall $forall
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
  local.get $fa
  ;; project field: body
  call $hydra.predicates.is_nominal_type
  br $end_type
)
)
  (func $hydra.predicates.is_serializable (param $cx i32) (param $graph i32) (param $el i32) (result i32)
  (local $all_variants i32)
  (local $deps i32)
  (local $m i32)
  (local $t i32)
  (local $typ i32)
  (local $variants i32)
  call $hydra.reflect.type_variant
  i32.const 0
  local.get $t
  local.get $m
  call $hydra.lib.lists.cons
  i32.const 0
  ;; list elements follow
  local.get $typ
  call $hydra.rewriting.fold_over_type
  call $hydra.lib.lists.map
  local.set $variants
  local.get $variants
  local.get $deps
  call $hydra.lib.maps.elems
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  call $hydra.lib.sets.from_list
  local.set $all_variants
  i32.const 0
  local.get $all_variants
  call $hydra.lib.sets.member
  call $hydra.lib.logic.not
  local.get $cx
  local.get $graph
  i32.const 0
  call $hydra.lib.equality.identity
  local.get $el
  ;; project field: name
  call $hydra.predicates.type_dependencies
  call $hydra.lib.eithers.map
)
  (func $hydra.predicates.is_serializable_by_name (param $cx i32) (param $graph i32) (param $name i32) (result i32)
  (local $all_variants i32)
  (local $deps i32)
  (local $m i32)
  (local $t i32)
  (local $typ i32)
  (local $variants i32)
  call $hydra.reflect.type_variant
  i32.const 0
  local.get $t
  local.get $m
  call $hydra.lib.lists.cons
  i32.const 0
  ;; list elements follow
  local.get $typ
  call $hydra.rewriting.fold_over_type
  call $hydra.lib.lists.map
  local.set $variants
  local.get $variants
  local.get $deps
  call $hydra.lib.maps.elems
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  call $hydra.lib.sets.from_list
  local.set $all_variants
  i32.const 0
  local.get $all_variants
  call $hydra.lib.sets.member
  call $hydra.lib.logic.not
  local.get $cx
  local.get $graph
  i32.const 0
  call $hydra.lib.equality.identity
  local.get $name
  call $hydra.predicates.type_dependencies
  call $hydra.lib.eithers.map
)
  (func $hydra.predicates.is_serializable_type (param $typ i32) (result i32)
  (local $all_variants i32)
  (local $m i32)
  (local $t i32)
  call $hydra.reflect.type_variant
  i32.const 0
  local.get $t
  local.get $m
  call $hydra.lib.lists.cons
  i32.const 0
  ;; list elements follow
  local.get $typ
  call $hydra.rewriting.fold_over_type
  call $hydra.lib.lists.map
  call $hydra.lib.sets.from_list
  local.set $all_variants
  i32.const 0
  local.get $all_variants
  call $hydra.lib.sets.member
  call $hydra.lib.logic.not
)
  (func $hydra.predicates.is_trivial_term (param $t i32) (result i32)
  (local $acc i32)
  (local $app i32)
  (local $arg i32)
  (local $e i32)
  (local $f i32)
  (local $fld i32)
  (local $fun i32)
  (local $inner i32)
  (local $opt i32)
  (local $rec i32)
  (local $ta i32)
  (local $tl i32)
  (local $wt i32)
  (block $end_term (result i32)
  (block $type_lambda
  (block $type_application
  (block $wrap
  (block $record
  (block $maybe
  (block $application
  (block $unit
  (block $variable
  (block $literal
  local.get $t
  call $hydra.strip.deannotate_term
  br_table $literal $variable $unit $application $maybe $record $wrap $type_application $type_lambda $type_lambda
)
  i32.const 1
  br $end_term
)
  i32.const 0 ;; string: "."
  nop
  call $hydra.lib.strings.split_on
  call $hydra.lib.lists.length
  i32.const 1
  call $hydra.lib.equality.equal
  br $end_term
)
  i32.const 1
  br $end_term
)
  local.get $app
  ;; project field: function
  local.set $fun
  local.get $app
  ;; project field: argument
  local.set $arg
  (block $end_term (result i32)
  (block $function
  local.get $fun
  br_table $function $function
)
  (block $end_function (result i32)
  (block $elimination
  local.get $f
  br_table $elimination $elimination
)
  (block $end_elimination (result i32)
  (block $wrap
  (block $record
  local.get $e
  br_table $record $wrap $wrap
)
  local.get $arg
  call $hydra.predicates.is_trivial_term
  br $end_elimination
)
  local.get $arg
  call $hydra.predicates.is_trivial_term
  br $end_elimination
)
  br $end_function
)
  br $end_term
)
  br $end_term
)
  i32.const 1
  local.get $inner
  call $hydra.predicates.is_trivial_term
  local.get $opt
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.get $acc
  local.get $fld
  ;; project field: term
  call $hydra.predicates.is_trivial_term
  call $hydra.lib.logic.and
  i32.const 1
  local.get $rec
  ;; project field: fields
  call $hydra.lib.lists.foldl
  br $end_term
)
  local.get $wt
  ;; project field: body
  call $hydra.predicates.is_trivial_term
  br $end_term
)
  local.get $ta
  ;; project field: body
  call $hydra.predicates.is_trivial_term
  br $end_term
)
  local.get $tl
  ;; project field: body
  call $hydra.predicates.is_trivial_term
  br $end_term
)
)
  (func $hydra.predicates.is_type (param $t i32) (result i32)
  (local $a i32)
  (local $l i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $variable
  (block $union
  (block $forall
  (block $application
  local.get $t
  call $hydra.strip.deannotate_type
  br_table $application $forall $union $variable $variable
)
  local.get $a
  ;; project field: function
  call $hydra.predicates.is_type
  br $end_type
)
  local.get $l
  ;; project field: body
  call $hydra.predicates.is_type
  br $end_type
)
  i32.const 0
  br $end_type
)
  local.get $v
  i32.const 0 ;; string: "hydra.core.Type"
  call $hydra.lib.equality.equal
  br $end_type
)
)
  (func $hydra.predicates.is_unit_term (param $arg_0 i32) (result i32)
  (block $end_term (result i32)
  (block $unit
  local.get $arg_0
  br_table $unit $unit
)
  i32.const 1
  br $end_term
)
)
  (func $hydra.predicates.is_unit_type (param $arg_0 i32) (result i32)
  (block $end_type (result i32)
  (block $unit
  local.get $arg_0
  br_table $unit $unit
)
  i32.const 1
  br $end_type
)
)
  (func $hydra.predicates.type_dependencies (param $cx i32) (param $graph i32) (param $with_schema i32) (param $transform i32) (param $name i32) (result i32)
  (local $_a i32)
  (local $_wc_a i32)
  (local $_wc_e i32)
  (local $cx1 i32)
  (local $deps i32)
  (local $el i32)
  (local $name2 i32)
  (local $names i32)
  (local $new_names i32)
  (local $new_seeds i32)
  (local $pair i32)
  (local $pairs i32)
  (local $refs i32)
  (local $require_type i32)
  (local $seeds i32)
  (local $to_pair i32)
  (local $typ i32)
  (local $visited i32)
  i32.const 0 ;; string: "type dependencies of "
  nop
  call $hydra.lib.strings.cat2
  local.get $cx
  ;; project field: trace
  call $hydra.lib.lists.cons
  local.get $cx
  ;; project field: messages
  local.get $cx
  ;; project field: other
  local.set $cx1
  local.get $cx1
  local.get $graph
  local.get $name2
  call $hydra.lexical.require_binding
  local.get $_wc_e
  local.get $cx1
  local.get $_wc_a
  nop
  local.get $_a
  local.get $graph
  local.get $el
  ;; project field: term
  call $hydra.decode.core.type
  call $hydra.lib.eithers.bimap
  call $hydra.lib.eithers.bimap
  call $hydra.lib.eithers.bind
  local.set $require_type
  local.get $name2
  local.get $typ
  local.get $transform
  local.get $name2
  local.get $require_type
  call $hydra.lib.eithers.map
  local.set $to_pair
  local.get $seeds
  call $hydra.lib.sets.null
  i32.const 1
  local.get $names
  local.get $to_pair
  local.get $seeds
  call $hydra.lib.sets.to_list
  call $hydra.lib.eithers.map_list
  local.get $names
  local.get $pairs
  call $hydra.lib.maps.from_list
  call $hydra.lib.maps.union
  local.set $new_names
  call $hydra.lib.sets.union
  call $hydra.lib.sets.empty
  local.get $with_schema
  local.get $pair
  call $hydra.lib.pairs.second
  call $hydra.dependencies.type_dependency_names
  local.get $pairs
  call $hydra.lib.lists.map
  call $hydra.lib.lists.foldl
  local.set $refs
  local.get $names
  call $hydra.lib.maps.keys
  call $hydra.lib.sets.from_list
  local.set $visited
  local.get $refs
  local.get $visited
  call $hydra.lib.sets.difference
  local.set $new_seeds
  local.get $new_seeds
  local.get $new_names
  local.get $deps
  call $hydra.lib.eithers.bind
  call $hydra.lib.logic.if_else
  local.set $deps
  local.get $name
  call $hydra.lib.sets.singleton
  call $hydra.lib.maps.empty
  local.get $deps
)
)
