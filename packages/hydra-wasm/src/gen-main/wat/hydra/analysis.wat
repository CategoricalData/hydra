(module
  (import "hydra.annotations" "hydra.annotations.normalize_term_annotations" (func $hydra.annotations.normalize_term_annotations (param i32) (result i32) ) )
  (import "hydra.checking" "hydra.checking.type_of_term" (func $hydra.checking.type_of_term (param i32) (result i32) ) )
  (import "hydra.constants" "hydra.constants.key_type" (func $hydra.constants.key_type (param i32) (result i32) ) )
  (import "hydra.decode.core" "hydra.decode.core.term" (func $hydra.decode.core.term (param i32) (result i32) ) )
  (import "hydra.decode.core" "hydra.decode.core.type" (func $hydra.decode.core.type (param i32) (result i32) ) )
  (import "hydra.dependencies" "hydra.dependencies.term_dependency_names" (func $hydra.dependencies.term_dependency_names (param i32) (result i32) ) )
  (import "hydra.dependencies" "hydra.dependencies.type_dependency_names" (func $hydra.dependencies.type_dependency_names (param i32) (result i32) ) )
  (import "hydra.encode.core" "hydra.encode.core.type" (func $hydra.encode.core.type (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bimap" (func $hydra.lib.eithers.bimap (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.reverse" (func $hydra.lib.lists.reverse (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.and" (func $hydra.lib.logic.and (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.or" (func $hydra.lib.logic.or (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.union" (func $hydra.lib.maps.union (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cat" (func $hydra.lib.maybes.cat (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.delete" (func $hydra.lib.sets.delete (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.empty" (func $hydra.lib.sets.empty (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.unions" (func $hydra.lib.sets.unions (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.namespace_of" (func $hydra.names.namespace_of (param i32) (result i32) ) )
  (import "hydra.predicates" "hydra.predicates.is_complex_binding" (func $hydra.predicates.is_complex_binding (param i32) (result i32) ) )
  (import "hydra.predicates" "hydra.predicates.is_encoded_term" (func $hydra.predicates.is_encoded_term (param i32) (result i32) ) )
  (import "hydra.predicates" "hydra.predicates.is_encoded_type" (func $hydra.predicates.is_encoded_type (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.fold_over_term" (func $hydra.rewriting.fold_over_term (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.extend_graph_for_lambda" (func $hydra.scoping.extend_graph_for_lambda (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.extend_graph_for_let" (func $hydra.scoping.extend_graph_for_let (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.extend_graph_for_type_lambda" (func $hydra.scoping.extend_graph_for_type_lambda (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_and_detype_term" (func $hydra.strip.deannotate_and_detype_term (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_term" (func $hydra.strip.deannotate_term (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.is_free_variable_in_term" (func $hydra.variables.is_free_variable_in_term (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.analysis.add_names_to_namespaces" (func $hydra.analysis.add_names_to_namespaces) )
  (export "hydra.analysis.analyze_function_term" (func $hydra.analysis.analyze_function_term) )
  (export "hydra.analysis.analyze_function_term_with" (func $hydra.analysis.analyze_function_term_with) )
  (export "hydra.analysis.analyze_function_term_with_finish" (func $hydra.analysis.analyze_function_term_with_finish) )
  (export "hydra.analysis.analyze_function_term_with_gather" (func $hydra.analysis.analyze_function_term_with_gather) )
  (export "hydra.analysis.definition_dependency_namespaces" (func $hydra.analysis.definition_dependency_namespaces) )
  (export "hydra.analysis.dependency_namespaces" (func $hydra.analysis.dependency_namespaces) )
  (export "hydra.analysis.gather_applications" (func $hydra.analysis.gather_applications) )
  (export "hydra.analysis.gather_args" (func $hydra.analysis.gather_args) )
  (export "hydra.analysis.gather_args_with_type_apps" (func $hydra.analysis.gather_args_with_type_apps) )
  (export "hydra.analysis.is_self_tail_recursive" (func $hydra.analysis.is_self_tail_recursive) )
  (export "hydra.analysis.is_simple_assignment" (func $hydra.analysis.is_simple_assignment) )
  (export "hydra.analysis.is_tail_recursive_in_tail_position" (func $hydra.analysis.is_tail_recursive_in_tail_position) )
  (export "hydra.analysis.module_contains_binary_literals" (func $hydra.analysis.module_contains_binary_literals) )
  (export "hydra.analysis.module_dependency_namespaces" (func $hydra.analysis.module_dependency_namespaces) )
  (export "hydra.analysis.namespaces_for_definitions" (func $hydra.analysis.namespaces_for_definitions) )
  (func $hydra.analysis.add_names_to_namespaces (param $encode_namespace i32) (param $names i32) (param $ns0 i32) (result i32)
  (local $ns i32)
  (local $nss i32)
  (local $to_pair i32)
  call $hydra.names.namespace_of
  local.get $names
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  call $hydra.lib.sets.from_list
  local.set $nss
  local.get $ns
  local.get $ns
  local.get $encode_namespace
  local.set $to_pair
  local.get $ns0
  ;; project field: focus
  local.get $ns0
  ;; project field: mapping
  local.get $to_pair
  local.get $nss
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  call $hydra.lib.maps.union
)
  (func $hydra.analysis.analyze_function_term (param $cx i32) (param $get_t_c i32) (param $set_t_c i32) (param $env i32) (param $term i32) (result i32)
  (local $b i32)
  (local $g i32)
  local.get $cx
  local.get $g
  local.get $b
  call $hydra.predicates.is_complex_binding
  i32.const 1
  i32.const 0
  call $hydra.lib.logic.if_else
  local.get $get_t_c
  local.get $set_t_c
  local.get $env
  local.get $term
  call $hydra.analysis.analyze_function_term_with
)
  (func $hydra.analysis.analyze_function_term_with (param $cx i32) (param $for_binding i32) (param $get_t_c i32) (param $set_t_c i32) (param $env i32) (param $term i32) (result i32)
  local.get $cx
  local.get $for_binding
  local.get $get_t_c
  local.get $set_t_c
  i32.const 1
  local.get $env
  i32.const 0
  ;; list elements follow
  i32.const 0
  ;; list elements follow
  i32.const 0
  ;; list elements follow
  i32.const 0
  ;; list elements follow
  i32.const 0
  ;; list elements follow
  local.get $term
  call $hydra.analysis.analyze_function_term_with_gather
)
  (func $hydra.analysis.analyze_function_term_with_finish (param $cx i32) (param $get_t_c i32) (param $f_env i32) (param $tparams i32) (param $args i32) (param $bindings i32) (param $doms i32) (param $tapps i32) (param $body i32) (result i32)
  (local $body_with_tapps i32)
  (local $c i32)
  (local $mcod i32)
  (local $trm i32)
  (local $typ i32)
  local.get $trm
  local.get $typ
  local.get $body
  local.get $tapps
  call $hydra.lib.lists.foldl
  local.set $body_with_tapps
  i32.const 0
  local.get $c
  local.get $cx
  local.get $f_env
  local.get $get_t_c
  local.get $body_with_tapps
  call $hydra.checking.type_of_term
  call $hydra.lib.eithers.either
  local.set $mcod
  i32.const 1
  local.get $tparams
  call $hydra.lib.lists.reverse
  local.get $args
  call $hydra.lib.lists.reverse
  local.get $bindings
  local.get $body_with_tapps
  local.get $doms
  call $hydra.lib.lists.reverse
  local.get $mcod
  local.get $f_env
)
  (func $hydra.analysis.analyze_function_term_with_gather (param $cx i32) (param $for_binding i32) (param $get_t_c i32) (param $set_t_c i32) (param $arg_mode i32) (param $g_env i32) (param $tparams i32) (param $args i32) (param $bindings i32) (param $doms i32) (param $tapps i32) (param $t i32) (result i32)
  (local $body i32)
  (local $dom i32)
  (local $f i32)
  (local $lam i32)
  (local $lt i32)
  (local $new_bindings i32)
  (local $new_env i32)
  (local $ta i32)
  (local $ta_body i32)
  (local $tl i32)
  (local $tl_body i32)
  (local $tvar i32)
  (local $typ i32)
  (local $v i32)
  (local $x_ i32)
  (block $end_term (result i32)
  (block $type_lambda
  (block $type_application
  (block $let
  (block $function
  local.get $t
  call $hydra.strip.deannotate_term
  br_table $function $let $type_application $type_lambda $type_lambda
)
  (block $end_function (result i32)
  (block $lambda
  local.get $f
  br_table $lambda $lambda
)
  local.get $arg_mode
  local.get $lam
  ;; project field: parameter
  local.set $v
  i32.const 0 ;; string: "_"
  local.get $x_
  local.get $lam
  ;; project field: domain
  call $hydra.lib.maybes.maybe
  local.set $dom
  local.get $lam
  ;; project field: body
  local.set $body
  local.get $g_env
  local.get $get_t_c
  local.get $lam
  call $hydra.scoping.extend_graph_for_lambda
  local.get $g_env
  local.get $set_t_c
  local.set $new_env
  local.get $cx
  local.get $for_binding
  local.get $get_t_c
  local.get $set_t_c
  local.get $arg_mode
  local.get $new_env
  local.get $tparams
  local.get $v
  local.get $args
  call $hydra.lib.lists.cons
  local.get $bindings
  local.get $dom
  local.get $doms
  call $hydra.lib.lists.cons
  local.get $tapps
  local.get $body
  call $hydra.analysis.analyze_function_term_with_gather
  local.get $cx
  local.get $get_t_c
  local.get $g_env
  local.get $tparams
  local.get $args
  local.get $bindings
  local.get $doms
  local.get $tapps
  local.get $t
  call $hydra.analysis.analyze_function_term_with_finish
  call $hydra.lib.logic.if_else
  br $end_function
)
  br $end_term
)
  local.get $lt
  ;; project field: bindings
  local.set $new_bindings
  local.get $lt
  ;; project field: body
  local.set $body
  local.get $for_binding
  local.get $g_env
  local.get $get_t_c
  local.get $lt
  call $hydra.scoping.extend_graph_for_let
  local.get $g_env
  local.get $set_t_c
  local.set $new_env
  local.get $cx
  local.get $for_binding
  local.get $get_t_c
  local.get $set_t_c
  i32.const 0
  local.get $new_env
  local.get $tparams
  local.get $args
  local.get $bindings
  local.get $new_bindings
  call $hydra.lib.lists.concat2
  local.get $doms
  local.get $tapps
  local.get $body
  call $hydra.analysis.analyze_function_term_with_gather
  br $end_term
)
  local.get $ta
  ;; project field: body
  local.set $ta_body
  local.get $ta
  ;; project field: type
  local.set $typ
  local.get $cx
  local.get $for_binding
  local.get $get_t_c
  local.get $set_t_c
  local.get $arg_mode
  local.get $g_env
  local.get $tparams
  local.get $args
  local.get $bindings
  local.get $doms
  local.get $typ
  local.get $tapps
  call $hydra.lib.lists.cons
  local.get $ta_body
  call $hydra.analysis.analyze_function_term_with_gather
  br $end_term
)
  local.get $tl
  ;; project field: parameter
  local.set $tvar
  local.get $tl
  ;; project field: body
  local.set $tl_body
  local.get $g_env
  local.get $get_t_c
  local.get $tl
  call $hydra.scoping.extend_graph_for_type_lambda
  local.get $g_env
  local.get $set_t_c
  local.set $new_env
  local.get $cx
  local.get $for_binding
  local.get $get_t_c
  local.get $set_t_c
  local.get $arg_mode
  local.get $new_env
  local.get $tvar
  local.get $tparams
  call $hydra.lib.lists.cons
  local.get $args
  local.get $bindings
  local.get $doms
  local.get $tapps
  local.get $tl_body
  call $hydra.analysis.analyze_function_term_with_gather
  br $end_term
)
)
  (func $hydra.analysis.definition_dependency_namespaces (param $defs i32) (result i32)
  (local $all_names i32)
  (local $def i32)
  (local $def_names i32)
  (local $term_def i32)
  (local $type_def i32)
  (block $end_definition (result i32)
  (block $term
  (block $type
  local.get $def
  br_table $type $term $term
)
  i32.const 1
  local.get $type_def
  ;; project field: type
  ;; project field: type
  call $hydra.dependencies.type_dependency_names
  br $end_definition
)
  i32.const 1
  i32.const 1
  i32.const 1
  local.get $term_def
  ;; project field: term
  call $hydra.dependencies.term_dependency_names
  br $end_definition
)
  local.set $def_names
  local.get $def_names
  local.get $defs
  call $hydra.lib.lists.map
  call $hydra.lib.sets.unions
  local.set $all_names
  call $hydra.names.namespace_of
  local.get $all_names
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  call $hydra.lib.sets.from_list
)
  (func $hydra.analysis.dependency_namespaces (param $cx i32) (param $graph i32) (param $binds i32) (param $with_prims i32) (param $with_noms i32) (param $with_schema i32) (param $els i32) (result i32)
  (local $_a i32)
  (local $_wc_a i32)
  (local $_wc_e i32)
  (local $data_names i32)
  (local $deannotated_term i32)
  (local $decoded_term i32)
  (local $dep_names i32)
  (local $el i32)
  (local $names_list i32)
  (local $schema_names i32)
  (local $term i32)
  (local $ts i32)
  (local $typ i32)
  local.get $el
  ;; project field: term
  local.set $term
  local.get $term
  call $hydra.strip.deannotate_term
  local.set $deannotated_term
  local.get $binds
  local.get $with_prims
  local.get $with_noms
  local.get $term
  call $hydra.dependencies.term_dependency_names
  local.set $data_names
  local.get $with_schema
  call $hydra.lib.sets.empty
  i32.const 1
  local.get $ts
  ;; project field: type
  call $hydra.dependencies.type_dependency_names
  local.get $el
  ;; project field: type
  call $hydra.lib.maybes.maybe
  call $hydra.lib.sets.empty
  call $hydra.lib.logic.if_else
  local.set $schema_names
  local.get $deannotated_term
  call $hydra.predicates.is_encoded_type
  i32.const 3
  ;; list elements follow
  local.get $data_names
  local.get $schema_names
  i32.const 1
  local.get $typ
  call $hydra.dependencies.type_dependency_names
  call $hydra.lib.sets.unions
  local.get $_wc_e
  i32.const 0 ;; string: "dependency namespace (type)"
  local.get $cx
  ;; project field: trace
  call $hydra.lib.lists.cons
  local.get $cx
  ;; project field: messages
  local.get $cx
  ;; project field: other
  local.get $_wc_a
  nop
  local.get $_a
  local.get $graph
  local.get $term
  call $hydra.decode.core.type
  call $hydra.lib.eithers.bimap
  call $hydra.lib.eithers.bimap
  call $hydra.lib.eithers.map
  local.get $deannotated_term
  call $hydra.predicates.is_encoded_term
  i32.const 3
  ;; list elements follow
  local.get $data_names
  local.get $schema_names
  local.get $binds
  local.get $with_prims
  local.get $with_noms
  local.get $decoded_term
  call $hydra.dependencies.term_dependency_names
  call $hydra.lib.sets.unions
  local.get $_wc_e
  i32.const 0 ;; string: "dependency namespace (term)"
  local.get $cx
  ;; project field: trace
  call $hydra.lib.lists.cons
  local.get $cx
  ;; project field: messages
  local.get $cx
  ;; project field: other
  local.get $_wc_a
  nop
  local.get $_a
  local.get $graph
  local.get $term
  call $hydra.decode.core.term
  call $hydra.lib.eithers.bimap
  call $hydra.lib.eithers.bimap
  call $hydra.lib.eithers.map
  i32.const 1
  i32.const 2
  ;; list elements follow
  local.get $data_names
  local.get $schema_names
  call $hydra.lib.sets.unions
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  local.set $dep_names
  call $hydra.names.namespace_of
  local.get $names_list
  call $hydra.lib.sets.unions
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  call $hydra.lib.sets.from_list
  local.get $dep_names
  local.get $els
  call $hydra.lib.eithers.map_list
  call $hydra.lib.eithers.map
)
  (func $hydra.analysis.gather_applications (param $term i32) (result i32)
  (local $app i32)
  (local $args i32)
  (local $go i32)
  (local $lhs i32)
  (local $rhs i32)
  (local $t i32)
  (block $end_term (result i32)
  (block $application
  local.get $t
  call $hydra.strip.deannotate_term
  br_table $application $application
)
  local.get $app
  ;; project field: function
  local.set $lhs
  local.get $app
  ;; project field: argument
  local.set $rhs
  local.get $rhs
  local.get $args
  call $hydra.lib.lists.cons
  local.get $lhs
  local.get $go
  br $end_term
)
  local.set $go
  i32.const 0
  ;; list elements follow
  local.get $term
  local.get $go
)
  (func $hydra.analysis.gather_args (param $term i32) (param $args i32) (result i32)
  (local $app i32)
  (local $body i32)
  (local $lhs i32)
  (local $rhs i32)
  (local $ta i32)
  (local $tl i32)
  (block $end_term (result i32)
  (block $type_application
  (block $type_lambda
  (block $application
  local.get $term
  call $hydra.strip.deannotate_term
  br_table $application $type_lambda $type_application $type_application
)
  local.get $app
  ;; project field: function
  local.set $lhs
  local.get $app
  ;; project field: argument
  local.set $rhs
  local.get $lhs
  local.get $rhs
  local.get $args
  call $hydra.lib.lists.cons
  call $hydra.analysis.gather_args
  br $end_term
)
  local.get $tl
  ;; project field: body
  local.set $body
  local.get $body
  local.get $args
  call $hydra.analysis.gather_args
  br $end_term
)
  local.get $ta
  ;; project field: body
  local.set $body
  local.get $body
  local.get $args
  call $hydra.analysis.gather_args
  br $end_term
)
)
  (func $hydra.analysis.gather_args_with_type_apps (param $term i32) (param $args i32) (param $ty_args i32) (result i32)
  (local $app i32)
  (local $body i32)
  (local $lhs i32)
  (local $rhs i32)
  (local $ta i32)
  (local $tl i32)
  (local $typ i32)
  (block $end_term (result i32)
  (block $type_application
  (block $type_lambda
  (block $application
  local.get $term
  call $hydra.strip.deannotate_term
  br_table $application $type_lambda $type_application $type_application
)
  local.get $app
  ;; project field: function
  local.set $lhs
  local.get $app
  ;; project field: argument
  local.set $rhs
  local.get $lhs
  local.get $rhs
  local.get $args
  call $hydra.lib.lists.cons
  local.get $ty_args
  call $hydra.analysis.gather_args_with_type_apps
  br $end_term
)
  local.get $tl
  ;; project field: body
  local.set $body
  local.get $body
  local.get $args
  local.get $ty_args
  call $hydra.analysis.gather_args_with_type_apps
  br $end_term
)
  local.get $ta
  ;; project field: body
  local.set $body
  local.get $ta
  ;; project field: type
  local.set $typ
  local.get $body
  local.get $args
  local.get $typ
  local.get $ty_args
  call $hydra.lib.lists.cons
  call $hydra.analysis.gather_args_with_type_apps
  br $end_term
)
)
  (func $hydra.analysis.is_self_tail_recursive (param $func_name i32) (param $body i32) (result i32)
  (local $calls_self i32)
  local.get $func_name
  local.get $body
  call $hydra.variables.is_free_variable_in_term
  call $hydra.lib.logic.not
  local.set $calls_self
  local.get $calls_self
  local.get $func_name
  local.get $body
  call $hydra.analysis.is_tail_recursive_in_tail_position
  i32.const 0
  call $hydra.lib.logic.if_else
)
  (func $hydra.analysis.is_simple_assignment (param $term i32) (result i32)
  (local $at i32)
  (local $f i32)
  (local $ta i32)
  (block $end_term (result i32)
  (block $type_application
  (block $type_lambda
  (block $let
  (block $function
  (block $annotated
  local.get $term
  br_table $annotated $function $let $type_lambda $type_application $type_application
)
  local.get $at
  ;; project field: body
  call $hydra.analysis.is_simple_assignment
  br $end_term
)
  (block $end_function (result i32)
  (block $lambda
  local.get $f
  br_table $lambda $lambda
)
  i32.const 0
  br $end_function
)
  br $end_term
)
  i32.const 0
  br $end_term
)
  i32.const 0
  br $end_term
)
  local.get $ta
  ;; project field: body
  call $hydra.analysis.is_simple_assignment
  br $end_term
)
)
  (func $hydra.analysis.is_tail_recursive_in_tail_position (param $func_name i32) (param $term i32) (result i32)
  (local $arg i32)
  (local $args_no_func i32)
  (local $args_no_lambda i32)
  (local $args_ok i32)
  (local $b i32)
  (local $bindings_ok i32)
  (local $branches_ok i32)
  (local $cases_ i32)
  (local $cs i32)
  (local $d i32)
  (local $dflt i32)
  (local $dflt_ok i32)
  (local $e i32)
  (local $f i32)
  (local $f2 i32)
  (local $field i32)
  (local $found i32)
  (local $gather_args i32)
  (local $gather_fun i32)
  (local $gathered i32)
  (local $ignore i32)
  (local $lam i32)
  (local $lt i32)
  (local $ok i32)
  (local $stripped i32)
  (local $stripped_fun i32)
  (local $t i32)
  (local $vname i32)
  local.get $term
  call $hydra.strip.deannotate_and_detype_term
  local.set $stripped
  (block $end_term (result i32)
  (block $let
  (block $function
  (block $application
  local.get $stripped
  br_table $application $function $let $let
)
  local.get $stripped
  call $hydra.analysis.gather_applications
  local.set $gathered
  local.get $gathered
  call $hydra.lib.pairs.first
  local.set $gather_args
  local.get $gathered
  call $hydra.lib.pairs.second
  local.set $gather_fun
  local.get $gather_fun
  call $hydra.strip.deannotate_and_detype_term
  local.set $stripped_fun
  (block $end_term (result i32)
  (block $function
  (block $variable
  local.get $stripped_fun
  br_table $variable $function $function
)
  local.get $vname
  local.get $func_name
  call $hydra.lib.equality.equal
  local.get $ok
  local.get $func_name
  local.get $arg
  call $hydra.variables.is_free_variable_in_term
  call $hydra.lib.logic.and
  i32.const 1
  local.get $gather_args
  call $hydra.lib.lists.foldl
  local.set $args_no_func
  local.get $ok
  i32.const 0
  local.get $found
  (block $end_term (result i32)
  (block $function
  local.get $t
  br_table $function $function
)
  (block $end_function (result i32)
  (block $lambda
  local.get $f2
  br_table $lambda $lambda
)
  local.get $lam
  ;; project field: body
  local.set $ignore
  i32.const 1
  br $end_function
)
  br $end_term
)
  call $hydra.lib.logic.or
  i32.const 0
  local.get $arg
  call $hydra.rewriting.fold_over_term
  call $hydra.lib.logic.not
  call $hydra.lib.logic.and
  i32.const 1
  local.get $gather_args
  call $hydra.lib.lists.foldl
  local.set $args_no_lambda
  local.get $args_no_func
  local.get $args_no_lambda
  call $hydra.lib.logic.and
  local.get $func_name
  local.get $term
  call $hydra.variables.is_free_variable_in_term
  call $hydra.lib.logic.if_else
  br $end_term
)
  (block $end_function (result i32)
  (block $elimination
  local.get $f
  br_table $elimination $elimination
)
  (block $end_elimination (result i32)
  (block $union
  local.get $e
  br_table $union $union
)
  local.get $cs
  ;; project field: cases
  local.set $cases_
  local.get $cs
  ;; project field: default
  local.set $dflt
  local.get $ok
  local.get $func_name
  local.get $field
  ;; project field: term
  call $hydra.analysis.is_tail_recursive_in_tail_position
  call $hydra.lib.logic.and
  i32.const 1
  local.get $cases_
  call $hydra.lib.lists.foldl
  local.set $branches_ok
  i32.const 1
  local.get $func_name
  local.get $d
  call $hydra.analysis.is_tail_recursive_in_tail_position
  local.get $dflt
  call $hydra.lib.maybes.maybe
  local.set $dflt_ok
  local.get $ok
  local.get $func_name
  local.get $arg
  call $hydra.variables.is_free_variable_in_term
  call $hydra.lib.logic.and
  i32.const 1
  local.get $gather_args
  call $hydra.lib.lists.foldl
  local.set $args_ok
  local.get $branches_ok
  local.get $dflt_ok
  call $hydra.lib.logic.and
  local.get $args_ok
  call $hydra.lib.logic.and
  br $end_elimination
)
  br $end_function
)
  br $end_term
)
  br $end_term
)
  (block $end_function (result i32)
  (block $lambda
  local.get $f
  br_table $lambda $lambda
)
  local.get $func_name
  local.get $lam
  ;; project field: body
  call $hydra.analysis.is_tail_recursive_in_tail_position
  br $end_function
)
  br $end_term
)
  local.get $ok
  local.get $func_name
  local.get $b
  ;; project field: term
  call $hydra.variables.is_free_variable_in_term
  call $hydra.lib.logic.and
  i32.const 1
  local.get $lt
  ;; project field: bindings
  call $hydra.lib.lists.foldl
  local.set $bindings_ok
  local.get $bindings_ok
  local.get $func_name
  local.get $lt
  ;; project field: body
  call $hydra.analysis.is_tail_recursive_in_tail_position
  call $hydra.lib.logic.and
  br $end_term
)
)
  (func $hydra.analysis.module_contains_binary_literals (param $mod i32) (result i32)
  (local $acc i32)
  (local $check_term i32)
  (local $d i32)
  (local $def_terms i32)
  (local $found i32)
  (local $lit i32)
  (local $t i32)
  (local $td i32)
  (local $term i32)
  (local $term_contains_binary i32)
  local.get $found
  (block $end_term (result i32)
  (block $literal
  local.get $term
  br_table $literal $literal
)
  (block $end_literal (result i32)
  (block $binary
  local.get $lit
  br_table $binary $binary
)
  i32.const 1
  br $end_literal
)
  br $end_term
)
  call $hydra.lib.logic.or
  local.set $check_term
  i32.const 0
  local.get $check_term
  i32.const 0
  local.get $term
  call $hydra.rewriting.fold_over_term
  local.set $term_contains_binary
  (block $end_definition (result i32)
  (block $term
  local.get $d
  br_table $term $term
)
  local.get $td
  ;; project field: term
  br $end_definition
)
  local.get $mod
  ;; project field: definitions
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  local.set $def_terms
  local.get $acc
  local.get $t
  local.get $term_contains_binary
  call $hydra.lib.logic.or
  i32.const 0
  local.get $def_terms
  call $hydra.lib.lists.foldl
)
  (func $hydra.analysis.module_dependency_namespaces (param $cx i32) (param $graph i32) (param $binds i32) (param $with_prims i32) (param $with_noms i32) (param $with_schema i32) (param $mod i32) (result i32)
  (local $all_bindings i32)
  (local $d i32)
  (local $data_term i32)
  (local $deps i32)
  (local $name i32)
  (local $schema_term i32)
  (local $td i32)
  (local $typ i32)
  (block $end_definition (result i32)
  (block $term
  (block $type
  local.get $d
  br_table $type $term $term
)
  i32.const 0 ;; string: "hydra.core.Type"
  local.set $schema_term
  local.get $typ
  call $hydra.encode.core.type
  i32.const 1
  ;; list elements follow
  call $hydra.constants.key_type
  local.get $schema_term
  call $hydra.lib.maps.from_list
  call $hydra.annotations.normalize_term_annotations
  local.set $data_term
  local.get $name
  local.get $data_term
  i32.const 0
  ;; list elements follow
  i32.const 0 ;; string: "hydra.core.Type"
  i32.const 0
  br $end_definition
)
  local.get $td
  ;; project field: name
  local.get $td
  ;; project field: term
  local.get $td
  ;; project field: type
  br $end_definition
)
  local.get $mod
  ;; project field: definitions
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  local.set $all_bindings
  local.get $mod
  ;; project field: namespace
  local.get $deps
  call $hydra.lib.sets.delete
  local.get $cx
  local.get $graph
  local.get $binds
  local.get $with_prims
  local.get $with_noms
  local.get $with_schema
  local.get $all_bindings
  call $hydra.analysis.dependency_namespaces
  call $hydra.lib.eithers.map
)
  (func $hydra.analysis.namespaces_for_definitions (param $encode_namespace i32) (param $focus_ns i32) (param $defs i32) (result i32)
  (local $ns i32)
  (local $nss i32)
  (local $to_pair i32)
  local.get $focus_ns
  local.get $defs
  call $hydra.analysis.definition_dependency_namespaces
  call $hydra.lib.sets.delete
  local.set $nss
  local.get $ns
  local.get $ns
  local.get $encode_namespace
  local.set $to_pair
  local.get $focus_ns
  local.get $to_pair
  local.get $to_pair
  local.get $nss
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
)
)
