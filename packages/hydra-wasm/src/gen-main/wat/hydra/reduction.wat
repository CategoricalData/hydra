(module
  (import "hydra.arity" "hydra.arity.primitive_arity" (func $hydra.arity.primitive_arity (param i32) (result i32) ) )
  (import "hydra.arity" "hydra.arity.type_arity" (func $hydra.arity.type_arity (param i32) (result i32) ) )
  (import "hydra.arity" "hydra.arity.type_scheme_arity" (func $hydra.arity.type_scheme_arity (param i32) (result i32) ) )
  (import "hydra.checking" "hydra.checking.type_of" (func $hydra.checking.type_of (param i32) (result i32) ) )
  (import "hydra.encode.core" "hydra.encode.core.type" (func $hydra.encode.core.type (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.injection" (func $hydra.extract.core.injection (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.record" (func $hydra.extract.core.record (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.wrap" (func $hydra.extract.core.wrap (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.lookup_binding" (func $hydra.lexical.lookup_binding (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.lookup_primitive" (func $hydra.lexical.lookup_primitive (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bimap" (func $hydra.lib.eithers.bimap (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_maybe" (func $hydra.lib.eithers.map_maybe (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.gt" (func $hydra.lib.equality.gt (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.lte" (func $hydra.lib.equality.lte (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.drop" (func $hydra.lib.lists.drop (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.filter" (func $hydra.lib.lists.filter (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.head" (func $hydra.lib.lists.head (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.reverse" (func $hydra.lib.lists.reverse (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.tail" (func $hydra.lib.lists.tail (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.take" (func $hydra.lib.lists.take (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.zip" (func $hydra.lib.lists.zip (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int32" (func $hydra.lib.literals.show_int32 (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.and" (func $hydra.lib.logic.and (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.or" (func $hydra.lib.logic.or (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.elems" (func $hydra.lib.maps.elems (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.singleton" (func $hydra.lib.maps.singleton (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.range" (func $hydra.lib.math.range (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.sub" (func $hydra.lib.math.sub (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.bind" (func $hydra.lib.maybes.bind (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.null" (func $hydra.lib.sets.null (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (import "hydra.resolution" "hydra.resolution.require_type" (func $hydra.resolution.require_type (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_term" (func $hydra.rewriting.rewrite_term (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_term_m" (func $hydra.rewriting.rewrite_term_m (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_term_with_context_m" (func $hydra.rewriting.rewrite_term_with_context_m (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_type_m" (func $hydra.rewriting.rewrite_type_m (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.extend_graph_for_lambda" (func $hydra.scoping.extend_graph_for_lambda (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.extend_graph_for_let" (func $hydra.scoping.extend_graph_for_let (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.extend_graph_for_type_lambda" (func $hydra.scoping.extend_graph_for_type_lambda (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.type_scheme_to_f_type" (func $hydra.scoping.type_scheme_to_f_type (param i32) (result i32) ) )
  (import "hydra.show.errors" "hydra.show.errors.error" (func $hydra.show.errors.error (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_term" (func $hydra.strip.deannotate_term (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.free_variables_in_term" (func $hydra.variables.free_variables_in_term (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.is_free_variable_in_term" (func $hydra.variables.is_free_variable_in_term (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.replace_free_term_variable" (func $hydra.variables.replace_free_term_variable (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.replace_free_type_variable" (func $hydra.variables.replace_free_type_variable (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.reduction.alpha_convert" (func $hydra.reduction.alpha_convert) )
  (export "hydra.reduction.beta_reduce_type" (func $hydra.reduction.beta_reduce_type) )
  (export "hydra.reduction.contract_term" (func $hydra.reduction.contract_term) )
  (export "hydra.reduction.count_primitive_invocations" (func $hydra.reduction.count_primitive_invocations) )
  (export "hydra.reduction.eta_expand_term" (func $hydra.reduction.eta_expand_term) )
  (export "hydra.reduction.eta_expand_typed_term" (func $hydra.reduction.eta_expand_typed_term) )
  (export "hydra.reduction.eta_expansion_arity" (func $hydra.reduction.eta_expansion_arity) )
  (export "hydra.reduction.eta_reduce_term" (func $hydra.reduction.eta_reduce_term) )
  (export "hydra.reduction.reduce_term" (func $hydra.reduction.reduce_term) )
  (export "hydra.reduction.term_is_closed" (func $hydra.reduction.term_is_closed) )
  (export "hydra.reduction.term_is_value" (func $hydra.reduction.term_is_value) )
  (func $hydra.reduction.alpha_convert (param $vold i32) (param $vnew i32) (param $term i32) (result i32)
  local.get $vold
  local.get $vnew
  local.get $term
  call $hydra.variables.replace_free_term_variable
)
  (func $hydra.reduction.beta_reduce_type (param $cx i32) (param $graph i32) (param $typ i32) (result i32)
  (local $a i32)
  (local $app i32)
  (local $at i32)
  (local $find_app i32)
  (local $ft i32)
  (local $lhs i32)
  (local $map_expr i32)
  (local $name i32)
  (local $r i32)
  (local $recurse i32)
  (local $reduce_app i32)
  (local $rhs i32)
  (local $t i32)
  (local $t' i32)
  local.get $app
  ;; project field: function
  local.set $lhs
  local.get $app
  ;; project field: argument
  local.set $rhs
  (block $end_type (result i32)
  (block $variable
  (block $forall
  (block $annotated
  local.get $lhs
  br_table $annotated $forall $variable $variable
)
  local.get $at
  ;; project field: body
  local.get $rhs
  local.get $reduce_app
  i32.const 1
  local.get $a
  local.get $at
  ;; project field: annotation
  call $hydra.lib.eithers.bind
  br $end_type
)
  local.get $cx
  local.get $graph
  local.get $ft
  ;; project field: parameter
  local.get $rhs
  local.get $ft
  ;; project field: body
  call $hydra.variables.replace_free_type_variable
  call $hydra.reduction.beta_reduce_type
  br $end_type
)
  local.get $cx
  local.get $graph
  local.get $name
  call $hydra.resolution.require_type
  local.get $cx
  local.get $graph
  local.get $t'
  local.get $rhs
  call $hydra.reduction.beta_reduce_type
  call $hydra.lib.eithers.bind
  br $end_type
)
  local.set $reduce_app
  (block $end_type (result i32)
  (block $application
  local.get $r
  br_table $application $application
)
  local.get $a
  local.get $reduce_app
  br $end_type
)
  local.set $find_app
  local.get $t
  local.get $recurse
  local.get $r
  local.get $find_app
  call $hydra.lib.eithers.bind
  local.set $map_expr
  local.get $map_expr
  local.get $typ
  call $hydra.rewriting.rewrite_type_m
)
  (func $hydra.reduction.contract_term (param $term i32) (result i32)
  (local $app i32)
  (local $body i32)
  (local $f i32)
  (local $l i32)
  (local $lhs i32)
  (local $rec i32)
  (local $recurse i32)
  (local $rewrite i32)
  (local $rhs i32)
  (local $t i32)
  (local $v i32)
  local.get $t
  local.get $recurse
  local.set $rec
  (block $end_term (result i32)
  (block $application
  local.get $rec
  br_table $application $application
)
  local.get $app
  ;; project field: function
  local.set $lhs
  local.get $app
  ;; project field: argument
  local.set $rhs
  (block $end_term (result i32)
  (block $function
  local.get $lhs
  call $hydra.strip.deannotate_term
  br_table $function $function
)
  (block $end_function (result i32)
  (block $lambda
  local.get $f
  br_table $lambda $lambda
)
  local.get $l
  ;; project field: parameter
  local.set $v
  local.get $l
  ;; project field: body
  local.set $body
  local.get $v
  local.get $body
  call $hydra.variables.is_free_variable_in_term
  local.get $body
  local.get $v
  local.get $rhs
  local.get $body
  call $hydra.variables.replace_free_term_variable
  call $hydra.lib.logic.if_else
  br $end_function
)
  br $end_term
)
  br $end_term
)
  local.set $rewrite
  local.get $rewrite
  local.get $term
  call $hydra.rewriting.rewrite_term
)
  (func $hydra.reduction.count_primitive_invocations (result i32)
  i32.const 1
)
  (func $hydra.reduction.eta_expand_term (param $tx0 i32) (param $term0 i32) (result i32)
  (local $_gpt_p i32)
  (local $after_recursion i32)
  (local $always_pad i32)
  (local $app i32)
  (local $applied i32)
  (local $arg i32)
  (local $args i32)
  (local $arity i32)
  (local $arty i32)
  (local $at i32)
  (local $at2 i32)
  (local $atyp i32)
  (local $b i32)
  (local $body i32)
  (local $branch_body i32)
  (local $branch_h_type i32)
  (local $codomain_type i32)
  (local $cs i32)
  (local $cs2 i32)
  (local $ct i32)
  (local $dom i32)
  (local $domain_types i32)
  (local $domains i32)
  (local $e i32)
  (local $el i32)
  (local $elim_head_type i32)
  (local $elim_term i32)
  (local $elm i32)
  (local $els i32)
  (local $expand i32)
  (local $f i32)
  (local $fn i32)
  (local $for_case_branch i32)
  (local $for_elimination i32)
  (local $for_field i32)
  (local $for_map i32)
  (local $for_pair i32)
  (local $ft2 i32)
  (local $ftyp i32)
  (local $fully_applied i32)
  (local $fully_applied_raw i32)
  (local $h_type i32)
  (local $head i32)
  (local $head_typ i32)
  (local $htyp2 i32)
  (local $i i32)
  (local $id_pair i32)
  (local $indexed_domains i32)
  (local $indices i32)
  (local $inj i32)
  (local $l i32)
  (local $l2 i32)
  (local $lhs i32)
  (local $lm i32)
  (local $lt i32)
  (local $map_binding i32)
  (local $mb i32)
  (local $mp i32)
  (local $mt i32)
  (local $mtyp i32)
  (local $n i32)
  (local $name i32)
  (local $needed i32)
  (local $nm i32)
  (local $num_args i32)
  (local $p i32)
  (local $pad_elim i32)
  (local $peel_function_domains i32)
  (local $pr i32)
  (local $prim_types i32)
  (local $r i32)
  (local $rc i32)
  (local $recurse i32)
  (local $remaining_type i32)
  (local $result i32)
  (local $rewrite_with_args i32)
  (local $rhs i32)
  (local $st i32)
  (local $t1 i32)
  (local $tat i32)
  (local $tat2 i32)
  (local $term i32)
  (local $term1 i32)
  (local $term_arity_with_context i32)
  (local $term_head_type i32)
  (local $tl i32)
  (local $tl2 i32)
  (local $trm i32)
  (local $trm2 i32)
  (local $tt i32)
  (local $tx i32)
  (local $tx1 i32)
  (local $tx2 i32)
  (local $typ i32)
  (local $v i32)
  (local $var_type i32)
  (local $vn i32)
  (local $vn2 i32)
  (local $wt i32)
  local.get $_gpt_p
  ;; project field: name
  local.get $_gpt_p
  ;; project field: type
  local.get $tx0
  ;; project field: primitives
  call $hydra.lib.maps.elems
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $prim_types
  (block $end_term (result i32)
  (block $variable
  (block $type_application
  (block $type_lambda
  (block $let
  (block $function
  (block $application
  (block $annotated
  local.get $term
  br_table $annotated $application $function $let $type_lambda $type_application $variable $variable
)
  local.get $tx
  local.get $at
  ;; project field: body
  local.get $term_arity_with_context
  br $end_term
)
  local.get $tx
  local.get $app
  ;; project field: function
  local.get $term_arity_with_context
  i32.const 1
  call $hydra.lib.math.sub
  br $end_term
)
  (block $end_function (result i32)
  (block $lambda
  (block $elimination
  local.get $f
  br_table $elimination $lambda $lambda
)
  i32.const 1
  br $end_function
)
  i32.const 0
  br $end_function
)
  br $end_term
)
  i32.const 0
  local.get $tx
  local.get $l
  call $hydra.scoping.extend_graph_for_let
  local.get $l
  ;; project field: body
  local.get $term_arity_with_context
  br $end_term
)
  local.get $tx
  local.get $tl
  call $hydra.scoping.extend_graph_for_type_lambda
  local.get $tl
  ;; project field: body
  local.get $term_arity_with_context
  br $end_term
)
  local.get $tx
  local.get $tat
  ;; project field: body
  local.get $term_arity_with_context
  br $end_term
)
  i32.const 0
  call $hydra.arity.type_scheme_arity
  local.get $name
  local.get $prim_types
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  call $hydra.arity.type_arity
  call $hydra.scoping.type_scheme_to_f_type
  local.get $name
  local.get $tx
  ;; project field: bound_types
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.map
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.set $term_arity_with_context
  local.get $n
  i32.const 0
  call $hydra.lib.equality.lte
  i32.const 0
  ;; list elements follow
  i32.const 0
  i32.const 1
  local.get $n
  call $hydra.lib.math.range
  call $hydra.lib.lists.map
  (block $end_type (result i32)
  (block $forall
  (block $application
  (block $annotated
  (block $function
  local.get $typ
  br_table $function $annotated $application $forall $forall
)
  local.get $ftyp
  ;; project field: domain
  local.get $n
  i32.const 1
  call $hydra.lib.math.sub
  local.get $ftyp
  ;; project field: codomain
  local.get $domain_types
  call $hydra.lib.lists.cons
  br $end_type
)
  local.get $n
  local.get $at
  ;; project field: body
  local.get $domain_types
  br $end_type
)
  local.get $n
  local.get $atyp
  ;; project field: function
  local.get $domain_types
  br $end_type
)
  i32.const 0
  i32.const 1
  local.get $n
  call $hydra.lib.math.range
  call $hydra.lib.lists.map
  br $end_type
)
  local.get $mt
  call $hydra.lib.maybes.maybe
  call $hydra.lib.logic.if_else
  local.set $domain_types
  local.get $n
  i32.const 0
  call $hydra.lib.equality.lte
  local.get $mtyp
  i32.const 0
  (block $end_type (result i32)
  (block $forall
  (block $application
  (block $annotated
  (block $function
  local.get $typ
  br_table $function $annotated $application $forall $forall
)
  local.get $ftyp
  ;; project field: codomain
  local.get $n
  i32.const 1
  call $hydra.lib.math.sub
  local.get $peel_function_domains
  br $end_type
)
  local.get $at
  ;; project field: body
  local.get $n
  local.get $peel_function_domains
  br $end_type
)
  local.get $atyp
  ;; project field: function
  local.get $n
  local.get $peel_function_domains
  br $end_type
)
  i32.const 0
  br $end_type
)
  local.get $mtyp
  call $hydra.lib.maybes.maybe
  call $hydra.lib.logic.if_else
  local.set $peel_function_domains
  local.get $lhs
  local.get $arg
  local.get $head
  local.get $args
  call $hydra.lib.lists.foldl
  local.set $applied
  local.get $args
  call $hydra.lib.lists.length
  local.set $num_args
  local.get $arity
  local.get $num_args
  call $hydra.lib.math.sub
  local.set $needed
  local.get $needed
  i32.const 0
  call $hydra.lib.equality.gt
  local.get $always_pad
  local.get $num_args
  i32.const 0
  call $hydra.lib.equality.gt
  call $hydra.lib.logic.or
  call $hydra.lib.logic.and
  i32.const 1
  local.get $needed
  call $hydra.lib.math.range
  local.set $indices
  local.get $head_typ
  local.get $num_args
  local.get $peel_function_domains
  local.set $remaining_type
  local.get $needed
  local.get $remaining_type
  local.get $domain_types
  local.set $domains
  local.get $remaining_type
  local.get $needed
  local.get $peel_function_domains
  local.set $codomain_type
  i32.const 0 ;; string: "v"
  local.get $i
  call $hydra.lib.literals.show_int32
  call $hydra.lib.strings.cat2
  local.set $vn
  local.get $body
  local.get $vn
  local.get $applied
  local.get $indices
  call $hydra.lib.lists.foldl
  local.set $fully_applied_raw
  local.get $fully_applied_raw
  local.get $fully_applied_raw
  i32.const 0 ;; string: "type"
  local.get $ct
  call $hydra.encode.core.type
  call $hydra.lib.maps.singleton
  local.get $codomain_type
  call $hydra.lib.maybes.maybe
  local.set $fully_applied
  local.get $indices
  local.get $domains
  call $hydra.lib.lists.zip
  local.set $indexed_domains
  local.get $id_pair
  call $hydra.lib.pairs.first
  local.set $i
  local.get $id_pair
  call $hydra.lib.pairs.second
  local.set $dom
  i32.const 0 ;; string: "v"
  local.get $i
  call $hydra.lib.literals.show_int32
  call $hydra.lib.strings.cat2
  local.set $vn
  local.get $vn
  local.get $dom
  local.get $body
  local.get $fully_applied
  local.get $indexed_domains
  call $hydra.lib.lists.reverse
  call $hydra.lib.lists.foldl
  local.get $applied
  call $hydra.lib.logic.if_else
  local.set $expand
  i32.const 0
  ;; list elements follow
  local.get $tx1
  local.get $term1
  local.get $rewrite_with_args
  local.set $recurse
  (block $end_term (result i32)
  (block $variable
  (block $type_application
  (block $type_lambda
  (block $let
  (block $function
  (block $annotated
  local.get $trm2
  br_table $annotated $function $let $type_lambda $type_application $variable $variable
)
  local.get $tx2
  local.get $at2
  ;; project field: body
  local.get $term_head_type
  br $end_term
)
  i32.const 0
  br $end_term
)
  i32.const 0
  local.get $tx2
  local.get $l2
  call $hydra.scoping.extend_graph_for_let
  local.get $l2
  ;; project field: body
  local.get $term_head_type
  br $end_term
)
  local.get $tx2
  local.get $tl2
  call $hydra.scoping.extend_graph_for_type_lambda
  local.get $tl2
  ;; project field: body
  local.get $term_head_type
  br $end_term
)
  local.get $tx2
  local.get $tat2
  ;; project field: body
  local.get $term_head_type
  (block $end_type (result i32)
  (block $forall
  local.get $htyp2
  br_table $forall $forall
)
  local.get $ft2
  ;; project field: parameter
  local.get $tat2
  ;; project field: type
  local.get $ft2
  ;; project field: body
  call $hydra.variables.replace_free_type_variable
  br $end_type
)
  call $hydra.lib.maybes.bind
  br $end_term
)
  call $hydra.scoping.type_scheme_to_f_type
  local.get $vn2
  local.get $tx2
  ;; project field: bound_types
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.map
  br $end_term
)
  local.set $term_head_type
  local.get $tx
  local.get $trm
  local.get $term_arity_with_context
  local.set $arity
  local.get $tx
  local.get $trm
  local.get $term_head_type
  local.set $h_type
  i32.const 0
  local.get $args
  local.get $arity
  local.get $h_type
  local.get $trm
  local.get $expand
  local.set $after_recursion
  local.get $f
  ;; project field: name
  local.get $tx
  local.get $f
  ;; project field: term
  local.get $recurse
  local.set $for_field
  local.get $tx
  local.get $f
  ;; project field: term
  local.get $recurse
  local.set $branch_body
  local.get $tx
  local.get $branch_body
  local.get $term_arity_with_context
  local.set $arty
  local.get $tx
  local.get $branch_body
  local.get $term_head_type
  local.set $branch_h_type
  local.get $f
  ;; project field: name
  i32.const 1
  i32.const 0
  ;; list elements follow
  local.get $arty
  local.get $branch_h_type
  local.get $branch_body
  local.get $expand
  local.set $for_case_branch
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
  local.get $tx
  local.get $t1
  local.get $recurse
  local.get $cs
  ;; project field: default
  call $hydra.lib.maybes.map
  local.get $for_case_branch
  local.get $cs
  ;; project field: cases
  call $hydra.lib.lists.map
  br $end_elimination
)
  local.get $nm
  br $end_elimination
)
  local.set $for_elimination
  local.get $tx
  local.get $pr
  call $hydra.lib.pairs.first
  local.get $recurse
  local.get $tx
  local.get $pr
  call $hydra.lib.pairs.second
  local.get $recurse
  local.set $for_pair
  local.get $for_pair
  local.get $mp
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
  local.get $term
  br_table $annotated $application $either $function $let $list $literal $map $maybe $pair $record $set $type_application $type_lambda $union $unit $variable $wrap $wrap
)
  local.get $tx
  local.get $at
  ;; project field: body
  local.get $recurse
  local.get $at
  ;; project field: annotation
  local.get $after_recursion
  br $end_term
)
  i32.const 0
  ;; list elements follow
  local.get $tx
  local.get $app
  ;; project field: argument
  local.get $rewrite_with_args
  local.set $rhs
  local.get $rhs
  local.get $args
  call $hydra.lib.lists.cons
  local.get $tx
  local.get $app
  ;; project field: function
  local.get $rewrite_with_args
  br $end_term
)
  i32.const 0
  local.get $tx
  local.get $l
  local.get $recurse
  i32.const 1
  local.get $tx
  local.get $r
  local.get $recurse
  local.get $e
  call $hydra.lib.eithers.either
  local.get $after_recursion
  br $end_term
)
  (block $end_function (result i32)
  (block $lambda
  (block $elimination
  local.get $fn
  br_table $elimination $lambda $lambda
)
  (block $end_elimination (result i32)
  (block $wrap
  (block $union
  (block $record
  local.get $elm
  br_table $record $union $wrap $wrap
)
  i32.const 0
  br $end_elimination
)
  i32.const 1
  br $end_elimination
)
  i32.const 0
  br $end_elimination
)
  local.set $pad_elim
  local.get $elm
  local.get $for_elimination
  local.set $elim_term
  (block $end_elimination (result i32)
  (block $union
  local.get $elm
  br_table $union $union
)
  local.get $cs2
  ;; project field: type_name
  i32.const 0
  br $end_elimination
)
  local.set $elim_head_type
  local.get $pad_elim
  local.get $args
  i32.const 1
  local.get $elim_head_type
  local.get $elim_term
  local.get $expand
  br $end_function
)
  local.get $tx
  local.get $lm
  call $hydra.scoping.extend_graph_for_lambda
  local.set $tx1
  i32.const 0
  ;; list elements follow
  local.get $tx1
  local.get $lm
  ;; project field: body
  local.get $rewrite_with_args
  local.set $body
  local.get $lm
  ;; project field: parameter
  local.get $lm
  ;; project field: domain
  local.get $body
  local.set $result
  local.get $tx
  local.get $result
  local.get $term_arity_with_context
  local.set $arty
  i32.const 0
  local.get $args
  local.get $arty
  i32.const 0
  local.get $result
  local.get $expand
  br $end_function
)
  br $end_term
)
  i32.const 0
  local.get $tx
  local.get $lt
  call $hydra.scoping.extend_graph_for_let
  local.set $tx1
  local.get $b
  ;; project field: name
  i32.const 0
  ;; list elements follow
  local.get $tx1
  local.get $b
  ;; project field: term
  local.get $rewrite_with_args
  local.get $b
  ;; project field: type
  local.set $map_binding
  local.get $map_binding
  local.get $lt
  ;; project field: bindings
  call $hydra.lib.lists.map
  i32.const 0
  ;; list elements follow
  local.get $tx1
  local.get $lt
  ;; project field: body
  local.get $rewrite_with_args
  local.set $result
  local.get $result
  local.get $after_recursion
  br $end_term
)
  local.get $tx
  local.get $el
  local.get $recurse
  local.get $els
  call $hydra.lib.lists.map
  local.get $after_recursion
  br $end_term
)
  local.get $v
  br $end_term
)
  local.get $mp
  local.get $for_map
  local.get $after_recursion
  br $end_term
)
  local.get $tx
  local.get $v
  local.get $recurse
  local.get $mb
  call $hydra.lib.maybes.map
  local.get $after_recursion
  br $end_term
)
  local.get $tx
  local.get $pr
  call $hydra.lib.pairs.first
  local.get $recurse
  local.get $tx
  local.get $pr
  call $hydra.lib.pairs.second
  local.get $recurse
  local.get $after_recursion
  br $end_term
)
  local.get $rc
  ;; project field: type_name
  local.get $for_field
  local.get $rc
  ;; project field: fields
  call $hydra.lib.lists.map
  local.get $after_recursion
  br $end_term
)
  local.get $tx
  local.get $el
  local.get $recurse
  local.get $st
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.sets.from_list
  local.get $after_recursion
  br $end_term
)
  local.get $tx
  local.get $tt
  ;; project field: body
  local.get $recurse
  local.get $tt
  ;; project field: type
  local.get $after_recursion
  br $end_term
)
  local.get $tx
  local.get $tl
  call $hydra.scoping.extend_graph_for_type_lambda
  local.set $tx1
  local.get $tl
  ;; project field: parameter
  i32.const 0
  ;; list elements follow
  local.get $tx1
  local.get $tl
  ;; project field: body
  local.get $rewrite_with_args
  local.set $result
  local.get $result
  local.get $after_recursion
  br $end_term
)
  local.get $inj
  ;; project field: type_name
  local.get $inj
  ;; project field: field
  local.get $for_field
  local.get $after_recursion
  br $end_term
)
  i32.const 0
  br $end_term
)
  local.get $tx
  local.get $term
  local.get $term_arity_with_context
  local.set $arty
  call $hydra.scoping.type_scheme_to_f_type
  local.get $vn
  local.get $tx
  ;; project field: bound_types
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.map
  local.set $var_type
  i32.const 0
  local.get $args
  local.get $arty
  local.get $var_type
  local.get $term
  local.get $expand
  br $end_term
)
  local.get $wt
  ;; project field: type_name
  local.get $tx
  local.get $wt
  ;; project field: body
  local.get $recurse
  local.get $after_recursion
  br $end_term
)
  local.set $rewrite_with_args
  i32.const 0
  ;; list elements follow
  local.get $tx0
  local.get $term0
  local.get $rewrite_with_args
  call $hydra.reduction.contract_term
)
  (func $hydra.reduction.eta_expand_typed_term (param $cx i32) (param $tx0 i32) (param $term0 i32) (result i32)
  (local $_tc i32)
  (local $a i32)
  (local $a2 i32)
  (local $ann i32)
  (local $arity i32)
  (local $arity_of i32)
  (local $at i32)
  (local $base i32)
  (local $body i32)
  (local $cases i32)
  (local $check_base i32)
  (local $cs i32)
  (local $dflt i32)
  (local $e i32)
  (local $elm i32)
  (local $elm2 i32)
  (local $extra_variables i32)
  (local $f i32)
  (local $for_case i32)
  (local $for_case_statement i32)
  (local $for_elimination i32)
  (local $for_function i32)
  (local $force_expansion i32)
  (local $forced i32)
  (local $i i32)
  (local $l i32)
  (local $lhs i32)
  (local $lhs2 i32)
  (local $lhsarity i32)
  (local $n i32)
  (local $name i32)
  (local $pad i32)
  (local $padn i32)
  (local $r i32)
  (local $rcases i32)
  (local $rdflt i32)
  (local $recurse i32)
  (local $recurse_or_force i32)
  (local $rewrite i32)
  (local $rewrite_spine i32)
  (local $rhs i32)
  (local $rhs2 i32)
  (local $t i32)
  (local $tat i32)
  (local $term i32)
  (local $term2 i32)
  (local $tl i32)
  (local $tname i32)
  (local $top_level i32)
  (local $tx i32)
  (local $tx2 i32)
  (local $tx3 i32)
  (local $txl i32)
  (local $txlt i32)
  (local $txt i32)
  (local $typ i32)
  (local $typ_cx i32)
  (local $type_args i32)
  (local $unwind i32)
  (local $vars i32)
  (block $end_term (result i32)
  (block $type_application
  (block $application
  (block $annotated
  local.get $term2
  br_table $annotated $application $type_application $type_application
)
  local.get $at
  ;; project field: body
  local.get $rewrite_spine
  local.get $at
  ;; project field: annotation
  local.set $ann
  i32.const 1
  local.get $body
  local.get $ann
  call $hydra.lib.eithers.bind
  br $end_term
)
  i32.const 0
  i32.const 1
  ;; list elements follow
  i32.const 0
  i32.const 0
  ;; list elements follow
  call $hydra.lib.logic.if_else
  local.set $l
  local.get $a
  ;; project field: function
  local.get $rewrite_spine
  i32.const 1
  i32.const 0
  local.get $l
  local.get $recurse
  local.get $tx
  local.get $a
  ;; project field: argument
  local.get $rewrite
  i32.const 1
  local.get $lhs
  local.get $rhs
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $tat
  ;; project field: body
  local.get $rewrite_spine
  local.get $tat
  ;; project field: type
  local.set $typ
  i32.const 1
  local.get $body
  local.get $typ
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.set $rewrite_spine
  local.get $_tc
  call $hydra.lib.pairs.first
  call $hydra.arity.type_arity
  local.get $cx
  local.get $tx2
  i32.const 0
  ;; list elements follow
  local.get $term2
  call $hydra.checking.type_of
  call $hydra.lib.eithers.map
  local.set $dflt
  (block $end_function (result i32)
  (block $lambda
  (block $elimination
  local.get $f
  br_table $elimination $lambda $lambda
)
  i32.const 1
  i32.const 1
  br $end_function
)
  local.get $tx3
  local.get $l
  call $hydra.scoping.extend_graph_for_lambda
  local.set $txl
  local.get $txl
  local.get $l
  ;; project field: body
  local.get $arity_of
  br $end_function
)
  local.set $for_function
  (block $end_term (result i32)
  (block $variable
  (block $type_lambda
  (block $type_application
  (block $let
  (block $function
  (block $annotated
  local.get $term2
  br_table $annotated $function $let $type_application $type_lambda $variable $variable
)
  local.get $tx2
  local.get $at
  ;; project field: body
  local.get $arity_of
  br $end_term
)
  local.get $tx2
  local.get $f
  local.get $for_function
  br $end_term
)
  i32.const 0
  local.get $tx2
  local.get $l
  call $hydra.scoping.extend_graph_for_let
  local.set $txl
  local.get $txl
  local.get $l
  ;; project field: body
  local.get $arity_of
  br $end_term
)
  local.get $tx2
  local.get $tat
  ;; project field: body
  local.get $arity_of
  br $end_term
)
  local.get $tx2
  local.get $tl
  call $hydra.scoping.extend_graph_for_type_lambda
  local.set $txt
  local.get $txt
  local.get $tl
  ;; project field: body
  local.get $arity_of
  br $end_term
)
  local.get $_tc
  call $hydra.lib.pairs.first
  call $hydra.arity.type_arity
  local.get $cx
  local.get $tx2
  i32.const 0
  ;; list elements follow
  local.get $name
  call $hydra.checking.type_of
  call $hydra.lib.eithers.map
  i32.const 1
  local.get $t
  call $hydra.arity.type_arity
  call $hydra.scoping.type_scheme_to_f_type
  local.get $name
  local.get $tx2
  ;; project field: bound_types
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.map
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.set $arity_of
  i32.const 0 ;; string: "v"
  local.get $i
  call $hydra.lib.literals.show_int32
  call $hydra.lib.strings.cat2
  i32.const 1
  local.get $n
  call $hydra.lib.math.range
  call $hydra.lib.lists.map
  local.set $extra_variables
  local.get $vars
  call $hydra.lib.lists.null
  local.get $body
  local.get $vars
  call $hydra.lib.lists.head
  i32.const 0
  local.get $vars
  call $hydra.lib.lists.tail
  local.get $body
  local.get $vars
  call $hydra.lib.lists.head
  local.get $pad
  call $hydra.lib.logic.if_else
  local.set $pad
  local.get $n
  local.get $extra_variables
  local.get $body
  local.get $pad
  local.set $padn
  local.get $e
  local.get $t
  local.get $term2
  local.get $type_args
  call $hydra.lib.lists.foldl
  local.set $unwind
  local.get $cx
  local.get $tx
  i32.const 0
  ;; list elements follow
  local.get $t
  call $hydra.checking.type_of
  local.get $typ_cx
  call $hydra.lib.pairs.first
  call $hydra.arity.type_arity
  local.set $arity
  i32.const 1
  local.get $arity
  local.get $t
  local.get $unwind
  local.get $padn
  call $hydra.lib.eithers.bind
  local.set $force_expansion
  local.get $forced
  local.get $term2
  local.get $force_expansion
  local.get $tx
  local.get $term2
  local.get $unwind
  local.get $recurse
  call $hydra.lib.logic.if_else
  local.set $recurse_or_force
  i32.const 0
  i32.const 1
  i32.const 0
  ;; list elements follow
  local.get $recurse
  local.get $tx
  local.get $f
  ;; project field: term
  local.get $rewrite
  i32.const 1
  local.get $f
  ;; project field: name
  local.get $r
  call $hydra.lib.eithers.bind
  local.set $for_case
  local.get $cs
  ;; project field: type_name
  local.set $tname
  local.get $cs
  ;; project field: default
  local.set $dflt
  local.get $cs
  ;; project field: cases
  local.set $cases
  i32.const 0
  i32.const 0
  i32.const 0
  ;; list elements follow
  local.get $recurse
  local.get $tx
  local.get $rewrite
  local.get $dflt
  call $hydra.lib.eithers.map_maybe
  local.get $for_case
  local.get $cases
  call $hydra.lib.eithers.map_list
  i32.const 1
  local.get $tname
  local.get $rdflt
  local.get $rcases
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  local.set $for_case_statement
  (block $end_elimination (result i32)
  (block $union
  local.get $elm2
  br_table $union $union
)
  local.get $cs
  local.get $for_case_statement
  br $end_elimination
)
  local.set $check_base
  local.get $unwind
  local.get $elm
  local.get $check_base
  call $hydra.lib.eithers.map
  i32.const 1
  local.get $top_level
  local.get $forced
  call $hydra.lib.logic.or
  i32.const 1
  local.get $base
  local.get $padn
  local.get $base
  call $hydra.lib.logic.if_else
  call $hydra.lib.eithers.bind
  local.set $for_elimination
  (block $end_term (result i32)
  (block $type_lambda
  (block $type_application
  (block $let
  (block $function
  (block $application
  local.get $term
  br_table $application $function $let $type_application $type_lambda $type_lambda
)
  local.get $a
  ;; project field: function
  local.set $lhs
  local.get $a
  ;; project field: argument
  local.set $rhs
  i32.const 1
  i32.const 0
  i32.const 0
  ;; list elements follow
  local.get $recurse
  local.get $tx
  local.get $rhs
  local.get $rewrite
  local.get $tx
  local.get $lhs
  local.get $arity_of
  local.get $lhs
  local.get $rewrite_spine
  local.get $lhs2
  local.get $rhs2
  local.set $a2
  i32.const 1
  local.get $lhsarity
  i32.const 1
  call $hydra.lib.equality.gt
  local.get $lhsarity
  i32.const 1
  call $hydra.lib.math.sub
  local.get $a2
  local.get $padn
  local.get $a2
  call $hydra.lib.logic.if_else
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  (block $end_function (result i32)
  (block $lambda
  (block $elimination
  local.get $f
  br_table $elimination $lambda $lambda
)
  local.get $elm
  local.get $for_elimination
  br $end_function
)
  local.get $tx
  local.get $l
  call $hydra.scoping.extend_graph_for_lambda
  local.set $txl
  local.get $unwind
  local.get $txl
  local.get $term
  local.get $recurse
  call $hydra.lib.eithers.map
  br $end_function
)
  br $end_term
)
  i32.const 0
  local.get $tx
  local.get $l
  call $hydra.scoping.extend_graph_for_let
  local.set $txlt
  local.get $txlt
  local.get $term
  local.get $recurse
  br $end_term
)
  local.get $top_level
  local.get $forced
  local.get $tat
  ;; project field: type
  local.get $type_args
  call $hydra.lib.lists.cons
  local.get $recurse
  local.get $tx
  local.get $tat
  ;; project field: body
  local.get $rewrite
  br $end_term
)
  local.get $tx
  local.get $tl
  call $hydra.scoping.extend_graph_for_type_lambda
  local.set $txt
  local.get $txt
  local.get $term
  local.get $recurse
  br $end_term
)
  local.set $rewrite
  i32.const 1
  i32.const 0
  i32.const 0
  ;; list elements follow
  local.get $rewrite
  local.get $tx0
  local.get $term0
  call $hydra.rewriting.rewrite_term_with_context_m
)
  (func $hydra.reduction.eta_expansion_arity (param $graph i32) (param $term i32) (result i32)
  (local $app i32)
  (local $at i32)
  (local $b i32)
  (local $f i32)
  (local $name i32)
  (local $ta i32)
  (local $ts i32)
  (local $tt i32)
  (block $end_term (result i32)
  (block $variable
  (block $type_application
  (block $type_lambda
  (block $function
  (block $application
  (block $annotated
  local.get $term
  br_table $annotated $application $function $type_lambda $type_application $variable $variable
)
  local.get $graph
  local.get $at
  ;; project field: body
  call $hydra.reduction.eta_expansion_arity
  br $end_term
)
  local.get $graph
  local.get $app
  ;; project field: function
  call $hydra.reduction.eta_expansion_arity
  i32.const 1
  call $hydra.lib.math.sub
  br $end_term
)
  (block $end_function (result i32)
  (block $lambda
  (block $elimination
  local.get $f
  br_table $elimination $lambda $lambda
)
  i32.const 1
  br $end_function
)
  i32.const 0
  br $end_function
)
  br $end_term
)
  local.get $graph
  local.get $ta
  ;; project field: body
  call $hydra.reduction.eta_expansion_arity
  br $end_term
)
  local.get $graph
  local.get $tt
  ;; project field: body
  call $hydra.reduction.eta_expansion_arity
  br $end_term
)
  i32.const 0
  local.get $ts
  ;; project field: type
  call $hydra.arity.type_arity
  local.get $graph
  local.get $name
  call $hydra.lexical.lookup_binding
  local.get $b
  ;; project field: type
  call $hydra.lib.maybes.bind
  call $hydra.lib.maybes.maybe
  br $end_term
)
)
  (func $hydra.reduction.eta_reduce_term (param $term i32) (result i32)
  (local $app i32)
  (local $at i32)
  (local $body i32)
  (local $d i32)
  (local $f i32)
  (local $l i32)
  (local $lhs i32)
  (local $no_change i32)
  (local $reduce_lambda i32)
  (local $rhs i32)
  (local $v i32)
  local.get $term
  local.set $no_change
  local.get $l
  ;; project field: parameter
  local.set $v
  local.get $l
  ;; project field: domain
  local.set $d
  local.get $l
  ;; project field: body
  local.set $body
  (block $end_term (result i32)
  (block $application
  (block $annotated
  local.get $body
  call $hydra.reduction.eta_reduce_term
  br_table $annotated $application $application
)
  local.get $v
  local.get $d
  local.get $at
  ;; project field: body
  local.get $reduce_lambda
  br $end_term
)
  local.get $app
  ;; project field: function
  local.set $lhs
  local.get $app
  ;; project field: argument
  local.set $rhs
  (block $end_term (result i32)
  (block $variable
  (block $annotated
  local.get $rhs
  call $hydra.reduction.eta_reduce_term
  br_table $annotated $variable $variable
)
  local.get $v
  local.get $d
  local.get $lhs
  local.get $at
  ;; project field: body
  local.get $reduce_lambda
  br $end_term
)
  nop
  nop
  call $hydra.lib.equality.equal
  local.get $v
  local.get $lhs
  call $hydra.variables.is_free_variable_in_term
  call $hydra.lib.logic.not
  call $hydra.lib.logic.and
  local.get $lhs
  call $hydra.reduction.eta_reduce_term
  local.get $no_change
  call $hydra.lib.logic.if_else
  br $end_term
)
  br $end_term
)
  local.set $reduce_lambda
  (block $end_term (result i32)
  (block $function
  (block $annotated
  local.get $term
  br_table $annotated $function $function
)
  local.get $at
  ;; project field: body
  call $hydra.reduction.eta_reduce_term
  local.get $at
  ;; project field: annotation
  br $end_term
)
  (block $end_function (result i32)
  (block $lambda
  local.get $f
  br_table $lambda $lambda
)
  local.get $l
  local.get $reduce_lambda
  br $end_function
)
  br $end_term
)
)
  (func $hydra.reduction.reduce_term (param $cx i32) (param $graph i32) (param $eager i32) (param $term i32) (result i32)
  (local $app i32)
  (local $apply_elimination i32)
  (local $apply_if_nullary i32)
  (local $apply_to_arguments i32)
  (local $arg i32)
  (local $arg_list i32)
  (local $args i32)
  (local $args2 i32)
  (local $arity i32)
  (local $b i32)
  (local $binding i32)
  (local $bindings i32)
  (local $body i32)
  (local $bs i32)
  (local $cs i32)
  (local $do_recurse i32)
  (local $eager2 i32)
  (local $elm i32)
  (local $expand_binding i32)
  (local $expanded_bindings i32)
  (local $expanded_body i32)
  (local $f i32)
  (local $field i32)
  (local $fields i32)
  (local $for_elimination i32)
  (local $for_lambda i32)
  (local $for_primitive i32)
  (local $fun i32)
  (local $ic i32)
  (local $inner i32)
  (local $is_non_lambda i32)
  (local $is_non_lambda_term i32)
  (local $l i32)
  (local $let_expr i32)
  (local $lt i32)
  (local $m_binding i32)
  (local $m_prim i32)
  (local $map_error_to_string i32)
  (local $mapping i32)
  (local $matching_fields i32)
  (local $mid i32)
  (local $name i32)
  (local $original i32)
  (local $param i32)
  (local $prim i32)
  (local $prim_result i32)
  (local $proj i32)
  (local $recurse i32)
  (local $reduce i32)
  (local $reduce_arg i32)
  (local $reduced_arg i32)
  (local $reduced_args i32)
  (local $reduced_body i32)
  (local $reduced_result i32)
  (local $remaining_args i32)
  (local $stripped i32)
  (local $stripped_args i32)
  (local $substitute_all i32)
  (local $substitute_binding i32)
  (local $term2 i32)
  (local $v i32)
  (local $x i32)
  local.get $cx
  local.get $graph
  local.get $eager2
  call $hydra.reduction.reduce_term
  local.set $reduce
  (block $end_function (result i32)
  (block $lambda
  local.get $f
  br_table $lambda $lambda
)
  i32.const 0
  br $end_function
)
  local.set $is_non_lambda
  (block $end_term (result i32)
  (block $let
  (block $function
  local.get $term2
  br_table $function $let $let
)
  local.get $f
  local.get $is_non_lambda
  br $end_term
)
  i32.const 0
  br $end_term
)
  local.set $is_non_lambda_term
  local.get $eager2
  local.get $is_non_lambda_term
  call $hydra.lib.logic.and
  local.set $do_recurse
  local.get $eager2
  i32.const 1
  local.get $arg
  i32.const 0
  local.get $arg
  local.get $reduce
  call $hydra.lib.logic.if_else
  local.set $reduce_arg
  local.get $args
  call $hydra.lib.lists.null
  local.get $fun
  local.get $fun
  local.get $args
  call $hydra.lib.lists.head
  local.get $args
  call $hydra.lib.lists.tail
  local.get $apply_to_arguments
  call $hydra.lib.logic.if_else
  local.set $apply_to_arguments
  local.get $ic
  ;; project field: object
  call $hydra.show.errors.error
  local.get $ic
  ;; project field: context
  local.set $map_error_to_string
  (block $end_elimination (result i32)
  (block $wrap
  (block $union
  (block $record
  local.get $elm
  br_table $record $union $wrap $wrap
)
  local.get $cx
  local.get $proj
  ;; project field: type_name
  local.get $graph
  local.get $reduced_arg
  call $hydra.strip.deannotate_term
  call $hydra.extract.core.record
  local.get $f
  ;; project field: name
  local.get $proj
  ;; project field: field
  call $hydra.lib.equality.equal
  local.get $fields
  call $hydra.lib.lists.filter
  local.set $matching_fields
  local.get $matching_fields
  call $hydra.lib.lists.null
  i32.const 0
  i32.const 5
  ;; list elements follow
  i32.const 0 ;; string: "no such field: "
  nop
  i32.const 0 ;; string: " in "
  nop
  i32.const 0 ;; string: " record"
  call $hydra.lib.strings.cat
  local.get $cx
  i32.const 1
  local.get $matching_fields
  call $hydra.lib.lists.head
  ;; project field: term
  call $hydra.lib.logic.if_else
  call $hydra.lib.eithers.bind
  br $end_elimination
)
  local.get $cx
  local.get $cs
  ;; project field: type_name
  local.get $graph
  local.get $reduced_arg
  call $hydra.extract.core.injection
  local.get $f
  ;; project field: name
  local.get $field
  ;; project field: name
  call $hydra.lib.equality.equal
  local.get $cs
  ;; project field: cases
  call $hydra.lib.lists.filter
  local.set $matching_fields
  local.get $matching_fields
  call $hydra.lib.lists.null
  i32.const 0
  i32.const 5
  ;; list elements follow
  i32.const 0 ;; string: "no such field "
  nop
  i32.const 0 ;; string: " in "
  nop
  i32.const 0 ;; string: " case statement"
  call $hydra.lib.strings.cat
  local.get $cx
  i32.const 1
  local.get $x
  local.get $cs
  ;; project field: default
  call $hydra.lib.maybes.maybe
  i32.const 1
  local.get $matching_fields
  call $hydra.lib.lists.head
  ;; project field: term
  local.get $field
  ;; project field: term
  call $hydra.lib.logic.if_else
  call $hydra.lib.eithers.bind
  br $end_elimination
)
  local.get $cx
  local.get $name
  local.get $graph
  local.get $reduced_arg
  call $hydra.extract.core.wrap
  br $end_elimination
)
  local.set $apply_elimination
  local.get $original
  call $hydra.strip.deannotate_term
  local.set $stripped
  local.get $args2
  call $hydra.lib.lists.head
  local.set $arg
  local.get $args2
  call $hydra.lib.lists.tail
  local.set $remaining_args
  local.get $eager2
  local.get $arg
  call $hydra.strip.deannotate_term
  local.get $reduce_arg
  local.get $elm
  local.get $reduced_arg
  local.get $apply_elimination
  local.get $eager2
  local.get $reduce
  call $hydra.lib.eithers.bind
  local.get $eager2
  local.get $reduced_result
  local.get $remaining_args
  local.get $apply_if_nullary
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  local.set $for_elimination
  local.get $l
  ;; project field: parameter
  local.set $param
  local.get $l
  ;; project field: body
  local.set $body
  local.get $args2
  call $hydra.lib.lists.head
  local.set $arg
  local.get $args2
  call $hydra.lib.lists.tail
  local.set $remaining_args
  local.get $eager2
  local.get $arg
  call $hydra.strip.deannotate_term
  local.get $reduce
  local.get $eager2
  local.get $param
  local.get $reduced_arg
  local.get $body
  call $hydra.variables.replace_free_term_variable
  local.get $reduce
  local.get $eager2
  local.get $reduced_result
  local.get $remaining_args
  local.get $apply_if_nullary
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  local.set $for_lambda
  local.get $arity
  local.get $args2
  call $hydra.lib.lists.take
  local.set $arg_list
  local.get $arity
  local.get $args2
  call $hydra.lib.lists.drop
  local.set $remaining_args
  local.get $eager2
  local.get $reduce_arg
  local.get $arg_list
  call $hydra.lib.eithers.map_list
  call $hydra.strip.deannotate_term
  local.get $reduced_args
  call $hydra.lib.lists.map
  local.set $stripped_args
  local.get $map_error_to_string
  local.get $x
  local.get $prim
  local.get $cx
  local.get $graph
  local.get $stripped_args
  ;; project field: implementation
  call $hydra.lib.eithers.bimap
  local.get $eager2
  local.get $prim_result
  local.get $reduce
  local.get $eager2
  local.get $reduced_result
  local.get $remaining_args
  local.get $apply_if_nullary
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  local.set $for_primitive
  (block $end_term (result i32)
  (block $let
  (block $variable
  (block $function
  (block $application
  local.get $stripped
  br_table $application $function $variable $let $let
)
  local.get $eager2
  local.get $app
  ;; project field: function
  local.get $app
  ;; project field: argument
  local.get $args
  call $hydra.lib.lists.cons
  local.get $apply_if_nullary
  br $end_term
)
  (block $end_function (result i32)
  (block $lambda
  (block $elimination
  local.get $v
  br_table $elimination $lambda $lambda
)
  local.get $args
  call $hydra.lib.lists.null
  i32.const 1
  local.get $original
  local.get $elm
  local.get $args
  local.get $for_elimination
  call $hydra.lib.logic.if_else
  br $end_function
)
  local.get $args
  call $hydra.lib.lists.null
  i32.const 1
  local.get $original
  local.get $l
  local.get $args
  local.get $for_lambda
  call $hydra.lib.logic.if_else
  br $end_function
)
  br $end_term
)
  local.get $graph
  local.get $v
  call $hydra.lexical.lookup_binding
  local.set $m_binding
  local.get $graph
  local.get $v
  call $hydra.lexical.lookup_primitive
  local.set $m_prim
  i32.const 1
  local.get $original
  local.get $args
  local.get $apply_to_arguments
  local.get $prim
  call $hydra.arity.primitive_arity
  local.set $arity
  local.get $arity
  local.get $args
  call $hydra.lib.lists.length
  call $hydra.lib.equality.gt
  i32.const 1
  local.get $original
  local.get $args
  local.get $apply_to_arguments
  local.get $prim
  local.get $arity
  local.get $args
  local.get $for_primitive
  call $hydra.lib.logic.if_else
  local.get $m_prim
  call $hydra.lib.maybes.maybe
  local.get $eager2
  local.get $binding
  ;; project field: term
  local.get $args
  local.get $apply_if_nullary
  local.get $m_binding
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.get $lt
  ;; project field: bindings
  local.set $bindings
  local.get $lt
  ;; project field: body
  local.set $body
  i32.const 1
  ;; list elements follow
  local.get $b
  local.get $b
  ;; project field: name
  local.set $let_expr
  local.get $b
  ;; project field: name
  local.get $b
  ;; project field: name
  local.get $b
  local.get $let_expr
  local.get $b
  ;; project field: term
  call $hydra.variables.replace_free_term_variable
  local.get $b
  ;; project field: type
  local.set $expand_binding
  local.get $expand_binding
  local.get $bindings
  call $hydra.lib.lists.map
  local.set $expanded_bindings
  local.get $b
  ;; project field: name
  local.get $b
  ;; project field: term
  local.get $term2
  call $hydra.variables.replace_free_term_variable
  local.set $substitute_binding
  local.get $substitute_binding
  local.get $term2
  local.get $bs
  call $hydra.lib.lists.foldl
  local.set $substitute_all
  local.get $expanded_bindings
  local.get $body
  local.get $substitute_all
  local.set $expanded_body
  local.get $eager2
  local.get $expanded_body
  local.get $reduce
  local.get $eager2
  local.get $reduced_body
  local.get $args
  local.get $apply_if_nullary
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.set $apply_if_nullary
  local.get $eager
  local.get $mid
  local.get $do_recurse
  local.get $mid
  local.get $recurse
  i32.const 1
  local.get $mid
  call $hydra.lib.logic.if_else
  local.get $eager
  local.get $inner
  i32.const 0
  ;; list elements follow
  local.get $apply_if_nullary
  call $hydra.lib.eithers.bind
  local.set $mapping
  local.get $mapping
  local.get $term
  call $hydra.rewriting.rewrite_term_m
)
  (func $hydra.reduction.term_is_closed (param $term i32) (result i32)
  local.get $term
  call $hydra.variables.free_variables_in_term
  call $hydra.lib.sets.null
)
  (func $hydra.reduction.term_is_value (param $term i32) (result i32)
  (local $b i32)
  (local $check_field i32)
  (local $check_fields i32)
  (local $cs i32)
  (local $e i32)
  (local $els i32)
  (local $f i32)
  (local $fields i32)
  (local $for_list i32)
  (local $function_is_value i32)
  (local $i i32)
  (local $kv i32)
  (local $l i32)
  (local $m i32)
  (local $r i32)
  (local $s i32)
  (local $t i32)
  local.get $b
  local.get $t
  call $hydra.reduction.term_is_value
  call $hydra.lib.logic.and
  i32.const 1
  local.get $els
  call $hydra.lib.lists.foldl
  local.set $for_list
  local.get $f
  ;; project field: term
  call $hydra.reduction.term_is_value
  local.set $check_field
  local.get $b
  local.get $f
  local.get $check_field
  call $hydra.lib.logic.and
  i32.const 1
  local.get $fields
  call $hydra.lib.lists.foldl
  local.set $check_fields
  (block $end_function (result i32)
  (block $lambda
  (block $elimination
  local.get $f
  br_table $elimination $lambda $lambda
)
  (block $end_elimination (result i32)
  (block $union
  (block $record
  (block $wrap
  local.get $e
  br_table $wrap $record $union $union
)
  i32.const 1
  br $end_elimination
)
  i32.const 1
  br $end_elimination
)
  local.get $cs
  ;; project field: cases
  local.get $check_fields
  i32.const 1
  call $hydra.reduction.term_is_value
  local.get $cs
  ;; project field: default
  call $hydra.lib.maybes.maybe
  call $hydra.lib.logic.and
  br $end_elimination
)
  br $end_function
)
  local.get $l
  ;; project field: body
  call $hydra.reduction.term_is_value
  br $end_function
)
  local.set $function_is_value
  (block $end_term (result i32)
  (block $variable
  (block $unit
  (block $union
  (block $set
  (block $record
  (block $maybe
  (block $map
  (block $list
  (block $function
  (block $literal
  (block $either
  (block $application
  local.get $term
  call $hydra.strip.deannotate_term
  br_table $application $either $literal $function $list $map $maybe $record $set $union $unit $variable $variable
)
  i32.const 0
  br $end_term
)
  local.get $l
  call $hydra.reduction.term_is_value
  local.get $r
  call $hydra.reduction.term_is_value
  local.get $e
  call $hydra.lib.eithers.either
  br $end_term
)
  i32.const 1
  br $end_term
)
  local.get $f
  local.get $function_is_value
  br $end_term
)
  local.get $els
  local.get $for_list
  br $end_term
)
  local.get $b
  local.get $kv
  call $hydra.lib.pairs.first
  call $hydra.reduction.term_is_value
  local.get $kv
  call $hydra.lib.pairs.second
  call $hydra.reduction.term_is_value
  call $hydra.lib.logic.and
  call $hydra.lib.logic.and
  i32.const 1
  local.get $m
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.foldl
  br $end_term
)
  i32.const 1
  call $hydra.reduction.term_is_value
  local.get $m
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.get $r
  ;; project field: fields
  local.get $check_fields
  br $end_term
)
  local.get $s
  call $hydra.lib.sets.to_list
  local.get $for_list
  br $end_term
)
  local.get $i
  ;; project field: field
  local.get $check_field
  br $end_term
)
  i32.const 1
  br $end_term
)
  i32.const 0
  br $end_term
)
)
)
