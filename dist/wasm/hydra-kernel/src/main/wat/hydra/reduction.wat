(module
  (import "hydra.arity" "hydra.arity.primitive_arity" (func $hydra.arity.primitive_arity (param i32) (result i32) ) )
  (import "hydra.arity" "hydra.arity.type_arity" (func $hydra.arity.type_arity (param i32) (result i32) ) )
  (import "hydra.checking" "hydra.checking.type_of" (func $hydra.checking.type_of (param i32) (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.encode.core" "hydra.encode.core.type" (func $hydra.encode.core.type (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.injection" (func $hydra.extract.core.injection (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.record" (func $hydra.extract.core.record (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.wrap" (func $hydra.extract.core.wrap (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.lookup_binding" (func $hydra.lexical.lookup_binding (param i32) (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.lookup_primitive" (func $hydra.lexical.lookup_primitive (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bimap" (func $hydra.lib.eithers.bimap (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_maybe" (func $hydra.lib.eithers.map_maybe (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.gt" (func $hydra.lib.equality.gt (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.lte" (func $hydra.lib.equality.lte (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.drop" (func $hydra.lib.lists.drop (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.find" (func $hydra.lib.lists.find (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.reverse" (func $hydra.lib.lists.reverse (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.take" (func $hydra.lib.lists.take (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.uncons" (func $hydra.lib.lists.uncons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.zip" (func $hydra.lib.lists.zip (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int32" (func $hydra.lib.literals.show_int32 (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.and" (func $hydra.lib.logic.and (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.or" (func $hydra.lib.logic.or (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.elems" (func $hydra.lib.maps.elems (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.singleton" (func $hydra.lib.maps.singleton (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.range" (func $hydra.lib.math.range (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.sub" (func $hydra.lib.math.sub (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.bind" (func $hydra.lib.maybes.bind (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.null" (func $hydra.lib.sets.null (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.resolution" "hydra.resolution.require_type" (func $hydra.resolution.require_type (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_term" (func $hydra.rewriting.rewrite_term (param i32) (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_term_m" (func $hydra.rewriting.rewrite_term_m (param i32) (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_term_with_context_m" (func $hydra.rewriting.rewrite_term_with_context_m (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_type_m" (func $hydra.rewriting.rewrite_type_m (param i32) (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.extend_graph_for_lambda" (func $hydra.scoping.extend_graph_for_lambda (param i32) (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.extend_graph_for_let" (func $hydra.scoping.extend_graph_for_let (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.extend_graph_for_type_lambda" (func $hydra.scoping.extend_graph_for_type_lambda (param i32) (param i32) (result i32) ) )
  (import "hydra.show.errors" "hydra.show.errors.error" (func $hydra.show.errors.error (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_term" (func $hydra.strip.deannotate_term (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.free_variables_in_term" (func $hydra.variables.free_variables_in_term (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.is_free_variable_in_term" (func $hydra.variables.is_free_variable_in_term (param i32) (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.replace_free_term_variable" (func $hydra.variables.replace_free_term_variable (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.replace_free_type_variable" (func $hydra.variables.replace_free_type_variable (param i32) (param i32) (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\04\00\00\00\74\79\70\65\01\00\00\00\76")
  (global $__bump_ptr (mut i32) i32.const 1040 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
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
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  local.get $vold
  i32.const 19
  local.get $vnew
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.get $term
  call $hydra.variables.replace_free_term_variable
)
  (func $hydra.reduction.beta_reduce_type (param $cx i32) (param $graph i32) (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
  (local $v i32)
  local.get $app
  i32.load
  local.set $lhs
  local.get $app
  i32.load offset=4
  local.set $rhs
  (block $end_type (result i32)
  (block $variable
  (block $forall
  (block $annotated
  local.get $lhs
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $forall $variable $variable
)
  local.get $v
  drop
  local.get $at
  i32.load
  local.get $rhs
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $reduce_app
  drop
  i32.const 0
  i32.const 1
  i32.const 0
  local.get $a
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  br $end_type
)
  local.get $v
  drop
  local.get $cx
  local.get $graph
  local.get $ft
  i32.load
  local.get $rhs
  local.get $ft
  i32.load offset=4
  call $hydra.variables.replace_free_type_variable
  call $hydra.reduction.beta_reduce_type
  br $end_type
)
  local.get $v
  drop
  local.get $cx
  local.get $graph
  local.get $name
  call $hydra.resolution.require_type
  local.get $cx
  local.get $graph
  i32.const 1
  local.get $t'
  local.get $rhs
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  call $hydra.reduction.beta_reduce_type
  call $hydra.lib.eithers.bind
  br $end_type
)
  local.set $reduce_app
  (block $end_type (result i32)
  (block $application
  local.get $r
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $application $application
)
  local.get $v
  drop
  local.get $a
  drop
  local.get $reduce_app
  drop
  i32.const 0
  br $end_type
)
  local.set $find_app
  local.get $t
  drop
  local.get $recurse
  drop
  i32.const 0
  local.get $r
  drop
  local.get $find_app
  drop
  i32.const 0
  call $hydra.lib.eithers.bind
  local.set $map_expr
  local.get $map_expr
  local.get $typ
  call $hydra.rewriting.rewrite_type_m
)
  (func $hydra.reduction.contract_term (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $app i32)
  (local $body i32)
  (local $l i32)
  (local $lhs i32)
  (local $rec i32)
  (local $recurse i32)
  (local $rewrite i32)
  (local $rhs i32)
  (local $t i32)
  (local $v i32)
  local.get $t
  drop
  local.get $recurse
  drop
  i32.const 0
  local.set $rec
  (block $end_term (result i32)
  (block $application
  local.get $rec
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $application $application
)
  local.get $v
  drop
  local.get $app
  i32.load
  local.set $lhs
  local.get $app
  i32.load offset=4
  local.set $rhs
  (block $end_term (result i32)
  (block $lambda
  local.get $lhs
  call $hydra.strip.deannotate_term
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
  local.get $l
  i32.load
  local.set $v
  local.get $l
  i32.load offset=8
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
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
  (local $ct i32)
  (local $dom i32)
  (local $domain_types i32)
  (local $domains i32)
  (local $e i32)
  (local $el i32)
  (local $elim_head_type i32)
  (local $elim_term i32)
  (local $els i32)
  (local $expand i32)
  (local $f i32)
  (local $for_case_branch i32)
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
  (local $new_cs i32)
  (local $nm i32)
  (local $num_args i32)
  (local $p i32)
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
  i32.load
  local.get $_gpt_p
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
  local.get $tx0
  i32.load offset=20
  call $hydra.lib.maps.elems
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $prim_types
  (block $end_term (result i32)
  (block $variable
  (block $type_application
  (block $type_lambda
  (block $let
  (block $unwrap
  (block $project
  (block $lambda
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
  br_table $annotated $application $cases $lambda $project $unwrap $let $type_lambda $type_application $variable $variable
)
  local.get $v
  drop
  local.get $tx
  drop
  local.get $at
  i32.load
  drop
  local.get $term_arity_with_context
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $tx
  drop
  local.get $app
  i32.load
  drop
  local.get $term_arity_with_context
  drop
  i32.const 0
  i32.const 1
  call $hydra.lib.math.sub
  br $end_term
)
  local.get $v
  drop
  i32.const 1
  br $end_term
)
  local.get $v
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 1
  br $end_term
)
  local.get $v
  drop
  i32.const 1
  br $end_term
)
  local.get $v
  drop
  i32.const 0
  local.get $tx
  local.get $l
  call $hydra.scoping.extend_graph_for_let
  drop
  local.get $l
  i32.load offset=4
  drop
  local.get $term_arity_with_context
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $tx
  local.get $tl
  call $hydra.scoping.extend_graph_for_type_lambda
  drop
  local.get $tl
  i32.load offset=4
  drop
  local.get $term_arity_with_context
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $tx
  drop
  local.get $tat
  i32.load
  drop
  local.get $term_arity_with_context
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 0
  i32.const 0
  local.get $name
  local.get $prim_types
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  i32.const 0
  i32.const 0
  local.get $name
  local.get $tx
  i32.load offset=4
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.map
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.set $term_arity_with_context
  local.get $n
  i32.const 0
  call $hydra.lib.equality.lte
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
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
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $function $annotated $application $forall $forall
)
  local.get $v
  drop
  local.get $ftyp
  i32.load
  local.get $n
  i32.const 1
  call $hydra.lib.math.sub
  drop
  local.get $ftyp
  i32.load offset=4
  drop
  local.get $domain_types
  drop
  i32.const 0
  call $hydra.lib.lists.cons
  br $end_type
)
  local.get $v
  drop
  local.get $n
  drop
  local.get $at
  i32.load
  drop
  local.get $domain_types
  drop
  i32.const 0
  br $end_type
)
  local.get $v
  drop
  local.get $n
  drop
  local.get $atyp
  i32.load
  drop
  local.get $domain_types
  drop
  i32.const 0
  br $end_type
)
  local.get $v
  drop
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
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $function $annotated $application $forall $forall
)
  local.get $v
  drop
  local.get $ftyp
  i32.load offset=4
  drop
  local.get $n
  i32.const 1
  call $hydra.lib.math.sub
  drop
  local.get $peel_function_domains
  drop
  i32.const 0
  br $end_type
)
  local.get $v
  drop
  local.get $at
  i32.load
  drop
  local.get $n
  drop
  local.get $peel_function_domains
  drop
  i32.const 0
  br $end_type
)
  local.get $v
  drop
  local.get $atyp
  i32.load
  drop
  local.get $n
  drop
  local.get $peel_function_domains
  drop
  i32.const 0
  br $end_type
)
  local.get $v
  drop
  i32.const 0
  br $end_type
)
  local.get $mtyp
  call $hydra.lib.maybes.maybe
  call $hydra.lib.logic.if_else
  local.set $peel_function_domains
  i32.const 1
  local.get $lhs
  local.get $arg
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  drop
  local.get $num_args
  drop
  local.get $peel_function_domains
  drop
  i32.const 0
  local.set $remaining_type
  local.get $needed
  drop
  local.get $remaining_type
  drop
  local.get $domain_types
  drop
  i32.const 0
  local.set $domains
  local.get $remaining_type
  drop
  local.get $needed
  drop
  local.get $peel_function_domains
  drop
  i32.const 0
  local.set $codomain_type
  i32.const 1032
  local.get $i
  call $hydra.lib.literals.show_int32
  call $hydra.lib.strings.cat2
  local.set $vn
  i32.const 1
  local.get $body
  i32.const 19
  local.get $vn
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $applied
  local.get $indices
  call $hydra.lib.lists.foldl
  local.set $fully_applied_raw
  local.get $fully_applied_raw
  i32.const 0
  local.get $fully_applied_raw
  i32.const 1024
  local.get $ct
  call $hydra.encode.core.type
  call $hydra.lib.maps.singleton
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  i32.const 1032
  local.get $i
  call $hydra.lib.literals.show_int32
  call $hydra.lib.strings.cat2
  local.set $vn
  i32.const 5
  local.get $vn
  local.get $dom
  local.get $body
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
  local.get $fully_applied
  local.get $indexed_domains
  call $hydra.lib.lists.reverse
  call $hydra.lib.lists.foldl
  local.get $applied
  call $hydra.lib.logic.if_else
  local.set $expand
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  drop
  local.get $tx1
  drop
  local.get $term1
  drop
  local.get $rewrite_with_args
  drop
  i32.const 0
  local.set $recurse
  (block $end_term (result i32)
  (block $variable
  (block $type_application
  (block $type_lambda
  (block $let
  (block $unwrap
  (block $project
  (block $cases
  (block $lambda
  (block $annotated
  local.get $trm2
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $lambda $cases $project $unwrap $let $type_lambda $type_application $variable $variable
)
  local.get $v
  drop
  local.get $tx2
  drop
  local.get $at2
  i32.load
  drop
  local.get $term_head_type
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 0
  local.get $tx2
  local.get $l2
  call $hydra.scoping.extend_graph_for_let
  drop
  local.get $l2
  i32.load offset=4
  drop
  local.get $term_head_type
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $tx2
  local.get $tl2
  call $hydra.scoping.extend_graph_for_type_lambda
  drop
  local.get $tl2
  i32.load offset=4
  drop
  local.get $term_head_type
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $tx2
  drop
  local.get $tat2
  i32.load
  drop
  local.get $term_head_type
  drop
  i32.const 0
  (block $end_type (result i32)
  (block $forall
  local.get $htyp2
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
  local.get $ft2
  i32.load
  local.get $tat2
  i32.load offset=4
  local.get $ft2
  i32.load offset=4
  call $hydra.variables.replace_free_type_variable
  br $end_type
)
  call $hydra.lib.maybes.bind
  br $end_term
)
  local.get $v
  drop
  i32.const 0
  local.get $vn2
  local.get $tx2
  i32.load offset=4
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.map
  br $end_term
)
  local.set $term_head_type
  local.get $tx
  drop
  local.get $trm
  drop
  local.get $term_arity_with_context
  drop
  i32.const 0
  local.set $arity
  local.get $tx
  drop
  local.get $trm
  drop
  local.get $term_head_type
  drop
  i32.const 0
  local.set $h_type
  i32.const 0
  drop
  local.get $args
  drop
  local.get $arity
  drop
  local.get $h_type
  drop
  local.get $trm
  drop
  local.get $expand
  drop
  i32.const 0
  local.set $after_recursion
  local.get $f
  i32.load
  local.get $tx
  drop
  local.get $f
  i32.load offset=4
  drop
  local.get $recurse
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
  local.get $tx
  drop
  local.get $f
  i32.load offset=4
  drop
  local.get $recurse
  drop
  i32.const 0
  local.set $branch_body
  local.get $tx
  drop
  local.get $branch_body
  drop
  local.get $term_arity_with_context
  drop
  i32.const 0
  local.set $arty
  local.get $tx
  drop
  local.get $branch_body
  drop
  local.get $term_head_type
  drop
  i32.const 0
  local.set $branch_h_type
  local.get $f
  i32.load
  i32.const 1
  drop
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  drop
  local.get $arty
  drop
  local.get $branch_h_type
  drop
  local.get $branch_body
  drop
  local.get $expand
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
  local.set $for_case_branch
  local.get $tx
  drop
  local.get $pr
  call $hydra.lib.pairs.first
  drop
  local.get $recurse
  drop
  i32.const 0
  local.get $tx
  drop
  local.get $pr
  call $hydra.lib.pairs.second
  drop
  local.get $recurse
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
  local.get $mp
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $for_map
  (block $end_term (result i32)
  (block $wrap
  (block $variable
  (block $unit
  (block $inject
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
  (block $lambda
  (block $unwrap
  (block $project
  (block $cases
  (block $either
  (block $application
  (block $annotated
  local.get $term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $application $either $cases $project $unwrap $lambda $let $list $literal $map $maybe $pair $record $set $type_application $type_lambda $inject $unit $variable $wrap $wrap
)
  local.get $v
  drop
  i32.const 0
  local.get $tx
  drop
  local.get $at
  i32.load
  drop
  local.get $recurse
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
  drop
  local.get $after_recursion
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  drop
  local.get $tx
  drop
  local.get $app
  i32.load offset=4
  drop
  local.get $rewrite_with_args
  drop
  i32.const 0
  local.set $rhs
  local.get $rhs
  local.get $args
  call $hydra.lib.lists.cons
  drop
  local.get $tx
  drop
  local.get $app
  i32.load
  drop
  local.get $rewrite_with_args
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 3
  i32.const 0
  local.get $tx
  drop
  local.get $l
  drop
  local.get $recurse
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
  local.get $tx
  drop
  local.get $r
  drop
  local.get $recurse
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
  drop
  local.get $after_recursion
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $cs
  i32.load
  local.get $tx
  drop
  local.get $t1
  drop
  local.get $recurse
  drop
  i32.const 0
  local.get $cs
  i32.load offset=4
  call $hydra.lib.maybes.map
  local.get $for_case_branch
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
  local.set $new_cs
  i32.const 2
  local.get $new_cs
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.set $elim_term
  i32.const 4
  i32.const 14
  local.get $cs
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
  local.set $elim_head_type
  i32.const 1
  drop
  local.get $args
  drop
  i32.const 1
  drop
  local.get $elim_head_type
  drop
  local.get $elim_term
  drop
  local.get $expand
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 0
  drop
  local.get $args
  drop
  i32.const 1
  drop
  i32.const 0
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
  drop
  local.get $expand
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 0
  drop
  local.get $args
  drop
  i32.const 1
  drop
  i32.const 0
  drop
  i32.const 18
  local.get $nm
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $expand
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $tx
  local.get $lm
  call $hydra.scoping.extend_graph_for_lambda
  local.set $tx1
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  drop
  local.get $tx1
  drop
  local.get $lm
  i32.load offset=8
  drop
  local.get $rewrite_with_args
  drop
  i32.const 0
  local.set $body
  i32.const 5
  local.get $lm
  i32.load
  local.get $lm
  i32.load offset=4
  local.get $body
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
  local.set $result
  local.get $tx
  drop
  local.get $result
  drop
  local.get $term_arity_with_context
  drop
  i32.const 0
  local.set $arty
  i32.const 0
  drop
  local.get $args
  drop
  local.get $arty
  drop
  i32.const 0
  drop
  local.get $result
  drop
  local.get $expand
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 0
  local.get $tx
  local.get $lt
  call $hydra.scoping.extend_graph_for_let
  local.set $tx1
  local.get $b
  i32.load
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  drop
  local.get $tx1
  drop
  local.get $b
  i32.load offset=4
  drop
  local.get $rewrite_with_args
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
  i32.const 6
  local.get $map_binding
  local.get $lt
  i32.load
  call $hydra.lib.lists.map
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  drop
  local.get $tx1
  drop
  local.get $lt
  i32.load offset=4
  drop
  local.get $rewrite_with_args
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
  local.set $result
  local.get $result
  drop
  local.get $after_recursion
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 7
  local.get $tx
  drop
  local.get $el
  drop
  local.get $recurse
  drop
  i32.const 0
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
  drop
  local.get $after_recursion
  drop
  i32.const 0
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
  local.get $mp
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
  drop
  local.get $after_recursion
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 10
  local.get $tx
  drop
  local.get $v
  drop
  local.get $recurse
  drop
  i32.const 0
  local.get $mb
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
  drop
  local.get $after_recursion
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 11
  local.get $tx
  drop
  local.get $pr
  call $hydra.lib.pairs.first
  drop
  local.get $recurse
  drop
  i32.const 0
  local.get $tx
  drop
  local.get $pr
  call $hydra.lib.pairs.second
  drop
  local.get $recurse
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
  drop
  local.get $after_recursion
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 13
  local.get $rc
  i32.load
  local.get $for_field
  local.get $rc
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
  drop
  local.get $after_recursion
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 14
  local.get $tx
  drop
  local.get $el
  drop
  local.get $recurse
  drop
  i32.const 0
  local.get $st
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
  drop
  local.get $after_recursion
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 15
  local.get $tx
  drop
  local.get $tt
  i32.load
  drop
  local.get $recurse
  drop
  i32.const 0
  local.get $tt
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
  local.get $after_recursion
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $tx
  local.get $tl
  call $hydra.scoping.extend_graph_for_type_lambda
  local.set $tx1
  i32.const 16
  local.get $tl
  i32.load
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  drop
  local.get $tx1
  drop
  local.get $tl
  i32.load offset=4
  drop
  local.get $rewrite_with_args
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
  local.set $result
  local.get $result
  drop
  local.get $after_recursion
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 4
  local.get $inj
  i32.load
  local.get $inj
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
  drop
  local.get $after_recursion
  drop
  i32.const 0
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
  local.get $tx
  drop
  local.get $term
  drop
  local.get $term_arity_with_context
  drop
  i32.const 0
  local.set $arty
  i32.const 0
  local.get $vn
  local.get $tx
  i32.load offset=4
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.map
  local.set $var_type
  i32.const 0
  drop
  local.get $args
  drop
  local.get $arty
  drop
  local.get $var_type
  drop
  local.get $term
  drop
  local.get $expand
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 20
  local.get $wt
  i32.load
  local.get $tx
  drop
  local.get $wt
  i32.load offset=4
  drop
  local.get $recurse
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
  drop
  local.get $after_recursion
  drop
  i32.const 0
  br $end_term
)
  local.set $rewrite_with_args
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  drop
  local.get $tx0
  drop
  local.get $term0
  drop
  local.get $rewrite_with_args
  drop
  i32.const 0
  call $hydra.reduction.contract_term
)
  (func $hydra.reduction.eta_expand_typed_term (param $cx i32) (param $tx0 i32) (param $term0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $_tc i32)
  (local $a i32)
  (local $a2 i32)
  (local $ann i32)
  (local $arity i32)
  (local $arity_of i32)
  (local $at i32)
  (local $base i32)
  (local $body i32)
  (local $cs i32)
  (local $cs_cases i32)
  (local $dflt i32)
  (local $e i32)
  (local $elim_term i32)
  (local $extra_variables i32)
  (local $f i32)
  (local $for_case i32)
  (local $for_case_statement i32)
  (local $for_cases i32)
  (local $for_nullary_elim i32)
  (local $force_expansion i32)
  (local $forced i32)
  (local $i i32)
  (local $l i32)
  (local $lhs i32)
  (local $lhs2 i32)
  (local $lhsarity i32)
  (local $n i32)
  (local $name i32)
  (local $p i32)
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
  (local $txl i32)
  (local $txlt i32)
  (local $txt i32)
  (local $typ i32)
  (local $typ_cx i32)
  (local $type_args i32)
  (local $uc i32)
  (local $unwind i32)
  (local $v i32)
  (local $v0 i32)
  (local $vars i32)
  (local $vrest i32)
  (block $end_term (result i32)
  (block $type_application
  (block $application
  (block $annotated
  local.get $term2
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $application $type_application $type_application
)
  local.get $v
  drop
  local.get $at
  i32.load
  drop
  local.get $rewrite_spine
  drop
  i32.const 0
  local.get $at
  i32.load offset=4
  local.set $ann
  i32.const 1
  i32.const 0
  local.get $body
  local.get $ann
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  i32.const 0
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
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  call $hydra.lib.logic.if_else
  local.set $l
  local.get $a
  i32.load
  drop
  local.get $rewrite_spine
  drop
  i32.const 0
  i32.const 1
  drop
  i32.const 0
  drop
  local.get $l
  drop
  local.get $recurse
  drop
  local.get $tx
  drop
  local.get $a
  i32.load offset=4
  drop
  local.get $rewrite
  drop
  i32.const 0
  i32.const 1
  i32.const 1
  local.get $lhs
  local.get $rhs
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $v
  drop
  local.get $tat
  i32.load
  drop
  local.get $rewrite_spine
  drop
  i32.const 0
  local.get $tat
  i32.load offset=4
  local.set $typ
  i32.const 1
  i32.const 15
  local.get $body
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
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.set $rewrite_spine
  local.get $_tc
  call $hydra.lib.pairs.first
  call $hydra.arity.type_arity
  local.get $cx
  local.get $tx2
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $term2
  call $hydra.checking.type_of
  call $hydra.lib.eithers.map
  local.set $dflt
  (block $end_term (result i32)
  (block $variable
  (block $type_lambda
  (block $type_application
  (block $let
  (block $lambda
  (block $unwrap
  (block $project
  (block $cases
  (block $annotated
  local.get $term2
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $cases $project $unwrap $lambda $let $type_application $type_lambda $variable $variable
)
  local.get $v
  drop
  local.get $tx2
  drop
  local.get $at
  i32.load
  drop
  local.get $arity_of
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
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
  br $end_term
)
  local.get $v
  drop
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
  br $end_term
)
  local.get $v
  drop
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
  br $end_term
)
  local.get $v
  drop
  local.get $tx2
  local.get $l
  call $hydra.scoping.extend_graph_for_lambda
  local.set $txl
  local.get $txl
  drop
  local.get $l
  i32.load offset=8
  drop
  local.get $arity_of
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 0
  local.get $tx2
  local.get $l
  call $hydra.scoping.extend_graph_for_let
  local.set $txl
  local.get $txl
  drop
  local.get $l
  i32.load offset=4
  drop
  local.get $arity_of
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $tx2
  drop
  local.get $tat
  i32.load
  drop
  local.get $arity_of
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $tx2
  local.get $tl
  call $hydra.scoping.extend_graph_for_type_lambda
  local.set $txt
  local.get $txt
  drop
  local.get $tl
  i32.load offset=4
  drop
  local.get $arity_of
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $_tc
  call $hydra.lib.pairs.first
  call $hydra.arity.type_arity
  local.get $cx
  local.get $tx2
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  i32.const 19
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
  call $hydra.checking.type_of
  call $hydra.lib.eithers.map
  i32.const 1
  local.get $t
  call $hydra.arity.type_arity
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $name
  local.get $tx2
  i32.load offset=4
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.map
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.set $arity_of
  i32.const 1032
  local.get $i
  call $hydra.lib.literals.show_int32
  call $hydra.lib.strings.cat2
  i32.const 1
  local.get $n
  call $hydra.lib.math.range
  call $hydra.lib.lists.map
  local.set $extra_variables
  local.get $body
  local.get $uc
  call $hydra.lib.pairs.first
  local.set $v0
  local.get $uc
  call $hydra.lib.pairs.second
  local.set $vrest
  i32.const 5
  local.get $v0
  i32.const 0
  local.get $vrest
  drop
  i32.const 1
  local.get $body
  i32.const 19
  local.get $v0
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $pad
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
  local.get $vars
  call $hydra.lib.lists.uncons
  call $hydra.lib.maybes.maybe
  local.set $pad
  local.get $n
  drop
  local.get $extra_variables
  drop
  i32.const 0
  drop
  local.get $body
  drop
  local.get $pad
  drop
  i32.const 0
  local.set $padn
  i32.const 15
  local.get $e
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.get $term2
  local.get $type_args
  call $hydra.lib.lists.foldl
  local.set $unwind
  local.get $cx
  local.get $tx
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $t
  call $hydra.checking.type_of
  local.get $typ_cx
  call $hydra.lib.pairs.first
  call $hydra.arity.type_arity
  local.set $arity
  i32.const 1
  local.get $arity
  drop
  local.get $t
  drop
  local.get $unwind
  drop
  i32.const 0
  drop
  local.get $padn
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
  call $hydra.lib.eithers.bind
  local.set $force_expansion
  local.get $forced
  local.get $term2
  drop
  local.get $force_expansion
  drop
  i32.const 0
  local.get $tx
  drop
  local.get $term2
  drop
  local.get $unwind
  drop
  i32.const 0
  drop
  local.get $recurse
  drop
  i32.const 0
  call $hydra.lib.logic.if_else
  local.set $recurse_or_force
  i32.const 0
  drop
  i32.const 1
  drop
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  drop
  local.get $recurse
  drop
  local.get $tx
  drop
  local.get $f
  i32.load offset=4
  drop
  local.get $rewrite
  drop
  i32.const 0
  i32.const 1
  local.get $f
  i32.load
  local.get $r
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.set $for_case
  local.get $cs
  i32.load
  local.set $tname
  local.get $cs
  i32.load offset=4
  local.set $dflt
  local.get $cs
  i32.load offset=8
  local.set $cs_cases
  i32.const 0
  drop
  i32.const 0
  drop
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  drop
  local.get $recurse
  drop
  local.get $tx
  drop
  local.get $rewrite
  drop
  i32.const 0
  local.get $dflt
  call $hydra.lib.eithers.map_maybe
  local.get $for_case
  local.get $cs_cases
  call $hydra.lib.eithers.map_list
  i32.const 1
  i32.const 2
  local.get $tname
  local.get $rdflt
  local.get $rcases
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
  call $hydra.lib.eithers.bind
  local.set $for_case_statement
  local.get $unwind
  local.get $cs
  drop
  local.get $for_case_statement
  drop
  i32.const 0
  call $hydra.lib.eithers.map
  i32.const 1
  local.get $top_level
  local.get $forced
  call $hydra.lib.logic.or
  i32.const 1
  drop
  local.get $base
  drop
  local.get $padn
  drop
  i32.const 0
  local.get $base
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
  call $hydra.lib.eithers.bind
  local.set $for_cases
  local.get $elim_term
  drop
  local.get $unwind
  drop
  i32.const 0
  local.set $base
  local.get $top_level
  local.get $forced
  call $hydra.lib.logic.or
  i32.const 1
  drop
  local.get $base
  drop
  local.get $padn
  drop
  i32.const 0
  local.get $base
  call $hydra.lib.logic.if_else
  local.set $for_nullary_elim
  (block $end_term (result i32)
  (block $type_lambda
  (block $type_application
  (block $let
  (block $lambda
  (block $unwrap
  (block $project
  (block $cases
  (block $application
  local.get $term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $application $cases $project $unwrap $lambda $let $type_application $type_lambda $type_lambda
)
  local.get $v
  drop
  local.get $a
  i32.load
  local.set $lhs
  local.get $a
  i32.load offset=4
  local.set $rhs
  i32.const 1
  drop
  i32.const 0
  drop
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  drop
  local.get $recurse
  drop
  local.get $tx
  drop
  local.get $rhs
  drop
  local.get $rewrite
  drop
  i32.const 0
  local.get $tx
  drop
  local.get $lhs
  drop
  local.get $arity_of
  drop
  i32.const 0
  local.get $lhs
  drop
  local.get $rewrite_spine
  drop
  i32.const 0
  i32.const 1
  local.get $lhs2
  local.get $rhs2
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.set $a2
  i32.const 1
  local.get $lhsarity
  i32.const 1
  call $hydra.lib.equality.gt
  local.get $lhsarity
  i32.const 1
  call $hydra.lib.math.sub
  drop
  local.get $a2
  drop
  local.get $padn
  drop
  i32.const 0
  local.get $a2
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $v
  drop
  local.get $cs
  drop
  local.get $for_cases
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 1
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
  drop
  local.get $for_nullary_elim
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
  i32.const 1
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
  drop
  local.get $for_nullary_elim
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
  local.get $tx
  local.get $l
  call $hydra.scoping.extend_graph_for_lambda
  local.set $txl
  local.get $unwind
  local.get $txl
  drop
  local.get $term
  drop
  local.get $recurse
  drop
  i32.const 0
  call $hydra.lib.eithers.map
  br $end_term
)
  local.get $v
  drop
  i32.const 0
  local.get $tx
  local.get $l
  call $hydra.scoping.extend_graph_for_let
  local.set $txlt
  local.get $txlt
  drop
  local.get $term
  drop
  local.get $recurse
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $top_level
  drop
  local.get $forced
  drop
  local.get $tat
  i32.load offset=4
  local.get $type_args
  call $hydra.lib.lists.cons
  drop
  local.get $recurse
  drop
  local.get $tx
  drop
  local.get $tat
  i32.load
  drop
  local.get $rewrite
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $tx
  local.get $tl
  call $hydra.scoping.extend_graph_for_type_lambda
  local.set $txt
  local.get $txt
  drop
  local.get $term
  drop
  local.get $recurse
  drop
  i32.const 0
  br $end_term
)
  local.set $rewrite
  i32.const 1
  drop
  i32.const 0
  drop
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  drop
  local.get $rewrite
  drop
  i32.const 0
  local.get $tx0
  local.get $term0
  call $hydra.rewriting.rewrite_term_with_context_m
)
  (func $hydra.reduction.eta_expansion_arity (param $graph i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $app i32)
  (local $at i32)
  (local $b i32)
  (local $name i32)
  (local $ta i32)
  (local $ts i32)
  (local $tt i32)
  (local $v i32)
  (block $end_term (result i32)
  (block $variable
  (block $type_application
  (block $type_lambda
  (block $unwrap
  (block $project
  (block $lambda
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
  br_table $annotated $application $cases $lambda $project $unwrap $type_lambda $type_application $variable $variable
)
  local.get $v
  drop
  local.get $graph
  local.get $at
  i32.load
  call $hydra.reduction.eta_expansion_arity
  br $end_term
)
  local.get $v
  drop
  local.get $graph
  local.get $app
  i32.load
  call $hydra.reduction.eta_expansion_arity
  i32.const 1
  call $hydra.lib.math.sub
  br $end_term
)
  local.get $v
  drop
  i32.const 1
  br $end_term
)
  local.get $v
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 1
  br $end_term
)
  local.get $v
  drop
  i32.const 1
  br $end_term
)
  local.get $v
  drop
  local.get $graph
  local.get $ta
  i32.load offset=4
  call $hydra.reduction.eta_expansion_arity
  br $end_term
)
  local.get $v
  drop
  local.get $graph
  local.get $tt
  i32.load
  call $hydra.reduction.eta_expansion_arity
  br $end_term
)
  local.get $v
  drop
  i32.const 0
  local.get $ts
  i32.load offset=4
  call $hydra.arity.type_arity
  local.get $graph
  local.get $name
  call $hydra.lexical.lookup_binding
  local.get $b
  i32.load offset=8
  call $hydra.lib.maybes.bind
  call $hydra.lib.maybes.maybe
  br $end_term
)
)
  (func $hydra.reduction.eta_reduce_term (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $app i32)
  (local $at i32)
  (local $body i32)
  (local $d i32)
  (local $l i32)
  (local $lhs i32)
  (local $no_change i32)
  (local $reduce_lambda i32)
  (local $rhs i32)
  (local $v i32)
  (local $v1 i32)
  local.get $term
  local.set $no_change
  local.get $l
  i32.load
  local.set $v
  local.get $l
  i32.load offset=4
  local.set $d
  local.get $l
  i32.load offset=8
  local.set $body
  (block $end_term (result i32)
  (block $application
  (block $annotated
  local.get $body
  call $hydra.reduction.eta_reduce_term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $application $application
)
  local.get $v
  drop
  local.get $v
  local.get $d
  local.get $at
  i32.load
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
  drop
  local.get $reduce_lambda
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $app
  i32.load
  local.set $lhs
  local.get $app
  i32.load offset=4
  local.set $rhs
  (block $end_term (result i32)
  (block $variable
  (block $annotated
  local.get $rhs
  call $hydra.reduction.eta_reduce_term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $variable $variable
)
  local.get $v
  drop
  local.get $v
  local.get $d
  i32.const 1
  local.get $lhs
  local.get $at
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  drop
  local.get $reduce_lambda
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $v
  drop
  i32.const 0
  drop
  i32.const 0
  local.get $v1
  drop
  i32.const 0
  drop
  i32.const 0
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
  (block $lambda
  (block $annotated
  local.get $term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $lambda $lambda
)
  local.get $v
  drop
  i32.const 0
  local.get $at
  i32.load
  call $hydra.reduction.eta_reduce_term
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
  local.get $l
  drop
  local.get $reduce_lambda
  drop
  i32.const 0
  br $end_term
)
)
  (func $hydra.reduction.reduce_term (param $cx i32) (param $graph i32) (param $eager i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $app i32)
  (local $apply_cases i32)
  (local $apply_if_nullary i32)
  (local $apply_projection i32)
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
  (local $e i32)
  (local $eager2 i32)
  (local $expand_binding i32)
  (local $expanded_bindings i32)
  (local $expanded_body i32)
  (local $f i32)
  (local $field i32)
  (local $fields i32)
  (local $for_cases i32)
  (local $for_lambda i32)
  (local $for_primitive i32)
  (local $for_projection i32)
  (local $for_unwrap i32)
  (local $fun i32)
  (local $inner i32)
  (local $is_non_lambda_term i32)
  (local $l i32)
  (local $let_expr i32)
  (local $lt i32)
  (local $m_binding i32)
  (local $m_prim i32)
  (local $map_error_to_string i32)
  (local $mapping i32)
  (local $matching i32)
  (local $mf i32)
  (local $mid i32)
  (local $n i32)
  (local $name i32)
  (local $original i32)
  (local $p i32)
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
  (local $uc i32)
  (local $v i32)
  (local $x i32)
  local.get $cx
  local.get $graph
  local.get $eager2
  i32.const 0
  call $hydra.reduction.reduce_term
  local.set $reduce
  (block $end_term (result i32)
  (block $let
  (block $lambda
  local.get $term2
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $lambda $let $let
)
  local.get $v
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  drop
  local.get $arg
  drop
  local.get $reduce
  drop
  i32.const 0
  call $hydra.lib.logic.if_else
  local.set $reduce_arg
  local.get $fun
  i32.const 1
  local.get $fun
  local.get $uc
  call $hydra.lib.pairs.first
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $uc
  call $hydra.lib.pairs.second
  drop
  local.get $apply_to_arguments
  drop
  i32.const 0
  local.get $args
  call $hydra.lib.lists.uncons
  call $hydra.lib.maybes.maybe
  local.set $apply_to_arguments
  i32.const 6
  local.get $e
  call $hydra.show.errors.error
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.set $map_error_to_string
  local.get $proj
  i32.load
  local.get $graph
  local.get $reduced_arg
  call $hydra.strip.deannotate_term
  call $hydra.extract.core.record
  local.get $f
  i32.load
  local.get $proj
  i32.load offset=4
  call $hydra.lib.equality.equal
  local.get $fields
  call $hydra.lib.lists.find
  local.set $matching
  i32.const 0
  i32.const 7
  i32.const 2
  local.get $proj
  i32.load offset=4
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
  local.get $mf
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
  local.get $matching
  call $hydra.lib.maybes.maybe
  call $hydra.lib.eithers.bind
  local.set $apply_projection
  local.get $cs
  i32.load
  local.get $graph
  local.get $reduced_arg
  call $hydra.extract.core.injection
  local.get $f
  i32.load
  local.get $field
  i32.load
  call $hydra.lib.equality.equal
  local.get $cs
  i32.load offset=8
  call $hydra.lib.lists.find
  local.set $matching
  i32.const 0
  i32.const 7
  i32.const 2
  local.get $field
  i32.load
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
  local.get $cs
  i32.load offset=4
  call $hydra.lib.maybes.maybe
  i32.const 1
  i32.const 1
  local.get $mf
  i32.load offset=4
  local.get $field
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
  local.get $matching
  call $hydra.lib.maybes.maybe
  call $hydra.lib.eithers.bind
  local.set $apply_cases
  local.get $original
  call $hydra.strip.deannotate_term
  local.set $stripped
  i32.const 1
  local.get $original
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.set $arg
  local.get $uc
  call $hydra.lib.pairs.second
  local.set $remaining_args
  local.get $eager2
  drop
  local.get $arg
  call $hydra.strip.deannotate_term
  drop
  local.get $reduce_arg
  drop
  i32.const 0
  local.get $proj
  drop
  local.get $reduced_arg
  drop
  local.get $apply_projection
  drop
  i32.const 0
  local.get $eager2
  drop
  local.get $reduce
  drop
  i32.const 0
  call $hydra.lib.eithers.bind
  local.get $eager2
  drop
  local.get $reduced_result
  drop
  local.get $remaining_args
  drop
  local.get $apply_if_nullary
  drop
  i32.const 0
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  local.get $args2
  call $hydra.lib.lists.uncons
  call $hydra.lib.maybes.maybe
  local.set $for_projection
  i32.const 1
  local.get $original
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.set $arg
  local.get $uc
  call $hydra.lib.pairs.second
  local.set $remaining_args
  local.get $eager2
  drop
  local.get $arg
  call $hydra.strip.deannotate_term
  drop
  local.get $reduce_arg
  drop
  i32.const 0
  local.get $cs
  drop
  local.get $reduced_arg
  drop
  local.get $apply_cases
  drop
  i32.const 0
  local.get $eager2
  drop
  local.get $reduce
  drop
  i32.const 0
  call $hydra.lib.eithers.bind
  local.get $eager2
  drop
  local.get $reduced_result
  drop
  local.get $remaining_args
  drop
  local.get $apply_if_nullary
  drop
  i32.const 0
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  local.get $args2
  call $hydra.lib.lists.uncons
  call $hydra.lib.maybes.maybe
  local.set $for_cases
  i32.const 1
  local.get $original
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.set $arg
  local.get $uc
  call $hydra.lib.pairs.second
  local.set $remaining_args
  local.get $eager2
  drop
  local.get $arg
  call $hydra.strip.deannotate_term
  drop
  local.get $reduce_arg
  drop
  i32.const 0
  local.get $name
  local.get $graph
  local.get $reduced_arg
  call $hydra.extract.core.wrap
  local.get $eager2
  drop
  local.get $reduce
  drop
  i32.const 0
  call $hydra.lib.eithers.bind
  local.get $eager2
  drop
  local.get $reduced_result
  drop
  local.get $remaining_args
  drop
  local.get $apply_if_nullary
  drop
  i32.const 0
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  local.get $args2
  call $hydra.lib.lists.uncons
  call $hydra.lib.maybes.maybe
  local.set $for_unwrap
  local.get $l
  i32.load
  local.set $param
  local.get $l
  i32.load offset=8
  local.set $body
  i32.const 1
  local.get $original
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.set $arg
  local.get $uc
  call $hydra.lib.pairs.second
  local.set $remaining_args
  local.get $eager2
  drop
  local.get $arg
  call $hydra.strip.deannotate_term
  drop
  local.get $reduce
  drop
  i32.const 0
  local.get $eager2
  drop
  local.get $param
  local.get $reduced_arg
  local.get $body
  call $hydra.variables.replace_free_term_variable
  drop
  local.get $reduce
  drop
  i32.const 0
  local.get $eager2
  drop
  local.get $reduced_result
  drop
  local.get $remaining_args
  drop
  local.get $apply_if_nullary
  drop
  i32.const 0
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  local.get $args2
  call $hydra.lib.lists.uncons
  call $hydra.lib.maybes.maybe
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
  drop
  local.get $reduce_arg
  drop
  i32.const 0
  local.get $arg_list
  call $hydra.lib.eithers.map_list
  i32.const 0
  local.get $reduced_args
  call $hydra.lib.lists.map
  local.set $stripped_args
  local.get $map_error_to_string
  local.get $x
  local.get $prim
  i32.load offset=8
  call $hydra.lib.eithers.bimap
  local.get $eager2
  drop
  local.get $prim_result
  drop
  local.get $reduce
  drop
  i32.const 0
  local.get $eager2
  drop
  local.get $reduced_result
  drop
  local.get $remaining_args
  drop
  local.get $apply_if_nullary
  drop
  i32.const 0
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  local.set $for_primitive
  (block $end_term (result i32)
  (block $let
  (block $variable
  (block $lambda
  (block $unwrap
  (block $project
  (block $cases
  (block $application
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $application $cases $project $unwrap $lambda $variable $let $let
)
  local.get $v
  drop
  local.get $eager2
  drop
  local.get $app
  i32.load
  drop
  local.get $app
  i32.load offset=4
  local.get $args
  call $hydra.lib.lists.cons
  drop
  local.get $apply_if_nullary
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $args
  call $hydra.lib.lists.null
  i32.const 1
  local.get $original
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.get $cs
  drop
  local.get $args
  drop
  local.get $for_cases
  drop
  i32.const 0
  call $hydra.lib.logic.if_else
  br $end_term
)
  local.get $v
  drop
  local.get $args
  call $hydra.lib.lists.null
  i32.const 1
  local.get $original
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.get $p
  drop
  local.get $args
  drop
  local.get $for_projection
  drop
  i32.const 0
  call $hydra.lib.logic.if_else
  br $end_term
)
  local.get $v
  drop
  local.get $args
  call $hydra.lib.lists.null
  i32.const 1
  local.get $original
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  drop
  local.get $args
  drop
  local.get $for_unwrap
  drop
  i32.const 0
  call $hydra.lib.logic.if_else
  br $end_term
)
  local.get $v
  drop
  local.get $args
  call $hydra.lib.lists.null
  i32.const 1
  local.get $original
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.get $l
  drop
  local.get $args
  drop
  local.get $for_lambda
  drop
  i32.const 0
  call $hydra.lib.logic.if_else
  br $end_term
)
  local.get $v
  drop
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
  drop
  local.get $args
  drop
  local.get $apply_to_arguments
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
  local.get $prim
  call $hydra.arity.primitive_arity
  local.set $arity
  local.get $arity
  local.get $args
  call $hydra.lib.lists.length
  call $hydra.lib.equality.gt
  i32.const 1
  local.get $original
  drop
  local.get $args
  drop
  local.get $apply_to_arguments
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
  local.get $prim
  drop
  local.get $arity
  drop
  local.get $args
  drop
  local.get $for_primitive
  drop
  i32.const 0
  call $hydra.lib.logic.if_else
  local.get $m_prim
  call $hydra.lib.maybes.maybe
  local.get $eager2
  drop
  local.get $binding
  i32.load offset=4
  drop
  local.get $args
  drop
  local.get $apply_if_nullary
  drop
  i32.const 0
  local.get $m_binding
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.get $v
  drop
  local.get $lt
  i32.load
  local.set $bindings
  local.get $lt
  i32.load offset=4
  local.set $body
  i32.const 6
  local.get $b
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
  i32.const 19
  local.get $b
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.set $let_expr
  local.get $b
  i32.load
  local.get $b
  i32.load
  local.get $b
  drop
  local.get $let_expr
  drop
  i32.const 0
  local.get $b
  i32.load offset=4
  call $hydra.variables.replace_free_term_variable
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
  local.set $expand_binding
  local.get $expand_binding
  local.get $bindings
  call $hydra.lib.lists.map
  local.set $expanded_bindings
  local.get $b
  i32.load
  local.get $b
  i32.load offset=4
  local.get $term2
  call $hydra.variables.replace_free_term_variable
  local.set $substitute_binding
  local.get $substitute_binding
  local.get $term2
  local.get $bs
  call $hydra.lib.lists.foldl
  local.set $substitute_all
  local.get $expanded_bindings
  drop
  local.get $body
  drop
  local.get $substitute_all
  drop
  i32.const 0
  local.set $expanded_body
  local.get $eager2
  drop
  local.get $expanded_body
  drop
  local.get $reduce
  drop
  i32.const 0
  local.get $eager2
  drop
  local.get $reduced_body
  drop
  local.get $args
  drop
  local.get $apply_if_nullary
  drop
  i32.const 0
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.set $apply_if_nullary
  local.get $eager
  drop
  local.get $mid
  drop
  local.get $do_recurse
  drop
  i32.const 0
  local.get $mid
  drop
  local.get $recurse
  drop
  i32.const 0
  i32.const 1
  local.get $mid
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $eager
  drop
  local.get $inner
  drop
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  drop
  local.get $apply_if_nullary
  drop
  i32.const 0
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
  (local $__rec_ptr i32)
  (local $b i32)
  (local $check_field i32)
  (local $check_fields i32)
  (local $cs i32)
  (local $e i32)
  (local $els i32)
  (local $f i32)
  (local $fields i32)
  (local $for_list i32)
  (local $i i32)
  (local $kv i32)
  (local $l i32)
  (local $m i32)
  (local $r i32)
  (local $s i32)
  (local $t i32)
  (local $v i32)
  local.get $b
  local.get $t
  call $hydra.reduction.term_is_value
  call $hydra.lib.logic.and
  i32.const 1
  local.get $els
  call $hydra.lib.lists.foldl
  local.set $for_list
  local.get $f
  i32.load offset=4
  call $hydra.reduction.term_is_value
  local.set $check_field
  local.get $b
  local.get $f
  drop
  local.get $check_field
  drop
  i32.const 0
  call $hydra.lib.logic.and
  i32.const 1
  local.get $fields
  call $hydra.lib.lists.foldl
  local.set $check_fields
  (block $end_term (result i32)
  (block $variable
  (block $unit
  (block $inject
  (block $set
  (block $record
  (block $maybe
  (block $map
  (block $list
  (block $unwrap
  (block $project
  (block $literal
  (block $lambda
  (block $either
  (block $cases
  (block $application
  local.get $term
  call $hydra.strip.deannotate_term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $application $cases $either $lambda $literal $project $unwrap $list $map $maybe $record $set $inject $unit $variable $variable
)
  local.get $v
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $cs
  i32.load offset=8
  drop
  local.get $check_fields
  drop
  i32.const 0
  i32.const 1
  i32.const 0
  local.get $cs
  i32.load offset=4
  call $hydra.lib.maybes.maybe
  call $hydra.lib.logic.and
  br $end_term
)
  local.get $v
  drop
  local.get $l
  call $hydra.reduction.term_is_value
  local.get $r
  call $hydra.reduction.term_is_value
  local.get $e
  call $hydra.lib.eithers.either
  br $end_term
)
  local.get $v
  drop
  local.get $l
  i32.load offset=8
  call $hydra.reduction.term_is_value
  br $end_term
)
  local.get $v
  drop
  i32.const 1
  br $end_term
)
  local.get $v
  drop
  i32.const 1
  br $end_term
)
  local.get $v
  drop
  i32.const 1
  br $end_term
)
  local.get $v
  drop
  local.get $els
  drop
  local.get $for_list
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
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
  local.get $v
  drop
  i32.const 1
  i32.const 0
  local.get $m
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.get $v
  drop
  local.get $r
  i32.load offset=4
  drop
  local.get $check_fields
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $s
  call $hydra.lib.sets.to_list
  drop
  local.get $for_list
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $i
  i32.load offset=4
  drop
  local.get $check_field
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  i32.const 1
  br $end_term
)
  local.get $v
  drop
  i32.const 0
  br $end_term
)
)
)
