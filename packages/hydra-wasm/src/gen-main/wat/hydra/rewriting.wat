(module
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_maybe" (func $hydra.lib.eithers.map_maybe (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat" (func $hydra.lib.lists.concat (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.reverse" (func $hydra.lib.lists.reverse (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.zip" (func $hydra.lib.lists.zip (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.add" (func $hydra.lib.math.add (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.pure" (func $hydra.lib.maybes.pure (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.extend_graph_for_lambda" (func $hydra.scoping.extend_graph_for_lambda (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.extend_graph_for_let" (func $hydra.scoping.extend_graph_for_let (param i32) (result i32) ) )
  (import "hydra.scoping" "hydra.scoping.extend_graph_for_type_lambda" (func $hydra.scoping.extend_graph_for_type_lambda (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.rewriting.apply_inside_type_lambdas_and_annotations" (func $hydra.rewriting.apply_inside_type_lambdas_and_annotations) )
  (export "hydra.rewriting.fold_over_term" (func $hydra.rewriting.fold_over_term) )
  (export "hydra.rewriting.fold_over_type" (func $hydra.rewriting.fold_over_type) )
  (export "hydra.rewriting.fold_term_with_graph_and_path" (func $hydra.rewriting.fold_term_with_graph_and_path) )
  (export "hydra.rewriting.map_beneath_type_annotations" (func $hydra.rewriting.map_beneath_type_annotations) )
  (export "hydra.rewriting.rewrite_and_fold_term" (func $hydra.rewriting.rewrite_and_fold_term) )
  (export "hydra.rewriting.rewrite_and_fold_term_with_graph" (func $hydra.rewriting.rewrite_and_fold_term_with_graph) )
  (export "hydra.rewriting.rewrite_and_fold_term_with_graph_and_path" (func $hydra.rewriting.rewrite_and_fold_term_with_graph_and_path) )
  (export "hydra.rewriting.rewrite_and_fold_term_with_path" (func $hydra.rewriting.rewrite_and_fold_term_with_path) )
  (export "hydra.rewriting.rewrite_term" (func $hydra.rewriting.rewrite_term) )
  (export "hydra.rewriting.rewrite_term_m" (func $hydra.rewriting.rewrite_term_m) )
  (export "hydra.rewriting.rewrite_term_with_context" (func $hydra.rewriting.rewrite_term_with_context) )
  (export "hydra.rewriting.rewrite_term_with_context_m" (func $hydra.rewriting.rewrite_term_with_context_m) )
  (export "hydra.rewriting.rewrite_term_with_graph" (func $hydra.rewriting.rewrite_term_with_graph) )
  (export "hydra.rewriting.rewrite_type" (func $hydra.rewriting.rewrite_type) )
  (export "hydra.rewriting.rewrite_type_m" (func $hydra.rewriting.rewrite_type_m) )
  (export "hydra.rewriting.subterms" (func $hydra.rewriting.subterms) )
  (export "hydra.rewriting.subterms_with_steps" (func $hydra.rewriting.subterms_with_steps) )
  (export "hydra.rewriting.subtypes" (func $hydra.rewriting.subtypes) )
  (func $hydra.rewriting.apply_inside_type_lambdas_and_annotations (param $f i32) (param $term0 i32) (result i32)
  (local $at i32)
  (local $tl i32)
  (block $end_term (result i32)
  (block $type_lambda
  (block $annotated
  local.get $term0
  br_table $annotated $type_lambda $type_lambda
)
  local.get $f
  local.get $at
  ;; project field: body
  call $hydra.rewriting.apply_inside_type_lambdas_and_annotations
  local.get $at
  ;; project field: annotation
  br $end_term
)
  local.get $tl
  ;; project field: parameter
  local.get $f
  local.get $tl
  ;; project field: body
  call $hydra.rewriting.apply_inside_type_lambdas_and_annotations
  br $end_term
)
)
  (func $hydra.rewriting.fold_over_term (param $order i32) (param $fld i32) (param $b0 i32) (param $term i32) (result i32)
  (block $end_traversal_order (result i32)
  (block $post
  (block $pre
  local.get $order
  br_table $pre $post $post
)
  local.get $order
  local.get $fld
  call $hydra.rewriting.fold_over_term
  local.get $b0
  local.get $term
  local.get $fld
  local.get $term
  call $hydra.rewriting.subterms
  call $hydra.lib.lists.foldl
  br $end_traversal_order
)
  local.get $order
  local.get $fld
  call $hydra.rewriting.fold_over_term
  local.get $b0
  local.get $term
  call $hydra.rewriting.subterms
  call $hydra.lib.lists.foldl
  local.get $term
  local.get $fld
  br $end_traversal_order
)
)
  (func $hydra.rewriting.fold_over_type (param $order i32) (param $fld i32) (param $b0 i32) (param $typ i32) (result i32)
  (block $end_traversal_order (result i32)
  (block $post
  (block $pre
  local.get $order
  br_table $pre $post $post
)
  local.get $order
  local.get $fld
  call $hydra.rewriting.fold_over_type
  local.get $b0
  local.get $typ
  local.get $fld
  local.get $typ
  call $hydra.rewriting.subtypes
  call $hydra.lib.lists.foldl
  br $end_traversal_order
)
  local.get $order
  local.get $fld
  call $hydra.rewriting.fold_over_type
  local.get $b0
  local.get $typ
  call $hydra.rewriting.subtypes
  call $hydra.lib.lists.foldl
  local.get $typ
  local.get $fld
  br $end_traversal_order
)
)
  (func $hydra.rewriting.fold_term_with_graph_and_path (param $f i32) (param $cx0 i32) (param $val0 i32) (param $term0 i32) (result i32)
  (local $cx i32)
  (local $path i32)
  (local $r i32)
  (local $recurse i32)
  (local $recurse_for_user i32)
  (local $result i32)
  (local $subterm i32)
  (local $term i32)
  (local $val i32)
  (local $val_in i32)
  (local $wrapper i32)
  local.get $val_in
  local.get $subterm
  local.get $recurse
  local.set $r
  local.get $r
  call $hydra.lib.pairs.first
  local.set $recurse_for_user
  local.get $recurse_for_user
  local.get $path
  local.get $cx
  local.get $val
  local.get $term
  local.get $f
  local.get $term
  local.set $wrapper
  local.get $wrapper
  local.get $cx0
  local.get $val0
  local.get $term0
  call $hydra.rewriting.rewrite_and_fold_term_with_graph_and_path
  local.set $result
  local.get $result
  call $hydra.lib.pairs.first
)
  (func $hydra.rewriting.map_beneath_type_annotations (param $f i32) (param $t i32) (result i32)
  (local $at i32)
  (block $end_type (result i32)
  (block $annotated
  local.get $t
  br_table $annotated $annotated
)
  local.get $f
  local.get $at
  ;; project field: body
  call $hydra.rewriting.map_beneath_type_annotations
  local.get $at
  ;; project field: annotation
  br $end_type
)
)
  (func $hydra.rewriting.rewrite_and_fold_term (param $f i32) (param $term0 i32) (result i32)
  (local $a i32)
  (local $at i32)
  (local $binding i32)
  (local $bins i32)
  (local $cons i32)
  (local $cs i32)
  (local $dflt i32)
  (local $e i32)
  (local $el i32)
  (local $elm i32)
  (local $els i32)
  (local $f2 i32)
  (local $f3 i32)
  (local $field i32)
  (local $fields i32)
  (local $for_binding i32)
  (local $for_elimination i32)
  (local $for_field i32)
  (local $for_fields i32)
  (local $for_function i32)
  (local $for_many i32)
  (local $for_pair i32)
  (local $for_single i32)
  (local $fsub i32)
  (local $fun i32)
  (local $inj i32)
  (local $kv i32)
  (local $l i32)
  (local $m i32)
  (local $mt i32)
  (local $p i32)
  (local $pairs i32)
  (local $r i32)
  (local $r2 i32)
  (local $rcases i32)
  (local $re i32)
  (local $rec i32)
  (local $recurse i32)
  (local $renv i32)
  (local $rf i32)
  (local $rk i32)
  (local $rl i32)
  (local $rlhs i32)
  (local $rmd i32)
  (local $rr i32)
  (local $rrhs i32)
  (local $rs i32)
  (local $rv i32)
  (local $t i32)
  (local $t1 i32)
  (local $ta i32)
  (local $term i32)
  (local $term02 i32)
  (local $tl i32)
  (local $val i32)
  (local $val0 i32)
  (local $val1 i32)
  (local $wt i32)
  (local $x i32)
  local.get $val
  local.get $term
  local.get $rec
  local.set $r
  local.get $r
  call $hydra.lib.pairs.first
  local.get $r
  call $hydra.lib.pairs.second
  local.get $cons
  local.set $for_single
  local.get $r
  call $hydra.lib.pairs.first
  local.get $el
  local.get $rec
  local.set $r2
  local.get $r2
  call $hydra.lib.pairs.first
  local.get $r2
  call $hydra.lib.pairs.second
  local.get $r
  call $hydra.lib.pairs.second
  call $hydra.lib.lists.cons
  local.get $val
  i32.const 0
  ;; list elements follow
  local.get $els
  call $hydra.lib.lists.foldl
  local.set $rr
  local.get $rr
  call $hydra.lib.pairs.first
  local.get $rr
  call $hydra.lib.pairs.second
  call $hydra.lib.lists.reverse
  local.get $cons
  local.set $for_many
  local.get $val
  local.get $field
  ;; project field: term
  local.get $recurse
  local.set $r
  local.get $r
  call $hydra.lib.pairs.first
  local.get $field
  ;; project field: name
  local.get $r
  call $hydra.lib.pairs.second
  local.set $for_field
  local.get $for_field
  local.get $x
  local.get $for_many
  local.set $for_fields
  local.get $val
  local.get $kv
  call $hydra.lib.pairs.first
  local.get $recurse
  local.set $rk
  local.get $rk
  call $hydra.lib.pairs.first
  local.get $kv
  call $hydra.lib.pairs.second
  local.get $recurse
  local.set $rv
  local.get $rv
  call $hydra.lib.pairs.first
  local.get $rk
  call $hydra.lib.pairs.second
  local.get $rv
  call $hydra.lib.pairs.second
  local.set $for_pair
  local.get $val
  local.get $binding
  ;; project field: term
  local.get $recurse
  local.set $r
  local.get $r
  call $hydra.lib.pairs.first
  local.get $binding
  ;; project field: name
  local.get $r
  call $hydra.lib.pairs.second
  local.get $binding
  ;; project field: type
  local.set $for_binding
  (block $end_elimination (result i32)
  (block $union
  local.get $elm
  br_table $union $union
)
  local.get $val
  local.get $recurse
  local.get $cs
  ;; project field: default
  call $hydra.lib.maybes.map
  local.set $rmd
  local.get $val
  call $hydra.lib.pairs.first
  local.get $rmd
  call $hydra.lib.maybes.maybe
  local.set $val1
  local.get $val1
  local.get $cs
  ;; project field: cases
  local.get $for_fields
  local.set $rcases
  local.get $rcases
  call $hydra.lib.pairs.first
  local.get $cs
  ;; project field: type_name
  call $hydra.lib.pairs.second
  local.get $rmd
  call $hydra.lib.maybes.map
  local.get $rcases
  call $hydra.lib.pairs.second
  br $end_elimination
)
  local.set $r
  local.get $r
  call $hydra.lib.pairs.first
  local.get $r
  call $hydra.lib.pairs.second
  local.set $for_elimination
  (block $end_function (result i32)
  (block $lambda
  (block $elimination
  local.get $fun
  br_table $elimination $lambda $lambda
)
  local.get $val
  local.get $elm
  local.get $for_elimination
  local.set $re
  local.get $re
  call $hydra.lib.pairs.first
  local.get $re
  call $hydra.lib.pairs.second
  br $end_function
)
  local.get $val
  local.get $l
  ;; project field: body
  local.get $recurse
  local.set $rl
  local.get $rl
  call $hydra.lib.pairs.first
  local.get $l
  ;; project field: parameter
  local.get $l
  ;; project field: domain
  local.get $rl
  call $hydra.lib.pairs.second
  br $end_function
)
  local.set $for_function
  local.get $val0
  local.get $term02
  local.set $dflt
  (block $end_term (result i32)
  (block $wrap
  (block $union
  (block $type_lambda
  (block $type_application
  (block $set
  (block $record
  (block $pair
  (block $maybe
  (block $map
  (block $list
  (block $let
  (block $function
  (block $either
  (block $application
  (block $annotated
  local.get $term02
  br_table $annotated $application $either $function $let $list $map $maybe $pair $record $set $type_application $type_lambda $union $wrap $wrap
)
  local.get $recurse
  local.get $t
  local.get $at
  ;; project field: annotation
  local.get $val0
  local.get $at
  ;; project field: body
  local.get $for_single
  br $end_term
)
  local.get $val0
  local.get $a
  ;; project field: function
  local.get $recurse
  local.set $rlhs
  local.get $rlhs
  call $hydra.lib.pairs.first
  local.get $a
  ;; project field: argument
  local.get $recurse
  local.set $rrhs
  local.get $rrhs
  call $hydra.lib.pairs.first
  local.get $rlhs
  call $hydra.lib.pairs.second
  local.get $rrhs
  call $hydra.lib.pairs.second
  br $end_term
)
  local.get $val0
  local.get $l
  local.get $recurse
  local.set $rl
  local.get $rl
  call $hydra.lib.pairs.first
  i32.const 0
  local.get $rl
  call $hydra.lib.pairs.second
  local.get $val0
  local.get $r
  local.get $recurse
  local.set $rr
  local.get $rr
  call $hydra.lib.pairs.first
  i32.const 1
  local.get $rr
  call $hydra.lib.pairs.second
  local.get $e
  call $hydra.lib.eithers.either
  br $end_term
)
  local.get $for_function
  local.get $f3
  local.get $val0
  local.get $f2
  local.get $for_single
  br $end_term
)
  local.get $val0
  local.get $l
  ;; project field: body
  local.get $recurse
  local.set $renv
  local.get $for_binding
  local.get $bins
  local.get $renv
  call $hydra.lib.pairs.second
  local.get $renv
  call $hydra.lib.pairs.first
  local.get $l
  ;; project field: bindings
  local.get $for_many
  br $end_term
)
  local.get $recurse
  local.get $x
  local.get $val0
  local.get $els
  local.get $for_many
  br $end_term
)
  local.get $for_pair
  local.get $pairs
  call $hydra.lib.maps.from_list
  local.get $val0
  local.get $m
  call $hydra.lib.maps.to_list
  local.get $for_many
  br $end_term
)
  local.get $dflt
  local.get $recurse
  local.get $t1
  local.get $val0
  local.get $t
  local.get $for_single
  local.get $mt
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.get $val0
  local.get $p
  call $hydra.lib.pairs.first
  local.get $recurse
  local.set $rf
  local.get $rf
  call $hydra.lib.pairs.first
  local.get $p
  call $hydra.lib.pairs.second
  local.get $recurse
  local.set $rs
  local.get $rs
  call $hydra.lib.pairs.first
  local.get $rf
  call $hydra.lib.pairs.second
  local.get $rs
  call $hydra.lib.pairs.second
  br $end_term
)
  local.get $for_field
  local.get $r
  ;; project field: type_name
  local.get $fields
  local.get $val0
  local.get $r
  ;; project field: fields
  local.get $for_many
  br $end_term
)
  local.get $recurse
  local.get $e
  call $hydra.lib.sets.from_list
  local.get $val0
  local.get $els
  call $hydra.lib.sets.to_list
  local.get $for_many
  br $end_term
)
  local.get $recurse
  local.get $t
  local.get $ta
  ;; project field: type
  local.get $val0
  local.get $ta
  ;; project field: body
  local.get $for_single
  br $end_term
)
  local.get $recurse
  local.get $tl
  ;; project field: parameter
  local.get $t
  local.get $val0
  local.get $tl
  ;; project field: body
  local.get $for_single
  br $end_term
)
  local.get $recurse
  local.get $inj
  ;; project field: type_name
  local.get $inj
  ;; project field: field
  ;; project field: name
  local.get $t
  local.get $val0
  local.get $inj
  ;; project field: field
  ;; project field: term
  local.get $for_single
  br $end_term
)
  local.get $recurse
  local.get $wt
  ;; project field: type_name
  local.get $t
  local.get $val0
  local.get $wt
  ;; project field: body
  local.get $for_single
  br $end_term
)
  local.set $fsub
  local.get $recurse
  local.get $fsub
  local.get $f
  local.set $recurse
  local.get $term0
  local.get $recurse
)
  (func $hydra.rewriting.rewrite_and_fold_term_with_graph (param $f i32) (param $cx0 i32) (param $val0 i32) (param $term0 i32) (result i32)
  (local $cx i32)
  (local $cx1 i32)
  (local $f_result i32)
  (local $fun i32)
  (local $l i32)
  (local $low_level_recurse i32)
  (local $new_val i32)
  (local $recurse_for_user i32)
  (local $result i32)
  (local $subterm i32)
  (local $term i32)
  (local $tl i32)
  (local $val i32)
  (local $val_and_cx i32)
  (local $wrapper i32)
  local.get $val_and_cx
  call $hydra.lib.pairs.first
  local.set $val
  local.get $val_and_cx
  call $hydra.lib.pairs.second
  local.set $cx
  (block $end_term (result i32)
  (block $type_lambda
  (block $let
  (block $function
  local.get $term
  br_table $function $let $type_lambda $type_lambda
)
  (block $end_function (result i32)
  (block $lambda
  local.get $fun
  br_table $lambda $lambda
)
  local.get $cx
  local.get $l
  call $hydra.scoping.extend_graph_for_lambda
  br $end_function
)
  br $end_term
)
  i32.const 0
  local.get $cx
  local.get $l
  call $hydra.scoping.extend_graph_for_let
  br $end_term
)
  local.get $cx
  local.get $tl
  call $hydra.scoping.extend_graph_for_type_lambda
  br $end_term
)
  local.set $cx1
  local.get $new_val
  local.get $cx1
  local.get $subterm
  local.get $low_level_recurse
  local.set $result
  local.get $result
  call $hydra.lib.pairs.first
  call $hydra.lib.pairs.first
  local.get $result
  call $hydra.lib.pairs.second
  local.set $recurse_for_user
  local.get $recurse_for_user
  local.get $cx1
  local.get $val
  local.get $term
  local.get $f
  local.set $f_result
  local.get $f_result
  call $hydra.lib.pairs.first
  local.get $cx
  local.get $f_result
  call $hydra.lib.pairs.second
  local.set $wrapper
  local.get $wrapper
  local.get $val0
  local.get $cx0
  local.get $term0
  call $hydra.rewriting.rewrite_and_fold_term
  local.set $result
  local.get $result
  call $hydra.lib.pairs.first
  call $hydra.lib.pairs.first
  local.get $result
  call $hydra.lib.pairs.second
)
  (func $hydra.rewriting.rewrite_and_fold_term_with_graph_and_path (param $f i32) (param $cx0 i32) (param $val0 i32) (param $term0 i32) (result i32)
  (local $cx i32)
  (local $cx1 i32)
  (local $cx_and_val i32)
  (local $f_result i32)
  (local $fun i32)
  (local $l i32)
  (local $path i32)
  (local $recurse i32)
  (local $recurse_for_user i32)
  (local $result i32)
  (local $term i32)
  (local $term_in i32)
  (local $tl i32)
  (local $val i32)
  (local $val_in i32)
  (local $wrapper i32)
  local.get $cx_and_val
  call $hydra.lib.pairs.first
  local.set $cx
  local.get $cx_and_val
  call $hydra.lib.pairs.second
  local.set $val
  (block $end_term (result i32)
  (block $type_lambda
  (block $let
  (block $function
  local.get $term
  br_table $function $let $type_lambda $type_lambda
)
  (block $end_function (result i32)
  (block $lambda
  local.get $fun
  br_table $lambda $lambda
)
  local.get $cx
  local.get $l
  call $hydra.scoping.extend_graph_for_lambda
  br $end_function
)
  br $end_term
)
  i32.const 0
  local.get $cx
  local.get $l
  call $hydra.scoping.extend_graph_for_let
  br $end_term
)
  local.get $cx
  local.get $tl
  call $hydra.scoping.extend_graph_for_type_lambda
  br $end_term
)
  local.set $cx1
  local.get $path
  local.get $cx1
  local.get $val_in
  local.get $term_in
  local.get $recurse
  local.set $result
  local.get $result
  call $hydra.lib.pairs.first
  call $hydra.lib.pairs.second
  local.get $result
  call $hydra.lib.pairs.second
  local.set $recurse_for_user
  local.get $recurse_for_user
  local.get $path
  local.get $cx1
  local.get $val
  local.get $term
  local.get $f
  local.set $f_result
  local.get $cx
  local.get $f_result
  call $hydra.lib.pairs.first
  local.get $f_result
  call $hydra.lib.pairs.second
  local.set $wrapper
  local.get $wrapper
  local.get $cx0
  local.get $val0
  local.get $term0
  call $hydra.rewriting.rewrite_and_fold_term_with_path
  local.set $result
  local.get $result
  call $hydra.lib.pairs.first
  call $hydra.lib.pairs.second
  local.get $result
  call $hydra.lib.pairs.second
)
  (func $hydra.rewriting.rewrite_and_fold_term_with_path (param $f i32) (param $term0 i32) (result i32)
  (local $a i32)
  (local $accessor i32)
  (local $accessor_term_pairs i32)
  (local $at i32)
  (local $atp i32)
  (local $binding i32)
  (local $cons i32)
  (local $cs i32)
  (local $def i32)
  (local $dflt i32)
  (local $e i32)
  (local $el i32)
  (local $elm i32)
  (local $els i32)
  (local $f2 i32)
  (local $field i32)
  (local $field1 i32)
  (local $for_binding_with_accessor i32)
  (local $for_elimination i32)
  (local $for_field_with_accessor i32)
  (local $for_fields_with_accessor i32)
  (local $for_function i32)
  (local $for_many_with_accessors i32)
  (local $for_pair_with_accessors i32)
  (local $for_single_with_accessor i32)
  (local $fsub i32)
  (local $ft i32)
  (local $fun i32)
  (local $idx i32)
  (local $inj i32)
  (local $key_accessor i32)
  (local $kv i32)
  (local $l i32)
  (local $m i32)
  (local $mk_accessor i32)
  (local $mt i32)
  (local $p i32)
  (local $path i32)
  (local $r i32)
  (local $r2 i32)
  (local $rb i32)
  (local $rbindings i32)
  (local $rcases i32)
  (local $re i32)
  (local $rec i32)
  (local $recurse i32)
  (local $renv i32)
  (local $rf i32)
  (local $rfields i32)
  (local $rk i32)
  (local $rl i32)
  (local $rlhs i32)
  (local $rmd i32)
  (local $rr i32)
  (local $rrhs i32)
  (local $rs i32)
  (local $rv i32)
  (local $t i32)
  (local $t1 i32)
  (local $ta i32)
  (local $term i32)
  (local $term02 i32)
  (local $tl i32)
  (local $val i32)
  (local $val0 i32)
  (local $val1 i32)
  (local $val_accessor i32)
  (local $wt i32)
  (local $x i32)
  local.get $path
  i32.const 1
  ;; list elements follow
  local.get $accessor
  call $hydra.lib.lists.concat2
  local.get $val
  local.get $term
  local.get $rec
  local.set $r
  local.get $r
  call $hydra.lib.pairs.first
  local.get $r
  call $hydra.lib.pairs.second
  local.get $cons
  local.set $for_single_with_accessor
  local.get $path
  i32.const 1
  ;; list elements follow
  local.get $atp
  call $hydra.lib.pairs.first
  call $hydra.lib.lists.concat2
  local.get $r
  call $hydra.lib.pairs.first
  local.get $atp
  call $hydra.lib.pairs.second
  local.get $rec
  local.set $r2
  local.get $r2
  call $hydra.lib.pairs.first
  local.get $r2
  call $hydra.lib.pairs.second
  local.get $r
  call $hydra.lib.pairs.second
  call $hydra.lib.lists.cons
  local.get $val
  i32.const 0
  ;; list elements follow
  local.get $accessor_term_pairs
  call $hydra.lib.lists.foldl
  local.set $rr
  local.get $rr
  call $hydra.lib.pairs.first
  local.get $rr
  call $hydra.lib.pairs.second
  call $hydra.lib.lists.reverse
  local.get $cons
  local.set $for_many_with_accessors
  local.get $path
  i32.const 1
  ;; list elements follow
  local.get $field
  ;; project field: name
  local.get $mk_accessor
  call $hydra.lib.lists.concat2
  local.get $val
  local.get $field
  ;; project field: term
  local.get $recurse
  local.set $r
  local.get $r
  call $hydra.lib.pairs.first
  local.get $field
  ;; project field: name
  local.get $r
  call $hydra.lib.pairs.second
  local.set $for_field_with_accessor
  local.get $mk_accessor
  local.get $val1
  local.get $field1
  local.get $for_field_with_accessor
  local.get $x
  local.get $for_many_with_accessors
  local.set $for_fields_with_accessor
  local.get $path
  i32.const 1
  ;; list elements follow
  local.get $key_accessor
  call $hydra.lib.lists.concat2
  local.get $val
  local.get $kv
  call $hydra.lib.pairs.first
  local.get $recurse
  local.set $rk
  local.get $path
  i32.const 1
  ;; list elements follow
  local.get $val_accessor
  call $hydra.lib.lists.concat2
  local.get $rk
  call $hydra.lib.pairs.first
  local.get $kv
  call $hydra.lib.pairs.second
  local.get $recurse
  local.set $rv
  local.get $rv
  call $hydra.lib.pairs.first
  local.get $rk
  call $hydra.lib.pairs.second
  local.get $rv
  call $hydra.lib.pairs.second
  local.set $for_pair_with_accessors
  local.get $path
  i32.const 1
  ;; list elements follow
  local.get $binding
  ;; project field: name
  call $hydra.lib.lists.concat2
  local.get $val
  local.get $binding
  ;; project field: term
  local.get $recurse
  local.set $r
  local.get $r
  call $hydra.lib.pairs.first
  local.get $binding
  ;; project field: name
  local.get $r
  call $hydra.lib.pairs.second
  local.get $binding
  ;; project field: type
  local.set $for_binding_with_accessor
  (block $end_elimination (result i32)
  (block $union
  local.get $elm
  br_table $union $union
)
  local.get $path
  i32.const 1
  ;; list elements follow
  i32.const 0
  call $hydra.lib.lists.concat2
  local.get $val
  local.get $def
  local.get $recurse
  local.get $cs
  ;; project field: default
  call $hydra.lib.maybes.map
  local.set $rmd
  local.get $val
  call $hydra.lib.pairs.first
  local.get $rmd
  call $hydra.lib.maybes.maybe
  local.set $val1
  local.get $recurse
  local.get $x
  local.get $val1
  local.get $f2
  ;; project field: name
  local.get $f2
  ;; project field: term
  local.get $cs
  ;; project field: cases
  call $hydra.lib.lists.map
  local.get $for_many_with_accessors
  local.set $rcases
  local.get $rcases
  call $hydra.lib.pairs.first
  local.get $cs
  ;; project field: type_name
  call $hydra.lib.pairs.second
  local.get $rmd
  call $hydra.lib.maybes.map
  local.get $ft
  call $hydra.lib.pairs.first
  local.get $ft
  call $hydra.lib.pairs.second
  ;; project field: name
  local.get $cs
  ;; project field: cases
  call $hydra.lib.lists.map
  local.get $rcases
  call $hydra.lib.pairs.second
  call $hydra.lib.lists.zip
  call $hydra.lib.lists.map
  br $end_elimination
)
  local.set $r
  local.get $r
  call $hydra.lib.pairs.first
  local.get $r
  call $hydra.lib.pairs.second
  local.set $for_elimination
  (block $end_function (result i32)
  (block $lambda
  (block $elimination
  local.get $fun
  br_table $elimination $lambda $lambda
)
  local.get $val
  local.get $elm
  local.get $for_elimination
  local.set $re
  local.get $re
  call $hydra.lib.pairs.first
  local.get $re
  call $hydra.lib.pairs.second
  br $end_function
)
  local.get $path
  i32.const 1
  ;; list elements follow
  i32.const 0
  call $hydra.lib.lists.concat2
  local.get $val
  local.get $l
  ;; project field: body
  local.get $recurse
  local.set $rl
  local.get $rl
  call $hydra.lib.pairs.first
  local.get $l
  ;; project field: parameter
  local.get $l
  ;; project field: domain
  local.get $rl
  call $hydra.lib.pairs.second
  br $end_function
)
  local.set $for_function
  local.get $val0
  local.get $term02
  local.set $dflt
  (block $end_term (result i32)
  (block $wrap
  (block $union
  (block $type_lambda
  (block $type_application
  (block $set
  (block $record
  (block $pair
  (block $maybe
  (block $map
  (block $list
  (block $let
  (block $function
  (block $either
  (block $application
  (block $annotated
  local.get $term02
  br_table $annotated $application $either $function $let $list $map $maybe $pair $record $set $type_application $type_lambda $union $wrap $wrap
)
  local.get $recurse
  local.get $t
  local.get $at
  ;; project field: annotation
  i32.const 0
  local.get $val0
  local.get $at
  ;; project field: body
  local.get $for_single_with_accessor
  br $end_term
)
  local.get $path
  i32.const 1
  ;; list elements follow
  i32.const 0
  call $hydra.lib.lists.concat2
  local.get $val0
  local.get $a
  ;; project field: function
  local.get $recurse
  local.set $rlhs
  local.get $path
  i32.const 1
  ;; list elements follow
  i32.const 0
  call $hydra.lib.lists.concat2
  local.get $rlhs
  call $hydra.lib.pairs.first
  local.get $a
  ;; project field: argument
  local.get $recurse
  local.set $rrhs
  local.get $rrhs
  call $hydra.lib.pairs.first
  local.get $rlhs
  call $hydra.lib.pairs.second
  local.get $rrhs
  call $hydra.lib.pairs.second
  br $end_term
)
  local.get $path
  i32.const 1
  ;; list elements follow
  i32.const 0
  call $hydra.lib.lists.concat2
  local.get $val0
  local.get $l
  local.get $recurse
  local.set $rl
  local.get $rl
  call $hydra.lib.pairs.first
  i32.const 0
  local.get $rl
  call $hydra.lib.pairs.second
  local.get $path
  i32.const 1
  ;; list elements follow
  i32.const 0
  call $hydra.lib.lists.concat2
  local.get $val0
  local.get $r
  local.get $recurse
  local.set $rr
  local.get $rr
  call $hydra.lib.pairs.first
  i32.const 1
  local.get $rr
  call $hydra.lib.pairs.second
  local.get $e
  call $hydra.lib.eithers.either
  br $end_term
)
  local.get $val0
  local.get $f2
  local.get $for_function
  local.set $rf
  local.get $rf
  call $hydra.lib.pairs.first
  local.get $rf
  call $hydra.lib.pairs.second
  br $end_term
)
  local.get $path
  i32.const 1
  ;; list elements follow
  i32.const 0
  call $hydra.lib.lists.concat2
  local.get $val0
  local.get $l
  ;; project field: body
  local.get $recurse
  local.set $renv
  local.get $r
  call $hydra.lib.pairs.first
  local.get $binding
  local.get $for_binding_with_accessor
  local.set $rb
  local.get $rb
  call $hydra.lib.pairs.first
  local.get $rb
  call $hydra.lib.pairs.second
  local.get $r
  call $hydra.lib.pairs.second
  call $hydra.lib.lists.cons
  local.get $renv
  call $hydra.lib.pairs.first
  i32.const 0
  ;; list elements follow
  local.get $l
  ;; project field: bindings
  call $hydra.lib.lists.foldl
  local.set $rbindings
  local.get $rbindings
  call $hydra.lib.pairs.first
  local.get $rbindings
  call $hydra.lib.pairs.second
  call $hydra.lib.lists.reverse
  local.get $renv
  call $hydra.lib.pairs.second
  br $end_term
)
  i32.const 0
  local.set $idx
  local.get $path
  i32.const 1
  ;; list elements follow
  local.get $r
  call $hydra.lib.pairs.first
  call $hydra.lib.lists.concat2
  local.get $r
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.get $el
  local.get $recurse
  local.set $r2
  local.get $r
  call $hydra.lib.pairs.first
  i32.const 1
  call $hydra.lib.math.add
  local.get $r2
  call $hydra.lib.pairs.first
  local.get $r2
  call $hydra.lib.pairs.second
  local.get $r
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  call $hydra.lib.lists.cons
  local.get $idx
  local.get $val0
  i32.const 0
  ;; list elements follow
  local.get $els
  call $hydra.lib.lists.foldl
  local.set $rr
  local.get $rr
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.get $rr
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  call $hydra.lib.lists.reverse
  br $end_term
)
  i32.const 0
  local.set $idx
  local.get $path
  i32.const 1
  ;; list elements follow
  local.get $r
  call $hydra.lib.pairs.first
  call $hydra.lib.lists.concat2
  local.get $r
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.get $kv
  call $hydra.lib.pairs.first
  local.get $recurse
  local.set $rk
  local.get $path
  i32.const 1
  ;; list elements follow
  local.get $r
  call $hydra.lib.pairs.first
  call $hydra.lib.lists.concat2
  local.get $rk
  call $hydra.lib.pairs.first
  local.get $kv
  call $hydra.lib.pairs.second
  local.get $recurse
  local.set $rv
  local.get $r
  call $hydra.lib.pairs.first
  i32.const 1
  call $hydra.lib.math.add
  local.get $rv
  call $hydra.lib.pairs.first
  local.get $rk
  call $hydra.lib.pairs.second
  local.get $rv
  call $hydra.lib.pairs.second
  local.get $r
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  call $hydra.lib.lists.cons
  local.get $idx
  local.get $val0
  i32.const 0
  ;; list elements follow
  local.get $m
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.foldl
  local.set $rr
  local.get $rr
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.get $rr
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  call $hydra.lib.lists.reverse
  call $hydra.lib.maps.from_list
  br $end_term
)
  local.get $dflt
  local.get $recurse
  local.get $t1
  i32.const 0
  local.get $val0
  local.get $t
  local.get $for_single_with_accessor
  local.get $mt
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.get $path
  i32.const 1
  ;; list elements follow
  i32.const 0
  call $hydra.lib.lists.concat2
  local.get $val0
  local.get $p
  call $hydra.lib.pairs.first
  local.get $recurse
  local.set $rf
  local.get $path
  i32.const 1
  ;; list elements follow
  i32.const 1
  call $hydra.lib.lists.concat2
  local.get $rf
  call $hydra.lib.pairs.first
  local.get $p
  call $hydra.lib.pairs.second
  local.get $recurse
  local.set $rs
  local.get $rs
  call $hydra.lib.pairs.first
  local.get $rf
  call $hydra.lib.pairs.second
  local.get $rs
  call $hydra.lib.pairs.second
  br $end_term
)
  local.get $recurse
  local.get $x
  local.get $val0
  local.get $f2
  ;; project field: name
  local.get $f2
  ;; project field: term
  local.get $r
  ;; project field: fields
  call $hydra.lib.lists.map
  local.get $for_many_with_accessors
  local.set $rfields
  local.get $rfields
  call $hydra.lib.pairs.first
  local.get $r
  ;; project field: type_name
  local.get $ft
  call $hydra.lib.pairs.first
  local.get $ft
  call $hydra.lib.pairs.second
  ;; project field: name
  local.get $r
  ;; project field: fields
  call $hydra.lib.lists.map
  local.get $rfields
  call $hydra.lib.pairs.second
  call $hydra.lib.lists.zip
  call $hydra.lib.lists.map
  br $end_term
)
  i32.const 0
  local.set $idx
  local.get $path
  i32.const 1
  ;; list elements follow
  local.get $r
  call $hydra.lib.pairs.first
  call $hydra.lib.lists.concat2
  local.get $r
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.get $el
  local.get $recurse
  local.set $r2
  local.get $r
  call $hydra.lib.pairs.first
  i32.const 1
  call $hydra.lib.math.add
  local.get $r2
  call $hydra.lib.pairs.first
  local.get $r2
  call $hydra.lib.pairs.second
  local.get $r
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  call $hydra.lib.lists.cons
  local.get $idx
  local.get $val0
  i32.const 0
  ;; list elements follow
  local.get $els
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.foldl
  local.set $rr
  local.get $rr
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.first
  local.get $rr
  call $hydra.lib.pairs.second
  call $hydra.lib.pairs.second
  call $hydra.lib.lists.reverse
  call $hydra.lib.sets.from_list
  br $end_term
)
  local.get $recurse
  local.get $t
  local.get $ta
  ;; project field: type
  i32.const 0
  local.get $val0
  local.get $ta
  ;; project field: body
  local.get $for_single_with_accessor
  br $end_term
)
  local.get $recurse
  local.get $tl
  ;; project field: parameter
  local.get $t
  i32.const 0
  local.get $val0
  local.get $tl
  ;; project field: body
  local.get $for_single_with_accessor
  br $end_term
)
  local.get $recurse
  local.get $inj
  ;; project field: type_name
  local.get $inj
  ;; project field: field
  ;; project field: name
  local.get $t
  i32.const 0
  local.get $val0
  local.get $inj
  ;; project field: field
  ;; project field: term
  local.get $for_single_with_accessor
  br $end_term
)
  local.get $recurse
  local.get $wt
  ;; project field: type_name
  local.get $t
  i32.const 0
  local.get $val0
  local.get $wt
  ;; project field: body
  local.get $for_single_with_accessor
  br $end_term
)
  local.set $fsub
  local.get $recurse
  local.get $fsub
  local.get $f
  local.set $recurse
  i32.const 0
  ;; list elements follow
  local.get $term0
  local.get $recurse
)
  (func $hydra.rewriting.rewrite_term (param $f i32) (param $term0 i32) (result i32)
  (local $a i32)
  (local $at i32)
  (local $b i32)
  (local $cs i32)
  (local $e i32)
  (local $elm i32)
  (local $els i32)
  (local $f2 i32)
  (local $for_elimination i32)
  (local $for_field i32)
  (local $for_function i32)
  (local $for_let i32)
  (local $for_map i32)
  (local $for_pair i32)
  (local $fsub i32)
  (local $fun i32)
  (local $i i32)
  (local $l i32)
  (local $lt i32)
  (local $m i32)
  (local $map_binding i32)
  (local $name i32)
  (local $p i32)
  (local $r i32)
  (local $recurse i32)
  (local $s i32)
  (local $ta i32)
  (local $term i32)
  (local $tt i32)
  (local $v i32)
  (local $wt i32)
  local.get $f2
  ;; project field: name
  local.get $f2
  ;; project field: term
  local.get $recurse
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
  local.get $recurse
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
  local.get $recurse
  br $end_function
)
  local.set $for_function
  local.get $b
  ;; project field: name
  local.get $b
  ;; project field: term
  local.get $recurse
  local.get $b
  ;; project field: type
  local.set $map_binding
  local.get $map_binding
  local.get $lt
  ;; project field: bindings
  call $hydra.lib.lists.map
  local.get $lt
  ;; project field: body
  local.get $recurse
  local.set $for_let
  local.get $p
  call $hydra.lib.pairs.first
  local.get $recurse
  local.get $p
  call $hydra.lib.pairs.second
  local.get $recurse
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
  local.get $term
  br_table $annotated $application $either $function $let $list $literal $map $maybe $pair $record $set $type_application $type_lambda $union $unit $variable $wrap $wrap
)
  local.get $at
  ;; project field: body
  local.get $recurse
  local.get $at
  ;; project field: annotation
  br $end_term
)
  local.get $a
  ;; project field: function
  local.get $recurse
  local.get $a
  ;; project field: argument
  local.get $recurse
  br $end_term
)
  i32.const 0
  local.get $l
  local.get $recurse
  i32.const 1
  local.get $r
  local.get $recurse
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
  local.get $recurse
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
  local.get $recurse
  local.get $m
  call $hydra.lib.maybes.map
  br $end_term
)
  local.get $p
  call $hydra.lib.pairs.first
  local.get $recurse
  local.get $p
  call $hydra.lib.pairs.second
  local.get $recurse
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
  local.get $recurse
  local.get $s
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.sets.from_list
  br $end_term
)
  local.get $tt
  ;; project field: body
  local.get $recurse
  local.get $tt
  ;; project field: type
  br $end_term
)
  local.get $ta
  ;; project field: parameter
  local.get $ta
  ;; project field: body
  local.get $recurse
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
  local.get $recurse
  br $end_term
)
  local.set $fsub
  local.get $recurse
  local.get $fsub
  local.get $f
  local.set $recurse
  local.get $term0
  local.get $recurse
)
  (func $hydra.rewriting.rewrite_term_m (param $f i32) (param $term0 i32) (result i32)
  (local $app i32)
  (local $at i32)
  (local $b i32)
  (local $bindings i32)
  (local $body i32)
  (local $cases i32)
  (local $cs i32)
  (local $d i32)
  (local $def i32)
  (local $e i32)
  (local $els i32)
  (local $env i32)
  (local $ex i32)
  (local $field i32)
  (local $fields i32)
  (local $for_elm i32)
  (local $for_field i32)
  (local $for_fun i32)
  (local $for_pair i32)
  (local $fsub i32)
  (local $fun i32)
  (local $fun2 i32)
  (local $i i32)
  (local $k i32)
  (local $kv i32)
  (local $l i32)
  (local $lhs i32)
  (local $lt i32)
  (local $m i32)
  (local $map_binding i32)
  (local $n i32)
  (local $name i32)
  (local $p i32)
  (local $pairs i32)
  (local $r i32)
  (local $rbindings i32)
  (local $rbody i32)
  (local $rcases i32)
  (local $rdef i32)
  (local $re i32)
  (local $recurse i32)
  (local $rels i32)
  (local $renv i32)
  (local $rf i32)
  (local $rfield i32)
  (local $rfields i32)
  (local $rfun i32)
  (local $rhs i32)
  (local $rlist i32)
  (local $rm i32)
  (local $rs i32)
  (local $rt i32)
  (local $s i32)
  (local $t i32)
  (local $term i32)
  (local $tl i32)
  (local $tt i32)
  (local $v i32)
  (local $wt i32)
  (local $x i32)
  local.get $field
  ;; project field: term
  local.get $recurse
  i32.const 1
  local.get $field
  ;; project field: name
  local.get $t
  call $hydra.lib.eithers.bind
  local.set $for_field
  local.get $kv
  call $hydra.lib.pairs.first
  local.get $recurse
  local.get $kv
  call $hydra.lib.pairs.second
  local.get $recurse
  i32.const 1
  local.get $k
  local.get $v
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  local.set $for_pair
  local.get $b
  ;; project field: term
  local.get $recurse
  i32.const 1
  local.get $b
  ;; project field: name
  local.get $v
  local.get $b
  ;; project field: type
  call $hydra.lib.eithers.bind
  local.set $map_binding
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
  local.get $at
  ;; project field: body
  local.get $recurse
  i32.const 1
  local.get $ex
  local.get $at
  ;; project field: annotation
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $app
  ;; project field: function
  local.get $recurse
  local.get $app
  ;; project field: argument
  local.get $recurse
  i32.const 1
  local.get $lhs
  local.get $rhs
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  i32.const 0
  local.get $x
  local.get $l
  local.get $recurse
  call $hydra.lib.eithers.map
  i32.const 1
  local.get $x
  local.get $r
  local.get $recurse
  call $hydra.lib.eithers.map
  local.get $e
  call $hydra.lib.eithers.either
  i32.const 1
  local.get $re
  call $hydra.lib.eithers.bind
  br $end_term
)
  (block $end_elimination (result i32)
  (block $wrap
  (block $union
  (block $record
  local.get $e
  br_table $record $union $wrap $wrap
)
  i32.const 1
  local.get $p
  br $end_elimination
)
  local.get $cs
  ;; project field: type_name
  local.set $n
  local.get $cs
  ;; project field: default
  local.set $def
  local.get $cs
  ;; project field: cases
  local.set $cases
  i32.const 1
  i32.const 0
  call $hydra.lib.maybes.pure
  local.get $t
  local.get $recurse
  call $hydra.lib.eithers.map
  local.get $def
  call $hydra.lib.maybes.maybe
  local.get $n
  local.get $rdef
  local.get $rcases
  local.get $for_field
  local.get $cases
  call $hydra.lib.eithers.map_list
  call $hydra.lib.eithers.map
  call $hydra.lib.eithers.bind
  br $end_elimination
)
  i32.const 1
  local.get $name
  br $end_elimination
)
  local.set $for_elm
  (block $end_function (result i32)
  (block $lambda
  (block $elimination
  local.get $fun2
  br_table $elimination $lambda $lambda
)
  local.get $e
  local.get $for_elm
  br $end_function
)
  local.get $l
  ;; project field: parameter
  local.set $v
  local.get $l
  ;; project field: domain
  local.set $d
  local.get $l
  ;; project field: body
  local.set $body
  local.get $body
  local.get $recurse
  i32.const 1
  local.get $v
  local.get $d
  local.get $rbody
  call $hydra.lib.eithers.bind
  br $end_function
)
  local.set $for_fun
  local.get $fun
  local.get $for_fun
  i32.const 1
  local.get $rfun
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $lt
  ;; project field: bindings
  local.set $bindings
  local.get $lt
  ;; project field: body
  local.set $env
  local.get $map_binding
  local.get $bindings
  call $hydra.lib.eithers.map_list
  local.get $env
  local.get $recurse
  i32.const 1
  local.get $rbindings
  local.get $renv
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $recurse
  local.get $els
  call $hydra.lib.eithers.map_list
  i32.const 1
  local.get $rels
  call $hydra.lib.eithers.bind
  br $end_term
)
  i32.const 1
  local.get $v
  br $end_term
)
  local.get $for_pair
  local.get $m
  call $hydra.lib.maps.to_list
  call $hydra.lib.eithers.map_list
  i32.const 1
  local.get $pairs
  call $hydra.lib.maps.from_list
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $recurse
  local.get $m
  call $hydra.lib.eithers.map_maybe
  i32.const 1
  local.get $rm
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $p
  call $hydra.lib.pairs.first
  local.get $recurse
  local.get $p
  call $hydra.lib.pairs.second
  local.get $recurse
  i32.const 1
  local.get $rf
  local.get $rs
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $r
  ;; project field: type_name
  local.set $n
  local.get $r
  ;; project field: fields
  local.set $fields
  local.get $n
  local.get $rfields
  local.get $for_field
  local.get $fields
  call $hydra.lib.eithers.map_list
  call $hydra.lib.eithers.map
  br $end_term
)
  local.get $recurse
  local.get $s
  call $hydra.lib.sets.to_list
  call $hydra.lib.eithers.map_list
  i32.const 1
  local.get $rlist
  call $hydra.lib.sets.from_list
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $tt
  ;; project field: body
  local.get $recurse
  i32.const 1
  local.get $t
  local.get $tt
  ;; project field: type
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $tl
  ;; project field: parameter
  local.set $v
  local.get $tl
  ;; project field: body
  local.set $body
  local.get $body
  local.get $recurse
  i32.const 1
  local.get $v
  local.get $rbody
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $i
  ;; project field: type_name
  local.set $n
  local.get $i
  ;; project field: field
  local.set $field
  local.get $n
  local.get $rfield
  local.get $field
  local.get $for_field
  call $hydra.lib.eithers.map
  br $end_term
)
  i32.const 1
  i32.const 0
  br $end_term
)
  i32.const 1
  local.get $v
  br $end_term
)
  local.get $wt
  ;; project field: type_name
  local.set $name
  local.get $wt
  ;; project field: body
  local.set $t
  local.get $t
  local.get $recurse
  i32.const 1
  local.get $name
  local.get $rt
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.set $fsub
  local.get $recurse
  local.get $fsub
  local.get $f
  local.set $recurse
  local.get $term0
  local.get $recurse
)
  (func $hydra.rewriting.rewrite_term_with_context (param $f i32) (param $cx0 i32) (param $term0 i32) (result i32)
  (local $a i32)
  (local $at i32)
  (local $b i32)
  (local $cs i32)
  (local $cx i32)
  (local $e i32)
  (local $elm i32)
  (local $els i32)
  (local $field i32)
  (local $for_elimination i32)
  (local $for_field i32)
  (local $for_function i32)
  (local $for_let i32)
  (local $for_map i32)
  (local $for_pair i32)
  (local $for_subterms i32)
  (local $fun i32)
  (local $i i32)
  (local $l i32)
  (local $lt i32)
  (local $m i32)
  (local $map_binding i32)
  (local $name i32)
  (local $p i32)
  (local $r i32)
  (local $recurse i32)
  (local $recurse0 i32)
  (local $rewrite i32)
  (local $s i32)
  (local $ta i32)
  (local $term i32)
  (local $tt i32)
  (local $v i32)
  (local $wt i32)
  local.get $cx
  local.get $recurse0
  local.set $recurse
  local.get $field
  ;; project field: name
  local.get $field
  ;; project field: term
  local.get $recurse
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
  local.get $recurse
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
  local.get $recurse
  br $end_function
)
  local.set $for_function
  local.get $b
  ;; project field: name
  local.get $b
  ;; project field: term
  local.get $recurse
  local.get $b
  ;; project field: type
  local.set $map_binding
  local.get $map_binding
  local.get $lt
  ;; project field: bindings
  call $hydra.lib.lists.map
  local.get $lt
  ;; project field: body
  local.get $recurse
  local.set $for_let
  local.get $p
  call $hydra.lib.pairs.first
  local.get $recurse
  local.get $p
  call $hydra.lib.pairs.second
  local.get $recurse
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
  local.get $term
  br_table $annotated $application $either $function $let $list $literal $map $maybe $pair $record $set $type_application $type_lambda $union $unit $variable $wrap $wrap
)
  local.get $at
  ;; project field: body
  local.get $recurse
  local.get $at
  ;; project field: annotation
  br $end_term
)
  local.get $a
  ;; project field: function
  local.get $recurse
  local.get $a
  ;; project field: argument
  local.get $recurse
  br $end_term
)
  i32.const 0
  local.get $l
  local.get $recurse
  i32.const 1
  local.get $r
  local.get $recurse
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
  local.get $recurse
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
  local.get $recurse
  local.get $m
  call $hydra.lib.maybes.map
  br $end_term
)
  local.get $p
  call $hydra.lib.pairs.first
  local.get $recurse
  local.get $p
  call $hydra.lib.pairs.second
  local.get $recurse
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
  local.get $recurse
  local.get $s
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.sets.from_list
  br $end_term
)
  local.get $tt
  ;; project field: body
  local.get $recurse
  local.get $tt
  ;; project field: type
  br $end_term
)
  local.get $ta
  ;; project field: parameter
  local.get $ta
  ;; project field: body
  local.get $recurse
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
  local.get $recurse
  br $end_term
)
  local.set $for_subterms
  local.get $rewrite
  local.get $for_subterms
  local.get $cx
  local.get $term
  local.get $f
  local.set $rewrite
  local.get $cx0
  local.get $term0
  local.get $rewrite
)
  (func $hydra.rewriting.rewrite_term_with_context_m (param $f i32) (param $cx0 i32) (param $term0 i32) (result i32)
  (local $app i32)
  (local $at i32)
  (local $b i32)
  (local $bindings i32)
  (local $body i32)
  (local $cases i32)
  (local $cs i32)
  (local $cx i32)
  (local $d i32)
  (local $def i32)
  (local $e i32)
  (local $els i32)
  (local $ex i32)
  (local $field i32)
  (local $fields i32)
  (local $for_elimination i32)
  (local $for_field i32)
  (local $for_function i32)
  (local $for_pair i32)
  (local $for_subterms i32)
  (local $fun i32)
  (local $i i32)
  (local $k i32)
  (local $kv i32)
  (local $l i32)
  (local $lhs i32)
  (local $lt i32)
  (local $m i32)
  (local $map_binding i32)
  (local $n i32)
  (local $name i32)
  (local $p i32)
  (local $pairs i32)
  (local $r i32)
  (local $rbindings i32)
  (local $rbody i32)
  (local $rcases i32)
  (local $rdef i32)
  (local $re i32)
  (local $recurse i32)
  (local $recurse0 i32)
  (local $rels i32)
  (local $rewrite i32)
  (local $rfield i32)
  (local $rfields i32)
  (local $rfirst i32)
  (local $rfun i32)
  (local $rhs i32)
  (local $rlist i32)
  (local $rm i32)
  (local $rsecond i32)
  (local $rt i32)
  (local $s i32)
  (local $t i32)
  (local $term i32)
  (local $tl i32)
  (local $tt i32)
  (local $v i32)
  (local $wt i32)
  (local $x i32)
  local.get $cx
  local.get $recurse0
  local.set $recurse
  local.get $field
  ;; project field: term
  local.get $recurse
  i32.const 1
  local.get $field
  ;; project field: name
  local.get $t
  call $hydra.lib.eithers.bind
  local.set $for_field
  local.get $kv
  call $hydra.lib.pairs.first
  local.get $recurse
  local.get $kv
  call $hydra.lib.pairs.second
  local.get $recurse
  i32.const 1
  local.get $k
  local.get $v
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  local.set $for_pair
  (block $end_elimination (result i32)
  (block $wrap
  (block $union
  (block $record
  local.get $e
  br_table $record $union $wrap $wrap
)
  i32.const 1
  local.get $p
  br $end_elimination
)
  local.get $cs
  ;; project field: type_name
  local.set $n
  local.get $cs
  ;; project field: default
  local.set $def
  local.get $cs
  ;; project field: cases
  local.set $cases
  i32.const 1
  i32.const 0
  call $hydra.lib.maybes.pure
  local.get $t
  local.get $recurse
  call $hydra.lib.eithers.map
  local.get $def
  call $hydra.lib.maybes.maybe
  local.get $n
  local.get $rdef
  local.get $rcases
  local.get $for_field
  local.get $cases
  call $hydra.lib.eithers.map_list
  call $hydra.lib.eithers.map
  call $hydra.lib.eithers.bind
  br $end_elimination
)
  i32.const 1
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
  local.get $e
  local.get $for_elimination
  br $end_function
)
  local.get $l
  ;; project field: parameter
  local.set $v
  local.get $l
  ;; project field: domain
  local.set $d
  local.get $l
  ;; project field: body
  local.set $body
  local.get $body
  local.get $recurse
  i32.const 1
  local.get $v
  local.get $d
  local.get $rbody
  call $hydra.lib.eithers.bind
  br $end_function
)
  local.set $for_function
  local.get $b
  ;; project field: term
  local.get $recurse
  i32.const 1
  local.get $b
  ;; project field: name
  local.get $v
  local.get $b
  ;; project field: type
  call $hydra.lib.eithers.bind
  local.set $map_binding
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
  local.get $at
  ;; project field: body
  local.get $recurse
  i32.const 1
  local.get $ex
  local.get $at
  ;; project field: annotation
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $app
  ;; project field: function
  local.get $recurse
  local.get $app
  ;; project field: argument
  local.get $recurse
  i32.const 1
  local.get $lhs
  local.get $rhs
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  i32.const 0
  local.get $x
  local.get $l
  local.get $recurse
  call $hydra.lib.eithers.map
  i32.const 1
  local.get $x
  local.get $r
  local.get $recurse
  call $hydra.lib.eithers.map
  local.get $e
  call $hydra.lib.eithers.either
  i32.const 1
  local.get $re
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $fun
  local.get $for_function
  i32.const 1
  local.get $rfun
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $lt
  ;; project field: bindings
  local.set $bindings
  local.get $lt
  ;; project field: body
  local.set $body
  local.get $map_binding
  local.get $bindings
  call $hydra.lib.eithers.map_list
  local.get $body
  local.get $recurse
  i32.const 1
  local.get $rbindings
  local.get $rbody
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $recurse
  local.get $els
  call $hydra.lib.eithers.map_list
  i32.const 1
  local.get $rels
  call $hydra.lib.eithers.bind
  br $end_term
)
  i32.const 1
  local.get $v
  br $end_term
)
  local.get $for_pair
  local.get $m
  call $hydra.lib.maps.to_list
  call $hydra.lib.eithers.map_list
  i32.const 1
  local.get $pairs
  call $hydra.lib.maps.from_list
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $recurse
  local.get $m
  call $hydra.lib.eithers.map_maybe
  i32.const 1
  local.get $rm
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $p
  call $hydra.lib.pairs.first
  local.get $recurse
  local.get $p
  call $hydra.lib.pairs.second
  local.get $recurse
  i32.const 1
  local.get $rfirst
  local.get $rsecond
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $r
  ;; project field: type_name
  local.set $n
  local.get $r
  ;; project field: fields
  local.set $fields
  local.get $n
  local.get $rfields
  local.get $for_field
  local.get $fields
  call $hydra.lib.eithers.map_list
  call $hydra.lib.eithers.map
  br $end_term
)
  local.get $recurse
  local.get $s
  call $hydra.lib.sets.to_list
  call $hydra.lib.eithers.map_list
  i32.const 1
  local.get $rlist
  call $hydra.lib.sets.from_list
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $tt
  ;; project field: body
  local.get $recurse
  i32.const 1
  local.get $t
  local.get $tt
  ;; project field: type
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $tl
  ;; project field: parameter
  local.set $v
  local.get $tl
  ;; project field: body
  local.set $body
  local.get $body
  local.get $recurse
  i32.const 1
  local.get $v
  local.get $rbody
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $i
  ;; project field: type_name
  local.set $n
  local.get $i
  ;; project field: field
  local.set $field
  local.get $n
  local.get $rfield
  local.get $field
  local.get $for_field
  call $hydra.lib.eithers.map
  br $end_term
)
  i32.const 1
  i32.const 0
  br $end_term
)
  i32.const 1
  local.get $v
  br $end_term
)
  local.get $wt
  ;; project field: type_name
  local.set $name
  local.get $wt
  ;; project field: body
  local.set $t
  local.get $t
  local.get $recurse
  i32.const 1
  local.get $name
  local.get $rt
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.set $for_subterms
  local.get $rewrite
  local.get $for_subterms
  local.get $cx
  local.get $term
  local.get $f
  local.set $rewrite
  local.get $cx0
  local.get $term0
  local.get $rewrite
)
  (func $hydra.rewriting.rewrite_term_with_graph (param $f i32) (param $cx0 i32) (param $term0 i32) (result i32)
  (local $cx i32)
  (local $cx1 i32)
  (local $f2 i32)
  (local $fun i32)
  (local $l i32)
  (local $recurse i32)
  (local $recurse1 i32)
  (local $recurse2 i32)
  (local $rewrite i32)
  (local $term i32)
  (local $term2 i32)
  (local $tl i32)
  local.get $cx
  local.get $term2
  local.get $recurse
  local.set $recurse1
  (block $end_term (result i32)
  (block $type_lambda
  (block $let
  (block $function
  local.get $term
  br_table $function $let $type_lambda $type_lambda
)
  (block $end_function (result i32)
  (block $lambda
  local.get $fun
  br_table $lambda $lambda
)
  local.get $cx
  local.get $l
  call $hydra.scoping.extend_graph_for_lambda
  local.set $cx1
  local.get $cx1
  local.get $term2
  local.get $recurse
  local.set $recurse2
  local.get $recurse2
  local.get $cx1
  local.get $term
  local.get $f
  br $end_function
)
  br $end_term
)
  i32.const 0
  local.get $cx
  local.get $l
  call $hydra.scoping.extend_graph_for_let
  local.set $cx1
  local.get $cx1
  local.get $term2
  local.get $recurse
  local.set $recurse2
  local.get $recurse2
  local.get $cx1
  local.get $term
  local.get $f
  br $end_term
)
  local.get $cx
  local.get $tl
  call $hydra.scoping.extend_graph_for_type_lambda
  local.set $cx1
  local.get $cx1
  local.get $term2
  local.get $recurse
  local.set $recurse2
  local.get $recurse2
  local.get $cx1
  local.get $term
  local.get $f
  br $end_term
)
  local.set $f2
  local.get $rewrite
  local.get $cx
  local.get $term
  local.get $f2
  local.set $rewrite
  local.get $cx0
  local.get $term0
  local.get $rewrite
)
  (func $hydra.rewriting.rewrite_type (param $f i32) (param $typ0 i32) (result i32)
  (local $app i32)
  (local $at i32)
  (local $et i32)
  (local $field i32)
  (local $for_field i32)
  (local $fsub i32)
  (local $fun i32)
  (local $lt i32)
  (local $mt i32)
  (local $pt i32)
  (local $recurse i32)
  (local $rt i32)
  (local $t i32)
  (local $typ i32)
  (local $v i32)
  (local $wt i32)
  local.get $field
  ;; project field: name
  local.get $field
  ;; project field: type
  local.get $recurse
  local.set $for_field
  (block $end_type (result i32)
  (block $wrap
  (block $void
  (block $variable
  (block $unit
  (block $union
  (block $set
  (block $record
  (block $maybe
  (block $map
  (block $literal
  (block $list
  (block $forall
  (block $function
  (block $pair
  (block $either
  (block $application
  (block $annotated
  local.get $typ
  br_table $annotated $application $either $pair $function $forall $list $literal $map $maybe $record $set $union $unit $variable $void $wrap $wrap
)
  local.get $at
  ;; project field: body
  local.get $recurse
  local.get $at
  ;; project field: annotation
  br $end_type
)
  local.get $app
  ;; project field: function
  local.get $recurse
  local.get $app
  ;; project field: argument
  local.get $recurse
  br $end_type
)
  local.get $et
  ;; project field: left
  local.get $recurse
  local.get $et
  ;; project field: right
  local.get $recurse
  br $end_type
)
  local.get $pt
  ;; project field: first
  local.get $recurse
  local.get $pt
  ;; project field: second
  local.get $recurse
  br $end_type
)
  local.get $fun
  ;; project field: domain
  local.get $recurse
  local.get $fun
  ;; project field: codomain
  local.get $recurse
  br $end_type
)
  local.get $lt
  ;; project field: parameter
  local.get $lt
  ;; project field: body
  local.get $recurse
  br $end_type
)
  local.get $t
  local.get $recurse
  br $end_type
)
  local.get $lt
  br $end_type
)
  local.get $mt
  ;; project field: keys
  local.get $recurse
  local.get $mt
  ;; project field: values
  local.get $recurse
  br $end_type
)
  local.get $t
  local.get $recurse
  br $end_type
)
  local.get $for_field
  local.get $rt
  call $hydra.lib.lists.map
  br $end_type
)
  local.get $t
  local.get $recurse
  br $end_type
)
  local.get $for_field
  local.get $rt
  call $hydra.lib.lists.map
  br $end_type
)
  i32.const 0
  br $end_type
)
  local.get $v
  br $end_type
)
  i32.const 0
  br $end_type
)
  local.get $wt
  local.get $recurse
  br $end_type
)
  local.set $fsub
  local.get $recurse
  local.get $fsub
  local.get $f
  local.set $recurse
  local.get $typ0
  local.get $recurse
)
  (func $hydra.rewriting.rewrite_type_m (param $f i32) (param $typ0 i32) (result i32)
  (local $at i32)
  (local $b i32)
  (local $cod i32)
  (local $dom i32)
  (local $et i32)
  (local $f2 i32)
  (local $for_field i32)
  (local $fsub i32)
  (local $ft i32)
  (local $kt i32)
  (local $left i32)
  (local $lhs i32)
  (local $lt i32)
  (local $mt i32)
  (local $pair_first i32)
  (local $pair_second i32)
  (local $pt i32)
  (local $recurse i32)
  (local $rfields i32)
  (local $rhs i32)
  (local $right i32)
  (local $rt i32)
  (local $t i32)
  (local $typ i32)
  (local $v i32)
  (local $vt i32)
  (local $wt i32)
  (block $end_type (result i32)
  (block $wrap
  (block $void
  (block $variable
  (block $unit
  (block $union
  (block $set
  (block $record
  (block $maybe
  (block $map
  (block $literal
  (block $list
  (block $forall
  (block $function
  (block $pair
  (block $either
  (block $application
  (block $annotated
  local.get $typ
  br_table $annotated $application $either $pair $function $forall $list $literal $map $maybe $record $set $union $unit $variable $void $wrap $wrap
)
  local.get $at
  ;; project field: body
  local.get $recurse
  i32.const 1
  local.get $t
  local.get $at
  ;; project field: annotation
  call $hydra.lib.eithers.bind
  br $end_type
)
  local.get $at
  ;; project field: function
  local.get $recurse
  local.get $at
  ;; project field: argument
  local.get $recurse
  i32.const 1
  local.get $lhs
  local.get $rhs
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_type
)
  local.get $et
  ;; project field: left
  local.get $recurse
  local.get $et
  ;; project field: right
  local.get $recurse
  i32.const 1
  local.get $left
  local.get $right
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_type
)
  local.get $pt
  ;; project field: first
  local.get $recurse
  local.get $pt
  ;; project field: second
  local.get $recurse
  i32.const 1
  local.get $pair_first
  local.get $pair_second
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_type
)
  local.get $ft
  ;; project field: domain
  local.get $recurse
  local.get $ft
  ;; project field: codomain
  local.get $recurse
  i32.const 1
  local.get $dom
  local.get $cod
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_type
)
  local.get $ft
  ;; project field: body
  local.get $recurse
  i32.const 1
  local.get $ft
  ;; project field: parameter
  local.get $b
  call $hydra.lib.eithers.bind
  br $end_type
)
  local.get $t
  local.get $recurse
  i32.const 1
  local.get $rt
  call $hydra.lib.eithers.bind
  br $end_type
)
  i32.const 1
  local.get $lt
  br $end_type
)
  local.get $mt
  ;; project field: keys
  local.get $recurse
  local.get $mt
  ;; project field: values
  local.get $recurse
  i32.const 1
  local.get $kt
  local.get $vt
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_type
)
  local.get $t
  local.get $recurse
  i32.const 1
  local.get $rt
  call $hydra.lib.eithers.bind
  br $end_type
)
  local.get $f2
  ;; project field: type
  local.get $recurse
  i32.const 1
  local.get $f2
  ;; project field: name
  local.get $t
  call $hydra.lib.eithers.bind
  local.set $for_field
  local.get $for_field
  local.get $rt
  call $hydra.lib.eithers.map_list
  i32.const 1
  local.get $rfields
  call $hydra.lib.eithers.bind
  br $end_type
)
  local.get $t
  local.get $recurse
  i32.const 1
  local.get $rt
  call $hydra.lib.eithers.bind
  br $end_type
)
  local.get $f2
  ;; project field: type
  local.get $recurse
  i32.const 1
  local.get $f2
  ;; project field: name
  local.get $t
  call $hydra.lib.eithers.bind
  local.set $for_field
  local.get $for_field
  local.get $rt
  call $hydra.lib.eithers.map_list
  i32.const 1
  local.get $rfields
  call $hydra.lib.eithers.bind
  br $end_type
)
  i32.const 1
  i32.const 0
  br $end_type
)
  i32.const 1
  local.get $v
  br $end_type
)
  i32.const 1
  i32.const 0
  br $end_type
)
  local.get $wt
  local.get $recurse
  i32.const 1
  local.get $t
  call $hydra.lib.eithers.bind
  br $end_type
)
  local.set $fsub
  local.get $recurse
  local.get $fsub
  local.get $f
  local.set $recurse
  local.get $typ0
  local.get $recurse
)
  (func $hydra.rewriting.subterms (param $arg_0 i32) (result i32)
  (local $at i32)
  (local $cs i32)
  (local $e i32)
  (local $l i32)
  (local $lt i32)
  (local $m i32)
  (local $n i32)
  (local $p i32)
  (local $r i32)
  (local $rt i32)
  (local $t i32)
  (local $ta i32)
  (local $ut i32)
  (local $v i32)
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
  local.get $arg_0
  br_table $annotated $application $either $function $let $list $literal $map $maybe $pair $record $set $type_application $type_lambda $union $unit $variable $wrap $wrap
)
  i32.const 1
  ;; list elements follow
  local.get $at
  ;; project field: body
  br $end_term
)
  i32.const 2
  ;; list elements follow
  local.get $p
  ;; project field: function
  local.get $p
  ;; project field: argument
  br $end_term
)
  i32.const 1
  ;; list elements follow
  local.get $l
  i32.const 1
  ;; list elements follow
  local.get $r
  local.get $e
  call $hydra.lib.eithers.either
  br $end_term
)
  (block $end_function (result i32)
  (block $lambda
  (block $elimination
  local.get $v
  br_table $elimination $lambda $lambda
)
  (block $end_elimination (result i32)
  (block $union
  local.get $v
  br_table $union $union
)
  i32.const 0
  ;; list elements follow
  i32.const 1
  ;; list elements follow
  local.get $t
  local.get $cs
  ;; project field: default
  call $hydra.lib.maybes.maybe
  ;; project field: term
  local.get $cs
  ;; project field: cases
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat2
  br $end_elimination
)
  br $end_function
)
  i32.const 1
  ;; list elements follow
  local.get $l
  ;; project field: body
  br $end_function
)
  br $end_term
)
  local.get $lt
  ;; project field: body
  ;; project field: term
  local.get $lt
  ;; project field: bindings
  call $hydra.lib.lists.map
  call $hydra.lib.lists.cons
  br $end_term
)
  local.get $l
  br $end_term
)
  i32.const 0
  ;; list elements follow
  br $end_term
)
  i32.const 2
  ;; list elements follow
  local.get $p
  call $hydra.lib.pairs.first
  local.get $p
  call $hydra.lib.pairs.second
  local.get $m
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  br $end_term
)
  i32.const 0
  ;; list elements follow
  i32.const 1
  ;; list elements follow
  local.get $t
  local.get $m
  call $hydra.lib.maybes.maybe
  br $end_term
)
  i32.const 2
  ;; list elements follow
  local.get $p
  call $hydra.lib.pairs.first
  local.get $p
  call $hydra.lib.pairs.second
  br $end_term
)
  ;; project field: term
  local.get $rt
  ;; project field: fields
  call $hydra.lib.lists.map
  br $end_term
)
  local.get $l
  call $hydra.lib.sets.to_list
  br $end_term
)
  i32.const 1
  ;; list elements follow
  local.get $ta
  ;; project field: body
  br $end_term
)
  i32.const 1
  ;; list elements follow
  local.get $ta
  ;; project field: body
  br $end_term
)
  i32.const 1
  ;; list elements follow
  local.get $ut
  ;; project field: field
  ;; project field: term
  br $end_term
)
  i32.const 0
  ;; list elements follow
  br $end_term
)
  i32.const 0
  ;; list elements follow
  br $end_term
)
  i32.const 1
  ;; list elements follow
  local.get $n
  ;; project field: body
  br $end_term
)
)
  (func $hydra.rewriting.subterms_with_steps (param $arg_0 i32) (result i32)
  (local $at i32)
  (local $b i32)
  (local $cs i32)
  (local $e i32)
  (local $f i32)
  (local $l i32)
  (local $lt i32)
  (local $m i32)
  (local $n i32)
  (local $p i32)
  (local $rt i32)
  (local $s i32)
  (local $t i32)
  (local $ta i32)
  (local $ut i32)
  (local $v i32)
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
  local.get $arg_0
  br_table $annotated $application $either $function $let $list $literal $map $maybe $pair $record $set $type_application $type_lambda $union $unit $variable $wrap $wrap
)
  i32.const 1
  ;; list elements follow
  i32.const 0
  local.get $at
  ;; project field: body
  br $end_term
)
  i32.const 2
  ;; list elements follow
  i32.const 0
  local.get $p
  ;; project field: function
  i32.const 0
  local.get $p
  ;; project field: argument
  br $end_term
)
  i32.const 0
  ;; list elements follow
  br $end_term
)
  (block $end_function (result i32)
  (block $lambda
  (block $elimination
  local.get $v
  br_table $elimination $lambda $lambda
)
  (block $end_elimination (result i32)
  (block $union
  local.get $v
  br_table $union $union
)
  i32.const 0
  ;; list elements follow
  i32.const 1
  ;; list elements follow
  i32.const 0
  local.get $t
  local.get $cs
  ;; project field: default
  call $hydra.lib.maybes.maybe
  local.get $f
  ;; project field: name
  local.get $f
  ;; project field: term
  local.get $cs
  ;; project field: cases
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat2
  br $end_elimination
)
  br $end_function
)
  i32.const 1
  ;; list elements follow
  i32.const 0
  local.get $l
  ;; project field: body
  br $end_function
)
  br $end_term
)
  i32.const 0
  local.get $lt
  ;; project field: body
  local.get $b
  ;; project field: name
  local.get $b
  ;; project field: term
  local.get $lt
  ;; project field: bindings
  call $hydra.lib.lists.map
  call $hydra.lib.lists.cons
  br $end_term
)
  i32.const 0
  local.get $e
  local.get $l
  call $hydra.lib.lists.map
  br $end_term
)
  i32.const 0
  ;; list elements follow
  br $end_term
)
  i32.const 2
  ;; list elements follow
  i32.const 0
  local.get $p
  call $hydra.lib.pairs.first
  i32.const 0
  local.get $p
  call $hydra.lib.pairs.second
  local.get $m
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  br $end_term
)
  i32.const 0
  ;; list elements follow
  i32.const 1
  ;; list elements follow
  i32.const 0
  local.get $t
  local.get $m
  call $hydra.lib.maybes.maybe
  br $end_term
)
  i32.const 0
  ;; list elements follow
  br $end_term
)
  local.get $f
  ;; project field: name
  local.get $f
  ;; project field: term
  local.get $rt
  ;; project field: fields
  call $hydra.lib.lists.map
  br $end_term
)
  i32.const 0
  local.get $e
  local.get $s
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  br $end_term
)
  i32.const 1
  ;; list elements follow
  i32.const 0
  local.get $ta
  ;; project field: body
  br $end_term
)
  i32.const 1
  ;; list elements follow
  i32.const 0
  local.get $ta
  ;; project field: body
  br $end_term
)
  i32.const 1
  ;; list elements follow
  i32.const 0
  local.get $ut
  ;; project field: field
  ;; project field: term
  br $end_term
)
  i32.const 0
  ;; list elements follow
  br $end_term
)
  i32.const 0
  ;; list elements follow
  br $end_term
)
  i32.const 1
  ;; list elements follow
  i32.const 0
  local.get $n
  ;; project field: body
  br $end_term
)
)
  (func $hydra.rewriting.subtypes (param $arg_0 i32) (result i32)
  (local $at i32)
  (local $et i32)
  (local $ft i32)
  (local $lt i32)
  (local $mt i32)
  (local $nt i32)
  (local $ot i32)
  (local $pt i32)
  (local $rt i32)
  (local $st i32)
  (block $end_type (result i32)
  (block $wrap
  (block $void
  (block $variable
  (block $unit
  (block $union
  (block $set
  (block $record
  (block $maybe
  (block $map
  (block $literal
  (block $list
  (block $forall
  (block $function
  (block $pair
  (block $either
  (block $application
  (block $annotated
  local.get $arg_0
  br_table $annotated $application $either $pair $function $forall $list $literal $map $maybe $record $set $union $unit $variable $void $wrap $wrap
)
  i32.const 1
  ;; list elements follow
  local.get $at
  ;; project field: body
  br $end_type
)
  i32.const 2
  ;; list elements follow
  local.get $at
  ;; project field: function
  local.get $at
  ;; project field: argument
  br $end_type
)
  i32.const 2
  ;; list elements follow
  local.get $et
  ;; project field: left
  local.get $et
  ;; project field: right
  br $end_type
)
  i32.const 2
  ;; list elements follow
  local.get $pt
  ;; project field: first
  local.get $pt
  ;; project field: second
  br $end_type
)
  i32.const 2
  ;; list elements follow
  local.get $ft
  ;; project field: domain
  local.get $ft
  ;; project field: codomain
  br $end_type
)
  i32.const 1
  ;; list elements follow
  local.get $lt
  ;; project field: body
  br $end_type
)
  i32.const 1
  ;; list elements follow
  local.get $lt
  br $end_type
)
  i32.const 0
  ;; list elements follow
  br $end_type
)
  i32.const 2
  ;; list elements follow
  local.get $mt
  ;; project field: keys
  local.get $mt
  ;; project field: values
  br $end_type
)
  i32.const 1
  ;; list elements follow
  local.get $ot
  br $end_type
)
  ;; project field: type
  local.get $rt
  call $hydra.lib.lists.map
  br $end_type
)
  i32.const 1
  ;; list elements follow
  local.get $st
  br $end_type
)
  ;; project field: type
  local.get $rt
  call $hydra.lib.lists.map
  br $end_type
)
  i32.const 0
  ;; list elements follow
  br $end_type
)
  i32.const 0
  ;; list elements follow
  br $end_type
)
  i32.const 0
  ;; list elements follow
  br $end_type
)
  i32.const 1
  ;; list elements follow
  local.get $nt
  br $end_type
)
)
)
