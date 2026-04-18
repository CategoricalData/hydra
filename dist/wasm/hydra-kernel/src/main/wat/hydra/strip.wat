(module
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_term" (func $hydra.rewriting.rewrite_term (param i32) (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_type" (func $hydra.rewriting.rewrite_type (param i32) (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "")
  (global $__bump_ptr (mut i32) i32.const 1024 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.strip.deannotate_and_detype_term" (func $hydra.strip.deannotate_and_detype_term) )
  (export "hydra.strip.deannotate_term" (func $hydra.strip.deannotate_term) )
  (export "hydra.strip.deannotate_type" (func $hydra.strip.deannotate_type) )
  (export "hydra.strip.deannotate_type_parameters" (func $hydra.strip.deannotate_type_parameters) )
  (export "hydra.strip.deannotate_type_recursive" (func $hydra.strip.deannotate_type_recursive) )
  (export "hydra.strip.deannotate_type_scheme_recursive" (func $hydra.strip.deannotate_type_scheme_recursive) )
  (export "hydra.strip.detype_term" (func $hydra.strip.detype_term) )
  (export "hydra.strip.remove_term_annotations" (func $hydra.strip.remove_term_annotations) )
  (export "hydra.strip.remove_type_annotations" (func $hydra.strip.remove_type_annotations) )
  (export "hydra.strip.remove_type_annotations_from_term" (func $hydra.strip.remove_type_annotations_from_term) )
  (export "hydra.strip.remove_types_from_term" (func $hydra.strip.remove_types_from_term) )
  (export "hydra.strip.strip_type_lambdas" (func $hydra.strip.strip_type_lambdas) )
  (func $hydra.strip.deannotate_and_detype_term (param $t i32) (result i32)
  (local $__rec_ptr i32)
  (local $at i32)
  (local $ta i32)
  (local $tt i32)
  (local $v i32)
  (block $end_term (result i32)
  (block $type_lambda
  (block $type_application
  (block $annotated
  local.get $t
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $type_application $type_lambda $type_lambda
)
  local.get $v
  drop
  local.get $at
  i32.load
  call $hydra.strip.deannotate_and_detype_term
  br $end_term
)
  local.get $v
  drop
  local.get $tt
  i32.load
  call $hydra.strip.deannotate_and_detype_term
  br $end_term
)
  local.get $v
  drop
  local.get $ta
  i32.load offset=4
  call $hydra.strip.deannotate_and_detype_term
  br $end_term
)
)
  (func $hydra.strip.deannotate_term (param $t i32) (result i32)
  (local $__rec_ptr i32)
  (local $at i32)
  (local $v i32)
  (block $end_term (result i32)
  (block $annotated
  local.get $t
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $annotated
)
  local.get $v
  drop
  local.get $at
  i32.load
  call $hydra.strip.deannotate_term
  br $end_term
)
)
  (func $hydra.strip.deannotate_type (param $t i32) (result i32)
  (local $__rec_ptr i32)
  (local $arg_ i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $annotated
  local.get $t
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $annotated
)
  local.get $v
  drop
  local.get $arg_
  i32.load
  call $hydra.strip.deannotate_type
  br $end_type
)
)
  (func $hydra.strip.deannotate_type_parameters (param $t i32) (result i32)
  (local $__rec_ptr i32)
  (local $lt i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $forall
  local.get $t
  call $hydra.strip.deannotate_type
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
  local.get $lt
  i32.load offset=4
  call $hydra.strip.deannotate_type_parameters
  br $end_type
)
)
  (func $hydra.strip.deannotate_type_recursive (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $at i32)
  (local $recurse i32)
  (local $rewritten i32)
  (local $strip i32)
  (local $typ2 i32)
  (local $v i32)
  local.get $typ2
  drop
  local.get $recurse
  drop
  i32.const 0
  local.set $rewritten
  (block $end_type (result i32)
  (block $annotated
  local.get $rewritten
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $annotated
)
  local.get $v
  drop
  local.get $at
  i32.load
  br $end_type
)
  local.set $strip
  local.get $strip
  local.get $typ
  call $hydra.rewriting.rewrite_type
)
  (func $hydra.strip.deannotate_type_scheme_recursive (param $ts i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $constraints i32)
  (local $typ i32)
  (local $vars i32)
  local.get $ts
  i32.load
  local.set $vars
  local.get $ts
  i32.load offset=4
  local.set $typ
  local.get $ts
  i32.load offset=8
  local.set $constraints
  local.get $vars
  local.get $typ
  call $hydra.strip.deannotate_type_recursive
  local.get $constraints
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
)
  (func $hydra.strip.detype_term (param $t i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $ann i32)
  (local $at i32)
  (local $subj i32)
  (local $ta i32)
  (local $tt i32)
  (local $v i32)
  (block $end_term (result i32)
  (block $type_lambda
  (block $type_application
  (block $annotated
  local.get $t
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $type_application $type_lambda $type_lambda
)
  local.get $v
  drop
  local.get $at
  i32.load
  local.set $subj
  local.get $at
  i32.load offset=4
  local.set $ann
  i32.const 0
  local.get $subj
  call $hydra.strip.detype_term
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
  br $end_term
)
  local.get $v
  drop
  local.get $tt
  i32.load
  call $hydra.strip.deannotate_and_detype_term
  br $end_term
)
  local.get $v
  drop
  local.get $ta
  i32.load offset=4
  call $hydra.strip.deannotate_and_detype_term
  br $end_term
)
)
  (func $hydra.strip.remove_term_annotations (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $at i32)
  (local $recurse i32)
  (local $remove i32)
  (local $rewritten i32)
  (local $term2 i32)
  (local $v i32)
  local.get $term2
  drop
  local.get $recurse
  drop
  i32.const 0
  local.set $rewritten
  (block $end_term (result i32)
  (block $annotated
  local.get $term2
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $annotated
)
  local.get $v
  drop
  local.get $at
  i32.load
  br $end_term
)
  local.set $remove
  local.get $remove
  local.get $term
  call $hydra.rewriting.rewrite_term
)
  (func $hydra.strip.remove_type_annotations (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $at i32)
  (local $recurse i32)
  (local $remove i32)
  (local $rewritten i32)
  (local $typ2 i32)
  (local $v i32)
  local.get $typ2
  drop
  local.get $recurse
  drop
  i32.const 0
  local.set $rewritten
  (block $end_type (result i32)
  (block $annotated
  local.get $rewritten
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $annotated
)
  local.get $v
  drop
  local.get $at
  i32.load
  br $end_type
)
  local.set $remove
  local.get $remove
  local.get $typ
  call $hydra.rewriting.rewrite_type
)
  (func $hydra.strip.remove_type_annotations_from_term (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (local $lt i32)
  (local $recurse i32)
  (local $rewritten i32)
  (local $strip i32)
  (local $strip_binding i32)
  (local $ta i32)
  (local $term2 i32)
  (local $tt i32)
  (local $v i32)
  local.get $term2
  drop
  local.get $recurse
  drop
  i32.const 0
  local.set $rewritten
  local.get $b
  i32.load
  local.get $b
  i32.load offset=4
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
  local.set $strip_binding
  (block $end_term (result i32)
  (block $type_lambda
  (block $type_application
  (block $let
  local.get $rewritten
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $let $type_application $type_lambda $type_lambda
)
  local.get $v
  drop
  i32.const 6
  local.get $strip_binding
  local.get $lt
  i32.load
  call $hydra.lib.lists.map
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
  br $end_term
)
  local.get $v
  drop
  local.get $tt
  i32.load
  br $end_term
)
  local.get $v
  drop
  local.get $ta
  i32.load offset=4
  br $end_term
)
  local.set $strip
  local.get $strip
  local.get $term
  call $hydra.rewriting.rewrite_term
)
  (func $hydra.strip.remove_types_from_term (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (local $l i32)
  (local $lt i32)
  (local $recurse i32)
  (local $rewritten i32)
  (local $strip i32)
  (local $strip_binding i32)
  (local $ta i32)
  (local $term2 i32)
  (local $tt i32)
  (local $v i32)
  local.get $term2
  drop
  local.get $recurse
  drop
  i32.const 0
  local.set $rewritten
  local.get $b
  i32.load
  local.get $b
  i32.load offset=4
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
  local.set $strip_binding
  (block $end_term (result i32)
  (block $type_lambda
  (block $type_application
  (block $let
  (block $lambda
  local.get $rewritten
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
  i32.const 5
  local.get $l
  i32.load
  i32.const 0
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
  br $end_term
)
  local.get $v
  drop
  i32.const 6
  local.get $strip_binding
  local.get $lt
  i32.load
  call $hydra.lib.lists.map
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
  br $end_term
)
  local.get $v
  drop
  local.get $tt
  i32.load
  br $end_term
)
  local.get $v
  drop
  local.get $ta
  i32.load offset=4
  br $end_term
)
  local.set $strip
  local.get $strip
  local.get $term
  call $hydra.rewriting.rewrite_term
)
  (func $hydra.strip.strip_type_lambdas (param $t i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $ann i32)
  (local $at i32)
  (local $subj i32)
  (local $ta i32)
  (local $v i32)
  (block $end_term (result i32)
  (block $type_lambda
  (block $annotated
  local.get $t
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $type_lambda $type_lambda
)
  local.get $v
  drop
  local.get $at
  i32.load
  local.set $subj
  local.get $at
  i32.load offset=4
  local.set $ann
  i32.const 0
  local.get $subj
  call $hydra.strip.strip_type_lambdas
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
  br $end_term
)
  local.get $v
  drop
  local.get $ta
  i32.load offset=4
  call $hydra.strip.strip_type_lambdas
  br $end_term
)
)
)
