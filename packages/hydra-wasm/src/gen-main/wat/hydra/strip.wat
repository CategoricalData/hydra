(module
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_term" (func $hydra.rewriting.rewrite_term (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_type" (func $hydra.rewriting.rewrite_type (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
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
  (local $at i32)
  (local $ta i32)
  (local $tt i32)
  (block $end_term (result i32)
  (block $type_lambda
  (block $type_application
  (block $annotated
  local.get $t
  br_table $annotated $type_application $type_lambda $type_lambda
)
  local.get $at
  ;; project field: body
  call $hydra.strip.deannotate_and_detype_term
  br $end_term
)
  local.get $tt
  ;; project field: body
  call $hydra.strip.deannotate_and_detype_term
  br $end_term
)
  local.get $ta
  ;; project field: body
  call $hydra.strip.deannotate_and_detype_term
  br $end_term
)
)
  (func $hydra.strip.deannotate_term (param $t i32) (result i32)
  (local $at i32)
  (block $end_term (result i32)
  (block $annotated
  local.get $t
  br_table $annotated $annotated
)
  local.get $at
  ;; project field: body
  call $hydra.strip.deannotate_term
  br $end_term
)
)
  (func $hydra.strip.deannotate_type (param $t i32) (result i32)
  (local $arg_ i32)
  (block $end_type (result i32)
  (block $annotated
  local.get $t
  br_table $annotated $annotated
)
  local.get $arg_
  ;; project field: body
  call $hydra.strip.deannotate_type
  br $end_type
)
)
  (func $hydra.strip.deannotate_type_parameters (param $t i32) (result i32)
  (local $lt i32)
  (block $end_type (result i32)
  (block $forall
  local.get $t
  call $hydra.strip.deannotate_type
  br_table $forall $forall
)
  local.get $lt
  ;; project field: body
  call $hydra.strip.deannotate_type_parameters
  br $end_type
)
)
  (func $hydra.strip.deannotate_type_recursive (param $typ i32) (result i32)
  (local $at i32)
  (local $recurse i32)
  (local $rewritten i32)
  (local $strip i32)
  (local $typ2 i32)
  local.get $typ2
  local.get $recurse
  local.set $rewritten
  (block $end_type (result i32)
  (block $annotated
  local.get $rewritten
  br_table $annotated $annotated
)
  local.get $at
  ;; project field: body
  br $end_type
)
  local.set $strip
  local.get $strip
  local.get $typ
  call $hydra.rewriting.rewrite_type
)
  (func $hydra.strip.deannotate_type_scheme_recursive (param $ts i32) (result i32)
  (local $constraints i32)
  (local $typ i32)
  (local $vars i32)
  local.get $ts
  ;; project field: variables
  local.set $vars
  local.get $ts
  ;; project field: type
  local.set $typ
  local.get $ts
  ;; project field: constraints
  local.set $constraints
  local.get $vars
  local.get $typ
  call $hydra.strip.deannotate_type_recursive
  local.get $constraints
)
  (func $hydra.strip.detype_term (param $t i32) (result i32)
  (local $ann i32)
  (local $at i32)
  (local $subj i32)
  (local $ta i32)
  (local $tt i32)
  (block $end_term (result i32)
  (block $type_lambda
  (block $type_application
  (block $annotated
  local.get $t
  br_table $annotated $type_application $type_lambda $type_lambda
)
  local.get $at
  ;; project field: body
  local.set $subj
  local.get $at
  ;; project field: annotation
  local.set $ann
  local.get $subj
  call $hydra.strip.detype_term
  local.get $ann
  br $end_term
)
  local.get $tt
  ;; project field: body
  call $hydra.strip.deannotate_and_detype_term
  br $end_term
)
  local.get $ta
  ;; project field: body
  call $hydra.strip.deannotate_and_detype_term
  br $end_term
)
)
  (func $hydra.strip.remove_term_annotations (param $term i32) (result i32)
  (local $at i32)
  (local $recurse i32)
  (local $remove i32)
  (local $rewritten i32)
  (local $term2 i32)
  local.get $term2
  local.get $recurse
  local.set $rewritten
  (block $end_term (result i32)
  (block $annotated
  local.get $term2
  br_table $annotated $annotated
)
  local.get $at
  ;; project field: body
  br $end_term
)
  local.set $remove
  local.get $remove
  local.get $term
  call $hydra.rewriting.rewrite_term
)
  (func $hydra.strip.remove_type_annotations (param $typ i32) (result i32)
  (local $at i32)
  (local $recurse i32)
  (local $remove i32)
  (local $rewritten i32)
  (local $typ2 i32)
  local.get $typ2
  local.get $recurse
  local.set $rewritten
  (block $end_type (result i32)
  (block $annotated
  local.get $rewritten
  br_table $annotated $annotated
)
  local.get $at
  ;; project field: body
  br $end_type
)
  local.set $remove
  local.get $remove
  local.get $typ
  call $hydra.rewriting.rewrite_type
)
  (func $hydra.strip.remove_type_annotations_from_term (param $term i32) (result i32)
  (local $b i32)
  (local $lt i32)
  (local $recurse i32)
  (local $rewritten i32)
  (local $strip i32)
  (local $strip_binding i32)
  (local $ta i32)
  (local $term2 i32)
  (local $tt i32)
  local.get $term2
  local.get $recurse
  local.set $rewritten
  local.get $b
  ;; project field: name
  local.get $b
  ;; project field: term
  i32.const 0
  local.set $strip_binding
  (block $end_term (result i32)
  (block $type_lambda
  (block $type_application
  (block $let
  local.get $rewritten
  br_table $let $type_application $type_lambda $type_lambda
)
  local.get $strip_binding
  local.get $lt
  ;; project field: bindings
  call $hydra.lib.lists.map
  local.get $lt
  ;; project field: body
  br $end_term
)
  local.get $tt
  ;; project field: body
  br $end_term
)
  local.get $ta
  ;; project field: body
  br $end_term
)
  local.set $strip
  local.get $strip
  local.get $term
  call $hydra.rewriting.rewrite_term
)
  (func $hydra.strip.remove_types_from_term (param $term i32) (result i32)
  (local $b i32)
  (local $e i32)
  (local $f i32)
  (local $l i32)
  (local $lt i32)
  (local $recurse i32)
  (local $rewritten i32)
  (local $strip i32)
  (local $strip_binding i32)
  (local $ta i32)
  (local $term2 i32)
  (local $tt i32)
  local.get $term2
  local.get $recurse
  local.set $rewritten
  local.get $b
  ;; project field: name
  local.get $b
  ;; project field: term
  i32.const 0
  local.set $strip_binding
  (block $end_term (result i32)
  (block $type_lambda
  (block $type_application
  (block $let
  (block $function
  local.get $rewritten
  br_table $function $let $type_application $type_lambda $type_lambda
)
  (block $end_function (result i32)
  (block $lambda
  (block $elimination
  local.get $f
  br_table $elimination $lambda $lambda
)
  local.get $e
  br $end_function
)
  local.get $l
  ;; project field: parameter
  i32.const 0
  local.get $l
  ;; project field: body
  br $end_function
)
  br $end_term
)
  local.get $strip_binding
  local.get $lt
  ;; project field: bindings
  call $hydra.lib.lists.map
  local.get $lt
  ;; project field: body
  br $end_term
)
  local.get $tt
  ;; project field: body
  br $end_term
)
  local.get $ta
  ;; project field: body
  br $end_term
)
  local.set $strip
  local.get $strip
  local.get $term
  call $hydra.rewriting.rewrite_term
)
  (func $hydra.strip.strip_type_lambdas (param $t i32) (result i32)
  (local $ann i32)
  (local $at i32)
  (local $subj i32)
  (local $ta i32)
  (block $end_term (result i32)
  (block $type_lambda
  (block $annotated
  local.get $t
  br_table $annotated $type_lambda $type_lambda
)
  local.get $at
  ;; project field: body
  local.set $subj
  local.get $at
  ;; project field: annotation
  local.set $ann
  local.get $subj
  call $hydra.strip.strip_type_lambdas
  local.get $ann
  br $end_term
)
  local.get $ta
  ;; project field: body
  call $hydra.strip.strip_type_lambdas
  br $end_term
)
)
)
