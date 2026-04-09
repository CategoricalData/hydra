(module
  (import "hydra.inference" "hydra.inference.infer_in_graph_context" (func $hydra.inference.infer_in_graph_context (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.empty_context" (func $hydra.lexical.empty_context (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bimap" (func $hydra.lib.eithers.bimap (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (result i32) ) )
  (import "hydra.show.errors" "hydra.show.errors.error" (func $hydra.show.errors.error (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.test.utils.infer_term" (func $hydra.test.utils.infer_term) )
  (export "hydra.test.utils.infer_test_case" (func $hydra.test.utils.infer_test_case) )
  (export "hydra.test.utils.infer_test_group_terms" (func $hydra.test.utils.infer_test_group_terms) )
  (func $hydra.test.utils.infer_term (param $g i32) (param $term i32) (result i32)
  ;; project field: object
  call $hydra.show.errors.error
  ;; project field: term
  call $hydra.lexical.empty_context
  local.get $g
  local.get $term
  call $hydra.inference.infer_in_graph_context
  call $hydra.lib.eithers.bimap
)
  (func $hydra.test.utils.infer_test_case (param $g i32) (param $tcm i32) (result i32)
  (local $desc i32)
  (local $inferred_case i32)
  (local $name_ i32)
  (local $tags_ i32)
  (local $tcase i32)
  ;; project field: name
  local.set $name_
  ;; project field: case
  local.set $tcase
  ;; project field: description
  local.set $desc
  ;; project field: tags
  local.set $tags_
  local.get $name_
  local.get $inferred_case
  local.get $desc
  local.get $tags_
  i32.const 1
  local.get $tcase
  call $hydra.lib.eithers.map
)
  (func $hydra.test.utils.infer_test_group_terms (param $g i32) (param $tg i32) (result i32)
  (local $cases_ i32)
  (local $desc i32)
  (local $inferred_cases i32)
  (local $inferred_subgroups i32)
  (local $name_ i32)
  (local $sg i32)
  (local $subgroups i32)
  (local $tc i32)
  ;; project field: name
  local.set $name_
  ;; project field: description
  local.set $desc
  ;; project field: subgroups
  local.set $subgroups
  ;; project field: cases
  local.set $cases_
  local.get $g
  local.get $sg
  call $hydra.test.utils.infer_test_group_terms
  local.get $subgroups
  call $hydra.lib.eithers.map_list
  local.get $name_
  local.get $desc
  local.get $inferred_subgroups
  local.get $inferred_cases
  local.get $g
  local.get $tc
  call $hydra.test.utils.infer_test_case
  local.get $cases_
  call $hydra.lib.eithers.map_list
  call $hydra.lib.eithers.map
  call $hydra.lib.eithers.bind
)
)
