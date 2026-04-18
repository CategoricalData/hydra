(module
  (import "hydra.inference" "hydra.inference.infer_in_graph_context" (func $hydra.inference.infer_in_graph_context (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bimap" (func $hydra.lib.eithers.bimap (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (param i32) (result i32) ) )
  (import "hydra.show.errors" "hydra.show.errors.error" (func $hydra.show.errors.error (param i32) (result i32) ) )
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
  (export "hydra.test.utils.infer_term" (func $hydra.test.utils.infer_term) )
  (export "hydra.test.utils.infer_test_case" (func $hydra.test.utils.infer_test_case) )
  (export "hydra.test.utils.infer_test_group_terms" (func $hydra.test.utils.infer_test_group_terms) )
  (func $hydra.test.utils.infer_term (param $g i32) (param $term i32) (result i32)
  (local $e i32)
  (local $x i32)
  local.get $e
  call $hydra.show.errors.error
  local.get $x
  i32.load
  i32.const 0
  local.get $g
  local.get $term
  call $hydra.inference.infer_in_graph_context
  call $hydra.lib.eithers.bimap
)
  (func $hydra.test.utils.infer_test_case (param $g i32) (param $tcm i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $desc i32)
  (local $inferred_case i32)
  (local $name_ i32)
  (local $tags_ i32)
  (local $tcase i32)
  local.get $tcm
  i32.load
  local.set $name_
  local.get $tcm
  i32.load offset=4
  local.set $tcase
  local.get $tcm
  i32.load offset=8
  local.set $desc
  local.get $tcm
  i32.load offset=12
  local.set $tags_
  local.get $name_
  local.get $inferred_case
  local.get $desc
  local.get $tags_
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
  i32.const 1
  local.get $tcase
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  call $hydra.lib.eithers.map
)
  (func $hydra.test.utils.infer_test_group_terms (param $g i32) (param $tg i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $cases_ i32)
  (local $desc i32)
  (local $inferred_cases i32)
  (local $inferred_subgroups i32)
  (local $name_ i32)
  (local $sg i32)
  (local $subgroups i32)
  (local $tc i32)
  local.get $tg
  i32.load
  local.set $name_
  local.get $tg
  i32.load offset=4
  local.set $desc
  local.get $tg
  i32.load offset=8
  local.set $subgroups
  local.get $tg
  i32.load offset=12
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
  local.get $g
  local.get $tc
  call $hydra.test.utils.infer_test_case
  local.get $cases_
  call $hydra.lib.eithers.map_list
  call $hydra.lib.eithers.map
  call $hydra.lib.eithers.bind
)
)
