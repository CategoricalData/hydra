(module
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.encode.testing.tag" (func $hydra.encode.testing.tag) )
  (export "hydra.encode.testing.test_case" (func $hydra.encode.testing.test_case) )
  (export "hydra.encode.testing.test_case_with_metadata" (func $hydra.encode.testing.test_case_with_metadata) )
  (export "hydra.encode.testing.test_group" (func $hydra.encode.testing.test_group) )
  (export "hydra.encode.testing.universal_test_case" (func $hydra.encode.testing.universal_test_case) )
  (func $hydra.encode.testing.tag (param $x i32) (result i32)
  (local $x2 i32)
  i32.const 0 ;; string: "hydra.testing.Tag"
  local.get $x2
)
  (func $hydra.encode.testing.test_case (param $arg_0 i32) (result i32)
  (local $y i32)
  (block $end_test_case (result i32)
  (block $universal
  local.get $arg_0
  br_table $universal $universal
)
  i32.const 0 ;; string: "hydra.testing.TestCase"
  i32.const 0 ;; string: "universal"
  local.get $y
  call $hydra.encode.testing.universal_test_case
  br $end_test_case
)
)
  (func $hydra.encode.testing.test_case_with_metadata (param $x i32) (result i32)
  (local $opt i32)
  (local $x2 i32)
  (local $xs i32)
  i32.const 0 ;; string: "hydra.testing.TestCaseWithMetadata"
  i32.const 4
  ;; list elements follow
  i32.const 0 ;; string: "name"
  local.get $x2
  i32.const 0 ;; string: "case"
  local.get $x
  ;; project field: case
  call $hydra.encode.testing.test_case
  i32.const 0 ;; string: "description"
  local.get $x2
  local.get $opt
  call $hydra.lib.maybes.map
  i32.const 0 ;; string: "tags"
  call $hydra.encode.testing.tag
  local.get $xs
  call $hydra.lib.lists.map
)
  (func $hydra.encode.testing.test_group (param $x i32) (result i32)
  (local $opt i32)
  (local $x2 i32)
  (local $xs i32)
  i32.const 0 ;; string: "hydra.testing.TestGroup"
  i32.const 4
  ;; list elements follow
  i32.const 0 ;; string: "name"
  local.get $x2
  i32.const 0 ;; string: "description"
  local.get $x2
  local.get $opt
  call $hydra.lib.maybes.map
  i32.const 0 ;; string: "subgroups"
  call $hydra.encode.testing.test_group
  local.get $xs
  call $hydra.lib.lists.map
  i32.const 0 ;; string: "cases"
  call $hydra.encode.testing.test_case_with_metadata
  local.get $xs
  call $hydra.lib.lists.map
)
  (func $hydra.encode.testing.universal_test_case (param $x i32) (result i32)
  (local $x2 i32)
  i32.const 0 ;; string: "hydra.testing.UniversalTestCase"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "actual"
  local.get $x2
  i32.const 0 ;; string: "expected"
  local.get $x2
)
)
