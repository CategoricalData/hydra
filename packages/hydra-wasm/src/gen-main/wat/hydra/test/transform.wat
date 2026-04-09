(module
  (import "hydra.lib.eithers" "hydra.lib.eithers.bimap" (func $hydra.lib.eithers.bimap (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat" (func $hydra.lib.lists.concat (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.and" (func $hydra.lib.logic.and (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cat" (func $hydra.lib.maybes.cat (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.test.transform.add_generation_prefix" (func $hydra.test.transform.add_generation_prefix) )
  (export "hydra.test.transform.build_convert_case_call" (func $hydra.test.transform.build_convert_case_call) )
  (export "hydra.test.transform.build_topological_sort_call" (func $hydra.test.transform.build_topological_sort_call) )
  (export "hydra.test.transform.build_topological_sort_s_c_c_call" (func $hydra.test.transform.build_topological_sort_s_c_c_call) )
  (export "hydra.test.transform.collect_test_cases" (func $hydra.test.transform.collect_test_cases) )
  (export "hydra.test.transform.encode_adjacency_list" (func $hydra.test.transform.encode_adjacency_list) )
  (export "hydra.test.transform.encode_case_convention" (func $hydra.test.transform.encode_case_convention) )
  (export "hydra.test.transform.encode_either_list_list" (func $hydra.test.transform.encode_either_list_list) )
  (export "hydra.test.transform.encode_int" (func $hydra.test.transform.encode_int) )
  (export "hydra.test.transform.encode_int_list" (func $hydra.test.transform.encode_int_list) )
  (export "hydra.test.transform.encode_list_list" (func $hydra.test.transform.encode_list_list) )
  (export "hydra.test.transform.transform_module" (func $hydra.test.transform.transform_module) )
  (export "hydra.test.transform.transform_test_case" (func $hydra.test.transform.transform_test_case) )
  (export "hydra.test.transform.transform_to_compiled_tests" (func $hydra.test.transform.transform_to_compiled_tests) )
  (func $hydra.test.transform.add_generation_prefix (param $ns_ i32) (result i32)
  i32.const 0 ;; string: "generation."
  nop
  call $hydra.lib.strings.cat2
)
  (func $hydra.test.transform.build_convert_case_call (param $from_conv i32) (param $to_conv i32) (param $input_ i32) (result i32)
  i32.const 0 ;; string: "hydra.formatting.convertCase"
  local.get $from_conv
  call $hydra.test.transform.encode_case_convention
  local.get $to_conv
  call $hydra.test.transform.encode_case_convention
  local.get $input_
)
  (func $hydra.test.transform.build_topological_sort_call (param $adj_list i32) (result i32)
  i32.const 0 ;; string: "hydra.sorting.topologicalSort"
  local.get $adj_list
  call $hydra.test.transform.encode_adjacency_list
)
  (func $hydra.test.transform.build_topological_sort_s_c_c_call (param $adj_list i32) (result i32)
  i32.const 0 ;; string: "hydra.sorting.topologicalSortComponents"
  local.get $adj_list
  call $hydra.test.transform.encode_adjacency_list
)
  (func $hydra.test.transform.collect_test_cases (param $tg i32) (result i32)
  (local $sg i32)
  ;; project field: cases
  local.get $sg
  call $hydra.test.transform.collect_test_cases
  ;; project field: subgroups
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  call $hydra.lib.lists.concat2
)
  (func $hydra.test.transform.encode_adjacency_list (param $pairs i32) (result i32)
  (local $d i32)
  (local $p i32)
  local.get $p
  call $hydra.lib.pairs.first
  call $hydra.test.transform.encode_int
  local.get $d
  call $hydra.test.transform.encode_int
  local.get $p
  call $hydra.lib.pairs.second
  call $hydra.lib.lists.map
  local.get $pairs
  call $hydra.lib.lists.map
)
  (func $hydra.test.transform.encode_case_convention (param $conv i32) (result i32)
  i32.const 0 ;; string: "hydra.util.CaseConvention"
  (block $end_case_convention (result i32)
  (block $pascal
  (block $camel
  (block $upper_snake
  (block $lower_snake
  local.get $conv
  br_table $lower_snake $upper_snake $camel $pascal $end_case_convention
  i32.const 0 ;; string: "lowerSnake"
  br $end_case_convention
)
  i32.const 0 ;; string: "upperSnake"
  br $end_case_convention
)
  i32.const 0 ;; string: "camel"
  br $end_case_convention
)
  i32.const 0 ;; string: "pascal"
  br $end_case_convention
)
)
  i32.const 0
)
  (func $hydra.test.transform.encode_either_list_list (param $e i32) (result i32)
  (local $cycles i32)
  (local $sorted i32)
  local.get $cycles
  call $hydra.test.transform.encode_list_list
  local.get $sorted
  call $hydra.test.transform.encode_int_list
  local.get $e
  call $hydra.lib.eithers.bimap
)
  (func $hydra.test.transform.encode_int (param $n i32) (result i32)
  local.get $n
)
  (func $hydra.test.transform.encode_int_list (param $ints i32) (result i32)
  (local $n i32)
  local.get $n
  call $hydra.test.transform.encode_int
  local.get $ints
  call $hydra.lib.lists.map
)
  (func $hydra.test.transform.encode_list_list (param $lists i32) (result i32)
  (local $l i32)
  local.get $l
  call $hydra.test.transform.encode_int_list
  local.get $lists
  call $hydra.lib.lists.map
)
  (func $hydra.test.transform.transform_module (param $m i32) (result i32)
  ;; project field: namespace
  call $hydra.test.transform.add_generation_prefix
  ;; project field: definitions
  ;; project field: term_dependencies
  ;; project field: type_dependencies
  ;; project field: description
)
  (func $hydra.test.transform.transform_test_case (param $tcm i32) (result i32)
  local.get $tcm
)
  (func $hydra.test.transform.transform_to_compiled_tests (param $tg i32) (result i32)
  (local $cases_ i32)
  (local $desc i32)
  (local $name_ i32)
  (local $sg i32)
  (local $subgroups i32)
  (local $tc i32)
  (local $transformed_cases i32)
  (local $transformed_subgroups i32)
  ;; project field: name
  local.set $name_
  ;; project field: description
  local.set $desc
  ;; project field: subgroups
  local.set $subgroups
  ;; project field: cases
  local.set $cases_
  local.get $tc
  call $hydra.test.transform.transform_test_case
  local.get $cases_
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  local.set $transformed_cases
  local.get $sg
  call $hydra.test.transform.transform_to_compiled_tests
  local.get $subgroups
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  local.set $transformed_subgroups
  local.get $transformed_cases
  call $hydra.lib.lists.null
  local.get $transformed_subgroups
  call $hydra.lib.lists.null
  call $hydra.lib.logic.and
  i32.const 0
  local.get $name_
  local.get $desc
  local.get $transformed_subgroups
  local.get $transformed_cases
  call $hydra.lib.logic.if_else
)
)
