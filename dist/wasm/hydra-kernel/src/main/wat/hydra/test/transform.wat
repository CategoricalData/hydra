(module
  (import "hydra.lib.eithers" "hydra.lib.eithers.bimap" (func $hydra.lib.eithers.bimap (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat" (func $hydra.lib.lists.concat (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.and" (func $hydra.lib.logic.and (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cat" (func $hydra.lib.maybes.cat (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\05\00\00\00\63\61\6d\65\6c\0b\00\00\00\67\65\6e\65\72\61\74\69\6f\6e\2e\1c\00\00\00\68\79\64\72\61\2e\66\6f\72\6d\61\74\74\69\6e\67\2e\63\6f\6e\76\65\72\74\43\61\73\65\1d\00\00\00\68\79\64\72\61\2e\73\6f\72\74\69\6e\67\2e\74\6f\70\6f\6c\6f\67\69\63\61\6c\53\6f\72\74\27\00\00\00\68\79\64\72\61\2e\73\6f\72\74\69\6e\67\2e\74\6f\70\6f\6c\6f\67\69\63\61\6c\53\6f\72\74\43\6f\6d\70\6f\6e\65\6e\74\73\19\00\00\00\68\79\64\72\61\2e\75\74\69\6c\2e\43\61\73\65\43\6f\6e\76\65\6e\74\69\6f\6e\0a\00\00\00\6c\6f\77\65\72\53\6e\61\6b\65\06\00\00\00\70\61\73\63\61\6c\0a\00\00\00\75\70\70\65\72\53\6e\61\6b\65")
  (global $__bump_ptr (mut i32) i32.const 1232 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
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
  i32.const 1033
  local.get $ns_
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.cat2
)
  (func $hydra.test.transform.build_convert_case_call (param $from_conv i32) (param $to_conv i32) (param $input_ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1
  i32.const 1
  i32.const 1
  i32.const 19
  i32.const 1048
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
  local.get $from_conv
  call $hydra.test.transform.encode_case_convention
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
  local.get $to_conv
  call $hydra.test.transform.encode_case_convention
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
  i32.const 5
  local.get $input_
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
)
  (func $hydra.test.transform.build_topological_sort_call (param $adj_list i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1
  i32.const 19
  i32.const 1080
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
  local.get $adj_list
  call $hydra.test.transform.encode_adjacency_list
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
)
  (func $hydra.test.transform.build_topological_sort_s_c_c_call (param $adj_list i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1
  i32.const 19
  i32.const 1113
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
  local.get $adj_list
  call $hydra.test.transform.encode_adjacency_list
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
)
  (func $hydra.test.transform.collect_test_cases (param $tg i32) (result i32)
  (local $sg i32)
  local.get $tg
  i32.load offset=12
  local.get $sg
  call $hydra.test.transform.collect_test_cases
  local.get $tg
  i32.load offset=8
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  call $hydra.lib.lists.concat2
)
  (func $hydra.test.transform.encode_adjacency_list (param $pairs i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $d i32)
  (local $p i32)
  i32.const 7
  i32.const 11
  local.get $p
  call $hydra.lib.pairs.first
  call $hydra.test.transform.encode_int
  i32.const 7
  local.get $d
  call $hydra.test.transform.encode_int
  local.get $p
  call $hydra.lib.pairs.second
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
  local.get $pairs
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
)
  (func $hydra.test.transform.encode_case_convention (param $conv i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $v i32)
  i32.const 4
  i32.const 1156
  (block $end_case_convention (result i32)
  (block $pascal
  (block $camel
  (block $upper_snake
  (block $lower_snake
  local.get $conv
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $lower_snake $upper_snake $camel $pascal $pascal
)
  local.get $v
  drop
  i32.const 1185
  br $end_case_convention
)
  local.get $v
  drop
  i32.const 1209
  br $end_case_convention
)
  local.get $v
  drop
  i32.const 1024
  br $end_case_convention
)
  local.get $v
  drop
  i32.const 1199
  br $end_case_convention
)
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
)
  (func $hydra.test.transform.encode_either_list_list (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $cycles i32)
  (local $sorted i32)
  i32.const 3
  local.get $cycles
  call $hydra.test.transform.encode_list_list
  local.get $sorted
  call $hydra.test.transform.encode_int_list
  local.get $e
  call $hydra.lib.eithers.bimap
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
)
  (func $hydra.test.transform.encode_int (param $n i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 8
  i32.const 4
  i32.const 3
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
)
  (func $hydra.test.transform.encode_int_list (param $ints i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $n i32)
  i32.const 7
  local.get $n
  call $hydra.test.transform.encode_int
  local.get $ints
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
)
  (func $hydra.test.transform.encode_list_list (param $lists i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $l i32)
  i32.const 7
  local.get $l
  call $hydra.test.transform.encode_int_list
  local.get $lists
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
)
  (func $hydra.test.transform.transform_module (param $m i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  local.get $m
  i32.load
  call $hydra.test.transform.add_generation_prefix
  local.get $m
  i32.load offset=4
  local.get $m
  i32.load offset=8
  local.get $m
  i32.load offset=12
  local.get $m
  i32.load offset=16
  i32.const 20
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=16
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
)
  (func $hydra.test.transform.transform_test_case (param $tcm i32) (result i32)
  local.get $tcm
)
  (func $hydra.test.transform.transform_to_compiled_tests (param $tg i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $cases_ i32)
  (local $desc i32)
  (local $name_ i32)
  (local $sg i32)
  (local $subgroups i32)
  (local $tc i32)
  (local $transformed_cases i32)
  (local $transformed_subgroups i32)
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
  call $hydra.lib.logic.if_else
)
)
