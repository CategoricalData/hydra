(module
  (import "hydra.decode.core" "hydra.decode.core.term" (func $hydra.decode.core.term (param i32) (param i32) (result i32) ) )
  (import "hydra.dependencies" "hydra.dependencies.term_dependency_names" (func $hydra.dependencies.term_dependency_names (param i32) (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.formatting" "hydra.formatting.capitalize" (func $hydra.formatting.capitalize (param i32) (result i32) ) )
  (import "hydra.haskell.utils" "hydra.haskell.utils.namespaces_for_module" (func $hydra.haskell.utils.namespaces_for_module (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bimap" (func $hydra.lib.eithers.bimap (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat" (func $hydra.lib.lists.concat (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.maybe_head" (func $hydra.lib.lists.maybe_head (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.maybe_last" (func $hydra.lib.lists.maybe_last (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.replicate" (func $hydra.lib.lists.replicate (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_string" (func $hydra.lib.literals.show_string (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.or" (func $hydra.lib.logic.or (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.filter_with_key" (func $hydra.lib.maps.filter_with_key (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.union" (func $hydra.lib.maps.union (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.add" (func $hydra.lib.math.add (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.mul" (func $hydra.lib.math.mul (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cat" (func $hydra.lib.maybes.cat (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.from_maybe" (func $hydra.lib.maybes.from_maybe (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.union" (func $hydra.lib.sets.union (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.unions" (func $hydra.lib.sets.unions (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.from_list" (func $hydra.lib.strings.from_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.intercalate" (func $hydra.lib.strings.intercalate (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.split_on" (func $hydra.lib.strings.split_on (param i32) (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.namespace_to_file_path" (func $hydra.names.namespace_to_file_path (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.predicates" "hydra.predicates.is_encoded_term" (func $hydra.predicates.is_encoded_term (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.fold_over_term" (func $hydra.rewriting.fold_over_term (param i32) (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.show.errors" "hydra.show.errors.error" (func $hydra.show.errors.error (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_term" (func $hydra.strip.deannotate_term (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\00\00\00\00\01\00\00\00\0a\03\00\00\00\20\20\28\0d\00\00\00\20\24\20\48\2e\73\68\6f\75\6c\64\42\65\05\00\00\00\20\24\20\64\6f\06\00\00\00\20\24\20\64\6f\0a\04\00\00\00\20\61\73\20\06\00\00\00\20\77\68\65\72\65\01\00\00\00\29\03\00\00\00\2d\2d\20\01\00\00\00\2e\0b\00\00\00\48\2e\64\65\73\63\72\69\62\65\20\05\00\00\00\48\2e\69\74\20\04\00\00\00\53\70\65\63\06\00\00\00\5f\74\65\73\74\5f\02\00\00\00\68\73\0b\00\00\00\68\79\64\72\61\2e\74\65\73\74\2e\13\00\00\00\69\6d\70\6f\72\74\20\48\79\64\72\61\2e\4b\65\72\6e\65\6c\11\00\00\00\69\6d\70\6f\72\74\20\71\75\61\6c\69\66\69\65\64\20\1f\00\00\00\69\6d\70\6f\72\74\20\71\75\61\6c\69\66\69\65\64\20\44\61\74\61\2e\4c\69\73\74\20\61\73\20\4c\1e\00\00\00\69\6d\70\6f\72\74\20\71\75\61\6c\69\66\69\65\64\20\44\61\74\61\2e\4d\61\70\20\61\73\20\4d\20\00\00\00\69\6d\70\6f\72\74\20\71\75\61\6c\69\66\69\65\64\20\44\61\74\61\2e\4d\61\79\62\65\20\61\73\20\59\1e\00\00\00\69\6d\70\6f\72\74\20\71\75\61\6c\69\66\69\65\64\20\44\61\74\61\2e\53\65\74\20\61\73\20\53\20\00\00\00\69\6d\70\6f\72\74\20\71\75\61\6c\69\66\69\65\64\20\54\65\73\74\2e\48\73\70\65\63\20\61\73\20\48\07\00\00\00\6d\6f\64\75\6c\65\20\0e\00\00\00\73\70\65\63\20\3a\3a\20\48\2e\53\70\65\63\12\00\00\00\73\70\65\63\20\3d\20\48\2e\64\65\73\63\72\69\62\65\20")
  (global $__bump_ptr (mut i32) i32.const 1456 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.haskell.testing.add_namespaces_to_namespaces" (func $hydra.haskell.testing.add_namespaces_to_namespaces) )
  (export "hydra.haskell.testing.build_namespaces_for_test_group" (func $hydra.haskell.testing.build_namespaces_for_test_group) )
  (export "hydra.haskell.testing.build_test_module" (func $hydra.haskell.testing.build_test_module) )
  (export "hydra.haskell.testing.collect_names" (func $hydra.haskell.testing.collect_names) )
  (export "hydra.haskell.testing.collect_test_cases" (func $hydra.haskell.testing.collect_test_cases) )
  (export "hydra.haskell.testing.extract_encoded_term_variable_names" (func $hydra.haskell.testing.extract_encoded_term_variable_names) )
  (export "hydra.haskell.testing.extract_test_terms" (func $hydra.haskell.testing.extract_test_terms) )
  (export "hydra.haskell.testing.find_haskell_imports" (func $hydra.haskell.testing.find_haskell_imports) )
  (export "hydra.haskell.testing.generate_haskell_test_file" (func $hydra.haskell.testing.generate_haskell_test_file) )
  (export "hydra.haskell.testing.generate_test_case" (func $hydra.haskell.testing.generate_test_case) )
  (export "hydra.haskell.testing.generate_test_file" (func $hydra.haskell.testing.generate_test_file) )
  (export "hydra.haskell.testing.generate_test_group_hierarchy" (func $hydra.haskell.testing.generate_test_group_hierarchy) )
  (export "hydra.haskell.testing.namespace_to_module_name" (func $hydra.haskell.testing.namespace_to_module_name) )
  (func $hydra.haskell.testing.add_namespaces_to_namespaces (param $ns0 i32) (param $names i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $namespace i32)
  (local $new_mappings i32)
  (local $new_namespaces i32)
  (local $ns_ i32)
  (local $to_module_name i32)
  i32.const 0
  local.get $names
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  call $hydra.lib.sets.from_list
  local.set $new_namespaces
  local.get $namespace
  drop
  i32.const 0
  drop
  i32.const 0
  i32.const 1106
  local.get $namespace
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.split_on
  call $hydra.lib.lists.maybe_last
  call $hydra.lib.maybes.from_maybe
  call $hydra.formatting.capitalize
  local.set $to_module_name
  local.get $ns_
  local.get $ns_
  drop
  local.get $to_module_name
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
  local.get $new_namespaces
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $new_mappings
  local.get $ns0
  i32.load
  local.get $ns0
  i32.load offset=4
  local.get $new_mappings
  call $hydra.lib.maps.union
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
  (func $hydra.haskell.testing.build_namespaces_for_test_group (param $mod i32) (param $tgroup i32) (param $graph_ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $a i32)
  (local $b i32)
  (local $base_namespaces i32)
  (local $e i32)
  (local $encoded_names i32)
  (local $t i32)
  (local $temp_module i32)
  (local $term i32)
  (local $test_bindings i32)
  (local $test_cases_ i32)
  (local $test_terms i32)
  local.get $tgroup
  call $hydra.haskell.testing.collect_test_cases
  local.set $test_cases_
  i32.const 0
  local.get $test_cases_
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  local.set $test_terms
  i32.const 1143
  local.get $term
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
  local.get $test_terms
  call $hydra.lib.lists.map
  local.set $test_bindings
  local.get $mod
  i32.load
  i32.const 0
  local.get $b
  i32.load
  local.get $b
  i32.load offset=4
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
  local.get $test_bindings
  call $hydra.lib.lists.map
  local.get $mod
  i32.load offset=8
  local.get $mod
  i32.load offset=12
  local.get $mod
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
  local.set $temp_module
  local.get $e
  call $hydra.show.errors.error
  local.get $a
  local.get $temp_module
  i32.const 0
  local.get $graph_
  call $hydra.haskell.utils.namespaces_for_module
  call $hydra.lib.eithers.bimap
  local.get $graph_
  local.get $t
  call $hydra.haskell.testing.extract_encoded_term_variable_names
  local.get $test_terms
  call $hydra.lib.lists.map
  call $hydra.lib.sets.unions
  local.set $encoded_names
  i32.const 1
  local.get $base_namespaces
  local.get $encoded_names
  call $hydra.haskell.testing.add_namespaces_to_namespaces
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
)
  (func $hydra.haskell.testing.build_test_module (param $test_module i32) (param $test_group i32) (param $test_body i32) (param $namespaces i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $all_imports i32)
  (local $domain_imports i32)
  (local $group_name_ i32)
  (local $header i32)
  (local $module_name_string i32)
  (local $ns_ i32)
  (local $spec_ns i32)
  (local $standard_imports i32)
  local.get $test_module
  i32.load
  local.set $ns_
  local.get $ns_
  drop
  i32.const 0
  drop
  i32.const 0
  i32.const 1135
  call $hydra.lib.strings.cat2
  local.set $spec_ns
  local.get $spec_ns
  call $hydra.haskell.testing.namespace_to_module_name
  local.set $module_name_string
  local.get $test_group
  i32.load
  local.set $group_name_
  local.get $namespaces
  i32.const 0
  call $hydra.haskell.testing.find_haskell_imports
  local.set $domain_imports
  i32.const 1174
  i32.const 1357
  i32.const 1218
  i32.const 1253
  i32.const 1323
  i32.const 1287
  i32.const 28
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 6
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=24
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=20
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
  local.get $__rec_ptr
  local.set $standard_imports
  local.get $standard_imports
  local.get $domain_imports
  call $hydra.lib.lists.concat2
  local.set $all_imports
  i32.const 1028
  i32.const 1099
  i32.const 0
  call $hydra.lib.strings.cat2
  i32.const 1024
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 2
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  i32.const 1024
  i32.const 1393
  local.get $module_name_string
  i32.const 1084
  i32.const 16
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 3
  i32.store
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
  local.get $__rec_ptr
  call $hydra.lib.strings.cat
  i32.const 1024
  i32.const 16
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 3
  i32.store
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
  local.get $__rec_ptr
  local.get $all_imports
  i32.const 1024
  i32.const 1404
  i32.const 1422
  local.get $group_name_
  call $hydra.lib.literals.show_string
  i32.const 1057
  i32.const 16
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 3
  i32.store
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
  local.get $__rec_ptr
  call $hydra.lib.strings.cat
  i32.const 16
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 3
  i32.store
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
  local.get $__rec_ptr
  i32.const 20
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 4
  i32.store
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
  local.get $__rec_ptr
  call $hydra.lib.lists.concat
  call $hydra.lib.strings.intercalate
  local.set $header
  local.get $header
  i32.const 1028
  local.get $test_body
  i32.const 1028
  i32.const 20
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 4
  i32.store
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
  local.get $__rec_ptr
  call $hydra.lib.strings.cat
)
  (func $hydra.haskell.testing.collect_names (param $graf i32) (param $names i32) (param $t i32) (result i32)
  (local $_a i32)
  (local $_e i32)
  (local $decoded_term i32)
  local.get $t
  call $hydra.strip.deannotate_term
  call $hydra.predicates.is_encoded_term
  local.get $names
  local.get $names
  i32.const 1
  i32.const 1
  i32.const 1
  local.get $decoded_term
  call $hydra.dependencies.term_dependency_names
  call $hydra.lib.sets.union
  local.get $_e
  local.get $_a
  local.get $graf
  local.get $t
  call $hydra.decode.core.term
  call $hydra.lib.eithers.bimap
  call $hydra.lib.eithers.either
  local.get $names
  call $hydra.lib.logic.if_else
)
  (func $hydra.haskell.testing.collect_test_cases (param $tg i32) (result i32)
  local.get $tg
  i32.load offset=12
  i32.const 0
  local.get $tg
  i32.load offset=8
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  call $hydra.lib.lists.concat2
)
  (func $hydra.haskell.testing.extract_encoded_term_variable_names (param $graf i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 0
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
  local.get $graf
  i32.const 0
  i32.const 0
  call $hydra.haskell.testing.collect_names
  i32.const 0
  local.get $term
  call $hydra.rewriting.fold_over_term
)
  (func $hydra.haskell.testing.extract_test_terms (param $tcm i32) (result i32)
  (local $__rec_ptr i32)
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
)
  (func $hydra.haskell.testing.find_haskell_imports (param $namespaces i32) (param $names_ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $entry i32)
  (local $filtered i32)
  (local $mapping_ i32)
  (local $ns_ i32)
  local.get $namespaces
  i32.load offset=4
  local.set $mapping_
  i32.const 1024
  i32.const 1159
  local.get $ns_
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.split_on
  call $hydra.lib.lists.maybe_head
  call $hydra.lib.maybes.from_maybe
  i32.const 1024
  call $hydra.lib.equality.equal
  call $hydra.lib.logic.not
  local.get $mapping_
  call $hydra.lib.maps.filter_with_key
  local.set $filtered
  i32.const 1197
  i32.const 1106
  i32.const 0
  i32.const 1106
  local.get $entry
  call $hydra.lib.pairs.first
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.split_on
  call $hydra.lib.lists.map
  call $hydra.lib.strings.intercalate
  i32.const 1076
  local.get $entry
  call $hydra.lib.pairs.second
  drop
  i32.const 0
  drop
  i32.const 0
  i32.const 20
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 4
  i32.store
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
  local.get $__rec_ptr
  call $hydra.lib.strings.cat
  local.get $filtered
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.map
)
  (func $hydra.haskell.testing.generate_haskell_test_file (param $test_module i32) (param $test_group i32) (param $g i32) (result i32)
  (local $namespaces i32)
  local.get $test_module
  local.get $test_group
  local.get $g
  call $hydra.haskell.testing.build_namespaces_for_test_group
  local.get $test_module
  local.get $test_group
  local.get $namespaces
  call $hydra.haskell.testing.generate_test_file
  call $hydra.lib.eithers.bind
)
  (func $hydra.haskell.testing.generate_test_case (param $depth i32) (param $tcm i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $actual_ i32)
  (local $expected_ i32)
  (local $name_ i32)
  (local $tcase i32)
  (local $u i32)
  (local $universal i32)
  (local $v i32)
  local.get $tcm
  i32.load
  local.set $name_
  local.get $tcm
  i32.load offset=4
  local.set $tcase
  (block $end_test_case (result i32)
  (block $universal
  local.get $tcase
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $universal $universal
)
  local.get $v
  drop
  local.get $u
  br $end_test_case
)
  local.set $universal
  local.get $universal
  i32.load
  local.set $actual_
  local.get $universal
  i32.load offset=4
  local.set $expected_
  i32.const 1
  i32.const 1126
  local.get $name_
  call $hydra.lib.literals.show_string
  i32.const 1040
  i32.const 16
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 3
  i32.store
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
  local.get $__rec_ptr
  call $hydra.lib.strings.cat
  i32.const 1033
  local.get $actual_
  i32.const 1094
  i32.const 16
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 3
  i32.store
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
  local.get $__rec_ptr
  call $hydra.lib.strings.cat
  i32.const 1033
  local.get $expected_
  i32.const 1094
  i32.const 16
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 3
  i32.store
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
  local.get $__rec_ptr
  call $hydra.lib.strings.cat
  i32.const 16
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 3
  i32.store
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
  (func $hydra.haskell.testing.generate_test_file (param $test_module i32) (param $test_group i32) (param $namespaces i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $file_path i32)
  (local $ns_ i32)
  (local $spec_ns i32)
  (local $test_body i32)
  (local $test_module_content i32)
  local.get $test_module
  local.get $test_group
  local.get $test_body
  local.get $namespaces
  call $hydra.haskell.testing.build_test_module
  local.set $test_module_content
  local.get $test_module
  i32.load
  local.set $ns_
  local.get $ns_
  drop
  i32.const 0
  drop
  i32.const 0
  i32.const 1135
  call $hydra.lib.strings.cat2
  local.set $spec_ns
  i32.const 1
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
  i32.const 1153
  local.get $spec_ns
  call $hydra.names.namespace_to_file_path
  local.set $file_path
  local.get $file_path
  local.get $test_module_content
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
  local.get $test_group
  call $hydra.haskell.testing.generate_test_group_hierarchy
  call $hydra.lib.eithers.map
)
  (func $hydra.haskell.testing.generate_test_group_hierarchy (param $depth i32) (param $test_group i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $blocks i32)
  (local $cases_ i32)
  (local $content i32)
  (local $group_name_ i32)
  (local $indent i32)
  (local $line i32)
  (local $lines_ i32)
  (local $subgroup i32)
  (local $subgroups i32)
  (local $subgroups_str i32)
  (local $tc i32)
  (local $test_case_lines i32)
  (local $test_case_lines_raw i32)
  (local $test_cases_str i32)
  local.get $test_group
  i32.load offset=12
  local.set $cases_
  local.get $test_group
  i32.load offset=8
  local.set $subgroups
  local.get $depth
  i32.const 2
  call $hydra.lib.math.mul
  i32.const 32
  call $hydra.lib.lists.replicate
  call $hydra.lib.strings.from_list
  local.set $indent
  local.get $depth
  local.get $tc
  call $hydra.haskell.testing.generate_test_case
  local.get $cases_
  call $hydra.lib.eithers.map_list
  local.get $indent
  local.get $line
  call $hydra.lib.strings.cat2
  local.get $lines_
  call $hydra.lib.lists.map
  local.get $test_case_lines_raw
  call $hydra.lib.lists.map
  local.set $test_case_lines
  i32.const 1028
  local.get $test_case_lines
  call $hydra.lib.lists.concat
  call $hydra.lib.strings.intercalate
  local.set $test_cases_str
  local.get $test_cases_str
  local.get $test_cases_str
  i32.const 1024
  call $hydra.lib.equality.equal
  local.get $subgroups_str
  i32.const 1024
  call $hydra.lib.equality.equal
  call $hydra.lib.logic.or
  i32.const 1024
  i32.const 1028
  call $hydra.lib.logic.if_else
  local.get $subgroups_str
  i32.const 16
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 3
  i32.store
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
  local.get $__rec_ptr
  call $hydra.lib.strings.cat
  i32.const 1028
  local.get $blocks
  call $hydra.lib.strings.intercalate
  local.get $subgroup
  i32.load
  local.set $group_name_
  local.get $indent
  i32.const 1111
  local.get $group_name_
  call $hydra.lib.literals.show_string
  i32.const 1066
  local.get $content
  i32.const 24
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 5
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=20
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
  local.get $__rec_ptr
  call $hydra.lib.strings.cat
  local.get $depth
  i32.const 1
  call $hydra.lib.math.add
  local.get $subgroup
  call $hydra.haskell.testing.generate_test_group_hierarchy
  call $hydra.lib.eithers.map
  local.get $subgroups
  call $hydra.lib.eithers.map_list
  call $hydra.lib.eithers.map
  call $hydra.lib.eithers.map
  call $hydra.lib.eithers.bind
)
  (func $hydra.haskell.testing.namespace_to_module_name (param $ns_ i32) (result i32)
  i32.const 1106
  i32.const 0
  i32.const 1106
  local.get $ns_
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.split_on
  call $hydra.lib.lists.map
  call $hydra.lib.strings.intercalate
)
)
