(module
  (import "hydra.constants" "hydra.constants.warning_auto_generated_file" (func $hydra.constants.warning_auto_generated_file (param i32) (result i32) ) )
  (import "hydra.decode.core" "hydra.decode.core.term" (func $hydra.decode.core.term (param i32) (result i32) ) )
  (import "hydra.dependencies" "hydra.dependencies.term_dependency_names" (func $hydra.dependencies.term_dependency_names (param i32) (result i32) ) )
  (import "hydra.ext.haskell.utils" "hydra.ext.haskell.utils.namespaces_for_module" (func $hydra.ext.haskell.utils.namespaces_for_module (param i32) (result i32) ) )
  (import "hydra.formatting" "hydra.formatting.capitalize" (func $hydra.formatting.capitalize (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.empty_context" (func $hydra.lexical.empty_context (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bimap" (func $hydra.lib.eithers.bimap (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat" (func $hydra.lib.lists.concat (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.head" (func $hydra.lib.lists.head (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.last" (func $hydra.lib.lists.last (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.replicate" (func $hydra.lib.lists.replicate (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_string" (func $hydra.lib.literals.show_string (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.not" (func $hydra.lib.logic.not (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.or" (func $hydra.lib.logic.or (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.filter_with_key" (func $hydra.lib.maps.filter_with_key (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.union" (func $hydra.lib.maps.union (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.add" (func $hydra.lib.math.add (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.mul" (func $hydra.lib.math.mul (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cat" (func $hydra.lib.maybes.cat (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.empty" (func $hydra.lib.sets.empty (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.union" (func $hydra.lib.sets.union (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.unions" (func $hydra.lib.sets.unions (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.from_list" (func $hydra.lib.strings.from_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.intercalate" (func $hydra.lib.strings.intercalate (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.split_on" (func $hydra.lib.strings.split_on (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.namespace_of" (func $hydra.names.namespace_of (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.namespace_to_file_path" (func $hydra.names.namespace_to_file_path (param i32) (result i32) ) )
  (import "hydra.predicates" "hydra.predicates.is_encoded_term" (func $hydra.predicates.is_encoded_term (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.fold_over_term" (func $hydra.rewriting.fold_over_term (param i32) (result i32) ) )
  (import "hydra.show.errors" "hydra.show.errors.error" (func $hydra.show.errors.error (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_term" (func $hydra.strip.deannotate_term (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.ext.haskell.testing.add_namespaces_to_namespaces" (func $hydra.ext.haskell.testing.add_namespaces_to_namespaces) )
  (export "hydra.ext.haskell.testing.build_namespaces_for_test_group" (func $hydra.ext.haskell.testing.build_namespaces_for_test_group) )
  (export "hydra.ext.haskell.testing.build_test_module" (func $hydra.ext.haskell.testing.build_test_module) )
  (export "hydra.ext.haskell.testing.collect_names" (func $hydra.ext.haskell.testing.collect_names) )
  (export "hydra.ext.haskell.testing.collect_test_cases" (func $hydra.ext.haskell.testing.collect_test_cases) )
  (export "hydra.ext.haskell.testing.extract_encoded_term_variable_names" (func $hydra.ext.haskell.testing.extract_encoded_term_variable_names) )
  (export "hydra.ext.haskell.testing.extract_test_terms" (func $hydra.ext.haskell.testing.extract_test_terms) )
  (export "hydra.ext.haskell.testing.find_haskell_imports" (func $hydra.ext.haskell.testing.find_haskell_imports) )
  (export "hydra.ext.haskell.testing.generate_haskell_test_file" (func $hydra.ext.haskell.testing.generate_haskell_test_file) )
  (export "hydra.ext.haskell.testing.generate_test_case" (func $hydra.ext.haskell.testing.generate_test_case) )
  (export "hydra.ext.haskell.testing.generate_test_file" (func $hydra.ext.haskell.testing.generate_test_file) )
  (export "hydra.ext.haskell.testing.generate_test_group_hierarchy" (func $hydra.ext.haskell.testing.generate_test_group_hierarchy) )
  (export "hydra.ext.haskell.testing.namespace_to_module_name" (func $hydra.ext.haskell.testing.namespace_to_module_name) )
  (func $hydra.ext.haskell.testing.add_namespaces_to_namespaces (param $ns0 i32) (param $names i32) (result i32)
  (local $new_mappings i32)
  (local $new_namespaces i32)
  (local $ns_ i32)
  (local $to_module_name i32)
  call $hydra.names.namespace_of
  local.get $names
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  call $hydra.lib.sets.from_list
  local.set $new_namespaces
  i32.const 0 ;; string: "."
  nop
  call $hydra.lib.strings.split_on
  call $hydra.lib.lists.last
  call $hydra.formatting.capitalize
  local.set $to_module_name
  local.get $ns_
  local.get $ns_
  local.get $to_module_name
  local.get $new_namespaces
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $new_mappings
  ;; project field: focus
  ;; project field: mapping
  local.get $new_mappings
  call $hydra.lib.maps.union
)
  (func $hydra.ext.haskell.testing.build_namespaces_for_test_group (param $mod i32) (param $tgroup i32) (param $graph_ i32) (result i32)
  (local $a i32)
  (local $base_namespaces i32)
  (local $encoded_names i32)
  (local $t i32)
  (local $temp_module i32)
  (local $term i32)
  (local $test_bindings i32)
  (local $test_cases_ i32)
  (local $test_terms i32)
  local.get $tgroup
  call $hydra.ext.haskell.testing.collect_test_cases
  local.set $test_cases_
  call $hydra.ext.haskell.testing.extract_test_terms
  local.get $test_cases_
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  local.set $test_terms
  i32.const 0 ;; string: "_test_"
  local.get $term
  i32.const 0
  local.get $test_terms
  call $hydra.lib.lists.map
  local.set $test_bindings
  ;; project field: namespace
  ;; project field: name
  ;; project field: term
  ;; project field: type
  local.get $test_bindings
  call $hydra.lib.lists.map
  ;; project field: term_dependencies
  ;; project field: type_dependencies
  ;; project field: description
  local.set $temp_module
  ;; project field: object
  call $hydra.show.errors.error
  local.get $a
  local.get $temp_module
  call $hydra.lexical.empty_context
  local.get $graph_
  call $hydra.ext.haskell.utils.namespaces_for_module
  call $hydra.lib.eithers.bimap
  local.get $graph_
  local.get $t
  call $hydra.ext.haskell.testing.extract_encoded_term_variable_names
  local.get $test_terms
  call $hydra.lib.lists.map
  call $hydra.lib.sets.unions
  local.set $encoded_names
  i32.const 1
  local.get $base_namespaces
  local.get $encoded_names
  call $hydra.ext.haskell.testing.add_namespaces_to_namespaces
  call $hydra.lib.eithers.bind
)
  (func $hydra.ext.haskell.testing.build_test_module (param $test_module i32) (param $test_group i32) (param $test_body i32) (param $namespaces i32) (result i32)
  (local $all_imports i32)
  (local $domain_imports i32)
  (local $group_name_ i32)
  (local $header i32)
  (local $module_name_string i32)
  (local $ns_ i32)
  (local $spec_ns i32)
  (local $standard_imports i32)
  ;; project field: namespace
  local.set $ns_
  nop
  i32.const 0 ;; string: "Spec"
  call $hydra.lib.strings.cat2
  local.set $spec_ns
  local.get $spec_ns
  call $hydra.ext.haskell.testing.namespace_to_module_name
  local.set $module_name_string
  ;; project field: name
  local.set $group_name_
  local.get $namespaces
  call $hydra.lib.sets.empty
  call $hydra.ext.haskell.testing.find_haskell_imports
  local.set $domain_imports
  i32.const 6
  ;; list elements follow
  i32.const 0 ;; string: "import Hydra.Kernel"
  i32.const 0 ;; string: "import qualified Test.Hspec as H"
  i32.const 0 ;; string: "import qualified Data.List as L"
  i32.const 0 ;; string: "import qualified Data.Map as M"
  i32.const 0 ;; string: "import qualified Data.Set as S"
  i32.const 0 ;; string: "import qualified Data.Maybe as Y"
  local.set $standard_imports
  local.get $standard_imports
  local.get $domain_imports
  call $hydra.lib.lists.concat2
  local.set $all_imports
  i32.const 0 ;; string: "
"
  i32.const 4
  ;; list elements follow
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "-- "
  call $hydra.constants.warning_auto_generated_file
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: ""
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: ""
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "module "
  local.get $module_name_string
  i32.const 0 ;; string: " where"
  call $hydra.lib.strings.cat
  i32.const 0 ;; string: ""
  local.get $all_imports
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: ""
  i32.const 0 ;; string: "spec :: H.Spec"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "spec = H.describe "
  local.get $group_name_
  call $hydra.lib.literals.show_string
  i32.const 0 ;; string: " $ do"
  call $hydra.lib.strings.cat
  call $hydra.lib.lists.concat
  call $hydra.lib.strings.intercalate
  local.set $header
  i32.const 4
  ;; list elements follow
  local.get $header
  i32.const 0 ;; string: "
"
  local.get $test_body
  i32.const 0 ;; string: "
"
  call $hydra.lib.strings.cat
)
  (func $hydra.ext.haskell.testing.collect_names (param $graf i32) (param $names i32) (param $t i32) (result i32)
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
  (func $hydra.ext.haskell.testing.collect_test_cases (param $tg i32) (result i32)
  ;; project field: cases
  call $hydra.ext.haskell.testing.collect_test_cases
  ;; project field: subgroups
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  call $hydra.lib.lists.concat2
)
  (func $hydra.ext.haskell.testing.extract_encoded_term_variable_names (param $graf i32) (param $term i32) (result i32)
  i32.const 0
  local.get $graf
  call $hydra.ext.haskell.testing.collect_names
  call $hydra.lib.sets.empty
  local.get $term
  call $hydra.rewriting.fold_over_term
)
  (func $hydra.ext.haskell.testing.extract_test_terms (param $tcm i32) (result i32)
  i32.const 0
  ;; list elements follow
)
  (func $hydra.ext.haskell.testing.find_haskell_imports (param $namespaces i32) (param $names_ i32) (result i32)
  (local $filtered i32)
  (local $mapping_ i32)
  ;; project field: mapping
  local.set $mapping_
  i32.const 0 ;; string: "hydra.test."
  nop
  call $hydra.lib.strings.split_on
  call $hydra.lib.lists.head
  i32.const 0 ;; string: ""
  call $hydra.lib.equality.equal
  call $hydra.lib.logic.not
  local.get $mapping_
  call $hydra.lib.maps.filter_with_key
  local.set $filtered
  i32.const 4
  ;; list elements follow
  i32.const 0 ;; string: "import qualified "
  i32.const 0 ;; string: "."
  call $hydra.formatting.capitalize
  i32.const 0 ;; string: "."
  nop
  call $hydra.lib.strings.split_on
  call $hydra.lib.lists.map
  call $hydra.lib.strings.intercalate
  i32.const 0 ;; string: " as "
  nop
  call $hydra.lib.strings.cat
  local.get $filtered
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.map
)
  (func $hydra.ext.haskell.testing.generate_haskell_test_file (param $test_module i32) (param $test_group i32) (param $g i32) (result i32)
  (local $namespaces i32)
  local.get $test_module
  local.get $test_group
  local.get $g
  call $hydra.ext.haskell.testing.build_namespaces_for_test_group
  local.get $test_module
  local.get $test_group
  local.get $namespaces
  call $hydra.ext.haskell.testing.generate_test_file
  call $hydra.lib.eithers.bind
)
  (func $hydra.ext.haskell.testing.generate_test_case (param $depth i32) (param $tcm i32) (result i32)
  (local $actual_ i32)
  (local $expected_ i32)
  (local $name_ i32)
  (local $tcase i32)
  (local $u i32)
  (local $universal i32)
  ;; project field: name
  local.set $name_
  ;; project field: case
  local.set $tcase
  (block $end_test_case (result i32)
  (block $universal
  local.get $tcase
  br_table $universal $end_test_case
  local.get $u
  br $end_test_case
)
)
  local.set $universal
  ;; project field: actual
  local.set $actual_
  ;; project field: expected
  local.set $expected_
  i32.const 1
  i32.const 3
  ;; list elements follow
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "H.it "
  local.get $name_
  call $hydra.lib.literals.show_string
  i32.const 0 ;; string: " $ H.shouldBe"
  call $hydra.lib.strings.cat
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "  ("
  local.get $actual_
  i32.const 0 ;; string: ")"
  call $hydra.lib.strings.cat
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "  ("
  local.get $expected_
  i32.const 0 ;; string: ")"
  call $hydra.lib.strings.cat
)
  (func $hydra.ext.haskell.testing.generate_test_file (param $test_module i32) (param $test_group i32) (param $namespaces i32) (result i32)
  (local $file_path i32)
  (local $ns_ i32)
  (local $spec_ns i32)
  (local $test_body i32)
  (local $test_module_content i32)
  local.get $test_module
  local.get $test_group
  local.get $test_body
  local.get $namespaces
  call $hydra.ext.haskell.testing.build_test_module
  local.set $test_module_content
  ;; project field: namespace
  local.set $ns_
  nop
  i32.const 0 ;; string: "Spec"
  call $hydra.lib.strings.cat2
  local.set $spec_ns
  i32.const 0
  i32.const 0 ;; string: "hs"
  local.get $spec_ns
  call $hydra.names.namespace_to_file_path
  local.set $file_path
  local.get $file_path
  local.get $test_module_content
  i32.const 1
  local.get $test_group
  call $hydra.ext.haskell.testing.generate_test_group_hierarchy
  call $hydra.lib.eithers.map
)
  (func $hydra.ext.haskell.testing.generate_test_group_hierarchy (param $depth i32) (param $test_group i32) (result i32)
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
  ;; project field: cases
  local.set $cases_
  ;; project field: subgroups
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
  call $hydra.ext.haskell.testing.generate_test_case
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
  i32.const 0 ;; string: "
"
  local.get $test_case_lines
  call $hydra.lib.lists.concat
  call $hydra.lib.strings.intercalate
  local.set $test_cases_str
  i32.const 3
  ;; list elements follow
  local.get $test_cases_str
  local.get $test_cases_str
  i32.const 0 ;; string: ""
  call $hydra.lib.equality.equal
  local.get $subgroups_str
  i32.const 0 ;; string: ""
  call $hydra.lib.equality.equal
  call $hydra.lib.logic.or
  i32.const 0 ;; string: ""
  i32.const 0 ;; string: "
"
  call $hydra.lib.logic.if_else
  local.get $subgroups_str
  call $hydra.lib.strings.cat
  i32.const 0 ;; string: "
"
  local.get $blocks
  call $hydra.lib.strings.intercalate
  ;; project field: name
  local.set $group_name_
  i32.const 5
  ;; list elements follow
  local.get $indent
  i32.const 0 ;; string: "H.describe "
  local.get $group_name_
  call $hydra.lib.literals.show_string
  i32.const 0 ;; string: " $ do
"
  local.get $content
  call $hydra.lib.strings.cat
  local.get $depth
  i32.const 1
  call $hydra.lib.math.add
  local.get $subgroup
  call $hydra.ext.haskell.testing.generate_test_group_hierarchy
  call $hydra.lib.eithers.map
  local.get $subgroups
  call $hydra.lib.eithers.map_list
  call $hydra.lib.eithers.map
  call $hydra.lib.eithers.map
  call $hydra.lib.eithers.bind
)
  (func $hydra.ext.haskell.testing.namespace_to_module_name (param $ns_ i32) (result i32)
  i32.const 0 ;; string: "."
  call $hydra.formatting.capitalize
  i32.const 0 ;; string: "."
  nop
  call $hydra.lib.strings.split_on
  call $hydra.lib.lists.map
  call $hydra.lib.strings.intercalate
)
)
