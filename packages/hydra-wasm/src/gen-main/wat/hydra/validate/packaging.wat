(module
  (import "hydra.formatting" "hydra.formatting.capitalize" (func $hydra.formatting.capitalize (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.take" (func $hydra.lib.lists.take (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.empty" (func $hydra.lib.maps.empty (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.insert" (func $hydra.lib.maps.insert (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cases" (func $hydra.lib.maybes.cases (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.empty" (func $hydra.lib.sets.empty (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.insert" (func $hydra.lib.sets.insert (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.member" (func $hydra.lib.sets.member (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.from_list" (func $hydra.lib.strings.from_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.length" (func $hydra.lib.strings.length (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.to_list" (func $hydra.lib.strings.to_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.to_lower" (func $hydra.lib.strings.to_lower (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.local_name_of" (func $hydra.names.local_name_of (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.validate.packaging.check_conflicting_module_namespaces" (func $hydra.validate.packaging.check_conflicting_module_namespaces) )
  (export "hydra.validate.packaging.check_conflicting_variant_names" (func $hydra.validate.packaging.check_conflicting_variant_names) )
  (export "hydra.validate.packaging.check_definition_namespaces" (func $hydra.validate.packaging.check_definition_namespaces) )
  (export "hydra.validate.packaging.check_duplicate_definition_names" (func $hydra.validate.packaging.check_duplicate_definition_names) )
  (export "hydra.validate.packaging.check_duplicate_module_namespaces" (func $hydra.validate.packaging.check_duplicate_module_namespaces) )
  (export "hydra.validate.packaging.definition_name" (func $hydra.validate.packaging.definition_name) )
  (export "hydra.validate.packaging.module" (func $hydra.validate.packaging.module) )
  (export "hydra.validate.packaging.package" (func $hydra.validate.packaging.package) )
  (func $hydra.validate.packaging.check_conflicting_module_namespaces (param $pkg i32) (result i32)
  (local $acc i32)
  (local $err i32)
  (local $existing i32)
  (local $first i32)
  (local $key i32)
  (local $mod i32)
  (local $ns i32)
  (local $result i32)
  (local $seen i32)
  local.get $acc
  call $hydra.lib.pairs.first
  local.set $seen
  local.get $acc
  call $hydra.lib.pairs.second
  local.set $err
  local.get $err
  local.get $mod
  ;; project field: namespace
  local.set $ns
  nop
  call $hydra.lib.strings.to_lower
  local.set $key
  local.get $key
  local.get $seen
  call $hydra.lib.maps.lookup
  local.set $existing
  local.get $existing
  local.get $key
  local.get $ns
  local.get $seen
  call $hydra.lib.maps.insert
  i32.const 0
  local.get $seen
  local.get $first
  local.get $ns
  call $hydra.lib.maybes.cases
  local.get $acc
  call $hydra.lib.maybes.cases
  call $hydra.lib.maps.empty
  i32.const 0
  local.get $pkg
  ;; project field: modules
  call $hydra.lib.lists.foldl
  local.set $result
  local.get $result
  call $hydra.lib.pairs.second
)
  (func $hydra.validate.packaging.check_conflicting_variant_names (param $mod i32) (result i32)
  (local $acc i32)
  (local $constructor_name i32)
  (local $def i32)
  (local $def_names i32)
  (local $defs i32)
  (local $field i32)
  (local $field_name i32)
  (local $fields i32)
  (local $inner_acc i32)
  (local $local_field_name i32)
  (local $local_type_name i32)
  (local $ns i32)
  (local $td i32)
  (local $typ i32)
  (local $type_name i32)
  local.get $mod
  ;; project field: namespace
  local.set $ns
  local.get $mod
  ;; project field: definitions
  local.set $defs
  local.get $def
  call $hydra.validate.packaging.definition_name
  call $hydra.names.local_name_of
  local.get $acc
  call $hydra.lib.sets.insert
  call $hydra.lib.sets.empty
  local.get $defs
  call $hydra.lib.lists.foldl
  local.set $def_names
  local.get $acc
  (block $end_definition (result i32)
  (block $type
  local.get $def
  br_table $type $type
)
  local.get $td
  ;; project field: name
  local.set $type_name
  local.get $type_name
  call $hydra.names.local_name_of
  local.set $local_type_name
  local.get $td
  ;; project field: type
  ;; project field: type
  local.set $typ
  (block $end_type (result i32)
  (block $union
  local.get $typ
  br_table $union $union
)
  local.get $inner_acc
  local.get $field
  ;; project field: name
  local.set $field_name
  local.get $field_name
  call $hydra.names.local_name_of
  local.set $local_field_name
  local.get $local_type_name
  call $hydra.formatting.capitalize
  local.get $local_field_name
  call $hydra.formatting.capitalize
  call $hydra.lib.strings.cat2
  local.set $constructor_name
  local.get $constructor_name
  local.get $def_names
  call $hydra.lib.sets.member
  local.get $ns
  local.get $type_name
  local.get $field_name
  local.get $constructor_name
  i32.const 0
  call $hydra.lib.logic.if_else
  local.get $inner_acc
  call $hydra.lib.maybes.cases
  i32.const 0
  local.get $fields
  call $hydra.lib.lists.foldl
  br $end_type
)
  br $end_definition
)
  local.get $acc
  call $hydra.lib.maybes.cases
  i32.const 0
  local.get $defs
  call $hydra.lib.lists.foldl
)
  (func $hydra.validate.packaging.check_definition_namespaces (param $mod i32) (result i32)
  (local $acc i32)
  (local $def i32)
  (local $name i32)
  (local $name_prefix i32)
  (local $name_str i32)
  (local $ns i32)
  (local $prefix i32)
  (local $prefix_len i32)
  local.get $mod
  ;; project field: namespace
  local.set $ns
  nop
  i32.const 0 ;; string: "."
  call $hydra.lib.strings.cat2
  local.set $prefix
  local.get $prefix
  call $hydra.lib.strings.length
  local.set $prefix_len
  local.get $acc
  local.get $def
  call $hydra.validate.packaging.definition_name
  local.set $name
  nop
  local.set $name_str
  local.get $prefix_len
  local.get $name_str
  call $hydra.lib.strings.to_list
  call $hydra.lib.lists.take
  local.set $name_prefix
  local.get $name_prefix
  call $hydra.lib.strings.from_list
  local.get $prefix
  call $hydra.lib.equality.equal
  i32.const 0
  local.get $ns
  local.get $name
  call $hydra.lib.logic.if_else
  local.get $acc
  call $hydra.lib.maybes.cases
  i32.const 0
  local.get $mod
  ;; project field: definitions
  call $hydra.lib.lists.foldl
)
  (func $hydra.validate.packaging.check_duplicate_definition_names (param $mod i32) (result i32)
  (local $acc i32)
  (local $def i32)
  (local $err i32)
  (local $name i32)
  (local $ns i32)
  (local $result i32)
  (local $seen i32)
  local.get $mod
  ;; project field: namespace
  local.set $ns
  local.get $acc
  call $hydra.lib.pairs.first
  local.set $seen
  local.get $acc
  call $hydra.lib.pairs.second
  local.set $err
  local.get $err
  local.get $def
  call $hydra.validate.packaging.definition_name
  local.set $name
  local.get $name
  local.get $seen
  call $hydra.lib.sets.member
  local.get $seen
  local.get $ns
  local.get $name
  local.get $name
  local.get $seen
  call $hydra.lib.sets.insert
  i32.const 0
  call $hydra.lib.logic.if_else
  local.get $acc
  call $hydra.lib.maybes.cases
  call $hydra.lib.sets.empty
  i32.const 0
  local.get $mod
  ;; project field: definitions
  call $hydra.lib.lists.foldl
  local.set $result
  local.get $result
  call $hydra.lib.pairs.second
)
  (func $hydra.validate.packaging.check_duplicate_module_namespaces (param $pkg i32) (result i32)
  (local $acc i32)
  (local $err i32)
  (local $mod i32)
  (local $ns i32)
  (local $result i32)
  (local $seen i32)
  local.get $acc
  call $hydra.lib.pairs.first
  local.set $seen
  local.get $acc
  call $hydra.lib.pairs.second
  local.set $err
  local.get $err
  local.get $mod
  ;; project field: namespace
  local.set $ns
  local.get $ns
  local.get $seen
  call $hydra.lib.sets.member
  local.get $seen
  local.get $ns
  local.get $ns
  local.get $seen
  call $hydra.lib.sets.insert
  i32.const 0
  call $hydra.lib.logic.if_else
  local.get $acc
  call $hydra.lib.maybes.cases
  call $hydra.lib.sets.empty
  i32.const 0
  local.get $pkg
  ;; project field: modules
  call $hydra.lib.lists.foldl
  local.set $result
  local.get $result
  call $hydra.lib.pairs.second
)
  (func $hydra.validate.packaging.definition_name (param $def i32) (result i32)
  (local $td i32)
  (block $end_definition (result i32)
  (block $type
  (block $term
  local.get $def
  br_table $term $type $type
)
  local.get $td
  ;; project field: name
  br $end_definition
)
  local.get $td
  ;; project field: name
  br $end_definition
)
)
  (func $hydra.validate.packaging.module (param $mod i32) (result i32)
  (local $r1 i32)
  (local $r2 i32)
  local.get $mod
  call $hydra.validate.packaging.check_definition_namespaces
  local.set $r1
  local.get $r1
  local.get $mod
  call $hydra.validate.packaging.check_duplicate_definition_names
  local.set $r2
  local.get $r2
  local.get $mod
  call $hydra.validate.packaging.check_conflicting_variant_names
  local.get $r2
  call $hydra.lib.maybes.cases
  local.get $r1
  call $hydra.lib.maybes.cases
)
  (func $hydra.validate.packaging.package (param $pkg i32) (result i32)
  (local $acc i32)
  (local $err i32)
  (local $mod i32)
  (local $r1 i32)
  (local $r2 i32)
  local.get $pkg
  call $hydra.validate.packaging.check_duplicate_module_namespaces
  local.set $r1
  local.get $r1
  local.get $pkg
  call $hydra.validate.packaging.check_conflicting_module_namespaces
  local.set $r2
  local.get $r2
  local.get $acc
  local.get $err
  local.get $mod
  call $hydra.validate.packaging.module
  call $hydra.lib.maybes.map
  local.get $acc
  call $hydra.lib.maybes.cases
  i32.const 0
  local.get $pkg
  ;; project field: modules
  call $hydra.lib.lists.foldl
  local.get $r2
  call $hydra.lib.maybes.cases
  local.get $r1
  call $hydra.lib.maybes.cases
)
)
