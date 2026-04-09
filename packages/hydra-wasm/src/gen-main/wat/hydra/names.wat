(module
  (import "hydra.annotations" "hydra.annotations.get_count" (func $hydra.annotations.get_count (param i32) (result i32) ) )
  (import "hydra.annotations" "hydra.annotations.put_count" (func $hydra.annotations.put_count (param i32) (result i32) ) )
  (import "hydra.constants" "hydra.constants.key_fresh_type_variable_count" (func $hydra.constants.key_fresh_type_variable_count (param i32) (result i32) ) )
  (import "hydra.formatting" "hydra.formatting.convert_case" (func $hydra.formatting.convert_case (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.head" (func $hydra.lib.lists.head (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.pure" (func $hydra.lib.lists.pure (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.replicate" (func $hydra.lib.lists.replicate (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.reverse" (func $hydra.lib.lists.reverse (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.tail" (func $hydra.lib.lists.tail (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int32" (func $hydra.lib.literals.show_int32 (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.add" (func $hydra.lib.math.add (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.member" (func $hydra.lib.sets.member (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.intercalate" (func $hydra.lib.strings.intercalate (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.split_on" (func $hydra.lib.strings.split_on (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.names.compact_name" (func $hydra.names.compact_name) )
  (export "hydra.names.fresh_name" (func $hydra.names.fresh_name) )
  (export "hydra.names.fresh_names" (func $hydra.names.fresh_names) )
  (export "hydra.names.local_name_of" (func $hydra.names.local_name_of) )
  (export "hydra.names.name_to_file_path" (func $hydra.names.name_to_file_path) )
  (export "hydra.names.namespace_of" (func $hydra.names.namespace_of) )
  (export "hydra.names.namespace_to_file_path" (func $hydra.names.namespace_to_file_path) )
  (export "hydra.names.normal_type_variable" (func $hydra.names.normal_type_variable) )
  (export "hydra.names.qname" (func $hydra.names.qname) )
  (export "hydra.names.qualify_name" (func $hydra.names.qualify_name) )
  (export "hydra.names.unique_label" (func $hydra.names.unique_label) )
  (export "hydra.names.unqualify_name" (func $hydra.names.unqualify_name) )
  (func $hydra.names.compact_name (param $namespaces i32) (param $name i32) (result i32)
  (local $local i32)
  (local $mns i32)
  (local $ns i32)
  (local $pre i32)
  (local $qual_name i32)
  local.get $name
  call $hydra.names.qualify_name
  local.set $qual_name
  local.get $qual_name
  ;; project field: namespace
  local.set $mns
  local.get $qual_name
  ;; project field: local
  local.set $local
  nop
  local.get $local
  i32.const 3
  ;; list elements follow
  local.get $pre
  i32.const 0 ;; string: ":"
  local.get $local
  call $hydra.lib.strings.cat
  local.get $ns
  local.get $namespaces
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  local.get $mns
  call $hydra.lib.maybes.maybe
)
  (func $hydra.names.fresh_name (param $cx i32) (result i32)
  (local $count i32)
  call $hydra.constants.key_fresh_type_variable_count
  local.get $cx
  call $hydra.annotations.get_count
  local.set $count
  local.get $count
  call $hydra.names.normal_type_variable
  call $hydra.constants.key_fresh_type_variable_count
  local.get $count
  i32.const 1
  call $hydra.lib.math.add
  local.get $cx
  call $hydra.annotations.put_count
)
  (func $hydra.names.fresh_names (param $n i32) (param $cx i32) (result i32)
  (local $acc i32)
  (local $cx0 i32)
  (local $cx1 i32)
  (local $go i32)
  (local $name i32)
  (local $names i32)
  (local $result i32)
  local.get $acc
  call $hydra.lib.pairs.first
  local.set $names
  local.get $acc
  call $hydra.lib.pairs.second
  local.set $cx0
  local.get $cx0
  call $hydra.names.fresh_name
  local.set $result
  local.get $result
  call $hydra.lib.pairs.first
  local.set $name
  local.get $result
  call $hydra.lib.pairs.second
  local.set $cx1
  local.get $names
  local.get $name
  call $hydra.lib.lists.pure
  call $hydra.lib.lists.concat2
  local.get $cx1
  local.set $go
  local.get $go
  i32.const 0
  ;; list elements follow
  local.get $cx
  local.get $n
  call $hydra.lib.lists.replicate
  call $hydra.lib.lists.foldl
)
  (func $hydra.names.local_name_of (param $arg_ i32) (result i32)
  local.get $arg_
  call $hydra.names.qualify_name
  ;; project field: local
)
  (func $hydra.names.name_to_file_path (param $ns_conv i32) (param $local_conv i32) (param $ext i32) (param $name i32) (result i32)
  (local $local i32)
  (local $n i32)
  (local $ns i32)
  (local $ns_to_file_path i32)
  (local $part i32)
  (local $prefix i32)
  (local $qual_name i32)
  (local $suffix i32)
  local.get $name
  call $hydra.names.qualify_name
  local.set $qual_name
  local.get $qual_name
  ;; project field: namespace
  local.set $ns
  local.get $qual_name
  ;; project field: local
  local.set $local
  i32.const 0 ;; string: "/"
  i32.const 0
  local.get $ns_conv
  local.get $part
  call $hydra.formatting.convert_case
  i32.const 0 ;; string: "."
  nop
  call $hydra.lib.strings.split_on
  call $hydra.lib.lists.map
  call $hydra.lib.strings.intercalate
  local.set $ns_to_file_path
  i32.const 0 ;; string: ""
  local.get $n
  local.get $ns_to_file_path
  i32.const 0 ;; string: "/"
  call $hydra.lib.strings.cat2
  local.get $ns
  call $hydra.lib.maybes.maybe
  local.set $prefix
  i32.const 0
  local.get $local_conv
  local.get $local
  call $hydra.formatting.convert_case
  local.set $suffix
  i32.const 4
  ;; list elements follow
  local.get $prefix
  local.get $suffix
  i32.const 0 ;; string: "."
  nop
  call $hydra.lib.strings.cat
)
  (func $hydra.names.namespace_of (param $arg_ i32) (result i32)
  local.get $arg_
  call $hydra.names.qualify_name
  ;; project field: namespace
)
  (func $hydra.names.namespace_to_file_path (param $case_conv i32) (param $ext i32) (param $ns i32) (result i32)
  (local $parts i32)
  i32.const 0
  local.get $case_conv
  call $hydra.formatting.convert_case
  i32.const 0 ;; string: "."
  nop
  call $hydra.lib.strings.split_on
  call $hydra.lib.lists.map
  local.set $parts
  i32.const 0 ;; string: "/"
  local.get $parts
  call $hydra.lib.strings.intercalate
  i32.const 0 ;; string: "."
  call $hydra.lib.strings.cat2
  nop
  call $hydra.lib.strings.cat2
)
  (func $hydra.names.normal_type_variable (param $i i32) (result i32)
  i32.const 0 ;; string: "t"
  local.get $i
  call $hydra.lib.literals.show_int32
  call $hydra.lib.strings.cat2
)
  (func $hydra.names.qname (param $ns i32) (param $name i32) (result i32)
  i32.const 3
  ;; list elements follow
  nop
  i32.const 0 ;; string: "."
  local.get $name
  call $hydra.lib.strings.cat
)
  (func $hydra.names.qualify_name (param $name i32) (result i32)
  (local $parts i32)
  i32.const 0 ;; string: "."
  nop
  call $hydra.lib.strings.split_on
  call $hydra.lib.lists.reverse
  local.set $parts
  i32.const 1
  local.get $parts
  call $hydra.lib.lists.length
  call $hydra.lib.equality.equal
  i32.const 0
  nop
  i32.const 0 ;; string: "."
  local.get $parts
  call $hydra.lib.lists.tail
  call $hydra.lib.lists.reverse
  call $hydra.lib.strings.intercalate
  local.get $parts
  call $hydra.lib.lists.head
  call $hydra.lib.logic.if_else
)
  (func $hydra.names.unique_label (param $visited i32) (param $l i32) (result i32)
  local.get $l
  local.get $visited
  call $hydra.lib.sets.member
  local.get $visited
  local.get $l
  i32.const 0 ;; string: "'"
  call $hydra.lib.strings.cat2
  call $hydra.names.unique_label
  local.get $l
  call $hydra.lib.logic.if_else
)
  (func $hydra.names.unqualify_name (param $qname i32) (result i32)
  (local $prefix i32)
  i32.const 0 ;; string: ""
  nop
  i32.const 0 ;; string: "."
  call $hydra.lib.strings.cat2
  local.get $qname
  ;; project field: namespace
  call $hydra.lib.maybes.maybe
  local.set $prefix
  local.get $prefix
  local.get $qname
  ;; project field: local
  call $hydra.lib.strings.cat2
)
)
