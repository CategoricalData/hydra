(module
  (import "hydra.analysis" "hydra.analysis.module_dependency_namespaces" (func $hydra.analysis.module_dependency_namespaces (param i32) (result i32) ) )
  (import "hydra.ext.haskell.language" "hydra.ext.haskell.language.reserved_words" (func $hydra.ext.haskell.language.reserved_words (param i32) (result i32) ) )
  (import "hydra.formatting" "hydra.formatting.capitalize" (func $hydra.formatting.capitalize (param i32) (result i32) ) )
  (import "hydra.formatting" "hydra.formatting.decapitalize" (func $hydra.formatting.decapitalize (param i32) (result i32) ) )
  (import "hydra.formatting" "hydra.formatting.sanitize_with_underscores" (func $hydra.formatting.sanitize_with_underscores (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.gt" (func $hydra.lib.equality.gt (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.head" (func $hydra.lib.lists.head (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.last" (func $hydra.lib.lists.last (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.reverse" (func $hydra.lib.lists.reverse (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.tail" (func $hydra.lib.lists.tail (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.empty" (func $hydra.lib.maps.empty (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.insert" (func $hydra.lib.maps.insert (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cases" (func $hydra.lib.maybes.cases (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.empty" (func $hydra.lib.sets.empty (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.insert" (func $hydra.lib.sets.insert (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.member" (func $hydra.lib.sets.member (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.split_on" (func $hydra.lib.strings.split_on (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.local_name_of" (func $hydra.names.local_name_of (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.qualify_name" (func $hydra.names.qualify_name (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.unqualify_name" (func $hydra.names.unqualify_name (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_type" (func $hydra.strip.deannotate_type (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.ext.haskell.utils.application_pattern" (func $hydra.ext.haskell.utils.application_pattern) )
  (export "hydra.ext.haskell.utils.element_reference" (func $hydra.ext.haskell.utils.element_reference) )
  (export "hydra.ext.haskell.utils.hsapp" (func $hydra.ext.haskell.utils.hsapp) )
  (export "hydra.ext.haskell.utils.hslambda" (func $hydra.ext.haskell.utils.hslambda) )
  (export "hydra.ext.haskell.utils.hslit" (func $hydra.ext.haskell.utils.hslit) )
  (export "hydra.ext.haskell.utils.hsvar" (func $hydra.ext.haskell.utils.hsvar) )
  (export "hydra.ext.haskell.utils.namespaces_for_module" (func $hydra.ext.haskell.utils.namespaces_for_module) )
  (export "hydra.ext.haskell.utils.newtype_accessor_name" (func $hydra.ext.haskell.utils.newtype_accessor_name) )
  (export "hydra.ext.haskell.utils.raw_name" (func $hydra.ext.haskell.utils.raw_name) )
  (export "hydra.ext.haskell.utils.record_field_reference" (func $hydra.ext.haskell.utils.record_field_reference) )
  (export "hydra.ext.haskell.utils.sanitize_haskell_name" (func $hydra.ext.haskell.utils.sanitize_haskell_name) )
  (export "hydra.ext.haskell.utils.simple_name" (func $hydra.ext.haskell.utils.simple_name) )
  (export "hydra.ext.haskell.utils.simple_value_binding" (func $hydra.ext.haskell.utils.simple_value_binding) )
  (export "hydra.ext.haskell.utils.to_type_application" (func $hydra.ext.haskell.utils.to_type_application) )
  (export "hydra.ext.haskell.utils.type_name_for_record" (func $hydra.ext.haskell.utils.type_name_for_record) )
  (export "hydra.ext.haskell.utils.union_field_reference" (func $hydra.ext.haskell.utils.union_field_reference) )
  (export "hydra.ext.haskell.utils.unpack_forall_type" (func $hydra.ext.haskell.utils.unpack_forall_type) )
  (func $hydra.ext.haskell.utils.application_pattern (param $name i32) (param $args i32) (result i32)
  local.get $name
  local.get $args
)
  (func $hydra.ext.haskell.utils.element_reference (param $namespaces i32) (param $name i32) (result i32)
  (local $alias_str i32)
  (local $esc_local i32)
  (local $gmod i32)
  (local $gname i32)
  (local $local i32)
  (local $mns i32)
  (local $namespace_pair i32)
  (local $namespaces_map i32)
  (local $ns i32)
  (local $qname i32)
  ;; project field: focus
  local.set $namespace_pair
  local.get $namespace_pair
  call $hydra.lib.pairs.first
  local.set $gname
  nop
  local.set $gmod
  ;; project field: mapping
  local.set $namespaces_map
  local.get $name
  call $hydra.names.qualify_name
  local.set $qname
  ;; project field: local
  local.set $local
  local.get $local
  call $hydra.ext.haskell.utils.sanitize_haskell_name
  local.set $esc_local
  ;; project field: namespace
  local.set $mns
  ;; project field: namespace
  local.get $local
  call $hydra.ext.haskell.utils.simple_name
  local.get $ns
  local.get $namespaces_map
  call $hydra.lib.maps.lookup
  local.get $local
  call $hydra.ext.haskell.utils.simple_name
  nop
  local.set $alias_str
  local.get $ns
  local.get $gname
  call $hydra.lib.equality.equal
  local.get $esc_local
  call $hydra.ext.haskell.utils.simple_name
  i32.const 3
  ;; list elements follow
  local.get $alias_str
  i32.const 0 ;; string: "."
  local.get $local
  call $hydra.ext.haskell.utils.sanitize_haskell_name
  call $hydra.lib.strings.cat
  call $hydra.ext.haskell.utils.raw_name
  call $hydra.lib.logic.if_else
  call $hydra.lib.maybes.cases
  call $hydra.lib.maybes.cases
)
  (func $hydra.ext.haskell.utils.hsapp (param $l i32) (param $r i32) (result i32)
  local.get $l
  local.get $r
)
  (func $hydra.ext.haskell.utils.hslambda (param $name i32) (param $rhs i32) (result i32)
  i32.const 1
  ;; list elements follow
  local.get $name
  local.get $rhs
)
  (func $hydra.ext.haskell.utils.hslit (param $lit i32) (result i32)
  local.get $lit
)
  (func $hydra.ext.haskell.utils.hsvar (param $s i32) (result i32)
  local.get $s
  call $hydra.ext.haskell.utils.raw_name
)
  (func $hydra.ext.haskell.utils.namespaces_for_module (param $mod i32) (param $cx i32) (param $g i32) (result i32)
  (local $add_pair i32)
  (local $alias i32)
  (local $alias_str i32)
  (local $capitalized i32)
  (local $current_map i32)
  (local $current_set i32)
  (local $empty_state i32)
  (local $final_state i32)
  (local $focus_pair i32)
  (local $last_part i32)
  (local $name i32)
  (local $name_pair i32)
  (local $namespace_str i32)
  (local $ns i32)
  (local $nss i32)
  (local $nss_as_list i32)
  (local $nss_pairs i32)
  (local $parts i32)
  (local $result_map i32)
  (local $state i32)
  (local $to_module_name i32)
  (local $to_pair i32)
  local.get $cx
  local.get $g
  i32.const 1
  i32.const 1
  i32.const 1
  i32.const 1
  local.get $mod
  call $hydra.analysis.module_dependency_namespaces
  ;; project field: namespace
  local.set $ns
  nop
  local.set $namespace_str
  i32.const 0 ;; string: "."
  local.get $namespace_str
  call $hydra.lib.strings.split_on
  local.set $parts
  local.get $parts
  call $hydra.lib.lists.last
  local.set $last_part
  local.get $last_part
  call $hydra.formatting.capitalize
  local.set $capitalized
  local.get $capitalized
  local.set $to_module_name
  local.get $name
  local.get $name
  local.get $to_module_name
  local.set $to_pair
  local.get $state
  call $hydra.lib.pairs.first
  local.set $current_map
  local.get $state
  call $hydra.lib.pairs.second
  local.set $current_set
  local.get $name_pair
  call $hydra.lib.pairs.first
  local.set $name
  local.get $name_pair
  call $hydra.lib.pairs.second
  local.set $alias
  nop
  local.set $alias_str
  local.get $alias
  local.get $current_set
  call $hydra.lib.sets.member
  local.get $state
  local.get $name
  local.get $alias_str
  i32.const 0 ;; string: "_"
  call $hydra.lib.strings.cat2
  local.get $add_pair
  local.get $name
  local.get $alias
  local.get $current_map
  call $hydra.lib.maps.insert
  local.get $alias
  local.get $current_set
  call $hydra.lib.sets.insert
  call $hydra.lib.logic.if_else
  local.set $add_pair
  local.get $ns
  local.get $to_pair
  local.set $focus_pair
  local.get $nss
  call $hydra.lib.sets.to_list
  local.set $nss_as_list
  local.get $to_pair
  local.get $nss_as_list
  call $hydra.lib.lists.map
  local.set $nss_pairs
  call $hydra.lib.maps.empty
  call $hydra.lib.sets.empty
  local.set $empty_state
  local.get $add_pair
  local.get $empty_state
  local.get $nss_pairs
  call $hydra.lib.lists.foldl
  local.set $final_state
  local.get $final_state
  call $hydra.lib.pairs.first
  local.set $result_map
  i32.const 1
  local.get $focus_pair
  local.get $result_map
  call $hydra.lib.eithers.bind
)
  (func $hydra.ext.haskell.utils.newtype_accessor_name (param $name i32) (result i32)
  i32.const 0 ;; string: "un"
  local.get $name
  call $hydra.names.local_name_of
  call $hydra.lib.strings.cat2
)
  (func $hydra.ext.haskell.utils.raw_name (param $n i32) (result i32)
  i32.const 0
  ;; list elements follow
  local.get $n
)
  (func $hydra.ext.haskell.utils.record_field_reference (param $namespaces i32) (param $sname i32) (param $fname i32) (result i32)
  (local $capitalized i32)
  (local $decapitalized i32)
  (local $fname_str i32)
  (local $nm i32)
  (local $ns i32)
  (local $qname i32)
  (local $qual_name i32)
  (local $type_name_str i32)
  (local $unqual_name i32)
  nop
  local.set $fname_str
  local.get $sname
  call $hydra.names.qualify_name
  local.set $qname
  ;; project field: namespace
  local.set $ns
  local.get $sname
  call $hydra.ext.haskell.utils.type_name_for_record
  local.set $type_name_str
  local.get $type_name_str
  call $hydra.formatting.decapitalize
  local.set $decapitalized
  local.get $fname_str
  call $hydra.formatting.capitalize
  local.set $capitalized
  local.get $decapitalized
  local.get $capitalized
  call $hydra.lib.strings.cat2
  local.set $nm
  local.get $ns
  local.get $nm
  local.set $qual_name
  local.get $qual_name
  call $hydra.names.unqualify_name
  local.set $unqual_name
  local.get $namespaces
  local.get $unqual_name
  call $hydra.ext.haskell.utils.element_reference
)
  (func $hydra.ext.haskell.utils.sanitize_haskell_name (result i32)
  call $hydra.ext.haskell.language.reserved_words
  call $hydra.formatting.sanitize_with_underscores
)
  (func $hydra.ext.haskell.utils.simple_name (param $arg_ i32) (result i32)
  local.get $arg_
  call $hydra.ext.haskell.utils.sanitize_haskell_name
  call $hydra.ext.haskell.utils.raw_name
)
  (func $hydra.ext.haskell.utils.simple_value_binding (param $hname i32) (param $rhs i32) (param $bindings i32) (result i32)
  (local $pat i32)
  (local $right_hand_side i32)
  local.get $hname
  i32.const 0
  ;; list elements follow
  local.set $pat
  local.get $rhs
  local.set $right_hand_side
  local.get $pat
  local.get $right_hand_side
  local.get $bindings
)
  (func $hydra.ext.haskell.utils.to_type_application (param $types i32) (result i32)
  (local $app i32)
  (local $l i32)
  local.get $l
  call $hydra.lib.lists.length
  i32.const 1
  call $hydra.lib.equality.gt
  local.get $l
  call $hydra.lib.lists.tail
  local.get $app
  local.get $l
  call $hydra.lib.lists.head
  local.get $l
  call $hydra.lib.lists.head
  call $hydra.lib.logic.if_else
  local.set $app
  local.get $types
  call $hydra.lib.lists.reverse
  local.get $app
)
  (func $hydra.ext.haskell.utils.type_name_for_record (param $sname i32) (result i32)
  (local $parts i32)
  (local $sname_str i32)
  nop
  local.set $sname_str
  i32.const 0 ;; string: "."
  local.get $sname_str
  call $hydra.lib.strings.split_on
  local.set $parts
  local.get $parts
  call $hydra.lib.lists.last
)
  (func $hydra.ext.haskell.utils.union_field_reference (param $bound_names i32) (param $namespaces i32) (param $sname i32) (param $fname i32) (result i32)
  (local $capitalized_field_name i32)
  (local $capitalized_type_name i32)
  (local $deconflict i32)
  (local $fname_str i32)
  (local $name i32)
  (local $nm i32)
  (local $ns i32)
  (local $qname i32)
  (local $qual_name i32)
  (local $tname i32)
  (local $type_name_str i32)
  (local $unqual_name i32)
  nop
  local.set $fname_str
  local.get $sname
  call $hydra.names.qualify_name
  local.set $qname
  ;; project field: namespace
  local.set $ns
  local.get $sname
  call $hydra.ext.haskell.utils.type_name_for_record
  local.set $type_name_str
  local.get $type_name_str
  call $hydra.formatting.capitalize
  local.set $capitalized_type_name
  local.get $fname_str
  call $hydra.formatting.capitalize
  local.set $capitalized_field_name
  local.get $ns
  local.get $name
  call $hydra.names.unqualify_name
  local.set $tname
  local.get $tname
  local.get $bound_names
  call $hydra.lib.sets.member
  local.get $name
  i32.const 0 ;; string: "_"
  call $hydra.lib.strings.cat2
  local.get $deconflict
  local.get $name
  call $hydra.lib.logic.if_else
  local.set $deconflict
  local.get $capitalized_type_name
  local.get $capitalized_field_name
  call $hydra.lib.strings.cat2
  local.get $deconflict
  local.set $nm
  local.get $ns
  local.get $nm
  local.set $qual_name
  local.get $qual_name
  call $hydra.names.unqualify_name
  local.set $unqual_name
  local.get $namespaces
  local.get $unqual_name
  call $hydra.ext.haskell.utils.element_reference
)
  (func $hydra.ext.haskell.utils.unpack_forall_type (param $t i32) (result i32)
  (local $final_type i32)
  (local $recursive_result i32)
  (local $tbody i32)
  (local $v i32)
  (local $vars i32)
  (block $end_type (result i32)
  (block $forall
  local.get $t
  call $hydra.strip.deannotate_type
  br_table $forall $end_type
  ;; project field: parameter
  local.set $v
  ;; project field: body
  local.set $tbody
  local.get $tbody
  call $hydra.ext.haskell.utils.unpack_forall_type
  local.set $recursive_result
  local.get $recursive_result
  call $hydra.lib.pairs.first
  local.set $vars
  local.get $recursive_result
  call $hydra.lib.pairs.second
  local.set $final_type
  local.get $v
  local.get $vars
  call $hydra.lib.lists.cons
  local.get $final_type
  br $end_type
)
)
)
)
