(module
  (import "hydra.analysis" "hydra.analysis.module_contains_binary_literals" (func $hydra.analysis.module_contains_binary_literals (param i32) (result i32) ) )
  (import "hydra.annotations" "hydra.annotations.get_term_description" (func $hydra.annotations.get_term_description (param i32) (result i32) ) )
  (import "hydra.annotations" "hydra.annotations.get_type_description" (func $hydra.annotations.get_type_description (param i32) (result i32) ) )
  (import "hydra.constants" "hydra.constants.ignored_variable" (func $hydra.constants.ignored_variable (param i32) (result i32) ) )
  (import "hydra.dependencies" "hydra.dependencies.simplify_term" (func $hydra.dependencies.simplify_term (param i32) (result i32) ) )
  (import "hydra.encode.core" "hydra.encode.core.type" (func $hydra.encode.core.type (param i32) (result i32) ) )
  (import "hydra.ext.haskell.serde" "hydra.ext.haskell.serde.module_to_expr" (func $hydra.ext.haskell.serde.module_to_expr (param i32) (result i32) ) )
  (import "hydra.ext.haskell.utils" "hydra.ext.haskell.utils.application_pattern" (func $hydra.ext.haskell.utils.application_pattern (param i32) (result i32) ) )
  (import "hydra.ext.haskell.utils" "hydra.ext.haskell.utils.element_reference" (func $hydra.ext.haskell.utils.element_reference (param i32) (result i32) ) )
  (import "hydra.ext.haskell.utils" "hydra.ext.haskell.utils.hsapp" (func $hydra.ext.haskell.utils.hsapp (param i32) (result i32) ) )
  (import "hydra.ext.haskell.utils" "hydra.ext.haskell.utils.hslambda" (func $hydra.ext.haskell.utils.hslambda (param i32) (result i32) ) )
  (import "hydra.ext.haskell.utils" "hydra.ext.haskell.utils.hslit" (func $hydra.ext.haskell.utils.hslit (param i32) (result i32) ) )
  (import "hydra.ext.haskell.utils" "hydra.ext.haskell.utils.hsvar" (func $hydra.ext.haskell.utils.hsvar (param i32) (result i32) ) )
  (import "hydra.ext.haskell.utils" "hydra.ext.haskell.utils.namespaces_for_module" (func $hydra.ext.haskell.utils.namespaces_for_module (param i32) (result i32) ) )
  (import "hydra.ext.haskell.utils" "hydra.ext.haskell.utils.newtype_accessor_name" (func $hydra.ext.haskell.utils.newtype_accessor_name (param i32) (result i32) ) )
  (import "hydra.ext.haskell.utils" "hydra.ext.haskell.utils.raw_name" (func $hydra.ext.haskell.utils.raw_name (param i32) (result i32) ) )
  (import "hydra.ext.haskell.utils" "hydra.ext.haskell.utils.record_field_reference" (func $hydra.ext.haskell.utils.record_field_reference (param i32) (result i32) ) )
  (import "hydra.ext.haskell.utils" "hydra.ext.haskell.utils.simple_name" (func $hydra.ext.haskell.utils.simple_name (param i32) (result i32) ) )
  (import "hydra.ext.haskell.utils" "hydra.ext.haskell.utils.simple_value_binding" (func $hydra.ext.haskell.utils.simple_value_binding (param i32) (result i32) ) )
  (import "hydra.ext.haskell.utils" "hydra.ext.haskell.utils.to_type_application" (func $hydra.ext.haskell.utils.to_type_application (param i32) (result i32) ) )
  (import "hydra.ext.haskell.utils" "hydra.ext.haskell.utils.union_field_reference" (func $hydra.ext.haskell.utils.union_field_reference (param i32) (result i32) ) )
  (import "hydra.ext.haskell.utils" "hydra.ext.haskell.utils.unpack_forall_type" (func $hydra.ext.haskell.utils.unpack_forall_type (param i32) (result i32) ) )
  (import "hydra.formatting" "hydra.formatting.capitalize" (func $hydra.formatting.capitalize (param i32) (result i32) ) )
  (import "hydra.formatting" "hydra.formatting.decapitalize" (func $hydra.formatting.decapitalize (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.fields_of" (func $hydra.lexical.fields_of (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat" (func $hydra.lib.lists.concat (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.head" (func $hydra.lib.lists.head (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.reverse" (func $hydra.lib.lists.reverse (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.tail" (func $hydra.lib.lists.tail (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.zip_with" (func $hydra.lib.lists.zip_with (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigfloat_to_float64" (func $hydra.lib.literals.bigfloat_to_float64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.binary_to_string" (func $hydra.lib.literals.binary_to_string (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.int16_to_bigint" (func $hydra.lib.literals.int16_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.int64_to_bigint" (func $hydra.lib.literals.int64_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.int8_to_bigint" (func $hydra.lib.literals.int8_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int32" (func $hydra.lib.literals.show_int32 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_string" (func $hydra.lib.literals.show_string (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.uint16_to_bigint" (func $hydra.lib.literals.uint16_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.uint32_to_bigint" (func $hydra.lib.literals.uint32_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.uint64_to_bigint" (func $hydra.lib.literals.uint64_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.uint8_to_bigint" (func $hydra.lib.literals.uint8_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.empty" (func $hydra.lib.maps.empty (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.keys" (func $hydra.lib.maps.keys (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.map" (func $hydra.lib.maps.map (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.null" (func $hydra.lib.maps.null (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.singleton" (func $hydra.lib.maps.singleton (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.union" (func $hydra.lib.maps.union (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.add" (func $hydra.lib.math.add (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.bind" (func $hydra.lib.maybes.bind (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cases" (func $hydra.lib.maybes.cases (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cat" (func $hydra.lib.maybes.cat (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.from_just" (func $hydra.lib.maybes.from_just (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.from_maybe" (func $hydra.lib.maybes.from_maybe (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.is_just" (func $hydra.lib.maybes.is_just (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.is_nothing" (func $hydra.lib.maybes.is_nothing (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.empty" (func $hydra.lib.sets.empty (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.insert" (func $hydra.lib.sets.insert (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.member" (func $hydra.lib.sets.member (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.null" (func $hydra.lib.sets.null (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.union" (func $hydra.lib.sets.union (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.intercalate" (func $hydra.lib.strings.intercalate (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.split_on" (func $hydra.lib.strings.split_on (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.local_name_of" (func $hydra.names.local_name_of (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.namespace_of" (func $hydra.names.namespace_of (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.namespace_to_file_path" (func $hydra.names.namespace_to_file_path (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.qname" (func $hydra.names.qname (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.qualify_name" (func $hydra.names.qualify_name (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.unqualify_name" (func $hydra.names.unqualify_name (param i32) (result i32) ) )
  (import "hydra.predicates" "hydra.predicates.is_serializable_by_name" (func $hydra.predicates.is_serializable_by_name (param i32) (result i32) ) )
  (import "hydra.resolution" "hydra.resolution.require_union_field" (func $hydra.resolution.require_union_field (param i32) (result i32) ) )
  (import "hydra.resolution" "hydra.resolution.require_union_type" (func $hydra.resolution.require_union_type (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.fold_over_term" (func $hydra.rewriting.fold_over_term (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.fold_over_type" (func $hydra.rewriting.fold_over_type (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_term" (func $hydra.rewriting.rewrite_term (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.parenthesize" (func $hydra.serialization.parenthesize (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.print_expr" (func $hydra.serialization.print_expr (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_term" (func $hydra.strip.deannotate_term (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_type" (func $hydra.strip.deannotate_type (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.is_free_variable_in_term" (func $hydra.variables.is_free_variable_in_term (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.ext.haskell.coder.adapt_type_to_haskell_and_encode" (func $hydra.ext.haskell.coder.adapt_type_to_haskell_and_encode) )
  (export "hydra.ext.haskell.coder.constant_for_field_name" (func $hydra.ext.haskell.coder.constant_for_field_name) )
  (export "hydra.ext.haskell.coder.constant_for_type_name" (func $hydra.ext.haskell.coder.constant_for_type_name) )
  (export "hydra.ext.haskell.coder.construct_module" (func $hydra.ext.haskell.coder.construct_module) )
  (export "hydra.ext.haskell.coder.empty_metadata" (func $hydra.ext.haskell.coder.empty_metadata) )
  (export "hydra.ext.haskell.coder.encode_case_expression" (func $hydra.ext.haskell.coder.encode_case_expression) )
  (export "hydra.ext.haskell.coder.encode_function" (func $hydra.ext.haskell.coder.encode_function) )
  (export "hydra.ext.haskell.coder.encode_literal" (func $hydra.ext.haskell.coder.encode_literal) )
  (export "hydra.ext.haskell.coder.encode_term" (func $hydra.ext.haskell.coder.encode_term) )
  (export "hydra.ext.haskell.coder.encode_type" (func $hydra.ext.haskell.coder.encode_type) )
  (export "hydra.ext.haskell.coder.encode_type_with_class_assertions" (func $hydra.ext.haskell.coder.encode_type_with_class_assertions) )
  (export "hydra.ext.haskell.coder.extend_meta_for_term" (func $hydra.ext.haskell.coder.extend_meta_for_term) )
  (export "hydra.ext.haskell.coder.extend_meta_for_type" (func $hydra.ext.haskell.coder.extend_meta_for_type) )
  (export "hydra.ext.haskell.coder.find_ord_variables" (func $hydra.ext.haskell.coder.find_ord_variables) )
  (export "hydra.ext.haskell.coder.gather_metadata" (func $hydra.ext.haskell.coder.gather_metadata) )
  (export "hydra.ext.haskell.coder.get_implicit_type_classes" (func $hydra.ext.haskell.coder.get_implicit_type_classes) )
  (export "hydra.ext.haskell.coder.include_type_definitions" (func $hydra.ext.haskell.coder.include_type_definitions) )
  (export "hydra.ext.haskell.coder.key_haskell_var" (func $hydra.ext.haskell.coder.key_haskell_var) )
  (export "hydra.ext.haskell.coder.module_to_haskell" (func $hydra.ext.haskell.coder.module_to_haskell) )
  (export "hydra.ext.haskell.coder.module_to_haskell_module" (func $hydra.ext.haskell.coder.module_to_haskell_module) )
  (export "hydra.ext.haskell.coder.name_decls" (func $hydra.ext.haskell.coder.name_decls) )
  (export "hydra.ext.haskell.coder.set_meta_uses_byte_string" (func $hydra.ext.haskell.coder.set_meta_uses_byte_string) )
  (export "hydra.ext.haskell.coder.set_meta_uses_int" (func $hydra.ext.haskell.coder.set_meta_uses_int) )
  (export "hydra.ext.haskell.coder.set_meta_uses_map" (func $hydra.ext.haskell.coder.set_meta_uses_map) )
  (export "hydra.ext.haskell.coder.set_meta_uses_set" (func $hydra.ext.haskell.coder.set_meta_uses_set) )
  (export "hydra.ext.haskell.coder.to_data_declaration" (func $hydra.ext.haskell.coder.to_data_declaration) )
  (export "hydra.ext.haskell.coder.to_type_declarations_from" (func $hydra.ext.haskell.coder.to_type_declarations_from) )
  (export "hydra.ext.haskell.coder.type_decl" (func $hydra.ext.haskell.coder.type_decl) )
  (export "hydra.ext.haskell.coder.type_scheme_constraints_to_class_map" (func $hydra.ext.haskell.coder.type_scheme_constraints_to_class_map) )
  (export "hydra.ext.haskell.coder.use_core_import" (func $hydra.ext.haskell.coder.use_core_import) )
  (func $hydra.ext.haskell.coder.adapt_type_to_haskell_and_encode (param $namespaces i32) (param $typ i32) (param $cx i32) (param $g i32) (result i32)
  (local $enc i32)
  (local $t i32)
  local.get $namespaces
  local.get $t
  local.get $cx
  local.get $g
  call $hydra.ext.haskell.coder.encode_type
  local.set $enc
  (block $end_type (result i32)
  (block $variable
  local.get $typ
  call $hydra.strip.deannotate_type
  br_table $variable $end_type
  local.get $typ
  local.get $enc
  br $end_type
)
)
)
  (func $hydra.ext.haskell.coder.constant_for_field_name (param $tname i32) (param $fname i32) (result i32)
  i32.const 4
  ;; list elements follow
  i32.const 0 ;; string: "_"
  local.get $tname
  call $hydra.names.local_name_of
  i32.const 0 ;; string: "_"
  nop
  call $hydra.lib.strings.cat
)
  (func $hydra.ext.haskell.coder.constant_for_type_name (param $tname i32) (result i32)
  i32.const 0 ;; string: "_"
  local.get $tname
  call $hydra.names.local_name_of
  call $hydra.lib.strings.cat2
)
  (func $hydra.ext.haskell.coder.construct_module (param $namespaces i32) (param $mod i32) (param $defs i32) (param $cx i32) (param $g i32) (result i32)
  (local $alias i32)
  (local $cond_import i32)
  (local $create_declarations i32)
  (local $d i32)
  (local $decl_lists i32)
  (local $decls i32)
  (local $def i32)
  (local $domain_imports i32)
  (local $flag i32)
  (local $h i32)
  (local $hidden i32)
  (local $import_name i32)
  (local $imports i32)
  (local $malias i32)
  (local $mc i32)
  (local $meta i32)
  (local $n i32)
  (local $name i32)
  (local $namespace i32)
  (local $pair i32)
  (local $spec i32)
  (local $standard_imports i32)
  (local $term i32)
  (local $to_import i32)
  (local $triple i32)
  (local $typ i32)
  (local $x i32)
  nop
  local.set $h
  (block $end_definition (result i32)
  (block $term
  (block $type
  local.get $def
  br_table $type $term $end_definition
  ;; project field: name
  local.set $name
  ;; project field: type
  local.set $typ
  local.get $namespaces
  local.get $name
  local.get $typ
  local.get $cx
  local.get $g
  call $hydra.ext.haskell.coder.to_type_declarations_from
  br $end_definition
)
  local.get $namespaces
  local.get $term
  local.get $cx
  local.get $g
  call $hydra.ext.haskell.coder.to_data_declaration
  i32.const 1
  i32.const 1
  ;; list elements follow
  local.get $d
  call $hydra.lib.eithers.bind
  br $end_definition
)
)
  local.set $create_declarations
  i32.const 0 ;; string: "."
  call $hydra.formatting.capitalize
  i32.const 0 ;; string: "."
  local.get $name
  call $hydra.lib.strings.split_on
  call $hydra.lib.lists.map
  call $hydra.lib.strings.intercalate
  local.set $import_name
  local.get $domain_imports
  local.get $standard_imports
  call $hydra.lib.lists.concat2
  local.set $imports
  local.get $pair
  call $hydra.lib.pairs.first
  local.set $namespace
  local.get $pair
  call $hydra.lib.pairs.second
  local.set $alias
  local.get $namespace
  local.get $h
  local.set $name
  i32.const 1
  local.get $name
  local.get $import_name
  local.get $alias
  i32.const 0
  local.set $to_import
  local.get $to_import
  ;; project field: mapping
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.map
  local.set $domain_imports
  local.get $defs
  call $hydra.ext.haskell.coder.gather_metadata
  local.set $meta
  local.get $flag
  i32.const 1
  ;; list elements follow
  local.get $triple
  i32.const 0
  ;; list elements follow
  call $hydra.lib.logic.if_else
  local.set $cond_import
  local.get $triple
  call $hydra.lib.pairs.first
  call $hydra.lib.pairs.first
  local.set $name
  local.get $triple
  call $hydra.lib.pairs.first
  call $hydra.lib.pairs.second
  local.set $malias
  local.get $triple
  call $hydra.lib.pairs.second
  local.set $hidden
  local.get $hidden
  call $hydra.lib.lists.null
  i32.const 0
  i32.const 0
  local.get $n
  call $hydra.ext.haskell.utils.simple_name
  i32.const 0
  local.get $hidden
  call $hydra.lib.lists.map
  call $hydra.lib.logic.if_else
  local.set $spec
  local.get $malias
  call $hydra.lib.maybes.is_just
  local.get $name
  local.get $x
  local.get $malias
  call $hydra.lib.maybes.map
  local.get $spec
  local.set $to_import
  local.get $to_import
  i32.const 6
  ;; list elements follow
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "Prelude"
  i32.const 0
  i32.const 8
  ;; list elements follow
  i32.const 0 ;; string: "Enum"
  i32.const 0 ;; string: "Ordering"
  i32.const 0 ;; string: "decodeFloat"
  i32.const 0 ;; string: "encodeFloat"
  i32.const 0 ;; string: "fail"
  i32.const 0 ;; string: "map"
  i32.const 0 ;; string: "pure"
  i32.const 0 ;; string: "sum"
  ;; project field: uses_byte_string
  i32.const 0 ;; string: "Data.ByteString"
  i32.const 0 ;; string: "B"
  i32.const 0
  ;; list elements follow
  local.get $cond_import
  ;; project field: uses_int
  i32.const 0 ;; string: "Data.Int"
  i32.const 0 ;; string: "I"
  i32.const 0
  ;; list elements follow
  local.get $cond_import
  ;; project field: uses_map
  i32.const 0 ;; string: "Data.Map"
  i32.const 0 ;; string: "M"
  i32.const 0
  ;; list elements follow
  local.get $cond_import
  ;; project field: uses_set
  i32.const 0 ;; string: "Data.Set"
  i32.const 0 ;; string: "S"
  i32.const 0
  ;; list elements follow
  local.get $cond_import
  local.get $mod
  call $hydra.analysis.module_contains_binary_literals
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "Hydra.Lib.Literals"
  i32.const 0 ;; string: "Literals"
  i32.const 0
  ;; list elements follow
  i32.const 0
  ;; list elements follow
  call $hydra.lib.logic.if_else
  call $hydra.lib.lists.concat
  call $hydra.lib.lists.map
  local.set $standard_imports
  local.get $create_declarations
  local.get $defs
  call $hydra.lib.eithers.map_list
  local.get $decl_lists
  call $hydra.lib.lists.concat
  local.set $decls
  ;; project field: description
  local.set $mc
  i32.const 1
  local.get $mc
  ;; project field: namespace
  local.get $h
  local.get $import_name
  i32.const 0
  ;; list elements follow
  local.get $imports
  local.get $decls
  call $hydra.lib.eithers.bind
)
  (func $hydra.ext.haskell.coder.empty_metadata (result i32)
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
)
  (func $hydra.ext.haskell.coder.encode_case_expression (param $depth i32) (param $namespaces i32) (param $stmt i32) (param $scrutinee i32) (param $cx i32) (param $g i32) (result i32)
  (local $alt i32)
  (local $args i32)
  (local $cs i32)
  (local $d i32)
  (local $dcases i32)
  (local $def i32)
  (local $dn i32)
  (local $ecases i32)
  (local $f i32)
  (local $field_map i32)
  (local $fields i32)
  (local $fn i32)
  (local $ft i32)
  (local $fun' i32)
  (local $hname i32)
  (local $lhs i32)
  (local $no_args i32)
  (local $raw i32)
  (local $rhs i32)
  (local $rhs_term i32)
  (local $rt i32)
  (local $single_arg i32)
  (local $to_alt i32)
  (local $to_field_map_entry i32)
  (local $v0 i32)
  (local $v1 i32)
  (local $x i32)
  ;; project field: type_name
  local.set $dn
  ;; project field: default
  local.set $def
  ;; project field: cases
  local.set $fields
  ;; project field: name
  local.set $fn
  ;; project field: term
  local.set $fun'
  i32.const 0 ;; string: "v"
  local.get $depth
  call $hydra.lib.literals.show_int32
  call $hydra.lib.strings.cat2
  local.set $v0
  local.get $fun'
  local.get $v0
  local.set $raw
  local.get $raw
  call $hydra.dependencies.simplify_term
  local.set $rhs_term
  local.get $v0
  local.get $rhs_term
  call $hydra.variables.is_free_variable_in_term
  call $hydra.constants.ignored_variable
  local.get $v0
  call $hydra.lib.logic.if_else
  local.set $v1
  ;; project field: bound_terms
  call $hydra.lib.maps.keys
  call $hydra.lib.sets.from_list
  ;; project field: schema_types
  call $hydra.lib.maps.keys
  call $hydra.lib.sets.from_list
  call $hydra.lib.sets.union
  local.get $namespaces
  local.get $dn
  local.get $fn
  call $hydra.ext.haskell.utils.union_field_reference
  local.set $hname
  local.get $fn
  local.get $field_map
  call $hydra.lib.maps.lookup
  i32.const 0
  i32.const 4
  ;; list elements follow
  i32.const 0 ;; string: "field "
  nop
  call $hydra.lib.literals.show_string
  i32.const 0 ;; string: " not found in "
  nop
  call $hydra.lib.literals.show_string
  call $hydra.lib.strings.cat
  local.get $cx
  ;; project field: type
  local.set $ft
  i32.const 0
  ;; list elements follow
  local.set $no_args
  i32.const 1
  ;; list elements follow
  local.get $v1
  call $hydra.ext.haskell.utils.raw_name
  local.set $single_arg
  (block $end_type (result i32)
  (block $unit
  local.get $ft
  call $hydra.strip.deannotate_type
  br_table $unit $end_type
  i32.const 1
  local.get $no_args
  br $end_type
)
)
  call $hydra.lib.maybes.cases
  local.get $hname
  local.get $args
  call $hydra.ext.haskell.utils.application_pattern
  local.set $lhs
  local.get $x
  local.get $depth
  i32.const 1
  call $hydra.lib.math.add
  local.get $namespaces
  local.get $rhs_term
  local.get $cx
  local.get $g
  call $hydra.ext.haskell.coder.encode_term
  call $hydra.lib.eithers.map
  i32.const 1
  local.get $lhs
  local.get $rhs
  i32.const 0
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  local.set $to_alt
  local.get $cx
  local.get $g
  local.get $dn
  call $hydra.resolution.require_union_type
  ;; project field: name
  local.get $f
  local.set $to_field_map_entry
  local.get $to_field_map_entry
  local.get $rt
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $field_map
  local.get $field_map
  local.get $to_alt
  local.get $fields
  call $hydra.lib.eithers.map_list
  local.get $def
  i32.const 1
  i32.const 0
  ;; list elements follow
  local.get $x
  local.get $depth
  local.get $namespaces
  local.get $d
  local.get $cx
  local.get $g
  call $hydra.ext.haskell.coder.encode_term
  call $hydra.lib.eithers.map
  call $hydra.constants.ignored_variable
  call $hydra.ext.haskell.utils.raw_name
  local.set $lhs
  local.get $lhs
  local.get $cs
  i32.const 0
  local.set $alt
  i32.const 1
  i32.const 1
  ;; list elements follow
  local.get $alt
  call $hydra.lib.eithers.bind
  call $hydra.lib.maybes.cases
  i32.const 1
  local.get $scrutinee
  local.get $ecases
  local.get $dcases
  call $hydra.lib.lists.concat2
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.ext.haskell.coder.encode_function (param $depth i32) (param $namespaces i32) (param $fun i32) (param $cx i32) (param $g i32) (result i32)
  (local $body i32)
  (local $dn i32)
  (local $e i32)
  (local $fname i32)
  (local $hbody i32)
  (local $name i32)
  (local $stmt i32)
  (local $v i32)
  (block $end_function (result i32)
  (block $lambda
  (block $elimination
  local.get $fun
  br_table $elimination $lambda $end_function
  (block $end_elimination (result i32)
  (block $union
  (block $record
  (block $wrap
  local.get $e
  br_table $wrap $record $union $end_elimination
  i32.const 1
  local.get $namespaces
  local.get $name
  call $hydra.names.namespace_of
  call $hydra.lib.maybes.from_just
  local.get $name
  call $hydra.ext.haskell.utils.newtype_accessor_name
  call $hydra.names.qname
  call $hydra.ext.haskell.utils.element_reference
  br $end_elimination
)
  ;; project field: type_name
  local.set $dn
  ;; project field: field
  local.set $fname
  i32.const 1
  local.get $namespaces
  local.get $dn
  local.get $fname
  call $hydra.ext.haskell.utils.record_field_reference
  br $end_elimination
)
  i32.const 0 ;; string: "x"
  call $hydra.ext.haskell.utils.raw_name
  call $hydra.ext.haskell.utils.hslambda
  local.get $depth
  local.get $namespaces
  local.get $stmt
  i32.const 0 ;; string: "x"
  call $hydra.ext.haskell.utils.hsvar
  local.get $cx
  local.get $g
  call $hydra.ext.haskell.coder.encode_case_expression
  call $hydra.lib.eithers.map
  br $end_elimination
)
)
  br $end_function
)
  ;; project field: parameter
  local.set $v
  ;; project field: body
  local.set $body
  local.get $depth
  local.get $namespaces
  local.get $body
  local.get $cx
  local.get $g
  call $hydra.ext.haskell.coder.encode_term
  i32.const 1
  local.get $namespaces
  local.get $v
  call $hydra.ext.haskell.utils.element_reference
  local.get $hbody
  call $hydra.ext.haskell.utils.hslambda
  call $hydra.lib.eithers.bind
  br $end_function
)
)
)
  (func $hydra.ext.haskell.coder.encode_literal (param $l i32) (param $cx i32) (result i32)
  (local $b i32)
  (local $bs i32)
  (local $f i32)
  (local $fv i32)
  (local $i i32)
  (local $iv i32)
  (local $s i32)
  (block $end_literal (result i32)
  (block $string
  (block $integer
  (block $float
  (block $boolean
  (block $binary
  local.get $l
  br_table $binary $boolean $float $integer $string $end_literal
  i32.const 1
  i32.const 0 ;; string: "Literals.stringToBinary"
  call $hydra.ext.haskell.utils.hsvar
  local.get $bs
  call $hydra.lib.literals.binary_to_string
  call $hydra.ext.haskell.utils.hslit
  call $hydra.ext.haskell.utils.hsapp
  br $end_literal
)
  i32.const 1
  local.get $b
  i32.const 0 ;; string: "True"
  i32.const 0 ;; string: "False"
  call $hydra.lib.logic.if_else
  call $hydra.ext.haskell.utils.hsvar
  br $end_literal
)
  (block $end_float_value (result i32)
  (block $bigfloat
  (block $float64
  (block $float32
  local.get $fv
  br_table $float32 $float64 $bigfloat $end_float_value
  i32.const 1
  local.get $f
  call $hydra.ext.haskell.utils.hslit
  br $end_float_value
)
  i32.const 1
  local.get $f
  call $hydra.ext.haskell.utils.hslit
  br $end_float_value
)
  i32.const 1
  local.get $f
  call $hydra.lib.literals.bigfloat_to_float64
  call $hydra.ext.haskell.utils.hslit
  br $end_float_value
)
)
  br $end_literal
)
  (block $end_integer_value (result i32)
  (block $uint64
  (block $uint32
  (block $uint16
  (block $uint8
  (block $int64
  (block $int32
  (block $int16
  (block $int8
  (block $bigint
  local.get $iv
  br_table $bigint $int8 $int16 $int32 $int64 $uint8 $uint16 $uint32 $uint64 $end_integer_value
  i32.const 1
  local.get $i
  call $hydra.ext.haskell.utils.hslit
  br $end_integer_value
)
  i32.const 1
  local.get $i
  call $hydra.lib.literals.int8_to_bigint
  call $hydra.ext.haskell.utils.hslit
  br $end_integer_value
)
  i32.const 1
  local.get $i
  call $hydra.lib.literals.int16_to_bigint
  call $hydra.ext.haskell.utils.hslit
  br $end_integer_value
)
  i32.const 1
  local.get $i
  call $hydra.ext.haskell.utils.hslit
  br $end_integer_value
)
  i32.const 1
  local.get $i
  call $hydra.lib.literals.int64_to_bigint
  call $hydra.ext.haskell.utils.hslit
  br $end_integer_value
)
  i32.const 1
  local.get $i
  call $hydra.lib.literals.uint8_to_bigint
  call $hydra.ext.haskell.utils.hslit
  br $end_integer_value
)
  i32.const 1
  local.get $i
  call $hydra.lib.literals.uint16_to_bigint
  call $hydra.ext.haskell.utils.hslit
  br $end_integer_value
)
  i32.const 1
  local.get $i
  call $hydra.lib.literals.uint32_to_bigint
  call $hydra.ext.haskell.utils.hslit
  br $end_integer_value
)
  i32.const 1
  local.get $i
  call $hydra.lib.literals.uint64_to_bigint
  call $hydra.ext.haskell.utils.hslit
  br $end_integer_value
)
)
  br $end_literal
)
  i32.const 1
  local.get $s
  call $hydra.ext.haskell.utils.hslit
  br $end_literal
)
)
)
  (func $hydra.ext.haskell.coder.encode_term (param $depth i32) (param $namespaces i32) (param $term i32) (param $cx i32) (param $g i32) (result i32)
  (local $all_bindings i32)
  (local $arg i32)
  (local $body i32)
  (local $bs i32)
  (local $collect_bindings i32)
  (local $collected i32)
  (local $deannotated_fun i32)
  (local $dflt i32)
  (local $e i32)
  (local $els i32)
  (local $encode i32)
  (local $encode_binding i32)
  (local $encode_pair i32)
  (local $f i32)
  (local $field i32)
  (local $field_ref i32)
  (local $fields i32)
  (local $final_body i32)
  (local $fn i32)
  (local $ft i32)
  (local $ftyp i32)
  (local $fun i32)
  (local $harg i32)
  (local $hbindings i32)
  (local $helems i32)
  (local $hexpr i32)
  (local $hft i32)
  (local $hinner i32)
  (local $hk i32)
  (local $hl i32)
  (local $hname i32)
  (local $hr i32)
  (local $ht i32)
  (local $hv i32)
  (local $inner_lt i32)
  (local $inner_result i32)
  (local $k i32)
  (local $l i32)
  (local $let_term i32)
  (local $lhs i32)
  (local $m i32)
  (local $name i32)
  (local $nonempty_map i32)
  (local $nonempty_set i32)
  (local $p i32)
  (local $pair i32)
  (local $r i32)
  (local $rhs i32)
  (local $s i32)
  (local $sname i32)
  (local $stmt i32)
  (local $t i32)
  (local $term' i32)
  (local $term1 i32)
  (local $tname i32)
  (local $to_field_update i32)
  (local $type_name i32)
  (local $updates i32)
  (local $v i32)
  (local $x i32)
  local.get $depth
  local.get $namespaces
  local.get $t
  local.get $cx
  local.get $g
  call $hydra.ext.haskell.coder.encode_term
  local.set $encode
  i32.const 0 ;; string: "M.fromList"
  call $hydra.ext.haskell.utils.hsvar
  local.set $lhs
  local.get $pair
  call $hydra.lib.pairs.first
  local.set $k
  local.get $pair
  call $hydra.lib.pairs.second
  local.set $v
  local.get $k
  local.get $encode
  local.get $v
  local.get $encode
  i32.const 1
  i32.const 2
  ;; list elements follow
  local.get $hk
  local.get $hv
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  local.set $encode_pair
  local.get $x
  local.get $encode_pair
  local.get $m
  call $hydra.lib.maps.to_list
  call $hydra.lib.eithers.map_list
  call $hydra.lib.eithers.map
  i32.const 1
  local.get $lhs
  local.get $rhs
  call $hydra.ext.haskell.utils.hsapp
  call $hydra.lib.eithers.bind
  local.set $nonempty_map
  i32.const 0 ;; string: "S.fromList"
  call $hydra.ext.haskell.utils.hsvar
  local.set $lhs
  local.get $depth
  local.get $namespaces
  local.get $s
  call $hydra.lib.sets.to_list
  local.get $cx
  local.get $g
  call $hydra.ext.haskell.coder.encode_term
  i32.const 1
  local.get $lhs
  local.get $rhs
  call $hydra.ext.haskell.utils.hsapp
  call $hydra.lib.eithers.bind
  local.set $nonempty_set
  (block $end_term (result i32)
  (block $wrap
  (block $variable
  (block $unit
  (block $union
  (block $type_application
  (block $type_lambda
  (block $set
  (block $record
  (block $pair
  (block $maybe
  (block $map
  (block $literal
  (block $list
  (block $let
  (block $function
  (block $either
  (block $application
  local.get $term
  call $hydra.strip.deannotate_term
  br_table $application $either $function $let $list $literal $map $maybe $pair $record $set $type_lambda $type_application $union $unit $variable $wrap $end_term
  ;; project field: function
  local.set $fun
  ;; project field: argument
  local.set $arg
  local.get $fun
  call $hydra.strip.deannotate_term
  local.set $deannotated_fun
  (block $end_term (result i32)
  (block $function
  local.get $deannotated_fun
  br_table $function $end_term
  (block $end_function (result i32)
  (block $elimination
  local.get $f
  br_table $elimination $end_function
  (block $end_elimination (result i32)
  (block $union
  local.get $e
  br_table $union $end_elimination
  local.get $arg
  local.get $encode
  local.get $depth
  local.get $namespaces
  local.get $stmt
  local.get $harg
  local.get $cx
  local.get $g
  call $hydra.ext.haskell.coder.encode_case_expression
  call $hydra.lib.eithers.bind
  br $end_elimination
)
)
  br $end_function
)
)
  br $end_term
)
)
  br $end_term
)
  local.get $l
  local.get $encode
  i32.const 1
  i32.const 0 ;; string: "Left"
  call $hydra.ext.haskell.utils.hsvar
  local.get $hl
  call $hydra.ext.haskell.utils.hsapp
  call $hydra.lib.eithers.bind
  local.get $r
  local.get $encode
  i32.const 1
  i32.const 0 ;; string: "Right"
  call $hydra.ext.haskell.utils.hsvar
  local.get $hr
  call $hydra.ext.haskell.utils.hsapp
  call $hydra.lib.eithers.bind
  local.get $e
  call $hydra.lib.eithers.either
  br $end_term
)
  local.get $depth
  local.get $namespaces
  local.get $f
  local.get $cx
  local.get $g
  call $hydra.ext.haskell.coder.encode_function
  br $end_term
)
  ;; project field: bindings
  local.set $bs
  ;; project field: body
  local.set $body
  (block $end_term (result i32)
  (block $let
  local.get $body
  call $hydra.strip.deannotate_term
  br_table $let $end_term
  local.get $inner_lt
  local.get $collect_bindings
  local.set $inner_result
  local.get $bs
  local.get $inner_result
  call $hydra.lib.pairs.first
  call $hydra.lib.lists.concat2
  local.get $inner_result
  call $hydra.lib.pairs.second
  br $end_term
)
)
  local.set $collect_bindings
  local.get $let_term
  local.get $collect_bindings
  local.set $collected
  local.get $collected
  call $hydra.lib.pairs.first
  local.set $all_bindings
  local.get $collected
  call $hydra.lib.pairs.second
  local.set $final_body
  ;; project field: name
  local.set $name
  ;; project field: term
  local.set $term'
  nop
  call $hydra.ext.haskell.utils.simple_name
  local.set $hname
  local.get $term'
  local.get $encode
  i32.const 1
  local.get $hname
  local.get $hexpr
  i32.const 0
  call $hydra.ext.haskell.utils.simple_value_binding
  call $hydra.lib.eithers.bind
  local.set $encode_binding
  local.get $encode_binding
  local.get $all_bindings
  call $hydra.lib.eithers.map_list
  local.get $final_body
  local.get $encode
  i32.const 1
  local.get $hbindings
  local.get $hinner
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $encode
  local.get $els
  call $hydra.lib.eithers.map_list
  i32.const 1
  local.get $helems
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $v
  local.get $cx
  call $hydra.ext.haskell.coder.encode_literal
  br $end_term
)
  local.get $m
  call $hydra.lib.maps.null
  i32.const 1
  i32.const 0 ;; string: "M.empty"
  call $hydra.ext.haskell.utils.hsvar
  local.get $m
  local.get $nonempty_map
  call $hydra.lib.logic.if_else
  br $end_term
)
  local.get $m
  i32.const 1
  i32.const 0 ;; string: "Nothing"
  call $hydra.ext.haskell.utils.hsvar
  local.get $t
  local.get $encode
  i32.const 1
  i32.const 0 ;; string: "Just"
  call $hydra.ext.haskell.utils.hsvar
  local.get $ht
  call $hydra.ext.haskell.utils.hsapp
  call $hydra.lib.eithers.bind
  call $hydra.lib.maybes.cases
  br $end_term
)
  local.get $p
  call $hydra.lib.pairs.first
  local.get $encode
  local.get $p
  call $hydra.lib.pairs.second
  local.get $encode
  i32.const 1
  i32.const 2
  ;; list elements follow
  local.get $f
  local.get $s
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  ;; project field: type_name
  local.set $sname
  ;; project field: fields
  local.set $fields
  ;; project field: name
  local.set $fn
  ;; project field: term
  local.set $ft
  local.get $namespaces
  local.get $sname
  local.get $fn
  call $hydra.ext.haskell.utils.record_field_reference
  local.set $field_ref
  local.get $ft
  local.get $encode
  i32.const 1
  local.get $field_ref
  local.get $hft
  call $hydra.lib.eithers.bind
  local.set $to_field_update
  local.get $namespaces
  local.get $sname
  call $hydra.ext.haskell.utils.element_reference
  local.set $type_name
  local.get $to_field_update
  local.get $fields
  call $hydra.lib.eithers.map_list
  i32.const 1
  local.get $type_name
  local.get $updates
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $s
  call $hydra.lib.sets.null
  i32.const 1
  i32.const 0 ;; string: "S.empty"
  call $hydra.ext.haskell.utils.hsvar
  local.get $s
  local.get $nonempty_set
  call $hydra.lib.logic.if_else
  br $end_term
)
  ;; project field: body
  local.set $term1
  local.get $term1
  local.get $encode
  br $end_term
)
  ;; project field: body
  local.set $term1
  local.get $term1
  local.get $encode
  br $end_term
)
  ;; project field: type_name
  local.set $sname
  ;; project field: field
  local.set $field
  ;; project field: name
  local.set $fn
  ;; project field: term
  local.set $ft
  ;; project field: bound_terms
  call $hydra.lib.maps.keys
  call $hydra.lib.sets.from_list
  ;; project field: schema_types
  call $hydra.lib.maps.keys
  call $hydra.lib.sets.from_list
  call $hydra.lib.sets.union
  local.get $namespaces
  local.get $sname
  local.get $fn
  call $hydra.ext.haskell.utils.union_field_reference
  local.set $lhs
  local.get $lhs
  call $hydra.ext.haskell.utils.hsapp
  local.get $ft
  local.get $encode
  call $hydra.lib.eithers.map
  local.set $dflt
  local.get $cx
  local.get $g
  local.get $sname
  local.get $fn
  call $hydra.resolution.require_union_field
  (block $end_type (result i32)
  (block $unit
  local.get $ftyp
  call $hydra.strip.deannotate_type
  br_table $unit $end_type
  i32.const 1
  local.get $lhs
  br $end_type
)
)
  call $hydra.lib.eithers.bind
  br $end_term
)
  i32.const 1
  i32.const 0
  ;; list elements follow
  br $end_term
)
  i32.const 1
  local.get $namespaces
  local.get $name
  call $hydra.ext.haskell.utils.element_reference
  br $end_term
)
  ;; project field: type_name
  local.set $tname
  ;; project field: body
  local.set $term'
  local.get $namespaces
  local.get $tname
  call $hydra.ext.haskell.utils.element_reference
  local.set $lhs
  local.get $term'
  local.get $encode
  i32.const 1
  local.get $lhs
  local.get $rhs
  call $hydra.ext.haskell.utils.hsapp
  call $hydra.lib.eithers.bind
  br $end_term
)
)
)
  (func $hydra.ext.haskell.coder.encode_type (param $namespaces i32) (param $typ i32) (param $cx i32) (param $g i32) (result i32)
  (local $body i32)
  (local $cod i32)
  (local $dom i32)
  (local $encode i32)
  (local $f i32)
  (local $ft i32)
  (local $hcod i32)
  (local $hdom i32)
  (local $hkt i32)
  (local $hleft i32)
  (local $hlhs i32)
  (local $hlt i32)
  (local $hot i32)
  (local $hrhs i32)
  (local $hright i32)
  (local $hst i32)
  (local $hvt i32)
  (local $it i32)
  (local $kt i32)
  (local $left' i32)
  (local $lhs i32)
  (local $lt i32)
  (local $name i32)
  (local $ot i32)
  (local $ref i32)
  (local $rhs i32)
  (local $right' i32)
  (local $s i32)
  (local $st i32)
  (local $t i32)
  (local $unit_tuple i32)
  (local $v i32)
  (local $v1 i32)
  (local $vt i32)
  local.get $namespaces
  local.get $t
  local.get $cx
  local.get $g
  call $hydra.ext.haskell.coder.encode_type
  local.set $encode
  i32.const 1
  local.get $namespaces
  local.get $name
  call $hydra.ext.haskell.utils.element_reference
  local.set $ref
  i32.const 0
  ;; list elements follow
  local.set $unit_tuple
  (block $end_type (result i32)
  (block $wrap
  (block $void
  (block $variable
  (block $unit
  (block $union
  (block $set
  (block $record
  (block $pair
  (block $maybe
  (block $map
  (block $literal
  (block $list
  (block $forall
  (block $function
  (block $either
  (block $application
  local.get $typ
  call $hydra.strip.deannotate_type
  br_table $application $either $function $forall $list $literal $map $maybe $pair $record $set $union $unit $variable $void $wrap $end_type
  ;; project field: function
  local.set $lhs
  ;; project field: argument
  local.set $rhs
  local.get $lhs
  local.get $encode
  local.get $rhs
  local.get $encode
  i32.const 1
  i32.const 2
  ;; list elements follow
  local.get $hlhs
  local.get $hrhs
  call $hydra.ext.haskell.utils.to_type_application
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_type
)
  ;; project field: left
  local.set $left'
  ;; project field: right
  local.set $right'
  local.get $left'
  local.get $encode
  local.get $right'
  local.get $encode
  i32.const 1
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "Either"
  call $hydra.ext.haskell.utils.raw_name
  local.get $hleft
  local.get $hright
  call $hydra.ext.haskell.utils.to_type_application
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_type
)
  ;; project field: domain
  local.set $dom
  ;; project field: codomain
  local.set $cod
  local.get $dom
  local.get $encode
  local.get $cod
  local.get $encode
  i32.const 1
  local.get $hdom
  local.get $hcod
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_type
)
  ;; project field: parameter
  local.set $v
  ;; project field: body
  local.set $body
  local.get $body
  local.get $encode
  br $end_type
)
  local.get $lt
  local.get $encode
  i32.const 1
  local.get $hlt
  call $hydra.lib.eithers.bind
  br $end_type
)
  (block $end_literal_type (result i32)
  (block $string
  (block $integer
  (block $float
  (block $boolean
  (block $binary
  local.get $lt
  br_table $binary $boolean $float $integer $string $end_literal_type
  i32.const 1
  i32.const 0 ;; string: "B.ByteString"
  call $hydra.ext.haskell.utils.raw_name
  br $end_literal_type
)
  i32.const 1
  i32.const 0 ;; string: "Bool"
  call $hydra.ext.haskell.utils.raw_name
  br $end_literal_type
)
  (block $end_float_type (result i32)
  (block $bigfloat
  (block $float64
  (block $float32
  local.get $ft
  br_table $float32 $float64 $bigfloat $end_float_type
  i32.const 1
  i32.const 0 ;; string: "Float"
  call $hydra.ext.haskell.utils.raw_name
  br $end_float_type
)
  i32.const 1
  i32.const 0 ;; string: "Double"
  call $hydra.ext.haskell.utils.raw_name
  br $end_float_type
)
  i32.const 1
  i32.const 0 ;; string: "Double"
  call $hydra.ext.haskell.utils.raw_name
  br $end_float_type
)
)
  br $end_literal_type
)
  (block $end_integer_type (result i32)
  (block $int64
  (block $int32
  (block $int16
  (block $int8
  (block $bigint
  local.get $it
  br_table $bigint $int8 $int16 $int32 $int64 $end_integer_type
  i32.const 1
  i32.const 0 ;; string: "Integer"
  call $hydra.ext.haskell.utils.raw_name
  br $end_integer_type
)
  i32.const 1
  i32.const 0 ;; string: "I.Int8"
  call $hydra.ext.haskell.utils.raw_name
  br $end_integer_type
)
  i32.const 1
  i32.const 0 ;; string: "I.Int16"
  call $hydra.ext.haskell.utils.raw_name
  br $end_integer_type
)
  i32.const 1
  i32.const 0 ;; string: "Int"
  call $hydra.ext.haskell.utils.raw_name
  br $end_integer_type
)
  i32.const 1
  i32.const 0 ;; string: "I.Int64"
  call $hydra.ext.haskell.utils.raw_name
  br $end_integer_type
)
)
  br $end_literal_type
)
  i32.const 1
  i32.const 0 ;; string: "String"
  call $hydra.ext.haskell.utils.raw_name
  br $end_literal_type
)
)
  br $end_type
)
  ;; project field: keys
  local.set $kt
  ;; project field: values
  local.set $vt
  local.get $kt
  local.get $encode
  local.get $vt
  local.get $encode
  i32.const 1
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "M.Map"
  call $hydra.ext.haskell.utils.raw_name
  local.get $hkt
  local.get $hvt
  call $hydra.ext.haskell.utils.to_type_application
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_type
)
  local.get $ot
  local.get $encode
  i32.const 1
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "Maybe"
  call $hydra.ext.haskell.utils.raw_name
  local.get $hot
  call $hydra.ext.haskell.utils.to_type_application
  call $hydra.lib.eithers.bind
  br $end_type
)
  ;; project field: first
  local.get $encode
  ;; project field: second
  local.get $encode
  i32.const 1
  i32.const 2
  ;; list elements follow
  local.get $f
  local.get $s
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_type
)
  i32.const 0 ;; string: "placeholder"
  local.get $ref
  br $end_type
)
  local.get $st
  local.get $encode
  i32.const 1
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "S.Set"
  call $hydra.ext.haskell.utils.raw_name
  local.get $hst
  call $hydra.ext.haskell.utils.to_type_application
  call $hydra.lib.eithers.bind
  br $end_type
)
  i32.const 0 ;; string: "placeholder"
  local.get $ref
  br $end_type
)
  i32.const 1
  local.get $unit_tuple
  br $end_type
)
  local.get $v1
  local.get $ref
  br $end_type
)
  i32.const 1
  i32.const 0 ;; string: "Void"
  call $hydra.ext.haskell.utils.raw_name
  br $end_type
)
  i32.const 0 ;; string: "placeholder"
  local.get $ref
  br $end_type
)
)
)
  (func $hydra.ext.haskell.coder.encode_type_with_class_assertions (param $namespaces i32) (param $explicit_classes i32) (param $typ i32) (param $cx i32) (param $g i32) (result i32)
  (local $assert_pairs i32)
  (local $c i32)
  (local $classes i32)
  (local $cls i32)
  (local $cls_set i32)
  (local $encode_assertion i32)
  (local $encoded i32)
  (local $hassert i32)
  (local $hname i32)
  (local $htyp i32)
  (local $htype i32)
  (local $implicit_classes i32)
  (local $map_entry i32)
  (local $name i32)
  (local $pair i32)
  (local $to_pair i32)
  (local $to_pairs i32)
  local.get $explicit_classes
  local.get $typ
  call $hydra.ext.haskell.coder.get_implicit_type_classes
  call $hydra.lib.maps.union
  local.set $classes
  local.get $typ
  call $hydra.ext.haskell.coder.get_implicit_type_classes
  local.set $implicit_classes
  local.get $pair
  call $hydra.lib.pairs.first
  local.set $name
  local.get $pair
  call $hydra.lib.pairs.second
  local.set $cls
  (block $end_type_class (result i32)
  (block $ordering
  (block $equality
  local.get $cls
  br_table $equality $ordering $end_type_class
  i32.const 0 ;; string: "Eq"
  br $end_type_class
)
  i32.const 0 ;; string: "Ord"
  br $end_type_class
)
)
  call $hydra.ext.haskell.utils.raw_name
  local.set $hname
  nop
  call $hydra.ext.haskell.utils.raw_name
  local.set $htype
  local.get $hname
  i32.const 1
  ;; list elements follow
  local.get $htype
  local.set $encode_assertion
  local.get $to_pairs
  local.get $classes
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.lists.concat
  local.set $assert_pairs
  local.get $map_entry
  call $hydra.lib.pairs.first
  local.set $name
  local.get $map_entry
  call $hydra.lib.pairs.second
  local.set $cls_set
  local.get $name
  local.get $c
  local.set $to_pair
  local.get $to_pair
  local.get $cls_set
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  local.set $to_pairs
  local.get $namespaces
  local.get $typ
  local.get $cx
  local.get $g
  call $hydra.ext.haskell.coder.adapt_type_to_haskell_and_encode
  local.get $assert_pairs
  call $hydra.lib.lists.null
  i32.const 1
  local.get $htyp
  local.get $encode_assertion
  local.get $assert_pairs
  call $hydra.lib.lists.map
  local.set $encoded
  local.get $encoded
  call $hydra.lib.lists.length
  i32.const 1
  call $hydra.lib.equality.equal
  local.get $encoded
  call $hydra.lib.lists.head
  local.get $encoded
  call $hydra.lib.logic.if_else
  local.set $hassert
  i32.const 1
  local.get $hassert
  local.get $htyp
  call $hydra.lib.logic.if_else
  call $hydra.lib.eithers.bind
)
  (func $hydra.ext.haskell.coder.extend_meta_for_term (param $meta i32) (param $term i32) (result i32)
  (block $end_term (result i32)
  (block $set
  (block $map
  local.get $term
  br_table $map $set $end_term
  i32.const 1
  local.get $meta
  call $hydra.ext.haskell.coder.set_meta_uses_map
  br $end_term
)
  i32.const 1
  local.get $meta
  call $hydra.ext.haskell.coder.set_meta_uses_set
  br $end_term
)
)
)
  (func $hydra.ext.haskell.coder.extend_meta_for_type (param $meta i32) (param $typ i32) (result i32)
  (local $it i32)
  (local $lt i32)
  (block $end_type (result i32)
  (block $set
  (block $map
  (block $literal
  local.get $typ
  call $hydra.strip.deannotate_type
  br_table $literal $map $set $end_type
  (block $end_literal_type (result i32)
  (block $integer
  (block $binary
  local.get $lt
  br_table $binary $integer $end_literal_type
  i32.const 1
  local.get $meta
  call $hydra.ext.haskell.coder.set_meta_uses_byte_string
  br $end_literal_type
)
  (block $end_integer_type (result i32)
  (block $int64
  (block $int16
  (block $int8
  local.get $it
  br_table $int8 $int16 $int64 $end_integer_type
  i32.const 1
  local.get $meta
  call $hydra.ext.haskell.coder.set_meta_uses_int
  br $end_integer_type
)
  i32.const 1
  local.get $meta
  call $hydra.ext.haskell.coder.set_meta_uses_int
  br $end_integer_type
)
  i32.const 1
  local.get $meta
  call $hydra.ext.haskell.coder.set_meta_uses_int
  br $end_integer_type
)
)
  br $end_literal_type
)
)
  br $end_type
)
  i32.const 1
  local.get $meta
  call $hydra.ext.haskell.coder.set_meta_uses_map
  br $end_type
)
  i32.const 1
  local.get $meta
  call $hydra.ext.haskell.coder.set_meta_uses_set
  br $end_type
)
)
)
  (func $hydra.ext.haskell.coder.find_ord_variables (param $typ i32) (result i32)
  (local $et i32)
  (local $fold i32)
  (local $is_type_variable i32)
  (local $kt i32)
  (local $names i32)
  (local $t i32)
  (local $try_type i32)
  (local $typ' i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $set
  (block $map
  local.get $typ'
  br_table $map $set $end_type
  ;; project field: keys
  local.set $kt
  local.get $names
  local.get $kt
  local.get $try_type
  br $end_type
)
  local.get $names
  local.get $et
  local.get $try_type
  br $end_type
)
)
  local.set $fold
  local.get $v
  call $hydra.names.namespace_of
  call $hydra.lib.maybes.is_nothing
  local.set $is_type_variable
  (block $end_type (result i32)
  (block $variable
  local.get $t
  call $hydra.strip.deannotate_type
  br_table $variable $end_type
  local.get $v
  local.get $is_type_variable
  local.get $v
  local.get $names
  call $hydra.lib.sets.insert
  local.get $names
  call $hydra.lib.logic.if_else
  br $end_type
)
)
  local.set $try_type
  i32.const 0
  local.get $fold
  call $hydra.lib.sets.empty
  local.get $typ
  call $hydra.rewriting.fold_over_type
)
  (func $hydra.ext.haskell.coder.gather_metadata (param $defs i32) (result i32)
  (local $add_def i32)
  (local $def i32)
  (local $m i32)
  (local $meta i32)
  (local $meta_with_term i32)
  (local $t i32)
  (local $term i32)
  (local $typ i32)
  (block $end_definition (result i32)
  (block $type
  (block $term
  local.get $def
  br_table $term $type $end_definition
  ;; project field: term
  local.set $term
  i32.const 0
  local.get $m
  local.get $t
  call $hydra.ext.haskell.coder.extend_meta_for_term
  local.get $meta
  local.get $term
  call $hydra.rewriting.fold_over_term
  local.set $meta_with_term
  local.get $meta_with_term
  i32.const 0
  local.get $m
  local.get $t
  call $hydra.ext.haskell.coder.extend_meta_for_type
  local.get $meta_with_term
  ;; project field: type
  call $hydra.rewriting.fold_over_type
  ;; project field: type
  call $hydra.lib.maybes.maybe
  br $end_definition
)
  ;; project field: type
  local.set $typ
  i32.const 0
  local.get $m
  local.get $t
  call $hydra.ext.haskell.coder.extend_meta_for_type
  local.get $meta
  local.get $typ
  call $hydra.rewriting.fold_over_type
  br $end_definition
)
)
  local.set $add_def
  local.get $add_def
  call $hydra.ext.haskell.coder.empty_metadata
  local.get $defs
  call $hydra.lib.lists.foldl
)
  (func $hydra.ext.haskell.coder.get_implicit_type_classes (param $typ i32) (result i32)
  (local $name i32)
  (local $to_pair i32)
  local.get $name
  i32.const 1
  ;; list elements follow
  i32.const 0
  call $hydra.lib.sets.from_list
  local.set $to_pair
  local.get $to_pair
  local.get $typ
  call $hydra.ext.haskell.coder.find_ord_variables
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
)
  (func $hydra.ext.haskell.coder.include_type_definitions (result i32)
  i32.const 0
)
  (func $hydra.ext.haskell.coder.key_haskell_var (result i32)
  i32.const 0 ;; string: "haskellVar"
)
  (func $hydra.ext.haskell.coder.module_to_haskell (param $mod i32) (param $defs i32) (param $cx i32) (param $g i32) (result i32)
  (local $filepath i32)
  (local $hsmod i32)
  (local $s i32)
  local.get $mod
  local.get $defs
  local.get $cx
  local.get $g
  call $hydra.ext.haskell.coder.module_to_haskell_module
  local.get $hsmod
  call $hydra.ext.haskell.serde.module_to_expr
  call $hydra.serialization.parenthesize
  call $hydra.serialization.print_expr
  local.set $s
  i32.const 0
  i32.const 0 ;; string: "hs"
  ;; project field: namespace
  call $hydra.names.namespace_to_file_path
  local.set $filepath
  i32.const 1
  local.get $filepath
  local.get $s
  call $hydra.lib.maps.singleton
  call $hydra.lib.eithers.bind
)
  (func $hydra.ext.haskell.coder.module_to_haskell_module (param $mod i32) (param $defs i32) (param $cx i32) (param $g i32) (result i32)
  (local $namespaces i32)
  local.get $mod
  local.get $cx
  local.get $g
  call $hydra.ext.haskell.utils.namespaces_for_module
  local.get $namespaces
  local.get $mod
  local.get $defs
  local.get $cx
  local.get $g
  call $hydra.ext.haskell.coder.construct_module
  call $hydra.lib.eithers.bind
)
  (func $hydra.ext.haskell.coder.name_decls (param $namespaces i32) (param $name i32) (param $typ i32) (result i32)
  (local $decl i32)
  (local $field_decls i32)
  (local $fname i32)
  (local $k i32)
  (local $n i32)
  (local $name_decl i32)
  (local $nm i32)
  (local $pair i32)
  (local $to_constant i32)
  (local $to_decl i32)
  (local $v i32)
  nop
  local.set $nm
  local.get $pair
  call $hydra.lib.pairs.first
  local.set $k
  local.get $pair
  call $hydra.lib.pairs.second
  local.set $v
  local.get $k
  call $hydra.ext.haskell.utils.simple_name
  i32.const 0
  ;; list elements follow
  call $hydra.ext.haskell.utils.application_pattern
  local.get $namespaces
  local.get $n
  call $hydra.ext.haskell.utils.element_reference
  local.get $v
  i32.const 0
  local.set $decl
  local.get $decl
  i32.const 0
  local.set $to_decl
  local.get $name
  call $hydra.ext.haskell.coder.constant_for_type_name
  local.get $nm
  local.set $name_decl
  local.get $to_constant
  local.get $typ
  call $hydra.lexical.fields_of
  call $hydra.lib.lists.map
  local.set $field_decls
  ;; project field: name
  local.set $fname
  local.get $name
  local.get $fname
  call $hydra.ext.haskell.coder.constant_for_field_name
  nop
  local.set $to_constant
  call $hydra.ext.haskell.coder.use_core_import
  i32.const 0 ;; string: "hydra.core.Name"
  local.get $name_decl
  local.get $to_decl
  i32.const 0 ;; string: "hydra.core.Name"
  local.get $to_decl
  local.get $field_decls
  call $hydra.lib.lists.map
  call $hydra.lib.lists.cons
  i32.const 0
  ;; list elements follow
  call $hydra.lib.logic.if_else
)
  (func $hydra.ext.haskell.coder.set_meta_uses_byte_string (param $b i32) (param $m i32) (result i32)
  local.get $b
  ;; project field: uses_int
  ;; project field: uses_map
  ;; project field: uses_set
)
  (func $hydra.ext.haskell.coder.set_meta_uses_int (param $b i32) (param $m i32) (result i32)
  ;; project field: uses_byte_string
  local.get $b
  ;; project field: uses_map
  ;; project field: uses_set
)
  (func $hydra.ext.haskell.coder.set_meta_uses_map (param $b i32) (param $m i32) (result i32)
  ;; project field: uses_byte_string
  ;; project field: uses_int
  local.get $b
  ;; project field: uses_set
)
  (func $hydra.ext.haskell.coder.set_meta_uses_set (param $b i32) (param $m i32) (result i32)
  ;; project field: uses_byte_string
  ;; project field: uses_int
  ;; project field: uses_map
  local.get $b
)
  (func $hydra.ext.haskell.coder.to_data_declaration (param $namespaces i32) (param $def i32) (param $cx i32) (param $g i32) (result i32)
  (local $all_bindings i32)
  (local $args i32)
  (local $bindings i32)
  (local $body i32)
  (local $comments i32)
  (local $env i32)
  (local $hbindings i32)
  (local $hname i32)
  (local $hname' i32)
  (local $hname'' i32)
  (local $hnames i32)
  (local $hterm' i32)
  (local $hterms i32)
  (local $lbindings i32)
  (local $name i32)
  (local $name' i32)
  (local $new_pattern i32)
  (local $new_rhs i32)
  (local $pattern' i32)
  (local $prev_bindings i32)
  (local $rewrite_value_binding i32)
  (local $rhs i32)
  (local $rhs_expr i32)
  (local $t i32)
  (local $term i32)
  (local $term' i32)
  (local $terms i32)
  (local $to_decl i32)
  (local $to_term_definition i32)
  (local $typ i32)
  (local $vars i32)
  (local $vb i32)
  ;; project field: name
  local.set $name
  ;; project field: term
  local.set $term
  ;; project field: type
  local.set $typ
  local.get $name
  call $hydra.names.local_name_of
  call $hydra.ext.haskell.utils.simple_name
  local.set $hname
  (block $end_value_binding (result i32)
  (block $simple
  local.get $vb
  br_table $simple $end_value_binding
  ;; project field: pattern
  local.set $pattern'
  ;; project field: rhs
  local.set $rhs
  ;; project field: local_bindings
  local.set $bindings
  (block $end_pattern (result i32)
  (block $application
  local.get $pattern'
  br_table $application $end_pattern
  ;; project field: name
  local.set $name'
  ;; project field: args
  local.set $args
  nop
  local.set $rhs_expr
  (block $end_expression (result i32)
  (block $lambda
  local.get $rhs_expr
  br_table $lambda $end_expression
  ;; project field: bindings
  local.set $vars
  ;; project field: inner
  local.set $body
  local.get $name'
  local.get $args
  local.get $vars
  call $hydra.lib.lists.concat2
  call $hydra.ext.haskell.utils.application_pattern
  local.set $new_pattern
  local.get $body
  local.set $new_rhs
  local.get $new_pattern
  local.get $new_rhs
  local.get $bindings
  local.get $rewrite_value_binding
  br $end_expression
)
)
  br $end_pattern
)
)
  br $end_value_binding
)
)
  local.set $rewrite_value_binding
  (block $end_term (result i32)
  (block $let
  local.get $term'
  call $hydra.strip.deannotate_term
  br_table $let $end_term
  ;; project field: bindings
  local.set $lbindings
  ;; project field: body
  local.set $env
  local.get $hname''
  local.get $hterm'
  i32.const 0
  call $hydra.ext.haskell.utils.simple_value_binding
  local.set $to_term_definition
  nop
  call $hydra.ext.haskell.utils.simple_name
  local.get $lbindings
  call $hydra.lib.lists.map
  local.set $hnames
  ;; project field: term
  local.get $lbindings
  call $hydra.lib.lists.map
  local.set $terms
  i32.const 0
  local.get $namespaces
  local.get $t
  local.get $cx
  local.get $g
  call $hydra.ext.haskell.coder.encode_term
  local.get $terms
  call $hydra.lib.eithers.map_list
  local.get $to_term_definition
  local.get $hnames
  local.get $hterms
  call $hydra.lib.lists.zip_with
  local.set $hbindings
  i32.const 0
  ;; list elements follow
  nop
  local.get $bindings
  call $hydra.lib.maybes.maybe
  local.set $prev_bindings
  local.get $prev_bindings
  local.get $hbindings
  call $hydra.lib.lists.concat2
  local.set $all_bindings
  local.get $comments
  local.get $hname'
  local.get $env
  local.get $all_bindings
  local.get $to_decl
  call $hydra.lib.eithers.bind
  br $end_term
)
)
  local.set $to_decl
  local.get $cx
  local.get $g
  local.get $term
  call $hydra.annotations.get_term_description
  local.get $comments
  local.get $hname
  local.get $term
  i32.const 0
  local.get $to_decl
  call $hydra.lib.eithers.bind
)
  (func $hydra.ext.haskell.coder.to_type_declarations_from (param $namespaces i32) (param $element_name i32) (param $typ i32) (param $cx i32) (param $g i32) (result i32)
  (local $bound_names' i32)
  (local $comments i32)
  (local $cons i32)
  (local $constructor_name i32)
  (local $decl i32)
  (local $decl' i32)
  (local $decl_head i32)
  (local $deconflict i32)
  (local $deriv i32)
  (local $fields i32)
  (local $fname i32)
  (local $ftype i32)
  (local $h i32)
  (local $h_fields i32)
  (local $hd i32)
  (local $hfield i32)
  (local $hname i32)
  (local $hname' i32)
  (local $hname0 i32)
  (local $htype i32)
  (local $hvar i32)
  (local $is_ser i32)
  (local $lname i32)
  (local $lname' i32)
  (local $main_decl i32)
  (local $name i32)
  (local $name_decls' i32)
  (local $newtype_cons i32)
  (local $nm i32)
  (local $record_cons i32)
  (local $rest i32)
  (local $rt i32)
  (local $t' i32)
  (local $tdecls i32)
  (local $tname i32)
  (local $to_field i32)
  (local $typ' i32)
  (local $type_list i32)
  (local $union_cons i32)
  (local $unpack_result i32)
  (local $vars i32)
  (local $vars' i32)
  (local $wrapped i32)
  local.get $element_name
  call $hydra.names.local_name_of
  local.set $lname
  local.get $lname
  call $hydra.ext.haskell.utils.simple_name
  local.set $hname
  local.get $vars'
  call $hydra.lib.lists.null
  local.get $name
  local.get $vars'
  call $hydra.lib.lists.head
  local.set $h
  local.get $vars'
  call $hydra.lib.lists.tail
  local.set $rest
  nop
  call $hydra.ext.haskell.utils.simple_name
  local.set $hvar
  local.get $name
  local.get $rest
  local.get $decl_head
  local.get $hvar
  call $hydra.lib.logic.if_else
  local.set $decl_head
  local.get $tname
  call $hydra.ext.haskell.utils.newtype_accessor_name
  call $hydra.ext.haskell.utils.simple_name
  local.set $hname0
  local.get $namespaces
  local.get $typ'
  local.get $cx
  local.get $g
  call $hydra.ext.haskell.coder.adapt_type_to_haskell_and_encode
  local.get $hname0
  local.get $htype
  i32.const 0
  local.set $hfield
  local.get $tname
  call $hydra.names.local_name_of
  call $hydra.ext.haskell.utils.simple_name
  local.set $constructor_name
  i32.const 1
  local.get $constructor_name
  i32.const 1
  ;; list elements follow
  local.get $hfield
  i32.const 0
  call $hydra.lib.eithers.bind
  local.set $newtype_cons
  ;; project field: name
  local.set $fname
  ;; project field: type
  local.set $ftype
  local.get $lname'
  call $hydra.formatting.decapitalize
  nop
  call $hydra.formatting.capitalize
  call $hydra.lib.strings.cat2
  call $hydra.ext.haskell.utils.simple_name
  local.set $hname'
  local.get $namespaces
  local.get $ftype
  local.get $cx
  local.get $g
  call $hydra.ext.haskell.coder.adapt_type_to_haskell_and_encode
  local.get $cx
  local.get $g
  local.get $ftype
  call $hydra.annotations.get_type_description
  i32.const 1
  local.get $hname'
  local.get $htype
  local.get $comments
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  local.set $to_field
  local.get $to_field
  local.get $fields
  call $hydra.lib.eithers.map_list
  i32.const 1
  local.get $lname'
  call $hydra.ext.haskell.utils.simple_name
  local.get $h_fields
  i32.const 0
  call $hydra.lib.eithers.bind
  local.set $record_cons
  ;; project field: name
  local.set $fname
  ;; project field: type
  local.set $ftype
  ;; project field: focus
  call $hydra.lib.pairs.first
  local.get $name
  call $hydra.names.unqualify_name
  local.set $tname
  local.get $tname
  local.get $bound_names'
  call $hydra.lib.sets.member
  local.get $name
  i32.const 0 ;; string: "_"
  call $hydra.lib.strings.cat2
  local.get $deconflict
  local.get $name
  call $hydra.lib.logic.if_else
  local.set $deconflict
  local.get $cx
  local.get $g
  local.get $ftype
  call $hydra.annotations.get_type_description
  local.get $lname'
  call $hydra.formatting.capitalize
  nop
  call $hydra.formatting.capitalize
  call $hydra.lib.strings.cat2
  local.get $deconflict
  local.set $nm
  local.get $ftype
  call $hydra.strip.deannotate_type
  i32.const 0
  call $hydra.lib.equality.equal
  i32.const 1
  i32.const 0
  ;; list elements follow
  local.get $namespaces
  local.get $ftype
  local.get $cx
  local.get $g
  call $hydra.ext.haskell.coder.adapt_type_to_haskell_and_encode
  i32.const 1
  i32.const 1
  ;; list elements follow
  local.get $htype
  call $hydra.lib.eithers.bind
  call $hydra.lib.logic.if_else
  i32.const 1
  local.get $nm
  call $hydra.ext.haskell.utils.simple_name
  local.get $type_list
  local.get $comments
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  local.set $union_cons
  local.get $cx
  local.get $g
  local.get $element_name
  call $hydra.predicates.is_serializable_by_name
  local.get $is_ser
  call $hydra.ext.haskell.utils.raw_name
  i32.const 4
  ;; list elements follow
  i32.const 0 ;; string: "Eq"
  i32.const 0 ;; string: "Ord"
  i32.const 0 ;; string: "Read"
  i32.const 0 ;; string: "Show"
  call $hydra.lib.lists.map
  i32.const 0
  ;; list elements follow
  call $hydra.lib.logic.if_else
  local.set $deriv
  local.get $typ
  call $hydra.ext.haskell.utils.unpack_forall_type
  local.set $unpack_result
  local.get $unpack_result
  call $hydra.lib.pairs.first
  local.set $vars
  local.get $unpack_result
  call $hydra.lib.pairs.second
  local.set $t'
  local.get $hname
  local.get $vars
  call $hydra.lib.lists.reverse
  local.get $decl_head
  local.set $hd
  (block $end_type (result i32)
  (block $wrap
  (block $union
  (block $record
  local.get $t'
  call $hydra.strip.deannotate_type
  br_table $record $union $wrap $end_type
  local.get $lname
  local.get $rt
  local.get $record_cons
  i32.const 1
  i32.const 0
  i32.const 0
  ;; list elements follow
  local.get $hd
  i32.const 1
  ;; list elements follow
  local.get $cons
  i32.const 1
  ;; list elements follow
  local.get $deriv
  call $hydra.lib.eithers.bind
  br $end_type
)
  ;; project field: bound_terms
  call $hydra.lib.maps.keys
  call $hydra.lib.sets.from_list
  local.get $lname
  local.get $union_cons
  local.get $rt
  call $hydra.lib.eithers.map_list
  i32.const 1
  i32.const 0
  i32.const 0
  ;; list elements follow
  local.get $hd
  local.get $cons
  i32.const 1
  ;; list elements follow
  local.get $deriv
  call $hydra.lib.eithers.bind
  br $end_type
)
  local.get $element_name
  local.get $wrapped
  local.get $newtype_cons
  i32.const 1
  i32.const 0
  i32.const 0
  ;; list elements follow
  local.get $hd
  i32.const 1
  ;; list elements follow
  local.get $cons
  i32.const 1
  ;; list elements follow
  local.get $deriv
  call $hydra.lib.eithers.bind
  br $end_type
)
)
  local.get $cx
  local.get $g
  local.get $typ
  call $hydra.annotations.get_type_description
  call $hydra.ext.haskell.coder.include_type_definitions
  local.get $namespaces
  local.get $element_name
  local.get $typ
  local.get $cx
  local.get $g
  call $hydra.ext.haskell.coder.type_decl
  i32.const 1
  i32.const 1
  ;; list elements follow
  local.get $decl'
  call $hydra.lib.eithers.bind
  i32.const 1
  i32.const 0
  ;; list elements follow
  call $hydra.lib.logic.if_else
  local.get $decl
  local.get $comments
  local.set $main_decl
  local.get $namespaces
  local.get $element_name
  local.get $typ
  call $hydra.ext.haskell.coder.name_decls
  local.set $name_decls'
  i32.const 1
  i32.const 3
  ;; list elements follow
  i32.const 1
  ;; list elements follow
  local.get $main_decl
  local.get $name_decls'
  local.get $tdecls
  call $hydra.lib.lists.concat
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.ext.haskell.coder.type_decl (param $namespaces i32) (param $name i32) (param $typ i32) (param $cx i32) (param $g i32) (result i32)
  (local $decl i32)
  (local $decode_name i32)
  (local $decode_string i32)
  (local $expr i32)
  (local $final_term i32)
  (local $fname i32)
  (local $for_type i32)
  (local $for_variable_type i32)
  (local $fterm i32)
  (local $hname i32)
  (local $lit i32)
  (local $local i32)
  (local $mns i32)
  (local $name' i32)
  (local $ns i32)
  (local $pat i32)
  (local $qname i32)
  (local $raw_term i32)
  (local $recurse i32)
  (local $rewrite i32)
  (local $rhs i32)
  (local $s i32)
  (local $term i32)
  (local $term2 i32)
  (local $type_name i32)
  (local $type_name_local i32)
  (local $variant_result i32)
  (local $vname i32)
  (local $x i32)
  local.get $ns
  local.get $name'
  local.get $type_name_local
  call $hydra.names.qname
  local.set $type_name
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "_"
  local.get $name'
  call $hydra.names.local_name_of
  i32.const 0 ;; string: "_type_"
  call $hydra.lib.strings.cat
  local.set $type_name_local
  local.get $typ
  call $hydra.encode.core.type
  local.set $raw_term
  (block $end_term (result i32)
  (block $union
  local.get $term
  call $hydra.strip.deannotate_term
  br_table $union $end_term
  ;; project field: type_name
  i32.const 0 ;; string: "hydra.core.Type"
  call $hydra.lib.equality.equal
  ;; project field: field
  i32.const 0
  call $hydra.lib.logic.if_else
  br $end_term
)
)
  local.set $variant_result
  (block $end_term (result i32)
  (block $literal
  local.get $term2
  call $hydra.strip.deannotate_term
  br_table $literal $end_term
  (block $end_literal (result i32)
  (block $string
  local.get $lit
  br_table $string $end_literal
  local.get $s
  br $end_literal
)
)
  br $end_term
)
)
  local.set $decode_string
  (block $end_term (result i32)
  (block $wrap
  local.get $term2
  call $hydra.strip.deannotate_term
  br_table $wrap $end_term
  ;; project field: type_name
  i32.const 0 ;; string: "hydra.core.Name"
  call $hydra.lib.equality.equal
  local.get $x
  ;; project field: body
  local.get $decode_string
  call $hydra.lib.maybes.map
  i32.const 0
  call $hydra.lib.logic.if_else
  br $end_term
)
)
  local.set $decode_name
  ;; project field: name
  local.set $fname
  ;; project field: term
  local.set $fterm
  local.get $fname
  i32.const 0 ;; string: "record"
  call $hydra.lib.equality.equal
  i32.const 0
  local.get $fname
  i32.const 0 ;; string: "variable"
  call $hydra.lib.equality.equal
  local.get $fterm
  local.get $decode_name
  local.get $for_variable_type
  call $hydra.lib.maybes.bind
  i32.const 0
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  local.set $for_type
  local.get $vname
  call $hydra.names.qualify_name
  local.set $qname
  ;; project field: namespace
  local.set $mns
  ;; project field: local
  local.set $local
  local.get $ns
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "_"
  local.get $local
  i32.const 0 ;; string: "_type_"
  call $hydra.lib.strings.cat
  call $hydra.names.qname
  local.get $mns
  call $hydra.lib.maybes.map
  local.set $for_variable_type
  local.get $term
  local.get $recurse
  local.get $variant_result
  local.get $for_type
  call $hydra.lib.maybes.bind
  call $hydra.lib.maybes.from_maybe
  local.set $rewrite
  local.get $rewrite
  local.get $raw_term
  call $hydra.rewriting.rewrite_term
  local.set $final_term
  i32.const 0
  local.get $namespaces
  local.get $final_term
  local.get $cx
  local.get $g
  call $hydra.ext.haskell.coder.encode_term
  local.get $expr
  local.set $rhs
  local.get $name
  local.get $type_name_local
  call $hydra.ext.haskell.utils.simple_name
  local.set $hname
  local.get $hname
  i32.const 0
  ;; list elements follow
  call $hydra.ext.haskell.utils.application_pattern
  local.set $pat
  local.get $pat
  local.get $rhs
  i32.const 0
  local.set $decl
  i32.const 1
  local.get $decl
  i32.const 0
  call $hydra.lib.eithers.bind
)
  (func $hydra.ext.haskell.coder.type_scheme_constraints_to_class_map (param $maybe_constraints i32) (result i32)
  (local $class_name_str i32)
  (local $constraints i32)
  (local $is_eq i32)
  (local $is_ord i32)
  (local $name_to_type_class i32)
  nop
  local.set $class_name_str
  local.get $class_name_str
  nop
  call $hydra.lib.equality.equal
  local.set $is_eq
  local.get $class_name_str
  nop
  call $hydra.lib.equality.equal
  local.set $is_ord
  local.get $is_eq
  i32.const 0
  local.get $is_ord
  i32.const 0
  i32.const 0
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  local.set $name_to_type_class
  call $hydra.lib.maps.empty
  local.get $name_to_type_class
  ;; project field: classes
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  call $hydra.lib.sets.from_list
  local.get $constraints
  call $hydra.lib.maps.map
  local.get $maybe_constraints
  call $hydra.lib.maybes.maybe
)
  (func $hydra.ext.haskell.coder.use_core_import (result i32)
  i32.const 1
)
)
